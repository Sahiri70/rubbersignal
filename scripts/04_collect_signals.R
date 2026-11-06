# =============================================================
# RUBBERSIGNAL.COM — Script 04 : Signaux faibles complets V2
# Auteur  : Martial Sahiri
# Version : 2.3 — mission 2 (nov. 2026) : plus aucune valeur figée
#           PMI manuel -> confiance des industriels OCDE (FRED) ; BDI/fret
#           estimés -> data/reference/fret.csv daté ; plus de taux de change
#           de secours ; géopolitique sans article = NA ; Pré-RSI v2 (sens,
#           poids et seuil de 3 composantes dans scripts/00_regles.R)
#           (2.2 : terrain CI et stocks sans valeurs figées)
# Modules :
#   1. Météo 4 zones productrices (Open-Meteo — automatique)
#   2. Devises (ExchangeRate API + référence 12 mois FRED — automatique)
#   3. Demande aval (FRED automatique, dont confiance industrielle OCDE)
#   4. Stocks mondiaux (indisponible : aucune source alimentée)
#   5. Shipping / Fret maritime (WTI FRED + saisie datée data/reference/fret.csv)
#   6. Terrain CI (relevé daté, data/reference/terrain_ci.csv)
#   7. Géopolitique / Tensions (NewsAPI — automatique, clé en en-tête)
#   + Pré-RSI composite 0-100 (non publié sous 3 composantes)
# Usage   : source("scripts/04_collect_signals.R")
# =============================================================

# ── 1. CHARGER LES PACKAGES ──────────────────────────────────

library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(rvest)


# ── 2. PARAMÈTRES GLOBAUX ────────────────────────────────────

DATE_COLLECTE    <- Sys.Date()
SEMAINE          <- isoweek(DATE_COLLECTE)
ANNEE            <- year(DATE_COLLECTE)
EXCHANGERATE_KEY <- Sys.getenv("EXCHANGERATE_KEY")
FRED_KEY         <- Sys.getenv("FRED_KEY")
NEWSAPI_KEY      <- Sys.getenv("NEWSAPI_KEY")

# Regles partagees : Pre-RSI (sens, poids, seuil), devises, IOSCO, %||% tolerant aux NA
source(if (file.exists("scripts/00_regles.R")) "scripts/00_regles.R" else "00_regles.R")

# Serie FRED (observations recentes, valeurs manquantes "." retirees).
# FRED n'accepte la cle qu'en parametre de requete : aucune erreur n'est
# affichee (l'URL contient la cle). Retourne NULL si indisponible.
get_fred_serie <- function(series_id, n = 60, fred_key = FRED_KEY) {
  if (!nzchar(fred_key)) return(NULL)
  rep <- tryCatch(GET("https://api.stlouisfed.org/fred/series/observations",
                      query = list(series_id = series_id, api_key = fred_key, file_type = "json",
                                   sort_order = "desc", limit = n),
                      timeout(20)), error = function(e) NULL)
  if (is.null(rep) || status_code(rep) != 200) return(NULL)
  o <- tryCatch(fromJSON(content(rep, as = "text", encoding = "UTF-8"))$observations,
                error = function(e) NULL)
  if (is.null(o) || NROW(o) == 0) return(NULL)
  o <- o[o$value != ".", c("date", "value"), drop = FALSE]
  o <- data.frame(date = as.Date(o$date), value = suppressWarnings(as.numeric(o$value)))
  o <- o[is.finite(o$value), , drop = FALSE]
  if (nrow(o) == 0) NULL else o[order(o$date, decreasing = TRUE), , drop = FALSE]
}

cat("=== RUBBERSIGNAL — Signaux faibles du",
    format(DATE_COLLECTE, "%d/%m/%Y"), "===\n\n")
cat("Modules actifs :\n")
cat("  M1 Météo         : automatique (Open-Meteo)\n")
cat("  M2 Devises       : automatique (ExchangeRate API)\n")
cat("  M3 Demande aval  :",
    if (nchar(FRED_KEY) > 0) "FRED auto (dont confiance industrielle OCDE)"
    else "INDISPONIBLE (clé FRED absente)", "\n")
cat("  M4 Stocks        : indisponible (aucune source alimentée)\n")
cat("  M5 Shipping      : WTI FRED + saisie datée (data/reference/fret.csv)\n")
cat("  M6 Terrain CI    : relevé daté (data/reference/terrain_ci.csv)\n")
cat("  M7 Géopolitique  : automatique (NewsAPI)\n\n")


# ══════════════════════════════════════════════════════════════
# MODULE 1 — MÉTÉO 4 ZONES PRODUCTRICES
# ══════════════════════════════════════════════════════════════

cat(strrep("─", 50), "\n")
cat("MODULE 1 — MÉTÉO ZONES PRODUCTRICES\n")
cat(strrep("─", 50), "\n\n")

zones_meteo <- tibble(
  pays          = c("Côte d'Ivoire", "Thaïlande",   "Malaisie",    "Indonésie"),
  ville         = c("Abidjan",       "Bangkok",      "Kuala Lumpur","Jakarta"),
  rang          = c("CI — Terrain",  "1er mondial",  "2ème mondial","3ème mondial"),
  lat           = c(5.36,            13.75,          3.14,          -6.21),
  lon           = c(-4.01,           100.52,         101.69,         106.85),
  pluie_opt_min = c(80,              100,            120,            100),
  pluie_opt_max = c(200,             250,            280,            260),
  poids_prod    = c(0.15,            0.35,           0.25,           0.25)
)

collecter_meteo <- function(lat, lon, pays, ville,
                            pluie_opt_min, pluie_opt_max) {
  cat(">> Météo", pays, "...\n")
  date_fin   <- DATE_COLLECTE - days(1)
  date_debut <- floor_date(DATE_COLLECTE, "month")
  if (as.numeric(date_fin - date_debut) < 5) {
    date_debut <- floor_date(DATE_COLLECTE - months(1), "month")
    date_fin   <- ceiling_date(DATE_COLLECTE - months(1), "month") - days(1)
  }
  url <- paste0(
    "https://archive-api.open-meteo.com/v1/archive?",
    "latitude=", lat, "&longitude=", lon,
    "&start_date=", format(date_debut, "%Y-%m-%d"),
    "&end_date=",   format(date_fin,   "%Y-%m-%d"),
    "&daily=precipitation_sum,temperature_2m_mean&timezone=auto"
  )
  rep <- tryCatch(GET(url, timeout(25)), error = function(e) NULL)
  if (is.null(rep) || status_code(rep) != 200) {
    cat("   Indisponible\n") ; return(NULL)
  }
  d     <- fromJSON(content(rep, as="text", encoding="UTF-8"))$daily
  pluie <- round(sum(d$precipitation_sum, na.rm=TRUE), 1)
  nb_j  <- length(d$precipitation_sum)
  p30   <- round(pluie * 30 / max(nb_j, 1), 1)
  temp  <- round(mean(d$temperature_2m_mean, na.rm=TRUE), 1)
  score <- if (p30 < pluie_opt_min)
    round(p30 / pluie_opt_min * 60)
  else if (p30 > pluie_opt_max)
    round(max(0, 100 - (p30 - pluie_opt_max) / pluie_opt_max * 40))
  else
    round(60 + (p30 - pluie_opt_min) /
            (pluie_opt_max - pluie_opt_min) * 40)
  score  <- min(100, max(0, score))
  signal <- case_when(score >= 75 ~ "Favorable",
                      score >= 50 ~ "Neutre",
                      score >= 25 ~ "Défavorable",
                      TRUE        ~ "Très défavorable")
  cat("   Pluie :", pluie, "mm →", p30, "mm/30j | Temp:",
      temp, "°C | Score:", score, "/100 →", signal, "\n")
  list(pays=pays, ville=ville, pluie_30j_mm=p30,
       temp_moyenne_c=temp, score_production=score,
       signal_production=signal)
}

meteo_resultats <- list()
for (i in 1:nrow(zones_meteo)) {
  r <- collecter_meteo(zones_meteo$lat[i], zones_meteo$lon[i],
                       zones_meteo$pays[i], zones_meteo$ville[i],
                       zones_meteo$pluie_opt_min[i],
                       zones_meteo$pluie_opt_max[i])
  if (!is.null(r)) meteo_resultats[[zones_meteo$pays[i]]] <- r
  Sys.sleep(1)
}

pays_ok  <- names(meteo_resultats)
poids_ok <- zones_meteo$poids_prod[zones_meteo$pays %in% pays_ok]
poids_n  <- poids_ok / sum(poids_ok)
scores_ok <- map_dbl(pays_ok, ~ meteo_resultats[[.x]]$score_production)
# v2.3 : sans aucune zone collectee, score NA (plus de 50 par defaut)
score_offre_mondiale <- if (length(scores_ok) > 0)
  round(sum(scores_ok * poids_n)) else NA_real_
if (is.na(score_offre_mondiale)) cat("::warning::Meteo : aucune zone collectee (Open-Meteo indisponible)\n")

signal_offre <- case_when(
  is.na(score_offre_mondiale) ~ "Données météo indisponibles",
  score_offre_mondiale >= 75 ~ "Offre abondante — pression baissière",
  score_offre_mondiale >= 50 ~ "Offre normale — marché équilibré",
  score_offre_mondiale >= 25 ~ "Offre sous pression — signal haussier",
  TRUE                       ~ "Offre perturbée — fort signal haussier"
)
cat("\n>> Score offre mondiale :", score_offre_mondiale,
    "/100 —", signal_offre, "\n\n")


# ══════════════════════════════════════════════════════════════
# MODULE 2 — DEVISES
# ══════════════════════════════════════════════════════════════

cat(strrep("─", 50), "\n")
cat("MODULE 2 — DEVISES\n")
cat(strrep("─", 50), "\n\n")

# v2.3 : plus AUCUN taux de secours (anciens 7.24 / 4.47 / 614 / 16250 / 0.90).
# Si l'API echoue ou qu'une devise manque : NA + statut + ::warning::.
collecter_devises <- function() {
  cat(">> Taux de change USD/...\n")
  url <- if (nchar(EXCHANGERATE_KEY) > 0)
    paste0("https://v6.exchangerate-api.com/v6/",
           EXCHANGERATE_KEY, "/latest/USD")
  else
    "https://open.er-api.com/v6/latest/USD"
  src <- if (nchar(EXCHANGERATE_KEY) > 0) "ExchangeRate API (v6)" else "ExchangeRate API (open.er-api)"
  rep <- tryCatch(GET(url, timeout(15)), error = function(e) NULL)   # erreur jamais affichee (URL avec cle)
  d <- if (is.null(rep) || status_code(rep) != 200) NULL else
    tryCatch(fromJSON(content(rep, as = "text", encoding = "UTF-8")), error = function(e) NULL)
  res <- rs_devises_lire(d, src)          # scripts/00_regles.R : NA + alertes, jamais de secours
  for (a in res$alertes) cat("::warning::", a, "\n", sep = "")
  cat("   CNY:", res$USD_CNY, "| MYR:", res$USD_MYR,
      "| XOF:", res$USD_XOF, "| CHF:", res$USD_CHF, "| maj", res$date %||% "inconnue",
      "| statut", res$statut, "\n")
  res
}
devises <- collecter_devises()

# Reference des signaux : moyenne des 12 derniers mois du taux FRED (H.10, Reserve
# federale) — remplace les references figees 7.10 / 4.40 (date et source inconnues).
ref_devise_12m <- function(series_id) {
  s <- get_fred_serie(series_id, n = 400)
  vide <- list(valeur = NA_real_, serie = series_id, source = paste0("FRED ", series_id),
               debut = NA_character_, fin = NA_character_, n_obs = 0L,
               methode = "Moyenne des cotations quotidiennes des 365 derniers jours")
  if (is.null(s)) return(vide)
  s <- s[s$date > DATE_COLLECTE - 365 & s$date <= DATE_COLLECTE, , drop = FALSE]
  if (nrow(s) < 150) return(vide)
  modifyList(vide, list(valeur = round(mean(s$value), 4), debut = as.character(min(s$date)),
                        fin = as.character(max(s$date)), n_obs = nrow(s)))
}
ref_cny <- ref_devise_12m("DEXCHUS")
ref_myr <- ref_devise_12m("DEXMAUS")
ecart_pct <- function(spot, ref) if (is.finite(spot) && is.finite(ref$valeur))
  round((spot - ref$valeur) / ref$valeur * 100, 2) else NA_real_
ecart_cny <- ecart_pct(devises$USD_CNY, ref_cny)
ecart_myr <- ecart_pct(devises$USD_MYR, ref_myr)
if (is.na(ecart_cny)) cat("::warning::Devises : ecart CNY non calculable (taux ou reference FRED DEXCHUS absent)\n")
signal_cny <- case_when(
  is.na(ecart_cny) ~ "Yuan : donnée indisponible",
  ecart_cny >  2 ~ paste0("Yuan faible (+", ecart_cny, "% vs moyenne 12 mois) — demande réduite"),
  ecart_cny < -2 ~ paste0("Yuan fort (", ecart_cny, "% vs moyenne 12 mois) — demande soutenue"),
  TRUE           ~ paste0("Yuan stable (", ecart_cny, "% vs moyenne 12 mois) — neutre"))
signal_myr <- case_when(
  is.na(ecart_myr) ~ "Ringgit : donnée indisponible",
  ecart_myr >  2 ~ paste0("Ringgit faible (+", ecart_myr, "% vs moyenne 12 mois) — export agressif"),
  ecart_myr < -2 ~ paste0("Ringgit fort (", ecart_myr, "% vs moyenne 12 mois) — export ralenti"),
  TRUE           ~ paste0("Ringgit stable (", ecart_myr, "% vs moyenne 12 mois) — neutre"))
cat("   Signal CNY :", signal_cny, "\n")
cat("   Signal MYR :", signal_myr, "\n\n")


# ══════════════════════════════════════════════════════════════
# MODULE 3 — DEMANDE AVAL (FRED + PMI MANUEL)
# ══════════════════════════════════════════════════════════════

cat(strrep("─", 50), "\n")
cat("MODULE 3 — DEMANDE AVAL\n")
cat(strrep("─", 50), "\n\n")

# Fonction utilitaire FRED — clé passée en paramètre
get_fred <- function(series_id, nom,
                     fred_key = Sys.getenv("FRED_KEY")) {
  url <- paste0(
    "https://api.stlouisfed.org/fred/series/observations?",
    "series_id=", series_id, "&api_key=", fred_key,
    "&limit=3&sort_order=desc&file_type=json"
  )
  rep <- tryCatch(GET(url, timeout(15)), error = function(e) NULL)
  if (is.null(rep) || status_code(rep) != 200) {
    cat(sprintf("   %-40s : inaccessible\n", nom))
    return(NULL)
  }
  obs <- fromJSON(content(rep, as="text", encoding="UTF-8"))$observations
  if (is.null(obs) || nrow(obs) == 0) return(NULL)
  val      <- as.numeric(obs$value[1])
  val_prec <- if (nrow(obs) >= 2) as.numeric(obs$value[2]) else NA
  variation <- if (!is.na(val_prec) && val_prec != 0)
    round((val - val_prec) / abs(val_prec) * 100, 2) else NA
  cat(sprintf("   %-40s : %14.2f (%s) [%s]\n",
              nom, val, if (is.finite(variation)) sprintf("%+.2f%%", variation) else "variation NA",
              obs$date[1]))
  list(serie=series_id, nom=nom, valeur=val,
       valeur_prec=val_prec, variation_pct=variation,
       date=obs$date[1])
}

collecter_demande_aval <- function() {
  
  cat(">> Indicateurs industriels FRED...\n")
  
  prod_manuf_us  <- get_fred("IPMAN",
                             "Production industrielle US manufacturing")
  Sys.sleep(0.3)
  ventes_auto_us <- get_fred("TOTALSA",
                             "Ventes automobiles US (millions/an)")
  Sys.sleep(0.3)
  commandes_dur  <- get_fred("DGORDER",
                             "Commandes biens durables US (M USD)")
  Sys.sleep(0.3)
  prod_indus_us  <- get_fred("INDPRO",
                             "Production industrielle US globale")
  Sys.sleep(0.3)
  imports_chine  <- get_fred("XTIMVA01CNM667S",
                             "Importations Chine (proxy demande)")
  Sys.sleep(0.3)
  permis_constr  <- get_fred("PERMIT",
                             "Permis construction US")
  Sys.sleep(0.3)
  
  # ── Confiance des industriels — OCDE, enquêtes de conjoncture (via FRED) ──
  # v2.3 : remplace le PMI saisi à la main (valeurs d'avril 2026 : 50.2 / 49.0 /
  # 51.1 / 58.8 / 48.7, sans date de mise à jour). Séries gratuites et
  # automatiques, identifiants vérifiés par l'API FRED le 03/11/2026 :
  #   BSCICP02USM460S  : BTS manufacturing, composite confidence, national indicator, USA (mensuel, % solde, CVS)
  #   BSCICP02DEM460S  : idem Allemagne (mensuel) — la série zone euro (BSCICP02EZM460S)
  #                      s'arrête en janvier 2026 : non retenue
  #   CHNBSCICP02STSAM : idem Chine (mensuel)
  #   JPNBSCICP02STSAQ : idem Japon (trimestriel, Tankan)
  #   Inde : aucune série à jour sur FRED -> non couverte.
  #   Les séries « BCI » amplitude-ajustées (BSCICP03*M665S) s'arrêtent en 2023-2024 : non retenues.
  # Méthode : les échelles diffèrent selon les pays ; chaque série est comparée à
  # SA PROPRE moyenne sur 5 ans : z = (dernière valeur - moyenne) / écart-type
  # (60 mois ou 20 trimestres). Une valeur n'est utilisée que si elle a moins de
  # 45 jours après la fin de sa période (règle mensuelle) ; sinon elle est
  # affichée « périmée » et exclue du score.

  cat("\n>> Confiance des industriels OCDE (FRED)...\n")

  CONFIANCE_SERIES <- tibble(
    pays      = c("USA", "Allemagne", "Chine", "Japon"),
    serie     = c("BSCICP02USM460S", "BSCICP02DEM460S", "CHNBSCICP02STSAM", "JPNBSCICP02STSAQ"),
    frequence = c("mensuelle", "mensuelle", "mensuelle", "trimestrielle")
  )

  confiance <- lapply(seq_len(nrow(CONFIANCE_SERIES)), function(i) {
    cs  <- CONFIANCE_SERIES[i, ]
    nb  <- if (cs$frequence == "mensuelle") 60 else 20
    s   <- get_fred_serie(cs$serie, n = nb)
    Sys.sleep(0.3)
    vide <- list(pays = cs$pays, serie = cs$serie, frequence = cs$frequence, valeur = NA_real_,
                 unite = "% (solde, CVS)", periode = NA_character_, date_source = NA_character_,
                 z_5ans = NA_real_, statut = "indisponible", utilise = FALSE,
                 source = paste0("OCDE, enquêtes de conjoncture, via FRED (", cs$serie, ")"),
                 signal = "Donnée indisponible")
    if (is.null(s) || nrow(s) < 8) {
      cat(sprintf("   %-10s : indisponible\n", cs$pays)); return(vide)
    }
    der  <- s$date[1]
    fin  <- if (cs$frequence == "mensuelle") seq(der, by = "1 month", length.out = 2)[2] - 1
            else seq(der, by = "3 months", length.out = 2)[2] - 1
    fr   <- rs_fraicheur(fin, DATE_COLLECTE, RS_MAX_JOURS_MENSUEL)
    z    <- if (sd(s$value) > 0) round((s$value[1] - mean(s$value)) / sd(s$value), 2) else 0
    sig  <- if (z > 0.5) "au-dessus de sa moyenne 5 ans" else if (z < -0.5) "en dessous de sa moyenne 5 ans"
            else "proche de sa moyenne 5 ans"
    res  <- modifyList(vide, list(valeur = s$value[1], periode = format(der, "%Y-%m"),
                                  date_source = as.character(der), z_5ans = z,
                                  statut = if (identical(fr$statut, "frais")) "derniere_valeur_disponible" else "perime",
                                  utilise = identical(fr$statut, "frais"),
                                  n_observations = nrow(s),
                                  signal = if (identical(fr$statut, "frais")) sig
                                           else paste0("périmée (dernière valeur ", format(der, "%m/%Y"), ")")))
    cat(sprintf("   %-10s : %6.1f (%s, z = %+.2f) %s\n", cs$pays, res$valeur, res$periode, z,
                if (res$utilise) "" else "[PÉRIMÉE, exclue]"))
    res
  })
  names(confiance) <- CONFIANCE_SERIES$pays
  z_utiles <- unlist(lapply(confiance, function(x) if (isTRUE(x$utilise)) x$z_5ans else NULL))
  z_moyen  <- if (length(z_utiles) > 0) round(mean(z_utiles), 2) else NA_real_

  # ── Score demande aval composite ───────────────────────────
  # Base 50 (neutre) + pas fixes par indicateur DISPONIBLE (seuils non backtestés) :
  #   ventes auto US (TOTALSA) var. mensuelle > +2 % : +10 ; < -2 % : -10
  #   commandes biens durables (DGORDER) > +3 % : +8 ; < -3 % : -8
  #   importations Chine (XTIMVA01CNM667S) > +5 % : +7 ; < -5 % : -7
  #   confiance industrielle OCDE, z moyen > +0.5 : +10 ; < -0.5 : -10
  # Aucun indicateur disponible -> score NA (plus de 50 par défaut).
  cat("\n>> Calcul score demande aval...\n")

  score_demande <- 50
  n_indicateurs <- 0
  pas <- function(x, seuil, points) {
    if (is.null(x) || !is.finite(x$variation_pct %||% NA)) return(0)
    n_indicateurs <<- n_indicateurs + 1
    if (x$variation_pct > seuil) points else if (x$variation_pct < -seuil) -points else 0
  }
  score_demande <- score_demande + pas(ventes_auto_us, 2, 10) + pas(commandes_dur, 3, 8) +
    pas(imports_chine, 5, 7)
  if (is.finite(z_moyen)) {
    n_indicateurs <- n_indicateurs + 1
    score_demande <- score_demande + (if (z_moyen > 0.5) 10 else if (z_moyen < -0.5) -10 else 0)
  }
  score_demande <- if (n_indicateurs == 0) NA_real_ else min(100, max(0, score_demande))
  if (is.na(score_demande)) cat("::warning::Demande aval : aucun indicateur disponible - score NA\n")

  signal_demande <- case_when(
    is.na(score_demande) ~ "Données de demande indisponibles",
    score_demande >= 70 ~ "Demande forte — signal haussier",
    score_demande >= 55 ~ "Demande modérée — bien orienté",
    score_demande >= 45 ~ "Demande neutre — équilibré",
    score_demande >= 30 ~ "Demande faible — signal baissier",
    TRUE                ~ "Demande très faible — fort signal baissier"
  )
  
  cat("   Score demande aval :", score_demande, "/100 —",
      signal_demande, "\n")
  
  list(
    fred = list(
      prod_manuf_us  = prod_manuf_us,
      ventes_auto_us = ventes_auto_us,
      commandes_dur  = commandes_dur,
      prod_indus_us  = prod_indus_us,
      imports_chine  = imports_chine,
      permis_constr  = permis_constr
    ),
    confiance_industrie = list(
      series  = confiance,
      z_moyen = z_moyen,
      n_utilisees = length(z_utiles),
      methode = paste("z = (dernière valeur - moyenne 5 ans) / écart-type 5 ans, par pays ;",
                      "moyenne des z des séries fraîches (< 45 jours après la fin de période)"),
      source  = "OCDE, Business Tendency Surveys (manufacturing), via FRED"
    ),
    pmi = list(statut = "retire",
               note = "PMI saisi à la main (avril 2026) retiré en v2.3 : remplacé par confiance_industrie (OCDE via FRED)"),
    score_demande  = score_demande,
    signal_demande = signal_demande,
    n_indicateurs  = n_indicateurs
  )
}

demande_aval <- collecter_demande_aval()
cat("\n")


# ══════════════════════════════════════════════════════════════
# MODULE 4 — STOCKS MONDIAUX
# ══════════════════════════════════════════════════════════════

cat(strrep("─", 50), "\n")
cat("MODULE 4 — STOCKS MONDIAUX\n")
cat(strrep("─", 50), "\n\n")

# ⚠ À mettre à jour mensuellement
# Sources :
# · Malaisie  : https://www.lgm.gov.my
# · Thaïlande : https://www.rubberthai.or.th
# · CI        : https://apromac.ci

# v2.2 : aucune source de stocks n'est alimentee depuis avril 2026.
# Plus aucune tendance « stable » inventee : statut « indisponible ».
# Quand une source sera trouvee : tonnage + mois + source + date_source.

stocks_manuels <- list(
  statut             = "indisponible",
  libelle            = "Donnée indisponible",
  malaisie_tonnes    = NA,
  thailande_tonnes   = NA,
  indonesie_tonnes   = NA,
  ci_tonnes          = NA,
  tendance           = NA,
  derniere_mise_a_jour_connue = NA,
  sources_a_explorer = c("https://www.lgm.gov.my", "https://www.rubberthai.or.th", "https://apromac.ci"),
  note = "Aucune donnée de stocks mesurée : module retiré du rapport (pas de tendance publiée)."
)

cat(">> Stocks : DONNÉE INDISPONIBLE (aucune source alimentée)\n")
cat("::warning::Stocks mondiaux : donnee indisponible (aucune source)\n")
cat("   · Malaisie  : https://www.lgm.gov.my\n")
cat("   · Thaïlande : https://www.rubberthai.or.th\n")
cat("   · CI        : https://apromac.ci\n\n")


# ══════════════════════════════════════════════════════════════
# MODULE 5 — SHIPPING / FRET MARITIME
# ══════════════════════════════════════════════════════════════

cat(strrep("─", 50), "\n")
cat("MODULE 5 — SHIPPING / FRET MARITIME\n")
cat(strrep("─", 50), "\n\n")

# v2.3 : plus de BDI « estimé » (1450), de tarif CI-UE (85 USD/t) ni de délai
# (14 j) codés en dur. Aucune source automatique gratuite du BDI : les valeurs
# sont SAISIES, DATÉES et SOURCÉES dans data/reference/fret.csv
#   date_valeur, indicateur, valeur, unite, source, url
#   indicateur : BDI | tarif_ci_ue_usd_t | delai_abidjan_rotterdam_j
# Seule une valeur récente est retenue (BDI : 14 jours ; tarif et délai : 45 jours),
# sinon statut « indisponible ». Le fret n'entre pas dans le Pré-RSI.

RS_FRET_MAX_JOURS <- c(BDI = 14, tarif_ci_ue_usd_t = 45, delai_abidjan_rotterdam_j = 45)

lire_fret <- function(f = "data/reference/fret.csv", date_ref = DATE_COLLECTE) {
  vide <- function(ind) list(indicateur = ind, valeur = NA_real_, unite = NA_character_,
                             date_valeur = NA_character_, source = NA_character_, url = NA_character_,
                             statut = "indisponible", max_jours = unname(RS_FRET_MAX_JOURS[ind]))
  out <- setNames(lapply(names(RS_FRET_MAX_JOURS), vide), names(RS_FRET_MAX_JOURS))
  if (!file.exists(f)) return(out)
  t <- utils::read.csv(f, stringsAsFactors = FALSE, colClasses = "character", encoding = "UTF-8")
  manque <- setdiff(c("date_valeur", "indicateur", "valeur", "unite", "source", "url"), names(t))
  if (length(manque) > 0) stop("Fret : colonnes manquantes dans ", f, " : ", paste(manque, collapse = ", "))
  if (nrow(t) == 0) return(out)
  t$d <- suppressWarnings(as.Date(t$date_valeur))
  t$v <- suppressWarnings(as.numeric(t$valeur))
  for (ind in names(RS_FRET_MAX_JOURS)) {
    age <- as.numeric(date_ref - t$d)
    ok  <- t$indicateur == ind & is.finite(t$v) & !is.na(age) & age >= 0 &
           age <= RS_FRET_MAX_JOURS[[ind]] & nzchar(trimws(t$source))
    if (!any(ok)) next
    r <- t[ok, , drop = FALSE]; r <- r[order(r$d, decreasing = TRUE)[1], ]
    out[[ind]] <- modifyList(out[[ind]], list(valeur = r$v, unite = r$unite, date_valeur = as.character(r$d),
                                              source = r$source, url = r$url, statut = "ok"))
  }
  out
}

collecter_shipping <- function() {
  cat(">> Prix pétrole WTI (FRED — proxy fret)...\n")

  wti_val <- NA_real_; wti_date <- NA_character_
  if (nchar(FRED_KEY) > 0) {
    wti_data <- get_fred("DCOILWTICO", "Pétrole WTI (USD/baril)")
    if (!is.null(wti_data)) { wti_val <- wti_data$valeur; wti_date <- wti_data$date }
  }

  fret <- lire_fret()
  bdi_val <- fret$BDI$valeur
  for (ind in names(fret))
    if (!identical(fret[[ind]]$statut, "ok"))
      cat("::warning::Fret : ", ind, " indisponible (aucune saisie datee recente dans data/reference/fret.csv)\n", sep = "")

  signal_fret <- case_when(
    is.na(bdi_val) ~ "Données indisponibles (aucune source datée)",
    bdi_val > 1800 ~ "Fret élevé — pression sur les marges export",
    bdi_val < 1200 ~ "Fret bas — export facilité",
    TRUE           ~ "Fret normal — pas de signal particulier"
  )

  cat("   BDI :", if (is.na(bdi_val)) "indisponible" else paste(bdi_val, "points"), "—", signal_fret, "\n")
  if (!is.na(wti_val))
    cat("   WTI :", wti_val, "USD/baril (", wti_date, ")\n")

  list(
    bdi_valeur              = bdi_val,
    wti_valeur              = wti_val,
    wti_date                = wti_date,
    signal_fret             = signal_fret,
    tarif_ci_eu_usd_tonne   = fret$tarif_ci_ue_usd_t$valeur,
    delai_abidjan_rotterdam = fret$delai_abidjan_rotterdam_j$valeur,
    statut_fret             = if (all(vapply(fret, function(x) identical(x$statut, "ok"), logical(1)))) "ok"
                              else "indisponible",
    fret                    = fret,
    fichier_saisie          = "data/reference/fret.csv",
    source = "WTI : FRED DCOILWTICO ; BDI, tarif et délai : saisie datée data/reference/fret.csv"
  )
}

shipping <- collecter_shipping()
cat("\n")


# ══════════════════════════════════════════════════════════════
# MODULE 6 — TERRAIN CI EXCLUSIF
# ══════════════════════════════════════════════════════════════

cat(strrep("─", 50), "\n")
cat("MODULE 6 — TERRAIN CI EXCLUSIF\n")
cat(strrep("─", 50), "\n\n")

# v2.2 : plus aucune valeur codée en dur (ancien 359 FCFA, texte identique
# chaque semaine, date de saisie = date du jour). Les relevés sont saisis,
# datés, dans data/reference/terrain_ci.csv :
#   date_releve, lieu, prix_marche_reel_fcfa, disponibilite_latex (1-5),
#   sentiment_planteurs (1-5), note_terrain, source
# Seul un relevé de moins de 7 jours est publié ; sinon « pas de relevé ».
# Le prix APROMAC officiel vient du script 01 (data/reference/prix_apromac.csv).

lire_terrain_ci <- function(f = "data/reference/terrain_ci.csv", date_ref = DATE_COLLECTE,
                            max_jours = 7) {
  absent <- list(statut = "absent", libelle = "Pas de relevé terrain cette semaine",
                 note_terrain = "", sentiment_planteurs = NA, disponibilite_latex = NA,
                 prix_marche_reel_fcfa = NA, date_releve = NA, lieu = NA, source = NA,
                 semaine = SEMAINE, fichier_saisie = f)
  if (!file.exists(f)) return(absent)
  t <- tryCatch(read.csv(f, stringsAsFactors = FALSE, encoding = "UTF-8"), error = function(e) data.frame())
  if (nrow(t) == 0 || !"date_releve" %in% names(t)) return(absent)
  t$date_releve <- suppressWarnings(as.Date(t$date_releve))
  age <- as.numeric(date_ref - t$date_releve)
  t <- t[!is.na(age) & age >= 0 & age <= max_jours, , drop = FALSE]
  if (nrow(t) == 0) return(absent)
  r <- t[order(t$date_releve, decreasing = TRUE)[1], ]
  num <- function(x, a, b) { v <- suppressWarnings(as.numeric(x)); if (length(v) == 1 && is.finite(v) && v >= a && v <= b) v else NA }
  list(statut = "ok", libelle = paste0("Relevé du ", format(r$date_releve, "%d/%m/%Y"), " (", r$lieu, ")"),
       note_terrain = if (is.na(r$note_terrain)) "" else as.character(r$note_terrain),
       sentiment_planteurs = num(r$sentiment_planteurs, 1, 5),
       disponibilite_latex = num(r$disponibilite_latex, 1, 5),
       prix_marche_reel_fcfa = num(r$prix_marche_reel_fcfa, 100, 2000),
       date_releve = as.character(r$date_releve), lieu = r$lieu, source = r$source,
       semaine = SEMAINE, fichier_saisie = f)
}

terrain_ci <- lire_terrain_ci()
# IOSCO : nature D (déclaration recueillie sur le terrain) + version de méthode
terrain_ci <- rs_iosco(c(terrain_ci, list(periode = terrain_ci$date_releve, date_source = terrain_ci$date_releve,
                                          unite = "sentiment 1-5", valeur = terrain_ci$sentiment_planteurs)),
                       "D", "terrain")

cat(">> Données terrain CI :", terrain_ci$libelle, "\n")
if (terrain_ci$statut == "ok") {
  cat("   Sentiment        :", terrain_ci$sentiment_planteurs, "/5\n")
  cat("   Note terrain     :", terrain_ci$note_terrain, "\n\n")
} else {
  cat("::warning::Terrain CI : pas de releve terrain date de moins de 7 jours\n\n")
}


# ══════════════════════════════════════════════════════════════
# MODULE 7 — GÉOPOLITIQUE / TENSIONS
# ══════════════════════════════════════════════════════════════

cat(strrep("─", 50), "\n")
cat("MODULE 7 — GÉOPOLITIQUE / TENSIONS\n")
cat(strrep("─", 50), "\n\n")

# v2.3 : clé NewsAPI en en-tête X-Api-Key (plus dans l'URL). Sans donnée
# (clé absente, API en erreur, 0 article pertinent) : nb_articles = NA et
# composante retirée du Pré-RSI (plus de « 0 article = aucune tension = 65/100 »).
collecter_geopolitique <- function() {
  cat(">> Actualités géopolitiques (NewsAPI)...\n")
  sans_donnee <- function(statut, signal) {
    cat("   ", signal, "\n", sep = "")
    cat("::warning::Geopolitique : ", statut, " - composante retiree du Pre-RSI\n", sep = "")
    list(nb_articles = NA_integer_, statut = statut, signal = signal, articles = list(),
         source = "NewsAPI (everything)", date_debut = format(DATE_COLLECTE - days(7), "%Y-%m-%d"),
         date_fin = as.character(DATE_COLLECTE))
  }

  if (nchar(NEWSAPI_KEY) == 0)
    return(sans_donnee("cle_absente", "Données indisponibles (clé NewsAPI absente)"))

  date_debut <- format(DATE_COLLECTE - days(7), "%Y-%m-%d")
  mots_geo   <- paste0(
    "rubber trade sanctions drought flood ",
    "Malaysia Thailand Indonesia strike ",
    "Ivory Coast political tension climate"
  )

  rep <- tryCatch(GET("https://newsapi.org/v2/everything",
                      query = list(q = mots_geo, language = "en", from = date_debut,
                                   sortBy = "relevancy", pageSize = 5),
                      add_headers(`X-Api-Key` = NEWSAPI_KEY),
                      timeout(15)),
                  error = function(e) NULL)

  if (is.null(rep) || status_code(rep) != 200)
    return(sans_donnee("api_indisponible",
                       paste0("Données indisponibles (NewsAPI, code ",
                              if (is.null(rep)) "aucune réponse" else status_code(rep), ")")))

  d <- tryCatch(
    fromJSON(content(rep, as="text", encoding="UTF-8"), flatten=TRUE),
    error = function(e) NULL
  )
  if (is.null(d)) return(sans_donnee("reponse_illisible", "Données indisponibles (réponse illisible)"))

  articles_bruts <- d$articles
  articles_df <- if (is.null(articles_bruts)) data.frame() else tryCatch(
    as.data.frame(articles_bruts, stringsAsFactors=FALSE),
    error = function(e) data.frame()
  )

  if (nrow(articles_df) == 0)
    return(sans_donnee("aucun_article", "Aucun article reçu cette semaine — donnée indisponible"))
  
  articles_df$titre  <- if ("title" %in% names(articles_df))
    articles_df$title else "Sans titre"
  articles_df$resume <- if ("description" %in% names(articles_df))
    replace(articles_df$description,
            is.na(articles_df$description), "")
  else ""
  
  mots_filtre <- c("rubber","caoutchouc","malaysia","thailand",
                   "indonesia","ivory coast","trade","sanctions",
                   "drought","flood","strike","tension")
  pattern <- paste(mots_filtre, collapse="|")
  idx     <- grepl(pattern, tolower(articles_df$titre)) |
    grepl(pattern, tolower(articles_df$resume))
  articles_filtres <- articles_df[idx, ]
  nb <- nrow(articles_filtres)

  cat("   OK —", nb, "actualités pertinentes\n")
  if (nb == 0)
    return(sans_donnee("aucun_article_pertinent",
                       "Aucun article pertinent cette semaine — donnée indisponible"))

  signal_geo <- case_when(
    nb <= 2 ~ "Tensions mineures à surveiller",
    nb <= 4 ~ "Tensions modérées — risque sur l'offre",
    TRUE    ~ "Tensions significatives — impact potentiel"
  )
  cat("   Signal :", signal_geo, "\n")
  
  if (nb > 0) {
    for (i in 1:min(3, nb))
      cat("  ", i, ".", substr(articles_filtres$titre[i], 1, 65), "\n")
  }
  
  articles_liste <- if (nb > 0) {
    lapply(1:nb, function(i) list(
      titre  = articles_filtres$titre[i],
      resume = articles_filtres$resume[i],
      url    = if ("url" %in% names(articles_filtres))
        articles_filtres$url[i] else "",
      date   = if ("publishedAt" %in% names(articles_filtres))
        articles_filtres$publishedAt[i] else ""
    ))
  } else list()
  
  list(nb_articles = nb, statut = "ok", signal = signal_geo, articles = articles_liste,
       source = "NewsAPI (everything)", date_debut = date_debut, date_fin = as.character(DATE_COLLECTE))
}

geopolitique <- collecter_geopolitique()
cat("\n")


# ══════════════════════════════════════════════════════════════
# PRÉ-RSI — Score composite préliminaire
# ══════════════════════════════════════════════════════════════

cat(strrep("─", 50), "\n")
cat("PRÉ-RSI — Score composite préliminaire\n")
cat(strrep("─", 50), "\n\n")

# v2 (mission 2) : sens, poids, renormalisation et seuil définis et documentés
# dans scripts/00_regles.R (section 9). Chaque composante est un score 0-100
# ORIENTÉ PRIX (> 55 haussier, < 45 baissier).
#   Composante      Poids  Sens / transformation
#   Météo / offre    25 %  inversé : offre abondante = baissier (classes 30/50/70/85)
#   Devises (yuan)   20 %  yuan fort vs moyenne 12 mois = haussier (70/50/30)
#   Demande aval     25 %  score demande tel quel (élevé = haussier)
#   Terrain CI       15 %  sentiment 1-5 -> (s - 1) / 4 x 100 (3/5 = 50)
#   Géopolitique     15 %  tensions sur l'offre = haussier ; 0 article = NA
# Composante absente (NA) : retirée, poids des autres renormalisés à 100 %.
# Moins de 3 composantes disponibles : Pré-RSI NON PUBLIÉ (score NA).

terrain_dispo <- identical(terrain_ci$statut, "ok") && is.finite(terrain_ci$sentiment_planteurs)
composantes_rsi <- c(
  meteo   = rs_pre_rsi_meteo(score_offre_mondiale),
  devises = rs_pre_rsi_devise(ecart_cny),
  demande = rs_pre_rsi_demande(demande_aval$score_demande),
  terrain = if (terrain_dispo) rs_pre_rsi_terrain(terrain_ci$sentiment_planteurs) else NA_real_,
  geo     = rs_pre_rsi_geo(geopolitique$nb_articles)
)
res_rsi    <- rs_pre_rsi(composantes_rsi)
pre_rsi    <- res_rsi$score
signal_rsi <- res_rsi$signal
if (!rs_pre_rsi_coherent(res_rsi)) stop("Pre-RSI incoherent (libelle / score / composantes) : arret volontaire")
if (!res_rsi$publie)
  cat("::warning::Pre-RSI non publie : ", res_rsi$raison_non_publie, "\n", sep = "")
if (length(res_rsi$composantes_absentes) > 0)
  cat("   Composante(s) absente(s) :", paste(res_rsi$composantes_absentes, collapse = ", "),
      "-> poids renormalisés\n")

cat("PRÉ-RSI Semaine", SEMAINE, ":", if (is.na(pre_rsi)) "NON PUBLIÉ" else paste(pre_rsi, "/100"), "\n")
cat("Signal :", signal_rsi, "\n\n")
cat("Composantes (score orienté prix x poids effectif) :\n")
for (k in names(RS_PRE_RSI_POIDS))
  cat(sprintf("  %-14s nominal %2.0f%% | effectif %5.1f%% | score %s | sens %s\n", k,
              RS_PRE_RSI_POIDS[[k]] * 100, res_rsi$poids_effectifs[[k]] * 100,
              if (is.finite(composantes_rsi[[k]])) sprintf("%3.0f", composantes_rsi[[k]]) else " NA",
              res_rsi$sens[[k]] %||% "absente"))


# ══════════════════════════════════════════════════════════════
# ASSEMBLER ET SAUVEGARDER
# ══════════════════════════════════════════════════════════════

cat("\n>> Assemblage JSON signaux...\n")

json_signaux <- list(
  meta = list(
    date_collecte = as.character(DATE_COLLECTE),
    semaine       = SEMAINE,
    annee         = ANNEE,
    version       = "2.3"
  ),
  module1_meteo        = list(zones=meteo_resultats,
                              score_offre_mondiale=score_offre_mondiale,
                              signal_offre=signal_offre,
                              source = "Open-Meteo (archive), cumul de pluie du mois",
                              date_source = as.character(DATE_COLLECTE - 1)),
  module2_devises      = c(devises, list(signal_cny=signal_cny,
                                         signal_myr=signal_myr,
                                         ecart_cny_pct = ecart_cny, ecart_myr_pct = ecart_myr,
                                         reference_cny = ref_cny, reference_myr = ref_myr)),
  module3_demande_aval = demande_aval,
  module4_stocks       = stocks_manuels,
  module5_shipping     = shipping,
  module6_terrain_ci   = terrain_ci,
  module7_geopolitique = geopolitique,
  pre_rsi = rs_iosco(c(res_rsi, list(
    valeur      = pre_rsi,
    unite       = "score 0-100 (> 55 haussier, < 45 baissier)",
    periode     = format(DATE_COLLECTE, "%G-W%V"),
    source      = "Calcul RubberSignal (modules 1, 2, 3, 6, 7 de ce fichier)",
    date_source = as.character(DATE_COLLECTE),
    publiable   = rs_serie_publiable("PRE_RSI"),
    methode = paste("Somme pondérée des composantes disponibles (scores orientés prix), divisée par la",
                    "somme de leurs poids ; poids nominaux météo 25 %, devises 20 %, demande 25 %,",
                    "terrain 15 %, géopolitique 15 % (non backtestés) ; non publié sous 3 composantes.",
                    "Analyse, pas un indice de prix.")
  )), "E", "pre_rsi")
)

fichier_signaux <- paste0(
  "data/processed/signaux_S", SEMAINE, "_", ANNEE, ".json"
)
write_json(json_signaux, fichier_signaux, pretty=TRUE, auto_unbox=TRUE,
           na="null", null="null")
cat("   JSON signaux :", fichier_signaux, "\n")

fichier_json <- paste0(
  "data/processed/rubbersignal_S", SEMAINE, "_", ANNEE, ".json"
)
if (file.exists(fichier_json)) {
  jp <- read_json(fichier_json)
  jp$signaux_faibles <- json_signaux
  write_json(jp, fichier_json, pretty=TRUE, auto_unbox=TRUE, na="null", null="null")
  cat("   JSON principal enrichi\n")
}


# ══════════════════════════════════════════════════════════════
# RÉSUMÉ FINAL
# ══════════════════════════════════════════════════════════════

cat("\n", strrep("=", 60), "\n")
cat("RÉSUMÉ SIGNAUX FAIBLES — Semaine", SEMAINE, "/", ANNEE, "\n")
cat(strrep("=", 60), "\n\n")

cat("M1 MÉTÉO OFFRE :", score_offre_mondiale, "/100 —",
    signal_offre, "\n")
for (p in names(meteo_resultats)) {
  m <- meteo_resultats[[p]]
  cat(sprintf("   %-20s Score %3d/100 | %s\n",
              m$pays, m$score_production, m$signal_production))
}

cat("\nM2 DEVISES :\n")
cat("   CNY:", devises$USD_CNY, "—", signal_cny, "\n")
cat("   MYR:", devises$USD_MYR, "—", signal_myr, "\n")
cat("   CHF:", devises$USD_CHF, "\n")

cat("\nM3 DEMANDE AVAL : Score", demande_aval$score_demande,
    "/100 —", demande_aval$signal_demande, "\n")
cat("   Confiance industrielle OCDE (z vs moyenne 5 ans) :\n")
for (p in names(demande_aval$confiance_industrie$series)) {
  x <- demande_aval$confiance_industrie$series[[p]]
  cat(sprintf("   %-10s : %s — %s\n", p,
              if (is.finite(x$valeur)) paste0(x$valeur, " (", x$periode, ")") else "NA", x$signal))
}

cat("\nM4 STOCKS : donnée indisponible (aucune source)\n")

cat("\nM5 SHIPPING : Signal —", shipping$signal_fret, "\n")
if (!is.na(shipping$wti_valeur))
  cat("   WTI :", shipping$wti_valeur, "USD/baril\n")

cat("\nM6 TERRAIN CI :", terrain_ci$libelle, "\n")
if (terrain_dispo) {
  cat("   Sentiment    :", terrain_ci$sentiment_planteurs, "/5\n")
  cat("   Note         :", terrain_ci$note_terrain, "\n")
}

cat("\nM7 GÉOPOLITIQUE :", geopolitique$signal, "\n")

cat("\n", strrep("★", 30), "\n")
cat("PRÉ-RSI SEMAINE", SEMAINE, ":", if (is.na(pre_rsi)) "NON PUBLIÉ" else paste(pre_rsi, "/100"), "\n")
cat("SIGNAL :", signal_rsi, "\n")
cat(strrep("★", 30), "\n\n")

cat("Fichier signaux :", fichier_signaux, "\n")
cat("\nProchaine étape : intégrer les signaux dans script 03\n\n")
cat(">> SAISIES DATÉES (data/reference/) :\n")
cat("   · fret.csv        : BDI, tarif CI-UE, délai Abidjan-Rotterdam (avec source)\n")
cat("   · terrain_ci.csv  : relevé terrain de la semaine\n")
cat("   · prix_apromac.csv: prix APROMAC du mois\n\n")