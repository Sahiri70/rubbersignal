# =============================================================
# RUBBERSIGNAL.COM — Script 04 : Signaux faibles complets V2
# Auteur  : Martial Sahiri
# Version : 2.1 — FRED fonctionnel + PMI manuel + Pré-RSI corrigé
# Modules :
#   1. Météo 4 zones productrices (Open-Meteo — automatique)
#   2. Devises (ExchangeRate API — automatique)
#   3. Demande aval (FRED automatique + PMI manuel mensuel)
#   4. Stocks mondiaux (manuel mensuel)
#   5. Shipping / Fret maritime (BDI + WTI FRED)
#   6. Terrain CI exclusif (saisie manuelle hebdomadaire)
#   7. Géopolitique / Tensions (NewsAPI — automatique)
#   + Pré-RSI composite 0-100
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

cat("=== RUBBERSIGNAL — Signaux faibles du",
    format(DATE_COLLECTE, "%d/%m/%Y"), "===\n\n")
cat("Modules actifs :\n")
cat("  M1 Météo         : automatique (Open-Meteo)\n")
cat("  M2 Devises       : automatique (ExchangeRate API)\n")
cat("  M3 Demande aval  :",
    if (nchar(FRED_KEY) > 0) "FRED auto + PMI manuel"
    else "PMI manuel uniquement", "\n")
cat("  M4 Stocks        : saisie manuelle mensuelle\n")
cat("  M5 Shipping      : automatique (WTI FRED)\n")
cat("  M6 Terrain CI    : saisie manuelle hebdomadaire\n")
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
score_offre_mondiale <- if (length(scores_ok) > 0)
  round(sum(scores_ok * poids_n)) else 50

signal_offre <- case_when(
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

collecter_devises <- function() {
  cat(">> Taux de change USD/...\n")
  url <- if (nchar(EXCHANGERATE_KEY) > 0)
    paste0("https://v6.exchangerate-api.com/v6/",
           EXCHANGERATE_KEY, "/latest/USD")
  else
    "https://open.er-api.com/v6/latest/USD"
  rep <- tryCatch(GET(url, timeout(15)), error = function(e) NULL)
  if (is.null(rep) || status_code(rep) != 200) {
    cat("   Secours\n")
    return(list(USD_CNY=7.24, USD_MYR=4.47, USD_XOF=614,
                USD_IDR=16250, USD_CHF=0.90, source="Secours"))
  }
  d <- fromJSON(content(rep, as="text", encoding="UTF-8"))
  r <- d$conversion_rates %||% d$rates
  if (is.null(r)) return(list(USD_CNY=7.24, USD_MYR=4.47,
                              USD_XOF=614, USD_IDR=16250,
                              USD_CHF=0.90, source="Secours"))
  res <- list(
    USD_CNY = round(r$CNY %||% 7.24, 4),
    USD_MYR = round(r$MYR %||% 4.47, 4),
    USD_XOF = round(r$XOF %||% 614,  2),
    USD_IDR = round(r$IDR %||% 16250, 0),
    USD_CHF = round(r$CHF %||% 0.90, 4),
    source  = "ExchangeRate API",
    date    = as.character(DATE_COLLECTE)
  )
  cat("   CNY:", res$USD_CNY, "| MYR:", res$USD_MYR,
      "| XOF:", res$USD_XOF, "| CHF:", res$USD_CHF, "\n")
  res
}
devises <- collecter_devises()

REF_CNY <- 7.10 ; REF_MYR <- 4.40
ecart_cny <- round((devises$USD_CNY - REF_CNY) / REF_CNY * 100, 2)
ecart_myr <- round((devises$USD_MYR - REF_MYR) / REF_MYR * 100, 2)
signal_cny <- case_when(
  ecart_cny >  2 ~ paste0("Yuan faible (+", ecart_cny, "%) — demande réduite"),
  ecart_cny < -2 ~ paste0("Yuan fort (", ecart_cny, "%) — demande soutenue"),
  TRUE           ~ paste0("Yuan stable (", ecart_cny, "%) — neutre"))
signal_myr <- case_when(
  ecart_myr >  2 ~ paste0("Ringgit faible (+", ecart_myr, "%) — export agressif"),
  ecart_myr < -2 ~ paste0("Ringgit fort (", ecart_myr, "%) — export ralenti"),
  TRUE           ~ paste0("Ringgit stable (", ecart_myr, "%) — neutre"))
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
  cat(sprintf("   %-40s : %14.2f (%+.2f%%)\n",
              nom, val, variation %||% 0))
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
  
  # ── PMI Manufacturing — saisie manuelle mensuelle ──────────
  # ⚠ À mettre à jour chaque 1er du mois
  # Sources : https://www.spglobal.com/marketintelligence/en/mi/research-analysis/pmi.html
  #           https://tradingeconomics.com/calendar (filtre PMI Manufacturing)
  
  cat("\n>> PMI Manufacturing (saisie manuelle — avril 2026)...\n")
  
  pmi_resultats <- list(
    USA    = list(pays="USA",    valeur=50.2,
                  signal="Légère expansion"),
    Europe = list(pays="Europe", valeur=49.0,
                  signal="Légère contraction"),
    Chine  = list(pays="Chine",  valeur=51.1,
                  signal="Légère expansion"),
    Inde   = list(pays="Inde",   valeur=58.8,
                  signal="Forte expansion"),
    Japon  = list(pays="Japon",  valeur=48.7,
                  signal="Légère contraction")
    # ← METTRE À JOUR MENSUELLEMENT
  )
  
  for (p in names(pmi_resultats)) {
    cat(sprintf("   PMI %-8s : %4.1f — %s\n",
                p,
                pmi_resultats[[p]]$valeur,
                pmi_resultats[[p]]$signal))
  }
  
  # ── Score demande aval composite ───────────────────────────
  cat("\n>> Calcul score demande aval...\n")
  
  score_demande <- 50
  
  if (!is.null(ventes_auto_us) &&
      !is.na(ventes_auto_us$variation_pct)) {
    if (ventes_auto_us$variation_pct > 2)
      score_demande <- score_demande + 10
    else if (ventes_auto_us$variation_pct < -2)
      score_demande <- score_demande - 10
  }
  
  if (!is.null(commandes_dur) &&
      !is.na(commandes_dur$variation_pct)) {
    if (commandes_dur$variation_pct > 3)
      score_demande <- score_demande + 8
    else if (commandes_dur$variation_pct < -3)
      score_demande <- score_demande - 8
  }
  
  if (!is.null(imports_chine) &&
      !is.na(imports_chine$variation_pct)) {
    if (imports_chine$variation_pct > 5)
      score_demande <- score_demande + 7
    else if (imports_chine$variation_pct < -5)
      score_demande <- score_demande - 7
  }
  
  # PMI moyen
  pmi_vals <- map_dbl(pmi_resultats, ~ .x$valeur %||% NA_real_)
  pmi_vals <- pmi_vals[!is.na(pmi_vals)]
  if (length(pmi_vals) > 0) {
    pmi_moy <- mean(pmi_vals)
    if (pmi_moy > 52)      score_demande <- score_demande + 10
    else if (pmi_moy < 49) score_demande <- score_demande - 10
  }
  
  score_demande <- min(100, max(0, score_demande))
  
  signal_demande <- case_when(
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
    pmi            = pmi_resultats,
    score_demande  = score_demande,
    signal_demande = signal_demande
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

stocks_manuels <- list(
  malaisie_tonnes    = NA,      # ← Mettre à jour
  malaisie_mois      = "2026-04",
  malaisie_tendance  = "stable",
  thailande_tonnes   = NA,      # ← Mettre à jour
  thailande_mois     = "2026-04",
  thailande_tendance = "stable",
  indonesie_tonnes   = NA,      # ← Mettre à jour
  indonesie_mois     = "2026-04",
  indonesie_tendance = "stable",
  ci_tonnes          = NA,      # ← Mettre à jour
  ci_mois            = "2026-04",
  ci_tendance        = "stable",
  abidjan_delai_jours = 7,
  abidjan_statut      = "Normal",
  note = "Données à compléter via lgm.gov.my et rubberthai.or.th"
)

cat(">> Stocks configurés — données à compléter mensuellement\n")
cat("   · Malaisie  : https://www.lgm.gov.my\n")
cat("   · Thaïlande : https://www.rubberthai.or.th\n")
cat("   · CI        : https://apromac.ci\n\n")


# ══════════════════════════════════════════════════════════════
# MODULE 5 — SHIPPING / FRET MARITIME
# ══════════════════════════════════════════════════════════════

cat(strrep("─", 50), "\n")
cat("MODULE 5 — SHIPPING / FRET MARITIME\n")
cat(strrep("─", 50), "\n\n")

collecter_shipping <- function() {
  cat(">> Prix pétrole WTI (FRED — proxy fret)...\n")
  
  wti_val <- NA
  if (nchar(FRED_KEY) > 0) {
    wti_data <- get_fred("DCOILWTICO", "Pétrole WTI (USD/baril)")
    if (!is.null(wti_data)) wti_val <- wti_data$valeur
  }
  
  # BDI estimé (données de secours — à enrichir avec abonnement)
  bdi_val <- 1450
  
  signal_fret <- case_when(
    is.na(bdi_val) ~ "Données indisponibles",
    bdi_val > 1800 ~ "Fret élevé — pression sur les marges export",
    bdi_val < 1200 ~ "Fret bas — export facilité",
    TRUE           ~ "Fret normal — pas de signal particulier"
  )
  
  cat("   BDI estimé :", bdi_val, "points —", signal_fret, "\n")
  if (!is.na(wti_val))
    cat("   WTI :", wti_val, "USD/baril\n")
  
  list(
    bdi_valeur            = bdi_val,
    wti_valeur            = wti_val,
    signal_fret           = signal_fret,
    tarif_ci_eu_usd_tonne = 85,    # ← À mettre à jour mensuellement
    delai_abidjan_rotterdam = 14,
    source = "BDI estimé + WTI FRED"
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

# ⚠ À COMPLÉTER CHAQUE SEMAINE — votre avantage concurrentiel

terrain_ci <- list(
  prix_bord_champ_fcfa   = 359,    # ← Prix APROMAC officiel
  prix_marche_reel_fcfa  = NA,     # ← Prix réel observé (si différent)
  disponibilite_latex    = 3,      # ← 1 à 5 (1=pénurie, 5=abondant)
  statut_disponibilite   = "Normal",
  activite_cooperatives  = "Normale",
  sentiment_planteurs    = 3,      # ← 1 à 5
  note_terrain = "Campagne de récolte en cours. Conditions normales.",
  tension_marche_local   = "Aucune",
  saison_saignee         = "Grande saison",
  semaine      = SEMAINE,
  date_saisie  = as.character(DATE_COLLECTE),
  source       = "Terrain CI — RubberSignal exclusif"
)

cat(">> Données terrain CI :\n")
cat("   Prix APROMAC     :", terrain_ci$prix_bord_champ_fcfa, "FCFA/kg\n")
cat("   Disponibilité    :", terrain_ci$statut_disponibilite, "\n")
cat("   Coopératives     :", terrain_ci$activite_cooperatives, "\n")
cat("   Sentiment        :", terrain_ci$sentiment_planteurs, "/5\n")
cat("   Saison           :", terrain_ci$saison_saignee, "\n")
cat("   Note terrain     :", terrain_ci$note_terrain, "\n\n")


# ══════════════════════════════════════════════════════════════
# MODULE 7 — GÉOPOLITIQUE / TENSIONS
# ══════════════════════════════════════════════════════════════

cat(strrep("─", 50), "\n")
cat("MODULE 7 — GÉOPOLITIQUE / TENSIONS\n")
cat(strrep("─", 50), "\n\n")

collecter_geopolitique <- function() {
  cat(">> Actualités géopolitiques (NewsAPI)...\n")
  
  if (nchar(NEWSAPI_KEY) == 0) {
    cat("   Clé NewsAPI manquante\n")
    return(list(nb_articles=0,
                signal="Données indisponibles",
                articles=list()))
  }
  
  date_debut <- format(DATE_COLLECTE - days(7), "%Y-%m-%d")
  mots_geo   <- paste0(
    "rubber trade sanctions drought flood ",
    "Malaysia Thailand Indonesia strike ",
    "Ivory Coast political tension climate"
  )
  
  url_geo <- paste0(
    "https://newsapi.org/v2/everything?",
    "q=", URLencode(mots_geo, reserved=TRUE),
    "&language=en&from=", date_debut,
    "&sortBy=relevancy&pageSize=5",
    "&apiKey=", NEWSAPI_KEY
  )
  
  rep <- tryCatch(GET(url_geo, timeout(15)),
                  error = function(e) NULL)
  
  if (is.null(rep) || status_code(rep) != 200) {
    return(list(nb_articles=0,
                signal="Données indisponibles",
                articles=list()))
  }
  
  d <- tryCatch(
    fromJSON(content(rep, as="text", encoding="UTF-8"), flatten=TRUE),
    error = function(e) NULL
  )
  if (is.null(d)) {
    return(list(nb_articles=0, signal="Erreur parsing",
                articles=list()))
  }
  
  articles_bruts <- d$articles
  if (is.null(articles_bruts)) {
    cat("   Aucune actualité\n")
    return(list(nb_articles=0,
                signal="Aucune tension détectée",
                articles=list()))
  }
  
  articles_df <- tryCatch(
    as.data.frame(articles_bruts, stringsAsFactors=FALSE),
    error = function(e) data.frame()
  )
  
  if (nrow(articles_df) == 0) {
    return(list(nb_articles=0,
                signal="Aucune tension détectée",
                articles=list()))
  }
  
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
  
  signal_geo <- case_when(
    nb == 0 ~ "Aucune tension détectée — marché stable",
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
  
  list(nb_articles=nb, signal=signal_geo, articles=articles_liste)
}

geopolitique <- collecter_geopolitique()
cat("\n")


# ══════════════════════════════════════════════════════════════
# PRÉ-RSI — Score composite préliminaire
# ══════════════════════════════════════════════════════════════

cat(strrep("─", 50), "\n")
cat("PRÉ-RSI — Score composite préliminaire\n")
cat(strrep("─", 50), "\n\n")

# Composante 1 — Météo/Offre (poids 25%)
score_meteo_w <- score_offre_mondiale * 0.25

# Composante 2 — Devises (poids 20%)
score_devise_w <- (if (!is.na(ecart_cny) && ecart_cny < -2) 70
                   else if (!is.na(ecart_cny) && ecart_cny > 2) 30
                   else 50) * 0.20

# Composante 3 — Demande aval (poids 25%)
# Utilise le score calculé dans le module 3
score_pmi_w <- demande_aval$score_demande * 0.25

# Composante 4 — Terrain CI (poids 15%)
score_terrain_w <- (terrain_ci$sentiment_planteurs / 5 * 100) * 0.15

# Composante 5 — Géopolitique (poids 15%)
score_geo_w <- (if (geopolitique$nb_articles == 0) 65
                else if (geopolitique$nb_articles <= 2) 50
                else if (geopolitique$nb_articles <= 4) 35
                else 20) * 0.15

pre_rsi <- round(score_meteo_w + score_devise_w + score_pmi_w +
                   score_terrain_w + score_geo_w)
pre_rsi <- min(100, max(0, pre_rsi))

signal_rsi <- case_when(
  pre_rsi >= 70 ~ "Haussier — conditions favorables",
  pre_rsi >= 55 ~ "Légèrement haussier — marché bien orienté",
  pre_rsi >= 45 ~ "Neutre — marché équilibré",
  pre_rsi >= 30 ~ "Légèrement baissier — pression sur les prix",
  TRUE          ~ "Baissier — conditions défavorables"
)

cat("PRÉ-RSI Semaine", SEMAINE, ":", pre_rsi, "/100\n")
cat("Signal :", signal_rsi, "\n\n")
cat("Composantes :\n")
cat(sprintf("  Météo/Offre  (25%%) : %4.1f pts\n", score_meteo_w))
cat(sprintf("  Devises      (20%%) : %4.1f pts\n", score_devise_w))
cat(sprintf("  Demande aval (25%%) : %4.1f pts\n", score_pmi_w))
cat(sprintf("  Terrain CI   (15%%) : %4.1f pts\n", score_terrain_w))
cat(sprintf("  Géopolitique (15%%) : %4.1f pts\n", score_geo_w))


# ══════════════════════════════════════════════════════════════
# ASSEMBLER ET SAUVEGARDER
# ══════════════════════════════════════════════════════════════

cat("\n>> Assemblage JSON signaux...\n")

json_signaux <- list(
  meta = list(
    date_collecte = as.character(DATE_COLLECTE),
    semaine       = SEMAINE,
    annee         = ANNEE,
    version       = "2.1"
  ),
  module1_meteo        = list(zones=meteo_resultats,
                              score_offre_mondiale=score_offre_mondiale,
                              signal_offre=signal_offre),
  module2_devises      = c(devises, list(signal_cny=signal_cny,
                                         signal_myr=signal_myr)),
  module3_demande_aval = demande_aval,
  module4_stocks       = stocks_manuels,
  module5_shipping     = shipping,
  module6_terrain_ci   = terrain_ci,
  module7_geopolitique = geopolitique,
  pre_rsi = list(
    score  = pre_rsi,
    signal = signal_rsi,
    detail = list(
      meteo_w   = score_meteo_w,
      devise_w  = score_devise_w,
      pmi_w     = score_pmi_w,
      terrain_w = score_terrain_w,
      geo_w     = score_geo_w
    )
  )
)

fichier_signaux <- paste0(
  "data/processed/signaux_S", SEMAINE, "_", ANNEE, ".json"
)
write_json(json_signaux, fichier_signaux, pretty=TRUE, auto_unbox=TRUE)
cat("   JSON signaux :", fichier_signaux, "\n")

fichier_json <- paste0(
  "data/processed/rubbersignal_S", SEMAINE, "_", ANNEE, ".json"
)
if (file.exists(fichier_json)) {
  jp <- read_json(fichier_json)
  jp$signaux_faibles <- json_signaux
  write_json(jp, fichier_json, pretty=TRUE, auto_unbox=TRUE)
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
cat("   PMI Manufacturing :\n")
for (p in names(demande_aval$pmi)) {
  cat(sprintf("   %-8s : %4.1f — %s\n",
              p, demande_aval$pmi[[p]]$valeur,
              demande_aval$pmi[[p]]$signal))
}

cat("\nM4 STOCKS : données à compléter mensuellement\n")

cat("\nM5 SHIPPING : Signal —", shipping$signal_fret, "\n")
if (!is.na(shipping$wti_valeur))
  cat("   WTI :", shipping$wti_valeur, "USD/baril\n")

cat("\nM6 TERRAIN CI :\n")
cat("   Prix APROMAC :", terrain_ci$prix_bord_champ_fcfa, "FCFA/kg\n")
cat("   Sentiment    :", terrain_ci$sentiment_planteurs, "/5\n")
cat("   Note         :", terrain_ci$note_terrain, "\n")

cat("\nM7 GÉOPOLITIQUE :", geopolitique$signal, "\n")

cat("\n", strrep("★", 30), "\n")
cat("PRÉ-RSI SEMAINE", SEMAINE, ":", pre_rsi, "/100\n")
cat("SIGNAL :", signal_rsi, "\n")
cat(strrep("★", 30), "\n\n")

cat("Fichier signaux :", fichier_signaux, "\n")
cat("\nProchaine étape : intégrer les signaux dans script 03\n\n")
cat(">> RAPPELS MENSUELS :\n")
cat("   · Mettre à jour PMI dans Module 3\n")
cat("   · Mettre à jour stocks dans Module 4\n")
cat("   · Vérifier prix APROMAC dans Module 6\n\n")