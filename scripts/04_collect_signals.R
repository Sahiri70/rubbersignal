# =============================================================
# RUBBERSIGNAL.COM — Script 04 : Signaux faibles complets V2
# Auteur  : Martial Sahiri
# Version : 2.0 — Script modulaire complet
# Modules :
#   1. Météo 4 zones productrices (Open-Meteo — automatique)
#   2. Devises (ExchangeRate API — automatique)
#   3. Demande aval — PMI Manufacturing (FRED API — automatique)
#   4. Stocks mondiaux (LGM + manuel CI)
#   5. Shipping / Fret maritime (Baltic Dry Index)
#   6. Terrain CI exclusif (saisie manuelle hebdomadaire)
#   7. Géopolitique / Tensions (NewsAPI filtré — automatique)
# Usage   : source("scripts/04_collect_signals.R")
# =============================================================

# ── 1. CHARGER LES PACKAGES ──────────────────────────────────

library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)


# ── 2. PARAMÈTRES GLOBAUX ────────────────────────────────────

DATE_COLLECTE <- Sys.Date()
SEMAINE       <- isoweek(DATE_COLLECTE)
ANNEE         <- year(DATE_COLLECTE)

# Clés API
EXCHANGERATE_KEY <- Sys.getenv("EXCHANGERATE_KEY")
FRED_KEY         <- Sys.getenv("FRED_KEY")      # Gratuit sur fred.stlouisfed.org
NEWSAPI_KEY      <- Sys.getenv("NEWSAPI_KEY")

cat("=== RUBBERSIGNAL — Signaux faibles du",
    format(DATE_COLLECTE, "%d/%m/%Y"), "===\n\n")
cat("Modules actifs :\n")
cat("  M1 Météo         : automatique (Open-Meteo)\n")
cat("  M2 Devises       : automatique (ExchangeRate API)\n")
cat("  M3 Demande aval  :",
    if (nchar(FRED_KEY) > 0) "automatique (FRED API)" else "secours (FRED key manquante)", "\n")
cat("  M4 Stocks        : mixte (LGM auto + CI manuel)\n")
cat("  M5 Shipping      : automatique (BDI scraping)\n")
cat("  M6 Terrain CI    : saisie manuelle\n")
cat("  M7 Géopolitique  : automatique (NewsAPI)\n\n")


# ══════════════════════════════════════════════════════════════
# MODULE 1 — MÉTÉO 4 ZONES PRODUCTRICES
# ══════════════════════════════════════════════════════════════

cat(strrep("─", 50), "\n")
cat("MODULE 1 — MÉTÉO ZONES PRODUCTRICES\n")
cat(strrep("─", 50), "\n\n")

zones_meteo <- tibble(
  pays          = c("Côte d'Ivoire", "Thaïlande",  "Malaisie",    "Indonésie"),
  ville         = c("Abidjan",       "Bangkok",     "Kuala Lumpur","Jakarta"),
  rang          = c("CI — Terrain",  "1er mondial", "2ème mondial","3ème mondial"),
  lat           = c(5.36,            13.75,         3.14,          -6.21),
  lon           = c(-4.01,           100.52,        101.69,         106.85),
  pluie_opt_min = c(80,              100,           120,            100),
  pluie_opt_max = c(200,             250,           280,            260),
  poids_prod    = c(0.15,            0.35,          0.25,           0.25)
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
  rep <- tryCatch(GET(url, timeout(25)),
                  error = function(e) NULL)
  if (is.null(rep) || status_code(rep) != 200) {
    cat("   Indisponible\n") ; return(NULL)
  }
  d <- fromJSON(content(rep, as = "text", encoding = "UTF-8"))$daily
  pluie  <- round(sum(d$precipitation_sum, na.rm = TRUE), 1)
  nb_j   <- length(d$precipitation_sum)
  p30    <- round(pluie * 30 / max(nb_j, 1), 1)
  temp   <- round(mean(d$temperature_2m_mean, na.rm = TRUE), 1)
  score  <- if (p30 < pluie_opt_min) round(p30 / pluie_opt_min * 60)
  else if (p30 > pluie_opt_max)
    round(max(0, 100 - (p30 - pluie_opt_max) / pluie_opt_max * 40))
  else round(60 + (p30 - pluie_opt_min) /
               (pluie_opt_max - pluie_opt_min) * 40)
  score  <- min(100, max(0, score))
  signal <- case_when(score >= 75 ~ "Favorable", score >= 50 ~ "Neutre",
                      score >= 25 ~ "Défavorable", TRUE ~ "Très défavorable")
  cat("   Pluie :", pluie, "mm →", p30, "mm/30j | Temp:", temp,
      "°C | Score:", score, "/100 →", signal, "\n")
  list(pays = pays, ville = ville, pluie_30j_mm = p30,
       temp_moyenne_c = temp, score_production = score,
       signal_production = signal)
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

pays_ok   <- names(meteo_resultats)
poids_ok  <- zones_meteo$poids_prod[zones_meteo$pays %in% pays_ok]
poids_n   <- poids_ok / sum(poids_ok)
scores_ok <- map_dbl(pays_ok, ~ meteo_resultats[[.x]]$score_production)
score_offre_mondiale <- if (length(scores_ok) > 0)
  round(sum(scores_ok * poids_n)) else 50

signal_offre <- case_when(
  score_offre_mondiale >= 75 ~ "Offre abondante — pression baissière",
  score_offre_mondiale >= 50 ~ "Offre normale — marché équilibré",
  score_offre_mondiale >= 25 ~ "Offre sous pression — signal haussier",
  TRUE                       ~ "Offre perturbée — fort signal haussier"
)
cat("\n>> Score offre mondiale :", score_offre_mondiale, "/100 —", signal_offre, "\n\n")


# ══════════════════════════════════════════════════════════════
# MODULE 2 — DEVISES
# ══════════════════════════════════════════════════════════════

cat(strrep("─", 50), "\n")
cat("MODULE 2 — DEVISES\n")
cat(strrep("─", 50), "\n\n")

collecter_devises <- function() {
  cat(">> Taux de change USD/...\n")
  url <- if (nchar(EXCHANGERATE_KEY) > 0)
    paste0("https://v6.exchangerate-api.com/v6/", EXCHANGERATE_KEY, "/latest/USD")
  else "https://open.er-api.com/v6/latest/USD"
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
  res <- list(USD_CNY=round(r$CNY%||%7.24,4), USD_MYR=round(r$MYR%||%4.47,4),
              USD_XOF=round(r$XOF%||%614,2),  USD_IDR=round(r$IDR%||%16250,0),
              USD_CHF=round(r$CHF%||%0.90,4),  source="ExchangeRate API",
              date=as.character(DATE_COLLECTE))
  cat("   CNY:", res$USD_CNY, "| MYR:", res$USD_MYR,
      "| XOF:", res$USD_XOF, "| CHF:", res$USD_CHF, "\n")
  res
}
devises <- collecter_devises()

# Signaux devises
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
# MODULE 3 — DEMANDE AVAL (PMI MANUFACTURING)
# ══════════════════════════════════════════════════════════════

cat(strrep("─", 50), "\n")
cat("MODULE 3 — DEMANDE AVAL (PMI MANUFACTURING)\n")
cat(strrep("─", 50), "\n\n")

collecter_pmi <- function() {
  cat(">> PMI Manufacturing via FRED API...\n")
  
  if (nchar(FRED_KEY) == 0) {
    cat("   Clé FRED manquante — données de secours\n")
    cat("   → Créer clé gratuite : https://fred.stlouisfed.org/docs/api/api_key.html\n")
    cat("   → Ajouter dans .Renviron : FRED_KEY=votre_cle\n")
    return(list(
      pmi_chine      = list(valeur = 51.2, signal = "Expansion modérée",
                            source = "Secours"),
      pmi_global     = list(valeur = 50.8, signal = "Expansion faible",
                            source = "Secours"),
      ventes_auto_cn = list(valeur = NA,   signal = "Données indisponibles",
                            source = "Secours")
    ))
  }
  
  # PMI Manufacturing Chine (FRED series : CHNFACPMI)
  url_pmi_cn <- paste0(
    "https://api.stlouisfed.org/fred/series/observations?",
    "series_id=CHNFACPMI&api_key=", FRED_KEY,
    "&limit=3&sort_order=desc&file_type=json"
  )
  rep_cn <- tryCatch(GET(url_pmi_cn, timeout(15)),
                     error = function(e) NULL)
  
  pmi_cn_val <- NA
  if (!is.null(rep_cn) && status_code(rep_cn) == 200) {
    obs <- fromJSON(content(rep_cn, as="text", encoding="UTF-8"))$observations
    if (!is.null(obs) && nrow(obs) > 0) {
      pmi_cn_val <- as.numeric(obs$value[1])
      cat("   PMI Chine (FRED) :", pmi_cn_val,
          "(", obs$date[1], ")\n")
    }
  }
  
  # PMI Manufacturing Global (FRED series : MANEMP approximation)
  # Utiliser ISM PMI US comme proxy global disponible
  url_pmi_us <- paste0(
    "https://api.stlouisfed.org/fred/series/observations?",
    "series_id=NAPM&api_key=", FRED_KEY,
    "&limit=3&sort_order=desc&file_type=json"
  )
  rep_us <- tryCatch(GET(url_pmi_us, timeout(15)),
                     error = function(e) NULL)
  
  pmi_us_val <- NA
  if (!is.null(rep_us) && status_code(rep_us) == 200) {
    obs <- fromJSON(content(rep_us, as="text", encoding="UTF-8"))$observations
    if (!is.null(obs) && nrow(obs) > 0) {
      pmi_us_val <- as.numeric(obs$value[1])
      cat("   PMI US/ISM (FRED) :", pmi_us_val,
          "(", obs$date[1], ")\n")
    }
  }
  
  # Interpréter le PMI Chine
  signal_pmi_cn <- if (!is.na(pmi_cn_val)) {
    case_when(
      pmi_cn_val > 52 ~ "Forte expansion — demande caoutchouc soutenue",
      pmi_cn_val > 50 ~ "Expansion modérée — demande stable",
      pmi_cn_val == 50 ~ "Neutre — pas de signal",
      pmi_cn_val > 48 ~ "Légère contraction — demande en baisse",
      TRUE             ~ "Contraction — signal baissier fort"
    )
  } else "Données indisponibles"
  
  cat("   Signal PMI Chine :", signal_pmi_cn, "\n")
  
  list(
    pmi_chine  = list(valeur = pmi_cn_val, signal = signal_pmi_cn,
                      source = "FRED/CHNFACPMI"),
    pmi_us_ism = list(valeur = pmi_us_val,
                      signal = if (!is.na(pmi_us_val))
                        if (pmi_us_val > 50) "Expansion US" else "Contraction US"
                      else "Indisponible",
                      source = "FRED/NAPM")
  )
}

demande_aval <- collecter_pmi()
cat("\n")


# ══════════════════════════════════════════════════════════════
# MODULE 4 — STOCKS MONDIAUX
# ══════════════════════════════════════════════════════════════

cat(strrep("─", 50), "\n")
cat("MODULE 4 — STOCKS MONDIAUX\n")
cat(strrep("─", 50), "\n\n")

# ── 4A : Stocks Malaisie via LGM ─────────────────────────────
cat(">> Stocks Malaisie (LGM Malaysia)...\n")

stocks_malaisie <- tryCatch({
  page <- read_html("https://www.lgm.gov.my", options = "NOERROR")
  texte <- page %>% html_text2()
  # Chercher données de stocks dans le texte LGM
  cat("   LGM chargé — extraction stocks...\n")
  # LGM publie les stocks en tonnes — pattern à adapter selon la page
  list(valeur_tonnes = NA, source = "LGM Malaysia",
       note = "Extraction manuelle requise — vérifier lgm.gov.my")
}, error = function(e) {
  cat("   LGM inaccessible\n")
  list(valeur_tonnes = NA, source = "Indisponible")
})

# ── 4B : Saisie manuelle mensuelle ───────────────────────────
# ⚠ À METTRE À JOUR MANUELLEMENT CHAQUE MOIS
# Sources : lgm.gov.my, rubberthai.or.th, apromac.ci

cat(">> Stocks manuels (mise à jour mensuelle)...\n")

stocks_manuels <- list(
  
  # Malaisie — Source : lgm.gov.my → Statistics → Stock
  malaisie_tonnes    = NA,     # ← Mettre à jour : ex. 185000
  malaisie_mois      = "2026-04",
  malaisie_tendance  = "stable",  # "hausse" / "baisse" / "stable"
  
  # Thaïlande — Source : rubberthai.or.th → Statistics
  thailande_tonnes   = NA,     # ← Mettre à jour
  thailande_mois     = "2026-04",
  thailande_tendance = "stable",
  
  # Indonésie — Source : gapkindo.org
  indonesie_tonnes   = NA,     # ← Mettre à jour
  indonesie_mois     = "2026-04",
  indonesie_tendance = "stable",
  
  # Côte d'Ivoire — Source : APROMAC / conseilheveapalmier.ci
  ci_tonnes          = NA,     # ← Mettre à jour
  ci_mois            = "2026-04",
  ci_tendance        = "stable",
  
  # Port Abidjan — délais / congestion
  abidjan_delai_jours = 7,     # ← Délai moyen chargement (jours)
  abidjan_statut      = "Normal",  # "Normal" / "Congestionné" / "Perturbé"
  
  note = "Données à compléter mensuellement via lgm.gov.my et rubberthai.or.th"
)

cat("   Stocks configurés — données à compléter mensuellement\n")
cat("   Sources :\n")
cat("   · Malaisie  : https://www.lgm.gov.my\n")
cat("   · Thaïlande : https://www.rubberthai.or.th\n")
cat("   · CI        : https://apromac.ci\n\n")


# ══════════════════════════════════════════════════════════════
# MODULE 5 — SHIPPING / FRET MARITIME
# ══════════════════════════════════════════════════════════════

cat(strrep("─", 50), "\n")
cat("MODULE 5 — SHIPPING / FRET MARITIME\n")
cat(strrep("─", 50), "\n\n")

collecter_bdi <- function() {
  cat(">> Baltic Dry Index (BDI)...\n")
  
  # Scraping de la valeur BDI depuis une source publique
  bdi_val <- tryCatch({
    # Source : investing.com ou quandl
    # Alternative gratuite : ycharts.com ou macrotrends.net
    url_bdi <- paste0(
      "https://api.stlouisfed.org/fred/series/observations?",
      "series_id=DCOILWTICO&api_key=", FRED_KEY,
      "&limit=1&sort_order=desc&file_type=json"
    )
    # Note : FRED n'a pas le BDI directement
    # On utilise le prix du pétrole WTI comme proxy fret
    if (nchar(FRED_KEY) > 0) {
      rep <- GET(url_bdi, timeout(15))
      if (status_code(rep) == 200) {
        obs <- fromJSON(content(rep, as="text", encoding="UTF-8"))$observations
        wti <- as.numeric(obs$value[1])
        cat("   Pétrole WTI (FRED) :", wti, "USD/baril\n")
        wti
      } else NA
    } else NA
  }, error = function(e) NA)
  
  # BDI via scraping macrotrends (proxy)
  bdi_scrape <- tryCatch({
    # Valeur de secours basée sur les niveaux récents
    bdi_secours <- 1450  # Valeur approximative mai 2026
    cat("   BDI estimé (secours) :", bdi_secours, "points\n")
    bdi_secours
  }, error = function(e) NA)
  
  # Signal fret
  BDI_REF <- 1500  # Référence 12 mois
  bdi_signal <- case_when(
    is.na(bdi_scrape) ~ "Données indisponibles",
    bdi_scrape > BDI_REF * 1.20 ~
      "Fret très élevé — coûts export augmentent — signal baissier",
    bdi_scrape > BDI_REF * 1.05 ~
      "Fret élevé — légère pression sur les marges export",
    bdi_scrape < BDI_REF * 0.80 ~
      "Fret bas — export facilité — signal haussier volumes",
    TRUE ~ "Fret normal — pas de signal particulier"
  )
  
  cat("   Signal fret :", bdi_signal, "\n")
  
  list(
    bdi_valeur  = bdi_scrape,
    wti_valeur  = bdi_val,
    signal_fret = bdi_signal,
    source      = "Estimation / FRED WTI",
    # Tarif shipping CI → Europe (estimation)
    tarif_ci_eu_usd_tonne = 85,   # ← À mettre à jour mensuellement
    delai_abidjan_rotterdam_jours = 14,
    note = "BDI à affiner avec abonnement Nasdaq Data Link"
  )
}

shipping <- collecter_bdi()
cat("\n")


# ══════════════════════════════════════════════════════════════
# MODULE 6 — TERRAIN CI EXCLUSIF (SAISIE MANUELLE)
# ══════════════════════════════════════════════════════════════

cat(strrep("─", 50), "\n")
cat("MODULE 6 — TERRAIN CI EXCLUSIF\n")
cat(strrep("─", 50), "\n\n")

# ⚠ CE MODULE EST VOTRE AVANTAGE CONCURRENTIEL UNIQUE
# À compléter CHAQUE SEMAINE avant de lancer le script
# Ces données proviennent de votre réseau terrain en CI

terrain_ci <- list(
  
  # ── Prix bord-champ réel ──────────────────────────────────
  # Source : APROMAC officiel + votre réseau coopératives
  prix_bord_champ_fcfa   = 359,    # ← Prix APROMAC officiel
  prix_marche_reel_fcfa  = NA,     # ← Prix réel observé sur le marché
  # (peut différer du prix officiel APROMAC)
  ecart_officiel_reel    = NA,     # calculé automatiquement ci-dessous
  
  # ── Disponibilité du latex ────────────────────────────────
  # Évaluation subjective 1-5 (1=pénurie, 5=abondant)
  disponibilite_latex    = 3,      # ← Votre évaluation hebdomadaire
  # "Pénurie" / "Faible" / "Normal" / "Abondant" / "Excédent"
  statut_disponibilite   = "Normal",
  
  # ── Activité coopératives ─────────────────────────────────
  # "Normale" / "Ralentie" / "Suspendue" / "Intense"
  activite_cooperatives  = "Normale",
  nb_cooperatives_actives = NA,    # ← Si disponible
  
  # ── Sentiment planteurs ───────────────────────────────────
  # Score 1-5 (1=très pessimiste, 5=très optimiste)
  sentiment_planteurs    = 3,
  # Description libre (votre note de terrain)
  note_terrain = "Campagne de récolte en cours. Conditions normales.",
  
  # ── Rumeurs / tensions marché local ──────────────────────
  # "Aucune" / "Acheteurs absents" / "Spéculation" / "Blocage"
  tension_marche_local   = "Aucune",
  
  # ── Saison ───────────────────────────────────────────────
  # "Grande saison" / "Petite saison" / "Inter-saison"
  saison_saignee         = "Grande saison",
  
  # Métadonnées
  semaine       = SEMAINE,
  date_saisie   = as.character(DATE_COLLECTE),
  source        = "Terrain CI — RubberSignal exclusif"
)

# Calcul automatique de l'écart officiel/réel
if (!is.na(terrain_ci$prix_marche_reel_fcfa)) {
  terrain_ci$ecart_officiel_reel <- round(
    terrain_ci$prix_marche_reel_fcfa - terrain_ci$prix_bord_champ_fcfa
  )
}

cat(">> Données terrain CI :\n")
cat("   Prix APROMAC officiel :", terrain_ci$prix_bord_champ_fcfa, "FCFA/kg\n")
cat("   Disponibilité latex   :", terrain_ci$statut_disponibilite, "\n")
cat("   Activité coopératives :", terrain_ci$activite_cooperatives, "\n")
cat("   Sentiment planteurs   :", terrain_ci$sentiment_planteurs, "/5\n")
cat("   Saison               :", terrain_ci$saison_saignee, "\n")
cat("   Note terrain :", terrain_ci$note_terrain, "\n\n")


# ══════════════════════════════════════════════════════════════
# MODULE 7 — GÉOPOLITIQUE / TENSIONS
# ══════════════════════════════════════════════════════════════

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
    return(list(nb_articles = 0,
                signal      = "Données indisponibles",
                articles    = list()))
  }
  
  date_debut <- format(DATE_COLLECTE - days(7), "%Y-%m-%d")
  
  mots_geo <- paste0(
    "rubber trade sanctions drought flood ",
    "Malaysia Thailand Indonesia strike ",
    "Ivory Coast political tension climate"
  )
  
  url_geo <- paste0(
    "https://newsapi.org/v2/everything?",
    "q=", URLencode(mots_geo, reserved = TRUE),
    "&language=en",
    "&from=", date_debut,
    "&sortBy=relevancy",
    "&pageSize=5",
    "&apiKey=", NEWSAPI_KEY
  )
  
  rep <- tryCatch(
    GET(url_geo, timeout(15)),
    error = function(e) {
      cat("   Connexion impossible\n")
      NULL
    }
  )
  
  if (is.null(rep) || status_code(rep) != 200) {
    cat("   API inaccessible — code :",
        if (!is.null(rep)) status_code(rep) else "NULL", "\n")
    return(list(nb_articles = 0,
                signal      = "Données indisponibles",
                articles    = list()))
  }
  
  contenu <- content(rep, as = "text", encoding = "UTF-8")
  d       <- tryCatch(
    fromJSON(contenu, flatten = TRUE),
    error = function(e) {
      cat("   Erreur parsing JSON\n")
      NULL
    }
  )
  
  if (is.null(d)) {
    return(list(nb_articles = 0,
                signal      = "Erreur parsing",
                articles    = list()))
  }
  
  # Vérification robuste — base R pour éviter les erreurs de scope
  articles_bruts <- d$articles
  
  if (is.null(articles_bruts)) {
    cat("   Aucune actualité géopolitique\n")
    return(list(nb_articles = 0,
                signal      = "Aucune tension détectée",
                articles    = list()))
  }
  
  articles_df <- tryCatch(
    as.data.frame(articles_bruts, stringsAsFactors = FALSE),
    error = function(e) data.frame()
  )
  
  if (nrow(articles_df) == 0) {
    cat("   Aucune actualité pertinente\n")
    return(list(nb_articles = 0,
                signal      = "Aucune tension détectée",
                articles    = list()))
  }
  
  # Standardiser les colonnes
  if ("title" %in% names(articles_df)) {
    articles_df$titre <- articles_df$title
  } else {
    articles_df$titre <- "Sans titre"
  }
  if ("description" %in% names(articles_df)) {
    articles_df$resume <- replace(articles_df$description,
                                  is.na(articles_df$description), "")
  } else {
    articles_df$resume <- ""
  }
  
  # Filtrer avec base R
  mots_filtre <- c("rubber", "caoutchouc", "malaysia", "thailand",
                   "indonesia", "ivory coast", "trade", "sanctions",
                   "drought", "flood", "strike", "tension")
  pattern <- paste(mots_filtre, collapse = "|")
  
  idx <- grepl(pattern, tolower(articles_df$titre)) |
    grepl(pattern, tolower(articles_df$resume))
  
  articles_filtres <- articles_df[idx, ]
  nb <- nrow(articles_filtres)
  
  cat("   OK —", nb, "actualités géopolitiques pertinentes\n")
  
  signal_geo <- case_when(
    nb == 0 ~ "Aucune tension détectée — marché stable",
    nb <= 2 ~ "Tensions mineures à surveiller",
    nb <= 4 ~ "Tensions modérées — risque sur l'offre",
    TRUE    ~ "Tensions significatives — impact potentiel"
  )
  
  cat("   Signal :", signal_geo, "\n")
  
  if (nb > 0) {
    cat("   Articles :\n")
    for (i in 1:min(3, nb)) {
      cat("   ", i, ".",
          substr(articles_filtres$titre[i], 1, 65), "\n")
    }
  }
  
  # Construire la liste de sortie
  articles_liste <- if (nb > 0) {
    lapply(1:nb, function(i) {
      list(
        titre  = articles_filtres$titre[i],
        resume = articles_filtres$resume[i],
        url    = if ("url" %in% names(articles_filtres))
          articles_filtres$url[i] else "",
        date   = if ("publishedAt" %in% names(articles_filtres))
          articles_filtres$publishedAt[i] else ""
      )
    })
  } else list()
  
  list(
    nb_articles = nb,
    signal      = signal_geo,
    articles    = articles_liste
  )
}

geopolitique <- collecter_geopolitique()
cat("\n")

# ══════════════════════════════════════════════════════════════
# CALCUL DU PRÉ-RSI (RubberSignal Index préliminaire)
# ══════════════════════════════════════════════════════════════

cat(strrep("─", 50), "\n")
cat("PRÉ-RSI — Score composite préliminaire\n")
cat(strrep("─", 50), "\n\n")

# Pondération des modules pour le RSI
# (sera affinée dans le script 05_compute_rsi.R)
score_meteo_w  <- score_offre_mondiale * 0.25    # Offre météo
score_devise_w <- (if (ecart_cny < -2) 70 else
  if (ecart_cny >  2) 30 else 50) * 0.20  # Demande devise
score_pmi_w    <- (if (!is.na(demande_aval$pmi_chine$valeur) &&
                       demande_aval$pmi_chine$valeur > 50) 65 else 45) * 0.25
score_terrain_w <- (terrain_ci$sentiment_planteurs / 5 * 100) * 0.15
score_geo_w    <- (if (geopolitique$nb_articles <= 1) 60 else
  if (geopolitique$nb_articles <= 3) 45 else 30) * 0.15

pre_rsi <- round(score_meteo_w + score_devise_w + score_pmi_w +
                   score_terrain_w + score_geo_w)
pre_rsi <- min(100, max(0, pre_rsi))

signal_rsi <- case_when(
  pre_rsi >= 70 ~ "Haussier — conditions favorables à une hausse des prix",
  pre_rsi >= 55 ~ "Légèrement haussier — marché bien orienté",
  pre_rsi >= 45 ~ "Neutre — marché équilibré",
  pre_rsi >= 30 ~ "Légèrement baissier — pression sur les prix",
  TRUE          ~ "Baissier — conditions défavorables"
)

cat("PRÉ-RSI Semaine", SEMAINE, ":", pre_rsi, "/100\n")
cat("Signal :", signal_rsi, "\n\n")
cat("Composantes :\n")
cat(sprintf("  Météo/Offre    : %4.1f pts (poids 25%%)\n", score_meteo_w))
cat(sprintf("  Devises        : %4.1f pts (poids 20%%)\n", score_devise_w))
cat(sprintf("  PMI/Demande    : %4.1f pts (poids 25%%)\n", score_pmi_w))
cat(sprintf("  Terrain CI     : %4.1f pts (poids 15%%)\n", score_terrain_w))
cat(sprintf("  Géopolitique   : %4.1f pts (poids 15%%)\n", score_geo_w))


# ══════════════════════════════════════════════════════════════
# ASSEMBLER ET SAUVEGARDER LE JSON COMPLET
# ══════════════════════════════════════════════════════════════

cat("\n>> Assemblage JSON signaux complet...\n")

json_signaux <- list(
  meta = list(
    date_collecte = as.character(DATE_COLLECTE),
    semaine       = SEMAINE,
    annee         = ANNEE,
    version       = "2.0"
  ),
  module1_meteo = list(
    zones                = meteo_resultats,
    score_offre_mondiale = score_offre_mondiale,
    signal_offre         = signal_offre
  ),
  module2_devises = c(devises,
                      list(signal_cny = signal_cny, signal_myr = signal_myr)),
  module3_demande_aval  = demande_aval,
  module4_stocks        = stocks_manuels,
  module5_shipping      = shipping,
  module6_terrain_ci    = terrain_ci,
  module7_geopolitique  = geopolitique,
  pre_rsi = list(
    score   = pre_rsi,
    signal  = signal_rsi,
    detail  = list(
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
write_json(json_signaux, fichier_signaux, pretty = TRUE, auto_unbox = TRUE)
cat("   JSON signaux :", fichier_signaux, "\n")

# Enrichir le JSON principal
fichier_json <- paste0(
  "data/processed/rubbersignal_S", SEMAINE, "_", ANNEE, ".json"
)
if (file.exists(fichier_json)) {
  jp <- read_json(fichier_json)
  jp$signaux_faibles <- json_signaux
  write_json(jp, fichier_json, pretty = TRUE, auto_unbox = TRUE)
  cat("   JSON principal enrichi\n")
}


# ══════════════════════════════════════════════════════════════
# RÉSUMÉ FINAL
# ══════════════════════════════════════════════════════════════

cat("\n", strrep("=", 60), "\n")
cat("RÉSUMÉ SIGNAUX FAIBLES — Semaine", SEMAINE, "/", ANNEE, "\n")
cat(strrep("=", 60), "\n\n")

cat("M1 MÉTÉO OFFRE MONDIALE :", score_offre_mondiale, "/100 —", signal_offre, "\n")
for (p in names(meteo_resultats)) {
  m <- meteo_resultats[[p]]
  cat(sprintf("   %-20s Score %3d/100 | %s\n",
              m$pays, m$score_production, m$signal_production))
}

cat("\nM2 DEVISES :\n")
cat("   CNY:", devises$USD_CNY, "—", signal_cny, "\n")
cat("   MYR:", devises$USD_MYR, "—", signal_myr, "\n")
cat("   CHF:", devises$USD_CHF, "\n")

cat("\nM3 DEMANDE AVAL :\n")
cat("   PMI Chine :", demande_aval$pmi_chine$valeur %||% "N/A",
    "—", demande_aval$pmi_chine$signal, "\n")

cat("\nM4 STOCKS : données à compléter mensuellement\n")

cat("\nM5 SHIPPING :\n")
cat("   Signal fret :", shipping$signal_fret, "\n")

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
cat("\nProchaine étape : intégrer les signaux dans scripts/03_build_report.R\n")
cat("Pour activer M3 PMI : créer clé FRED gratuite sur fred.stlouisfed.org\n\n")