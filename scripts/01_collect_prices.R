# =============================================================
# RUBBERSIGNAL.COM — Script 01 : Collecte des prix
# Auteur  : Martial Sahiri
# Version : 3.1 — RSCI + correction taux FCFA/USD + retrait FRED
# Objectif: Recuperer chaque semaine le prix TSR20 (LGM),
#           le prix bord-champ CI (APROMAC) et calculer le RSCI
# Usage   : source("scripts/01_collect_prices.R")
# =============================================================

# ── 1. CHARGER LES PACKAGES ──────────────────────────────────

library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(rvest)


# ── 2. PARAMETRES GLOBAUX ────────────────────────────────────

DATE_COLLECTE <- Sys.Date()
SEMAINE       <- isoweek(DATE_COLLECTE)
ANNEE         <- year(DATE_COLLECTE)

cat("=== RUBBERSIGNAL — Collecte du", format(DATE_COLLECTE, "%d/%m/%Y"), "===\n\n")


# ── 3. SOURCE A : PRIX TSR20 — LGM MALAYSIA ──────────────────
# Lembaga Getah Malaysia — reference officielle SMR20 / TSR20
# Prix publie chaque jour ouvrable a 15h00 heure Malaisie
# URL : https://www.lgm.gov.my

cat(">> Collecte prix TSR20 — LGM Malaysia...\n")

prix_lgm <- tryCatch({
  
  page <- read_html("https://www.lgm.gov.my", options = "NOERROR")
  texte <- page %>% html_text2()
  
  # Chercher le prix SMR20 en US Cents/Kg
  # Format attendu : "229.255 (US Cents/Kg)"
  match_cents <- regmatches(
    texte,
    regexpr("[0-9]{2,3}\\.[0-9]{1,2}(?=\\s*\\(?US Cents)", texte, perl = TRUE)
  )
  
  if (length(match_cents) > 0) {
    prix_cents <- as.numeric(match_cents[1])
    prix_usd   <- round(prix_cents / 100, 4)
    cat("   OK — SMR20 :", prix_cents, "US Cents/kg =", prix_usd, "USD/kg\n")
    prix_usd
  } else {
    cat("   Pattern LGM non trouve — tentative IndexMundi\n")
    NA
  }
  
}, error = function(e) {
  cat("   LGM inaccessible :", conditionMessage(e), "\n")
  NA
})


# ── 4. SOURCE B : PRIX TSR20 — INDEXMUNDI (SECOURS) ──────────

cat("\n>> Collecte prix IndexMundi (source de secours)...\n")

prix_indexmundi_val <- tryCatch({
  
  page_html <- read_html(
    "https://www.indexmundi.com/commodities/?commodity=rubber&months=12"
  )
  
  tableau <- page_html %>%
    html_element("table") %>%
    html_table(header = TRUE)
  
  tableau_propre <- tableau %>%
    as_tibble() %>%
    rename_with(~ c("mois", "prix_usd", "variation"), .cols = 1:3) %>%
    filter(!is.na(prix_usd), prix_usd != "") %>%
    mutate(
      prix_usd = as.numeric(gsub("[^0-9.]", "", prix_usd)),
      source   = "IndexMundi",
      unite    = "USD/kg"
    ) %>%
    select(mois, prix_usd, source, unite) %>%
    filter(!is.na(prix_usd))
  
  if (nrow(tableau_propre) > 0) {
    cat("   OK — IndexMundi :", tableau_propre$prix_usd[1],
        "USD/kg (", tableau_propre$mois[1], ")\n")
  }
  
  tableau_propre
  
}, error = function(e) {
  cat("   IndexMundi inaccessible :", conditionMessage(e), "\n")
  tibble()
})


# ── 5. DETERMINER LE PRIX TSR20 RETENU ───────────────────────
# Priorite : LGM > IndexMundi > donnees de secours (mise a jour manuelle)
#
# NOTE (verifie S23, juin 2026) : une 3e source automatique FRED
# (serie PRUBBUSDM "Global price of Rubber") repond en HTTP 200 mais
# retourne un INDICE general (base ~100, valeur ~110.4), PAS un prix
# TSR20 en USD/kg. L'utiliser comme fallback ferait passer
# prix_actuel_usd a ~110 USD/kg de maniere silencieuse. Ecartee de la
# chaine de decision active. Code conserve en reference plus bas
# (desactive).

cat("\n>> Determination du prix TSR20 retenu...\n")

if (!is.na(prix_lgm)) {
  prix_actuel_usd <- prix_lgm
  source_prix     <- "LGM Malaysia"
  cat("   Source retenue : LGM Malaysia\n")
  
} else if (nrow(prix_indexmundi_val) > 0 && !is.na(prix_indexmundi_val$prix_usd[1])) {
  prix_actuel_usd <- prix_indexmundi_val$prix_usd[1]
  source_prix     <- "IndexMundi"
  cat("   Source retenue : IndexMundi\n")
  
} else {
  # Donnees de secours — dernier prix LGM connu
  # METTRE A JOUR CHAQUE SEMAINE si LGM et IndexMundi inaccessibles
  # Source : https://www.lgm.gov.my -> SMR20 en US Cents/Kg / 100
  prix_actuel_usd <- 2.3685  # Dernier prix LGM connu (03/06/2026)
  source_prix     <- "LGM Malaysia (03/06/2026) — mise a jour manuelle"
  cat("   Source retenue : donnees de secours\n")
  cat("   Prix manuel :", prix_actuel_usd, "USD/kg\n")
  cat("   -> Si LGM inaccessible, mettre a jour cette valeur\n")
  cat("      Source : https://www.lgm.gov.my\n")
}

cat("   Prix TSR20 retenu :", prix_actuel_usd, "USD/kg (", source_prix, ")\n")

# ── SOURCE FRED — DESACTIVEE, conservee en reference ─────────
# fred_key <- Sys.getenv("FRED_KEY")
# url_fred <- paste0(
#   "https://api.stlouisfed.org/fred/series/observations",
#   "?series_id=PRUBBUSDM&api_key=", fred_key,
#   "&sort_order=desc&limit=2&file_type=json")
# -> PRUBBUSDM = "Global price of Rubber", indice base 100 (~2016),
#    PAS le TSR20. Verifie S23 : valeur ~110.39 alors que TSR20 LGM
#    = 2.3685 USD/kg. A reconsiderer uniquement si une formule de
#    conversion indice -> TSR20 est etablie et validee.


# ── 6. CONSTRUIRE L'HISTORIQUE DES PRIX ──────────────────────

# Historique glissant 8 mois — mis a jour avec le prix actuel
prix_bm <- tibble(
  periode  = c(
    format(DATE_COLLECTE, "%Y-%m"),
    format(DATE_COLLECTE %m-% months(1), "%Y-%m"),
    format(DATE_COLLECTE %m-% months(2), "%Y-%m"),
    format(DATE_COLLECTE %m-% months(3), "%Y-%m"),
    format(DATE_COLLECTE %m-% months(4), "%Y-%m"),
    format(DATE_COLLECTE %m-% months(5), "%Y-%m"),
    format(DATE_COLLECTE %m-% months(6), "%Y-%m"),
    format(DATE_COLLECTE %m-% months(7), "%Y-%m")
  ),
  prix_usd = c(
    prix_actuel_usd,
    2.25, 2.20, 2.15, 2.10, 2.05, 2.00, 1.95
    # Les valeurs passees peuvent etre affinees manuellement
    # avec les donnees historiques LGM ou IndexMundi
  ),
  source = source_prix,
  unite  = "USD/kg"
)


# ── 7. SOURCE : PRIX BORD-CHAMP CI — APROMAC ─────────────────
# Prix officiel mensuel fixe par l'APROMAC
# Source : apromac.ci / fratmat.info / conseilheveapalmier.ci
# A mettre a jour manuellement chaque 1er du mois

cat("\n>> Prix bord-champ CI — APROMAC...\n")

# ── PARAMETRES A METTRE A JOUR MANUELLEMENT CHAQUE MOIS ──────
PRIX_APROMAC_FCFA <- 474        # Prix APROMAC en FCFA/kg — juin 2026
MOIS_APROMAC      <- "2026-06"  # Mois de reference
# Source : FratMat, 01/06/2026 (Didier Assoumou)
# https://www.fratmat.info/article/2642261/flash-info/caoutchouc-prix-apromac-juin-2026-474-f-cfa-kg

# ── TAUX DE CHANGE FCFA/USD — via parite fixe FCFA/EUR ───────
# Le FCFA (XOF) est arrime a l'EUR a un taux FIXE (structurel,
# ne change jamais) :
XOF_PAR_EUR <- 655.957
# Seul le taux EUR/USD varie -> A VERIFIER CHAQUE SEMAINE
# Source : xe.com / Reuters — verifie le 12/06/2026
TAUX_EUR_USD <- 1.157
FCFA_PAR_USD <- round(XOF_PAR_EUR / TAUX_EUR_USD, 2)
# ─────────────────────────────────────────────────────────────

prix_apromac_usd <- round(PRIX_APROMAC_FCFA / FCFA_PAR_USD, 4)

cat("   Prix bord-champ APROMAC :", PRIX_APROMAC_FCFA, "FCFA/kg (", MOIS_APROMAC, ")\n")
cat("   Taux FCFA/USD           :", FCFA_PAR_USD, "(EUR/USD =", TAUX_EUR_USD, ")\n")
cat("   Equivalent USD          :", prix_apromac_usd, "USD/kg\n")

# Calcul du spread export-planteur (brut, sans correction DRC)
# Donnee strategique — stockee en JSON uniquement, PAS publiee dans le rapport V1
# NOTE : ce spread compare un prix "sec" (TSR20 LGM) a un prix "fonds de
#        tasse humide" (APROMAC) — meme limite methodologique que celle
#        resolue pour le RSCI ci-dessous via DRC_OFFICIEL. A corriger
#        avant toute publication V2 du spread.
spread_usd <- round(prix_actuel_usd - prix_apromac_usd, 3)
spread_pct <- round(spread_usd / prix_actuel_usd * 100, 1)

cat("   Spread export-planteur  :", spread_usd, "USD/kg (",
    spread_pct, "% de la valeur export) — confidentiel V2\n")

# Objet bord-champ pour le JSON
synthese_bord_champ <- list(
  prix_fcfa         = PRIX_APROMAC_FCFA,
  prix_usd          = prix_apromac_usd,
  mois              = MOIS_APROMAC,
  source            = "APROMAC (via FratMat)",
  fcfa_par_usd      = FCFA_PAR_USD,
  taux_eur_usd      = TAUX_EUR_USD,
  spread_export_usd = spread_usd,
  spread_export_pct = spread_pct,
  note              = "Spread confidentiel — publication prevue RubberSignal V2"
)


# ── 7B. RSCI : RUBBERSIGNAL CHAIN INDEX ──────────────────────
# Part du prix planteur (corrigee DRC) dans le prix international TSR20
#
# Cadre legal : Loi N.2017-540 du 03/08/2017 (creation du CHPH)
# Mecanisme   : decision CHPH n.0037, mai 2022 -> 63% planteurs /
#               37% transformateurs (contre 61% avant 2022)
# DRC         : le calcul officiel retient un Dry Rubber Content (DRC)
#               de reference de 60% pour le fonds de tasse. Sans cette
#               correction, comparer FCFA/kg (humide) a USD/kg LGM
#               (sec) n'a pas de sens.
# Debat ouvert (non integre ici) : usines CI majoritairement TSR10
#               (grade superieur au TSR20 de reference) -> prime
#               potentielle non quantifiee a ce stade -> RSCI v2.

cat("\n>> RSCI — RubberSignal Chain Index...\n")

DRC_OFFICIEL       <- 0.60   # CHPH — taux de caoutchouc sec de reference
MECANISME_OFFICIEL <- 0.63   # CHPH decision mai 2022 — part planteur cible

prix_planteur_sec_usd <- round(prix_apromac_usd / DRC_OFFICIEL, 4)
rsci_pct              <- round(prix_planteur_sec_usd / prix_actuel_usd * 100, 1)
rsci_ecart_pts        <- round(rsci_pct - MECANISME_OFFICIEL * 100, 1)

cat("   Prix planteur (DRC-corrige) :", prix_planteur_sec_usd, "USD/kg\n")
cat("   RSCI (part planteur)        :", rsci_pct, "%\n")
cat("   Mecanisme officiel CHPH      :", MECANISME_OFFICIEL * 100, "%\n")
cat("   Ecart vs mecanisme officiel  :", rsci_ecart_pts, "points\n")

synthese_rsci <- list(
  drc_officiel           = DRC_OFFICIEL,
  mecanisme_officiel_pct = MECANISME_OFFICIEL * 100,
  prix_planteur_sec_usd  = prix_planteur_sec_usd,
  rsci_pct               = rsci_pct,
  ecart_pts              = rsci_ecart_pts,
  cadre_legal            = "Loi N.2017-540 du 03/08/2017 - CHPH",
  mecanisme_source       = "Decision CHPH n.0037 (mai 2022) : 63% planteurs / 37% transformateurs, contre 61% avant",
  drc_source             = "DRC reference officiel 60% (fonds de tasse) - debat en cours sur DRC reel et grade TSR10 vs TSR20",
  note                   = "RSCI = part du prix planteur (corrige DRC 60%) dans le prix international TSR20 (LGM)."
)


# ── 8. CALCULS DE SYNTHESE ────────────────────────────────────

cat("\n>> Calcul des statistiques de synthese...\n")

if (nrow(prix_bm) >= 2) {
  
  prix_precedent <- prix_bm$prix_usd[2]
  variation_pct  <- round(
    (prix_actuel_usd - prix_precedent) / prix_precedent * 100, 2
  )
  tendance <- if (variation_pct > 0) "hausse" else if (variation_pct < 0) "baisse" else "stable"
  
  moyenne_3m <- round(mean(prix_bm$prix_usd[1:min(3, nrow(prix_bm))]), 3)
  moyenne_6m <- round(mean(prix_bm$prix_usd[1:min(6, nrow(prix_bm))]), 3)
  min_12m    <- round(min(prix_bm$prix_usd, na.rm = TRUE), 3)
  max_12m    <- round(max(prix_bm$prix_usd, na.rm = TRUE), 3)
  
  cat("   Prix actuel    :", prix_actuel_usd, "USD/kg\n")
  cat("   Variation      :", variation_pct, "% vs periode precedente\n")
  cat("   Tendance       :", tendance, "\n")
  cat("   Moyenne 3 mois :", moyenne_3m, "USD/kg\n")
  cat("   Moyenne 6 mois :", moyenne_6m, "USD/kg\n")
  
  synthese_prix <- list(
    prix_actuel   = prix_actuel_usd,
    periode       = format(DATE_COLLECTE, "%Y-%m"),
    source        = source_prix,
    variation_pct = variation_pct,
    tendance      = tendance,
    moyenne_3m    = moyenne_3m,
    moyenne_6m    = moyenne_6m,
    min_12m       = min_12m,
    max_12m       = max_12m
  )
  
} else {
  cat("   ATTENTION : donnees insuffisantes\n")
  synthese_prix <- list(
    prix_actuel   = prix_actuel_usd,
    periode       = format(DATE_COLLECTE, "%Y-%m"),
    source        = source_prix,
    variation_pct = NA,
    tendance      = "indeterminee"
  )
}


# ── 9. CONVERSION EN CHF ──────────────────────────────────────

TAUX_USD_CHF <- 0.90  # A verifier chaque semaine

synthese_prix$prix_chf <- round(prix_actuel_usd * TAUX_USD_CHF, 3)
cat("   Prix en CHF    :", synthese_prix$prix_chf, "CHF/kg\n")


# ── 9C. MARCHÉS MONDIAUX — SAISIE MANUELLE (J-1) ─────────────
# Cours décalés d'un jour — gratuits sur TradingView / investing.com
#
# SICOM  (SGX, Singapour) — TSR20  — USD cts/kg  — ticker : TF
#   -> https://www.tradingview.com/symbols/SGX-TF1!/
# TOCOM  (JPX, Tokyo)     — RSS3   — JPY/kg       — ticker : TRB
#   -> https://www.tradingview.com/symbols/TOCOM-TRB1!/
# SHFE   (Shanghai)       — SCR WF — CNY/tonne    — ticker : RU
#   -> https://www.tradingview.com/symbols/SHFE-RU1!/
#
# ⚠ A METTRE A JOUR CHAQUE LUNDI AVANT DE LANCER LE PIPELINE

cat("\n>> Marches mondiaux (saisie manuelle J-1)...\n")

SICOM_TSR20_CENTS  <- 216.80  # USD cts/kg  — SGX TF   | investing.com 08/07/2026
TOCOM_RSS3_JPY     <- 418.00  # JPY/kg      — JPX TRB  | TradingView TOCOM-TRB1! 09/07/2026
SHFE_RU_CNY_TONNE  <- 15900   # CNY/tonne   — SHFE RU  | ESTIMATION — verifier SHFE-RU1! sur TradingView
TAUX_JPY_USD       <- 0.00619 # 1 JPY en USD  | USD/JPY = 161.6 le 10/07/2026
TAUX_CNY_USD       <- 0.1473  # 1 CNY en USD  | USD/CNY =  6.79 le 10/07/2026
DATE_MARCHES       <- Sys.Date() - 1

# ── Conversions en USD/kg ────────────────────────────────────
sicom_usd <- round(SICOM_TSR20_CENTS / 100, 4)
tocom_usd <- round(TOCOM_RSS3_JPY * TAUX_JPY_USD, 4)
shfe_usd  <- round((SHFE_RU_CNY_TONNE / 1000) * TAUX_CNY_USD, 4)

spread_sicom_lgm <- round(sicom_usd - prix_actuel_usd, 4)

cat("   SICOM  TSR20 :", SICOM_TSR20_CENTS, "cts/kg =", sicom_usd, "USD/kg\n")
cat("   TOCOM  RSS3  :", TOCOM_RSS3_JPY,    "JPY/kg =", tocom_usd, "USD/kg\n")
cat("   SHFE   RU    :", SHFE_RU_CNY_TONNE, "CNY/t  =", shfe_usd,  "USD/kg\n")
cat("   Spread SICOM vs LGM (meme grade TSR20) :", spread_sicom_lgm, "USD/kg\n")

synthese_marches <- list(
  date_cours       = as.character(DATE_MARCHES),
  spread_sicom_lgm = spread_sicom_lgm,
  lgm = list(
    grade      = "TSR20",
    bourse     = "LGM (Malaisie)",
    prix_usd   = prix_actuel_usd,
    source     = source_prix
  ),
  sicom = list(
    grade      = "TSR20",
    bourse     = "SGX/SICOM (Singapour)",
    ticker     = "TF",
    prix_orig  = SICOM_TSR20_CENTS,
    unite_orig = "USD cts/kg",
    prix_usd   = sicom_usd
  ),
  tocom = list(
    grade        = "RSS3",
    bourse       = "JPX/TOCOM (Tokyo)",
    ticker       = "TRB",
    prix_orig    = TOCOM_RSS3_JPY,
    unite_orig   = "JPY/kg",
    taux_jpy_usd = TAUX_JPY_USD,
    prix_usd     = tocom_usd
  ),
  shfe = list(
    grade        = "SCR WF / RSS3",
    bourse       = "SHFE (Shanghai)",
    ticker       = "RU",
    prix_orig    = SHFE_RU_CNY_TONNE,
    unite_orig   = "CNY/tonne",
    taux_cny_usd = TAUX_CNY_USD,
    prix_usd     = shfe_usd
  )
)


# ── 10. SAUVEGARDER LES DONNEES BRUTES ───────────────────────

cat("\n>> Sauvegarde des donnees brutes...\n")

fichier_csv <- paste0("data/raw/prix_bm_", ANNEE, "_S", SEMAINE, ".csv")
write_csv(prix_bm, fichier_csv)
cat("   Sauvegarde :", fichier_csv, "\n")

if (nrow(prix_indexmundi_val) > 0) {
  fichier_csv2 <- paste0("data/raw/prix_indexmundi_", ANNEE, "_S", SEMAINE, ".csv")
  write_csv(prix_indexmundi_val, fichier_csv2)
  cat("   Sauvegarde :", fichier_csv2, "\n")
}


# ── 11. CONSTRUIRE LE JSON DE SORTIE ─────────────────────────

cat("\n>> Construction du JSON de sortie...\n")

json_sortie <- list(
  
  meta = list(
    date_collecte = as.character(DATE_COLLECTE),
    semaine       = SEMAINE,
    annee         = ANNEE,
    source        = "rubbersignal.com",
    version       = "3.1"
  ),
  
  prix = list(
    synthese              = synthese_prix,
    bord_champ_ci         = synthese_bord_champ,
    rsci                  = synthese_rsci,
    marches_mondiaux      = synthese_marches,
    historique_bm         = prix_bm,
    historique_indexmundi = if (nrow(prix_indexmundi_val) > 0)
      head(prix_indexmundi_val, 6)
    else
      list()
  )
)

fichier_json <- paste0("data/processed/rubbersignal_S", SEMAINE, "_", ANNEE, ".json")
write_json(json_sortie, fichier_json, pretty = TRUE, auto_unbox = TRUE)
cat("   JSON sauvegarde :", fichier_json, "\n")


# ── 12. RESUME FINAL ─────────────────────────────────────────

cat("\n", strrep("=", 55), "\n")
cat("RESUME COLLECTE PRIX — Semaine", SEMAINE, "/", ANNEE, "\n")
cat(strrep("=", 55), "\n")
cat("Prix TSR20 actuel  :", prix_actuel_usd, "USD/kg (", source_prix, ")\n")
cat("Prix en CHF        :", synthese_prix$prix_chf, "CHF/kg\n")
cat("Tendance           :", synthese_prix$tendance,
    "(", synthese_prix$variation_pct, "%)\n")
cat("Moyenne 3 mois     :", synthese_prix$moyenne_3m, "USD/kg\n")
cat("Prix APROMAC CI    :", PRIX_APROMAC_FCFA, "FCFA/kg =",
    prix_apromac_usd, "USD/kg (", MOIS_APROMAC, ")\n")
cat("Spread V2 (confid.):", spread_usd, "USD/kg\n")
cat("RSCI               :", rsci_pct, "% (ecart vs 63% officiel :", rsci_ecart_pts, "pts)\n")
cat(strrep("-", 55), "\n")
cat("SICOM  TSR20       :", sicom_usd, "USD/kg (", SICOM_TSR20_CENTS, "cts)\n")
cat("TOCOM  RSS3        :", tocom_usd, "USD/kg (", TOCOM_RSS3_JPY, "JPY/kg)\n")
cat("SHFE   RU          :", shfe_usd,  "USD/kg (", SHFE_RU_CNY_TONNE, "CNY/t)\n")
cat("Spread SICOM/LGM   :", spread_sicom_lgm, "USD/kg\n")
cat("Fichier JSON       :", fichier_json, "\n")
cat(strrep("=", 55), "\n")
cat("\nProchaine etape : lancer scripts/02_collect_news.R\n\n")
cat(">> RAPPELS HEBDOMADAIRES (a mettre a jour avant chaque pipeline) :\n")
cat("   1. PRIX_APROMAC_FCFA + MOIS_APROMAC (mensuel)\n")
cat("      -> https://www.fratmat.info ou conseilheveapalmier.ci\n")
cat("   2. TAUX_EUR_USD (hebdomadaire)\n")
cat("      -> xe.com (parite XOF/EUR fixe a 655.957, ne change jamais)\n")
cat("   3. SICOM_TSR20_CENTS (hebdomadaire)\n")
cat("      -> TradingView : https://www.tradingview.com/symbols/SGX-TF1!/\n")
cat("   4. TOCOM_RSS3_JPY + TAUX_JPY_USD (hebdomadaire)\n")
cat("      -> TradingView : https://www.tradingview.com/symbols/TOCOM-TRB1!/\n")
cat("   5. SHFE_RU_CNY_TONNE + TAUX_CNY_USD (hebdomadaire)\n")
cat("      -> TradingView : https://www.tradingview.com/symbols/SHFE-RU1!/\n\n")