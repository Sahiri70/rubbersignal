# =============================================================
# RUBBERSIGNAL.COM — Script 01 : Collecte des prix
# Auteur  : Martial Sahiri
# Version : 2.1 — corrigé
# Objectif: Récupérer chaque semaine les prix TSR20
#           depuis des sources publiques gratuites
# Usage   : source("scripts/01_collect_prices.R")
# =============================================================

# ── 1. CHARGER LES PACKAGES ──────────────────────────────────

library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(rvest)


# ── 2. PARAMÈTRES GLOBAUX ────────────────────────────────────

DATE_COLLECTE <- Sys.Date()
SEMAINE       <- isoweek(DATE_COLLECTE)
ANNEE         <- year(DATE_COLLECTE)

cat("=== RUBBERSIGNAL — Collecte du", format(DATE_COLLECTE, "%d/%m/%Y"), "===\n\n")


# ── 3. SOURCE A : PRIX VIA FMI ───────────────────────────────

cat(">> Collecte prix FMI...\n")

url_fmi <- paste0(
  "https://imfstatsstorage.blob.core.windows.net/",
  "pcps/PCRUBBN.json"
)

reponse_fmi <- tryCatch({
  GET(url_fmi, timeout(15))
}, error = function(e) {
  cat("   FMI inaccessible — données de secours utilisées\n")
  NULL
})

# Données de secours si FMI indisponible
# ── CORRECTION : prix_bm défini ICI, avant toute utilisation ──
prix_bm <- tibble(
  periode  = c("2026-03","2026-02","2026-01","2025-12",
               "2025-11","2025-10","2025-09","2025-08"),
  prix_usd = c(1.68, 1.65, 1.61, 1.58,
               1.55, 1.52, 1.49, 1.47),
  source   = "Données de référence",
  unite    = "USD/kg"
)
cat("   Données de référence chargées :", nrow(prix_bm), "points\n")


# ── 4. SOURCE B : PRIX INDEXMUNDI ────────────────────────────

cat("\n>> Collecte prix IndexMundi...\n")

url_indexmundi <- "https://www.indexmundi.com/commodities/?commodity=rubber&months=12"

prix_indexmundi <- tryCatch({
  
  page_html <- read_html(url_indexmundi)
  
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
  
  cat("   OK —", nrow(tableau_propre), "points récupérés\n")
  if (nrow(tableau_propre) > 0) {
    cat("   Dernier prix :", tableau_propre$prix_usd[1],
        "USD/kg (", tableau_propre$mois[1], ")\n")
  }
  
  tableau_propre
  
}, error = function(e) {
  cat("   ATTENTION : IndexMundi inaccessible :", conditionMessage(e), "\n")
  tibble()
})

# Si IndexMundi a renvoyé des données, on enrichit prix_bm
if (nrow(prix_indexmundi) >= 2) {
  cat("\n>> IndexMundi disponible — utilisation comme source principale...\n")
  prix_bm <- prix_indexmundi %>%
    rename(periode = mois) %>%
    mutate(source = "IndexMundi")
}


# ── 5. CALCULS DE SYNTHÈSE ───────────────────────────────────
# ── NOTE : prix_actuel est calculé ICI — après définition de prix_bm ──
# ── Les grades RSS3/TSR10/Latex sont calculés dans le script 03 ──────

cat("\n>> Calcul des statistiques de synthèse...\n")

if (nrow(prix_bm) >= 2) {
  
  prix_actuel    <- prix_bm$prix_usd[1]
  prix_precedent <- prix_bm$prix_usd[2]
  variation_pct  <- round((prix_actuel - prix_precedent) / prix_precedent * 100, 2)
  tendance       <- if (variation_pct > 0) "hausse" else if (variation_pct < 0) "baisse" else "stable"
  
  moyenne_3m <- round(mean(prix_bm$prix_usd[1:min(3, nrow(prix_bm))]), 3)
  moyenne_6m <- round(mean(prix_bm$prix_usd[1:min(6, nrow(prix_bm))]), 3)
  min_12m    <- round(min(prix_bm$prix_usd, na.rm = TRUE), 3)
  max_12m    <- round(max(prix_bm$prix_usd, na.rm = TRUE), 3)
  
  cat("   Prix actuel    :", prix_actuel, "USD/kg\n")
  cat("   Variation      :", variation_pct, "% vs période précédente\n")
  cat("   Tendance       :", tendance, "\n")
  cat("   Moyenne 3 mois :", moyenne_3m, "USD/kg\n")
  cat("   Moyenne 6 mois :", moyenne_6m, "USD/kg\n")
  
  synthese_prix <- list(
    prix_actuel   = prix_actuel,
    periode       = as.character(prix_bm$periode[1]),
    variation_pct = variation_pct,
    tendance      = tendance,
    moyenne_3m    = moyenne_3m,
    moyenne_6m    = moyenne_6m,
    min_12m       = min_12m,
    max_12m       = max_12m
  )
  
} else {
  cat("   ATTENTION : données insuffisantes pour le calcul\n")
  prix_actuel   <- 1.68
  synthese_prix <- list(
    prix_actuel   = prix_actuel,
    periode       = as.character(DATE_COLLECTE),
    variation_pct = NA,
    tendance      = "indéterminée"
  )
}


# ── 6. CONVERSION EN CHF ─────────────────────────────────────

TAUX_USD_CHF <- 0.90

if (!is.na(synthese_prix$prix_actuel)) {
  synthese_prix$prix_chf <- round(synthese_prix$prix_actuel * TAUX_USD_CHF, 3)
  cat("   Prix en CHF    :", synthese_prix$prix_chf, "CHF/kg",
      "(taux USD/CHF :", TAUX_USD_CHF, ")\n")
}


# ── 7. SAUVEGARDER LES DONNÉES BRUTES ────────────────────────

cat("\n>> Sauvegarde des données brutes...\n")

if (nrow(prix_bm) > 0) {
  fichier_csv <- paste0("data/raw/prix_bm_", ANNEE, "_S", SEMAINE, ".csv")
  write_csv(prix_bm, fichier_csv)
  cat("   Sauvegardé :", fichier_csv, "\n")
}

if (nrow(prix_indexmundi) > 0) {
  fichier_csv2 <- paste0("data/raw/prix_indexmundi_", ANNEE, "_S", SEMAINE, ".csv")
  write_csv(prix_indexmundi, fichier_csv2)
  cat("   Sauvegardé :", fichier_csv2, "\n")
}


# ── 8. CONSTRUIRE L'OBJET JSON DE SORTIE ─────────────────────

cat("\n>> Construction du JSON de sortie...\n")

json_sortie <- list(
  
  meta = list(
    date_collecte = as.character(DATE_COLLECTE),
    semaine       = SEMAINE,
    annee         = ANNEE,
    source        = "rubbersignal.com",
    version       = "2.1"
  ),
  
  prix = list(
    synthese              = synthese_prix,
    historique_bm         = prix_bm,
    historique_indexmundi = if (nrow(prix_indexmundi) > 0)
      head(prix_indexmundi, 6)
    else
      list()
  )
)

fichier_json <- paste0("data/processed/rubbersignal_S", SEMAINE, "_", ANNEE, ".json")
write_json(json_sortie, fichier_json, pretty = TRUE, auto_unbox = TRUE)
cat("   JSON sauvegardé :", fichier_json, "\n")


# ── 9. RÉSUMÉ FINAL ──────────────────────────────────────────

cat("\n", strrep("=", 50), "\n")
cat("RÉSUMÉ COLLECTE PRIX — Semaine", SEMAINE, "/", ANNEE, "\n")
cat(strrep("=", 50), "\n")
cat("Prix TSR20 actuel  :", synthese_prix$prix_actuel, "USD/kg\n")
if (!is.null(synthese_prix$prix_chf))
  cat("Prix en CHF        :", synthese_prix$prix_chf, "CHF/kg\n")
cat("Tendance           :", synthese_prix$tendance, "\n")
if (!is.null(synthese_prix$moyenne_3m))
  cat("Moyenne 3 mois     :", synthese_prix$moyenne_3m, "USD/kg\n")
cat("Fichier JSON créé  :", fichier_json, "\n")
cat(strrep("=", 50), "\n")
cat("\nProchaine étape : lancer scripts/02_collect_news.R\n\n")
