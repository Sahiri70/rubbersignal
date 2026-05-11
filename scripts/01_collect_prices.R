# # =============================================================
# RUBBERSIGNAL.COM — Script 01 : Collecte des prix
# Auteur  : Martial Sahiri
# Version : 3.0 — LGM Malaysia + APROMAC bord-champ CI
# Objectif: Récupérer chaque semaine les prix TSR20 (LGM)
#           et le prix bord-champ CI (APROMAC)
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


# ── 3. SOURCE A : PRIX TSR20 — LGM MALAYSIA ──────────────────
# Lembaga Getah Malaysia — référence officielle SMR20 / TSR20
# Prix publié chaque jour ouvrable à 15h00 heure Malaisie
# URL : https://www.lgm.gov.my

cat(">> Collecte prix TSR20 — LGM Malaysia...\n")

prix_lgm <- tryCatch({
  
  page <- read_html("https://www.lgm.gov.my", options = "NOERROR")
  texte <- page %>% html_text2()
  
  # Chercher le prix SMR20 en US Cents/Kg
  # Format attendu : "229.25 (US Cents/Kg)"
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
    cat("   Pattern LGM non trouvé — tentative IndexMundi\n")
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


# ── 5. DÉTERMINER LE PRIX TSR20 RETENU ───────────────────────
# Priorité : LGM > IndexMundi > données de secours

cat("\n>> Détermination du prix TSR20 retenu...\n")

if (!is.na(prix_lgm)) {
  prix_actuel_usd <- prix_lgm
  source_prix     <- "LGM Malaysia"
  cat("   Source retenue : LGM Malaysia\n")
  
} else if (nrow(prix_indexmundi_val) > 0 && !is.na(prix_indexmundi_val$prix_usd[1])) {
  prix_actuel_usd <- prix_indexmundi_val$prix_usd[1]
  source_prix     <- "IndexMundi"
  cat("   Source retenue : IndexMundi\n")
  
} else {
  # Données de secours — à mettre à jour manuellement chaque mois
  # Source : https://www.lgm.gov.my ou https://www.indexmundi.com
  prix_actuel_usd <- 2.29   # ← METTRE À JOUR MANUELLEMENT SI NÉCESSAIRE
  source_prix     <- "Données de secours (LGM 11/05/2026)"
  cat("   Source retenue : données de secours\n")
}

cat("   Prix TSR20 retenu :", prix_actuel_usd, "USD/kg (", source_prix, ")\n")


# ── 6. CONSTRUIRE L'HISTORIQUE DES PRIX ──────────────────────

# Historique glissant 8 mois — mis à jour avec le prix actuel
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
    # ↑ Les valeurs passées peuvent être affinées manuellement
    # avec les données historiques LGM ou IndexMundi
  ),
  source = source_prix,
  unite  = "USD/kg"
)


# ── 7. SOURCE C : PRIX BORD-CHAMP CI — APROMAC ───────────────
# Prix officiel mensuel fixé par l'APROMAC
# Source : apromac.ci / 7info.ci / conseilheveapalmier.ci
# ⚠ À mettre à jour manuellement chaque 1er du mois

cat("\n>> Prix bord-champ CI — APROMAC...\n")

# ── PARAMÈTRES À METTRE À JOUR MANUELLEMENT CHAQUE MOIS ──────
PRIX_APROMAC_FCFA  <- 359      # Prix APROMAC en FCFA/kg — mai 2026
MOIS_APROMAC       <- "2026-05" # Mois de référence
TAUX_FCFA_USD      <- 0.00158   # Taux de change FCFA/USD
# ─────────────────────────────────────────────────────────────

prix_apromac_usd <- round(PRIX_APROMAC_FCFA * TAUX_FCFA_USD, 3)

cat("   Prix bord-champ APROMAC :", PRIX_APROMAC_FCFA, "FCFA/kg\n")
cat("   Équivalent USD          :", prix_apromac_usd, "USD/kg\n")

# Calcul du spread export-planteur
# ⚠ Donnée stratégique — stockée en JSON uniquement, PAS publiée dans le rapport V1
spread_usd     <- round(prix_actuel_usd - prix_apromac_usd, 3)
spread_pct     <- round(spread_usd / prix_actuel_usd * 100, 1)

cat("   Spread export-planteur  :", spread_usd, "USD/kg (",
    spread_pct, "% de la valeur export) — confidentiel V2\n")

# Objet bord-champ pour le JSON
synthese_bord_champ <- list(
  prix_fcfa          = PRIX_APROMAC_FCFA,
  prix_usd           = prix_apromac_usd,
  mois               = MOIS_APROMAC,
  source             = "APROMAC",
  taux_fcfa_usd      = TAUX_FCFA_USD,
  # Spread confidentiel — réservé V2
  spread_export_usd  = spread_usd,
  spread_export_pct  = spread_pct,
  note               = "Spread confidentiel — publication prévue RubberSignal V2"
)


# ── 8. CALCULS DE SYNTHÈSE ────────────────────────────────────

cat("\n>> Calcul des statistiques de synthèse...\n")

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
  cat("   Variation      :", variation_pct, "% vs période précédente\n")
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
  cat("   ATTENTION : données insuffisantes\n")
  synthese_prix <- list(
    prix_actuel   = prix_actuel_usd,
    periode       = format(DATE_COLLECTE, "%Y-%m"),
    source        = source_prix,
    variation_pct = NA,
    tendance      = "indéterminée"
  )
}


# ── 9. CONVERSION EN CHF ──────────────────────────────────────

TAUX_USD_CHF <- 0.90  # À vérifier chaque semaine

synthese_prix$prix_chf <- round(prix_actuel_usd * TAUX_USD_CHF, 3)
cat("   Prix en CHF    :", synthese_prix$prix_chf, "CHF/kg\n")


# ── 10. SAUVEGARDER LES DONNÉES BRUTES ───────────────────────

cat("\n>> Sauvegarde des données brutes...\n")

fichier_csv <- paste0("data/raw/prix_bm_", ANNEE, "_S", SEMAINE, ".csv")
write_csv(prix_bm, fichier_csv)
cat("   Sauvegardé :", fichier_csv, "\n")

if (nrow(prix_indexmundi_val) > 0) {
  fichier_csv2 <- paste0("data/raw/prix_indexmundi_", ANNEE, "_S", SEMAINE, ".csv")
  write_csv(prix_indexmundi_val, fichier_csv2)
  cat("   Sauvegardé :", fichier_csv2, "\n")
}


# ── 11. CONSTRUIRE LE JSON DE SORTIE ─────────────────────────

cat("\n>> Construction du JSON de sortie...\n")

json_sortie <- list(
  
  meta = list(
    date_collecte = as.character(DATE_COLLECTE),
    semaine       = SEMAINE,
    annee         = ANNEE,
    source        = "rubbersignal.com",
    version       = "3.0"
  ),
  
  prix = list(
    synthese              = synthese_prix,
    bord_champ_ci         = synthese_bord_champ,  # Confidentiel V2
    historique_bm         = prix_bm,
    historique_indexmundi = if (nrow(prix_indexmundi_val) > 0)
      head(prix_indexmundi_val, 6)
    else
      list()
  )
)

fichier_json <- paste0("data/processed/rubbersignal_S", SEMAINE, "_", ANNEE, ".json")
write_json(json_sortie, fichier_json, pretty = TRUE, auto_unbox = TRUE)
cat("   JSON sauvegardé :", fichier_json, "\n")


# ── 12. RÉSUMÉ FINAL ─────────────────────────────────────────

cat("\n", strrep("=", 55), "\n")
cat("RÉSUMÉ COLLECTE PRIX — Semaine", SEMAINE, "/", ANNEE, "\n")
cat(strrep("=", 55), "\n")
cat("Prix TSR20 actuel  :", prix_actuel_usd, "USD/kg (", source_prix, ")\n")
cat("Prix en CHF        :", synthese_prix$prix_chf, "CHF/kg\n")
cat("Tendance           :", synthese_prix$tendance,
    "(", synthese_prix$variation_pct, "%)\n")
cat("Moyenne 3 mois     :", synthese_prix$moyenne_3m, "USD/kg\n")
cat("Prix APROMAC CI    :", PRIX_APROMAC_FCFA, "FCFA/kg =",
    prix_apromac_usd, "USD/kg\n")
cat("Spread V2 (confid.):", spread_usd, "USD/kg\n")
cat("Fichier JSON       :", fichier_json, "\n")
cat(strrep("=", 55), "\n")
cat("\nProchaine étape : lancer scripts/02_collect_news.R\n\n")
cat(">> RAPPEL MENSUEL : mettre à jour PRIX_APROMAC_FCFA\n")
cat("   Source : https://www.7info.ci (recherche 'prix caoutchouc')\n")
cat("   Ou : https://conseilheveapalmier.ci\n\n")