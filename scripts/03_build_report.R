# =============================================================
# RUBBERSIGNAL.COM — Script 03 : Assemblage du rapport bilingue
# Auteur  : Martial Sahiri
# Version : 4.1 — intégration RSCI (RubberSignal Chain Index)
# Objectif: Générer deux rapports Markdown séparés FR + EN
# Prérequis: Scripts 01, 02 et 04 déjà exécutés
# Usage   : source("scripts/03_build_report.R")
# =============================================================

# ── 1. CHARGER LES PACKAGES ──────────────────────────────────

library(jsonlite)
library(tidyverse)
library(lubridate)


# ── 2. PARAMÈTRES GLOBAUX ────────────────────────────────────

DATE_COLLECTE <- Sys.Date()
SEMAINE       <- isoweek(DATE_COLLECTE)
ANNEE         <- year(DATE_COLLECTE)
DATE_FR       <- format(DATE_COLLECTE, "%d %B %Y")
DATE_EN       <- format(DATE_COLLECTE, "%B %d, %Y")

# ── Semaine d'activation du Pré-RSI dans le rapport ──────────
# Changer cette valeur pour activer/désactiver
SEMAINE_ACTIVATION_RSI <- 21

cat("=== RUBBERSIGNAL — Rapport bilingue Semaine", SEMAINE, "/", ANNEE, "===\n\n")


# ── 3. LIRE LE JSON ENRICHI (scripts 01 + 02) ────────────────

fichier_json <- paste0("data/processed/rubbersignal_S",
                       SEMAINE, "_", ANNEE, ".json")

if (!file.exists(fichier_json)) {
  stop(paste(
    "ERREUR : JSON introuvable :", fichier_json,
    "\nVérifiez que les scripts 01 et 02 ont bien été exécutés."
  ))
}

donnees <- read_json(fichier_json)
cat(">> JSON principal chargé :", fichier_json, "\n\n")


# ── 3B. LIRE LE JSON SIGNAUX (script 04) ─────────────────────

fichier_signaux <- paste0("data/processed/signaux_S",
                          SEMAINE, "_", ANNEE, ".json")

if (file.exists(fichier_signaux)) {
  signaux        <- read_json(fichier_signaux)
  pre_rsi        <- signaux$pre_rsi$score                          %||% NA
  signal_rsi     <- signaux$pre_rsi$signal                         %||% "Indisponible"
  score_offre    <- signaux$module1_meteo$score_offre_mondiale      %||% NA
  signal_offre   <- signaux$module1_meteo$signal_offre             %||% ""
  signal_cny_r   <- signaux$module2_devises$signal_cny             %||% ""
  signal_myr_r   <- signaux$module2_devises$signal_myr             %||% ""
  score_demande  <- signaux$module3_demande_aval$score_demande      %||% NA
  signal_demande <- signaux$module3_demande_aval$signal_demande     %||% ""
  note_terrain   <- signaux$module6_terrain_ci$note_terrain         %||% ""
  signal_geo     <- signaux$module7_geopolitique$signal             %||% ""
  cat(">> Signaux faibles chargés — Pré-RSI :", pre_rsi, "/100\n")
  if (SEMAINE >= SEMAINE_ACTIVATION_RSI) {
    cat("   Pré-RSI activé dans le rapport (semaine", SEMAINE,
        "≥", SEMAINE_ACTIVATION_RSI, ")\n\n")
  } else {
    cat("   Pré-RSI désactivé cette semaine (activation semaine",
        SEMAINE_ACTIVATION_RSI, ")\n\n")
  }
} else {
  pre_rsi        <- NA
  signal_rsi     <- ""
  score_offre    <- NA
  signal_offre   <- ""
  signal_cny_r   <- ""
  signal_myr_r   <- ""
  score_demande  <- NA
  signal_demande <- ""
  note_terrain   <- ""
  signal_geo     <- ""
  cat(">> Signaux faibles indisponibles — script 04 non exécuté\n\n")
}

# ── 3C. LIRE LE JSON MONTE CARLO (script 05) ─────────────────

fichier_mc <- paste0("data/processed/monte_carlo_S",
                     SEMAINE, "_", ANNEE, ".json")

if (file.exists(fichier_mc)) {
  mc            <- read_json(fichier_mc)
  texte_mc_fr   <- mc$texte_fr                  %||% ""
  texte_mc_en   <- mc$texte_en                  %||% ""
  texte_boot_fr <- mc$bootstrap_rsi$texte_fr    %||% ""
  texte_boot_en <- mc$bootstrap_rsi$texte_en    %||% ""
  texte_sc_fr   <- mc$scenarios_meteo$texte_fr  %||% ""
  texte_sc_en   <- mc$scenarios_meteo$texte_en  %||% ""
  cat(">> Monte Carlo chargé — 3 modules disponibles\n\n")
} else {
  texte_mc_fr <- texte_mc_en <- ""
  texte_boot_fr <- texte_boot_en <- ""
  texte_sc_fr <- texte_sc_en <- ""
  cat(">> Monte Carlo indisponible — script 05 non exécuté\n\n")
}


# ── 4. EXTRAIRE LES DONNÉES PRIX ─────────────────────────────

cat(">> Extraction des données prix...\n")

synthese <- donnees$prix$synthese

# ── RSCI (RubberSignal Chain Index) — script 01 v3.1+ ────────

rsci_data  <- donnees$prix$rsci          %||% NULL
bord_champ <- donnees$prix$bord_champ_ci %||% NULL

if (!is.null(rsci_data)) {
  rsci_pct       <- rsci_data$rsci_pct               %||% NA
  rsci_ecart_pts <- rsci_data$ecart_pts              %||% NA
  rsci_prix_sec  <- rsci_data$prix_planteur_sec_usd  %||% NA
  rsci_drc       <- rsci_data$drc_officiel           %||% NA
  rsci_mecanisme <- rsci_data$mecanisme_officiel_pct %||% NA
} else {
  rsci_pct <- NA; rsci_ecart_pts <- NA; rsci_prix_sec <- NA
  rsci_drc <- NA; rsci_mecanisme <- NA
}

if (!is.null(bord_champ)) {
  aprom_fcfa <- bord_champ$prix_fcfa %||% NA
  aprom_mois <- bord_champ$mois      %||% ""
} else {
  aprom_fcfa <- NA
  aprom_mois <- ""
}

cat("   RSCI        :",
    if (!is.na(rsci_pct))
      paste0(rsci_pct, "% (écart ", rsci_ecart_pts, " pts vs ",
             rsci_mecanisme, "% officiel)")
    else "indisponible",
    "\n")

prix_actuel   <- synthese$prix_actuel   %||% NA
prix_chf      <- synthese$prix_chf      %||% NA
variation_pct <- synthese$variation_pct %||% NA
tendance      <- synthese$tendance      %||% "indéterminée"
moyenne_3m    <- synthese$moyenne_3m    %||% NA
moyenne_6m    <- synthese$moyenne_6m    %||% NA
min_12m       <- synthese$min_12m       %||% NA
max_12m       <- synthese$max_12m       %||% NA
source_prix   <- synthese$source        %||% "IndexMundi"

symbole_tendance <- case_when(
  tendance == "hausse" ~ "▲",
  tendance == "baisse" ~ "▼",
  tendance == "stable" ~ "▬",
  TRUE                 ~ "~"
)

texte_variation_fr <- ifelse(
  !is.na(variation_pct),
  paste0(if (variation_pct > 0) "+" else "",
         round(variation_pct, 2), "%"),
  "données indisponibles"
)

texte_variation_en <- ifelse(
  !is.na(variation_pct),
  paste0(if (variation_pct > 0) "+" else "",
         round(variation_pct, 2), "%"),
  "data unavailable"
)

cat("   Prix actuel :", prix_actuel, "USD/kg (", source_prix, ")\n")
cat("   Tendance    :", symbole_tendance, tendance,
    "(", texte_variation_fr, ")\n")


# ── 5. GÉNÉRER L'ANALYSE EN FRANÇAIS ─────────────────────────

cat("\n>> Rédaction analyse FR...\n")

generer_analyse_fr <- function(prix, variation, tendance,
                               moy3m, moy6m, min12, max12) {
  intro <- case_when(
    tendance == "hausse" && !is.na(variation) && variation > 2 ~
      paste0("Le marché du TSR20 affiche une progression notable cette semaine, ",
             "avec un prix atteignant **", prix, " USD/kg** (+",
             round(variation, 2), "% sur la période précédente)."),
    tendance == "hausse" && !is.na(variation) ~
      paste0("Le TSR20 poursuit son mouvement haussier à **", prix,
             " USD/kg** (+", round(variation, 2), "% vs période précédente)."),
    tendance == "baisse" && !is.na(variation) && abs(variation) > 2 ~
      paste0("Le marché du TSR20 marque un repli significatif cette semaine, ",
             "le prix reculant à **", prix, " USD/kg** (",
             round(variation, 2), "% sur la période précédente)."),
    tendance == "baisse" && !is.na(variation) ~
      paste0("Le TSR20 cède légèrement du terrain à **", prix, " USD/kg** (",
             round(variation, 2), "% vs période précédente)."),
    TRUE ~
      paste0("Le TSR20 se stabilise cette semaine à **", prix, " USD/kg**.")
  )
  contexte_moy <- if (!is.na(moy3m) && !is.na(moy6m)) {
    pos <- if (prix > moy3m) "au-dessus" else "en-dessous"
    paste0("Le prix actuel se situe ", pos, " de sa moyenne sur 3 mois (",
           moy3m, " USD/kg) et de sa moyenne sur 6 mois (", moy6m, " USD/kg).")
  } else ""
  contexte_range <- if (!is.na(min12) && !is.na(max12) &&
                        max12 > min12) {
    pct <- round((prix - min12) / (max12 - min12) * 100)
    paste0("Sur les 12 derniers mois, le TSR20 a évolué entre ", min12,
           " et ", max12, " USD/kg — le niveau actuel représente ",
           pct, "% de cette fourchette.")
  } else ""
  paste(intro, contexte_moy, contexte_range, sep = " ")
}


# ── 6. GÉNÉRER L'ANALYSE EN ANGLAIS ──────────────────────────

cat(">> Rédaction analyse EN...\n")

generer_analyse_en <- function(prix, variation, tendance,
                               moy3m, moy6m, min12, max12) {
  intro <- case_when(
    tendance == "hausse" && !is.na(variation) && variation > 2 ~
      paste0("The TSR20 market posted a significant gain this week, ",
             "with prices reaching **", prix, " USD/kg** (+",
             round(variation, 2), "% vs previous period)."),
    tendance == "hausse" && !is.na(variation) ~
      paste0("TSR20 continued its upward trend at **", prix, " USD/kg** (+",
             round(variation, 2), "% vs previous period)."),
    tendance == "baisse" && !is.na(variation) && abs(variation) > 2 ~
      paste0("The TSR20 market recorded a significant decline this week, ",
             "falling to **", prix, " USD/kg** (",
             round(variation, 2), "% vs previous period)."),
    tendance == "baisse" && !is.na(variation) ~
      paste0("TSR20 edged lower to **", prix, " USD/kg** (",
             round(variation, 2), "% vs previous period)."),
    TRUE ~
      paste0("TSR20 stabilized this week at **", prix, " USD/kg**.")
  )
  contexte_moy <- if (!is.na(moy3m) && !is.na(moy6m)) {
    pos <- if (prix > moy3m) "above" else "below"
    paste0("The current price stands ", pos, " its 3-month average (",
           moy3m, " USD/kg) and 6-month average (", moy6m, " USD/kg).")
  } else ""
  contexte_range <- if (!is.na(min12) && !is.na(max12) &&
                        max12 > min12) {
    pct <- round((prix - min12) / (max12 - min12) * 100)
    paste0("Over the past 12 months, TSR20 traded between ", min12,
           " and ", max12, " USD/kg — the current level represents ",
           pct, "% of this range.")
  } else ""
  paste(intro, contexte_moy, contexte_range, sep = " ")
}

texte_analyse_fr <- generer_analyse_fr(
  prix=prix_actuel, variation=variation_pct, tendance=tendance,
  moy3m=moyenne_3m, moy6m=moyenne_6m, min12=min_12m, max12=max_12m
)
texte_analyse_en <- generer_analyse_en(
  prix=prix_actuel, variation=variation_pct, tendance=tendance,
  moy3m=moyenne_3m, moy6m=moyenne_6m, min12=min_12m, max12=max_12m
)

cat("   Analyses FR/EN générées — OK\n")


# ── 6B. CONSTRUIRE LA SECTION PRÉ-RSI ────────────────────────
# Visible uniquement à partir de SEMAINE_ACTIVATION_RSI

cat(">> Construction section Pré-RSI...\n")

# Icône selon le score
icone_rsi <- function(score) {
  if (is.na(score)) return("➡️")
  if (score >= 70) "⬆️"
  else if (score >= 55) "↗️"
  else if (score >= 45) "➡️"
  else "↘️"
}

rapport_rsi_fr <- if (!is.na(pre_rsi) &&
                      SEMAINE >= SEMAINE_ACTIVATION_RSI) {
  paste0(
    "## 📡 Signaux faibles — Pré-RSI\n\n",
    "### ", icone_rsi(pre_rsi),
    " RubberSignal Index : **", pre_rsi, " / 100**\n\n",
    "*", signal_rsi, "*\n\n",
    "| Signal | Valeur |\n",
    "|---|---|\n",
    if (!is.na(score_offre))
      paste0("| 🌧️ Offre mondiale (météo 4 zones) | Score ",
             score_offre, "/100 — ", signal_offre, " |\n")
    else "",
    if (nchar(signal_cny_r) > 0)
      paste0("| 💱 Yuan chinois (CNY) | ", signal_cny_r, " |\n")
    else "",
    if (nchar(signal_myr_r) > 0)
      paste0("| 💱 Ringgit malaisien (MYR) | ", signal_myr_r, " |\n")
    else "",
    if (!is.na(score_demande))
      paste0("| 🏭 Demande industrielle | Score ", score_demande,
             "/100 — ", signal_demande, " |\n")
    else "",
    if (nchar(note_terrain) > 0)
      paste0("| 🌿 Terrain CI | ", note_terrain, " |\n")
    else "",
    if (nchar(signal_geo) > 0)
      paste0("| 🌍 Géopolitique | ", signal_geo, " |\n")
    else "",
    "\n> *Le Pré-RSI (RubberSignal Index) est un score composite 0–100 ",
    "intégrant météo, devises, demande industrielle et données terrain CI. ",
    "Score > 55 = marché haussier. Score < 45 = marché baissier. ",
    "Indicateur exclusif RubberSignal.*\n\n",
    "---\n\n"
  )
} else ""

rapport_rsi_en <- if (!is.na(pre_rsi) &&
                      SEMAINE >= SEMAINE_ACTIVATION_RSI) {
  paste0(
    "## 📡 Weak Signals — Pre-RSI\n\n",
    "### ", icone_rsi(pre_rsi),
    " RubberSignal Index: **", pre_rsi, " / 100**\n\n",
    "*", signal_rsi, "*\n\n",
    "| Signal | Value |\n",
    "|---|---|\n",
    if (!is.na(score_offre))
      paste0("| 🌧️ Global supply (weather — 4 zones) | Score ",
             score_offre, "/100 — ", signal_offre, " |\n")
    else "",
    if (nchar(signal_cny_r) > 0)
      paste0("| 💱 Chinese Yuan (CNY) | ", signal_cny_r, " |\n")
    else "",
    if (nchar(signal_myr_r) > 0)
      paste0("| 💱 Malaysian Ringgit (MYR) | ", signal_myr_r, " |\n")
    else "",
    if (!is.na(score_demande))
      paste0("| 🏭 Industrial demand | Score ", score_demande,
             "/100 — ", signal_demande, " |\n")
    else "",
    if (nchar(note_terrain) > 0)
      paste0("| 🌿 CI Field data | ", note_terrain, " |\n")
    else "",
    if (nchar(signal_geo) > 0)
      paste0("| 🌍 Geopolitics | ", signal_geo, " |\n")
    else "",
    "\n> *The Pre-RSI (RubberSignal Index) is a composite score 0–100 ",
    "combining weather, currencies, industrial demand and CI field data. ",
    "Score > 55 = bullish market. Score < 45 = bearish market. ",
    "Exclusive RubberSignal indicator.*\n\n",
    "---\n\n"
  )
} else ""

# ── RSCI : section rapport (FR/EN) ───────────────────────────

icone_rsci <- function(ecart) {
  if (is.na(ecart)) return("➡️")
  if (ecart >= 0) "🟢"
  else if (ecart >= -5) "🟡"
  else "🔴"
}

rapport_rsci_fr <- if (!is.na(rsci_pct)) {
  paste0(
    "## 🇨🇮 RSCI — RubberSignal Chain Index\n\n",
    "### ", icone_rsci(rsci_ecart_pts),
    " Part planteur (DRC-corrigée) : **", rsci_pct, "%**\n\n",
    "*Première publication de cet indicateur exclusif RubberSignal. ",
    "Le RSCI mesure la part du prix international TSR20 (LGM) ",
    "effectivement captée par le planteur ivoirien, après correction ",
    "du taux de caoutchouc sec (DRC) officiel.*\n\n",
    "| Indicateur | Valeur |\n",
    "|---|---|\n",
    "| Prix APROMAC (", aprom_mois, ") | ", aprom_fcfa, " FCFA/kg |\n",
    "| Prix planteur, équivalent sec (DRC ", round(rsci_drc * 100),
      "%) | ", rsci_prix_sec, " USD/kg |\n",
    "| **RSCI** (part planteur) | **", rsci_pct, "%** |\n",
    "| Mécanisme officiel CHPH | ", rsci_mecanisme, "% |\n",
    "| Écart | **", if (rsci_ecart_pts > 0) "+" else "", rsci_ecart_pts,
      " points** |\n\n",
    "Le mécanisme de répartition CHPH (décision n°0037, mai 2022 — ",
    "cadre fixé par la Loi N°2017-540) prévoit ", rsci_mecanisme,
    "% du prix international pour les planteurs. L'écart actuel entre ",
    "le RSCI et ce seuil est de ",
    if (rsci_ecart_pts > 0) "+" else "", rsci_ecart_pts, " points.\n\n",
    "> *Méthodologie v1 : DRC retenu = taux officiel ", round(rsci_drc * 100),
    "%. La prime TSR10 (grade majoritairement produit en CI, supérieur ",
    "au TSR20 de référence) n'est pas encore intégrée — voir RSCI v2. ",
    "Indicateur exclusif RubberSignal, suivi hebdomadaire.*\n\n",
    "---\n\n"
  )
} else ""

rapport_rsci_en <- if (!is.na(rsci_pct)) {
  paste0(
    "## 🇨🇮 RSCI — RubberSignal Chain Index\n\n",
    "### ", icone_rsci(rsci_ecart_pts),
    " Farmer share (DRC-adjusted): **", rsci_pct, "%**\n\n",
    "*First publication of this exclusive RubberSignal indicator. ",
    "RSCI measures the share of the international TSR20 (LGM) price ",
    "actually captured by the Ivorian farmer, after adjustment for ",
    "the official Dry Rubber Content (DRC).*\n\n",
    "| Indicator | Value |\n",
    "|---|---|\n",
    "| APROMAC price (", aprom_mois, ") | ", aprom_fcfa, " FCFA/kg |\n",
    "| Dry-equivalent farmer price (DRC ", round(rsci_drc * 100),
      "%) | ", rsci_prix_sec, " USD/kg |\n",
    "| **RSCI** (farmer share) | **", rsci_pct, "%** |\n",
    "| Official CHPH mechanism | ", rsci_mecanisme, "% |\n",
    "| Gap | **", if (rsci_ecart_pts > 0) "+" else "", rsci_ecart_pts,
      " points** |\n\n",
    "The CHPH allocation mechanism (decision n°0037, May 2022 — ",
    "framework set by Law N°2017-540) provides for ", rsci_mecanisme,
    "% of the international price to go to farmers. The current gap ",
    "between RSCI and this threshold is ",
    if (rsci_ecart_pts > 0) "+" else "", rsci_ecart_pts, " points.\n\n",
    "> *V1 methodology: DRC used = official rate ", round(rsci_drc * 100),
    "%. The TSR10 premium (CI mills mostly produce TSR10, a higher ",
    "grade than the TSR20 benchmark) is not yet included — see RSCI ",
    "v2. Exclusive RubberSignal indicator, tracked weekly.*\n\n",
    "---\n\n"
  )
} else ""

cat("   Section RSCI :",
    if (!is.na(rsci_pct)) "incluse" else "absente (données indisponibles)",
    "\n")

if (SEMAINE >= SEMAINE_ACTIVATION_RSI) {
  cat("   Pré-RSI :", pre_rsi, "/100 —", signal_rsi, "\n\n")
} else {
  cat("   Pré-RSI non publié cette semaine (activation S",
      SEMAINE_ACTIVATION_RSI, ")\n\n")
}


# ── 7. CONSTRUIRE LES TABLEAUX COMMUNS ───────────────────────

cat(">> Construction des tableaux...\n")

tableau_grades <- paste0(
  "| Grade | Prix USD/kg | Prix CHF/kg | Note |\n",
  "|---|---|---|---|\n",
  "| **TSR20** | **", prix_actuel, "** | **",
  round(prix_actuel * 0.90, 3), "** | ",
  "Référence mondiale / World reference |\n",
  "| RSS3 | ", round(prix_actuel * 1.04, 3),
  " | ", round(prix_actuel * 1.04 * 0.90, 3),
  " | Premium quality |\n",
  "| TSR10 | ", round(prix_actuel * 0.97, 3),
  " | ", round(prix_actuel * 0.97 * 0.90, 3),
  " | Technical grade |\n",
  "| Latex concentré | ", round(prix_actuel * 0.82, 3),
  " | ", round(prix_actuel * 0.82 * 0.90, 3),
  " | Liquid material |\n\n"
)

tableau_geo <- paste0(
  "| Zone | Statut / Status | Rôle / Role |\n",
  "|---|---|---|\n",
  "| 🇨🇮 Côte d'Ivoire | 1er producteur africain | Export TSR20 |\n",
  "| 🇬🇭 Ghana | Producteur régional | Export latex |\n",
  "| 🇳🇬 Nigeria | Producteur émergent | Marché local |\n",
  "| 🇨🇲 Cameroun | Producteur régional | Export RSS |\n",
  "| 🇹🇭 Thaïlande / Thailand | 1er producteur mondial | Référence prix |\n",
  "| 🇲🇾 Malaisie / Malaysia | 2ème producteur mondial | SGX Futures |\n",
  "| 🇸🇬 Singapour / Singapore | Hub négoce mondial | Bourse SGX |\n",
  "| 🇪🇺 Europe | Principal importateur | Demande aval |\n\n"
)


# ── 8. ASSEMBLER LE RAPPORT FRANÇAIS ─────────────────────────

cat(">> Assemblage rapport FR...\n")

titre_fr <- paste0(
  "RubberSignal #", SEMAINE, " | TSR20 : ", prix_actuel, " USD/kg ",
  symbole_tendance, " | Semaine ", SEMAINE, " - ", ANNEE
)

rapport_fr <- paste0(
  
  "# ", titre_fr, "\n\n",
  "*Votre veille hebdomadaire sur le marché du caoutchouc naturel ",
  "— Afrique de l'Ouest & marchés mondiaux*\n\n",
  "---\n\n",
  
  "## 📊 Prix de la semaine\n\n",
  "### Grades de caoutchouc naturel\n\n",
  tableau_grades,
  
  "### Variation & tendance\n\n",
  "| Indicateur | Valeur |\n",
  "|---|---|\n",
  "| Variation vs période préc. | ",
  texte_variation_fr, " ", symbole_tendance, " |\n",
  "| Moyenne 3 mois | ",
  if (!is.na(moyenne_3m)) paste0(moyenne_3m, " USD/kg") else "—", " |\n",
  "| Moyenne 6 mois | ",
  if (!is.na(moyenne_6m)) paste0(moyenne_6m, " USD/kg") else "—", " |\n",
  "| Fourchette 12 mois | ",
  if (!is.na(min_12m) && !is.na(max_12m))
    paste0(min_12m, " – ", max_12m, " USD/kg")
  else "—", " |\n\n",
  
  "### Contexte géographique\n\n",
  tableau_geo,
  
  "## 🔍 Analyse de la semaine\n\n",
  texte_analyse_fr, "\n\n",
  "> *Cette analyse est générée automatiquement à partir de données publiques. ",
  "Elle ne constitue pas un conseil en investissement.*\n\n",
  
  # ── PRÉ-RSI FR (vide si semaine < SEMAINE_ACTIVATION_RSI) ──
  rapport_rsi_fr,
  if (nchar(texte_mc_fr) > 0 &&
      SEMAINE >= SEMAINE_ACTIVATION_RSI) texte_mc_fr else "",
  if (nchar(texte_boot_fr) > 0 &&
      SEMAINE >= SEMAINE_ACTIVATION_RSI) texte_boot_fr else "",
  if (nchar(texte_sc_fr) > 0 &&
      SEMAINE >= SEMAINE_ACTIVATION_RSI) texte_sc_fr else "",
  
  rapport_rsci_fr,
  "## 📝 Note éditoriale de la semaine\n\n",
  "*[À compléter manuellement — votre observation terrain : ",
  "prix local CI, activité plantation, retour d'un négociant...]*\n\n",
  "---\n\n",
  
  "## 🌿 Focus Côte d'Ivoire\n\n",
  "*La Côte d'Ivoire est le 1er producteur africain de caoutchouc naturel ",
  "et figure dans le top 5 mondial. RubberSignal vous apporte chaque semaine ",
  "une perspective terrain sur la filière ivoirienne.*\n\n",
  "**Données de référence CI :**\n\n",
  "- Production annuelle : ~1,2 million de tonnes\n",
  "- Principal port d'export : Abidjan\n",
  "- Organisme de référence : APROCAG\n\n",
  "---\n\n",
  
  "**RubberSignal** — Intelligence marché caoutchouc naturel | ",
  "Afrique de l'Ouest & marchés mondiaux\n\n",
  "*Source prix : ", source_prix, " | ",
  format(DATE_COLLECTE, "%d/%m/%Y"), "*\n\n",
  "*Pour vous abonner : rubbersignal.substack.com*\n"
)

cat("   Rapport FR :", nchar(rapport_fr), "caractères\n")


# ── 9. ASSEMBLER LE RAPPORT ANGLAIS ──────────────────────────

cat(">> Assemblage rapport EN...\n")

titre_en <- paste0(
  "RubberSignal #", SEMAINE, " | TSR20: ", prix_actuel, " USD/kg ",
  symbole_tendance, " | Week ", SEMAINE, " - ", ANNEE
)

rapport_en <- paste0(
  
  "# ", titre_en, "\n\n",
  "*Your weekly natural rubber market intelligence ",
  "— West Africa & global markets*\n\n",
  "---\n\n",
  
  "## 📊 Weekly Prices\n\n",
  "### Natural Rubber Grades\n\n",
  tableau_grades,
  
  "### Variation & Trend\n\n",
  "| Indicator | Value |\n",
  "|---|---|\n",
  "| Change vs previous period | ",
  texte_variation_en, " ", symbole_tendance, " |\n",
  "| 3-month average | ",
  if (!is.na(moyenne_3m)) paste0(moyenne_3m, " USD/kg") else "—", " |\n",
  "| 6-month average | ",
  if (!is.na(moyenne_6m)) paste0(moyenne_6m, " USD/kg") else "—", " |\n",
  "| 12-month range | ",
  if (!is.na(min_12m) && !is.na(max_12m))
    paste0(min_12m, " – ", max_12m, " USD/kg")
  else "—", " |\n\n",
  
  "### Geographic Context\n\n",
  tableau_geo,
  
  "## 🔍 Weekly Analysis\n\n",
  texte_analyse_en, "\n\n",
  "> *This analysis is automatically generated from public data. ",
  "It does not constitute investment advice.*\n\n",
  
  # ── PRÉ-RSI EN (vide si semaine < SEMAINE_ACTIVATION_RSI) ──
  rapport_rsi_en,
  if (nchar(texte_mc_en) > 0 &&
      SEMAINE >= SEMAINE_ACTIVATION_RSI) texte_mc_en else "",
  if (nchar(texte_boot_en) > 0 &&
      SEMAINE >= SEMAINE_ACTIVATION_RSI) texte_boot_en else "",
  if (nchar(texte_sc_en) > 0 &&
      SEMAINE >= SEMAINE_ACTIVATION_RSI) texte_sc_en else "",
  
  rapport_rsci_en,
  "## 📝 Editorial Note\n\n",
  "*[To be completed manually — your field observation this week: ",
  "local CI prices, plantation activity, trader feedback...]*\n\n",
  "---\n\n",
  
  "## 🌿 Ivory Coast Focus\n\n",
  "*Côte d'Ivoire is Africa's leading natural rubber producer ",
  "and ranks in the global top 5. RubberSignal delivers weekly ",
  "field-level insights on the Ivorian rubber industry.*\n\n",
  "**Key CI Reference Data:**\n\n",
  "- Annual production: ~1.2 million tonnes\n",
  "- Main export port: Abidjan\n",
  "- Reference body: APROCAG\n\n",
  "---\n\n",
  
  "**RubberSignal** — Natural Rubber Market Intelligence | ",
  "West Africa & Global Markets\n\n",
  "*Price source: ", source_prix, " | ",
  format(DATE_COLLECTE, "%B %d, %Y"), "*\n\n",
  "*Subscribe: rubbersignal.substack.com*\n"
)

cat("   Rapport EN :", nchar(rapport_en), "caractères\n")


# ── 10. SAUVEGARDER LES DEUX RAPPORTS ────────────────────────

cat("\n>> Sauvegarde des rapports...\n")

fichier_fr <- paste0("output/rubbersignal_S", SEMAINE, "_", ANNEE, "_FR.md")
fichier_en <- paste0("output/rubbersignal_S", SEMAINE, "_", ANNEE, "_EN.md")

writeLines(rapport_fr, fichier_fr, useBytes = FALSE)
writeLines(rapport_en, fichier_en, useBytes = FALSE)
cat("   FR :", fichier_fr, "\n")
cat("   EN :", fichier_en, "\n")

nettoyer_md <- function(texte) {
  texte %>%
    str_remove_all("\\*\\*|\\*|#{1,3} |`|\\[|\\]\\(.*?\\)") %>%
    str_replace_all("---", "──────────────────────────────")
}

writeLines(nettoyer_md(rapport_fr),
           paste0("output/rubbersignal_S", SEMAINE, "_", ANNEE, "_FR_plain.txt"),
           useBytes = FALSE)
writeLines(nettoyer_md(rapport_en),
           paste0("output/rubbersignal_S", SEMAINE, "_", ANNEE, "_EN_plain.txt"),
           useBytes = FALSE)
cat("   Versions plain text sauvegardées\n")

donnees$rapport <- list(
  titre_fr     = titre_fr,
  titre_en     = titre_en,
  date_rapport = as.character(DATE_COLLECTE),
  semaine      = SEMAINE,
  fichier_fr   = fichier_fr,
  fichier_en   = fichier_en,
  pre_rsi      = pre_rsi,
  pre_rsi_actif = SEMAINE >= SEMAINE_ACTIVATION_RSI
)
write_json(donnees, fichier_json, pretty = TRUE, auto_unbox = TRUE)
cat("   JSON final mis à jour\n")


# ── 11. APERÇU CONSOLE ───────────────────────────────────────

cat("\n", strrep("=", 60), "\n")
cat("APERÇU RAPPORT FR — Semaine", SEMAINE, "/", ANNEE, "\n")
cat(strrep("=", 60), "\n\n")
apercu_fr <- str_split(rapport_fr, "\n")[[1]]
cat(paste(head(apercu_fr, 35), collapse = "\n"))
cat("\n\n[... rapport complet dans :", fichier_fr, "]\n")

cat("\n", strrep("=", 60), "\n")
cat("APERÇU RAPPORT EN — Week", SEMAINE, "/", ANNEE, "\n")
cat(strrep("=", 60), "\n\n")
apercu_en <- str_split(rapport_en, "\n")[[1]]
cat(paste(head(apercu_en, 35), collapse = "\n"))
cat("\n\n[... full report in:", fichier_en, "]\n")


# ── 12. RÉSUMÉ FINAL ─────────────────────────────────────────

cat("\n\n", strrep("=", 60), "\n")
cat("RÉSUMÉ SCRIPT 03 — Semaine", SEMAINE, "/", ANNEE, "\n")
cat(strrep("=", 60), "\n")
cat("Rapport FR     :", fichier_fr, "\n")
cat("Rapport EN     :", fichier_en, "\n")
cat("Prix TSR20     :", prix_actuel, "USD/kg", symbole_tendance,
    "(", source_prix, ")\n")
if (!is.na(pre_rsi)) {
  cat("Pré-RSI        :", pre_rsi, "/100 —", signal_rsi, "\n")
  cat("RSI publié     :",
      if (SEMAINE >= SEMAINE_ACTIVATION_RSI) "OUI" else
        paste("NON — activation semaine", SEMAINE_ACTIVATION_RSI), "\n")
}
cat(strrep("=", 60), "\n")
cat("\nAction suivante :\n")
cat("1. Ouvrez", fichier_fr, "→ copiez dans Substack FR\n")
cat("2. Ouvrez", fichier_en, "→ copiez dans Substack EN\n")
cat("3. Complétez la note éditoriale terrain\n")
cat("4. Publiez !\n\n")
cat("Prochaine étape technique : GitHub Actions pipeline\n\n")
