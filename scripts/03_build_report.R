# =============================================================
# RUBBERSIGNAL.COM — Script 03 : Assemblage du rapport bilingue
# Auteur  : Martial Sahiri
# Version : 3.0 — bilingue FR / EN
# Objectif: Générer deux rapports Markdown séparés
#           FR : pour abonnés francophones (Afrique de l'Ouest, Europe)
#           EN : pour abonnés anglophones (Asie, traders internationaux)
# Prérequis: Scripts 01 et 02 déjà exécutés
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

cat("=== RUBBERSIGNAL — Rapport bilingue Semaine", SEMAINE, "/", ANNEE, "===\n\n")


# ── 3. LIRE LE JSON ENRICHI ──────────────────────────────────

fichier_json <- paste0("data/processed/rubbersignal_S", SEMAINE, "_", ANNEE, ".json")

if (!file.exists(fichier_json)) {
  stop(paste(
    "ERREUR : JSON introuvable :", fichier_json,
    "\nVérifiez que les scripts 01 et 02 ont bien été exécutés."
  ))
}

donnees <- read_json(fichier_json)
cat(">> JSON chargé :", fichier_json, "\n\n")


# ── 4. EXTRAIRE LES DONNÉES PRIX ─────────────────────────────

cat(">> Extraction des données prix...\n")

synthese <- donnees$prix$synthese

prix_actuel   <- synthese$prix_actuel   %||% NA
prix_chf      <- synthese$prix_chf      %||% NA
variation_pct <- synthese$variation_pct %||% NA
tendance      <- synthese$tendance      %||% "indéterminée"
moyenne_3m    <- synthese$moyenne_3m    %||% NA
moyenne_6m    <- synthese$moyenne_6m    %||% NA
min_12m       <- synthese$min_12m       %||% NA
max_12m       <- synthese$max_12m       %||% NA

symbole_tendance <- case_when(
  tendance == "hausse" ~ "▲",
  tendance == "baisse" ~ "▼",
  tendance == "stable" ~ "▬",
  TRUE                 ~ "~"
)

texte_variation_fr <- if (!is.na(variation_pct)) {
  paste0(if (variation_pct > 0) "+" else "", round(variation_pct, 2), "%")
} else "données indisponibles"

texte_variation_en <- if (!is.na(variation_pct)) {
  paste0(if (variation_pct > 0) "+" else "", round(variation_pct, 2), "%")
} else "data unavailable"

cat("   Prix actuel :", prix_actuel, "USD/kg\n")
cat("   Tendance    :", symbole_tendance, tendance, "(", texte_variation_fr, ")\n")


# ── 5. GÉNÉRER L'ANALYSE EN FRANÇAIS ─────────────────────────

cat("\n>> Rédaction analyse FR...\n")

generer_analyse_fr <- function(prix, variation, tendance, moy3m, moy6m, min12, max12) {
  
  intro <- case_when(
    tendance == "hausse" && variation > 2 ~
      paste0("Le marché du TSR20 affiche une progression notable cette semaine, ",
             "avec un prix atteignant **", prix, " USD/kg** (",
             "+", round(variation, 2), "% sur la période précédente)."),
    tendance == "hausse" ~
      paste0("Le TSR20 poursuit son mouvement haussier à **", prix, " USD/kg** ",
             "(+", round(variation, 2), "% vs période précédente)."),
    tendance == "baisse" && abs(variation) > 2 ~
      paste0("Le marché du TSR20 marque un repli significatif cette semaine, ",
             "le prix reculant à **", prix, " USD/kg** (",
             round(variation, 2), "% sur la période précédente)."),
    tendance == "baisse" ~
      paste0("Le TSR20 cède légèrement du terrain à **", prix, " USD/kg** ",
             "(", round(variation, 2), "% vs période précédente)."),
    TRUE ~
      paste0("Le TSR20 se stabilise cette semaine à **", prix, " USD/kg**, ",
             "sans variation significative par rapport à la période précédente.")
  )
  
  contexte_moy <- if (!is.na(moy3m) && !is.na(moy6m)) {
    position <- if (prix > moy3m) "au-dessus" else "en-dessous"
    paste0("Le prix actuel se situe ", position,
           " de sa moyenne sur 3 mois (", moy3m, " USD/kg) ",
           "et de sa moyenne sur 6 mois (", moy6m, " USD/kg).")
  } else ""
  
  contexte_range <- if (!is.na(min12) && !is.na(max12)) {
    pct_range <- round((prix - min12) / (max12 - min12) * 100)
    paste0("Sur les 12 derniers mois, le TSR20 a évolué entre ", min12,
           " et ", max12, " USD/kg — le niveau actuel représente ",
           pct_range, "% de cette fourchette.")
  } else ""
  
  paste(intro, contexte_moy, contexte_range, sep = " ")
}


# ── 6. GÉNÉRER L'ANALYSE EN ANGLAIS ──────────────────────────

cat(">> Rédaction analyse EN...\n")

generer_analyse_en <- function(prix, variation, tendance, moy3m, moy6m, min12, max12) {
  
  intro <- case_when(
    tendance == "hausse" && variation > 2 ~
      paste0("The TSR20 market posted a significant gain this week, ",
             "with prices reaching **", prix, " USD/kg** (",
             "+", round(variation, 2), "% vs previous period)."),
    tendance == "hausse" ~
      paste0("TSR20 continued its upward trend at **", prix, " USD/kg** ",
             "(+", round(variation, 2), "% vs previous period)."),
    tendance == "baisse" && abs(variation) > 2 ~
      paste0("The TSR20 market recorded a significant decline this week, ",
             "with prices falling to **", prix, " USD/kg** (",
             round(variation, 2), "% vs previous period)."),
    tendance == "baisse" ~
      paste0("TSR20 edged lower to **", prix, " USD/kg** ",
             "(", round(variation, 2), "% vs previous period)."),
    TRUE ~
      paste0("TSR20 stabilized this week at **", prix, " USD/kg**, ",
             "with no significant change compared to the previous period.")
  )
  
  contexte_moy <- if (!is.na(moy3m) && !is.na(moy6m)) {
    position <- if (prix > moy3m) "above" else "below"
    paste0("The current price stands ", position,
           " its 3-month average (", moy3m, " USD/kg) ",
           "and its 6-month average (", moy6m, " USD/kg).")
  } else ""
  
  contexte_range <- if (!is.na(min12) && !is.na(max12)) {
    pct_range <- round((prix - min12) / (max12 - min12) * 100)
    paste0("Over the past 12 months, TSR20 has traded between ", min12,
           " and ", max12, " USD/kg — the current level represents ",
           pct_range, "% of this range.")
  } else ""
  
  paste(intro, contexte_moy, contexte_range, sep = " ")
}

texte_analyse_fr <- generer_analyse_fr(
  prix = prix_actuel, variation = variation_pct, tendance = tendance,
  moy3m = moyenne_3m, moy6m = moyenne_6m, min12 = min_12m, max12 = max_12m
)

texte_analyse_en <- generer_analyse_en(
  prix = prix_actuel, variation = variation_pct, tendance = tendance,
  moy3m = moyenne_3m, moy6m = moyenne_6m, min12 = min_12m, max12 = max_12m
)

cat("   Analyses générées — OK\n")


# ── 7. CONSTRUIRE LES SECTIONS COMMUNES ──────────────────────

cat("\n>> Construction des sections...\n")

# Tableau des grades — identique FR/EN (chiffres universels)
tableau_grades <- paste0(
  "| Grade | Prix USD/kg | Prix CHF/kg | ",
  if (TRUE) "Note |" else "Note |", "\n",
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

# Tableau géographique — identique FR/EN
tableau_geo <- paste0(
  "| Zone | Statut / Status | Rôle / Role |\n",
  "|---|---|---|\n",
  "| 🇨🇮 Côte d'Ivoire | 1er producteur africain | Export TSR20 |\n",
  "| 🇬🇭 Ghana | Producteur régional | Export latex |\n",
  "| 🇳🇬 Nigeria | Producteur émergent | Marché local |\n",
  "| 🇨🇲 Cameroun | Producteur régional | Export RSS |\n",
  "| 🇹🇭 Thaïlande / Thailand | 1er producteur mondial | Référence prix / Price reference |\n",
  "| 🇲🇾 Malaisie / Malaysia | 2ème producteur mondial | SGX Futures |\n",
  "| 🇸🇬 Singapour / Singapore | Hub négoce mondial | Bourse SGX |\n",
  "| 🇪🇺 Europe | Principal importateur | Demande aval / Downstream demand |\n\n"
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
  "| Variation vs période préc. | ", texte_variation_fr, " ", symbole_tendance, " |\n",
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
  "Collecte automatisée chaque semaine. ",
  "Données : Banque Mondiale, IndexMundi, NewsAPI.\n\n",
  "*Pour vous abonner ou nous contacter : rubbersignal.substack.com*\n"
)

cat("   Rapport FR assemblé —", nchar(rapport_fr), "caractères\n")


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
  "| Change vs previous period | ", texte_variation_en, " ", symbole_tendance, " |\n",
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
  "Automated weekly collection. ",
  "Data sources: World Bank, IndexMundi, NewsAPI.\n\n",
  "*Subscribe or contact us: rubbersignal.substack.com*\n"
)

cat("   Rapport EN assemblé —", nchar(rapport_en), "caractères\n")


# ── 10. SAUVEGARDER LES DEUX RAPPORTS ────────────────────────

cat("\n>> Sauvegarde des rapports...\n")

# Rapport français
fichier_fr <- paste0("output/rubbersignal_S", SEMAINE, "_", ANNEE, "_FR.md")
writeLines(rapport_fr, fichier_fr, useBytes = FALSE)
cat("   Rapport FR :", fichier_fr, "\n")

# Rapport anglais
fichier_en <- paste0("output/rubbersignal_S", SEMAINE, "_", ANNEE, "_EN.md")
writeLines(rapport_en, fichier_en, useBytes = FALSE)
cat("   Rapport EN :", fichier_en, "\n")

# Versions texte brut
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
cat("   Versions texte brut sauvegardées\n")

# Mettre à jour le JSON final
donnees$rapport <- list(
  titre_fr      = titre_fr,
  titre_en      = titre_en,
  date_rapport  = as.character(DATE_COLLECTE),
  semaine       = SEMAINE,
  fichier_fr    = fichier_fr,
  fichier_en    = fichier_en
)
write_json(donnees, fichier_json, pretty = TRUE, auto_unbox = TRUE)
cat("   JSON final mis à jour\n")


# ── 11. APERÇU CONSOLE ───────────────────────────────────────

cat("\n", strrep("=", 60), "\n")
cat("APERÇU RAPPORT FR — Semaine", SEMAINE, "/", ANNEE, "\n")
cat(strrep("=", 60), "\n\n")
apercu_fr <- str_split(rapport_fr, "\n")[[1]]
cat(paste(head(apercu_fr, 30), collapse = "\n"))
cat("\n\n[... rapport complet dans :", fichier_fr, "]\n")

cat("\n", strrep("=", 60), "\n")
cat("APERÇU RAPPORT EN — Week", SEMAINE, "/", ANNEE, "\n")
cat(strrep("=", 60), "\n\n")
apercu_en <- str_split(rapport_en, "\n")[[1]]
cat(paste(head(apercu_en, 30), collapse = "\n"))
cat("\n\n[... full report in:", fichier_en, "]\n")


# ── 12. RÉSUMÉ FINAL ─────────────────────────────────────────

cat("\n\n", strrep("=", 60), "\n")
cat("RÉSUMÉ SCRIPT 03 — Semaine", SEMAINE, "/", ANNEE, "\n")
cat(strrep("=", 60), "\n")
cat("Rapport FR généré  :", fichier_fr, "\n")
cat("Rapport EN généré  :", fichier_en, "\n")
cat("Prix TSR20         :", prix_actuel, "USD/kg", symbole_tendance, "\n")
cat(strrep("=", 60), "\n")
cat("\nAction suivante :\n")
cat("1. Ouvrez", fichier_fr, "→ copiez dans Substack FR\n")
cat("2. Ouvrez", fichier_en, "→ copiez dans Substack EN\n")
cat("3. Complétez la note éditoriale dans les deux versions\n")
cat("4. Publiez les deux articles !\n\n")
cat("Prochaine étape technique : scripts/04_automate.R (GitHub Actions)\n\n")