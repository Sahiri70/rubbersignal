# =============================================================
# RUBBERSIGNAL.COM — Script 03 : Assemblage du rapport bilingue
# Auteur  : Martial Sahiri
# Version : 4.2 — corrections S45 : prix daté (fraîcheur 14 j), références
#           mensuelles FMI/Banque mondiale, RSCI v2 (taux en vigueur),
#           terrain/stocks/actualités affichés « indisponibles » si absents
#           (4.1 — intégration RSCI (RubberSignal Chain Index))
# Objectif: Générer deux rapports Markdown séparés FR + EN
# Prérequis: Scripts 01, 02 et 04 déjà exécutés
# Usage   : source("scripts/03_build_report.R")
# =============================================================

# ── 1. CHARGER LES PACKAGES ──────────────────────────────────

library(jsonlite)
library(tidyverse)
library(lubridate)

# Règles partagées (fraîcheur, libellés de mois, %||% tolérant aux NA/listes vides)
source(if (file.exists("scripts/00_regles.R")) "scripts/00_regles.R" else "00_regles.R")

# Lecture sûre d'un nombre du JSON (null, {} ou absent -> NA)
num <- function(x) {
  v <- suppressWarnings(as.numeric(unlist(x %||% NA)))
  if (length(v) != 1) NA_real_ else v
}
txt <- function(x, defaut = "") {
  v <- unlist(x %||% NA)
  if (length(v) != 1 || is.na(v)) defaut else as.character(v)
}


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
  # Terrain : publié uniquement s'il existe un relevé daté (statut "ok")
  terrain_ok     <- identical(txt(signaux$module6_terrain_ci$statut), "ok")
  note_terrain   <- if (terrain_ok)
    paste0(txt(signaux$module6_terrain_ci$libelle), " — ",
           txt(signaux$module6_terrain_ci$note_terrain))
  else ""
  stocks_statut  <- txt(signaux$module4_stocks$statut, "indisponible")
  rsi_absentes   <- unlist(signaux$pre_rsi$composantes_absentes %||% character())
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
  terrain_ok     <- FALSE
  note_terrain   <- ""
  stocks_statut  <- "indisponible"
  rsi_absentes   <- character()
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

# ── RSCI (RubberSignal Chain Index) — script 01 v4.1+ ────────

rsci_data  <- donnees$prix$rsci          %||% list()
bord_champ <- donnees$prix$bord_champ_ci %||% list()

rsci_pct       <- num(rsci_data$rsci_pct)
rsci_ecart_pts <- num(rsci_data$ecart_pts)
rsci_prix_sec  <- num(rsci_data$prix_planteur_sec_usd)
rsci_drc       <- num(rsci_data$drc_officiel)
rsci_drc_stat  <- txt(rsci_data$drc_statut, "")
rsci_mecanisme <- num(rsci_data$mecanisme_officiel_pct)
rsci_mec_date  <- txt(rsci_data$mecanisme_date_effet, "")
rsci_mois      <- txt(rsci_data$mois_rsci, txt(bord_champ$mois, ""))
rsci_ref_usd   <- num(rsci_data$reference_tsr20$valeur)
rsci_ref_mois  <- substr(txt(rsci_data$reference_tsr20$date_source, ""), 1, 7)
rsci_ref_src   <- txt(rsci_data$reference_tsr20$source, "")

aprom_fcfa     <- num(bord_champ$prix_fcfa)
aprom_mois     <- txt(bord_champ$mois, "")
aprom_type     <- txt(bord_champ$type_prix, "")
aprom_source   <- txt(bord_champ$source, "")
aprom_manquant <- isTRUE(bord_champ$mois_courant_manquant)
aprom_mois_cur <- txt(bord_champ$mois_courant, "")

cat("   RSCI        :",
    if (!is.na(rsci_pct))
      paste0(rsci_pct, "% (", rsci_mois, ", écart ", rsci_ecart_pts, " pts vs ",
             rsci_mecanisme, "% officiel)")
    else "indisponible",
    "\n")

# ── Prix hebdomadaire : publié comme « actuel » seulement s'il est frais ──
prix_actuel   <- num(synthese$prix_actuel)          # NA si > 14 jours ou sans date
prix_lu       <- num(synthese$prix_hebdo$valeur)    # valeur lue, quelle que soit sa date
date_prix     <- txt(synthese$date_source, NA_character_)
statut_prix   <- txt(synthese$statut_fraicheur, "date_inconnue")
prix_frais    <- !is.na(prix_actuel) && identical(statut_prix, "frais")
if (!prix_frais) prix_actuel <- NA_real_
prix_chf      <- num(synthese$prix_chf)
variation_pct <- num(synthese$variation_pct)
tendance      <- txt(synthese$tendance, "indéterminée")
moyenne_3m    <- num(synthese$moyenne_3m)
moyenne_6m    <- num(synthese$moyenne_6m)
min_12m       <- num(synthese$min_12m)
max_12m       <- num(synthese$max_12m)
source_prix   <- txt(synthese$source, "source inconnue")
if (!prix_frais) { variation_pct <- NA_real_; tendance <- "indéterminée"; prix_chf <- NA_real_ }

# ── Références mensuelles (sources libres) ──
ref_m     <- donnees$prix$reference_mensuelle %||% list()
fmi_val   <- num(ref_m$fmi_prubbusdm$valeur)
fmi_orig  <- num(ref_m$fmi_prubbusdm$valeur_orig)
fmi_per   <- txt(ref_m$fmi_prubbusdm$periode, "")
bm_val    <- num(ref_m$bm_tsr20$valeur)
bm_per    <- txt(ref_m$bm_tsr20$periode, "")

# ── Actualités (script 02) ──
actu        <- donnees$actualites %||% list()
actu_top    <- actu$top_newsletter %||% list()
actu_n      <- num(actu$total_articles)
actu_ok     <- identical(txt(actu$statut, if (isTRUE(actu_n > 0)) "ok" else "indisponible"), "ok") &&
               length(actu_top) > 0
actu_raison <- txt(actu$raison, "aucun article collecté")

symbole_tendance <- case_when(
  tendance == "hausse" ~ "▲",
  tendance == "baisse" ~ "▼",
  tendance == "stable" ~ "▬",
  TRUE                 ~ "~"
)

texte_variation_fr <- if (!is.na(variation_pct))
  paste0(if (variation_pct > 0) "+" else "", round(variation_pct, 2), "%") else "données indisponibles"
texte_variation_en <- if (!is.na(variation_pct))
  paste0(if (variation_pct > 0) "+" else "", round(variation_pct, 2), "%") else "data unavailable"

libelle_statut_fr <- c(frais = "à jour", perime = "PÉRIMÉE", date_inconnue = "date inconnue",
                       date_future = "date incohérente")
libelle_statut_en <- c(frais = "current", perime = "STALE", date_inconnue = "unknown date",
                       date_future = "inconsistent date")
statut_prix_fr <- unname(libelle_statut_fr[statut_prix] %||% statut_prix)
statut_prix_en <- unname(libelle_statut_en[statut_prix] %||% statut_prix)

cat("   Prix hebdo  :", prix_lu, "USD/kg (", source_prix, ", date", date_prix %||% "inconnue",
    ", statut", statut_prix, ")\n")
cat("   Prix actuel :", if (prix_frais) prix_actuel else "NON PUBLIÉ (cotation non fraîche)", "\n")
cat("   Réf. FMI    :", fmi_val, "USD/kg (", fmi_per, ") | BM TSR20 :", bm_val, "(", bm_per, ")\n")
cat("   Tendance    :", symbole_tendance, tendance,
    "(", texte_variation_fr, ")\n")


# ── 5. GÉNÉRER L'ANALYSE EN FRANÇAIS ─────────────────────────

cat("\n>> Rédaction analyse FR...\n")

generer_analyse_fr <- function(prix, variation, tendance,
                               moy3m, moy6m, min12, max12) {
  intro <- case_when(
    is.na(variation) ~
      paste0("Le TSR20 cote **", prix, " USD/kg** cette semaine ; variation hebdomadaire ",
             "non calculable (historique hebdomadaire insuffisant)."),
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
    is.na(variation) ~
      paste0("TSR20 is quoted at **", prix, " USD/kg** this week; weekly change ",
             "cannot be computed (insufficient weekly history)."),
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

# Phrase « références mensuelles » (uniquement des chiffres présents dans le JSON)
ref_mens_fr <- paste0(
  "Dernières références mensuelles disponibles : ",
  if (!is.na(fmi_val)) paste0("FMI (FRED PRUBBUSDM) ", rs_mois_libelle(fmi_per, "fr"), " : **",
                              fmi_val, " USD/kg**") else "FMI : indisponible",
  " ; ",
  if (!is.na(bm_val)) paste0("Banque mondiale TSR20 ", rs_mois_libelle(bm_per, "fr"), " : **",
                             bm_val, " USD/kg**") else "Banque mondiale : indisponible",
  ".")
ref_mens_en <- paste0(
  "Latest available monthly references: ",
  if (!is.na(fmi_val)) paste0("IMF (FRED PRUBBUSDM) ", rs_mois_libelle(fmi_per, "en"), ": **",
                              fmi_val, " USD/kg**") else "IMF: unavailable",
  "; ",
  if (!is.na(bm_val)) paste0("World Bank TSR20 ", rs_mois_libelle(bm_per, "en"), ": **",
                             bm_val, " USD/kg**") else "World Bank: unavailable",
  ".")

texte_analyse_fr <- if (prix_frais) paste(generer_analyse_fr(
  prix=prix_actuel, variation=variation_pct, tendance=tendance,
  moy3m=moyenne_3m, moy6m=moyenne_6m, min12=min_12m, max12=max_12m
), ref_mens_fr) else paste0(
  "**Pas de cotation hebdomadaire TSR20 de moins de 14 jours cette semaine** ",
  "(dernière cotation lue : ", if (!is.na(prix_lu)) paste0(prix_lu, " USD/kg") else "aucune",
  ", ", source_prix, ", date ", rs_date_fr(date_prix), ", statut ", statut_prix_fr,
  ") : aucun prix « actuel » n'est publié. ", ref_mens_fr)
texte_analyse_en <- if (prix_frais) paste(generer_analyse_en(
  prix=prix_actuel, variation=variation_pct, tendance=tendance,
  moy3m=moyenne_3m, moy6m=moyenne_6m, min12=min_12m, max12=max_12m
), ref_mens_en) else paste0(
  "**No weekly TSR20 quote less than 14 days old this week** ",
  "(last quote read: ", if (!is.na(prix_lu)) paste0(prix_lu, " USD/kg") else "none",
  ", ", source_prix, ", dated ", rs_date_fr(date_prix), ", status ", statut_prix_en,
  "): no \"current\" price is published. ", ref_mens_en)

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
    if (terrain_ok && nchar(note_terrain) > 0)
      paste0("| 🌿 Terrain CI | ", note_terrain, " |\n")
    else "| 🌿 Terrain CI | Pas de relevé terrain cette semaine |\n",
    if (nchar(signal_geo) > 0)
      paste0("| 🌍 Géopolitique | ", signal_geo, " |\n")
    else "",
    "| 📦 Stocks mondiaux | Donnée indisponible |\n",
    if (length(rsi_absentes) > 0)
      paste0("\n> *Composante(s) absente(s) cette semaine : ", paste(rsi_absentes, collapse = ", "),
             ". Le score est calculé sur les composantes disponibles (poids renormalisés).*\n")
    else "",
    "\n> *Le Pré-RSI (RubberSignal Index) est un score composite 0–100 ",
    "intégrant météo, devises, demande industrielle, géopolitique et, lorsqu'un relevé daté existe, terrain CI. ",
    "Score > 55 = marché haussier. Score < 45 = marché baissier. ",
    "Indicateur RubberSignal en cours de validation (pondérations non encore backtestées).*\n\n",
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
    if (terrain_ok && nchar(note_terrain) > 0)
      paste0("| 🌿 CI Field data | ", note_terrain, " |\n")
    else "| 🌿 CI Field data | No field report this week |\n",
    if (nchar(signal_geo) > 0)
      paste0("| 🌍 Geopolitics | ", signal_geo, " |\n")
    else "",
    "| 📦 Global stocks | Data unavailable |\n",
    if (length(rsi_absentes) > 0)
      paste0("\n> *Component(s) missing this week: ", paste(rsi_absentes, collapse = ", "),
             ". The score uses the available components (weights renormalised).*\n")
    else "",
    "\n> *The Pre-RSI (RubberSignal Index) is a composite score 0–100 ",
    "combining weather, currencies, industrial demand, geopolitics and, when a dated report exists, CI field data. ",
    "Score > 55 = bullish market. Score < 45 = bearish market. ",
    "RubberSignal indicator under validation (weights not yet backtested).*\n\n",
    "---\n\n"
  )
} else ""

# ── RSCI : section rapport (FR/EN) ───────────────────────────

# Pas de feu tricolore : l'écart RSCI / part officielle n'est pas un indicateur de
# conformité (la part officielle s'applique au prix de référence NET, le RSCI au
# prix BRUT). Voir METHODOLOGIE_Indices_RubberSignal_v1, section 2.3.
icone_rsci <- function(ecart) "📊"

signe <- function(x) if (!is.na(x) && x > 0) "+" else ""

# Libellé du régime officiel en vigueur au mois du prix (taux lu dans le JSON)
regime_fr <- function(taux) {
  if (is.na(taux)) return("régime officiel inconnu")
  if (taux == 66) "accord interprofessionnel du 16 février 2026, entériné par le CHPC le 29 avril 2026 (en vigueur depuis le 1er mai 2026)"
  else if (taux == 63) "décision provisoire CHPH n°0037 (mai 2022)"
  else if (taux == 61) "régime antérieur à mai 2022"
  else "régime officiel (voir JSON)"
}
regime_en <- function(taux) {
  if (is.na(taux)) return("unknown official regime")
  if (taux == 66) "inter-professional agreement of 16 February 2026, endorsed by the CHPC on 29 April 2026 (in force since 1 May 2026)"
  else if (taux == 63) "provisional CHPH decision n°0037 (May 2022)"
  else if (taux == 61) "regime prior to May 2022"
  else "official regime (see JSON)"
}

alerte_apromac_fr <- if (aprom_manquant)
  paste0("> ⚠️ *Prix APROMAC de ", rs_mois_libelle(aprom_mois_cur, "fr"),
         " non encore saisi : dernier mois disponible ", rs_mois_libelle(aprom_mois, "fr"), ".*\n\n") else ""
alerte_apromac_en <- if (aprom_manquant)
  paste0("> ⚠️ *APROMAC price for ", rs_mois_libelle(aprom_mois_cur, "en"),
         " not yet entered: latest available month ", rs_mois_libelle(aprom_mois, "en"), ".*\n\n") else ""

drc_txt_fr <- paste0(round(rsci_drc * 100), " %", if (nzchar(rsci_drc_stat)) ", à confirmer après la réforme 2026" else "")
drc_txt_en <- paste0(round(rsci_drc * 100), "%", if (nzchar(rsci_drc_stat)) ", to be confirmed after the 2026 reform" else "")

rapport_rsci_fr <- if (!is.na(rsci_pct)) {
  paste0(
    "## 🇨🇮 RSCI — RubberSignal Chain Index (", rs_mois_libelle(rsci_mois, "fr"), ")\n\n",
    "### ", icone_rsci(rsci_ecart_pts),
    " Part planteur (DRC-corrigée) : **", rsci_pct, "%**\n\n",
    "*Le RSCI mesure la part du prix de référence TSR20 du mois précédent ",
    "effectivement captée par le planteur ivoirien, après correction ",
    "du taux de caoutchouc sec (DRC) de référence.*\n\n",
    alerte_apromac_fr,
    "| Indicateur | Valeur |\n",
    "|---|---|\n",
    "| Prix APROMAC (", rs_mois_libelle(aprom_mois, "fr"),
      if (nzchar(aprom_type)) paste0(", ", aprom_type) else "", ") | ", aprom_fcfa, " FCFA/kg |\n",
    "| Référence TSR20 (", rs_mois_libelle(rsci_ref_mois, "fr"), ", Banque mondiale) | ",
      rsci_ref_usd, " USD/kg |\n",
    "| Prix planteur, équivalent sec (DRC ", drc_txt_fr, ") | ", rsci_prix_sec, " USD/kg |\n",
    "| **RSCI** (part planteur) | **", rsci_pct, "%** |\n",
    "| Part officielle en vigueur | ", rsci_mecanisme, "% |\n",
    "| Différence RSCI − part officielle (indicative) | ", signe(rsci_ecart_pts), rsci_ecart_pts, " points |\n\n",
    "Pour le prix de ", rs_mois_libelle(aprom_mois, "fr"), ", la part officielle du planteur est de ",
    rsci_mecanisme, " % — ", regime_fr(rsci_mecanisme), " — cadre : Loi N°2017-540.\n\n",
    "> ⚠️ *Avertissement de lecture : la part officielle s'applique au prix de référence **net** ",
    "(prix de Singapour diminué des coûts de transformation, de logistique, d'export et de la fiscalité, ",
    "non publiés en détail), alors que le RSCI se rapporte au prix international **brut**. ",
    "La différence n'indique donc pas un non-respect du mécanisme : elle mesure la part du prix mondial ",
    "qui reste au planteur après l'ensemble des coûts de la filière.*\n\n",
    "> *Méthodologie v2 : référence TSR20 = moyenne mensuelle Banque mondiale du mois précédant le prix ",
    "APROMAC (le mécanisme 2026 se fonde sur les cotations SGX-SICOM TSR20 du mois précédent) ; ",
    "DRC de référence ", drc_txt_fr, ". La prime TSR10 n'est pas encore intégrée.*\n\n",
    "---\n\n"
  )
} else if (!is.na(aprom_fcfa)) {
  paste0(
    "## 🇨🇮 RSCI — RubberSignal Chain Index\n\n",
    alerte_apromac_fr,
    "**RSCI non calculé cette semaine** : référence TSR20 du mois précédant le prix APROMAC (",
    rs_mois_libelle(rs_mois_precedent(aprom_mois), "fr"), ") indisponible. ",
    "Prix APROMAC de ", rs_mois_libelle(aprom_mois, "fr"), " : ", aprom_fcfa, " FCFA/kg.\n\n",
    "---\n\n"
  )
} else ""

rapport_rsci_en <- if (!is.na(rsci_pct)) {
  paste0(
    "## 🇨🇮 RSCI — RubberSignal Chain Index (", rs_mois_libelle(rsci_mois, "en"), ")\n\n",
    "### ", icone_rsci(rsci_ecart_pts),
    " Farmer share (DRC-adjusted): **", rsci_pct, "%**\n\n",
    "*RSCI measures the share of the previous month's TSR20 reference price ",
    "actually captured by the Ivorian farmer, after adjustment for ",
    "the reference Dry Rubber Content (DRC).*\n\n",
    alerte_apromac_en,
    "| Indicator | Value |\n",
    "|---|---|\n",
    "| APROMAC price (", rs_mois_libelle(aprom_mois, "en"),
      if (nzchar(aprom_type)) paste0(", ", if (aprom_type == "carreau usine") "factory-gate" else aprom_type) else "",
      ") | ", aprom_fcfa, " FCFA/kg |\n",
    "| TSR20 reference (", rs_mois_libelle(rsci_ref_mois, "en"), ", World Bank) | ",
      rsci_ref_usd, " USD/kg |\n",
    "| Dry-equivalent farmer price (DRC ", drc_txt_en, ") | ", rsci_prix_sec, " USD/kg |\n",
    "| **RSCI** (farmer share) | **", rsci_pct, "%** |\n",
    "| Official share in force | ", rsci_mecanisme, "% |\n",
    "| Difference RSCI − official share (indicative) | ", signe(rsci_ecart_pts), rsci_ecart_pts, " points |\n\n",
    "For the ", rs_mois_libelle(aprom_mois, "en"), " price, the official farmer share is ",
    rsci_mecanisme, "% — ", regime_en(rsci_mecanisme), " — framework: Law N°2017-540.\n\n",
    "> ⚠️ *Reading note: the official share applies to the **net** reference price ",
    "(Singapore price minus processing, logistics, export costs and levies, not published in detail), ",
    "whereas RSCI is measured against the **gross** international price. ",
    "The difference therefore does not indicate non-compliance with the mechanism: it measures the share ",
    "of the world price left to the farmer after all supply-chain costs.*\n\n",
    "> *V2 methodology: TSR20 reference = World Bank monthly average for the month preceding the APROMAC ",
    "price (the 2026 mechanism is based on the previous month's SGX-SICOM TSR20 quotes); ",
    "reference DRC ", drc_txt_en, ". The TSR10 premium is not yet included.*\n\n",
    "---\n\n"
  )
} else if (!is.na(aprom_fcfa)) {
  paste0(
    "## 🇨🇮 RSCI — RubberSignal Chain Index\n\n",
    alerte_apromac_en,
    "**RSCI not computed this week**: TSR20 reference for the month preceding the APROMAC price (",
    rs_mois_libelle(rs_mois_precedent(aprom_mois), "en"), ") unavailable. ",
    "APROMAC price for ", rs_mois_libelle(aprom_mois, "en"), ": ", aprom_fcfa, " FCFA/kg.\n\n",
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

# Tableau des références datées (FR / EN) : chaque ligne porte sa date et sa source
ligne_hebdo_fr <- paste0(
  "| Cotation hebdomadaire (", txt(synthese$indice, "TSR20"), ") | ",
  if (!is.na(prix_lu)) paste0(prix_lu, " USD/kg") else "—", " | ", rs_date_fr(date_prix), " | ",
  source_prix, " | ",
  if (prix_frais) "à jour" else paste0("**", statut_prix_fr, "** — non publiée comme prix actuel"), " |\n")
ligne_hebdo_en <- paste0(
  "| Weekly quote (", txt(synthese$indice, "TSR20"), ") | ",
  if (!is.na(prix_lu)) paste0(prix_lu, " USD/kg") else "—", " | ", rs_date_fr(date_prix), " | ",
  source_prix, " | ",
  if (prix_frais) "current" else paste0("**", statut_prix_en, "** — not published as current price"), " |\n")

tableau_refs_fr <- paste0(
  "| Série | Valeur | Date de la valeur | Source | Statut |\n",
  "|---|---|---|---|---|\n",
  ligne_hebdo_fr,
  "| Prix mondial FMI (mensuel) | ",
  if (!is.na(fmi_val)) paste0(fmi_val, " USD/kg (", fmi_orig, " cents/lb)") else "indisponible", " | ",
  rs_mois_libelle(fmi_per, "fr"), " | FMI via FRED (PRUBBUSDM) | dernière valeur disponible |\n",
  "| TSR20 Banque mondiale (mensuel) | ",
  if (!is.na(bm_val)) paste0(bm_val, " USD/kg") else "indisponible", " | ",
  rs_mois_libelle(bm_per, "fr"), " | Banque mondiale, Pink Sheet | dernière valeur disponible |\n\n"
)
tableau_refs_en <- paste0(
  "| Series | Value | Value date | Source | Status |\n",
  "|---|---|---|---|---|\n",
  ligne_hebdo_en,
  "| IMF world price (monthly) | ",
  if (!is.na(fmi_val)) paste0(fmi_val, " USD/kg (", fmi_orig, " cents/lb)") else "unavailable", " | ",
  rs_mois_libelle(fmi_per, "en"), " | IMF via FRED (PRUBBUSDM) | latest available value |\n",
  "| World Bank TSR20 (monthly) | ",
  if (!is.na(bm_val)) paste0(bm_val, " USD/kg") else "unavailable", " | ",
  rs_mois_libelle(bm_per, "en"), " | World Bank, Pink Sheet | latest available value |\n\n"
)

# Grades dérivés : uniquement si la cotation hebdomadaire est fraîche
# (coefficients 1.04 / 0.97 / 0.82 et CHF x 0.90 codés en dur : à remplacer, voir rapport batisseur)
tableau_grades <- if (!prix_frais) {
  "*Grades dérivés non affichés : pas de cotation hebdomadaire fraîche. / Derived grades not shown: no fresh weekly quote.*\n\n"
} else paste0(
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

# Le titre ne porte un prix que s'il est frais (<= 14 jours), avec sa date
titre_fr <- if (prix_frais) paste0(
  "RubberSignal #", SEMAINE, " | TSR20 : ", prix_actuel, " USD/kg (", rs_date_fr(date_prix), ") ",
  if (symbole_tendance != "~") paste0(symbole_tendance, " ") else "", "| Semaine ", SEMAINE, " - ", ANNEE
) else paste0(
  "RubberSignal #", SEMAINE, " | Semaine ", SEMAINE, " - ", ANNEE,
  " | Cotation hebdomadaire TSR20 indisponible"
)

# Actualités : liste datée ou mention explicite d'indisponibilité
section_actu <- function(langue = "fr") {
  titre <- if (langue == "fr") "## 📰 Actualités de la semaine\n\n" else "## 📰 News of the week\n\n"
  if (!actu_ok) {
    return(paste0(titre, if (langue == "fr")
      paste0("*Actualités indisponibles cette semaine (", actu_raison, ").*\n\n---\n\n")
      else paste0("*News unavailable this week (",
                  if (grepl("erreur", actu_raison)) "NewsAPI error"
                  else if (grepl("filtre", actu_raison)) "no article passed the relevance filter"
                  else "NewsAPI returned no article for our queries", ").*\n\n---\n\n")))
  }
  lignes <- vapply(actu_top, function(a) paste0(
    "- **", txt(a$titre, "—"), "** — ", txt(a$source, "—"), ", ", txt(a$date_fr, "—"),
    if (nzchar(txt(a$lien))) paste0(" ([lien](", txt(a$lien), "))") else "", "\n"), character(1))
  paste0(titre, paste(lignes, collapse = ""), "\n---\n\n")
}

rapport_fr <- paste0(
  
  "# ", titre_fr, "\n\n",
  "*Votre veille hebdomadaire sur le marché du caoutchouc naturel ",
  "— Afrique de l'Ouest & marchés mondiaux*\n\n",
  "---\n\n",
  
  "## 📊 Prix de la semaine\n\n",
  "### Références datées\n\n",
  tableau_refs_fr,
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
  section_actu("fr"),
  "## 📝 Note éditoriale de la semaine\n\n",
  "*[À compléter manuellement — votre observation terrain : ",
  "prix local CI, activité plantation, retour d'un négociant...]*\n\n",
  "---\n\n",
  
  "## 🌿 Focus Côte d'Ivoire\n\n",
  "*La Côte d'Ivoire est le 1er producteur africain de caoutchouc naturel ",
  "et figure dans le top 5 mondial. RubberSignal suit chaque semaine ",
  "la filière ivoirienne.*\n\n",
  "**Données de référence CI :**\n\n",
  "- Production annuelle : environ 1,8 million de tonnes ",
  "(904 000 t au 1er semestre 2026 selon SunSirs)\n",
  "- Principal port d'export : Abidjan\n",
  "- Organisme de référence : APROCAG\n\n",
  "---\n\n",
  
  "**RubberSignal** — Intelligence marché caoutchouc naturel | ",
  "Afrique de l'Ouest & marchés mondiaux\n\n",
  "*Source prix hebdomadaire : ", source_prix, " (cotation du ", rs_date_fr(date_prix), ", ",
  statut_prix_fr, ") | Références mensuelles : FMI (FRED PRUBBUSDM), Banque mondiale (Pink Sheet) | ",
  "Rapport du ", format(DATE_COLLECTE, "%d/%m/%Y"), "*\n\n",
  "*Pour vous abonner : rubbersignal.substack.com*\n"
)

cat("   Rapport FR :", nchar(rapport_fr), "caractères\n")


# ── 9. ASSEMBLER LE RAPPORT ANGLAIS ──────────────────────────

cat(">> Assemblage rapport EN...\n")

titre_en <- if (prix_frais) paste0(
  "RubberSignal #", SEMAINE, " | TSR20: ", prix_actuel, " USD/kg (", rs_date_fr(date_prix), ") ",
  if (symbole_tendance != "~") paste0(symbole_tendance, " ") else "", "| Week ", SEMAINE, " - ", ANNEE
) else paste0(
  "RubberSignal #", SEMAINE, " | Week ", SEMAINE, " - ", ANNEE,
  " | Weekly TSR20 quote unavailable"
)

rapport_en <- paste0(
  
  "# ", titre_en, "\n\n",
  "*Your weekly natural rubber market intelligence ",
  "— West Africa & global markets*\n\n",
  "---\n\n",
  
  "## 📊 Weekly Prices\n\n",
  "### Dated references\n\n",
  tableau_refs_en,
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
  section_actu("en"),
  "## 📝 Editorial Note\n\n",
  "*[To be completed manually — your field observation this week: ",
  "local CI prices, plantation activity, trader feedback...]*\n\n",
  "---\n\n",
  
  "## 🌿 Ivory Coast Focus\n\n",
  "*Côte d'Ivoire is Africa's leading natural rubber producer ",
  "and ranks in the global top 5. RubberSignal tracks ",
  "the Ivorian rubber industry every week.*\n\n",
  "**Key CI Reference Data:**\n\n",
  "- Annual production: about 1.8 million tonnes ",
  "(904,000 t in H1 2026 according to SunSirs)\n",
  "- Main export port: Abidjan\n",
  "- Reference body: APROCAG\n\n",
  "---\n\n",
  
  "**RubberSignal** — Natural Rubber Market Intelligence | ",
  "West Africa & Global Markets\n\n",
  "*Weekly price source: ", source_prix, " (quote dated ", rs_date_fr(date_prix), ", ",
  statut_prix_en, ") | Monthly references: IMF (FRED PRUBBUSDM), World Bank (Pink Sheet) | ",
  "Report of ", format(DATE_COLLECTE, "%B %d, %Y"), "*\n\n",
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
write_json(donnees, fichier_json, pretty = TRUE, auto_unbox = TRUE, na = "null", null = "null")
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
