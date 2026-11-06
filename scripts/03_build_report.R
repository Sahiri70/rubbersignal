# =============================================================
# RUBBERSIGNAL.COM — Script 03 : Assemblage du rapport bilingue
# Auteur  : Martial Sahiri
# Version : 4.4 — mission 3 (nov. 2026) : section « Matrice Afrique de l'Ouest — RAPI-CI »
#           (douanes UE, bloc rapi_ci du JSON, script 06)
# Version : 4.3 — mission 2 (nov. 2026) : colonne Nature (T/O/D/E/J) et version
#           de méthode ; réglage RS_PUBLIER_SOURCES_SOUS_LICENCE ; Pré-RSI v2
#           (sens, poids effectifs, non publié sous 3 composantes) ; fourchette
#           de volatilité ; grades dérivés, CHF x 0.90, tableau géographique et
#           « Focus CI » statiques retirés ; scénarios météo retirés
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
  pr             <- signaux$pre_rsi %||% list()
  # v2 : score NA si moins de 3 composantes (non publié) — jamais de valeur par défaut
  pre_rsi        <- if (isTRUE(pr$publie)) num(pr$score) else NA_real_
  pre_rsi_raison <- txt(pr$raison_non_publie, "Pré-RSI absent du fichier signaux")
  signal_rsi     <- txt(pr$signal, "Non publié")
  signal_rsi_en  <- txt(pr$signal_en, "Not published")
  score_offre    <- num(signaux$module1_meteo$score_offre_mondiale)
  signal_offre   <- txt(signaux$module1_meteo$signal_offre, "")
  signal_cny_r   <- txt(signaux$module2_devises$signal_cny, "")
  signal_myr_r   <- txt(signaux$module2_devises$signal_myr, "")
  dev            <- signaux$module2_devises %||% list()
  score_demande  <- num(signaux$module3_demande_aval$score_demande)
  signal_demande <- txt(signaux$module3_demande_aval$signal_demande, "")
  confiance      <- signaux$module3_demande_aval$confiance_industrie %||% list()
  # Terrain : publié uniquement s'il existe un relevé daté (statut "ok")
  terrain_ok     <- identical(txt(signaux$module6_terrain_ci$statut), "ok")
  note_terrain   <- if (terrain_ok)
    paste0(txt(signaux$module6_terrain_ci$libelle), " — ",
           txt(signaux$module6_terrain_ci$note_terrain))
  else ""
  stocks_statut  <- txt(signaux$module4_stocks$statut, "indisponible")
  rsi_absentes   <- unlist(pr$composantes_absentes %||% character())
  signal_geo     <- txt(signaux$module7_geopolitique$signal, "")
  geo_nb         <- num(signaux$module7_geopolitique$nb_articles)
  shipping       <- signaux$module5_shipping %||% list()
  cat(">> Signaux faibles chargés — Pré-RSI :", if (is.na(pre_rsi)) paste("non publié :", pre_rsi_raison) else paste(pre_rsi, "/100"), "\n")
  if (SEMAINE >= SEMAINE_ACTIVATION_RSI) {
    cat("   Pré-RSI activé dans le rapport (semaine", SEMAINE,
        "≥", SEMAINE_ACTIVATION_RSI, ")\n\n")
  } else {
    cat("   Pré-RSI désactivé cette semaine (activation semaine",
        SEMAINE_ACTIVATION_RSI, ")\n\n")
  }
} else {
  signaux        <- list()
  pr             <- list()
  pre_rsi        <- NA_real_
  pre_rsi_raison <- "script 04 non exécuté"
  signal_rsi     <- "Non publié"
  signal_rsi_en  <- "Not published"
  score_offre    <- NA
  signal_offre   <- ""
  signal_cny_r   <- ""
  signal_myr_r   <- ""
  dev            <- list()
  score_demande  <- NA
  signal_demande <- ""
  confiance      <- list()
  terrain_ok     <- FALSE
  note_terrain   <- ""
  stocks_statut  <- "indisponible"
  rsi_absentes   <- character()
  signal_geo     <- ""
  geo_nb         <- NA_real_
  shipping       <- list()
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
  # v4.3 : scénarios météo retirés (élasticité et chocs d'offre sans source)
  texte_sc_fr   <- ""
  texte_sc_en   <- ""
  # Bootstrap publié seulement si le Pré-RSI l'est
  if (is.na(pre_rsi)) texte_boot_fr <- texte_boot_en <- ""
  cat(">> Monte Carlo chargé\n\n")
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
# IOSCO : nature et version de méthode lues dans le JSON (script 01 v4.2)
aprom_nat      <- txt(rsci_data$prix_apromac$nature, txt(bord_champ$nature, "—"))
aprom_version  <- txt(bord_champ$version_methode, rs_version_methode("apromac"))
aprom_date     <- txt(rsci_data$prix_apromac$date_source, NA_character_)
aprom_releve   <- grepl("releve", txt(bord_champ$date_source_type, ""))
aprom_date_fr  <- if (is.na(aprom_date)) ", date de source inconnue" else paste0(if (aprom_releve) ", relevé presse du " else ", publié le ", rs_date_fr(aprom_date))
aprom_date_en  <- if (is.na(aprom_date)) ", source date unknown" else paste0(if (aprom_releve) ", press reading of " else ", published ", rs_date_fr(aprom_date))
rsci_ref_nat   <- txt(rsci_data$reference_tsr20$nature, "—")
rsci_nat       <- txt(rsci_data$nature, "—")
rsci_version   <- txt(rsci_data$version_methode, "version inconnue")

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

# ── Licence (décision D1 en attente) : réglage unique, défaut TRUE = affichage actuel ──
PUBLIER_LICENCE <- rs_publier_sous_licence()
serie_hebdo     <- txt(synthese$indice, "INCONNUE")
hebdo_publiable <- PUBLIER_LICENCE || rs_serie_publiable(serie_hebdo)
nature_hebdo    <- txt(synthese$prix_hebdo$nature, txt(rs_serie_info(serie_hebdo)$nature, "—"))
cat("   Licence     : sources sous licence publiées =", PUBLIER_LICENCE,
    "| série hebdo", serie_hebdo, if (rs_serie_publiable(serie_hebdo)) "(publiable)" else "(sous licence)", "\n")
# Le prix hebdo et tout ce qui en dérive (variation, moyennes, CHF) suivent le réglage
if (!hebdo_publiable) { variation_pct <- NA_real_; tendance <- "indéterminée"; prix_chf <- NA_real_ }

# Taux USD/CHF réellement collecté (BCE via script 01), daté — plus de x 0.90
taux_chf      <- num(synthese$taux_usd_chf$valeur)
taux_chf_date <- txt(synthese$taux_usd_chf$date_source, NA_character_)
taux_chf_src  <- txt(synthese$taux_usd_chf$source, "BCE")

# ── Références mensuelles (sources libres) ──
ref_m     <- donnees$prix$reference_mensuelle %||% list()
fmi_val   <- num(ref_m$fmi_prubbusdm$valeur)
fmi_orig  <- num(ref_m$fmi_prubbusdm$valeur_orig)
fmi_per   <- txt(ref_m$fmi_prubbusdm$periode, "")
bm_val    <- num(ref_m$bm_tsr20$valeur)
bm_per    <- txt(ref_m$bm_tsr20$periode, "")
fmi_nat   <- txt(ref_m$fmi_prubbusdm$nature, "—")
bm_nat    <- txt(ref_m$bm_tsr20$nature, "—")
fmi_stat  <- txt(ref_m$fmi_prubbusdm$statut, "indisponible")
bm_stat   <- txt(ref_m$bm_tsr20$statut, "indisponible")

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

texte_analyse_fr <- if (!hebdo_publiable) paste0(
  "**Cotation hebdomadaire réservée (licence)** : elle n'est pas reproduite dans cette publication. ",
  ref_mens_fr) else if (prix_frais) paste(generer_analyse_fr(
  prix=prix_actuel, variation=variation_pct, tendance=tendance,
  moy3m=moyenne_3m, moy6m=moyenne_6m, min12=min_12m, max12=max_12m
), ref_mens_fr) else paste0(
  "**Pas de cotation hebdomadaire TSR20 de moins de 14 jours cette semaine** ",
  "(dernière cotation lue : ", if (!is.na(prix_lu)) paste0(prix_lu, " USD/kg") else "aucune",
  ", ", source_prix, ", date ", rs_date_fr(date_prix), ", statut ", statut_prix_fr,
  ") : aucun prix « actuel » n'est publié. ", ref_mens_fr)
texte_analyse_en <- if (!hebdo_publiable) paste0(
  "**Weekly quote reserved (licence)**: it is not reproduced in this publication. ",
  ref_mens_en) else if (prix_frais) paste(generer_analyse_en(
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
  # mêmes seuils que rs_pre_rsi_libelle (00_regles.R) : > 55 haussier, < 45 baissier
  if (score >= 70) "⬆️"
  else if (score > 55) "↗️"
  else if (score >= 45) "➡️"
  else if (score > 30) "↘️"
  else "⬇️"
}

# v4.3 (mission 2) : Pré-RSI v2. Chaque composante affiche sa mesure, son SENS
# pour le prix et son poids effectif ; le score n'est publié qu'avec au moins
# 3 composantes disponibles (règle et poids : scripts/00_regles.R, section 9).
section_pre_rsi <- function(langue = "fr") {
  fr <- langue == "fr"
  if (SEMAINE < SEMAINE_ACTIVATION_RSI || length(pr) == 0) return("")
  sens_lib <- function(k) {
    s <- txt(pr$sens[[k]], NA_character_)
    if (is.na(s)) return(if (fr) "— absente" else "— missing")
    if (fr) c(hausse = "↑ hausse", baisse = "↓ baisse", neutre = "→ neutre")[[s]]
    else c(hausse = "↑ up", baisse = "↓ down", neutre = "→ neutral")[[s]]
  }
  poids_lib <- function(k) {
    pe <- num(pr$poids_effectifs[[k]]); pn <- RS_PRE_RSI_POIDS[[k]]
    if (!is.finite(pe) || pe == 0) return(if (fr) paste0("0 % (nominal ", pn * 100, " %)") else paste0("0% (nominal ", pn * 100, "%)"))
    if (fr) paste0(round(pe * 100, 1), " % (nominal ", pn * 100, " %)") else paste0(round(pe * 100, 1), "% (nominal ", pn * 100, "%)")
  }
  cny  <- num(dev$USD_CNY); ref_cny <- num(dev$reference_cny$valeur)
  mesure <- list(
    meteo = if (is.finite(score_offre)) paste0(if (fr) "Score d'offre " else "Supply score ", score_offre, "/100 — ", signal_offre)
            else if (fr) "Données météo indisponibles" else "Weather data unavailable",
    devises = if (is.finite(cny)) paste0(signal_cny_r, " — USD/CNY ", cny, " (", rs_date_fr(txt(dev$date, NA)), ", ",
                                       txt(dev$source, ""), ") ; ", if (fr) "réf. moyenne 12 mois " else "ref. 12-month average ",
                                       if (is.finite(ref_cny)) paste0(ref_cny, " (FRED DEXCHUS)") else if (fr) "indisponible" else "unavailable")
              else if (fr) "Taux de change indisponible (aucune valeur de secours)" else "Exchange rate unavailable (no fallback value)",
    demande = if (is.finite(score_demande)) paste0("Score ", score_demande, "/100 — ", signal_demande,
                                                  if (is.finite(num(confiance$z_moyen))) paste0(
                                                    if (fr) " (confiance industrielle OCDE : z moyen " else " (OECD industrial confidence: mean z ",
                                                    num(confiance$z_moyen), ", ", num(confiance$n_utilisees), if (fr) " pays)" else " countries)") else "")
              else if (fr) "Données de demande indisponibles" else "Demand data unavailable",
    terrain = if (terrain_ok && nchar(note_terrain) > 0) paste0(note_terrain, " (nature D)")
              else if (fr) "Pas de relevé terrain cette semaine" else "No field report this week",
    geo = if (is.finite(geo_nb)) signal_geo else paste0(signal_geo, if (fr) " — composante retirée" else " — component removed")
  )
  noms <- if (fr) RS_PRE_RSI_NOMS_FR else RS_PRE_RSI_NOMS_EN
  lignes <- vapply(names(RS_PRE_RSI_POIDS), function(k)
    paste0("| ", noms[[k]], " | ", mesure[[k]], " | ", sens_lib(k), " | ", poids_lib(k), " |\n"), character(1))

  fret_bdi <- shipping$fret$BDI %||% list()
  ligne_fret <- if (identical(txt(fret_bdi$statut), "ok"))
    paste0("BDI ", num(fret_bdi$valeur), " pts (", rs_date_fr(txt(fret_bdi$date_valeur, NA)), ", ", txt(fret_bdi$source), ")")
  else if (fr) "Indisponible : aucune source datée (BDI, tarif CI-UE, délai Abidjan-Rotterdam)"
  else "Unavailable: no dated source (BDI, CI-EU rate, Abidjan-Rotterdam transit time)"
  myr <- num(dev$USD_MYR)
  ligne_myr <- if (is.finite(myr)) paste0(signal_myr_r, " — USD/MYR ", myr) else if (fr) "Taux indisponible" else "Rate unavailable"

  entete <- if (fr) "| Composante | Mesure | Sens pour le prix | Poids effectif |\n|---|---|---|---|\n"
            else "| Component | Measure | Price direction | Effective weight |\n|---|---|---|---|\n"
  hors <- if (fr) paste0(
    "\n*Hors score :* \n\n| Signal | Valeur |\n|---|---|\n",
    "| 💱 Ringgit malaisien (MYR) | ", ligne_myr, " |\n",
    "| 🚢 Fret maritime | ", ligne_fret, " |\n",
    "| 📦 Stocks mondiaux | Donnée indisponible |\n")
  else paste0(
    "\n*Outside the score:* \n\n| Signal | Value |\n|---|---|\n",
    "| 💱 Malaysian ringgit (MYR) | ", ligne_myr, " |\n",
    "| 🚢 Ocean freight | ", ligne_fret, " |\n",
    "| 📦 Global stocks | Data unavailable |\n")

  tete <- if (!is.na(pre_rsi)) {
    paste0("### ", icone_rsi(pre_rsi), if (fr) " RubberSignal Index : **" else " RubberSignal Index: **",
           pre_rsi, " / 100**\n\n*", if (fr) signal_rsi else signal_rsi_en, "*\n\n")
  } else {
    paste0(if (fr) "**Pré-RSI non publié cette semaine** : " else "**Pre-RSI not published this week**: ",
           if (fr) pre_rsi_raison else sub("composante\\(s\\) disponible\\(s\\) sur", "component(s) available out of",
                                           sub("minimum", "minimum", pre_rsi_raison)), ".\n\n")
  }
  absentes <- if (length(rsi_absentes) > 0 && !is.na(pre_rsi)) paste0(
    if (fr) "\n> *Composante(s) absente(s) : " else "\n> *Missing component(s): ",
    paste(unname(noms[rsi_absentes]), collapse = ", "),
    if (fr) ". Les poids des composantes disponibles sont renormalisés à 100 %.*\n"
    else ". Weights of the available components are renormalised to 100%.*\n") else ""
  note <- if (fr) paste0(
    "\n> *Pré-RSI : score composite 0–100 orienté prix (> 55 haussier, < 45 baissier, entre les deux neutre). ",
    "Poids nominaux : météo 25 %, devises 20 %, demande 25 %, terrain 15 %, géopolitique 15 %, renormalisés sur ",
    "les composantes disponibles ; non publié avec moins de ", RS_PRE_RSI_MIN_COMPOSANTES, " composantes. ",
    "Pondérations non backtestées. Nature : E (estimation calculée) — ", rs_version_methode("pre_rsi"),
    ". Analyse, pas un indice de prix.*\n\n---\n\n")
  else paste0(
    "\n> *Pre-RSI: composite 0–100 score oriented to price (> 55 bullish, < 45 bearish, neutral in between). ",
    "Nominal weights: weather 25%, currencies 20%, demand 25%, field 15%, geopolitics 15%, renormalised over ",
    "the available components; not published with fewer than ", RS_PRE_RSI_MIN_COMPOSANTES, " components. ",
    "Weights not backtested. Nature: E (calculated estimate) — ", rs_version_methode("pre_rsi"),
    ". Analysis, not a price index.*\n\n---\n\n")
  paste0(if (fr) "## 📡 Signaux faibles — Pré-RSI\n\n" else "## 📡 Weak Signals — Pre-RSI\n\n",
         tete, entete, paste(lignes, collapse = ""), absentes, hors, note)
}

rapport_rsi_fr <- section_pre_rsi("fr")
rapport_rsi_en <- section_pre_rsi("en")

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
    "| Indicateur | Valeur | Nature |\n",
    "|---|---|---|\n",
    "| Prix APROMAC (", rs_mois_libelle(aprom_mois, "fr"),
      if (nzchar(aprom_type)) paste0(", ", aprom_type) else "", aprom_date_fr, ") | ", aprom_fcfa, " FCFA/kg | ", aprom_nat, " |\n",
    "| Référence TSR20 (", rs_mois_libelle(rsci_ref_mois, "fr"), ", Banque mondiale) | ",
      rsci_ref_usd, " USD/kg | ", rsci_ref_nat, " |\n",
    "| Prix planteur, équivalent sec (DRC ", drc_txt_fr, ") | ", rsci_prix_sec, " USD/kg | E |\n",
    "| **RSCI** (part planteur) | **", rsci_pct, "%** | ", rsci_nat, " |\n",
    "| Part officielle en vigueur | ", rsci_mecanisme, "% | O |\n",
    "| Différence RSCI − part officielle (indicative) | ", signe(rsci_ecart_pts), rsci_ecart_pts, " points | E |\n\n",
    "*", RS_LEGENDE_NATURE_FR, ". Version de méthode : ", rsci_version, " ; ", aprom_version, ".*\n\n",
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
    "| Indicator | Value | Nature |\n",
    "|---|---|---|\n",
    "| APROMAC price (", rs_mois_libelle(aprom_mois, "en"),
      if (nzchar(aprom_type)) paste0(", ", if (aprom_type == "carreau usine") "factory-gate" else aprom_type) else "", aprom_date_en,
      ") | ", aprom_fcfa, " FCFA/kg | ", aprom_nat, " |\n",
    "| TSR20 reference (", rs_mois_libelle(rsci_ref_mois, "en"), ", World Bank) | ",
      rsci_ref_usd, " USD/kg | ", rsci_ref_nat, " |\n",
    "| Dry-equivalent farmer price (DRC ", drc_txt_en, ") | ", rsci_prix_sec, " USD/kg | E |\n",
    "| **RSCI** (farmer share) | **", rsci_pct, "%** | ", rsci_nat, " |\n",
    "| Official share in force | ", rsci_mecanisme, "% | O |\n",
    "| Difference RSCI − official share (indicative) | ", signe(rsci_ecart_pts), rsci_ecart_pts, " points | E |\n\n",
    "*", RS_LEGENDE_NATURE_EN, ". Method version: ", rsci_version, "; ", aprom_version, ".*\n\n",
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


# ── 6C. MATRICE AFRIQUE DE L'OUEST — RAPI-CI (script 06) ─────
# v4.4 (mission 3) : tous les chiffres viennent du bloc `rapi_ci` du JSON
# (aucun calcul ici, sauf la mise en forme). La décote est un écart de prix
# observé : jamais présentée comme un « manque à gagner » ni comme un jugement.

rapi <- donnees$rapi_ci %||% list()

section_rapi <- function(langue = "fr") {
  fr <- langue == "fr"
  titre <- if (fr) "## 🌍 Matrice Afrique de l'Ouest — RAPI-CI\n\n" else "## 🌍 West Africa Matrix — RAPI-CI\n\n"
  if (length(rapi) == 0)
    return(paste0(titre, if (fr) "*Module douanes UE non exécuté cette semaine (script 06) : section indisponible.*\n\n---\n\n"
                         else "*EU customs module not run this week (script 06): section unavailable.*\n\n---\n\n"))
  mois <- rapi$mois %||% list()
  if (length(mois) == 0)
    return(paste0(titre, if (fr) "*Données douanières Eurostat indisponibles cette semaine et aucune valeur en base : section non publiée.*\n\n---\n\n"
                         else "*Eurostat customs data unavailable this week and no value in the database: section not published.*\n\n---\n\n"))
  pct <- if (fr) " %" else "%"
  f3 <- function(x) rs_rapi_fmt(x, 3)                    # USD/kg et €/kg : 3 décimales (convention)
  fpct <- function(x, signe = FALSE) { s <- rs_rapi_fmt(x, 1, signe); if (s == "—") s else paste0(s, pct) }
  maj <- txt(rapi$date_maj_eurostat, NA_character_)
  six <- utils::tail(mois, 6)
  drapeaux <- function(m) {
    d <- c(if (isTRUE(m$faible_liquidite)) (if (fr) "faible liquidité" else "low liquidity"),
           if (isTRUE(m$revise)) (if (fr) "révisé" else "revised"),
           if (isTRUE(m$alerte_variation)) paste0("variation ", fpct(m$variation_ci_pct, signe = TRUE)),
           if (!isTRUE(m$asie_complet)) (if (fr) "composite Asie incomplet" else "Asian composite incomplete"),
           if (isTRUE(m$taux_source_secondaire)) (if (fr) "taux FRED (secondaire)" else "FRED rate (secondary)"))
    if (length(d) == 0) "—" else paste(d, collapse = ", ")
  }
  nat <- paste0(txt(rapi$nature$prix_cif, "T"), " / ", txt(rapi$nature$decote, "E"))
  lignes <- vapply(six, function(m) paste0(
    "| ", rs_mois_libelle(txt(m$periode), langue), " | ", rs_rapi_fmt(m$volume_ci_t, 0), " | ", f3(m$ci_eur_kg), " | ",
    f3(m$ci_usd_kg), " | ", f3(m$asie_usd_kg), " | ", fpct(m$decote_pct), " | ", nat, " | ", drapeaux(m), " |\n"),
    character(1))
  entete <- if (fr) "| Mois | Volume CI (t) | CI €/kg | CI USD/kg | Composite Asie USD/kg | Décote CI/Asie | Nature | Drapeaux |\n|---|---|---|---|---|---|---|---|\n"
            else "| Month | CI volume (t) | CI €/kg | CI USD/kg | Asian composite USD/kg | CI/Asia gap | Nature | Flags |\n|---|---|---|---|---|---|---|---|\n"
  der <- rapi$derniere %||% mois[[length(mois)]]
  der_mois <- rs_mois_libelle(txt(der$periode), langue)
  statut <- txt(rapi$statut, "indisponible"); fraicheur <- txt(rapi$statut_fraicheur, "indisponible")
  etat <- if (statut != "ok") {
    if (fr) paste0("> ⚠️ *Eurostat Comext indisponible cette semaine : valeurs reprises de la base RubberSignal (extraction du ",
                   rs_date_fr(txt(rapi$date_extraction_valeurs, NA)), ", mise à jour Eurostat du ", rs_date_fr(maj),
                   "). Elles ne sont pas nouvelles. Dernière valeur disponible : ", der_mois, ".*\n\n")
    else paste0("> ⚠️ *Eurostat Comext unavailable this week: values taken from the RubberSignal database (extracted ",
                rs_date_fr(txt(rapi$date_extraction_valeurs, NA)), ", Eurostat update of ", rs_date_fr(maj),
                "). They are not new. Latest available value: ", der_mois, ".*\n\n")
  } else if (fraicheur != "derniere_valeur_disponible") {
    if (fr) paste0("> ⚠️ *Mise à jour Eurostat du ", rs_date_fr(maj), " : plus de ", RS_MAX_JOURS_MENSUEL,
                   " jours, valeurs **périmées**. Dernière valeur disponible : ", der_mois, ".*\n\n")
    else paste0("> ⚠️ *Eurostat update of ", rs_date_fr(maj), ": more than ", RS_MAX_JOURS_MENSUEL,
                " days old, values are **stale**. Latest available value: ", der_mois, ".*\n\n")
  } else {
    if (fr) paste0("*Dernière valeur disponible : ", der_mois, " (mise à jour Eurostat du ", rs_date_fr(maj),
                   " ; Eurostat publie un mois environ 6 à 7 semaines après sa fin).*\n\n")
    else paste0("*Latest available value: ", der_mois, " (Eurostat update of ", rs_date_fr(maj),
                "; Eurostat publishes a month about 6 to 7 weeks after it ends).*\n\n")
  }
  fx_note <- if (isTRUE(rapi$taux_change$secondaire_utilise) || any(vapply(six, function(m) isTRUE(m$taux_source_secondaire), logical(1))))
    (if (fr) "Taux USD/EUR : FRED EXUSEU (source secondaire déclarée, BCE indisponible)"
     else "USD/EUR rate: FRED EXUSEU (declared secondary source, ECB unavailable)")
  else (if (fr) "Taux USD/EUR : moyenne mensuelle BCE (EXR.M.USD.EUR.SP00.A)"
        else "USD/EUR rate: ECB monthly average (EXR.M.USD.EUR.SP00.A)")
  seuil_t <- num(rapi$seuils$faible_liquidite_t); seuil_v <- num(rapi$seuils$variation_pct)
  legende <- if (fr) paste0(
    "*", RS_LEGENDE_NATURE_FR, " (prix CIF : T ; décote : E). Drapeaux : faible liquidité = moins de ", seuil_t,
    " t importées de Côte d'Ivoire dans le mois ; variation = plus de ", seuil_v,
    " % d'un mois à l'autre (valeur publiée avec mention, jamais supprimée) ; révisé = mois révisé par Eurostat, ",
    "historique conservé. Version de méthode : ", txt(rapi$version_methode, rs_version_methode("rapi_ci")), ".*\n\n")
  else paste0(
    "*", RS_LEGENDE_NATURE_EN, " (CIF prices: T; gap: E). Flags: low liquidity = less than ", seuil_t,
    " t imported from Côte d'Ivoire in the month; variation = more than ", seuil_v,
    "% month on month (value published with a note, never removed); revised = month revised by Eurostat, ",
    "history kept. Method version: ", txt(rapi$version_methode, rs_version_methode("rapi_ci")), ".*\n\n")
  lecture <- if (fr) paste0(
    "*Lecture : décote = prix CIF UE ivoirien ÷ composite asiatique (Thaïlande, Indonésie, Malaisie, Vietnam, pondéré par les volumes) − 1. ",
    "Une décote négative signifie que le caoutchouc ivoirien entre dans l'UE moins cher que l'asiatique : c'est un écart de prix observé.*\n\n")
  else paste0(
    "*Reading: gap = Ivorian EU CIF price ÷ Asian composite (Thailand, Indonesia, Malaysia, Vietnam, volume-weighted) − 1. ",
    "A negative gap means Ivorian rubber enters the EU cheaper than Asian rubber: it is an observed price difference.*\n\n")
  limites <- unlist(if (fr) rapi$limites_fr else rapi$limites_en) %||% (if (fr) RS_RAPI_LIMITES_FR else RS_RAPI_LIMITES_EN)
  encadre <- paste0(
    if (fr) "> **Limites**\n" else "> **Limitations**\n",
    paste0("> - ", limites, "\n", collapse = ""),
    if (fr) "> - FOB Abidjan : **non publié** (aucune source de fret publique, datée et réutilisable n'est retenue).\n\n"
    else "> - FOB Abidjan: **not published** (no public, dated and reusable freight source has been selected).\n\n")
  source_txt <- if (fr) paste0(
    "*Source : ", txt(rapi$source, RS_RAPI_SOURCE), ", ", RS_RAPI_PRODUIT_LIBELLE, ", mise à jour du ", rs_date_fr(maj),
    ". ", fx_note, ". Comparaison : TSR20 Banque mondiale (Pink Sheet), hors indice. Calcul RubberSignal.*\n\n---\n\n")
  else paste0(
    "*Source: ", txt(rapi$source, RS_RAPI_SOURCE), ", CN ", sub("^NC ", "", RS_RAPI_PRODUIT_LIBELLE), ", updated ", rs_date_fr(maj),
    ". ", fx_note, ". Comparison: World Bank TSR20 (Pink Sheet), outside the index. RubberSignal calculation.*\n\n---\n\n")
  paste0(titre, etat, entete, paste(lignes, collapse = ""), "\n", legende,
         rs_rapi_phrase(der, langue), "\n\n", lecture, encadre, source_txt)
}

rapport_rapi_fr <- section_rapi("fr")
rapport_rapi_en <- section_rapi("en")
cat("   Section RAPI-CI :", if (length(rapi) == 0) "absente (script 06 non exécuté)"
    else paste0(txt(rapi$statut, "?"), ", dernière période ", txt(rapi$derniere_periode, "—")), "\n")

cat(">> Construction des tableaux...\n")

# v4.3 (mission 2) : tableau unique (rs_tableau_prix, 00_regles.R) avec une
# colonne Nature (IOSCO) ; les séries sous licence en sont retirées quand
# RS_PUBLIER_SOURCES_SOUS_LICENCE = FALSE. Grades dérivés (x 1.04 / 0.97 /
# 0.82, sans source) et tableau géographique statique : RETIRÉS. Le CHF
# utilise le taux USD/CHF BCE réellement collecté (plus de x 0.90).
lignes_prix <- function(langue = "fr") {
  fr <- langue == "fr"
  stat_m <- function(s) if (identical(s, "derniere_valeur_disponible")) (if (fr) "dernière valeur disponible" else "latest available value")
                        else if (identical(s, "perime")) (if (fr) "**PÉRIMÉE**" else "**STALE**")
                        else (if (fr) "indisponible" else "unavailable")
  l <- list(
    list(serie = serie_hebdo,
         libelle = paste0(if (fr) "Cotation hebdomadaire (" else "Weekly quote (", serie_hebdo, ")"),
         valeur = if (!is.na(prix_lu)) paste0(prix_lu, " USD/kg") else "—", date = rs_date_fr(date_prix),
         source = source_prix, nature = nature_hebdo,
         statut = if (prix_frais) (if (fr) "à jour" else "current")
                  else paste0("**", if (fr) statut_prix_fr else statut_prix_en, "** — ",
                              if (fr) "non publiée comme prix actuel" else "not published as current price")))
  if (prix_frais && is.finite(prix_chf) && is.finite(taux_chf))
    l[[length(l) + 1]] <- list(serie = serie_hebdo,
      libelle = if (fr) "Équivalent en francs suisses" else "Swiss franc equivalent",
      valeur = paste0(prix_chf, " CHF/kg"), date = rs_date_fr(taux_chf_date),
      source = paste0(if (fr) "cotation ci-dessus × USD/CHF " else "quote above × USD/CHF ", taux_chf, " (", taux_chf_src, ")"),
      nature = "E", statut = if (fr) "conversion" else "conversion")
  c(l, list(
    list(serie = "FMI_PRUBBUSDM", libelle = if (fr) "Prix mondial FMI (mensuel)" else "IMF world price (monthly)",
         valeur = if (!is.na(fmi_val)) paste0(fmi_val, " USD/kg (", fmi_orig, " cents/lb)") else (if (fr) "indisponible" else "unavailable"),
         date = rs_mois_libelle(fmi_per, langue), source = "FMI via FRED (PRUBBUSDM)", nature = fmi_nat, statut = stat_m(fmi_stat)),
    list(serie = "BM_TSR20", libelle = if (fr) "TSR20 Banque mondiale (mensuel)" else "World Bank TSR20 (monthly)",
         valeur = if (!is.na(bm_val)) paste0(bm_val, " USD/kg") else (if (fr) "indisponible" else "unavailable"),
         date = rs_mois_libelle(bm_per, langue), source = if (fr) "Banque mondiale, Pink Sheet" else "World Bank, Pink Sheet",
         nature = bm_nat, statut = stat_m(bm_stat))))
}
legende_prix <- function(langue = "fr") {
  fr <- langue == "fr"
  paste0("*", if (fr) RS_LEGENDE_NATURE_FR else RS_LEGENDE_NATURE_EN, ". ",
         if (fr) "Version de méthode : " else "Method version: ",
         if (hebdo_publiable) paste0(txt(synthese$prix_hebdo$version_methode, rs_version_methode("prix_hebdo")), " ; ") else "",
         txt(ref_m$bm_tsr20$version_methode, rs_version_methode("reference_mensuelle")), ".*\n\n")
}
tableau_refs_fr <- paste0(rs_tableau_prix(lignes_prix("fr"), "fr", PUBLIER_LICENCE), legende_prix("fr"))
tableau_refs_en <- paste0(rs_tableau_prix(lignes_prix("en"), "en", PUBLIER_LICENCE), legende_prix("en"))


# ── 8. ASSEMBLER LE RAPPORT FRANÇAIS ─────────────────────────

cat(">> Assemblage rapport FR...\n")

# Le titre ne porte un prix que s'il est frais (<= 14 jours), daté, et publiable
# selon le réglage licence (sinon : TSR20 Banque mondiale du dernier mois)
hebdo_titre <- list(valeur = prix_actuel, date = date_prix, frais = prix_frais, serie = serie_hebdo)
bm_titre    <- list(valeur = bm_val, periode = bm_per)
titre_fr <- rs_titre_rapport("fr", SEMAINE, ANNEE, hebdo_titre, bm_titre, symbole_tendance, PUBLIER_LICENCE)

# Variation & tendance : dérivées de la cotation hebdomadaire (même règle de licence)
section_variation <- function(langue = "fr") {
  fr <- langue == "fr"
  if (!hebdo_publiable)
    return(paste0(if (fr) "### Variation & tendance\n\n*Calculées sur la cotation hebdomadaire : "
                  else "### Variation & Trend\n\n*Computed on the weekly quote: ",
                  if (fr) RS_MENTION_LICENCE_FR else RS_MENTION_LICENCE_EN, ".*\n\n"))
  paste0(
    if (fr) "### Variation & tendance\n\n| Indicateur | Valeur |\n|---|---|\n| Variation vs période préc. | "
    else "### Variation & Trend\n\n| Indicator | Value |\n|---|---|\n| Change vs previous period | ",
    if (fr) texte_variation_fr else texte_variation_en, " ", symbole_tendance, " |\n",
    if (fr) "| Moyenne 3 mois | " else "| 3-month average | ",
    if (!is.na(moyenne_3m)) paste0(moyenne_3m, " USD/kg") else "—", " |\n",
    if (fr) "| Moyenne 6 mois | " else "| 6-month average | ",
    if (!is.na(moyenne_6m)) paste0(moyenne_6m, " USD/kg") else "—", " |\n",
    if (fr) "| Fourchette 12 mois | " else "| 12-month range | ",
    if (!is.na(min_12m) && !is.na(max_12m)) paste0(min_12m, " – ", max_12m, " USD/kg") else "—", " |\n\n")
}

source_hebdo_pied <- function(langue = "fr") {
  fr <- langue == "fr"
  if (!hebdo_publiable) return(if (fr) RS_MENTION_LICENCE_FR else RS_MENTION_LICENCE_EN)
  if (fr) paste0("Source prix hebdomadaire : ", source_prix, " (cotation du ", rs_date_fr(date_prix), ", ", statut_prix_fr, ")")
  else paste0("Weekly price source: ", source_prix, " (quote dated ", rs_date_fr(date_prix), ", ", statut_prix_en, ")")
}

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
  section_variation("fr"),

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
  rapport_rapi_fr,
  section_actu("fr"),
  "## 📝 Note éditoriale de la semaine\n\n",
  "*[À compléter manuellement — votre observation terrain : ",
  "prix local CI, activité plantation, retour d'un négociant...]*\n\n",
  "---\n\n",
  
  # v4.3 : « Focus Côte d'Ivoire » statique retiré (« top 5 mondial », production
  # SunSirs, « APROCAG » : aucune source dans le JSON). Le suivi CI = section RSCI.

  "**RubberSignal** — Intelligence marché caoutchouc naturel | ",
  "Afrique de l'Ouest & marchés mondiaux\n\n",
  "*", source_hebdo_pied("fr"), " | Références mensuelles : FMI (FRED PRUBBUSDM), Banque mondiale (Pink Sheet) | Douanes UE : Eurostat Comext (DS-045409) | ",
  "Rapport du ", format(DATE_COLLECTE, "%d/%m/%Y"), "*\n\n",
  "*Calculs préparés et contrôlés par des outils automatisés, dont des modèles d'IA, puis validés par ",
  "l'évaluateur responsable (Martial Sahiri).*\n\n",
  "*Pour vous abonner : rubbersignal.substack.com*\n"
)

cat("   Rapport FR :", nchar(rapport_fr), "caractères\n")


# ── 9. ASSEMBLER LE RAPPORT ANGLAIS ──────────────────────────

cat(">> Assemblage rapport EN...\n")

titre_en <- rs_titre_rapport("en", SEMAINE, ANNEE, hebdo_titre, bm_titre, symbole_tendance, PUBLIER_LICENCE)

rapport_en <- paste0(
  
  "# ", titre_en, "\n\n",
  "*Your weekly natural rubber market intelligence ",
  "— West Africa & global markets*\n\n",
  "---\n\n",
  
  "## 📊 Weekly Prices\n\n",
  "### Dated references\n\n",
  tableau_refs_en,
  section_variation("en"),

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
  rapport_rapi_en,
  section_actu("en"),
  "## 📝 Editorial Note\n\n",
  "*[To be completed manually — your field observation this week: ",
  "local CI prices, plantation activity, trader feedback...]*\n\n",
  "---\n\n",
  
  "**RubberSignal** — Natural Rubber Market Intelligence | ",
  "West Africa & Global Markets\n\n",
  "*", source_hebdo_pied("en"), " | Monthly references: IMF (FRED PRUBBUSDM), World Bank (Pink Sheet) | EU customs: Eurostat Comext (DS-045409) | ",
  "Report of ", format(DATE_COLLECTE, "%B %d, %Y"), "*\n\n",
  "*Calculations prepared and checked by automated tools, including AI models, then validated by ",
  "the responsible assessor (Martial Sahiri).*\n\n",
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
  pre_rsi_actif = SEMAINE >= SEMAINE_ACTIVATION_RSI,
  pre_rsi_publie = !is.na(pre_rsi),
  publier_sources_sous_licence = PUBLIER_LICENCE,
  serie_hebdo_publiee = hebdo_publiable,
  rapi_ci_statut = txt(rapi$statut, "absent"),
  rapi_ci_derniere_periode = txt(rapi$derniere_periode, NA_character_),
  versions_methode = setNames(as.list(RS_METHODES$version), RS_METHODES$cle)
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
