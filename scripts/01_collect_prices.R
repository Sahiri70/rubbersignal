# =============================================================
# RUBBERSIGNAL.COM — Script 01 : Collecte des prix
# Auteur  : Martial Sahiri
# Version : 4.2 — collecte automatique + saisies datees (APROMAC dans
#           data/reference/prix_apromac.csv) ; nature T/O/D/E/J et version
#           de methode sur chaque valeur publiee (novembre 2026)
#           (4.0 — octobre 2026 : sources automatiques)
# Objectif: Recuperer chaque semaine le prix TSR20, le taux de change,
#           le prix bord-champ CI (APROMAC) et calculer le RSCI.
# Changements vs v3.1 :
#   - taux EUR/USD, JPY, CNY, CHF : automatiques (BCE)
#   - prix TSR20 : chaine LGM/MRB > rtas.sg (MRB) > SGX > IndexMundi,
#     plus de prix de secours code en dur (arret si tout echoue)
#   - prix APROMAC : extrait automatiquement de la presse, historise
#     dans data/reference/apromac_history.csv ; statut de fraicheur
#   - historique TSR20 : reel (data/reference/tsr20_hebdo.csv),
#     l'ancien historique 2.25/2.20/... code en dur est supprime
#   - SICOM : automatique (SGX) ; TOCOM et SHFE : non collectes
#     automatiquement (NA, jamais de valeur figee)
#   - bloc controle_qualite ajoute au JSON
# Changements v4.1 (novembre 2026, corrections rapport S45) :
#   - cotation hebdo datee (date_source) ; > 14 jours ou sans date :
#     prix_actuel = NA, jamais presentee comme prix actuel
#   - references mensuelles libres : FMI PRUBBUSDM (FRED, cents/lb ->
#     USD/kg) et Banque mondiale Pink Sheet TSR20
#   - APROMAC : saisie datee data/reference/prix_apromac.csv (prioritaire)
#     + historique presse ; alerte si le mois en cours manque
#   - RSCI v2 : taux officiel en vigueur au mois du prix (61/63/66 %),
#     reference TSR20 du mois precedent, DRC 60 % a confirmer
# Changements v4.2 (mission 2, novembre 2026) :
#   - champs IOSCO `nature` et `version_methode` (scripts/00_regles.R, section 7)
#   - `publiable` (registre des licences, 00_regles.R section 8)
#   - historique Banque mondiale TSR20 sur 61 mois (estimation de volatilite, script 05)
#   - taux USD/CHF (BCE) date dans la synthese
# Changements v4.3 (mission 3, novembre 2026) :
#   - alimentation de la base data/db/ (scripts/00_base.R, section 11 bis)
# Usage   : source("scripts/01_collect_prices.R")   (depuis la racine)
# =============================================================

# ── 1. CHARGER LES PACKAGES ET LE MODULE DE SOURCES ──────────

library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(rvest)

if (file.exists("scripts/00_sources.R")) {
  source("scripts/00_sources.R")
} else {
  source("00_sources.R")
}


# ── 2. PARAMETRES GLOBAUX ────────────────────────────────────

# RUBBERSIGNAL_TODAY : uniquement pour les tests (rejouer une date)
DATE_COLLECTE <- as.Date(Sys.getenv("RUBBERSIGNAL_TODAY", as.character(Sys.Date())))
SEMAINE       <- isoweek(DATE_COLLECTE)
ANNEE         <- year(DATE_COLLECTE)

dir.create("data/raw",       recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("data/reference", recursive = TRUE, showWarnings = FALSE)

ALERTES <- character()   # tout ce qui est degrade est liste ici

cat("=== RUBBERSIGNAL — Collecte du", format(DATE_COLLECTE, "%d/%m/%Y"), "===\n\n")


# ── 3. TAUX DE CHANGE (BCE) ──────────────────────────────────

cat(">> Taux de change (BCE)...\n")

fx <- rs_fx()
if (is.null(fx)) stop("Taux de change : aucune source accessible. Arret volontaire.")

XOF_PAR_EUR  <- 655.957            # parite FIXE XOF/EUR (arrimage), ne change jamais
TAUX_EUR_USD <- fx$eur_usd
FCFA_PAR_USD <- round(XOF_PAR_EUR / TAUX_EUR_USD, 2)

TAUX_JPY_USD <- if (is.finite(fx$eur_jpy)) round(TAUX_EUR_USD / fx$eur_jpy, 6) else NA_real_
TAUX_CNY_USD <- if (is.finite(fx$eur_cny)) round(TAUX_EUR_USD / fx$eur_cny, 5) else NA_real_
TAUX_USD_CHF <- if (is.finite(fx$eur_chf)) round(fx$eur_chf / TAUX_EUR_USD, 4) else NA_real_

if (!is.na(fx$date) && as.numeric(DATE_COLLECTE - as.Date(fx$date)) > 7)
  ALERTES <- c(ALERTES, paste0("Taux de change date du ", fx$date, " (plus de 7 jours)"))

cat("   EUR/USD =", TAUX_EUR_USD, "(", fx$date, ",", fx$source, ")\n")
cat("   FCFA/USD =", FCFA_PAR_USD, "\n")


# ── 4. PRIX TSR20 ────────────────────────────────────────────

cat("\n>> Prix TSR20...\n")

tsr <- rs_tsr20()          # s'arrete avec une erreur claire si tout echoue
prix_actuel_usd <- tsr$usd
source_prix     <- tsr$source
ALERTES         <- c(ALERTES, tsr$alertes)

cat("   Prix TSR20 retenu :", prix_actuel_usd, "USD/kg (", source_prix, ")\n")
if (length(tsr$candidats) > 1)
  cat("   Sources concordantes :", length(tsr$candidats),
      "| ecart max :", tsr$divergence_max_pct, "%\n")

# Garde-fou : rejeter une valeur absurde (ex. un indice base 100 pris pour un prix)
if (!is.finite(prix_actuel_usd) || prix_actuel_usd < 1 || prix_actuel_usd > 5)
  stop("Prix TSR20 hors plage plausible (1-5 USD/kg) : ", prix_actuel_usd)

# Fraicheur (v4.1) : une cotation de plus de 14 jours, ou sans date, n'est
# JAMAIS presentee comme prix actuel (ni dans le titre du rapport).
fr_hebdo         <- rs_fraicheur(tsr$date_source, DATE_COLLECTE, RS_MAX_JOURS_HEBDO)
prix_hebdo_frais <- identical(fr_hebdo$statut, "frais")
cat("   Date de la cotation :", tsr$date_source %||% "inconnue",
    "| statut :", fr_hebdo$statut, "\n")
if (!prix_hebdo_frais)
  ALERTES <- c(ALERTES, sprintf(
    "TSR20 hebdo : cotation %s (date de source %s, seuil %d jours) - non presentee comme prix actuel",
    fr_hebdo$statut, tsr$date_source %||% "inconnue", RS_MAX_JOURS_HEBDO))


# ── 4 bis. REFERENCES MENSUELLES LIBRES (FMI / Banque mondiale) ──
# NOTE (v4.1) : FRED PRUBBUSDM est bien un PRIX (US cents/lb, verifie sur
# l'API FRED le 03/11/2026), pas un indice base 100 : il est reintegre
# comme reference mensuelle, converti en USD/kg.

cat("\n>> References mensuelles (FMI via FRED, Banque mondiale)...\n")

fmi <- rs_fred_prubbusdm()
bm  <- rs_wb_pinksheet_tsr20(n = 61)   # 61 mois = 60 rendements (volatilite, script 05)
if (is.null(fmi)) ALERTES <- c(ALERTES, "Reference mensuelle FMI (FRED PRUBBUSDM) indisponible")
if (is.null(bm))  ALERTES <- c(ALERTES, "Reference mensuelle Banque mondiale (Pink Sheet TSR20) indisponible")

# Une moyenne mensuelle est datee a la fin de son mois ; seuil 45 jours.
ref_mensuelle <- function(x, source_si_absent) {
  if (is.null(x))
    return(rs_valeur(NA_real_, "USD/kg", source_si_absent, NA, "indisponible cette semaine",
                     statut = "indisponible"))
  fin <- seq(as.Date(x$date_source), by = "1 month", length.out = 2)[2] - 1
  fr  <- rs_fraicheur(fin, DATE_COLLECTE, RS_MAX_JOURS_MENSUEL)
  if (!identical(fr$statut, "frais"))
    ALERTES <<- c(ALERTES, sprintf("%s : derniere valeur %s, plus de %d jours apres la fin du mois",
                                   x$source, x$periode, RS_MAX_JOURS_MENSUEL))
  rs_valeur(x$valeur, x$unite, x$source, x$date_source, x$methode,
            periode = x$periode, valeur_orig = x$valeur_orig %||% NA_real_,
            unite_orig = x$unite_orig %||% NA_character_, url = x$url,
            statut = if (identical(fr$statut, "frais")) "derniere_valeur_disponible" else "perime",
            age_jours_fin_periode = fr$age_jours)
}
ref_iosco <- function(x, serie) {
  info <- rs_serie_info(serie)
  rs_iosco(c(x, list(serie = serie, publiable = info$publiable, licence = info$licence)),
           info$nature, "reference_mensuelle")
}
reference_mensuelle <- list(
  fmi_prubbusdm = ref_iosco(ref_mensuelle(fmi, "FMI, Global price of Rubber (FRED, serie PRUBBUSDM)"), "FMI_PRUBBUSDM"),
  bm_tsr20      = ref_iosco(ref_mensuelle(bm,  "Banque mondiale, Pink Sheet mensuelle (Rubber, TSR20)"), "BM_TSR20"),
  note = "Dernieres moyennes mensuelles publiees (sources libres). Fraicheur mesuree depuis la fin du mois (seuil 45 jours)."
)
cat("   FMI PRUBBUSDM :", reference_mensuelle$fmi_prubbusdm$valeur, "USD/kg (",
    reference_mensuelle$fmi_prubbusdm$periode %||% "-", ")\n")
cat("   BM TSR20      :", reference_mensuelle$bm_tsr20$valeur, "USD/kg (",
    reference_mensuelle$bm_tsr20$periode %||% "-", ")\n")


# ── 5. HISTORIQUE REEL DU PRIX ───────────────────────────────

# Une cotation perimee n'entre pas dans l'historique (sinon fausse variation).
f_hist_tsr <- "data/reference/tsr20_hebdo.csv"
h_tsr <- if (prix_hebdo_frais) {
  rs_hist_tsr20_maj(DATE_COLLECTE, prix_actuel_usd, source_prix, tsr$indice, f = f_hist_tsr)
} else if (file.exists(f_hist_tsr)) {
  utils::read.csv(f_hist_tsr, stringsAsFactors = FALSE)
} else {
  data.frame(cle = character(), date = character(), prix_usd = numeric(),
             source = character(), indice = character(), stringsAsFactors = FALSE)
}
hs    <- h_tsr[h_tsr$indice == tsr$indice, , drop = FALSE]   # memes indices uniquement
n_hs  <- nrow(hs)

# Moyennes mensuelles de NOTRE historique hebdomadaire reel
prix_hebdo_mensuel <- hs %>%
  mutate(periode = format(as.Date(date), "%Y-%m")) %>%
  group_by(periode) %>%
  summarise(prix_usd = round(mean(prix_usd), 4), .groups = "drop") %>%
  arrange(desc(periode)) %>%
  head(8) %>%
  mutate(source = source_prix, unite = "USD/kg")

# historique_bm = vraie serie Banque mondiale (Pink Sheet), plus de valeurs figees
prix_bm <- if (!is.null(bm)) {
  as_tibble(bm$serie) %>%      # 61 mois (n = 61 ci-dessus)
    transmute(periode, prix_usd = usd_kg, source = bm$source, unite = "USD/kg",
              date_source = paste0(periode, "-01"))
} else tibble()


# ── 6. PRIX BORD-CHAMP CI — APROMAC (automatique) ────────────

cat("\n>> Prix bord-champ CI — APROMAC...\n")

items <- rs_articles_apromac()
cat("   Articles examines :", length(items), "\n")
cons  <- rs_consensus_apromac(items, DATE_COLLECTE)
if (nrow(cons) > 0)
  cat("   Prix trouves :", paste(cons$mois, cons$prix_fcfa, collapse = " | "), "\n")
upd   <- rs_hist_apromac_maj(cons, aujourdhui = DATE_COLLECTE)   # historique presse (auto)
ALERTES <- c(ALERTES, upd$alertes)

# v4.1 : la saisie datee data/reference/prix_apromac.csv prime sur la presse.
apromac_manuel <- rs_apromac_manuel_lire("data/reference/prix_apromac.csv")
fus <- rs_apromac_fusion(apromac_manuel, upd$historique)
ALERTES <- c(ALERTES, fus$alertes)
ap  <- rs_apromac_dernier(fus$historique, DATE_COLLECTE)
ap$statut <- if (ap$age_mois == 0) "ok" else if (ap$age_mois == 1) "reporte" else "perime"

PRIX_APROMAC_FCFA <- ap$prix_fcfa
# Date de la source : date de publication (saisie) sinon date du releve presse automatique
AP_DATE_SOURCE <- as.character(ap$date_publication %||% ap$date_releve_presse %||% NA_character_)
AP_DATE_TYPE   <- if (!is.na(ap$date_publication %||% NA)) "date de publication" else if (!is.na(AP_DATE_SOURCE)) "date du releve presse automatique (publication non datee)" else "inconnue"
MOIS_APROMAC      <- ap$mois

# Alerte VISIBLE des que le prix du mois en cours manque (l'APROMAC publie
# vers le 1er-5 : en debut de mois, saisir le prix des sa parution).
if (isTRUE(ap$mois_courant_manquant)) ALERTES <- c(ALERTES, ap$alerte)

prix_apromac_usd <- round(PRIX_APROMAC_FCFA / FCFA_PAR_USD, 4)

cat("   Prix APROMAC retenu     :", PRIX_APROMAC_FCFA, "FCFA/kg (", MOIS_APROMAC,
    ", statut :", ap$statut, ", origine :", ap$origine, ")\n")
cat("   Equivalent USD          :", prix_apromac_usd, "USD/kg\n")

# Spread export-planteur (brut, sans correction DRC) — confidentiel V2
# Calcule seulement si la cotation hebdomadaire est fraiche.
spread_usd <- if (prix_hebdo_frais) round(prix_actuel_usd - prix_apromac_usd, 3) else NA_real_
spread_pct <- if (prix_hebdo_frais) round(spread_usd / prix_actuel_usd * 100, 1) else NA_real_

synthese_bord_champ <- list(
  prix_fcfa         = PRIX_APROMAC_FCFA,
  prix_usd          = prix_apromac_usd,
  mois              = MOIS_APROMAC,
  type_prix         = ap$type,
  statut            = ap$statut,
  age_mois          = ap$age_mois,
  mois_courant      = ap$mois_courant,
  mois_courant_manquant = ap$mois_courant_manquant,
  alerte            = ap$alerte,
  origine           = ap$origine,
  source            = ap$source,
  source_url        = ap$url,
  date_publication  = ap$date_publication,
  fichier_saisie    = "data/reference/prix_apromac.csv",
  historique        = fus$historique[, c("mois", "prix_fcfa", "type", "source", "url", "date_publication", "origine")],
  fcfa_par_usd      = FCFA_PAR_USD,
  taux_eur_usd      = TAUX_EUR_USD,
  taux_date         = fx$date,
  spread_export_usd = spread_usd,
  spread_export_pct = spread_pct,
  note              = "Spread confidentiel — publication prevue RubberSignal V2"
)
synthese_bord_champ <- rs_iosco(c(synthese_bord_champ,
                                  list(valeur = PRIX_APROMAC_FCFA, unite = "FCFA/kg", periode = MOIS_APROMAC,
                                       date_source = AP_DATE_SOURCE, date_source_type = AP_DATE_TYPE,
                                       publiable = rs_serie_publiable("APROMAC"))),
                                "O", "apromac")


# ── 7. RSCI : RUBBERSIGNAL CHAIN INDEX (v2, mecanisme 2026) ──
# Part du prix planteur (corrigee DRC) dans le prix de reference TSR20.
# Cadre legal : Loi N.2017-540 du 03/08/2017 (creation du CHPH)
# Mecanisme   : taux en vigueur AU MOIS DU PRIX APROMAC (table datee
#               RS_MECANISME_CHPH, scripts/00_regles.R) : 61 % -> 63 %
#               (mai 2022) -> 66 % (depuis le 01/05/2026).
# Reference   : comme le mecanisme officiel 2026, cotation SGX TSR20 du
#               MOIS PRECEDENT, ici la moyenne mensuelle Banque mondiale
#               (Pink Sheet, serie TSR20 = SGX/SICOM, source libre).
# DRC         : 60 % de reference, A CONFIRMER apres la reforme 2026.
# Debat ouvert (non integre) : prime TSR10 vs TSR20 -> RSCI v3.

cat("\n>> RSCI — RubberSignal Chain Index...\n")

DRC_OFFICIEL <- RS_DRC_REFERENCE
mois_ref_rsci <- rs_mois_precedent(MOIS_APROMAC)
ref_rsci_usd  <- if (!is.null(bm)) bm$serie$usd_kg[bm$serie$periode == mois_ref_rsci][1] else NA_real_
ref_rsci_usd  <- ref_rsci_usd %||% NA_real_

calc <- rs_calcul_rsci(PRIX_APROMAC_FCFA, FCFA_PAR_USD, ref_rsci_usd, MOIS_APROMAC, DRC_OFFICIEL)
prix_planteur_sec_usd <- calc$prix_planteur_sec_usd
rsci_pct              <- calc$rsci_pct
rsci_ecart_pts        <- calc$ecart_pts
MECANISME_OFFICIEL    <- calc$taux$part_planteur_pct / 100

if (is.finite(rsci_pct)) {
  cat("   Mois du RSCI                :", MOIS_APROMAC, "(reference TSR20 de", mois_ref_rsci, ":",
      ref_rsci_usd, "USD/kg)\n")
  cat("   Prix planteur (DRC-corrige) :", prix_planteur_sec_usd, "USD/kg\n")
  cat("   RSCI (part planteur)        :", rsci_pct, "%\n")
  cat("   Taux officiel en vigueur    :", calc$taux$part_planteur_pct, "%\n")
  cat("   Ecart vs mecanisme officiel :", rsci_ecart_pts, "points\n")
} else {
  ALERTES <- c(ALERTES, sprintf(
    "RSCI non calcule : reference TSR20 Banque mondiale de %s indisponible (prix APROMAC de %s)",
    mois_ref_rsci, MOIS_APROMAC))
  cat("   RSCI NON CALCULE : reference TSR20 de", mois_ref_rsci, "indisponible\n")
}

synthese_rsci <- list(
  mois_rsci              = MOIS_APROMAC,
  drc_officiel           = DRC_OFFICIEL,
  drc_statut             = RS_DRC_STATUT,
  mecanisme_officiel_pct = calc$taux$part_planteur_pct,
  mecanisme_date_effet   = calc$taux$date_effet,
  mecanisme_source       = calc$taux$reference,
  mecanisme_url          = calc$taux$url,
  historique_mecanisme   = transform(RS_MECANISME_CHPH, date_effet = as.character(date_effet)),
  prix_planteur_sec_usd  = prix_planteur_sec_usd,
  rsci_pct               = rsci_pct,
  ecart_pts              = rsci_ecart_pts,
  statut_donnee          = ap$statut,
  mois_apromac_utilise   = ap$mois,
  prix_apromac           = rs_iosco(rs_valeur(PRIX_APROMAC_FCFA, "FCFA/kg", ap$source, AP_DATE_SOURCE,
                                     paste0("Prix officiel APROMAC du mois ", ap$mois, " (", ap$type, ")"),
                                     url = ap$url, origine = ap$origine, periode = ap$mois), "O", "apromac"),
  reference_tsr20        = rs_iosco(rs_valeur(ref_rsci_usd, "USD/kg",
                                     "Banque mondiale, Pink Sheet mensuelle (Rubber, TSR20)",
                                     paste0(mois_ref_rsci, "-01"),
                                     "Moyenne mensuelle du mois precedant le prix APROMAC (regle du mecanisme 2026 : cotations SGX-SICOM TSR20 du mois precedent)",
                                     periode = mois_ref_rsci), "T", "reference_mensuelle"),
  taux_change            = rs_valeur(FCFA_PAR_USD, "FCFA/USD", fx$source, fx$date,
                                     "655.957 FCFA/EUR (parite fixe) / EUR-USD BCE du jour de collecte"),
  cadre_legal            = "Loi N.2017-540 du 03/08/2017 - CHPH",
  drc_source             = "DRC reference 60% (fonds de tasse), a confirmer apres reforme 2026 - debat en cours sur DRC reel et grade TSR10 vs TSR20",
  methode                = "RSCI = (prix APROMAC du mois / taux FCFA-USD / DRC) / TSR20 Banque mondiale du mois precedent x 100 ; compare au taux officiel en vigueur au mois du prix APROMAC.",
  note                   = "RSCI = part du prix planteur (corrige DRC) dans le prix de reference TSR20 du mois precedent."
)
synthese_rsci <- rs_iosco(c(synthese_rsci,
                            list(valeur = rsci_pct, unite = "%", periode = MOIS_APROMAC,
                                 source = "Calcul RubberSignal (APROMAC, Banque mondiale, BCE)",
                                 date_source = as.character(DATE_COLLECTE),
                                 publiable = rs_serie_publiable("RSCI"))),
                          "E", "rsci")


# ── 8. CALCULS DE SYNTHESE ───────────────────────────────────

cat("\n>> Statistiques de synthese...\n")

if (prix_hebdo_frais && n_hs >= 2) {
  prix_precedent <- hs$prix_usd[n_hs - 1]
  variation_pct  <- round((prix_actuel_usd - prix_precedent) / prix_precedent * 100, 2)
  tendance <- if (variation_pct > 0.5) "hausse" else if (variation_pct < -0.5) "baisse" else "stable"
} else {
  variation_pct <- NA_real_
  tendance      <- "indeterminee"
}
fenetre <- function(k, minimum) {
  if (n_hs >= minimum) tail(hs$prix_usd, k) else numeric()
}
v3  <- fenetre(13, 4); v6 <- fenetre(26, 8); v12 <- fenetre(52, 4)
moyenne_3m <- if (length(v3)  > 0) round(mean(v3), 3)  else NA_real_
moyenne_6m <- if (length(v6)  > 0) round(mean(v6), 3)  else NA_real_
min_12m    <- if (length(v12) > 0) round(min(v12), 3)  else NA_real_
max_12m    <- if (length(v12) > 0) round(max(v12), 3)  else NA_real_

cat("   Historique reel :", n_hs, "semaine(s) pour l'indice", tsr$indice, "\n")
cat("   Variation :", variation_pct, "% | tendance :", tendance, "\n")

synthese_prix <- list(
  # prix_actuel = NA si la cotation a plus de 14 jours ou n'a pas de date
  prix_actuel   = if (prix_hebdo_frais) prix_actuel_usd else NA_real_,
  periode       = if (!is.na(tsr$date_source)) format(as.Date(tsr$date_source), "%Y-%m") else NA_character_,
  date_source   = tsr$date_source,
  statut_fraicheur = fr_hebdo$statut,
  age_jours     = fr_hebdo$age_jours,
  prix_hebdo    = rs_iosco(rs_valeur(prix_actuel_usd, "USD/kg", source_prix, tsr$date_source,
                            "Cotation quotidienne lue le jour de collecte (priorite LGM > MRB via rtas.sg > SGX > IndexMundi) ; perimee au-dela de 14 jours",
                            indice = tsr$indice, statut = fr_hebdo$statut, age_jours = fr_hebdo$age_jours,
                            periode = tsr$date_source %||% NA_character_,
                            publiable = rs_serie_publiable(tsr$indice), licence = rs_serie_info(tsr$indice)$licence),
                            rs_serie_info(tsr$indice)$nature %||% "O", "prix_hebdo"),
  taux_usd_chf  = rs_valeur(TAUX_USD_CHF, "CHF/USD", fx$source, fx$date,
                            "EUR/CHF BCE / EUR/USD BCE du jour de collecte"),
  source        = source_prix,
  indice        = tsr$indice,
  variation_pct = variation_pct,
  tendance      = tendance,
  moyenne_3m    = moyenne_3m,
  moyenne_6m    = moyenne_6m,
  min_12m       = min_12m,
  max_12m       = max_12m,
  semaines_historique = n_hs,
  note_historique = "Variation = vs semaine precedente ; moyennes/min/max sur historique reel (NA tant que l'historique est trop court)",
  prix_chf      = if (prix_hebdo_frais && is.finite(TAUX_USD_CHF)) round(prix_actuel_usd * TAUX_USD_CHF, 3) else NA_real_
)
cat("   Prix en CHF :", synthese_prix$prix_chf, "CHF/kg\n")


# ── 9. MARCHES MONDIAUX ──────────────────────────────────────
# SICOM (SGX TSR20) : automatique. TOCOM (RSS3) et SHFE (RU) : pas de
# source gratuite fiable -> NA, jamais de valeur figee dans le code.

cat("\n>> Marches mondiaux...\n")

DATE_MARCHES <- DATE_COLLECTE - 1
sicom_cents  <- tsr$sgx_cents
sicom_usd    <- if (is.finite(sicom_cents)) round(sicom_cents / 100, 4) else NA_real_
spread_sicom_lgm <- if (is.finite(sicom_usd)) round(sicom_usd - prix_actuel_usd, 4) else NA_real_

cat("   SICOM TSR20 :", sicom_cents, "cts/kg =", sicom_usd, "USD/kg\n")
cat("   TOCOM RSS3 / SHFE RU : non collectes automatiquement (NA)\n")

synthese_marches <- list(
  date_cours       = as.character(DATE_MARCHES),
  spread_sicom_lgm = spread_sicom_lgm,
  lgm = list(
    grade    = "TSR20/SMR20",
    bourse   = "MRB/LGM (Malaisie)",
    prix_usd = prix_actuel_usd,
    source   = source_prix,
    date_source = tsr$date_source,
    statut_fraicheur = fr_hebdo$statut
  ),
  sicom = list(
    grade      = "TSR20",
    bourse     = "SGX/SICOM (Singapour)",
    ticker     = "TF",
    contrat    = tsr$sgx_mois,
    prix_orig  = sicom_cents,
    unite_orig = "USD cts/kg",
    prix_usd   = sicom_usd,
    statut     = if (is.finite(sicom_usd)) "auto" else "indisponible"
  ),
  tocom = list(
    grade = "RSS3", bourse = "JPX/TOCOM (Tokyo)", ticker = "TRB",
    prix_orig = NA_real_, unite_orig = "JPY/kg",
    taux_jpy_usd = TAUX_JPY_USD, prix_usd = NA_real_,
    statut = "non_collecte_auto"
  ),
  shfe = list(
    grade = "SCR WF / RSS3", bourse = "SHFE (Shanghai)", ticker = "RU",
    prix_orig = NA_real_, unite_orig = "CNY/tonne",
    taux_cny_usd = TAUX_CNY_USD, prix_usd = NA_real_,
    statut = "non_collecte_auto"
  )
)


# ── 10. SAUVEGARDER LES DONNEES BRUTES ───────────────────────

cat("\n>> Sauvegarde des donnees brutes...\n")

fichier_csv <- paste0("data/raw/prix_bm_", ANNEE, "_S", SEMAINE, ".csv")
write_csv(prix_bm, fichier_csv)
cat("   Sauvegarde :", fichier_csv, "\n")

prix_indexmundi_val <- tibble()
if (!is.null(tsr$indexmundi) && nrow(tsr$indexmundi) > 0) {
  prix_indexmundi_val <- as_tibble(tsr$indexmundi) %>%
    mutate(source = "IndexMundi", unite = "USD/kg")
  fichier_csv2 <- paste0("data/raw/prix_indexmundi_", ANNEE, "_S", SEMAINE, ".csv")
  write_csv(prix_indexmundi_val, fichier_csv2)
  cat("   Sauvegarde :", fichier_csv2, "\n")
}


# ── 11. CONTROLE QUALITE + JSON DE SORTIE ────────────────────

cat("\n>> Controle qualite...\n")

controle_qualite <- list(
  statut_global = if (length(ALERTES) == 0) "ok" else "degrade",
  alertes       = as.list(ALERTES),
  tsr20 = list(
    source = source_prix, indice = tsr$indice,
    nb_sources = length(tsr$candidats),
    divergence_max_pct = tsr$divergence_max_pct,
    candidats = tsr$candidats
  ),
  taux_change = list(source = fx$source, date = fx$date),
  apromac = list(statut = ap$statut, mois = ap$mois, age_mois = ap$age_mois,
                 mois_courant_manquant = ap$mois_courant_manquant, origine = ap$origine,
                 articles_examines = length(items), prix_trouves = nrow(cons)),
  fraicheur = list(tsr20_hebdo = fr_hebdo$statut, tsr20_date_source = tsr$date_source,
                   seuil_hebdo_jours = RS_MAX_JOURS_HEBDO, seuil_mensuel_jours = RS_MAX_JOURS_MENSUEL)
)

for (a in ALERTES) cat("::warning::", a, "\n", sep = "")   # visible dans GitHub Actions
cat("   Statut global :", controle_qualite$statut_global, "\n")

json_sortie <- list(

  meta = list(
    date_collecte = as.character(DATE_COLLECTE),
    semaine       = SEMAINE,
    annee         = ANNEE,
    source        = "rubbersignal.com",
    version       = "4.3"
  ),

  prix = list(
    synthese              = synthese_prix,
    reference_mensuelle   = reference_mensuelle,
    bord_champ_ci         = synthese_bord_champ,
    rsci                  = synthese_rsci,
    marches_mondiaux      = synthese_marches,
    historique_bm         = prix_bm,
    historique_hebdo_mensuel = prix_hebdo_mensuel,
    historique_indexmundi = if (nrow(prix_indexmundi_val) > 0)
      head(prix_indexmundi_val, 6) else list()
  ),

  controle_qualite = controle_qualite
)

fichier_json <- paste0("data/processed/rubbersignal_S", SEMAINE, "_", ANNEE, ".json")
write_json(json_sortie, fichier_json, pretty = TRUE, auto_unbox = TRUE,
           na = "null", null = "null")
cat("   JSON sauvegarde :", fichier_json, "\n")


# ── 11 bis. BASE DE DONNEES data/db/ (mission 3) ─────────────
# Ajout idempotent (00_base.R) : references mensuelles BM / FMI, APROMAC,
# RSCI, taux USD/EUR du jour, cotations hebdomadaires datees (sous licence :
# stockees pour usage interne, publiable = non dans series.csv).

cat("\n>> Base de donnees data/db/...\n")
source(if (file.exists("scripts/00_base.R")) "scripts/00_base.R" else "00_base.R")
rs_db_init()
res_db <- rs_db_ajouter(rs_db_obs_collecte(DATE_COLLECTE, bm = bm, fmi = fmi, apromac = fus$historique,
                                           rsci = synthese_rsci, fx = fx, tsr = tsr))
cat("   ", res_db$n_ajoutees, " ligne(s) ajoutee(s), ", res_db$n_identiques, " deja connue(s), ",
    res_db$n_revisions, " revision(s)\n", sep = "")
for (i in seq_len(nrow(res_db$revisions))) {
  r <- res_db$revisions[i, ]
  cat("::warning::Base : revision ", r$serie_id, " ", r$date_valeur, " : ", r$ancienne, " -> ", r$nouvelle,
      " (extraction precedente du ", r$date_extraction_ancienne, "), historique conserve\n", sep = "")
}


# ── 12. RESUME FINAL ─────────────────────────────────────────

cat("\n", strrep("=", 55), "\n")
cat("RESUME COLLECTE PRIX — Semaine", SEMAINE, "/", ANNEE, "\n")
cat(strrep("=", 55), "\n")
cat("Prix TSR20 lu      :", prix_actuel_usd, "USD/kg (", source_prix, ", date",
    tsr$date_source %||% "inconnue", ",", fr_hebdo$statut, ")\n")
cat("Ref. FMI mensuelle :", reference_mensuelle$fmi_prubbusdm$valeur, "USD/kg (",
    reference_mensuelle$fmi_prubbusdm$periode %||% "-", ")\n")
cat("Prix en CHF        :", synthese_prix$prix_chf, "CHF/kg\n")
cat("Tendance           :", tendance, "(", variation_pct, "%)\n")
cat("Prix APROMAC CI    :", PRIX_APROMAC_FCFA, "FCFA/kg =", prix_apromac_usd,
    "USD/kg (", MOIS_APROMAC, ",", ap$statut, ")\n")
cat("RSCI               :", rsci_pct, "% (", MOIS_APROMAC, "; ecart vs",
    calc$taux$part_planteur_pct, "% officiel :", rsci_ecart_pts, "pts)\n")
cat("SICOM  TSR20       :", sicom_usd, "USD/kg\n")
cat("Qualite            :", controle_qualite$statut_global, "\n")
cat("Fichier JSON       :", fichier_json, "\n")
cat(strrep("=", 55), "\n")
cat("\nProchaine etape : lancer scripts/02_collect_news.R\n\n")
cat(">> Saisie mensuelle : ajouter le prix APROMAC du mois dans data/reference/prix_apromac.csv (v4.1).\n")
