# =============================================================
# RUBBERSIGNAL.COM — Script 00 : Regles metier (fonctions pures)
# Version : 1.0 (novembre 2026)
# Role    : regles partagees par les scripts 01, 03, 04 et les tests.
#           Aucune lecture reseau ici : tout est testable hors ligne.
#   1. Fraicheur d'une valeur (14 jours hebdo, 45 jours mensuel)
#   2. Conversion FRED PRUBBUSDM (US cents/lb -> USD/kg)
#   3. Mecanisme officiel CHPH/CHPC : part planteur selon la date
#   4. Calcul du RSCI
#   5. Prix APROMAC : fusion saisie manuelle + historique presse
# Usage   : source("scripts/00_regles.R")
# =============================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# Format commun d'une valeur publiee (principe 3 du batisseur)
rs_valeur <- function(valeur, unite, source, date_source, methode, ...) {
  c(list(valeur = valeur, unite = unite, source = source,
         date_source = if (is.null(date_source) || is.na(date_source)) NA_character_
                       else as.character(date_source),
         methode = methode), list(...))
}


# ── 1. FRAICHEUR ─────────────────────────────────────────────
# statut : "frais"         age entre 0 et max_jours
#          "perime"        age > max_jours  -> jamais presente comme prix actuel
#          "date_inconnue" pas de date de source -> traite comme non frais
#          "date_future"   date posterieure a la reference (incoherent)

RS_MAX_JOURS_HEBDO   <- 14
RS_MAX_JOURS_MENSUEL <- 45

rs_fraicheur <- function(date_source, date_ref = Sys.Date(), max_jours = RS_MAX_JOURS_HEBDO) {
  d <- suppressWarnings(as.Date(date_source %||% NA_character_))
  if (length(d) != 1 || is.na(d))
    return(list(statut = "date_inconnue", age_jours = NA_real_, max_jours = max_jours))
  age <- as.numeric(as.Date(date_ref) - d)
  statut <- if (age < 0) "date_future" else if (age > max_jours) "perime" else "frais"
  list(statut = statut, age_jours = age, max_jours = max_jours)
}

rs_est_frais <- function(date_source, date_ref = Sys.Date(), max_jours = RS_MAX_JOURS_HEBDO)
  identical(rs_fraicheur(date_source, date_ref, max_jours)$statut, "frais")


# ── 2. CONVERSIONS ───────────────────────────────────────────

RS_LB_PAR_KG <- 2.20462

# FRED PRUBBUSDM (FMI, "Global price of Rubber") : U.S. cents per pound
rs_cents_lb_vers_usd_kg <- function(x) x * RS_LB_PAR_KG / 100


# ── 3. MECANISME OFFICIEL (part planteur) ────────────────────
# Parametre reglementaire (pas une donnee de marche) : historique date.
# date_effet NA = regime anterieur, date de debut non documentee.

RS_MECANISME_CHPH <- data.frame(
  date_effet        = as.Date(c(NA, "2022-05-01", "2026-05-01")),
  part_planteur_pct = c(61, 63, 66),
  reference = c(
    "Regime anterieur a mai 2022 : 61 % (date de debut non documentee)",
    "Decision provisoire CHPH n.0037 (mai 2022) : 63 % planteurs / 37 % transformateurs",
    paste("Accord interprofessionnel du 16/02/2026, enterine par le CHPC le 29/04/2026 :",
          "66 % du prix de reference net, decote de 2 FCFA/kg supprimee,",
          "reference SGX-SICOM TSR20 du mois precedent, prix APROMAC = prix carreau usine,",
          "marge intermediaire plafonnee a 20 FCFA/kg")
  ),
  url = c(
    NA_character_,
    NA_character_,
    paste("https://www.ecofinagency.com/news-agriculture/2606-56831-ivory-coast-raises-natural-rubber-growers-share-of-reference-price-for-2026",
          "https://www.sikafinance.com/marches/cote-d-ivoire-fin-de-la-decote-et-meilleure-remuneration-des-planteurs-la-campagne-heveicole-2026-demarre-sous-de-bons-auspices_62460")
  ),
  stringsAsFactors = FALSE
)

# DRC de reference : 60 %, A CONFIRMER apres la reforme 2026 (texte de
# l'accord du 16/02/2026 non encore verifie sur ce point).
RS_DRC_REFERENCE        <- 0.60
RS_DRC_STATUT           <- "a confirmer apres reforme 2026"

# Taux en vigueur a une date (ex. premier jour du mois du prix APROMAC)
rs_taux_planteur <- function(date, table = RS_MECANISME_CHPH) {
  d <- as.Date(date)
  if (is.na(d)) stop("rs_taux_planteur : date manquante")
  ok <- is.na(table$date_effet) | table$date_effet <= d
  if (!any(ok)) stop("rs_taux_planteur : aucun regime en vigueur au ", d)
  i <- max(which(ok))
  list(part_planteur_pct = table$part_planteur_pct[i],
       date_effet = if (is.na(table$date_effet[i])) NA_character_ else as.character(table$date_effet[i]),
       reference = table$reference[i], url = table$url[i])
}

# "2026-09" -> as.Date("2026-09-01")
rs_mois_vers_date <- function(mois) as.Date(paste0(substr(mois, 1, 7), "-01"))

# Mois precedent : "2026-05" -> "2026-04"
rs_mois_precedent <- function(mois) {
  d <- rs_mois_vers_date(mois)
  format(seq(d, by = "-1 month", length.out = 2)[2], "%Y-%m")
}


# ── 4. RSCI ──────────────────────────────────────────────────
# RSCI = (prix APROMAC en USD / DRC) / prix de reference TSR20 (USD/kg) x 100
# Le taux officiel compare est celui en vigueur au mois du prix APROMAC.

rs_calcul_rsci <- function(prix_fcfa_kg, fcfa_par_usd, ref_usd_kg, mois_prix,
                           drc = RS_DRC_REFERENCE) {
  taux <- rs_taux_planteur(rs_mois_vers_date(mois_prix))
  if (!all(is.finite(c(prix_fcfa_kg, fcfa_par_usd, ref_usd_kg, drc))) || ref_usd_kg <= 0)
    return(list(prix_usd = NA_real_, prix_planteur_sec_usd = NA_real_, rsci_pct = NA_real_,
                ecart_pts = NA_real_, taux = taux))
  prix_usd <- round(prix_fcfa_kg / fcfa_par_usd, 4)
  sec      <- round(prix_usd / drc, 4)
  rsci     <- round(sec / ref_usd_kg * 100, 1)
  list(prix_usd = prix_usd, prix_planteur_sec_usd = sec, rsci_pct = rsci,
       ecart_pts = round(rsci - taux$part_planteur_pct, 1), taux = taux)
}


# ── 5. PRIX APROMAC ──────────────────────────────────────────
# Saisie manuelle datee : data/reference/prix_apromac.csv
#   mois, prix_fcfa_kg, type, source, url, date_publication
# Historique presse automatique (script 00_sources) : data/reference/apromac_history.csv
#   mois, prix_fcfa, source, url, date_ajout
# Regle : la saisie manuelle prime ; un desaccord est signale.

rs_apromac_manuel_lire <- function(f = "data/reference/prix_apromac.csv") {
  if (!file.exists(f)) stop("APROMAC : fichier de saisie introuvable : ", f)
  m <- utils::read.csv(f, stringsAsFactors = FALSE, colClasses = "character", encoding = "UTF-8")
  attendues <- c("mois", "prix_fcfa_kg", "type", "source", "url", "date_publication")
  manque <- setdiff(attendues, names(m))
  if (length(manque) > 0) stop("APROMAC : colonnes manquantes dans ", f, " : ", paste(manque, collapse = ", "))
  m$prix_fcfa_kg <- suppressWarnings(as.numeric(m$prix_fcfa_kg))
  bad <- !grepl("^[0-9]{4}-[0-9]{2}$", m$mois) | !is.finite(m$prix_fcfa_kg)
  if (any(bad)) stop("APROMAC : ligne(s) invalide(s) dans ", f, " : ", paste(which(bad), collapse = ", "))
  m
}

rs_apromac_fusion <- function(manuel, auto = NULL) {
  out <- data.frame(mois = manuel$mois, prix_fcfa = manuel$prix_fcfa_kg,
                    type = manuel$type, source = manuel$source, url = manuel$url,
                    date_publication = manuel$date_publication, origine = "saisie",
                    stringsAsFactors = FALSE)
  alertes <- character()
  if (!is.null(auto) && nrow(auto) > 0) {
    for (i in seq_len(nrow(auto))) {
      a <- auto[i, ]
      if (a$mois %in% out$mois) {
        v <- out$prix_fcfa[out$mois == a$mois][1]
        if (!isTRUE(all.equal(as.numeric(a$prix_fcfa), v)))
          alertes <- c(alertes, sprintf("APROMAC %s : saisie %s FCFA, presse %s FCFA (saisie retenue)",
                                        a$mois, v, a$prix_fcfa))
      } else {
        out <- rbind(out, data.frame(mois = a$mois, prix_fcfa = as.numeric(a$prix_fcfa),
                                     type = "type non precise, releve presse", source = a$source, url = a$url,
                                     date_publication = NA_character_, origine = "presse_auto",
                                     stringsAsFactors = FALSE))
      }
    }
  }
  out <- out[order(out$mois), , drop = FALSE]
  rownames(out) <- NULL
  list(historique = out, alertes = alertes)
}

# Dernier mois disponible <= mois de la date de collecte + alerte si le mois en cours manque
rs_apromac_dernier <- function(h, aujourdhui = Sys.Date()) {
  cur <- format(as.Date(aujourdhui), "%Y-%m")
  h   <- h[h$mois <= cur, , drop = FALSE]
  if (nrow(h) == 0) stop("APROMAC : aucun prix pour le mois courant ou avant.")
  r   <- h[order(h$mois, decreasing = TRUE)[1], , drop = FALSE]
  age <- (as.integer(substr(cur, 1, 4)) - as.integer(substr(r$mois, 1, 4))) * 12 +
         (as.integer(substr(cur, 6, 7)) - as.integer(substr(r$mois, 6, 7)))
  alerte <- if (age == 0) NA_character_ else
    sprintf("APROMAC : prix de %s absent du fichier de saisie ; dernier mois disponible %s (%s FCFA/kg, %d mois de retard)",
            cur, r$mois, r$prix_fcfa, age)
  c(as.list(r), list(age_mois = age, mois_courant = cur, mois_courant_manquant = age > 0,
                     alerte = alerte))
}


# ── 6. AFFICHAGE ─────────────────────────────────────────────

RS_MOIS_LONG_FR <- c("janvier", "fevrier", "mars", "avril", "mai", "juin", "juillet",
                     "aout", "septembre", "octobre", "novembre", "decembre")
RS_MOIS_LONG_FR_ACC <- c("janvier", "février", "mars", "avril", "mai", "juin", "juillet",
                         "août", "septembre", "octobre", "novembre", "décembre")
RS_MOIS_LONG_EN <- month.name

rs_mois_libelle <- function(mois, langue = "fr") {
  if (is.null(mois) || is.na(mois) || !nzchar(mois)) return("mois inconnu")
  i <- as.integer(substr(mois, 6, 7)); a <- substr(mois, 1, 4)
  if (langue == "fr") paste(RS_MOIS_LONG_FR_ACC[i], a) else paste(RS_MOIS_LONG_EN[i], a)
}

rs_date_fr <- function(d) if (is.null(d) || is.na(d)) "date inconnue" else format(as.Date(d), "%d/%m/%Y")
