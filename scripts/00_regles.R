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
#   6. Affichage (mois, dates)
#   7. IOSCO : nature T/O/D/E/J et version de methode       (v1.1, mission 2)
#   8. Licence : registre des series + RS_PUBLIER_SOURCES_SOUS_LICENCE
#   9. Pre-RSI : sens, poids, renormalisation, seuil de 3 composantes
#      (9 bis : lecture des taux de change sans valeur de secours)
#  10. Volatilite historique (Monte Carlo, fourchette sans derive)
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
                    date_publication = manuel$date_publication, date_releve_presse = NA_character_, origine = "saisie",
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
                                     date_publication = NA_character_,
                                     date_releve_presse = as.character(a$date_ajout %||% NA_character_),
                                     origine = "presse_auto",
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


# ── 7. IOSCO : NATURE DES DONNEES ET VERSION DE METHODE ──────
# Charte de gouvernance, section 4 : T = transaction, O = officiel,
# D = declaration, E = estimation calculee, J = jugement.
# Chaque valeur publiee porte `nature` et `version_methode` (charte, section 5).
# Toute nouvelle version doit avoir une entree datee au journal des versions
# de METHODOLOGIE_Indices_RubberSignal (sinon le gardien bloque).

RS_METHODES <- data.frame(
  cle        = c("prix_hebdo", "reference_mensuelle", "apromac", "rsci", "terrain",
                 "pre_rsi", "volatilite"),
  version    = c("Prix hebdo v1", "Références mensuelles v1", "APROMAC v1", "RSCI v2",
                 "Terrain v1", "Pré-RSI v2", "Volatilité v1"),
  date_effet = "2026-11-03",
  stringsAsFactors = FALSE
)

rs_version_methode <- function(cle) {
  i <- match(cle, RS_METHODES$cle)
  if (is.na(i)) stop("version de methode inconnue : ", cle)
  RS_METHODES$version[i]
}

# Ajoute nature + version de methode (+ date d'effet) a une fiche de valeur
rs_iosco <- function(x, nature, cle_methode) {
  stopifnot(nature %in% c("T", "O", "D", "E", "J"))
  c(x, list(nature = nature, version_methode = rs_version_methode(cle_methode),
            date_effet_methode = RS_METHODES$date_effet[match(cle_methode, RS_METHODES$cle)]))
}

RS_LEGENDE_NATURE_FR <- "Nature : T = transaction, O = officiel, D = déclaration, E = estimation calculée, J = jugement"
RS_LEGENDE_NATURE_EN <- "Nature: T = transaction, O = official, D = declaration, E = calculated estimate, J = judgement"


# ── 8. LICENCE DES SERIES (decision D1 en attente) ───────────
# Registre des series : publiable = FALSE -> redistribution soumise a licence
# (bourses, LGM/MRB, rtas.sg, IndexMundi). Usage interne uniquement tant que
# la licence n'est pas obtenue (charte, section 5).

RS_REGISTRE_SERIES <- data.frame(
  serie = c("MRB_SMR20", "SGX_TSR20", "LGM", "RTAS_SG", "JPX_TOCOM", "SHFE_RU", "INDEXMUNDI",
            "BM_TSR20", "FMI_PRUBBUSDM", "APROMAC", "RSCI", "TERRAIN_CI", "PRE_RSI", "VOLATILITE"),
  libelle = c("MRB SMR20 (ex-LGM)", "SGX-SICOM TSR20", "LGM / MRB (site officiel)", "rtas.sg (republication MRB / SGX)",
              "JPX / TOCOM RSS3", "SHFE RU", "IndexMundi",
              "Banque mondiale, Pink Sheet TSR20", "FMI, Global price of Rubber (FRED PRUBBUSDM)",
              "APROMAC, prix bord-champ", "RSCI (calcul RubberSignal)", "Releve terrain CI",
              "Pre-RSI (calcul RubberSignal)", "Fourchette de volatilite (calcul RubberSignal)"),
  publiable = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
                TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  licence = c("sous licence", "sous licence", "sous licence", "sous licence", "sous licence",
              "sous licence", "sous licence", "CC BY 4.0", "donnees publiques",
              "publication officielle", "calcul RubberSignal (entrees publiables)",
              "saisie RubberSignal", "calcul RubberSignal", "calcul RubberSignal (entree BM)"),
  nature = c("O", "T", "O", "O", "T", "T", "O", "T", "T", "O", "E", "D", "E", "E"),
  stringsAsFactors = FALSE
)

rs_serie_info <- function(serie) {
  i <- match(serie, RS_REGISTRE_SERIES$serie)
  if (is.na(i)) return(list(serie = serie, publiable = FALSE, licence = "inconnue (non publiable par defaut)",
                            nature = NA_character_, libelle = serie))
  as.list(RS_REGISTRE_SERIES[i, ])
}
rs_serie_publiable <- function(serie) isTRUE(rs_serie_info(serie)$publiable)

# Reglage unique. Defaut TRUE = affichage actuel conserve (decision D1 non tranchee).
# FALSE : seules les series publiables apparaissent dans le titre et le tableau des prix.
rs_publier_sous_licence <- function(v = Sys.getenv("RS_PUBLIER_SOURCES_SOUS_LICENCE", "TRUE")) {
  v <- toupper(trimws(as.character(v)))
  if (v %in% c("TRUE", "T", "1", "OUI", "YES")) return(TRUE)
  if (v %in% c("FALSE", "F", "0", "NON", "NO")) return(FALSE)
  stop("RS_PUBLIER_SOURCES_SOUS_LICENCE : valeur invalide '", v, "' (TRUE ou FALSE)")
}

RS_MENTION_LICENCE_FR <- "cotation hebdomadaire réservée (licence)"
RS_MENTION_LICENCE_EN <- "weekly quote reserved (licence)"

# Titre du rapport. hebdo : list(valeur, date, frais, serie) ; bm : list(valeur, periode)
rs_titre_rapport <- function(langue, semaine, annee, hebdo, bm, symbole = "~",
                             publier_licence = rs_publier_sous_licence()) {
  fr <- langue == "fr"
  sem <- if (fr) paste0("Semaine ", semaine, " - ", annee) else paste0("Week ", semaine, " - ", annee)
  tete <- paste0("RubberSignal #", semaine)
  hebdo_ok <- isTRUE(hebdo$frais) && is.finite(hebdo$valeur %||% NA_real_) &&
    (publier_licence || rs_serie_publiable(hebdo$serie))
  if (hebdo_ok)
    return(paste0(tete, " | TSR20", if (fr) " : " else ": ", hebdo$valeur, " USD/kg (", rs_date_fr(hebdo$date), ") ",
                  if (symbole != "~") paste0(symbole, " ") else "", "| ", sem))
  if (!publier_licence && is.finite(bm$valeur %||% NA_real_))
    return(paste0(tete, " | TSR20 ", if (fr) "Banque mondiale" else "World Bank", if (fr) " : " else ": ",
                  bm$valeur, " USD/kg (", rs_mois_libelle(bm$periode, langue), ") | ", sem))
  paste0(tete, " | ", sem, " | ", if (fr) "Cotation hebdomadaire TSR20 indisponible" else "Weekly TSR20 quote unavailable")
}

# Tableau des prix. lignes : liste de list(serie, libelle, valeur, date, source, nature, statut)
rs_tableau_prix <- function(lignes, langue = "fr", publier_licence = rs_publier_sous_licence()) {
  fr <- langue == "fr"
  garde <- Filter(function(l) publier_licence || rs_serie_publiable(l$serie), lignes)
  retire <- length(garde) < length(lignes)
  entete <- if (fr) "| Série | Valeur | Date de la valeur | Source | Nature | Statut |\n|---|---|---|---|---|---|\n"
            else "| Series | Value | Value date | Source | Nature | Status |\n|---|---|---|---|---|---|\n"
  corps <- vapply(garde, function(l) paste0("| ", l$libelle, " | ", l$valeur, " | ", l$date, " | ",
                                             l$source, " | ", l$nature %||% "—", " | ", l$statut, " |\n"), character(1))
  paste0(entete, paste(corps, collapse = ""),
         if (retire) paste0("\n*", if (fr) "Cotation hebdomadaire réservée (licence)" else "Weekly quote reserved (licence)",
                            if (fr) " : non reproduite dans cette publication.*\n" else ": not reproduced in this publication.*\n")
         else "", "\n")
}


# ── 9. PRE-RSI : composantes, sens, poids, seuil ─────────────
# Convention : chaque composante est ramenee a un score 0-100 ORIENTE PRIX
#   > 55 = pousse le prix a la hausse, < 45 = a la baisse, 45-55 = neutre.
# Sens de chaque composante (verifie en mission 2, novembre 2026) :
#   meteo   : score d'offre eleve = offre abondante = BAISSIER -> score inverse
#             (classes du module 1 : >=75 abondante -> 30 ; 50-74 normale -> 50 ;
#              25-49 sous pression -> 70 ; <25 perturbee -> 85)
#   devises : yuan fort (USD/CNY < moyenne 12 mois - 2 %) = demande chinoise
#             soutenue = HAUSSIER (70) ; yuan faible (> +2 %) = BAISSIER (30)
#   demande : score demande aval utilise tel quel (eleve = HAUSSIER)
#   terrain : sentiment planteurs 1-5 ramene a 0-100 par (s - 1) / 4 x 100
#             (3/5 = 50 = neutre ; l'ancienne formule s/5 donnait 60 pour 3/5)
#   geo     : articles de tensions sur l'offre (secheresse, inondations, greves,
#             sanctions) = risque sur l'offre = HAUSSIER : 1-2 -> 50, 3-4 -> 60,
#             5 et plus -> 70. 0 article ou API indisponible -> NA (pas de donnee)
#             (ancienne regle : 0 article = 65 « aucune tension », et plus de
#              tensions = plus baissier, contraire au libelle « risque sur l'offre »)
# Poids nominaux (non backtestes) : meteo 25 %, devises 20 %, demande 25 %,
#   terrain 15 %, geopolitique 15 % (total 100 %).
# Renormalisation : score = somme(poids_i x score_i) / somme(poids_i) sur les
#   seules composantes disponibles (score non NA).
# Seuil : moins de 3 composantes disponibles -> Pre-RSI NON PUBLIE (NA).

RS_PRE_RSI_POIDS <- c(meteo = 0.25, devises = 0.20, demande = 0.25, terrain = 0.15, geo = 0.15)
RS_PRE_RSI_MIN_COMPOSANTES <- 3
RS_PRE_RSI_NOMS_FR <- c(meteo = "Météo / offre (4 zones)", devises = "Devises (yuan)",
                        demande = "Demande industrielle", terrain = "Terrain CI", geo = "Géopolitique")
RS_PRE_RSI_NOMS_EN <- c(meteo = "Weather / supply (4 zones)", devises = "Currencies (yuan)",
                        demande = "Industrial demand", terrain = "CI field data", geo = "Geopolitics")

rs_pre_rsi_meteo <- function(score_offre) {
  s <- suppressWarnings(as.numeric(score_offre))
  if (length(s) != 1 || !is.finite(s)) return(NA_real_)
  if (s >= 75) 30 else if (s >= 50) 50 else if (s >= 25) 70 else 85
}
rs_pre_rsi_devise <- function(ecart_cny_pct) {
  e <- suppressWarnings(as.numeric(ecart_cny_pct))
  if (length(e) != 1 || !is.finite(e)) return(NA_real_)
  if (e < -2) 70 else if (e > 2) 30 else 50
}
rs_pre_rsi_demande <- function(score_demande) {
  s <- suppressWarnings(as.numeric(score_demande))
  if (length(s) != 1 || !is.finite(s)) NA_real_ else min(100, max(0, s))
}
rs_pre_rsi_terrain <- function(sentiment_1_5) {
  s <- suppressWarnings(as.numeric(sentiment_1_5))
  if (length(s) != 1 || !is.finite(s) || s < 1 || s > 5) return(NA_real_)
  round((s - 1) / 4 * 100)
}
rs_pre_rsi_geo <- function(nb_articles) {
  n <- suppressWarnings(as.numeric(nb_articles))
  if (length(n) != 1 || !is.finite(n) || n <= 0) return(NA_real_)
  if (n <= 2) 50 else if (n <= 4) 60 else 70
}

rs_sens_prix <- function(score) {
  if (length(score) != 1 || !is.finite(score)) return(NA_character_)
  if (score > 55) "hausse" else if (score < 45) "baisse" else "neutre"
}

rs_pre_rsi_libelle <- function(score, langue = "fr") {
  if (length(score) != 1 || !is.finite(score))
    return(if (langue == "fr") "Non publié" else "Not published")
  fr <- c("Haussier — conditions favorables", "Légèrement haussier — marché bien orienté",
          "Neutre — marché équilibré", "Légèrement baissier — pression sur les prix",
          "Baissier — conditions défavorables")
  en <- c("Bullish — favourable conditions", "Slightly bullish — market well oriented",
          "Neutral — balanced market", "Slightly bearish — pressure on prices",
          "Bearish — unfavourable conditions")
  i <- if (score >= 70) 1 else if (score > 55) 2 else if (score >= 45) 3 else if (score > 30) 4 else 5
  if (langue == "fr") fr[i] else en[i]
}

# scores : vecteur nomme (meteo, devises, demande, terrain, geo), NA = absent
rs_pre_rsi <- function(scores, poids = RS_PRE_RSI_POIDS, min_comp = RS_PRE_RSI_MIN_COMPOSANTES) {
  scores <- scores[names(poids)]
  names(scores) <- names(poids)
  dispo  <- is.finite(scores)
  n      <- sum(dispo)
  p_eff  <- ifelse(dispo, poids / sum(poids[dispo]), 0)
  score  <- if (n >= min_comp) round(sum(scores[dispo] * p_eff[dispo])) else NA_real_
  list(score = score, publie = n >= min_comp, n_disponibles = n, min_composantes = min_comp,
       scores = as.list(scores), poids_nominaux = as.list(poids), poids_effectifs = as.list(round(p_eff, 4)),
       contributions = as.list(ifelse(dispo, round(scores * p_eff, 2), NA_real_)),
       sens = as.list(vapply(scores, rs_sens_prix, character(1))),
       composantes_absentes = names(poids)[!dispo],
       signal = rs_pre_rsi_libelle(score, "fr"), signal_en = rs_pre_rsi_libelle(score, "en"),
       raison_non_publie = if (n >= min_comp) NA_character_ else
         sprintf("%d composante(s) disponible(s) sur %d : minimum %d", n, length(poids), min_comp))
}

# Coherence : (1) le libelle correspond au score ; (2) un score haussier a au moins
# une composante haussiere, un score baissier au moins une composante baissiere.
rs_pre_rsi_coherent <- function(res) {
  if (!isTRUE(res$publie)) return(is.na(res$score) && identical(res$signal, rs_pre_rsi_libelle(NA)))
  if (!identical(res$signal, rs_pre_rsi_libelle(res$score))) return(FALSE)
  sens <- unlist(res$sens)
  s <- rs_sens_prix(res$score)
  if (identical(s, "hausse")) return(any(sens == "hausse", na.rm = TRUE))
  if (identical(s, "baisse")) return(any(sens == "baisse", na.rm = TRUE))
  TRUE
}


# ── 9 bis. DEVISES (script 04) : jamais de taux de secours ───
# d = reponse ExchangeRate API deja lue (liste) ou NULL si l'appel a echoue.
# Taux absent -> NA + alerte ; date = date de mise a jour annoncee par l'API.

RS_DEVISES <- c(USD_CNY = "CNY", USD_MYR = "MYR", USD_XOF = "XOF", USD_IDR = "IDR", USD_CHF = "CHF")
RS_DEVISES_CHIFFRES <- c(USD_CNY = 4, USD_MYR = 4, USD_XOF = 2, USD_IDR = 0, USD_CHF = 4)

rs_devises_lire <- function(d, source = "ExchangeRate API") {
  out <- c(as.list(setNames(rep(NA_real_, length(RS_DEVISES)), names(RS_DEVISES))),
           list(source = source, date = NA_character_, statut = "indisponible", alertes = character()))
  r <- if (is.list(d)) (d$conversion_rates %||% d$rates) else NULL
  if (is.null(r) || length(r) == 0) {
    out$alertes <- paste0("Devises : ", source, " indisponible ou sans taux - valeurs NA, aucune valeur de secours")
    return(out)
  }
  for (k in names(RS_DEVISES)) {
    v <- suppressWarnings(as.numeric(r[[RS_DEVISES[[k]]]] %||% NA))
    if (length(v) == 1 && is.finite(v) && v > 0) out[[k]] <- round(v, RS_DEVISES_CHIFFRES[[k]])
    else out$alertes <- c(out$alertes, paste0("Devises : taux ", k, " absent de la reponse - NA"))
  }
  maj <- suppressWarnings(as.numeric(d$time_last_update_unix %||% NA))
  if (length(maj) == 1 && is.finite(maj))
    out$date <- as.character(as.Date(as.POSIXct(maj, origin = "1970-01-01", tz = "UTC")))
  else out$alertes <- c(out$alertes, "Devises : date de mise a jour absente de la reponse")
  n_ok <- sum(vapply(names(RS_DEVISES), function(k) is.finite(out[[k]]), logical(1)))
  out$statut <- if (n_ok == length(RS_DEVISES)) "ok" else if (n_ok > 0) "partiel" else "indisponible"
  out
}


# ── 10. VOLATILITE HISTORIQUE (Monte Carlo) ──────────────────
# Estimee sur la serie mensuelle Banque mondiale TSR20 (Pink Sheet) :
#   rendements logarithmiques mensuels r_t = ln(P_t / P_t-1), mois consecutifs
#   uniquement, fenetre = n_mois derniers rendements (defaut 60 = 5 ans) ;
#   volatilite annuelle = ecart-type(r) x sqrt(12) ; derive annuelle = moyenne(r) x 12
#   (calculee pour information, NON utilisee : la simulation est sans derive).
# Minimum 24 rendements, sinon NULL (pas de simulation).

rs_volatilite_historique <- function(periodes, prix, n_mois = 60, min_obs = 24) {
  d <- data.frame(periode = as.character(periodes), prix = suppressWarnings(as.numeric(prix)),
                  stringsAsFactors = FALSE)
  d <- d[grepl("^[0-9]{4}-[0-9]{2}$", d$periode) & is.finite(d$prix) & d$prix > 0, , drop = FALSE]
  d <- d[!duplicated(d$periode), , drop = FALSE]
  d <- d[order(d$periode), , drop = FALSE]
  if (nrow(d) < 2) return(NULL)
  idx <- as.integer(substr(d$periode, 1, 4)) * 12 + as.integer(substr(d$periode, 6, 7))
  r   <- diff(log(d$prix))
  ok  <- diff(idx) == 1
  r   <- r[ok]; fin_r <- d$periode[-1][ok]
  if (length(r) > n_mois) { r <- utils::tail(r, n_mois); fin_r <- utils::tail(fin_r, n_mois) }
  if (length(r) < min_obs) return(NULL)
  debut <- rs_mois_precedent(fin_r[1])
  list(volatilite_annuelle = round(stats::sd(r) * sqrt(12), 4),
       derive_annuelle_estimee = round(mean(r) * 12, 4),
       erreur_type_derive = round(stats::sd(r) / sqrt(length(r)) * 12, 4),
       n_rendements = length(r), periode_debut = debut, periode_fin = utils::tail(fin_r, 1),
       methode = paste0("Ecart-type des rendements logarithmiques mensuels x sqrt(12), ",
                        length(r), " rendements de ", debut, " a ", utils::tail(fin_r, 1),
                        " (mois consecutifs uniquement)"))
}

# Quantiles analytiques d'un GBM sans derive (mediane = prix de depart)
rs_quantile_gbm <- function(prix0, vol_annuelle, horizon_sem, p)
  prix0 * exp(stats::qnorm(p) * vol_annuelle * sqrt(horizon_sem / 52))
