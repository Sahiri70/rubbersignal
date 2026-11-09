# =============================================================
# RUBBERSIGNAL.COM — Script 00 : Base de donnees (CSV versionnes)
# Version : 1.0 (mission 3, novembre 2026)
# Role    : base de donnees de RubberSignal en CSV dans data/db/
#           (cahier des charges, section 4 : R + CSV, pas de nouvelle technologie)
#   data/db/series.csv       : une ligne = une serie (catalogue, licence, publiable)
#   data/db/observations.csv : une ligne = une valeur datee (format long)
# Regles :
#   1. observations.csv est en AJOUT SEULEMENT : une ligne ecrite n'est jamais
#      reecrite ni supprimee (ouverture du fichier en mode ajout).
#   2. Idempotence : meme serie_id + date_valeur + valeur -> pas de nouvelle ligne.
#   3. Revision : meme serie_id + date_valeur mais valeur differente de la
#      DERNIERE ligne connue -> nouvelle ligne avec revise = TRUE ; l'ancienne
#      est conservee (historique des revisions).
#   4. Toute serie doit etre declaree dans series.csv avant d'avoir des valeurs.
#   5. Valeurs stockees avec 12 chiffres significatifs (pas de bruit flottant).
#   6. Fichiers en UTF-8, fin de ligne LF, champs entre guillemets.
# Usage   : source("scripts/00_base.R")  (scripts 01, 06 et tests)
# Aucune lecture reseau ici : tout est testable hors ligne.
# =============================================================

if (!exists("rs_serie_info", mode = "function"))
  source(if (file.exists("scripts/00_regles.R")) "scripts/00_regles.R" else "00_regles.R")

RS_DB_DIR <- "data/db"

RS_DB_COLS_SERIES <- c("serie_id", "libelle_fr", "libelle_en", "frequence", "unite", "nature",
                       "licence", "publiable", "source", "url_source", "version_methode")
RS_DB_COLS_OBS <- c("serie_id", "date_valeur", "valeur", "unite", "devise", "zone", "point_chaine",
                    "grade", "source", "url_source", "date_source", "date_extraction", "methode",
                    "nature", "version_methode", "statut_qc", "revise")

RS_DB_CHIFFRES <- 12   # chiffres significatifs stockes


# ── 1. CATALOGUE DES SERIES ──────────────────────────────────
# `registre` : cle du registre des licences (00_regles.R, section 8) qui fixe
# `publiable` et `licence`. Les colonnes devise/zone/point_chaine/grade servent
# de valeurs par defaut aux observations (elles ne vont pas dans series.csv).

.rs_s <- function(serie_id, registre, libelle_fr, libelle_en, frequence, unite, nature,
                  source, url_source, version_methode, devise, zone, point_chaine, grade)
  data.frame(serie_id, registre, libelle_fr, libelle_en, frequence, unite, nature, source,
             url_source, version_methode, devise, zone, point_chaine, grade, stringsAsFactors = FALSE)

RS_URL_COMEXT <- "https://ec.europa.eu/eurostat/databrowser/view/DS-045409/default/table"
RS_URL_BCE_M  <- "https://data.ecb.europa.eu/data/datasets/EXR/EXR.M.USD.EUR.SP00.A"

RS_DB_CATALOGUE <- do.call(rbind, c(
  list(
    .rs_s("BM_TSR20", "BM_TSR20", "TSR20, Banque mondiale (Pink Sheet), moyenne mensuelle",
          "TSR20, World Bank (Pink Sheet), monthly average", "mensuelle", "USD/kg", "T",
          "Banque mondiale, Pink Sheet mensuelle (Rubber, TSR20)",
          "https://www.worldbank.org/en/research/commodity-markets", "Références mensuelles v1",
          "USD", "Monde", "marché international (SGX TSR20)", "TSR20"),
    .rs_s("FMI_PRUBBUSDM", "FMI_PRUBBUSDM", "Prix mondial du caoutchouc, FMI (FRED PRUBBUSDM), converti en USD/kg",
          "Global price of rubber, IMF (FRED PRUBBUSDM), converted to USD/kg", "mensuelle", "USD/kg", "T",
          "FMI, Global price of Rubber (FRED, serie PRUBBUSDM)", "https://fred.stlouisfed.org/series/PRUBBUSDM",
          "Références mensuelles v1", "USD", "Monde", "marché international", "non précisé (à vérifier)"),
    .rs_s("APROMAC", "APROMAC", "Prix officiel APROMAC du mois (Côte d'Ivoire)",
          "APROMAC official monthly price (Côte d'Ivoire)", "mensuelle", "FCFA/kg", "O",
          "APROMAC (saisie datée data/reference/prix_apromac.csv, puis presse)", "data/reference/prix_apromac.csv",
          "APROMAC v1", "XOF", "CI", "carreau usine (depuis le 01/05/2026)", "caoutchouc humide (fonds de tasse)"),
    .rs_s("RSCI", "RSCI", "RSCI : part du prix TSR20 reçue par le planteur (calcul du jour de collecte)",
          "RSCI: farmer share of the TSR20 price (computed on collection day)", "hebdomadaire (calcul)", "%", "E",
          "Calcul RubberSignal (APROMAC, Banque mondiale, BCE)", "", "RSCI v2",
          "", "CI", "planteur", "équivalent sec (DRC 60 %)"),
    .rs_s("FX_USD_EUR_J", "BCE_USD_EUR", "Taux USD pour 1 EUR du jour de collecte (référence BCE)",
          "USD per 1 EUR on collection day (ECB reference)", "quotidienne", "USD/EUR", "O",
          "BCE (via Frankfurter, repli API BCE)", "https://api.frankfurter.dev", "sans objet (donnée source)",
          "USD", "Zone euro", "", ""),
    .rs_s("BCE_USD_EUR_M", "BCE_USD_EUR", "Taux USD pour 1 EUR, moyenne mensuelle BCE (EXR.M.USD.EUR.SP00.A)",
          "USD per 1 EUR, ECB monthly average (EXR.M.USD.EUR.SP00.A)", "mensuelle", "USD/EUR", "O",
          "BCE, taux de référence (EXR.M.USD.EUR.SP00.A)", RS_URL_BCE_M, "RAPI-CI v1",
          "USD", "Zone euro", "", ""),
    .rs_s("FRED_EXUSEU", "FRED_EXUSEU", "Taux USD pour 1 EUR, moyenne mensuelle (FRED EXUSEU, source secondaire)",
          "USD per 1 EUR, monthly average (FRED EXUSEU, secondary source)", "mensuelle", "USD/EUR", "O",
          "Federal Reserve, via FRED (EXUSEU)", "https://fred.stlouisfed.org/series/EXUSEU", "RAPI-CI v1",
          "USD", "Zone euro", "", "")
  ),
  lapply(c("CI", "TH", "ID", "MY", "VN"), function(p) {
    nom <- c(CI = "la Côte d'Ivoire", TH = "la Thaïlande", ID = "l'Indonésie", MY = "la Malaisie", VN = "le Vietnam")[[p]]
    nom_en <- c(CI = "Côte d'Ivoire", TH = "Thailand", ID = "Indonesia", MY = "Malaysia", VN = "Vietnam")[[p]]
    rbind(
      .rs_s(paste0("COMEXT_VAL_", p), "EUROSTAT_COMEXT",
            paste0("Importations UE de NC 4001 22 00 depuis ", nom, " : valeur déclarée"),
            paste0("EU imports of CN 4001 22 00 from ", nom_en, ": declared value"), "mensuelle", "EUR", "T",
            "Eurostat Comext (DS-045409)", RS_URL_COMEXT, "RAPI-CI v1",
            "EUR", p, "CIF frontière UE", "TSNR (NC 4001 22 00)"),
      .rs_s(paste0("COMEXT_QTE_", p), "EUROSTAT_COMEXT",
            paste0("Importations UE de NC 4001 22 00 depuis ", nom, " : quantité"),
            paste0("EU imports of CN 4001 22 00 from ", nom_en, ": quantity"), "mensuelle", "100 kg", "T",
            "Eurostat Comext (DS-045409)", RS_URL_COMEXT, "RAPI-CI v1",
            "", p, "CIF frontière UE", "TSNR (NC 4001 22 00)"))
  }),
  list(
    .rs_s("RAPI_CI_CIF_EUR", "RAPI_CI", "RAPI-CI : prix CIF UE origine Côte d'Ivoire", "RAPI-CI: EU CIF price, Côte d'Ivoire origin",
          "mensuelle", "EUR/kg", "T", "Calcul RubberSignal sur Eurostat Comext (DS-045409)", RS_URL_COMEXT, "RAPI-CI v1",
          "EUR", "CI", "CIF frontière UE", "TSNR (NC 4001 22 00)"),
    .rs_s("RAPI_CI_CIF_USD", "RAPI_CI", "RAPI-CI : prix CIF UE origine Côte d'Ivoire, en USD", "RAPI-CI: EU CIF price, Côte d'Ivoire origin, in USD",
          "mensuelle", "USD/kg", "T", "Calcul RubberSignal sur Eurostat Comext (DS-045409) et BCE", RS_URL_COMEXT, "RAPI-CI v1",
          "USD", "CI", "CIF frontière UE", "TSNR (NC 4001 22 00)"),
    .rs_s("RAPI_ASIE_CIF_EUR", "RAPI_CI", "RAPI-CI : prix CIF UE composite Asie (TH, ID, MY, VN, pondéré par les volumes)",
          "RAPI-CI: EU CIF price, Asian composite (TH, ID, MY, VN, volume-weighted)",
          "mensuelle", "EUR/kg", "T", "Calcul RubberSignal sur Eurostat Comext (DS-045409)", RS_URL_COMEXT, "RAPI-CI v1",
          "EUR", "Asie (TH, ID, MY, VN)", "CIF frontière UE", "TSNR (NC 4001 22 00)"),
    .rs_s("RAPI_ASIE_CIF_USD", "RAPI_CI", "RAPI-CI : prix CIF UE composite Asie, en USD", "RAPI-CI: EU CIF price, Asian composite, in USD",
          "mensuelle", "USD/kg", "T", "Calcul RubberSignal sur Eurostat Comext (DS-045409) et BCE", RS_URL_COMEXT, "RAPI-CI v1",
          "USD", "Asie (TH, ID, MY, VN)", "CIF frontière UE", "TSNR (NC 4001 22 00)"),
    .rs_s("RAPI_CI_DECOTE_ASIE", "RAPI_CI", "RAPI-CI : écart de prix CIF UE Côte d'Ivoire / composite Asie",
          "RAPI-CI: EU CIF price gap, Côte d'Ivoire vs Asian composite",
          "mensuelle", "%", "E", "Calcul RubberSignal sur Eurostat Comext (DS-045409)", RS_URL_COMEXT, "RAPI-CI v1",
          "", "CI", "CIF frontière UE", "TSNR (NC 4001 22 00)"),
    .rs_s("MRB_SMR20", "MRB_SMR20", "Cotation SMR20 (MRB, ex-LGM) — usage interne", "SMR20 quote (MRB, ex-LGM) — internal use",
          "quotidienne (relevé hebdomadaire)", "USD/kg", "O", "MRB SMR20 (via rtas.sg ou site LGM)", "https://www.rtas.sg/rubber-prices/",
          "Prix hebdo v1", "USD", "Malaisie", "FOB", "SMR20"),
    .rs_s("SGX_TSR20", "SGX_TSR20", "Règlement SGX-SICOM TSR20 — usage interne", "SGX-SICOM TSR20 settlement — internal use",
          "quotidienne (relevé hebdomadaire)", "USD/kg", "T", "SGX TSR20 (via rtas.sg)", "https://www.rtas.sg/rubber-prices/",
          "Prix hebdo v1", "USD", "Singapour", "FOB", "TSR20"),
    .rs_s("INDEXMUNDI", "INDEXMUNDI", "IndexMundi caoutchouc — usage interne", "IndexMundi rubber — internal use",
          "mensuelle", "USD/kg", "O", "IndexMundi", "https://www.indexmundi.com/commodities/?commodity=rubber&months=12",
          "Prix hebdo v1", "USD", "Monde", "", "non précisé")
  )))
rownames(RS_DB_CATALOGUE) <- NULL

# Ligne de series.csv (publiable et licence lus dans le registre des licences)
rs_db_catalogue_series <- function(cat = RS_DB_CATALOGUE) {
  info <- lapply(cat$registre, rs_serie_info)
  data.frame(serie_id = cat$serie_id, libelle_fr = cat$libelle_fr, libelle_en = cat$libelle_en,
             frequence = cat$frequence, unite = cat$unite, nature = cat$nature,
             licence = vapply(info, function(i) as.character(i$licence), character(1)),
             publiable = ifelse(vapply(info, function(i) isTRUE(i$publiable), logical(1)), "oui", "non"),
             source = cat$source, url_source = cat$url_source, version_methode = cat$version_methode,
             stringsAsFactors = FALSE)
}


# ── 2. LECTURE / ECRITURE CSV (UTF-8, LF, ajout seulement) ───

rs_db_fichiers <- function(dir = RS_DB_DIR)
  list(series = file.path(dir, "series.csv"), obs = file.path(dir, "observations.csv"))

.rs_csv_champ <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  paste0("\"", gsub("\"", "\"\"", enc2utf8(x), fixed = TRUE), "\"")
}

# Ecrit des lignes en fin de fichier, sans jamais toucher aux lignes existantes
.rs_csv_ajouter <- function(f, df, cols, entete = FALSE) {
  lignes <- if (nrow(df) > 0) do.call(paste, c(lapply(cols, function(k) .rs_csv_champ(df[[k]])), sep = ",")) else character()
  if (entete) lignes <- c(paste(cols, collapse = ","), lignes)
  if (length(lignes) == 0) return(invisible(0L))
  con <- file(f, open = "ab")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(paste0(paste(lignes, collapse = "\n"), "\n"))), con)
  invisible(length(lignes) - entete)
}

.rs_csv_lire <- function(f, cols) {
  if (!file.exists(f)) stop("Base : fichier introuvable : ", f, " (lancer rs_db_init())")
  ent <- readLines(f, n = 1, warn = FALSE, encoding = "UTF-8")
  if (!identical(ent, paste(cols, collapse = ",")))
    stop("Base : en-tete inattendu dans ", f, " : ", ent)
  d <- utils::read.csv(f, colClasses = "character", na.strings = "", encoding = "UTF-8",
                       stringsAsFactors = FALSE, check.names = FALSE)
  if (!identical(names(d), cols)) stop("Base : colonnes inattendues dans ", f)
  d
}

# Cree data/db/ et ses deux fichiers si absents ; ajoute au catalogue les
# series nouvelles (ajout seulement). Arret si un en-tete est different.
rs_db_init <- function(dir = RS_DB_DIR, cat = RS_DB_CATALOGUE) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  f <- rs_db_fichiers(dir)
  s_code <- rs_db_catalogue_series(cat)
  if (!file.exists(f$series)) {
    .rs_csv_ajouter(f$series, s_code, RS_DB_COLS_SERIES, entete = TRUE)
  } else {
    s <- .rs_csv_lire(f$series, RS_DB_COLS_SERIES)
    nouv <- s_code[!s_code$serie_id %in% s$serie_id, , drop = FALSE]
    if (nrow(nouv) > 0) .rs_csv_ajouter(f$series, nouv, RS_DB_COLS_SERIES)
    # Garde-fou licence : le fichier ne peut pas rendre publiable une serie que le registre interdit
    m <- merge(s, s_code[, c("serie_id", "publiable")], by = "serie_id", suffixes = c("", "_code"))
    bad <- m$serie_id[m$publiable == "oui" & m$publiable_code == "non"]
    if (length(bad) > 0) stop("Base : series.csv marque publiable une serie sous licence : ", paste(bad, collapse = ", "))
  }
  if (!file.exists(f$obs)) .rs_csv_ajouter(f$obs, data.frame(), RS_DB_COLS_OBS, entete = TRUE)
  else invisible(.rs_csv_lire(f$obs, RS_DB_COLS_OBS))
  invisible(f)
}

rs_db_lire_series <- function(dir = RS_DB_DIR) .rs_csv_lire(rs_db_fichiers(dir)$series, RS_DB_COLS_SERIES)

rs_db_lire_obs <- function(dir = RS_DB_DIR) {
  d <- .rs_csv_lire(rs_db_fichiers(dir)$obs, RS_DB_COLS_OBS)
  d$valeur <- suppressWarnings(as.numeric(d$valeur))
  d$revise <- toupper(d$revise %||% character()) == "TRUE"
  d$.ligne <- seq_len(nrow(d))   # ordre d'ecriture (= ordre des extractions)
  d
}


# ── 3. CONSTRUCTION DES OBSERVATIONS ─────────────────────────

# 12 chiffres significatifs, notation decimale (jamais 1e+05), sans espaces
rs_db_format_valeur <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.finite(x), trimws(formatC(signif(x, RS_DB_CHIFFRES), digits = RS_DB_CHIFFRES, format = "fg")), NA_character_)
}

# Une ou plusieurs observations ; les champs absents sont repris du catalogue.
rs_db_obs <- function(serie_id, date_valeur, valeur, date_source, date_extraction,
                      methode = NA_character_, statut_qc = "ok", source = NULL, url_source = NULL,
                      nature = NULL, unite = NULL, devise = NULL, zone = NULL, point_chaine = NULL,
                      grade = NULL, version_methode = NULL, cat = RS_DB_CATALOGUE) {
  n <- if (length(date_valeur) == 0 || length(valeur) == 0) 0L
       else max(length(serie_id), length(date_valeur), length(valeur))
  serie_id <- rep_len(serie_id, n)
  i <- match(serie_id, cat$serie_id)
  if (anyNA(i)) stop("Base : serie(s) non declaree(s) dans le catalogue : ", paste(unique(serie_id[is.na(i)]), collapse = ", "))
  def <- function(x, col) if (is.null(x)) cat[[col]][i] else rep_len(as.character(x), n)
  data.frame(serie_id = serie_id, date_valeur = rep_len(as.character(date_valeur), n),
             valeur = rep_len(as.numeric(valeur), n), unite = def(unite, "unite"),
             devise = def(devise, "devise"), zone = def(zone, "zone"), point_chaine = def(point_chaine, "point_chaine"),
             grade = def(grade, "grade"), source = def(source, "source"), url_source = def(url_source, "url_source"),
             date_source = rep_len(as.character(date_source), n), date_extraction = rep_len(as.character(date_extraction), n),
             methode = rep_len(as.character(methode), n), nature = def(nature, "nature"),
             version_methode = def(version_methode, "version_methode"),
             statut_qc = rep_len(as.character(statut_qc), n), stringsAsFactors = FALSE)
}

.rs_meme_valeur <- function(a, b) {
  a <- as.numeric(rs_db_format_valeur(a)); b <- as.numeric(rs_db_format_valeur(b))
  is.finite(a) & is.finite(b) & abs(a - b) <= 1e-9 * pmax(1, abs(a), abs(b))
}


# ── 4. AJOUT IDEMPOTENT + DETECTION DES REVISIONS ────────────
# Retour : list(n_ajoutees, n_identiques, n_revisions, n_ignorees, revisions)
#   revisions : data.frame(serie_id, date_valeur, ancienne, nouvelle, date_extraction_ancienne)

rs_db_ajouter <- function(nouv, dir = RS_DB_DIR) {
  vide <- list(n_ajoutees = 0L, n_identiques = 0L, n_revisions = 0L, n_ignorees = 0L,
               revisions = data.frame(serie_id = character(), date_valeur = character(), ancienne = numeric(),
                                      nouvelle = numeric(), date_extraction_ancienne = character()))
  if (is.null(nouv) || nrow(nouv) == 0) return(vide)
  manque <- setdiff(setdiff(RS_DB_COLS_OBS, "revise"), names(nouv))
  if (length(manque) > 0) stop("Base : colonnes manquantes : ", paste(manque, collapse = ", "))
  series <- rs_db_lire_series(dir)
  inconnues <- setdiff(unique(nouv$serie_id), series$serie_id)
  if (length(inconnues) > 0) stop("Base : serie(s) absente(s) de series.csv : ", paste(inconnues, collapse = ", "))
  # Valeurs absentes : jamais stockees (une absence n'est pas une observation)
  ign <- !is.finite(as.numeric(nouv$valeur))
  nouv <- nouv[!ign, , drop = FALSE]
  res <- vide; res$n_ignorees <- sum(ign)
  if (nrow(nouv) == 0) return(res)
  bad_date <- is.na(nouv$date_valeur) | !grepl("^[0-9]{4}-[0-9]{2}(-[0-9]{2})?$", nouv$date_valeur)
  if (any(bad_date)) stop("Base : date_valeur invalide (AAAA-MM ou AAAA-MM-JJ) : ",
                          paste(unique(nouv$date_valeur[bad_date]), collapse = ", "))
  nouv$valeur <- as.numeric(rs_db_format_valeur(nouv$valeur))
  cle_n <- paste(nouv$serie_id, nouv$date_valeur, sep = "|")
  # Doublons dans le lot : identiques -> un seul ; contradictoires -> arret
  if (anyDuplicated(cle_n)) {
    for (k in unique(cle_n[duplicated(cle_n)])) {
      v <- nouv$valeur[cle_n == k]
      if (!all(.rs_meme_valeur(v, v[1]))) stop("Base : valeurs contradictoires dans le meme lot pour ", k)
    }
    nouv <- nouv[!duplicated(cle_n), , drop = FALSE]; cle_n <- cle_n[!duplicated(cle_n)]
  }
  obs <- rs_db_lire_obs(dir)
  cle_o <- paste(obs$serie_id, obs$date_valeur, sep = "|")
  # derniere ligne connue par cle (ordre d'ecriture)
  der <- obs[!duplicated(cle_o, fromLast = TRUE), , drop = FALSE]
  j <- match(cle_n, paste(der$serie_id, der$date_valeur, sep = "|"))
  connu <- !is.na(j)
  identique <- connu & .rs_meme_valeur(nouv$valeur, der$valeur[ifelse(connu, j, 1L)])
  revision  <- connu & !identique
  nouv$revise <- ifelse(revision, "TRUE", "FALSE")
  a_ecrire <- nouv[!identique, , drop = FALSE]
  a_ecrire$valeur <- rs_db_format_valeur(a_ecrire$valeur)
  .rs_csv_ajouter(rs_db_fichiers(dir)$obs, a_ecrire, RS_DB_COLS_OBS)
  res$n_ajoutees  <- nrow(a_ecrire)
  res$n_identiques <- sum(identique)
  res$n_revisions <- sum(revision)
  if (any(revision))
    res$revisions <- data.frame(serie_id = nouv$serie_id[revision], date_valeur = nouv$date_valeur[revision],
                                ancienne = der$valeur[j[revision]], nouvelle = nouv$valeur[revision],
                                date_extraction_ancienne = der$date_extraction[j[revision]],
                                stringsAsFactors = FALSE)
  res
}


# ── 5. LECTURE D'UNE SERIE ───────────────────────────────────
# Historique : derniere version connue par date_valeur ; `deja_revise` = la
# valeur a ete revisee au moins une fois ; `n_versions` = nombre de lignes.

rs_db_historique <- function(obs, serie_id) {
  s <- obs[obs$serie_id == serie_id, , drop = FALSE]
  if (nrow(s) == 0) return(s)
  cle <- s$date_valeur
  rev <- tapply(s$revise, cle, any)
  nv  <- tapply(s$revise, cle, length)
  h <- s[!duplicated(cle, fromLast = TRUE), , drop = FALSE]
  h$deja_revise <- as.logical(rev[h$date_valeur])
  h$n_versions  <- as.integer(nv[h$date_valeur])
  h <- h[order(h$date_valeur), , drop = FALSE]
  rownames(h) <- NULL
  h
}

# Derniere valeur connue (date_valeur la plus recente, derniere version), ou NULL
rs_db_derniere <- function(obs, serie_id) {
  h <- rs_db_historique(obs, serie_id)
  if (nrow(h) == 0) return(NULL)
  as.list(h[nrow(h), , drop = FALSE])
}


# ── 6. OBSERVATIONS DE LA COLLECTE HEBDOMADAIRE (script 01) ──
# Entrees = objets deja calcules par le script 01 (aucun calcul nouveau) :
#   bm, fmi : sorties de rs_wb_pinksheet_tsr20 / rs_fred_prubbusdm (ou NULL)
#   apromac : historique fusionne (rs_apromac_fusion()$historique)
#   rsci    : synthese_rsci ; fx : rs_fx() ; tsr : rs_tsr20()
# Les cotations sous licence ne sont stockees que si publier_licence = TRUE :
# data/db/ est dans le depot public (decision D1, 00_regles.R section 8 bis).

rs_db_obs_collecte <- function(date_extraction, bm = NULL, fmi = NULL, apromac = NULL,
                               rsci = NULL, fx = NULL, tsr = NULL,
                               publier_licence = rs_publier_sous_licence()) {
  out <- list()
  de <- as.character(date_extraction)
  if (!is.null(bm) && nrow(bm$serie %||% data.frame()) > 0)
    out$bm <- rs_db_obs("BM_TSR20", bm$serie$periode, bm$serie$usd_kg, paste0(bm$serie$periode, "-01"), de,
                        methode = bm$methode, url_source = bm$url)
  if (!is.null(fmi) && nrow(fmi$serie %||% data.frame()) > 0)
    out$fmi <- rs_db_obs("FMI_PRUBBUSDM", fmi$serie$periode, fmi$serie$usd_kg, paste0(fmi$serie$periode, "-01"), de,
                         methode = paste0(fmi$methode, " (valeur d'origine en US cents/lb)"))
  if (!is.null(apromac) && nrow(apromac) > 0) {
    ds <- ifelse(is.na(apromac$date_publication), apromac$date_releve_presse %||% NA_character_, apromac$date_publication)
    out$ap <- rs_db_obs("APROMAC", apromac$mois, apromac$prix_fcfa, ds, de,
                        methode = paste0("Prix officiel APROMAC (", apromac$type, ") ; origine : ", apromac$origine),
                        source = apromac$source, url_source = ifelse(is.na(apromac$url), "", apromac$url),
                        point_chaine = ifelse(grepl("carreau", apromac$type), "carreau usine", "bord-champ ou non précisé"))
  }
  if (!is.null(rsci) && is.finite(suppressWarnings(as.numeric(rsci$rsci_pct %||% NA))))
    out$rsci <- rs_db_obs("RSCI", de, rsci$rsci_pct, de, de,
                          methode = paste0(rsci$methode %||% "", " Mois APROMAC : ", rsci$mois_rsci %||% "?",
                                           " ; reference TSR20 : ", rsci$reference_tsr20$periode %||% "?",
                                           " ; part officielle ", rsci$mecanisme_officiel_pct %||% "?", " %."))
  if (!is.null(fx) && is.finite(fx$eur_usd %||% NA) && !is.na(fx$date %||% NA))
    out$fx <- rs_db_obs("FX_USD_EUR_J", fx$date, fx$eur_usd, fx$date, de,
                        methode = "Taux de reference BCE du jour (USD pour 1 EUR)", source = fx$source)
  if (!is.null(tsr) && length(tsr$candidats) > 0) {
    for (k in names(tsr$candidats)) {
      cnd <- tsr$candidats[[k]]
      d <- cnd$date_source %||% NA_character_
      if (is.na(d) || !is.finite(cnd$usd %||% NA) || !cnd$indice %in% RS_DB_CATALOGUE$serie_id) next
      if (!publier_licence && !rs_serie_publiable(cnd$indice)) next
      out[[paste0("hebdo_", k)]] <- rs_db_obs(cnd$indice, d, cnd$usd, d, de,
                                              methode = "Cotation lue le jour de collecte (usage interne, sous licence)",
                                              source = cnd$source)
    }
  }
  if (length(out) == 0) return(NULL)
  do.call(rbind, unname(out))
}
