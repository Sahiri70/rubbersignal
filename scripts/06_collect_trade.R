# =============================================================
# RUBBERSIGNAL.COM — Script 06 : Douanes UE -> RAPI-CI v1
# Auteur  : Martial Sahiri
# Version : 1.0 (mission 3, novembre 2026)
# Objectif: chaque semaine, lire les importations UE de caoutchouc naturel
#           techniquement specifie (NC 4001 22 00) dans Eurostat Comext
#           (DS-045409, API publique sans cle), depuis la Cote d'Ivoire et
#           depuis TH, ID, MY, VN, puis calculer (METHODOLOGIE, section 1) :
#             prix CIF UE CI (EUR/kg, USD/kg), composite Asie pondere,
#             ecart CI / composite Asie, drapeaux de controle.
# Entrees : API Eurostat Comext ; BCE (taux USD/EUR moyen mensuel,
#           EXR.M.USD.EUR.SP00.A) ; FRED EXUSEU seulement si la BCE echoue.
# Sorties : data/raw/comext_AAAAMMJJ.json (reponse brute)
#           data/db/observations.csv (ajout idempotent, revisions detectees)
#           bloc `rapi_ci` du JSON de la semaine (data/processed/rubbersignal_SXX_AAAA.json)
# Echec de l'API : statut « indisponible », ::warning::, et la derniere valeur
#           de la base est reprise AVEC sa date (jamais presentee comme nouvelle).
# Usage   : Rscript scripts/06_collect_trade.R   (apres 01, avant 03)
# =============================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(lubridate)
})

source(if (file.exists("scripts/00_sources.R")) "scripts/00_sources.R" else "00_sources.R")   # + 00_regles.R
source(if (file.exists("scripts/00_base.R")) "scripts/00_base.R" else "00_base.R")

# RUBBERSIGNAL_TODAY : uniquement pour les tests (rejouer une date), comme le script 01
DATE_COLLECTE <- as.Date(Sys.getenv("RUBBERSIGNAL_TODAY", as.character(Sys.Date())))
SEMAINE       <- isoweek(DATE_COLLECTE)
ANNEE         <- year(DATE_COLLECTE)
fichier_json  <- paste0("data/processed/rubbersignal_S", SEMAINE, "_", ANNEE, ".json")
if (!file.exists(fichier_json))
  stop("JSON introuvable : ", fichier_json, " - lancer le script 01 d'abord.")

ALERTES <- character()
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
rs_db_init()

cat("=== RUBBERSIGNAL — Douanes UE (Eurostat Comext) du", format(DATE_COLLECTE, "%d/%m/%Y"), "===\n\n")

PARTENAIRES <- c(RS_RAPI_PARTENAIRE_CI, RS_RAPI_PARTENAIRES_ASIE)
requete     <- rs_comext_requete(PARTENAIRES, n_mois = 24)
URL_API     <- rs_comext_url(requete)
METHODE_COMEXT <- paste0("Eurostat Comext DS-045409 : declarant EU, flux importations, produit ",
                         RS_RAPI_PRODUIT, ", frequence mensuelle")


# ── 1. EUROSTAT COMEXT ───────────────────────────────────────

cat(">> Eurostat Comext (DS-045409, NC 40012200, partenaires", paste(PARTENAIRES, collapse = ", "), ")...\n")
brut <- rs_comext_telecharger(requete)
large <- NULL; maj <- NA_character_; maj_brut <- NA_character_; fichier_brut <- NA_character_
if (!is.null(brut)) {
  fichier_brut <- sprintf("data/raw/comext_%s.json", format(DATE_COLLECTE, "%Y%m%d"))
  con <- file(fichier_brut, open = "wb"); writeBin(charToRaw(enc2utf8(brut)), con); close(con)
  cat("   Reponse brute :", fichier_brut, "\n")
  large <- tryCatch(rs_comext_large(rs_comext_lire(brut)), error = function(e) {
    ALERTES <<- c(ALERTES, paste0("Eurostat Comext : reponse illisible (", conditionMessage(e), ")"))
    NULL })
  if (!is.null(large)) {
    maj <- attr(large, "updated")
    maj_brut <- tryCatch(as.character(fromJSON(brut)$updated %||% NA), error = function(e) NA_character_)
    if (is.na(maj)) {
      ALERTES <- c(ALERTES, "Eurostat Comext : date de mise a jour (champ updated) absente - valeurs non publiees")
      large <- NULL
    }
  }
} else {
  ALERTES <- c(ALERTES, "Eurostat Comext indisponible cette semaine : derniere valeur de la base reprise avec sa date")
}
STATUT <- if (!is.null(large)) "ok" else "indisponible"
cat("   Statut :", STATUT, if (!is.na(maj)) paste("| mise a jour Eurostat :", maj) else "", "\n")


# ── 2. TAUX USD/EUR MOYEN MENSUEL (BCE, puis FRED EXUSEU) ────

taux <- NULL; fx_secondaire <- FALSE
if (STATUT == "ok") {
  debut <- min(large$periode)
  cat("\n>> Taux USD/EUR moyen mensuel (BCE EXR.M.USD.EUR.SP00.A) depuis", debut, "...\n")
  taux <- rs_bce_usd_eur_mensuel(debut)
  if (is.null(taux)) {
    ALERTES <- c(ALERTES, "BCE (taux mensuel USD/EUR) indisponible : source secondaire FRED EXUSEU utilisee")
    taux <- rs_fred_exuseu(debut)
    fx_secondaire <- !is.null(taux)
  }
  if (is.null(taux))
    ALERTES <- c(ALERTES, "Taux USD/EUR mensuels indisponibles (BCE et FRED) : prix en USD non calcules")
  else cat("   ", nrow(taux), " mois (", taux$source[1], ")\n", sep = "")
}


# ── 3. CALCUL ET ALIMENTATION DE LA BASE ─────────────────────

statut_qc <- function(fl, av) {
  s <- c(if (isTRUE(fl)) "faible_liquidite", if (isTRUE(av)) "alerte_variation")
  if (length(s) == 0) "ok" else paste(s, collapse = ";")
}

if (STATUT == "ok") {
  d <- rs_rapi_calcul(large, taux)
  de <- as.character(DATE_COLLECTE)
  obs <- list()
  for (p in PARTENAIRES) {
    s <- large[large$partenaire == p, , drop = FALSE]
    obs[[paste0("v", p)]] <- rs_db_obs(paste0("COMEXT_VAL_", p), s$periode, s$valeur_eur, maj, de,
                                       methode = METHODE_COMEXT, url_source = URL_API)
    obs[[paste0("q", p)]] <- rs_db_obs(paste0("COMEXT_QTE_", p), s$periode, s$quantite_100kg, maj, de,
                                       methode = METHODE_COMEXT, url_source = URL_API)
  }
  if (!is.null(taux))
    obs$taux <- rs_db_obs(taux$serie_id, taux$periode, taux$taux_usd_eur, paste0(taux$periode, "-01"), de,
                          methode = paste0("Moyenne mensuelle (USD pour 1 EUR) ; ", taux$source))
  if (nrow(d) > 0) {
    qc <- mapply(statut_qc, d$faible_liquidite, d$alerte_variation)
    m_usd <- paste0("EUR/kg x taux USD/EUR moyen du mois (", ifelse(is.na(d$taux_source), "taux indisponible", d$taux_source), ")")
    obs$ci_eur <- rs_db_obs("RAPI_CI_CIF_EUR", d$periode, d$ci_eur_kg, maj, de, statut_qc = qc,
                            methode = "Valeur declaree (EUR) / (quantite en 100 kg x 100)", url_source = URL_API)
    obs$ci_usd <- rs_db_obs("RAPI_CI_CIF_USD", d$periode, d$ci_usd_kg, maj, de, statut_qc = qc,
                            methode = m_usd, url_source = URL_API)
    obs$as_eur <- rs_db_obs("RAPI_ASIE_CIF_EUR", d$periode, d$asie_eur_kg, maj, de,
                            methode = "Somme des valeurs / somme des quantites (TH, ID, MY, VN)", url_source = URL_API)
    obs$as_usd <- rs_db_obs("RAPI_ASIE_CIF_USD", d$periode, d$asie_usd_kg, maj, de, methode = m_usd, url_source = URL_API)
    obs$dec    <- rs_db_obs("RAPI_CI_DECOTE_ASIE", d$periode, d$decote_pct, maj, de, statut_qc = qc,
                            methode = "(prix CIF UE CI / composite Asie - 1) x 100, calcule en EUR/kg", url_source = URL_API)
  }
  res <- rs_db_ajouter(do.call(rbind, unname(obs)))
  cat("\n>> Base data/db/ : ", res$n_ajoutees, " ligne(s) ajoutee(s), ", res$n_identiques, " deja connue(s), ",
      res$n_revisions, " revision(s)\n", sep = "")
  if (res$n_revisions > 0) for (i in seq_len(nrow(res$revisions))) {
    r <- res$revisions[i, ]
    ALERTES <- c(ALERTES, sprintf("Revision : %s %s : %s -> %s (extraction precedente du %s), historique conserve",
                                  r$serie_id, r$date_valeur, r$ancienne, r$nouvelle, r$date_extraction_ancienne))
  }
}


# ── 4. VALEURS AFFICHEES (API du jour, sinon base avec sa date) ──

obs_db <- rs_db_lire_obs()
date_extraction_valeurs <- as.character(DATE_COLLECTE)

if (STATUT == "indisponible") {
  # Reconstruction depuis la base : derniere version connue de chaque valeur
  morceaux <- lapply(PARTENAIRES, function(p) {
    v <- rs_db_historique(obs_db, paste0("COMEXT_VAL_", p)); q <- rs_db_historique(obs_db, paste0("COMEXT_QTE_", p))
    per <- sort(unique(c(v$date_valeur, q$date_valeur)))
    if (length(per) == 0) return(NULL)
    data.frame(periode = per, partenaire = p, valeur_eur = v$valeur[match(per, v$date_valeur)],
               quantite_100kg = q$valeur[match(per, q$date_valeur)],
               date_source = pmax(v$date_source[match(per, v$date_valeur)], q$date_source[match(per, q$date_valeur)], na.rm = TRUE),
               date_extraction = pmax(v$date_extraction[match(per, v$date_valeur)], q$date_extraction[match(per, q$date_valeur)], na.rm = TRUE),
               stringsAsFactors = FALSE)
  })
  large <- do.call(rbind, morceaux)
  if (!is.null(large) && nrow(large) > 0) {
    maj <- max(large$date_source, na.rm = TRUE)
    date_extraction_valeurs <- max(large$date_extraction, na.rm = TRUE)
    tb <- rs_db_historique(obs_db, "BCE_USD_EUR_M"); tf <- rs_db_historique(obs_db, "FRED_EXUSEU")
    taux <- rbind(
      if (nrow(tb) > 0) data.frame(periode = tb$date_valeur, taux_usd_eur = tb$valeur, source = tb$source, stringsAsFactors = FALSE),
      if (nrow(tf) > 0) { tf <- tf[!tf$date_valeur %in% tb$date_valeur, , drop = FALSE]
        if (nrow(tf) > 0) data.frame(periode = tf$date_valeur, taux_usd_eur = tf$valeur,
                                     source = paste0(tf$source, " (source secondaire)"), stringsAsFactors = FALSE) })
    fx_secondaire <- any(grepl("FRED", taux$source %||% character()))
    cat("   Valeurs reprises de la base (extraction du", date_extraction_valeurs, ", mise a jour Eurostat du", maj, ")\n")
  } else {
    large <- NULL
    ALERTES <- c(ALERTES, "RAPI-CI : aucune valeur douaniere en base - section non alimentee cette semaine")
  }
}

d <- if (!is.null(large)) rs_rapi_calcul(large, taux) else data.frame()

# Revision Eurostat d'un mois = une des series brutes (CI ou Asie) du mois a ete revisee
rev_mois <- function(per) {
  ids <- c(paste0("COMEXT_VAL_", PARTENAIRES), paste0("COMEXT_QTE_", PARTENAIRES))
  s <- obs_db[obs_db$serie_id %in% ids & obs_db$date_valeur == per, , drop = FALSE]
  any(s$revise)
}

# TSR20 Banque mondiale du meme mois (deja dans le JSON, script 01)
jp <- read_json(fichier_json)
hbm <- jp$prix$historique_bm %||% list()
bm_tab <- if (length(hbm) > 0) data.frame(
  periode = vapply(hbm, function(x) as.character(x$periode %||% NA), character(1)),
  usd = vapply(hbm, function(x) suppressWarnings(as.numeric(x$prix_usd %||% NA)), numeric(1)),
  stringsAsFactors = FALSE) else data.frame(periode = character(), usd = numeric())
bm_src <- as.character(jp$prix$reference_mensuelle$bm_tsr20$source %||% "Banque mondiale, Pink Sheet mensuelle (Rubber, TSR20)")

r3 <- function(x) if (is.finite(x)) round(x, 3) else NA_real_
r1 <- function(x) if (is.finite(x)) round(x, 1) else NA_real_

fiche <- function(valeur, unite, periode, nature, methode, source = RS_RAPI_SOURCE, ...)
  rs_iosco(rs_valeur(valeur, unite, source, maj, methode, periode = periode, url = URL_API,
                     publiable = rs_serie_publiable("RAPI_CI"), ...), nature, "rapi_ci")

mois <- list()
if (nrow(d) > 0) {
  d <- d[is.finite(d$ci_eur_kg) | is.finite(d$volume_ci_t), , drop = FALSE]
  d <- utils::tail(d, 12)
  for (i in seq_len(nrow(d))) {
    x <- d[i, ]
    sec <- isTRUE(grepl("FRED", x$taux_source))
    src_usd <- paste0(RS_RAPI_SOURCE, " ; taux : ", if (is.na(x$taux_source)) "indisponible" else x$taux_source)
    bm <- bm_tab$usd[match(x$periode, bm_tab$periode)]
    ec <- if (is.finite(bm) && is.finite(x$ci_usd_kg)) (x$ci_usd_kg / bm - 1) * 100 else NA_real_
    mois[[i]] <- list(
      periode = x$periode,
      volume_ci_t = if (is.finite(x$volume_ci_t)) round(x$volume_ci_t) else NA_real_,
      ci_eur_kg = r3(x$ci_eur_kg), ci_usd_kg = r3(x$ci_usd_kg),
      asie_eur_kg = r3(x$asie_eur_kg), asie_usd_kg = r3(x$asie_usd_kg),
      decote_pct = r1(x$decote_pct),
      taux_usd_eur = if (is.finite(x$taux_usd_eur)) round(x$taux_usd_eur, 4) else NA_real_,
      taux_source = x$taux_source, taux_source_secondaire = sec,
      variation_ci_pct = r1(x$variation_pct),
      faible_liquidite = isTRUE(x$faible_liquidite),
      alerte_variation = isTRUE(x$alerte_variation),
      revise = rev_mois(x$periode),
      asie_complet = isTRUE(x$asie_complet),
      asie_manquants = if (nzchar(x$asie_manquants)) strsplit(x$asie_manquants, ",")[[1]] else character(),
      bm_tsr20_usd_kg = if (is.finite(bm)) bm else NA_real_,
      ecart_ci_bm_pct = r1(ec),
      fiches = list(
        volume_ci = fiche(if (is.finite(x$volume_ci_t)) round(x$volume_ci_t) else NA_real_, "t", x$periode, "T",
                          "Quantite declaree (100 kg) / 10"),
        ci_eur_kg = fiche(r3(x$ci_eur_kg), "EUR/kg", x$periode, "T", "Valeur declaree (EUR) / (quantite en 100 kg x 100)"),
        ci_usd_kg = fiche(r3(x$ci_usd_kg), "USD/kg", x$periode, "T", "EUR/kg x taux USD/EUR moyen du mois",
                          source = src_usd, taux_source_secondaire = sec),
        asie_usd_kg = fiche(r3(x$asie_usd_kg), "USD/kg", x$periode, "T",
                            "Somme des valeurs / somme des quantites (TH, ID, MY, VN) x taux USD/EUR moyen du mois",
                            source = src_usd, taux_source_secondaire = sec),
        decote_ci_asie_pct = fiche(r1(x$decote_pct), "%", x$periode, "E",
                                   "(prix CIF UE CI / composite Asie - 1) x 100, calcule en EUR/kg"),
        bm_tsr20 = rs_iosco(rs_valeur(if (is.finite(bm)) bm else NA_real_, "USD/kg", bm_src,
                                      if (is.finite(bm)) paste0(x$periode, "-01") else NA,
                                      "Moyenne mensuelle Banque mondiale du meme mois (comparaison, hors indice)",
                                      periode = x$periode), "T", "reference_mensuelle")
      ))
    if (isTRUE(x$faible_liquidite))
      ALERTES <- c(ALERTES, sprintf("RAPI-CI %s : faible liquidite (%s t < %s t)", x$periode, round(x$volume_ci_t), RS_RAPI_SEUIL_LIQUIDITE_T))
    if (isTRUE(x$alerte_variation))
      ALERTES <- c(ALERTES, sprintf("RAPI-CI %s : variation mensuelle du prix CI de %s %% (seuil %s %%) - publiee avec mention",
                                    x$periode, r1(x$variation_pct), RS_RAPI_SEUIL_VARIATION * 100))
    if (!isTRUE(x$asie_complet))
      ALERTES <- c(ALERTES, sprintf("RAPI-CI %s : composite Asie incomplet (manque %s) - non calcule", x$periode, x$asie_manquants))
  }
  ALERTES <- unique(ALERTES)   # alertes limitees aux 12 mois publies
}

derniere <- if (length(mois) > 0) mois[[length(mois)]] else NULL
fr_rapi  <- rs_rapi_fraicheur(maj, derniere$periode %||% NA_character_, DATE_COLLECTE)
if (!is.null(derniere) && !identical(fr_rapi$statut, "derniere_valeur_disponible"))
  ALERTES <- c(ALERTES, sprintf("RAPI-CI : mise a jour Eurostat du %s, plus de %d jours - valeurs perimees",
                                maj, RS_MAX_JOURS_MENSUEL))


# ── 5. BLOC rapi_ci DU JSON DE LA SEMAINE ────────────────────

bloc <- list(
  statut = STATUT,
  nouvelle_extraction = STATUT == "ok",
  valeurs_issues_de = if (STATUT == "ok") "API Eurostat du jour" else if (length(mois) > 0) "base RubberSignal (derniere extraction)" else "aucune",
  date_extraction_valeurs = if (length(mois) > 0) date_extraction_valeurs else NA_character_,
  version_methode = rs_version_methode("rapi_ci"),
  date_effet_methode = RS_METHODES$date_effet[RS_METHODES$cle == "rapi_ci"],
  source = RS_RAPI_SOURCE,
  produit = paste0(RS_RAPI_PRODUIT_LIBELLE, " : caoutchouc naturel techniquement specifie (TSNR)"),
  declarant = "UE (code API : EU)", flux = "importations",
  partenaire = RS_RAPI_PARTENAIRE_CI, partenaires_asie = RS_RAPI_PARTENAIRES_ASIE,
  url_source = URL_API,
  date_maj_eurostat = maj,
  updated_eurostat = maj_brut,
  derniere_periode = derniere$periode %||% NA_character_,
  statut_fraicheur = if (length(mois) > 0) fr_rapi$statut else "indisponible",
  age_jours_maj_eurostat = fr_rapi$age_jours_maj,
  age_jours_fin_periode = fr_rapi$age_jours_fin_periode,
  fichier_brut = fichier_brut,
  taux_change = list(
    source_principale = "BCE, moyenne mensuelle du taux de reference USD/EUR (EXR.M.USD.EUR.SP00.A)",
    url = RS_URL_BCE_EXR_M,
    source_secondaire = "FRED EXUSEU (Federal Reserve), utilisee seulement si la BCE echoue",
    secondaire_utilise = isTRUE(fx_secondaire)),
  seuils = list(faible_liquidite_t = RS_RAPI_SEUIL_LIQUIDITE_T, variation_pct = RS_RAPI_SEUIL_VARIATION * 100),
  nature = list(prix_cif = "T", decote = "E", volume = "T"),
  publiable = rs_serie_publiable("RAPI_CI"),
  licence = rs_serie_info("EUROSTAT_COMEXT")$licence,
  mois = mois,
  derniere = derniere,
  fob_abidjan = list(statut = "non_publie",
                     raison = "aucune source de fret publique, datee et reutilisable n'est retenue (methodologie 1.3)"),
  limites_fr = RS_RAPI_LIMITES_FR,
  limites_en = RS_RAPI_LIMITES_EN,
  alertes = as.list(ALERTES)
)

jp$rapi_ci <- bloc
write_json(jp, fichier_json, pretty = TRUE, auto_unbox = TRUE, na = "null", null = "null")

for (a in ALERTES) cat("::warning::", a, "\n", sep = "")   # visible dans GitHub Actions


# ── 6. RESUME ────────────────────────────────────────────────

cat("\n", strrep("=", 55), "\n")
cat("RESUME DOUANES UE — Semaine", SEMAINE, "/", ANNEE, "\n")
cat(strrep("=", 55), "\n")
cat("Statut Comext      :", STATUT, "(valeurs :", bloc$valeurs_issues_de, ")\n")
cat("Mise a jour        :", maj %||% "inconnue", "| derniere periode :", bloc$derniere_periode, "\n")
if (!is.null(derniere)) {
  cat("Volume CI          :", derniere$volume_ci_t, "t\n")
  cat("Prix CIF UE CI     :", derniere$ci_eur_kg, "EUR/kg =", derniere$ci_usd_kg, "USD/kg\n")
  cat("Composite Asie     :", derniere$asie_usd_kg, "USD/kg | ecart CI/Asie :", derniere$decote_pct, "%\n")
}
cat("Alertes            :", length(ALERTES), "\n")
cat("JSON               :", fichier_json, "\n")
cat(strrep("=", 55), "\n\n")
