# =============================================================
# RUBBERSIGNAL TECHNOLOGIES — Script 06b : Douanes UE -> RAPI-Afrique v1
# Auteur  : Martial Sahiri
# Version : 1.0 (18/11/2026)
# Objectif: étendre RAPI-CI aux autres origines africaines qui exportent chaque
#           mois vers l'UE (Liberia, Ghana, Nigeria, Cameroun) :
#             - volumes et valeurs de trois produits (NC 4001 22 00 TSNR,
#               4001 29 00 autres formes sèches, 4001 10 00 latex), CI comprise ;
#             - prix CIF UE du TSNR (EUR/kg, USD/kg) et écart à la CI.
#           Règles : 00_regles.R, section 11 bis.
# Entrées : API Eurostat Comext (sans clé) ; taux USD/EUR mensuels déjà en base
#           (écrits par le script 06 : aucun appel BCE supplémentaire).
# Sorties : data/raw/comext_afrique_AAAAMMJJ.json (réponse brute)
#           data/db/observations.csv (ajout idempotent, révisions détectées)
#           bloc `rapi_afrique` du JSON de la semaine
# Échec de l'API : statut « indisponible », ::warning::, valeurs reprises de la
#           base AVEC leur date (jamais présentées comme nouvelles).
# Usage   : Rscript scripts/06b_collect_afrique.R   (après 06, avant 03)
# =============================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(lubridate)
})

source(if (file.exists("scripts/00_sources.R")) "scripts/00_sources.R" else "00_sources.R")   # + 00_regles.R
source(if (file.exists("scripts/00_base.R")) "scripts/00_base.R" else "00_base.R")

DATE_COLLECTE <- as.Date(Sys.getenv("RUBBERSIGNAL_TODAY", as.character(Sys.Date())))
SEMAINE       <- isoweek(DATE_COLLECTE)
ANNEE         <- year(DATE_COLLECTE)
fichier_json  <- paste0("data/processed/rubbersignal_S", SEMAINE, "_", ANNEE, ".json")
if (!file.exists(fichier_json))
  stop("JSON introuvable : ", fichier_json, " - lancer le script 01 d'abord.")

ALERTES <- character()
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
rs_db_init()

cat("=== RUBBERSIGNAL — Origines africaines (Eurostat Comext) du", format(DATE_COLLECTE, "%d/%m/%Y"), "===\n\n")

ORIGINES <- setdiff(RS_AFR_PARTENAIRES, RS_RAPI_PARTENAIRE_CI)
requete  <- rs_comext_requete(RS_AFR_PARTENAIRES, n_mois = 24, produits = RS_AFR_PRODUITS$code)
URL_API  <- rs_comext_url(requete)
METHODE_COMEXT <- "Eurostat Comext DS-045409 : declarant EU, flux importations, frequence mensuelle"


# ── 1. EUROSTAT COMEXT ───────────────────────────────────────

cat(">> Eurostat Comext (", paste(RS_AFR_PRODUITS$libelle_nc, collapse = ", "), " ; ",
    paste(RS_AFR_PARTENAIRES, collapse = ", "), ")...\n", sep = "")
brut <- rs_comext_telecharger(requete)
large <- NULL; maj <- NA_character_; fichier_brut <- NA_character_
if (!is.null(brut)) {
  fichier_brut <- sprintf("data/raw/comext_afrique_%s.json", format(DATE_COLLECTE, "%Y%m%d"))
  con <- file(fichier_brut, open = "wb"); writeBin(charToRaw(enc2utf8(brut)), con); close(con)
  cat("   Reponse brute :", fichier_brut, "\n")
  large <- tryCatch(rs_comext_large_produits(rs_comext_lire(brut)), error = function(e) {
    ALERTES <<- c(ALERTES, paste0("Eurostat Comext (Afrique) : reponse illisible (", conditionMessage(e), ")"))
    NULL })
  if (!is.null(large)) {
    maj <- attr(large, "updated")
    if (is.na(maj)) {
      ALERTES <- c(ALERTES, "Eurostat Comext (Afrique) : date de mise a jour absente - valeurs non publiees")
      large <- NULL
    }
  }
} else {
  ALERTES <- c(ALERTES, "Eurostat Comext (Afrique) indisponible cette semaine : dernieres valeurs de la base reprises avec leur date")
}
STATUT <- if (!is.null(large)) "ok" else "indisponible"
cat("   Statut :", STATUT, if (!is.na(maj)) paste("| mise a jour Eurostat :", maj) else "", "\n")


# ── 2. ALIMENTATION DE LA BASE ───────────────────────────────

obs_db <- rs_db_lire_obs()
taux   <- rs_db_taux_mensuels(obs_db)
de     <- as.character(DATE_COLLECTE)
tsnr   <- NULL

statut_qc <- function(fl, av) {
  s <- c(if (isTRUE(fl)) "faible_liquidite", if (isTRUE(av)) "alerte_variation")
  if (length(s) == 0) "ok" else paste(s, collapse = ";")
}

if (STATUT == "ok") {
  obs <- list()
  for (k in RS_AFR_PRODUITS$code) for (p in RS_AFR_PARTENAIRES) {
    if (k == RS_RAPI_PRODUIT && p == RS_RAPI_PARTENAIRE_CI) next   # serie de RAPI-CI (script 06)
    s <- large[large$produit == k & large$partenaire == p &
                 (is.finite(large$valeur_eur) | is.finite(large$quantite_100kg)), , drop = FALSE]
    if (nrow(s) == 0) next
    m <- paste0(METHODE_COMEXT, ", produit ", k)
    v <- s[is.finite(s$valeur_eur), , drop = FALSE]; q <- s[is.finite(s$quantite_100kg), , drop = FALSE]
    if (nrow(v) > 0) obs[[paste0("v", p, k)]] <- rs_db_obs(rs_afr_id_brut("VAL", p, k), v$periode, v$valeur_eur, maj, de,
                                                           methode = m, url_source = URL_API)
    if (nrow(q) > 0) obs[[paste0("q", p, k)]] <- rs_db_obs(rs_afr_id_brut("QTE", p, k), q$periode, q$quantite_100kg, maj, de,
                                                           methode = m, url_source = URL_API)
  }
  tsnr <- large[large$produit == RS_RAPI_PRODUIT, , drop = FALSE]
  d <- rs_afrique_calcul(tsnr, taux)
  if (nrow(d) > 0) {
    qc <- mapply(statut_qc, d$faible_liquidite, d$alerte_variation)
    m_usd <- paste0("EUR/kg x taux USD/EUR moyen du mois (", ifelse(is.na(d$taux_source), "taux indisponible", d$taux_source), ")")
    for (p in unique(d$partenaire)) {
      i <- d$partenaire == p
      e <- i & is.finite(d$eur_kg); u <- i & is.finite(d$usd_kg); g <- i & is.finite(d$ecart_ci_pct)
      if (any(e)) obs[[paste0("pe", p)]] <- rs_db_obs(paste0("RAPI_", p, "_CIF_EUR"), d$periode[e], d$eur_kg[e], maj, de,
                                                      statut_qc = qc[e], url_source = URL_API,
                                                      methode = "Valeur declaree (EUR) / (quantite en 100 kg x 100)")
      if (any(u)) obs[[paste0("pu", p)]] <- rs_db_obs(paste0("RAPI_", p, "_CIF_USD"), d$periode[u], d$usd_kg[u], maj, de,
                                                      statut_qc = qc[u], url_source = URL_API, methode = m_usd[u])
      if (any(g)) obs[[paste0("pg", p)]] <- rs_db_obs(paste0("RAPI_", p, "_ECART_CI"), d$periode[g], d$ecart_ci_pct[g], maj, de,
                                                      statut_qc = qc[g], url_source = URL_API,
                                                      methode = "(prix CIF UE origine / prix CIF UE CI du meme mois - 1) x 100, en EUR/kg")
    }
  }
  if (length(obs) > 0) {
    res <- rs_db_ajouter(do.call(rbind, unname(obs)))
    cat("\n>> Base data/db/ : ", res$n_ajoutees, " ligne(s) ajoutee(s), ", res$n_identiques, " deja connue(s), ",
        res$n_revisions, " revision(s)\n", sep = "")
    if (res$n_revisions > 0) for (i in seq_len(nrow(res$revisions))) {
      r <- res$revisions[i, ]
      ALERTES <- c(ALERTES, sprintf("Revision : %s %s : %s -> %s (extraction precedente du %s), historique conserve",
                                    r$serie_id, r$date_valeur, r$ancienne, r$nouvelle, r$date_extraction_ancienne))
    }
  }
  obs_db <- rs_db_lire_obs()
}


# ── 3. VALEURS AFFICHEES (API du jour, sinon base avec sa date) ──

date_extraction_valeurs <- de
if (is.null(tsnr)) {
  morceaux <- lapply(RS_AFR_PARTENAIRES, function(p) {
    v <- rs_db_historique(obs_db, rs_afr_id_brut("VAL", p)); q <- rs_db_historique(obs_db, rs_afr_id_brut("QTE", p))
    per <- sort(unique(c(v$date_valeur, q$date_valeur)))
    if (length(per) == 0) return(NULL)
    data.frame(periode = per, partenaire = p, valeur_eur = as.numeric(v$valeur[match(per, v$date_valeur)]),
               quantite_100kg = as.numeric(q$valeur[match(per, q$date_valeur)]),
               date_source = v$date_source[match(per, v$date_valeur)],
               date_extraction = v$date_extraction[match(per, v$date_valeur)], stringsAsFactors = FALSE)
  })
  tsnr <- do.call(rbind, morceaux)
  if (!is.null(tsnr) && nrow(tsnr) > 0) {
    maj <- max(tsnr$date_source, na.rm = TRUE)
    date_extraction_valeurs <- max(tsnr$date_extraction, na.rm = TRUE)
  }
}
d <- rs_afrique_calcul(tsnr, taux)

# Volumes des trois produits sur les 6 derniers mois publies par la CI (TSNR)
volumes_6m <- function(p) {
  ci_q <- rs_db_historique(obs_db, rs_afr_id_brut("QTE", RS_RAPI_PARTENAIRE_CI))
  mois <- utils::tail(sort(ci_q$date_valeur[is.finite(as.numeric(ci_q$valeur))]), 6)
  setNames(lapply(seq_len(nrow(RS_AFR_PRODUITS)), function(k) {
    h <- rs_db_historique(obs_db, rs_afr_id_brut("QTE", p, RS_AFR_PRODUITS$code[k]))
    q <- as.numeric(h$valeur[match(mois, h$date_valeur)])
    if (any(is.finite(q))) round(sum(q, na.rm = TRUE) / 10) else 0
  }), RS_AFR_PRODUITS$cle)
}

r3 <- function(x) if (is.finite(x)) round(x, 3) else NA_real_
r1 <- function(x) if (is.finite(x)) round(x, 1) else NA_real_

origines <- lapply(RS_AFR_PARTENAIRES, function(p) {
  nm <- RS_AFR_NOMS[[p]]
  out <- list(code = p, nom_fr = nm[1], nom_en = nm[2], volumes_6m_t = volumes_6m(p))
  if (p == RS_RAPI_PARTENAIRE_CI) return(c(out, list(reference = TRUE)))
  x <- d[d$partenaire == p, , drop = FALSE]
  if (nrow(x) == 0) return(c(out, list(derniere = NULL, mois = list())))
  x <- utils::tail(x, 12)
  mois <- lapply(seq_len(nrow(x)), function(i) list(
    periode = x$periode[i], vol_t = if (is.finite(x$vol_t[i])) round(x$vol_t[i]) else NA_real_,
    eur_kg = r3(x$eur_kg[i]), usd_kg = r3(x$usd_kg[i]), ecart_ci_pct = r1(x$ecart_ci_pct[i]),
    faible_liquidite = isTRUE(x$faible_liquidite[i]), alerte_variation = isTRUE(x$alerte_variation[i]),
    taux_source_secondaire = isTRUE(grepl("FRED", x$taux_source[i]))))
  der <- mois[[length(mois)]]
  if (isTRUE(der$faible_liquidite))
    ALERTES <<- c(ALERTES, sprintf("RAPI-Afrique %s %s : faible liquidite (%s t < %s t) - prix publie avec mention",
                                   nm[1], der$periode, der$vol_t, RS_AFR_SEUIL_LIQUIDITE_T))
  if (isTRUE(der$alerte_variation))
    ALERTES <<- c(ALERTES, sprintf("RAPI-Afrique %s %s : variation mensuelle du prix de %s %% (seuil %s %%)",
                                   nm[1], der$periode, r1(x$variation_pct[nrow(x)]), RS_RAPI_SEUIL_VARIATION * 100))
  c(out, list(derniere = der, mois = mois))
})

derniere_periode <- if (nrow(d) > 0) max(d$periode[is.finite(d$eur_kg)]) else NA_character_
fr <- rs_rapi_fraicheur(maj, derniere_periode, DATE_COLLECTE)
if (nrow(d) > 0 && !identical(fr$statut, "derniere_valeur_disponible"))
  ALERTES <- c(ALERTES, sprintf("RAPI-Afrique : mise a jour Eurostat du %s, plus de %d jours - valeurs perimees",
                                maj, RS_MAX_JOURS_MENSUEL))


# ── 4. BLOC rapi_afrique DU JSON DE LA SEMAINE ───────────────

jp <- read_json(fichier_json)
jp$rapi_afrique <- list(
  statut = STATUT,
  nouvelle_extraction = STATUT == "ok",
  valeurs_issues_de = if (STATUT == "ok") "API Eurostat du jour" else if (nrow(d) > 0) "base RubberSignal Technologies (derniere extraction)" else "aucune",
  date_extraction_valeurs = if (nrow(d) > 0) date_extraction_valeurs else NA_character_,
  version_methode = rs_version_methode("rapi_afrique"),
  date_effet_methode = RS_METHODES$date_effet[RS_METHODES$cle == "rapi_afrique"],
  source = RS_RAPI_SOURCE, url_source = URL_API, fichier_brut = fichier_brut,
  date_maj_eurostat = maj, derniere_periode = derniere_periode,
  statut_fraicheur = if (nrow(d) > 0) fr$statut else "indisponible",
  produits = lapply(seq_len(nrow(RS_AFR_PRODUITS)), function(k) as.list(RS_AFR_PRODUITS[k, ])),
  seuils = list(faible_liquidite_t = RS_AFR_SEUIL_LIQUIDITE_T, variation_pct = RS_RAPI_SEUIL_VARIATION * 100),
  nature = list(prix_cif = "T", ecart_ci = "E", volume = "T"),
  publiable = rs_serie_publiable("RAPI_AFRIQUE"),
  licence = rs_serie_info("EUROSTAT_COMEXT")$licence,
  phrase_fr = rs_afrique_phrase(d, "fr"), phrase_en = rs_afrique_phrase(d, "en"),
  origines = origines,
  alertes = as.list(ALERTES))
write_json(jp, fichier_json, pretty = TRUE, auto_unbox = TRUE, na = "null", null = "null")

for (a in ALERTES) cat("::warning::", a, "\n", sep = "")


# ── 5. RESUME ────────────────────────────────────────────────

cat("\n", strrep("=", 55), "\n")
cat("RESUME ORIGINES AFRICAINES — Semaine", SEMAINE, "/", ANNEE, "\n")
cat(strrep("=", 55), "\n")
cat("Statut Comext :", STATUT, "| mise a jour :", maj %||% "inconnue", "| dernier mois :", derniere_periode, "\n")
for (o in origines) if (!isTRUE(o$reference) && !is.null(o$derniere))
  cat(sprintf("  %-9s %s : %s EUR/kg, %s t, ecart CI %s %%\n", o$nom_fr, o$derniere$periode,
              o$derniere$eur_kg, o$derniere$vol_t, o$derniere$ecart_ci_pct))
cat(rs_afrique_phrase(d, "fr"), "\n")
cat("Alertes       :", length(ALERTES), "\n")
cat(strrep("=", 55), "\n\n")
