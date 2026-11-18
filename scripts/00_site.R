# ============================================================
# RUBBERSIGNAL — Données publiques du site vitrine et du terminal
# ============================================================
# Role    : construire, à partir de la base data/db/ et du JSON de la semaine,
#           le jeu de données PUBLIC lu par site/index.html et site/terminal.html.
# Règles  : - séries « publiable : oui » uniquement (registre des licences) ;
#           - champs choisis un par un dans le JSON (liste blanche) : rien de ce
#             qui dérive d'une cotation sous licence n'est copié ;
#           - aucune clé, aucun appel réseau, aucun calcul nouveau hors RSCI
#             historique (même fonction rs_calcul_rsci que le rapport).
# Prérequis : 00_regles.R et 00_base.R chargés.
# ============================================================

RS_SITE_DIR <- "site/data"
RS_XOF_PAR_EUR <- 655.957   # parité fixe FCFA/EUR

.rs_num1 <- function(x) { v <- suppressWarnings(as.numeric(unlist(x %||% NA))); if (length(v) == 1 && is.finite(v)) v else NA_real_ }
.rs_txt1 <- function(x, d = NA_character_) { v <- unlist(x %||% NA); if (length(v) == 1 && !is.na(v)) as.character(v) else d }

# Points d'une série : dernière version connue par date, valeurs numériques
rs_site_points <- function(obs, serie_id) {
  h <- rs_db_historique(obs, serie_id)
  if (nrow(h) == 0) return(data.frame(p = character(), v = numeric(), revise = logical(), qc = character()))
  data.frame(p = h$date_valeur, v = as.numeric(h$valeur), revise = as.logical(h$deja_revise),
             qc = ifelse(is.na(h$statut_qc), "", h$statut_qc), stringsAsFactors = FALSE)
}

# Matrice RAPI-CI : tout l'historique de la base (un élément par mois)
rs_site_rapi_mois <- function(obs) {
  s <- lapply(c(ci_eur = "RAPI_CI_CIF_EUR", ci_usd = "RAPI_CI_CIF_USD", asie_eur = "RAPI_ASIE_CIF_EUR",
                asie_usd = "RAPI_ASIE_CIF_USD", decote = "RAPI_CI_DECOTE_ASIE", qte = "COMEXT_QTE_CI",
                val = "COMEXT_VAL_CI", bm = "BM_TSR20"), function(id) rs_site_points(obs, id))
  mois <- sort(s$ci_eur$p)
  g <- function(k, m) { i <- match(m, s[[k]]$p); if (is.na(i)) NA_real_ else s[[k]]$v[i] }
  lapply(mois, function(m) {
    i <- match(m, s$ci_usd$p); qc <- if (is.na(i)) "" else s$ci_usd$qc[i]
    rev <- any(s$qte$revise[s$qte$p == m], s$val$revise[s$val$p == m], na.rm = TRUE)
    list(p = m, vol_t = if (is.finite(g("qte", m))) round(g("qte", m) / 10) else NA_real_,
         ci_eur = g("ci_eur", m), ci_usd = g("ci_usd", m), asie_eur = g("asie_eur", m), asie_usd = g("asie_usd", m),
         decote = g("decote", m), tsr20_bm = g("bm", m), revise = isTRUE(rev),
         faible_liquidite = grepl("faible_liquidite", qc), alerte_variation = grepl("alerte_variation", qc),
         asie_complet = is.finite(g("asie_usd", m)))
  })
}

# Importations UE par origine sur les n derniers mois communs (volume, prix moyen pondéré)
rs_site_origines <- function(obs, n = 6) {
  pays <- list(CI = c("Côte d'Ivoire", "Côte d'Ivoire"), TH = c("Thaïlande", "Thailand"), ID = c("Indonésie", "Indonesia"),
               MY = c("Malaisie", "Malaysia"), VN = c("Vietnam", "Vietnam"))
  ci <- rs_site_points(obs, "COMEXT_QTE_CI")
  ci <- ci[is.finite(ci$v), , drop = FALSE]
  if (nrow(ci) == 0) return(NULL)
  mois <- utils::tail(sort(ci$p), n)
  lignes <- lapply(names(pays), function(k) {
    q <- rs_site_points(obs, paste0("COMEXT_QTE_", k)); v <- rs_site_points(obs, paste0("COMEXT_VAL_", k))
    qm <- q$v[match(mois, q$p)]; vm <- v$v[match(mois, v$p)]
    ok <- is.finite(qm) & is.finite(vm) & qm > 0
    list(code = k, nom_fr = pays[[k]][1], nom_en = pays[[k]][2], n_mois = sum(ok),
         vol_t = if (any(ok)) round(sum(qm[ok]) / 10) else NA_real_,
         prix_eur_kg = if (any(ok)) round(sum(vm[ok]) / (sum(qm[ok]) * 100), 3) else NA_real_)
  })
  list(debut = mois[1], fin = mois[length(mois)], pays = lignes)
}

# RAPI-Afrique : historique mensuel par origine (TSNR) et résumé des n derniers
# mois publiés pour la CI (volumes des trois produits, prix moyen pondéré).
rs_site_afrique <- function(obs, n = 6) {
  ci_q <- rs_site_points(obs, "COMEXT_QTE_CI"); ci_q <- ci_q[is.finite(ci_q$v), , drop = FALSE]
  if (nrow(ci_q) == 0) return(NULL)
  autres <- setdiff(RS_AFR_PARTENAIRES, RS_RAPI_PARTENAIRE_CI)
  if (!any(vapply(autres, function(p) nrow(rs_site_points(obs, paste0("RAPI_", p, "_CIF_EUR"))) > 0, logical(1)))) return(NULL)
  mois6 <- utils::tail(sort(ci_q$p), n)
  sumq <- function(id) { q <- rs_site_points(obs, id); v <- q$v[match(mois6, q$p)]; if (any(is.finite(v))) round(sum(v, na.rm = TRUE) / 10) else 0 }
  pond <- function(id_prix, id_q) {
    pr <- rs_site_points(obs, id_prix); q <- rs_site_points(obs, id_q)
    a <- pr$v[match(mois6, pr$p)]; w <- q$v[match(mois6, q$p)]; ok <- is.finite(a) & is.finite(w) & w > 0
    if (any(ok)) round(sum(a[ok] * w[ok]) / sum(w[ok]), 3) else NA_real_
  }
  origines <- lapply(RS_AFR_PARTENAIRES, function(p) {
    ci <- p == RS_RAPI_PARTENAIRE_CI; nm <- RS_AFR_NOMS[[p]]
    ids <- if (ci) c(eur = "RAPI_CI_CIF_EUR", usd = "RAPI_CI_CIF_USD", ecart = NA)
           else c(eur = paste0("RAPI_", p, "_CIF_EUR"), usd = paste0("RAPI_", p, "_CIF_USD"), ecart = paste0("RAPI_", p, "_ECART_CI"))
    s <- list(eur = rs_site_points(obs, ids[["eur"]]), usd = rs_site_points(obs, ids[["usd"]]),
              ecart = if (ci) NULL else rs_site_points(obs, ids[["ecart"]]), qte = rs_site_points(obs, rs_afr_id_brut("QTE", p)),
              val = rs_site_points(obs, rs_afr_id_brut("VAL", p)))
    g <- function(k, m) { x <- s[[k]]; if (is.null(x)) return(NA_real_); i <- match(m, x$p); if (is.na(i)) NA_real_ else x$v[i] }
    mois <- lapply(sort(s$eur$p), function(m) {
      i <- match(m, s$eur$p); qc <- s$eur$qc[i]
      list(p = m, vol_t = if (is.finite(g("qte", m))) round(g("qte", m) / 10) else NA_real_,
           eur = g("eur", m), usd = g("usd", m), ecart = if (ci) 0 else g("ecart", m),
           faible_liquidite = grepl("faible_liquidite", qc), alerte_variation = grepl("alerte_variation", qc),
           revise = isTRUE(any(s$qte$revise[s$qte$p == m], s$val$revise[s$val$p == m], na.rm = TRUE)))
    })
    pe <- pond(ids[["eur"]], rs_afr_id_brut("QTE", p)); pce <- pond("RAPI_CI_CIF_EUR", "COMEXT_QTE_CI")
    list(code = p, nom_fr = nm[1], nom_en = nm[2], reference = ci,
         vol_tsnr_t = sumq(rs_afr_id_brut("QTE", p)),
         vol_autres_t = sumq(rs_afr_id_brut("QTE", p, "40012900")), vol_latex_t = sumq(rs_afr_id_brut("QTE", p, "40011000")),
         prix_eur_kg = pe, prix_usd_kg = pond(ids[["usd"]], rs_afr_id_brut("QTE", p)),
         ecart_ci_pct = if (ci) 0 else if (is.finite(pe) && is.finite(pce)) round((pe / pce - 1) * 100, 1) else NA_real_,
         mois = mois)
  })
  list(version = rs_version_methode("rapi_afrique"), seuil_liquidite_t = RS_AFR_SEUIL_LIQUIDITE_T,
       debut = mois6[1], fin = mois6[length(mois6)], origines = origines)
}

# Production FAOSTAT : dernière année par pays (production, surface, rendement,
# nature d'après le drapeau FAO), parts du monde et de l'Afrique, historique de
# production ; zones satellite (data/reference/zones_satellite.csv).
rs_site_production <- function(obs, zones_f = "data/reference/zones_satellite.csv") {
  pp <- rs_site_points(obs, rs_fao_id("PROD", "WLD"))
  if (nrow(pp) == 0) return(NULL)
  an <- max(pp$p)
  serie <- function(e, code) rs_site_points(obs, rs_fao_id(e, code))
  nat <- function(code) { h <- obs[obs$serie_id == rs_fao_id("PROD", code) & obs$date_valeur == an, , drop = FALSE]
                          if (nrow(h)) h$nature[nrow(h)] else NA_character_ }
  val <- function(e, code, a = an) { s <- serie(e, code); v <- s$v[match(a, s$p)]; if (length(v) && is.finite(v)) v else NA_real_ }
  monde <- val("PROD", "WLD"); afr <- val("PROD", "AFR")
  pays <- lapply(which(RS_FAO_PAYS$groupe != "total"), function(i) {
    p <- RS_FAO_PAYS[i, ]; pr <- val("PROD", p$code)
    h <- serie("PROD", p$code); h <- h[as.integer(h$p) >= 2010, , drop = FALSE]
    list(code = p$code, nom_fr = p$nom_fr, nom_en = p$nom_en, groupe = p$groupe,
         prod_t = pr, surf_ha = val("SURF", p$code), rend_kg_ha = val("REND", p$code),
         prod_2020_t = val("PROD", p$code, "2020"), nature = nat(p$code),
         part_monde_pct = if (is.finite(pr) && is.finite(monde) && monde > 0) round(100 * pr / monde, 2) else NA_real_,
         part_afrique_pct = if (p$groupe == "afrique" && is.finite(pr) && is.finite(afr) && afr > 0) round(100 * pr / afr, 1) else NA_real_,
         historique = lapply(seq_len(nrow(h)), function(k) list(p = h$p[k], v = h$v[k])))
  })
  zones <- if (file.exists(zones_f)) {
    z <- utils::read.csv(zones_f, stringsAsFactors = FALSE, encoding = "UTF-8")
    lapply(seq_len(nrow(z)), function(k) list(id = z$id[k], pays = z$pays[k], lat = z$lat[k], lon = z$lon[k],
      image = paste0("assets/sat/", z$id[k], ".jpg"), titre_fr = z$titre_fr[k], titre_en = z$titre_en[k],
      legende_fr = z$legende_fr[k], legende_en = z$legende_en[k], source = z$sources[k], coordonnees = z$coordonnees[k],
      largeur_km = round(256 * z$nx[k] * 156543.03392 * cos(z$lat[k] * pi / 180) / 2^z$zoom[k] / 1000, 1)))
  } else list()
  list(version = rs_version_methode("faostat"), annee = an, monde_t = monde, afrique_t = afr, asie_t = val("PROD", "ASI"),
       maj_fao = max(obs$date_source[startsWith(obs$serie_id, "FAO_")], na.rm = TRUE),
       licence = "FAOSTAT, CC BY 4.0", pays = pays, zones = zones,
       satellite_credit = "EOxCloudless 2016 https://cloudless.eox.at by EOX IT Services GmbH (Contains modified Copernicus Sentinel data 2016), CC BY 4.0",
       carte_credit = "Natural Earth (domaine public)")
}

# Historique du RSCI (v2.1) : un point par mois de prix APROMAC
#   apromac : data.frame(mois, prix_fcfa, type, source, date_publication, origine)
rs_site_rsci_historique <- function(apromac, obs) {
  if (is.null(apromac) || nrow(apromac) == 0) return(list())
  bm <- rs_site_points(obs, "BM_TSR20"); fx <- rs_site_points(obs, "BCE_USD_EUR_M")
  fx2 <- rs_site_points(obs, "FRED_EXUSEU")
  lapply(seq_len(nrow(apromac)), function(i) {
    a <- apromac[i, ]
    ref <- rs_mois_precedent(a$mois)
    tsr <- bm$v[match(ref, bm$p)]
    t   <- fx$v[match(ref, fx$p)]; sec <- FALSE
    if (!is.finite(t)) { t <- fx2$v[match(ref, fx2$p)]; sec <- is.finite(t) }
    calc <- rs_calcul_rsci(as.numeric(a$prix_fcfa), if (is.finite(t)) RS_XOF_PAR_EUR / t else NA_real_,
                           tsr, a$mois, RS_DRC_REFERENCE)
    faible <- grepl("source faible|non officielle", a$source %||% "", ignore.case = TRUE) ||
              grepl("confirmer", a$type %||% "", ignore.case = TRUE)
    list(mois = a$mois, apromac_fcfa = as.numeric(a$prix_fcfa), type = .rs_txt1(a$type, ""),
         source = .rs_txt1(a$source, ""), date_publication = .rs_txt1(a$date_publication),
         origine = .rs_txt1(a$origine, ""), source_faible = faible,
         ref_mois = ref, tsr20_ref = if (is.finite(tsr)) tsr else NA_real_,
         taux_usd_eur = if (is.finite(t)) round(t, 4) else NA_real_, taux_secondaire = sec,
         prix_sec_usd = calc$prix_planteur_sec_usd, rsci_pct = calc$rsci_pct,
         part_officielle_pct = calc$taux$part_planteur_pct, ecart_pts = calc$ecart_pts)
  })
}

# Jeu de données public complet. j : JSON de la semaine (liste, simplifyVector = FALSE) ou NULL
rs_site_donnees <- function(dir_db = RS_DB_DIR, j = NULL, validations = rs_validation_lire(),
                            aujourdhui = Sys.Date()) {
  series <- rs_db_lire_series(dir_db)
  obs    <- rs_db_lire_obs(dir_db)
  pub    <- series[series$publiable == "oui", , drop = FALSE]
  obs    <- obs[obs$serie_id %in% pub$serie_id, , drop = FALSE]          # liste blanche

  # ── RAPI-CI (douanes UE) : historique complet de la base ; état lu dans le JSON
  rapi <- j$rapi_ci
  mois <- rs_site_rapi_mois(obs)

  # ── RSCI : historique recalculé (v2.1) à partir des prix APROMAC fusionnés
  h <- j$prix$bord_champ_ci$historique
  apromac <- if (is.list(h) && length(h) > 0) do.call(rbind, lapply(h, function(x) data.frame(
    mois = .rs_txt1(x$mois), prix_fcfa = .rs_num1(x$prix_fcfa), type = .rs_txt1(x$type, ""),
    source = .rs_txt1(x$source, ""), date_publication = .rs_txt1(x$date_publication),
    origine = .rs_txt1(x$origine, ""), stringsAsFactors = FALSE))) else NULL
  rsci_hist <- rs_site_rsci_historique(apromac, obs)

  # ── Pré-RSI et volatilité : repris tels quels s'ils sont publiés
  pr <- j$signaux_faibles$pre_rsi
  pre_rsi <- if (is.list(pr) && isTRUE(pr$publie) && isTRUE(pr$publiable)) list(
    score = .rs_num1(pr$score), signal_fr = .rs_txt1(pr$signal), signal_en = .rs_txt1(pr$signal_en),
    periode = .rs_txt1(pr$periode), version = .rs_txt1(pr$version_methode), nature = .rs_txt1(pr$nature, "E"),
    composantes = lapply(names(pr$poids_nominaux), function(k) list(
      cle = k, score = .rs_num1(pr$scores[[k]]), poids_nominal = .rs_num1(pr$poids_nominaux[[k]]),
      poids_effectif = .rs_num1(pr$poids_effectifs[[k]]), sens = .rs_txt1(pr$sens[[k]], "absente")))) else NULL
  mc <- j$monte_carlo
  vol <- if (is.list(mc) && isTRUE(mc$disponible) && rs_serie_publiable(.rs_txt1(mc$meta$depart$serie, ""))) list(
    depart = .rs_num1(mc$meta$prix_depart), depart_periode = .rs_txt1(mc$meta$depart$date),
    volatilite_pct = .rs_num1(mc$meta$volatilite_an_pct), periode = .rs_txt1(mc$periode),
    version = .rs_txt1(mc$version_methode), n_simulations = .rs_num1(mc$meta$n_simulations),
    horizons = unname(lapply(mc$resultats, function(r) list(
      sem = .rs_num1(r$horizon_sem), date = .rs_txt1(r$date_horizon), p10 = .rs_num1(r$p10),
      p25 = .rs_num1(r$p25), p50 = .rs_num1(r$p50), p75 = .rs_num1(r$p75), p90 = .rs_num1(r$p90))))) else NULL
  mz <- j$signaux_faibles$module1_meteo
  meteo <- if (is.list(mz) && length(mz$zones) > 0) list(
    source = .rs_txt1(mz$source, "Open-Meteo"), date = .rs_txt1(mz$date_source),
    zones = lapply(unname(mz$zones), function(z) list(pays = .rs_txt1(z$pays), ville = .rs_txt1(z$ville),
      pluie_30j_mm = .rs_num1(z$pluie_30j_mm), temp_c = .rs_num1(z$temp_moyenne_c),
      score_production = .rs_num1(z$score_production)))) else NULL

  # ── Actualités : titre, éditeur, date, lien, catégorie (liste blanche ; aucun texte recopié)
  ac <- j$actualites
  actualites <- if (is.list(ac) && identical(.rs_txt1(ac$statut), "ok") && length(ac$tous_articles) > 0) list(
    date = .rs_txt1(ac$date_collecte), n = .rs_num1(ac$total_articles),
    articles = lapply(utils::head(ac$tous_articles, 10), function(a) list(
      titre = .rs_txt1(a$titre, ""), source = .rs_txt1(a$source, ""), date = .rs_txt1(a$date),
      lien = if (grepl("^https://", .rs_txt1(a$lien, ""))) .rs_txt1(a$lien) else NA_character_,
      categorie_fr = .rs_txt1(a$categorie_fr, ""), categorie_en = .rs_txt1(a$categorie_en, ""),
      tension = isTRUE(a$tension)))) else NULL

  # ── Catalogue des séries publiables (fiche IOSCO de chaque série)
  # (séries sans aucune valeur exclues : le site ne compte que ce qui est publié)
  pub <- pub[pub$serie_id %in% obs$serie_id, , drop = FALSE]
  catalogue <- lapply(seq_len(nrow(pub)), function(i) {
    s <- pub[i, ]; pts <- rs_site_points(obs, s$serie_id); n <- nrow(pts)
    list(serie_id = s$serie_id, libelle_fr = s$libelle_fr, libelle_en = s$libelle_en, frequence = s$frequence,
         unite = s$unite, nature = s$nature, licence = s$licence, source = s$source, url = s$url_source,
         version = s$version_methode, n = n,
         derniere_date = if (n > 0) pts$p[n] else NA_character_, derniere_valeur = if (n > 0) pts$v[n] else NA_real_)
  })
  ref <- function(id) { p <- rs_site_points(obs, id); lapply(seq_len(nrow(p)), function(i) list(p = p$p[i], v = p$v[i])) }

  sem <- .rs_num1(j$meta$semaine); an <- .rs_num1(j$meta$annee)
  val <- if (is.finite(sem) && is.finite(an)) rs_validation_etat(validations, sem, an)
         else list(statut = "en_attente", date = NA_character_)
  list(
    meta = list(
      genere_le = as.character(aujourdhui), semaine = sem, annee = an,
      date_collecte = .rs_txt1(j$meta$date_collecte),
      validation = list(statut = val$statut, date = val$date),
      versions_methode = setNames(as.list(RS_METHODES$version), RS_METHODES$cle),
      alertes = unname(lapply(j$controle_qualite$alertes %||% list(), .rs_txt1)),
      licence_note = "Séries sous licence libre uniquement. Cotations de bourse et MRB non reproduites.",
      evaluateur = "Martial Sahiri"),
    rapi = list(statut = .rs_txt1(rapi$statut, "indisponible"), maj_eurostat = .rs_txt1(rapi$date_maj_eurostat),
                version = .rs_txt1(rapi$version_methode, rs_version_methode("rapi_ci")), mois = mois,
                origines = rs_site_origines(obs)),
    afrique = if (!is.null(af <- rs_site_afrique(obs)))
      c(af, list(maj_eurostat = .rs_txt1(j$rapi_afrique$date_maj_eurostat, .rs_txt1(rapi$date_maj_eurostat)),
                 statut = .rs_txt1(j$rapi_afrique$statut, "base"),
                 phrase_fr = .rs_txt1(j$rapi_afrique$phrase_fr, ""), phrase_en = .rs_txt1(j$rapi_afrique$phrase_en, ""))) else NULL,
    rsci =list(version = rs_version_methode("rsci"), drc = RS_DRC_REFERENCE, historique = rsci_hist),
    references = list(bm_tsr20 = ref("BM_TSR20"), fmi = ref("FMI_PRUBBUSDM")),
    production = rs_site_production(obs),
    pre_rsi = pre_rsi, volatilite = vol, meteo = meteo, actualites = actualites, catalogue = catalogue)
}

# Écrit site/data/terminal.json, terminal.js (lisible sans serveur) et le CSV public
rs_site_ecrire <- function(d, dir_out = RS_SITE_DIR, dir_db = RS_DB_DIR) {
  dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
  txt <- jsonlite::toJSON(d, auto_unbox = TRUE, na = "null", null = "null", digits = NA, pretty = TRUE)
  ecrire <- function(x, f) { con <- file(f, open = "wb"); on.exit(close(con)); writeBin(charToRaw(enc2utf8(x)), con) }
  ecrire(paste0(txt, "\n"), file.path(dir_out, "terminal.json"))
  ecrire(paste0("window.RS_DATA = ", txt, ";\n"), file.path(dir_out, "terminal.js"))
  series <- rs_db_lire_series(dir_db); obs <- rs_db_lire_obs(dir_db)
  pub <- obs[obs$serie_id %in% series$serie_id[series$publiable == "oui"], , drop = FALSE]
  con <- file(file.path(dir_out, "observations_publiques.csv"), open = "wb")
  utils::write.csv(pub, con, row.names = FALSE, na = "", fileEncoding = "UTF-8"); close(con)
  invisible(list(n_series = length(d$catalogue), n_obs = nrow(pub)))
}
