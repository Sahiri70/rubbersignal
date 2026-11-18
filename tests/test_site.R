# Tests testthat : données publiques du site et du terminal (00_site.R, script 08),
# registre de validation, note éditoriale, formats (00_regles.R section 12).
# Usage (depuis la racine) : Rscript -e "testthat::test_file('tests/test_site.R')"
# Hors ligne. Valeurs inventées pour tout ce qui touche aux séries sous licence.

library(testthat)
racine <- normalizePath(if (file.exists("scripts/00_regles.R")) "." else "..")
source(file.path(racine, "scripts/00_regles.R"))
source(file.path(racine, "scripts/00_base.R"))
source(file.path(racine, "scripts/00_site.R"))

# Base de test : séries publiables + une cotation sous licence (valeur inventée)
base_test <- function() {
  dir <- file.path(tempfile(), "db"); rs_db_init(dir)
  mois <- c("2026-06", "2026-07", "2026-08", "2026-09")
  o <- function(id, p, v) rs_db_obs(id, p, v, paste0(substr(p, 1, 7), "-01"), "2026-11-16")
  lot <- rbind(
    o("BM_TSR20", mois, c(2.25, 2.14, 2.24, 2.41)),
    o("BCE_USD_EUR_M", mois, c(1.1518, 1.1680, 1.1593, 1.1513)),
    o("COMEXT_QTE_CI", mois, c(305890, 310120, 297210, 344590)),
    o("COMEXT_VAL_CI", mois, c(58111000, 59982000, 61940846, 72038166)),
    o("COMEXT_QTE_TH", mois, c(150000, 160000, 155000, 158000)),
    o("COMEXT_VAL_TH", mois, c(31000000, 33500000, 34000000, 36000000)),
    o("RAPI_CI_CIF_EUR", mois, c(1.900, 1.934, 2.084, 2.091)),
    o("RAPI_CI_CIF_USD", mois, c(2.188, 2.208, 2.416, 2.407)),
    o("RAPI_ASIE_CIF_EUR", mois, c(2.017, 2.092, 2.256, 2.309)),
    o("RAPI_ASIE_CIF_USD", mois, c(2.324, 2.389, 2.616, 2.658)),
    o("RAPI_CI_DECOTE_ASIE", mois, c(-5.8, -7.6, -7.6, -9.4)),
    o("MRB_SMR20", "2026-11-13", 9.876))            # sous licence : ne doit jamais sortir
  rs_db_ajouter(lot, dir)
  dir
}
json_test <- function() list(
  meta = list(semaine = 47, annee = 2026, date_collecte = "2026-11-16"),
  prix = list(
    synthese = list(prix_actuel = 9.876, indice = "MRB_SMR20"),
    bord_champ_ci = list(historique = list(
      list(mois = "2026-07", prix_fcfa = 493, type = "carreau usine", source = "AIP", date_publication = "2026-07-03", origine = "saisie"),
      list(mois = "2026-09", prix_fcfa = 484, type = "carreau usine", source = "Presse", date_publication = "2026-09-02", origine = "saisie"),
      list(mois = "2026-10", prix_fcfa = 470, type = "a confirmer", source = "blog (source faible, non officielle)", date_publication = NULL, origine = "saisie")))),
  controle_qualite = list(alertes = list("APROMAC : prix de 2026-11 absent"),
                          tsr20 = list(candidats = list(mrb = list(usd = 9.876, indice = "MRB_SMR20")))),
  rapi_ci = list(statut = "ok", publiable = TRUE, date_maj_eurostat = "2026-11-13", version_methode = "RAPI-CI v1"),
  signaux_faibles = list(
    pre_rsi = list(publie = TRUE, publiable = TRUE, score = 52, signal = "Neutre — marché équilibré", signal_en = "Neutral — balanced market",
                   periode = "2026-W47", version_methode = "Pré-RSI v2", nature = "E",
                   scores = list(meteo = 50, devises = 70, demande = 40, terrain = NULL, geo = NULL),
                   poids_nominaux = list(meteo = 0.25, devises = 0.2, demande = 0.25, terrain = 0.15, geo = 0.15),
                   poids_effectifs = list(meteo = 0.357, devises = 0.286, demande = 0.357, terrain = 0, geo = 0),
                   sens = list(meteo = "neutre", devises = "hausse", demande = "baisse", terrain = NULL, geo = NULL)),
    module1_meteo = list(source = "Open-Meteo", date_source = "2026-11-15",
                         zones = list(ci = list(pays = "Côte d'Ivoire", ville = "Abidjan", pluie_30j_mm = 76.2, temp_moyenne_c = 27.5, score_production = 57)))),
  monte_carlo = list(disponible = TRUE, periode = "2021-10/2026-10", version_methode = "Volatilité v1",
                     meta = list(prix_depart = 2.55, volatilite_an_pct = 15.9, n_simulations = 10000,
                                 depart = list(serie = "BM_TSR20", date = "2026-10")),
                     resultats = list(S4 = list(horizon_sem = 4, date_horizon = "2026-12-14", p10 = 2.41, p25 = 2.47, p50 = 2.55, p75 = 2.63, p90 = 2.70)))
)

test_that("Export : séries publiables seulement, aucune valeur sous licence", {
  dir <- base_test()
  d <- rs_site_donnees(dir, json_test(), validations = rs_validation_lire(tempfile()), aujourdhui = as.Date("2026-11-16"))
  ids <- vapply(d$catalogue, function(s) s$serie_id, character(1))
  expect_false(any(c("MRB_SMR20", "SGX_TSR20", "INDEXMUNDI") %in% ids))
  expect_true(all(c("BM_TSR20", "RAPI_CI_CIF_USD") %in% ids))
  # 18/11/2026 : une série publiable sans aucune valeur n'est pas comptée (APROMAC est vide ici)
  expect_false("APROMAC" %in% ids)
  expect_true(all(vapply(d$catalogue, function(s) s$n > 0, logical(1))))
  out <- file.path(tempfile(), "site"); r <- rs_site_ecrire(d, out, dir)
  for (f in c("terminal.json", "terminal.js", "observations_publiques.csv")) {
    txt <- paste(readLines(file.path(out, f), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    expect_false(grepl("9\\.876", txt), info = f)
    expect_false(grepl("MRB_SMR20", txt), info = f)
  }
  relu <- jsonlite::fromJSON(file.path(out, "terminal.json"), simplifyVector = FALSE)
  expect_equal(relu$meta$semaine, 47)
  expect_match(readLines(file.path(out, "terminal.js"), n = 1, warn = FALSE), "^window\\.RS_DATA = ")
  # La garde licence ne trouve rien dans les fichiers du site
  x <- rs_controle_licence(fichiers = list.files(out, full.names = TRUE), valeurs = "9.876", publier_licence = FALSE)
  expect_length(x, 0)
})

test_that("Export : matrice RAPI-CI tirée de la base, volumes en tonnes, drapeaux", {
  dir <- base_test()
  d <- rs_site_donnees(dir, json_test(), validations = rs_validation_lire(tempfile()))
  m <- d$rapi$mois
  expect_equal(vapply(m, function(x) x$p, character(1)), c("2026-06", "2026-07", "2026-08", "2026-09"))
  der <- m[[4]]
  expect_equal(der$vol_t, 34459); expect_equal(der$ci_usd, 2.407); expect_equal(der$decote, -9.4)
  expect_equal(der$tsr20_bm, 2.41); expect_false(der$revise); expect_true(der$asie_complet)
  # Révision Eurostat d'une quantité : le mois est marqué révisé
  rs_db_ajouter(rs_db_obs("COMEXT_QTE_CI", "2026-08", 299000, "2026-08-01", "2026-11-23"), dir)
  m2 <- rs_site_donnees(dir, json_test(), validations = rs_validation_lire(tempfile()))$rapi$mois
  expect_true(m2[[3]]$revise); expect_equal(m2[[3]]$vol_t, 29900)
  o <- d$rapi$origines
  expect_equal(o$fin, "2026-09")
  ci <- Filter(function(p) p$code == "CI", o$pays)[[1]]
  expect_equal(ci$vol_t, round((305890 + 310120 + 297210 + 344590) / 10))
  expect_equal(ci$prix_eur_kg, round((58111000 + 59982000 + 61940846 + 72038166) / ((305890 + 310120 + 297210 + 344590) * 100), 3))
})

test_that("Export : historique RSCI v2.1 (taux BCE du mois de référence), source faible signalée", {
  dir <- base_test()
  h <- rs_site_donnees(dir, json_test(), validations = rs_validation_lire(tempfile()))$rsci$historique
  sept <- Filter(function(x) x$mois == "2026-09", h)[[1]]
  expect_equal(sept$ref_mois, "2026-08"); expect_equal(sept$tsr20_ref, 2.24); expect_equal(sept$taux_usd_eur, 1.1593)
  expect_equal(sept$rsci_pct, rs_calcul_rsci(484, 655.957 / 1.1593, 2.24, "2026-09", 0.60)$rsci_pct)
  expect_equal(sept$part_officielle_pct, 66); expect_false(sept$source_faible)
  oct <- Filter(function(x) x$mois == "2026-10", h)[[1]]
  expect_true(oct$source_faible)
  expect_equal(oct$ref_mois, "2026-09")
})

test_that("Export : Pré-RSI, volatilité et météo repris seulement s'ils sont publiables", {
  dir <- base_test(); j <- json_test()
  d <- rs_site_donnees(dir, j, validations = rs_validation_lire(tempfile()))
  expect_equal(d$pre_rsi$score, 52); expect_length(d$pre_rsi$composantes, 5)
  expect_equal(d$volatilite$horizons[[1]]$p90, 2.70)
  expect_null(names(d$meteo$zones))                 # tableau JSON, pas un objet
  j$signaux_faibles$pre_rsi$publie <- FALSE
  j$monte_carlo$meta$depart$serie <- "MRB_SMR20"    # départ sous licence : volatilité non exportée
  d2 <- rs_site_donnees(dir, j, validations = rs_validation_lire(tempfile()))
  expect_null(d2$pre_rsi); expect_null(d2$volatilite)
})

test_that("Catalogue des séries : mis à jour depuis le code, observations intactes", {
  dir <- base_test()
  f <- rs_db_fichiers(dir)
  s <- rs_db_lire_series(dir); n_obs <- nrow(rs_db_lire_obs(dir))
  expect_equal(s$version_methode[s$serie_id == "RSCI"], "RSCI v2.1")
  # Fichier modifié à la main sur un libellé : remis d'aplomb par rs_db_init
  l <- readLines(f$series, warn = FALSE, encoding = "UTF-8"); l <- sub("RSCI v2.1", "RSCI v0", l, fixed = TRUE)
  con <- file(f$series, open = "wb"); writeBin(charToRaw(enc2utf8(paste0(paste(l, collapse = "\n"), "\n"))), con); close(con)
  rs_db_init(dir)
  expect_equal(rs_db_lire_series(dir)$version_methode[s$serie_id == "RSCI"], "RSCI v2.1")
  expect_equal(nrow(rs_db_lire_obs(dir)), n_obs)
})

test_that("Registre de validation : jamais « validé » sans ligne au registre", {
  f <- tempfile(fileext = ".csv")
  writeLines("annee,semaine,date_validation,evaluateur,decision,commentaire", f)
  reg <- rs_validation_lire(f)
  e <- rs_validation_etat(reg, 47, 2026)
  expect_equal(e$statut, "en_attente")
  m <- rs_mention_validation(e, as.Date("2026-11-16"), "fr")
  expect_match(m, "en attente de validation"); expect_false(grepl("[Vv]alidé par", m))
  expect_match(rs_mention_validation(e, as.Date("2026-11-16"), "en"), "pending validation")
  writeLines(c("annee,semaine,date_validation,evaluateur,decision,commentaire",
               "2026,47,2026-11-17,Martial Sahiri,valide,",
               "2026,46,2026-11-17,Martial Sahiri,valide_avec_reserves,corrections à partir de S47"), f)
  reg <- rs_validation_lire(f)
  expect_match(rs_mention_validation(rs_validation_etat(reg, 47, 2026), as.Date("2026-11-16"), "fr"),
               "Validé par l'évaluateur responsable \\(Martial Sahiri\\) le 17/11/2026")
  expect_match(rs_mention_validation(rs_validation_etat(reg, 46, 2026), as.Date("2026-11-09"), "fr"), "Validé avec réserves")
  expect_equal(rs_validation_etat(reg, 48, 2026)$statut, "en_attente")
  expect_equal(rs_validation_etat(rs_validation_lire(tempfile()), 47, 2026)$statut, "en_attente")   # fichier absent
  writeLines(c("annee,semaine,date_validation,evaluateur,decision,commentaire", "2026,47,2026-11-17,X,approuve,"), f)
  expect_error(rs_validation_lire(f), "decision inconnue")
})

test_that("Note éditoriale : absente = aucun texte d'attente ; formats de nombres", {
  expect_true(is.na(rs_note_editoriale(47, 2026, "fr", tempfile())))
  f <- tempfile(fileext = ".csv")
  writeLines(c("annee,semaine,date,texte_fr,texte_en", "2026,47,2026-11-16,Récolte normale à Daloa.,Normal harvest in Daloa."), f)
  expect_equal(rs_note_editoriale(47, 2026, "fr", f), "Récolte normale à Daloa.")
  expect_equal(rs_note_editoriale(47, 2026, "en", f), "Normal harvest in Daloa.")
  expect_true(is.na(rs_note_editoriale(48, 2026, "fr", f)))
  expect_equal(rs_fmt_milliers(27768, "fr"), "27\u00a0768")
  expect_equal(rs_fmt_milliers(27768, "en"), "27,768")
  expect_equal(rs_fmt_milliers(NA), "—"); expect_equal(rs_fmt_milliers(950, "fr"), "950")
})

test_that("Fichiers du dépôt : registres présents, prix APROMAC de septembre daté à sa source", {
  expect_equal(rs_validation_etat(rs_validation_lire(file.path(racine, "data/reference/validations.csv")), 1, 2000)$statut, "en_attente")
  ap <- utils::read.csv(file.path(racine, "data/reference/prix_apromac.csv"), stringsAsFactors = FALSE)
  s <- ap[ap$mois == "2026-09", ]
  expect_equal(s$prix_fcfa_kg, 484); expect_equal(s$date_publication, "2026-09-02")
  w <- readLines(file.path(racine, ".github/workflows/rubbersignal_pipeline.yml"))
  i03 <- grep("scripts/03_build_report.R", w); i08 <- grep("scripts/08_export_site.R", w)
  i07 <- grep("scripts/07_garde_licence.R", w); ic <- grep("Commit and push", w)
  expect_true(i03 < i08 && i08 < i07 && i07 < ic)             # export du site avant la garde, garde avant le commit
  expect_true(any(grepl("git add output/ data/ site/data/", w, fixed = TRUE)))
  expect_false(any(grepl("uses: .*@v[0-9]", w)))               # actions épinglées par empreinte
  expect_true(any(grepl("^permissions:", w)))
  g <- readLines(file.path(racine, "scripts/07_garde_licence.R"))
  expect_true(any(grepl('"site"', g, fixed = TRUE)))
})

test_that("Pages du site : politique de contenu stricte, aucune ressource externe, aucune clé", {
  pages <- file.path(racine, "site", c("index.html", "terminal.html", "methode.html"))
  for (p in pages) {
    expect_true(file.exists(p), info = p)
    h <- paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    expect_match(h, "Content-Security-Policy", info = p)
    expect_match(h, "script-src 'self'", info = p)
    expect_false(grepl("<script(?![^>]*src=)", h, perl = TRUE), info = p)     # pas de script en ligne
    expect_false(grepl("\\sstyle=\"", h), info = p)                           # pas de style en ligne
    expect_false(grepl("(src|href)=\"https?://", h), info = p)                # aucune ressource externe
  }
  js <- list.files(file.path(racine, "site/assets"), "\\.js$", full.names = TRUE)
  for (f in js) {
    t <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    expect_false(grepl("innerHTML|document\\.write|eval\\(", t), info = f)    # données insérées en texte seulement
    expect_false(grepl("fetch\\(|XMLHttpRequest|api[_-]?key", t, ignore.case = TRUE), info = f)
  }
})
