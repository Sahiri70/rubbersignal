# Tests testthat : Production FAOSTAT v1, carte et vues satellite (00_regles.R
# section 11 ter, 00_base.R, 00_site.R, script 09, outils/carte et vignettes).
# Usage (depuis la racine) : Rscript -e "testthat::test_file('tests/test_production.R')"
# Hors ligne. Fixture : tests/fixtures/production/faostat_extrait_afrique.csv,
# extrait RÉEL du fichier FAOSTAT « Afrique » (publication du 31/12/2025) :
# caoutchouc naturel (836), Côte d'Ivoire, Ghana, Guinée.

library(testthat)
racine <- normalizePath(if (file.exists("scripts/00_regles.R")) "." else "..")
source(file.path(racine, "scripts/00_regles.R"))
source(file.path(racine, "scripts/00_base.R"))
source(file.path(racine, "scripts/00_site.R"))
FX <- file.path(racine, "tests/fixtures/production/faostat_extrait_afrique.csv")

test_that("nature d'après le drapeau FAO", {
  expect_equal(rs_fao_nature(c("A", "X", "", NA, "P", "E", "I", "T", "M")),
               c("O", "O", "O", "O", "O", "E", "E", "E", NA))
})

test_that("lecture FAOSTAT réelle : format long, 2024 connu, drapeaux conservés", {
  d <- rs_fao_lire(FX)
  expect_setequal(unique(d$code), c("CI", "GH", "GN"))
  expect_setequal(unique(d$element), c("PROD", "SURF", "REND"))
  expect_true(all(as.integer(d$annee) >= 2000))
  ci <- d[d$code == "CI" & d$annee == "2024", ]
  expect_equal(ci$valeur[ci$element == "PROD"], 1686599)
  expect_equal(ci$valeur[ci$element == "SURF"], 1027211)
  expect_equal(ci$flag[ci$element == "PROD"], "A")
  expect_equal(ci$flag[ci$element == "SURF"], "I")             # surface imputée par la FAO -> nature E
  expect_equal(d$valeur[d$code == "GH" & d$annee == "2024" & d$element == "PROD"], 136500)
  expect_false(any(duplicated(d[, c("code", "element", "annee")])))
})

test_that("lecture FAOSTAT : autre produit et valeur manquante écartés ; fichier inattendu refusé", {
  x <- utils::read.csv(FX, check.names = FALSE, stringsAsFactors = FALSE, encoding = "UTF-8")
  x$`Item Code`[1] <- 15                                            # une ligne « blé »
  x$Y2024F[x$`Area Code` == 81 & x$`Element Code` == 5510] <- "M"   # production Ghana 2024 manquante
  f <- tempfile(fileext = ".csv"); utils::write.csv(x, f, row.names = FALSE, fileEncoding = "UTF-8")
  d <- rs_fao_lire(f)
  expect_equal(nrow(d[d$code == "GH" & d$annee == "2024" & d$element == "PROD", ]), 0)
  expect_lt(nrow(d), nrow(rs_fao_lire(FX)))
  g <- tempfile(fileext = ".csv"); writeLines("a,b\n1,2", g)
  expect_error(rs_fao_lire(g), "colonnes inattendues")
})

test_that("catalogue : 72 séries FAOSTAT publiables, version déclarée", {
  x <- RS_DB_CATALOGUE[RS_DB_CATALOGUE$version_methode == "Production FAOSTAT v1", ]
  expect_equal(nrow(x), 72)
  expect_true(all(x$registre == "FAOSTAT"))
  expect_true(rs_serie_publiable("FAOSTAT"))
  expect_match(rs_serie_info("FAOSTAT")$licence, "CC BY 4.0")
  expect_true(all(c("FAO_PROD_CI", "FAO_SURF_LR", "FAO_REND_WLD") %in% x$serie_id))
  expect_equal(rs_version_methode("faostat"), "Production FAOSTAT v1")
})

base_prod <- function() {
  dir <- file.path(tempfile(), "db"); rs_db_init(dir)
  d <- rs_fao_lire(FX)
  tot <- data.frame(code = c("WLD", "AFR"), element = "PROD", annee = "2024", valeur = c(14829085, 2152142), flag = "A")
  d <- rbind(d, tot)
  rs_db_ajouter(rs_db_obs(rs_fao_id(d$element, d$code), d$annee, d$valeur, "2025-12-31", "2026-11-18",
                          methode = "test", nature = rs_fao_nature(d$flag)), dir)
  dir
}

test_that("base : séries annuelles acceptées, dates invalides toujours refusées", {
  dir <- base_prod()
  o <- rs_db_lire_obs(dir)
  expect_true(all(grepl("^[0-9]{4}$", o$date_valeur[startsWith(o$serie_id, "FAO_")])))
  expect_equal(o$nature[o$serie_id == "FAO_SURF_CI" & o$date_valeur == "2024"], "E")
  expect_error(rs_db_ajouter(rs_db_obs("FAO_PROD_CI", "08/2026", 1, "2026-01-01", "2026-11-18"), dir), "date_valeur invalide")
  r <- rs_db_ajouter(rs_db_obs("FAO_PROD_CI", "2024", 1686599, "2025-12-31", "2026-11-25", nature = "O"), dir)
  expect_equal(r$n_ajoutees, 0)                                    # idempotent
})

test_that("site : bloc production (parts, nature, zones satellite)", {
  p <- rs_site_production(rs_db_lire_obs(base_prod()), file.path(racine, "data/reference/zones_satellite.csv"))
  expect_equal(p$annee, "2024")
  ci <- Filter(function(x) x$code == "CI", p$pays)[[1]]
  expect_equal(ci$prod_t, 1686599)
  expect_equal(ci$part_monde_pct, round(100 * 1686599 / 14829085, 2))
  expect_equal(ci$part_afrique_pct, round(100 * 1686599 / 2152142, 1))
  expect_equal(ci$nature, "O")
  expect_true(length(ci$historique) >= 10)
  expect_equal(length(p$zones), 4)
  expect_match(p$satellite_credit, "EOxCloudless 2016.*CC BY 4.0")
  expect_null(rs_site_production(rs_db_lire_obs({ d <- file.path(tempfile(), "db"); rs_db_init(d); d })))
})

test_that("vues satellite : fichiers présents, couche sous licence libre, sources https", {
  z <- utils::read.csv(file.path(racine, "data/reference/zones_satellite.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
  expect_setequal(z$id, c("harbel", "toupah", "niete", "soubre"))
  for (id in z$id) {
    f <- file.path(racine, "site/assets/sat", paste0(id, ".jpg"))
    expect_true(file.exists(f), info = id)
    expect_lt(file.size(f), 300 * 1024)
  }
  expect_true(all(grepl("^https://", z$sources)))
  expect_true(all(nzchar(z$legende_fr) & nzchar(z$legende_en)))
  outil <- paste(readLines(file.path(racine, "scripts/outils/vignettes_satellite.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(outil, 'EOX_COUCHE  <- "s2cloudless_3857"', fixed = TRUE)    # 2016, CC BY 4.0
  expect_false(grepl('EOX_COUCHE  <- "s2cloudless-20(1[89]|2[0-9])', outil))  # années non commerciales interdites
})

test_that("carte : 24 pays, producteurs présents, points satellite dans le cadre ; site sans ressource externe", {
  txt <- paste(readLines(file.path(racine, "site/data/carte_afrique.js"), warn = FALSE, encoding = "UTF-8"), collapse = "")
  k <- jsonlite::fromJSON(sub(";\\s*$", "", sub("^window.RS_CARTE = ", "", txt)), simplifyVector = FALSE)
  iso <- vapply(k$pays, function(x) x$iso2, character(1))
  expect_equal(length(iso), 24)
  expect_true(all(c("CI", "GH", "LR", "NG", "CM", "GA", "GN", "GW") %in% iso))
  z <- utils::read.csv(file.path(racine, "data/reference/zones_satellite.csv"), stringsAsFactors = FALSE)
  expect_true(all(z$lon > k$bbox$xmin & z$lon < k$bbox$xmax & z$lat > k$bbox$ymin & z$lat < k$bbox$ymax))
  h <- paste(readLines(file.path(racine, "site/terminal.html"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(h, '<script src="data/carte_afrique.js"></script>', fixed = TRUE)
  expect_match(h, "img-src 'self' data:;", fixed = TRUE)                   # images servies par le site seulement
  expect_false(grepl("tiles\\.maps\\.eox\\.at|https://[^\"]*\\.jpg", h))
})

test_that("indicateur EUDR : données cohérentes, sources sous licence libre, aucune entreprise nommée", {
  txt <- paste(readLines(file.path(racine, "site/data/eudr_bassins.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  e <- jsonlite::fromJSON(sub(";[[:space:]]*$", "", sub("^window.RS_EUDR = ", "", txt)), simplifyVector = FALSE)
  expect_equal(e$version, "EUDR bassins v1.1")
  expect_equal(e$date_reference_eudr, "2020-12-31")
  expect_match(e$sources$foret, "JRC.*version 4.*10.2905/JRC.3KATEH8.*CC BY 4.0")
  expect_match(e$sources$pertes, "Hansen.*v1.12.*CC BY 4.0")
  expect_match(e$sources$hevea, "Forest Data Partnership.*2026a.*Produced by Google for the Forest Data Partnership.*CC BY 4.0")
  expect_false(grepl("Bearer|ya29\\.|x-goog-user-project", txt))                               # aucun jeton d'accès publié
  ids <- vapply(e$bassins, function(b) b$id, character(1))
  expect_setequal(ids, c("harbel", "toupah", "niete", "soubre"))
  for (b in e$bassins) {
    expect_true(b$foret_2020_ha <= b$surface_ha, info = b$id)
    expect_true(b$perte_apres_2020_foret_ha <= b$foret_2020_ha, info = b$id)
    expect_equal(sum(unlist(b$perte_foret_par_an)), b$perte_apres_2020_foret_ha, tolerance = 1, info = b$id)
    expect_equal(sum(unlist(b$perte_par_an)[as.character(2021:2024)]),
                 b$perte_apres_2020_foret_ha + b$perte_apres_2020_hors_foret_ha, tolerance = 1, info = b$id)
    expect_equal(length(b$perte_par_an), 24)
    expect_equal(b$hevea_seuil, 0.5, info = b$id)
    expect_true(b$hevea_2024_ha <= b$surface_ha && b$hevea_2020_ha <= b$surface_ha, info = b$id)
    expect_true(b$hevea_nouveau_sur_foret_2020_ha <= min(b$hevea_nouveau_ha, b$foret_2020_ha), info = b$id)
    expect_true(b$hevea_nouveau_ha <= b$hevea_2024_ha, info = b$id)
    expect_true(file.exists(file.path(racine, "site/assets/sat", paste0(b$id, "_eudr.jpg"))), info = b$id)
  }
  expect_false(grepl("Firestone|SAPH|HEVECAM|SIFCA|Bridgestone", txt, ignore.case = TRUE))   # données sans nom d'entreprise
  js <- paste(readLines(file.path(racine, "site/assets/rs-terminal.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(js, "t('Zone de ', 'Area around ') + lieu", fixed = TRUE)                     # titre géographique en mode EUDR
  h <- paste(readLines(file.path(racine, "site/terminal.html"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(h, "ne désigne aucun acteur", fixed = TRUE)
  expect_match(h, '<script src="data/eudr_bassins.js"></script>', fixed = TRUE)
  outil <- paste(readLines(file.path(racine, "scripts/outils/eudr_bassins.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(outil, "GFC2020/LATEST/single-cog/JRC_GFC2020_V4_COG.tif", fixed = TRUE)
  expect_match(outil, "GFC-2024-v1.12/Hansen_GFC-2024-v1.12_lossyear", fixed = TRUE)
  expect_match(outil, "hl >= 21 & hl <= 24", fixed = TRUE)                                    # 2021 à 2024
  expect_match(outil, "forestdatapartnership/2026a/rubber", fixed = TRUE)
  expect_match(outil, "FDP_SEUIL <- 0.5", fixed = TRUE)
  expect_match(outil, 'on.exit(setGDALconfig("GDAL_HTTP_HEADERS", ""), add = TRUE)', fixed = TRUE) # en-tête retiré après lecture
  expect_false(grepl("print-access-token[^\n]*(cat|print|writeLines|message)\\(", outil))       # jeton jamais affiché
  expect_match(js, "eudr-hevea", fixed = TRUE)
  m <- paste(readLines(file.path(racine, "site/methode.html"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(m, "EUDR bassins v1.1", fixed = TRUE)
  expect_match(m, "produite par Google pour le Forest Data Partnership", fixed = TRUE)
})
