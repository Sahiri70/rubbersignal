# Tests testthat : actualités RSS (00_actualites.R, script 02 v4), composante
# géopolitique du Pré-RSI v2.1 (00_regles.R), bloc actualités du site (00_site.R).
# Usage (depuis la racine) : Rscript -e "testthat::test_file('tests/test_actualites.R')"
# Hors ligne. Fixtures : flux RÉELS du 18/11/2026 réduits au titre, au lien, à
# la date et à l'éditeur (aucun texte d'article) :
#   tests/fixtures/actualites/gn_fr_20261118.xml : Google News, requête française
#   tests/fixtures/actualites/ba_20261118.xml    : Business & Actuality TV, recherche « hévéa »

library(testthat)
racine <- normalizePath(if (file.exists("scripts/00_regles.R")) "." else "..")
source(file.path(racine, "scripts/00_regles.R"))
source(file.path(racine, "scripts/00_base.R"))
source(file.path(racine, "scripts/00_site.R"))
source(file.path(racine, "scripts/00_actualites.R"))

FX <- file.path(racine, "tests/fixtures/actualites")
lire <- function(f) paste(readLines(file.path(FX, f), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
J <- as.Date("2026-11-18")

test_that("lecture RSS Google News : éditeur retiré du titre, dates lues", {
  a <- rs_actu_lire_rss(lire("gn_fr_20261118.xml"), origine = "gn_fr_ci", nom_source = "Google News")
  expect_equal(nrow(a), 28)
  expect_false(any(is.na(a$date)))
  i <- grep("^Caoutchouc naturel : un march", a$titre)
  expect_length(i, 1)
  expect_equal(a$source[i], "AGENCE ECOFIN")
  expect_false(grepl(" - AGENCE ECOFIN$", a$titre[i]))
  expect_true(all(grepl("^https://", a$lien)))
})

test_that("lecture RSS d'éditeur : nom de source par défaut ; texte invalide -> vide", {
  a <- rs_actu_lire_rss(lire("ba_20261118.xml"), origine = "ba_hevea", nom_source = "Business & Actuality TV")
  expect_equal(nrow(a), 10)
  expect_true(all(a$source == "Business & Actuality TV"))
  expect_equal(nrow(rs_actu_lire_rss("<html>pas un flux</html>")), 0)
  expect_equal(nrow(rs_actu_lire_rss("<rss><channel></channel></rss>")), 0)
})

test_that("filtre : caoutchouc naturel gardé, bruit écarté (FR et EN)", {
  garder <- c("Caoutchouc naturel : un marché mondial sous tension, des prix attendus en hausse en 2027",
              "SIKA COMMODITIES WEEKLY 45 : Des marchés contrastés, entre repli du pétrole et rebond du caoutchouc",
              "CAOUTCHOUC — Les contrats japonais évoluent dans une fourchette étroite",
              "La filière hévéa, un géant africain en quête de valeur ajoutée.",
              "Indonesia revises natural rubber export rules, clarifies China shipment requirements",
              "Rubber Planter Commends Lawmakers for Intervention in Executive Order 166 Controversy",
              "Rubber Prices Slip As Oil Falls And Tyre Demand Softens",
              "Caoutchouc naturel: Le prix Apromac fixé à 578 frs CFA/kg en novembre.")
  ecarter <- c("Armés et prêts à cancaner : la Bundeswehr lance une boutique en ligne de canards en caoutchouc armés",
               "\"Si 2000 litres d’eau par jour coulent dans ton jardin, tu vis avec des bottes en caoutchouc !\"",
               "Michelin invente la pale d’hydrolienne en caoutchouc",
               "Controversial rubber duck shuts down Bundeswehr’s launch day",
               "Five Reputable Rubber Slippers Suppliers in 2026",
               "Barium Titanate Preferentially Localizes in Epoxidized Rubber Blends with Tunable Properties",
               "Natural Rubber Latex Market Growth: How Rising Global Passenger Vehicle Production Drives Demand",
               "Fouilles en vue de retrouver les restes de soldats tombés au combat dans une forêt d'hévéas",
               "What are Nigeria’s major exports?")
  expect_true(all(rs_actu_pertinent(garder)))
  expect_false(any(rs_actu_pertinent(ecarter)))
  # éditeurs écartés (encyclopédie, communiqués payants), même avec un titre pertinent
  expect_equal(rs_actu_pertinent(rep("How did La Condamine contribute to the study of natural rubber?", 2),
                                 c("Britannica", "Reuters")), c(FALSE, TRUE))
})

test_that("catégories et tensions", {
  t <- c("Caoutchouc naturel: Le prix Apromac fixé à 578 frs CFA/kg en novembre.",
         "Rubber Planter Commends Lawmakers in Liberia", "Indonesia revises natural rubber export rules",
         "EUDR: rubber traceability rules confirmed", "Michelin raises natural rubber purchases", "Natural rubber prices rise")
  expect_equal(rs_actu_categorie(t), c("ci", "afrique", "asie", "eudr", "industrie", "marche"))
  expect_equal(rs_actu_categorie_libelle("ci", "fr"), "Côte d'Ivoire")
  expect_equal(rs_actu_categorie_libelle("marche", "en"), "World market")
  expect_equal(rs_actu_tension(c("Thailand drought hits natural rubber output", "Grève des planteurs d'hévéa",
                                 "Natural rubber prices rise")), c(TRUE, TRUE, FALSE))
})

test_that("assemblage : fenêtre de 7 jours, doublons retirés, tri par score", {
  a <- rs_actu_lire_rss(lire("gn_fr_20261118.xml"), "gn_fr_ci", "Google News")
  b <- rs_actu_lire_rss(lire("ba_20261118.xml"), "ba_hevea", "Business & Actuality TV")
  dbl <- a[grep("^Caoutchouc naturel : un march", a$titre), ]; dbl$origine <- "doublon"; dbl$titre <- toupper(dbl$titre)
  r <- rs_actu_assembler(list(a, b, dbl), J)
  expect_equal(r$n_bruts, 28 + 10 + 1)
  x <- r$articles
  expect_true(all(as.Date(x$date) > J - 7))
  expect_true(all(rs_actu_pertinent(x$titre)))
  expect_false(any(duplicated(rs_actu_cle(x$titre))))
  expect_equal(sum(grepl("^caoutchouc naturel : un march", tolower(x$titre))), 1)
  expect_true(all(diff(x$score) <= 0))
  expect_true(all(c("categorie", "tension", "score") %in% names(x)))
  expect_null(rs_actu_assembler(list(NULL), J)$articles)
})

test_that("annonce APROMAC : détectée dans un titre réel, comparée à la saisie et à l'historique", {
  b <- rs_actu_lire_rss(lire("ba_20261118.xml"), "ba_hevea", "Business & Actuality TV")
  p <- rs_actu_prix_apromac(b)
  expect_true("2026-11" %in% p$mois)
  expect_equal(p$prix_fcfa[p$mois == "2026-11"], 578)
  expect_true("2026-09" %in% p$mois)                      # « 484 FCFA/kg en septembre 2026 »
  expect_equal(p$prix_fcfa[p$mois == "2026-09"], 484)
  saisie <- data.frame(mois = c("2026-09"), prix_fcfa_kg = c("484"), stringsAsFactors = FALSE)
  c1 <- rs_actu_comparer_apromac(p, saisie)
  expect_equal(c1$statut[c1$mois == "2026-11"], "a_saisir")
  expect_equal(c1$statut[c1$mois == "2026-09"], "conforme")
  c2 <- rs_actu_comparer_apromac(p, saisie, data.frame(mois = "2026-11", prix_fcfa = 578))
  expect_equal(c2$statut[c2$mois == "2026-11"], "releve_presse")
  c3 <- rs_actu_comparer_apromac(p, data.frame(mois = "2026-11", prix_fcfa_kg = "570"))
  expect_equal(c3$statut[c3$mois == "2026-11"], "ecart")
  # aucune saisie automatique : la fonction ne touche à aucun fichier
  expect_false(any(grepl("write|cat\\(", deparse(rs_actu_comparer_apromac))))
})

test_that("annonce APROMAC : bornes et formes", {
  mk <- function(t, d = "2026-12-28") data.frame(titre = t, source = "x", lien = "https://x", date = as.POSIXct(d, tz = "UTC"))
  expect_equal(rs_actu_prix_apromac(mk("Prix APROMAC : 600 FCFA/kg en janvier"))$mois, "2027-01")   # annoncé fin décembre
  expect_equal(nrow(rs_actu_prix_apromac(mk("APROMAC : 25 000 FCFA de subvention"))), 0)
  expect_equal(nrow(rs_actu_prix_apromac(mk("Le prix du cacao à 1800 FCFA en octobre"))), 0)
  expect_equal(nrow(rs_actu_prix_apromac(NULL)), 0)
})

test_that("Pré-RSI v2.1 : géopolitique tirée des actualités", {
  expect_equal(rs_version_methode("pre_rsi"), "Pré-RSI v2.1")
  g0 <- rs_geo_depuis_actualites(NULL, J)
  expect_true(is.na(g0$nb_articles)); expect_equal(g0$statut, "actualites_indisponibles")
  g1 <- rs_geo_depuis_actualites(list(statut = "ok", tous_articles = list(list(titre = "a", tension = FALSE))), J)
  expect_true(is.na(g1$nb_articles)); expect_equal(g1$statut, "aucun_article_tension")
  arts <- lapply(1:3, function(i) list(titre = paste("t", i), source = "s", lien = "https://l", date = "2026-11-17", tension = TRUE))
  g3 <- rs_geo_depuis_actualites(list(statut = "ok", tous_articles = arts), J)
  expect_equal(g3$nb_articles, 3); expect_equal(g3$statut, "ok")
  expect_equal(rs_pre_rsi_geo(g3$nb_articles), 60)        # règle de score inchangée
  expect_true(is.na(rs_pre_rsi_geo(g1$nb_articles)))
})

test_that("sources : fichier de référence valide, aucune clé, aucun NewsAPI", {
  s <- rs_actu_sources(file.path(racine, RS_ACTU_SOURCES))
  expect_gte(nrow(s), 4)
  expect_false(any(duplicated(s$id)))
  expect_true(all(grepl("^https://", s$url)))
  expect_false(any(grepl("apikey|api_key|newsapi", s$url, ignore.case = TRUE)))
  wf <- paste(readLines(file.path(racine, ".github/workflows/rubbersignal_pipeline.yml"), warn = FALSE), collapse = "\n")
  expect_false(grepl("NEWSAPI_KEY", wf))
})

test_that("site : actualités en liste blanche (titre, éditeur, date, lien https)", {
  dir <- file.path(tempfile(), "db"); rs_db_init(dir)
  j <- list(meta = list(semaine = 47, annee = 2026),
            actualites = list(statut = "ok", date_collecte = "2026-11-18", total_articles = 2,
              tous_articles = list(
                list(titre = "Natural rubber prices rise", source = "S", date = "2026-11-17", lien = "https://a",
                     categorie_fr = "Marché mondial", categorie_en = "World market", tension = FALSE, resume = "texte"),
                list(titre = "Hévéa : grève", source = "T", date = "2026-11-16", lien = "javascript:alert(1)",
                     categorie_fr = "Côte d'Ivoire", categorie_en = "Côte d'Ivoire", tension = TRUE))))
  d <- rs_site_donnees(dir, j, validations = rs_validation_lire(tempfile()))
  expect_equal(length(d$actualites$articles), 2)
  a1 <- d$actualites$articles[[1]]
  expect_setequal(names(a1), c("titre", "source", "date", "lien", "categorie_fr", "categorie_en", "tension"))
  expect_true(is.na(d$actualites$articles[[2]]$lien))     # lien non https refusé
  expect_null(rs_site_donnees(dir, list(meta = list(semaine = 47, annee = 2026),
                                        actualites = list(statut = "indisponible")),
                              validations = rs_validation_lire(tempfile()))$actualites)
})
