# Tests testthat : RAPI-Afrique v1 (00_regles.R section 11 bis, 00_base.R,
# 00_site.R, script 06b, rapport 03).
# Usage (depuis la racine) : Rscript -e "testthat::test_file('tests/test_afrique.R')"
# Hors ligne. Fixture :
#   tests/fixtures/afrique/comext_afrique_reel_20261118.json : réponse RÉELLE de
#   l'API Eurostat Comext du 18/11/2026 (CI, LR, GH, NG, CM ; NC 40012200,
#   40012900, 40011000 ; mise à jour Eurostat du 13/11/2026).

library(testthat)
racine <- normalizePath(if (file.exists("scripts/00_regles.R")) "." else "..")
source(file.path(racine, "scripts/00_regles.R"))
source(file.path(racine, "scripts/00_sources.R"))   # construction de la requête (aucun appel réseau)
source(file.path(racine, "scripts/00_base.R"))
source(file.path(racine, "scripts/00_site.R"))

F_REEL <- file.path(racine, "tests/fixtures/afrique/comext_afrique_reel_20261118.json")
lire <- function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
taux_test <- function(mois) data.frame(periode = mois, taux_usd_eur = 1.15,
                                       source = "BCE, moyenne mensuelle (test)", stringsAsFactors = FALSE)

test_that("requête Comext : plusieurs produits ; requête RAPI-CI inchangée", {
  q <- rs_comext_requete(RS_AFR_PARTENAIRES, 24, RS_AFR_PRODUITS$code)
  expect_equal(unlist(q[names(q) == "product"], use.names = FALSE), c("40012200", "40012900", "40011000"))
  expect_equal(unlist(q[names(q) == "partner"], use.names = FALSE), c("CI", "LR", "GH", "NG", "CM"))
  q0 <- rs_comext_requete()
  expect_equal(unlist(q0[names(q0) == "product"], use.names = FALSE), "40012200")
  expect_match(rs_comext_url(q0), "product=40012200&flow=1", fixed = TRUE)
})

test_that("identifiants : le TSNR garde les séries RAPI-CI, les autres produits ont un suffixe", {
  expect_equal(rs_afr_id_brut("VAL", "CI"), "COMEXT_VAL_CI")
  expect_equal(rs_afr_id_brut("QTE", "LR", "40012200"), "COMEXT_QTE_LR")
  expect_equal(rs_afr_id_brut("QTE", "CM", "40011000"), "COMEXT_QTE_CM_40011000")
  expect_error(rs_afr_id_brut("PRIX", "CI"))
})

test_that("réponse réelle : les produits ne sont jamais mélangés", {
  l <- rs_comext_lire(lire(F_REEL))
  expect_true("product" %in% names(l))
  w <- rs_comext_large_produits(l)
  expect_equal(attr(w, "updated"), "2026-11-13")
  expect_setequal(unique(w$produit), RS_AFR_PRODUITS$code)
  # une seule ligne par (produit, mois, origine)
  expect_false(any(duplicated(w[, c("produit", "periode", "partenaire")])))
  # TSNR : chaque origine a une valeur chaque mois publié
  t <- w[w$produit == "40012200" & is.finite(w$valeur_eur), ]
  expect_equal(as.integer(table(t$partenaire)[RS_AFR_PARTENAIRES]), rep(20L, 5))
  # CI septembre 2026 : même valeur que la série RAPI-CI (une seule source)
  ci <- t[t$partenaire == "CI" & t$periode == "2026-09", ]
  expect_equal(round(rs_prix_cif_eur_kg(ci$valeur_eur, ci$quantite_100kg), 3), 2.091)
  # le latex du Cameroun n'est pas confondu avec son TSNR
  cm9 <- w[w$partenaire == "CM" & w$periode == "2026-09", ]
  expect_equal(nrow(cm9), 3)
  expect_false(isTRUE(all.equal(cm9$quantite_100kg[cm9$produit == "40011000"], cm9$quantite_100kg[cm9$produit == "40012200"])))
})

test_that("réponse sans dimension produit : TSNR par défaut", {
  long <- data.frame(time = c("2026-08", "2026-08"), partner = "CI", indicators = c("VALUE_IN_EUROS", "QUANTITY_IN_100KG"),
                     valeur = c(2e6, 1e4), stringsAsFactors = FALSE)
  w <- rs_comext_large_produits(long)
  expect_equal(w$produit, "40012200"); expect_equal(w$valeur_eur, 2e6)
})

test_that("calcul sur la réponse réelle : septembre 2026", {
  w <- rs_comext_large_produits(rs_comext_lire(lire(F_REEL)))
  tsnr <- w[w$produit == RS_RAPI_PRODUIT, ]
  d <- rs_afrique_calcul(tsnr, taux_test(unique(tsnr$periode)))
  expect_setequal(unique(d$partenaire), c("LR", "GH", "NG", "CM"))
  s <- d[d$periode == "2026-09", ]; s <- s[order(s$partenaire), ]
  expect_equal(round(s$eur_kg, 3), c(2.035, 2.084, 2.105, 2.083))          # CM, GH, LR, NG
  expect_equal(round(s$ecart_ci_pct, 1), c(-2.7, -0.3, 0.7, -0.4))
  expect_equal(round(s$vol_t), c(1189, 2021, 1774, 665))
  expect_equal(s$usd_kg, s$eur_kg * 1.15)
  expect_false(any(s$faible_liquidite))
})

test_that("calcul : liquidité, variation, mois non consécutifs, taux absent, CI absente", {
  l <- data.frame(periode = c("2026-05", "2026-06", "2026-08", "2026-05", "2026-06", "2026-08"),
                  partenaire = c("CI", "CI", "CI", "GH", "GH", "GH"),
                  valeur_eur = c(2e8, 2e8, NA, 1e6, 1.4e6, 2e6),
                  quantite_100kg = c(1e6, 1e6, NA, 5000, 4000, 4000), stringsAsFactors = FALSE)
  d <- rs_afrique_calcul(l, taux_test(c("2026-05", "2026-06")), origines = "GH")
  expect_equal(nrow(d), 3)
  expect_equal(d$vol_t, c(500, 400, 400))
  expect_equal(d$faible_liquidite, c(FALSE, TRUE, TRUE))                  # seuil 500 t : strictement en dessous
  expect_equal(d$eur_kg, c(2, 3.5, 5))
  expect_equal(round(d$variation_pct, 1), c(NA, 75, NA))                  # 08 n'est pas consécutif à 06
  expect_equal(d$alerte_variation, c(FALSE, TRUE, FALSE))
  expect_equal(d$ecart_ci_pct, c(0, 75, NA))                              # pas de prix CI en août -> NA
  expect_true(is.na(d$usd_kg[3]))                                         # pas de taux en août -> USD NA
  expect_equal(nrow(rs_afrique_calcul(NULL)), 0)
  expect_equal(nrow(rs_afrique_calcul(l, origines = "NG")), 0)
})

test_that("phrase de lecture : bornes et formulation sans jugement", {
  d <- data.frame(periode = "2026-09", partenaire = c("LR", "CM", "GH"), ecart_ci_pct = c(0.7, -2.7, -0.3), stringsAsFactors = FALSE)
  p <- rs_afrique_phrase(d, "fr")
  expect_match(p, "-2.7 % (Cameroun) à +0.7 % (Liberia)", fixed = TRUE)
  expect_match(rs_afrique_phrase(d, "en"), "from -2.7% (Cameroon) to +0.7% (Liberia)", fixed = TRUE)
  expect_false(grepl("manque|perte|sous-pay", p))
  expect_match(rs_afrique_phrase(d[0, ], "fr"), "Aucune donnée")
})

test_that("catalogue et registre : 40 séries nouvelles, publiables, version déclarée", {
  x <- RS_DB_CATALOGUE[RS_DB_CATALOGUE$version_methode == "RAPI-Afrique v1", ]
  expect_equal(nrow(x), 40)
  expect_false(any(duplicated(RS_DB_CATALOGUE$serie_id)))
  expect_true(all(x$registre %in% c("EUROSTAT_COMEXT", "RAPI_AFRIQUE")))
  expect_true(all(vapply(x$registre, rs_serie_publiable, logical(1))))
  expect_equal(rs_version_methode("rapi_afrique"), "RAPI-Afrique v1")
  expect_equal(RS_METHODES$date_effet[RS_METHODES$cle == "rapi_afrique"], "2026-11-18")
  for (p in c("LR", "GH", "NG", "CM")) for (s in c("_CIF_EUR", "_CIF_USD", "_ECART_CI"))
    expect_true(paste0("RAPI_", p, s) %in% x$serie_id)
  expect_equal(x$nature[x$serie_id == "RAPI_GH_ECART_CI"], "E")
  expect_equal(x$nature[x$serie_id == "RAPI_GH_CIF_EUR"], "T")
  # aucune série RAPI-Afrique ne double la CI (TSNR CI = séries RAPI-CI)
  expect_false("COMEXT_VAL_CI_40012200" %in% RS_DB_CATALOGUE$serie_id)
})

test_that("méthode publiée : RAPI-Afrique v1 au journal du site", {
  h <- paste(readLines(file.path(racine, "site/methode.html"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(h, "RAPI-Afrique v1</td><td class=\"l\">2026-11-18", fixed = TRUE)
  expect_match(h, "500 t", fixed = TRUE)
})

# Base de test minimale : CI + Ghana sur deux mois, avec un latex camerounais
base_afrique <- function(avec_afrique = TRUE) {
  dir <- file.path(tempfile(), "db"); rs_db_init(dir)
  m <- c("2026-08", "2026-09")
  o <- function(id, p, v) rs_db_obs(id, p, v, "2026-11-13", "2026-11-18")
  lot <- rbind(
    o("COMEXT_QTE_CI", m, c(297210, 344590)), o("COMEXT_VAL_CI", m, c(61940846, 72038166)),
    o("RAPI_CI_CIF_EUR", m, c(2.084, 2.091)), o("RAPI_CI_CIF_USD", m, c(2.416, 2.407)),
    o("COMEXT_QTE_CI_40011000", m, c(3000, 4000)),
    if (avec_afrique) rbind(
      o("COMEXT_QTE_GH", m, c(20000, 20210)), o("COMEXT_VAL_GH", m, c(4100000, 4211000)),
      o("RAPI_GH_CIF_EUR", m, c(2.05, 2.084)), o("RAPI_GH_CIF_USD", m, c(2.377, 2.40)),
      o("RAPI_GH_ECART_CI", m, c(-1.6, -0.3)),
      o("COMEXT_QTE_CM_40011000", m, c(9000, 9500))))
  rs_db_ajouter(lot, dir)
  dir
}

test_that("site : bloc afrique absent sans donnée, complet sinon", {
  expect_null(rs_site_afrique(rs_db_lire_obs(base_afrique(FALSE))))
  a <- rs_site_afrique(rs_db_lire_obs(base_afrique()))
  expect_equal(a$version, "RAPI-Afrique v1")
  expect_equal(c(a$debut, a$fin), c("2026-08", "2026-09"))
  codes <- vapply(a$origines, function(o) o$code, character(1))
  expect_equal(codes, RS_AFR_PARTENAIRES)
  gh <- a$origines[[which(codes == "GH")]]
  expect_equal(gh$vol_tsnr_t, round((20000 + 20210) / 10))
  expect_equal(gh$prix_eur_kg, round((2.05 * 20000 + 2.084 * 20210) / 40210, 3))
  expect_equal(length(gh$mois), 2)
  expect_equal(gh$mois[[2]]$ecart, -0.3)
  ci <- a$origines[[1]]
  expect_true(ci$reference); expect_equal(ci$vol_latex_t, 700)
  cm <- a$origines[[which(codes == "CM")]]
  expect_equal(cm$vol_latex_t, 1850); expect_equal(cm$vol_tsnr_t, 0); expect_equal(length(cm$mois), 0)
  expect_true(is.na(cm$prix_eur_kg))
})

test_that("site : jeu de données public avec le bloc afrique, sans série vide au catalogue", {
  d <- rs_site_donnees(base_afrique(), j = list(meta = list(semaine = 47, annee = 2026),
                                               rapi_afrique = list(statut = "ok", date_maj_eurostat = "2026-11-13",
                                                                   phrase_fr = "Phrase FR", phrase_en = "Phrase EN")),
                       validations = data.frame())
  expect_equal(d$afrique$statut, "ok")
  expect_equal(d$afrique$phrase_fr, "Phrase FR")
  expect_true(all(vapply(d$catalogue, function(s) s$n > 0, logical(1))))
  expect_true("RAPI_GH_ECART_CI" %in% vapply(d$catalogue, function(s) s$serie_id, character(1)))
  d0 <- rs_site_donnees(base_afrique(FALSE), j = list(meta = list(semaine = 47, annee = 2026)), validations = data.frame())
  expect_null(d0$afrique)
})

test_that("taux mensuels en base : BCE d'abord, FRED pour les mois manquants", {
  dir <- file.path(tempfile(), "db"); rs_db_init(dir)
  rs_db_ajouter(rbind(rs_db_obs("BCE_USD_EUR_M", c("2026-08", "2026-09"), c(1.1593, 1.1513), "2026-10-01", "2026-11-18"),
                      rs_db_obs("FRED_EXUSEU", c("2026-09", "2026-10"), c(1.15, 1.16), "2026-11-01", "2026-11-18")), dir)
  tx <- rs_db_taux_mensuels(rs_db_lire_obs(dir))
  expect_equal(tx$periode, c("2026-08", "2026-09", "2026-10"))
  expect_equal(tx$taux_usd_eur, c(1.1593, 1.1513, 1.16))
  expect_match(tx$source[3], "source secondaire")
  expect_equal(nrow(rs_db_taux_mensuels(rs_db_lire_obs(file.path(tempfile(), "x") |> (\(d) { rs_db_init(d); d })()))), 0)
})
