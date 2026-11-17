# Tests testthat : RSCI v2.1 (decision de l'evaluateur du 09/11/2026)
# Taux USD/EUR = moyenne mensuelle BCE du mois de reference TSR20 ;
# FRED EXUSEU en source secondaire ; jamais de repli sur le taux du jour.
# Usage (depuis la racine) : Rscript -e "testthat::test_file('tests/test_rsci_v21.R')"
# Hors ligne : les fonctions BCE / FRED sont remplacees par des tables fixes.

library(testthat)
racine <- normalizePath(if (file.exists("scripts/00_regles.R")) "." else "..")
source(file.path(racine, "scripts/00_regles.R"))
source(file.path(racine, "scripts/00_sources.R"))

# Moyennes BCE reelles (EXR.M.USD.EUR.SP00.A, lues le 09/11/2026)
bce_ok <- function(debut) data.frame(periode = c("2026-08", "2026-09", "2026-10"),
                                     taux_usd_eur = c(1.15930952381, 1.15132272727, 1.12015909091),
                                     source = "BCE, moyenne mensuelle du taux de reference (EXR.M.USD.EUR.SP00.A)")
bce_ko  <- function(debut) NULL
fred_ok <- function(debut) data.frame(periode = "2026-08", taux_usd_eur = 1.1601,
                                      source = "FRED EXUSEU (Federal Reserve), source secondaire : BCE indisponible")
fred_ko <- function(debut) NULL

test_that("Taux du mois de reference : BCE en priorite", {
  t <- rs_taux_mensuel_usd_eur("2026-08", bce_ok, fred_ok)
  expect_equal(t$usd_eur, 1.15930952381)
  expect_equal(t$periode, "2026-08")
  expect_false(t$secondaire)
  expect_match(t$source, "BCE")
})

test_that("BCE indisponible : FRED EXUSEU, signale comme source secondaire", {
  t <- rs_taux_mensuel_usd_eur("2026-08", bce_ko, fred_ok)
  expect_equal(t$usd_eur, 1.1601)
  expect_true(t$secondaire)
})

test_that("Mois absent ou sources en panne : aucun taux (pas de repli sur le taux du jour)", {
  expect_null(rs_taux_mensuel_usd_eur("2026-11", bce_ok, fred_ko))   # moyenne de novembre pas encore publiee
  expect_null(rs_taux_mensuel_usd_eur("2026-08", bce_ko, fred_ko))
  expect_null(rs_taux_mensuel_usd_eur(NA, bce_ok, fred_ok))
  expect_null(rs_taux_mensuel_usd_eur("08/2026", bce_ok, fred_ok))
})

test_that("RSCI septembre 2026 : 63,6 % avec la moyenne d'aout (fiche de relecture du 09/11)", {
  t <- rs_taux_mensuel_usd_eur("2026-08", bce_ok, fred_ko)
  calc <- rs_calcul_rsci(484, 655.957 / t$usd_eur, 2.24, "2026-09", 0.60)
  expect_equal(calc$prix_planteur_sec_usd, 1.4257)
  expect_equal(calc$rsci_pct, 63.6)
  expect_equal(calc$ecart_pts, -2.4)
  expect_equal(calc$taux$part_planteur_pct, 66)
  # Ancien calcul (taux du jour 06/11 = 1.1165) : 61,3 %
  expect_equal(rs_calcul_rsci(484, round(655.957 / 1.1165, 2), 2.24, "2026-09", 0.60)$rsci_pct, 61.3)
})

test_that("Sans taux : RSCI non calcule", {
  expect_true(is.na(rs_calcul_rsci(484, NA_real_, 2.24, "2026-09", 0.60)$rsci_pct))
})

test_that("Version de methode RSCI v2.1, en vigueur le 09/11/2026", {
  expect_equal(rs_version_methode("rsci"), "RSCI v2.1")
  expect_equal(RS_METHODES$date_effet[RS_METHODES$cle == "rsci"], "2026-11-09")
})
