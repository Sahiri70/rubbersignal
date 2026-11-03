# Tests testthat des corrections du rapport S45 (regles de scripts/00_regles.R)
# Usage (depuis la racine du projet) :
#   Rscript -e "testthat::test_file('tests/test_corrections_S45.R')"
# Hors ligne : aucune requete reseau.

library(testthat)
racine <- if (file.exists("scripts/00_regles.R")) "." else ".."
source(file.path(racine, "scripts/00_regles.R"))

test_that("regle de fraicheur : 14 jours pour une valeur hebdomadaire", {
  ref <- as.Date("2026-11-03")
  expect_equal(rs_fraicheur("2026-11-03", ref)$statut, "frais")
  expect_equal(rs_fraicheur("2026-10-20", ref)$statut, "frais")      # 14 jours pile
  expect_equal(rs_fraicheur("2026-10-19", ref)$statut, "perime")     # 15 jours
  expect_equal(rs_fraicheur("2026-06-03", ref)$statut, "perime")     # cas S45 (LGM 03/06)
  expect_equal(rs_fraicheur("2026-06-03", ref)$age_jours, 153)
  expect_equal(rs_fraicheur(NA, ref)$statut, "date_inconnue")
  expect_equal(rs_fraicheur(NULL, ref)$statut, "date_inconnue")
  expect_equal(rs_fraicheur("2026-11-10", ref)$statut, "date_future")
  expect_false(rs_est_frais(NA, ref))
  expect_true(rs_est_frais("2026-10-30", ref))
  # seuil mensuel (45 jours)
  expect_equal(rs_fraicheur("2026-09-30", ref, RS_MAX_JOURS_MENSUEL)$statut, "frais")
  expect_equal(rs_fraicheur("2026-08-31", ref, RS_MAX_JOURS_MENSUEL)$statut, "perime")
})

test_that("taux planteur en vigueur selon la date (61 -> 63 -> 66 %)", {
  expect_equal(rs_taux_planteur("2022-04-30")$part_planteur_pct, 61)
  expect_equal(rs_taux_planteur("2022-05-01")$part_planteur_pct, 63)
  expect_equal(rs_taux_planteur("2026-04-30")$part_planteur_pct, 63)
  expect_equal(rs_taux_planteur("2026-05-01")$part_planteur_pct, 66)
  expect_equal(rs_taux_planteur("2026-11-03")$part_planteur_pct, 66)
  expect_equal(rs_taux_planteur("2026-05-01")$date_effet, "2026-05-01")
  expect_error(rs_taux_planteur(NA))
})

test_that("RSCI : taux compare = taux en vigueur au mois du prix APROMAC", {
  fx <- round(655.957 / 1.157, 2)   # 566.95 FCFA/USD
  # Avril 2026 : 63 %
  a <- rs_calcul_rsci(474, fx, 2.3685, "2026-04")
  expect_equal(a$taux$part_planteur_pct, 63)
  expect_equal(a$prix_planteur_sec_usd, 1.3935)        # regression formule S24
  expect_equal(a$rsci_pct, 58.8)
  expect_equal(a$ecart_pts, -4.2)
  # Memes chiffres en juin 2026 : 66 %
  j <- rs_calcul_rsci(474, fx, 2.3685, "2026-06")
  expect_equal(j$taux$part_planteur_pct, 66)
  expect_equal(j$rsci_pct, 58.8)
  expect_equal(j$ecart_pts, -7.2)
  # Reference absente -> RSCI NA (jamais de valeur de secours)
  n <- rs_calcul_rsci(474, fx, NA, "2026-06")
  expect_true(is.na(n$rsci_pct))
  expect_true(is.na(n$ecart_pts))
  expect_equal(n$taux$part_planteur_pct, 66)
  # Mois de reference = mois precedent
  expect_equal(rs_mois_precedent("2026-05"), "2026-04")
  expect_equal(rs_mois_precedent("2026-01"), "2025-12")
  expect_equal(RS_DRC_REFERENCE, 0.60)
  expect_match(RS_DRC_STATUT, "confirmer")
})

test_that("conversion FRED PRUBBUSDM : US cents/lb -> USD/kg", {
  expect_equal(rs_cents_lb_vers_usd_kg(100), 2.20462)
  expect_equal(round(rs_cents_lb_vers_usd_kg(125.075), 4), 2.7574)   # septembre 2026
  expect_equal(rs_cents_lb_vers_usd_kg(c(0, 50)), c(0, 1.10231))
})

test_that("APROMAC : saisie prioritaire, dernier mois disponible, alerte si mois en cours manquant", {
  f <- tempfile(fileext = ".csv")
  writeLines(c(
    "mois,prix_fcfa_kg,type,source,url,date_publication",
    "2026-02,368,a confirmer,gouv.ci,,",
    "2026-05,439,carreau usine,x,,",
    "2026-06,474,carreau usine,FratMat,https://www.fratmat.info/x,2026-06-01",
    "2026-07,493,carreau usine,AIP,https://www.aip.ci/?p=386489,2026-07-03"), f)
  m <- rs_apromac_manuel_lire(f)
  expect_equal(nrow(m), 4)

  # Sans historique presse : novembre -> dernier mois = juillet, alerte
  h <- rs_apromac_fusion(m)$historique
  r <- rs_apromac_dernier(h, as.Date("2026-11-03"))
  expect_equal(r$mois, "2026-07")
  expect_equal(r$prix_fcfa, 493)
  expect_equal(r$age_mois, 4)
  expect_true(r$mois_courant_manquant)
  expect_match(r$alerte, "2026-11")

  # Mois en cours present -> pas d'alerte
  r2 <- rs_apromac_dernier(h, as.Date("2026-07-15"))
  expect_equal(r2$mois, "2026-07")
  expect_false(r2$mois_courant_manquant)
  expect_true(is.na(r2$alerte))

  # Un mois futur n'est jamais choisi
  r3 <- rs_apromac_dernier(h, as.Date("2026-06-10"))
  expect_equal(r3$mois, "2026-06")

  # Fusion avec l'historique presse : mois nouveau ajoute, desaccord signale (saisie gagne)
  auto <- data.frame(mois = c("2026-06", "2026-09"), prix_fcfa = c(470, 484),
                     source = "presse", url = "u", date_ajout = "2026-10-06", stringsAsFactors = FALSE)
  fu <- rs_apromac_fusion(m, auto)
  expect_equal(fu$historique$prix_fcfa[fu$historique$mois == "2026-06"], 474)
  expect_length(fu$alertes, 1)
  r4 <- rs_apromac_dernier(fu$historique, as.Date("2026-11-03"))
  expect_equal(r4$mois, "2026-09")
  expect_equal(r4$origine, "presse_auto")

  # Fichier absent ou invalide -> erreur bruyante
  expect_error(rs_apromac_manuel_lire(tempfile()))
  g <- tempfile(fileext = ".csv")
  writeLines(c("mois,prix_fcfa_kg,type,source,url,date_publication", "juillet,493,,,,"), g)
  expect_error(rs_apromac_manuel_lire(g))
  expect_error(rs_apromac_dernier(h, as.Date("2026-01-15")))
})

test_that("le fichier de saisie reel est lisible et contient juillet 2026 = 493", {
  f <- file.path(racine, "data/reference/prix_apromac.csv")
  skip_if_not(file.exists(f))
  m <- rs_apromac_manuel_lire(f)
  expect_equal(m$prix_fcfa_kg[m$mois == "2026-07"], 493)
  expect_equal(m$prix_fcfa_kg[m$mois == "2026-06"], 474)
})
