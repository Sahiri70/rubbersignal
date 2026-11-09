# Tests testthat : aucune valeur sous licence dans les fichiers publics
# (decision D1, 00_regles.R section 8 bis, scripts/07_garde_licence.R).
# Usage (depuis la racine du projet) :
#   Rscript -e "testthat::test_file('tests/test_licence.R')"
# Hors ligne. Toutes les valeurs sont INVENTEES (9.876, 8.765...) : le depot
# est public, aucun test ne doit contenir une vraie cotation sous licence.

library(testthat)
racine <- normalizePath(if (file.exists("scripts/00_regles.R")) "." else "..")
source(file.path(racine, "scripts/00_regles.R"))
source(file.path(racine, "scripts/00_base.R"))

# JSON de passage synthetique, meme structure que data/processed/rubbersignal_S*.json
json_test <- function(indice = "MRB_SMR20") list(
  prix = list(
    synthese = list(prix_actuel = 9.876, indice = indice, variation_pct = 1.5, tendance = "hausse",
                    moyenne_3m = 9.5, moyenne_6m = NULL, prix_chf = 7.9,
                    prix_hebdo = list(valeur = 9.876, unite = "USD/kg", source = "MRB (via rtas.sg)",
                                      publiable = rs_serie_publiable(indice), date_source = "2026-11-06")),
    reference_mensuelle = list(bm_tsr20 = list(valeur = 2.55, unite = "USD/kg", publiable = TRUE)),
    marches_mondiaux = list(spread_sicom_lgm = 0.123,
                            lgm = list(prix_usd = 9.876, source = "MRB"),
                            sicom = list(prix_orig = 999.9, prix_usd = 9.999, contrat = "Dec 2026")),
    historique_bm = list(list(periode = "2026-10", prix_usd = 2.55)),
    historique_hebdo_mensuel = list(list(periode = "2026-11", prix_usd = 9.876)),
    historique_indexmundi = list()),
  controle_qualite = list(tsr20 = list(indice = indice, divergence_max_pct = 1.2,
    candidats = list(mrb = list(usd = 9.876, indice = "MRB_SMR20", date_source = "2026-11-06"),
                     sgx = list(usd = 9.999, indice = "SGX_TSR20", date_source = "2026-11-06"),
                     bm  = list(usd = 2.55, indice = "BM_TSR20", date_source = "2026-10-01")))),
  autre = list(bloc = list(valeur = 8.765, publiable = FALSE), libre = list(valeur = 1.234, publiable = TRUE))
)

test_that("Expurgation : valeurs sous licence retirees, sources, dates et valeurs publiques gardees", {
  e <- rs_expurger_json(json_test(), publier_licence = FALSE)
  s <- e$prix$synthese
  for (k in c("prix_actuel", "variation_pct", "moyenne_3m", "prix_chf")) expect_true(is.na(s[[k]]), info = k)
  expect_null(s$moyenne_6m)                          # absent reste absent
  expect_equal(s$tendance, "non publiée (licence)")
  expect_true(is.na(s$prix_hebdo$valeur))
  expect_equal(s$prix_hebdo$source, "MRB (via rtas.sg)")
  expect_equal(s$prix_hebdo$date_source, "2026-11-06")
  m <- e$prix$marches_mondiaux
  expect_true(is.na(m$spread_sicom_lgm)); expect_true(is.na(m$lgm$prix_usd))
  expect_true(is.na(m$sicom$prix_usd)); expect_true(is.na(m$sicom$prix_orig))
  expect_equal(m$sicom$contrat, "Dec 2026")
  expect_length(e$prix$historique_hebdo_mensuel, 0)
  cand <- e$controle_qualite$tsr20$candidats
  expect_true(is.na(cand$mrb$usd)); expect_true(is.na(cand$sgx$usd))
  expect_equal(cand$bm$usd, 2.55)                    # Banque mondiale : publiable, gardee
  expect_equal(e$prix$reference_mensuelle$bm_tsr20$valeur, 2.55)
  expect_equal(e$prix$historique_bm[[1]]$prix_usd, 2.55)
  expect_true(is.na(e$autre$bloc$valeur))            # regle generale publiable = FALSE
  expect_equal(e$autre$libre$valeur, 1.234)
  expect_equal(e$controle_qualite$tsr20$divergence_max_pct, 1.2)
})

test_that("Expurgation : rien ne change si D1 = TRUE ; idempotente si D1 = FALSE", {
  j <- json_test()
  expect_identical(rs_expurger_json(j, publier_licence = TRUE), j)
  e <- rs_expurger_json(j, FALSE)
  expect_identical(rs_expurger_json(e, FALSE), e)
})

test_that("Expurgation : un prix hebdo issu d'une serie publiable n'est pas retire", {
  e <- rs_expurger_json(json_test("BM_TSR20"), FALSE)
  expect_equal(e$prix$synthese$prix_actuel, 9.876)
  expect_true(is.na(e$prix$marches_mondiaux$sicom$prix_usd))   # bourses : toujours sous licence
})

test_that("Aller-retour JSON : fichier expurge = conforme au controle", {
  tmp <- tempfile(fileext = ".json")
  jsonlite::write_json(rs_expurger_json(json_test(), FALSE), tmp, auto_unbox = TRUE, na = "null", null = "null", digits = NA)
  relu <- jsonlite::fromJSON(tmp, simplifyVector = FALSE)
  expect_length(rs_controle_licence(json = setNames(list(relu), tmp), publier_licence = FALSE), 0)
  expect_false(any(grepl("9\\.876|9\\.999", readLines(tmp))))
  expect_true(any(grepl("2\\.55", readLines(tmp))))
})

test_that("Valeurs recherchees : cotations sous licence seulement, 2 decimales ou plus", {
  v <- rs_valeurs_licence(json_test())
  expect_setequal(v, c("9.876", "9.999"))
  expect_false("2.55" %in% v)
})

test_that("Controle gardien : chaque type de fuite est detecte", {
  d <- tempfile(); dir.create(d)
  j <- json_test()
  f_md <- file.path(d, "rapport.md"); writeLines(c("Titre", "Prix : 9.876 USD/kg"), f_md)
  f_ok <- file.path(d, "propre.md"); writeLines(c("TSR20 Banque mondiale 2.55", "volume 19.8765"), f_ok)
  f_im <- file.path(d, "prix_indexmundi_2026_S46.csv"); writeLines("a,b", f_im)
  series <- data.frame(serie_id = c("BM_TSR20", "MRB_SMR20"), publiable = c("oui", "non"))
  obs <- data.frame(serie_id = c("BM_TSR20", "MRB_SMR20", "MRB_SMR20"))
  hist <- data.frame(cle = "2026-W46", indice = "MRB_SMR20")
  x <- rs_controle_licence(json = list(j.json = j), obs = obs, series = series, hist = hist,
                           fichiers = c(f_md, f_ok, f_im), valeurs = rs_valeurs_licence(j), publier_licence = FALSE)
  expect_true(any(grepl("j.json : contient", x)))
  expect_true(any(grepl("2 valeur\\(s\\) de MRB_SMR20", x)))
  expect_true(any(grepl("tsr20_hebdo.csv : cotations MRB_SMR20", x)))
  expect_true(any(grepl("rapport.md ligne 2", x)))
  expect_true(any(grepl("fichier IndexMundi", x)))
  expect_false(any(grepl("propre.md", x)))          # 19.8765 ne contient pas la valeur 9.876 isolee
  # Tout conforme : aucune infraction
  expect_length(rs_controle_licence(json = list(j.json = rs_expurger_json(j, FALSE)), obs = obs[1, , drop = FALSE],
                                    series = series, hist = hist[0, ], fichiers = f_ok,
                                    valeurs = rs_valeurs_licence(j), publier_licence = FALSE), 0)
  # D1 = TRUE : le controle ne bloque pas
  expect_length(rs_controle_licence(json = list(j.json = j), obs = obs, series = series, publier_licence = TRUE), 0)
})

test_that("Base et historique : les cotations sous licence ne sont ecrites que si D1 = TRUE", {
  tsr <- list(candidats = list(
    mrb = list(usd = 9.876, indice = "MRB_SMR20", date_source = "2026-11-06", source = "MRB"),
    sgx = list(usd = 9.999, indice = "SGX_TSR20", date_source = "2026-11-06", source = "SGX")))
  fx <- list(eur_usd = 1.1, date = "2026-11-06", source = "BCE")
  sans <- rs_db_obs_collecte("2026-11-09", fx = fx, tsr = tsr, publier_licence = FALSE)
  expect_false(any(sans$serie_id %in% c("MRB_SMR20", "SGX_TSR20")))
  expect_true("FX_USD_EUR_J" %in% sans$serie_id)
  avec <- rs_db_obs_collecte("2026-11-09", fx = fx, tsr = tsr, publier_licence = TRUE)
  expect_true(all(c("MRB_SMR20", "SGX_TSR20") %in% avec$serie_id))
  expect_equal(rs_fichier_hist_tsr20("MRB_SMR20", FALSE), "data/prive/tsr20_hebdo.csv")
  expect_equal(rs_fichier_hist_tsr20("MRB_SMR20", TRUE), "data/reference/tsr20_hebdo.csv")
  expect_equal(rs_fichier_hist_tsr20("BM_TSR20", FALSE), "data/reference/tsr20_hebdo.csv")
})

test_that("Journaux (publics sur GitHub Actions) : valeur sous licence masquee si D1 = FALSE", {
  expect_equal(rs_journal_val(9.876, "MRB_SMR20", FALSE), "[sous licence, non affiche]")
  expect_equal(rs_journal_val(9.876, "MRB_SMR20", TRUE), 9.876)
  expect_equal(rs_journal_val(2.55, "BM_TSR20", FALSE), 2.55)
  expect_equal(rs_journal_val(1, "SERIE_INCONNUE", FALSE), "[sous licence, non affiche]")
})

test_that("Workflow : la garde 07 passe apres le rapport et avant le commit ; data/prive ignore par git", {
  w <- readLines(file.path(racine, ".github/workflows/rubbersignal_pipeline.yml"))
  i03 <- grep("scripts/03_build_report.R", w); i07 <- grep("scripts/07_garde_licence.R", w); ic <- grep("Commit and push", w)
  expect_length(i07, 1)
  expect_true(i03 < i07 && i07 < ic)
  expect_true("data/prive/" %in% trimws(readLines(file.path(racine, ".gitignore"))))
})
