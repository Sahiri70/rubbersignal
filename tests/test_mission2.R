# Tests testthat de la mission 2 (valeurs figees retirees, Pre-RSI v2, licence,
# IOSCO, volatilite). Regles : scripts/00_regles.R (sections 7 a 10).
# Usage (depuis la racine du projet) :
#   Rscript -e "testthat::test_file('tests/test_mission2.R')"
# Hors ligne : aucune requete reseau, aucune cle.

library(testthat)
racine <- if (file.exists("scripts/00_regles.R")) "." else ".."
source(file.path(racine, "scripts/00_regles.R"))
# Code seul : lignes de commentaire et commentaires de fin de ligne retires
# (les commentaires citent volontairement les anciennes valeurs supprimees).
lire_script <- function(nom) {
  l <- readLines(file.path(racine, "scripts", nom), warn = FALSE, encoding = "UTF-8")
  l <- l[!grepl("^\\s*#", l)]
  paste(sub("\\s#\\s.*$", "", l), collapse = "\n")
}

# ── Pre-RSI : sens des composantes ───────────────────────────
test_that("Pre-RSI : sens de chaque composante (score oriente prix)", {
  # Meteo : offre abondante = baissier ; offre sous pression = haussier
  expect_equal(rs_sens_prix(rs_pre_rsi_meteo(80)), "baisse")   # cas S45 : « Offre abondante »
  expect_equal(rs_sens_prix(rs_pre_rsi_meteo(60)), "neutre")
  expect_equal(rs_sens_prix(rs_pre_rsi_meteo(30)), "hausse")
  expect_equal(rs_sens_prix(rs_pre_rsi_meteo(10)), "hausse")
  expect_true(is.na(rs_pre_rsi_meteo(NA)))
  # Devises : yuan fort (USD/CNY sous sa moyenne) = haussier
  expect_equal(rs_sens_prix(rs_pre_rsi_devise(-6.1)), "hausse")
  expect_equal(rs_sens_prix(rs_pre_rsi_devise(3)), "baisse")
  expect_equal(rs_sens_prix(rs_pre_rsi_devise(0.5)), "neutre")
  expect_true(is.na(rs_pre_rsi_devise(NA)))
  # Demande : tel quel
  expect_equal(rs_pre_rsi_demande(40), 40)
  expect_true(is.na(rs_pre_rsi_demande(NA)))
  # Terrain : 3/5 = neutre (l'ancienne formule s/5 donnait 60, haussier)
  expect_equal(rs_pre_rsi_terrain(3), 50)
  expect_equal(rs_pre_rsi_terrain(1), 0)
  expect_equal(rs_pre_rsi_terrain(5), 100)
  expect_true(is.na(rs_pre_rsi_terrain(6)))
  # Geopolitique : 0 article ou pas de donnee = NA (plus « aucune tension = 65 »)
  expect_true(is.na(rs_pre_rsi_geo(0)))
  expect_true(is.na(rs_pre_rsi_geo(NA)))
  expect_equal(rs_sens_prix(rs_pre_rsi_geo(1)), "neutre")
  expect_equal(rs_sens_prix(rs_pre_rsi_geo(5)), "hausse")
})

test_that("Pre-RSI : poids documentes et renormalisation sur les composantes disponibles", {
  expect_equal(sum(RS_PRE_RSI_POIDS), 1)
  expect_equal(unname(RS_PRE_RSI_POIDS[c("meteo", "devises", "demande", "terrain", "geo")]),
               c(0.25, 0.20, 0.25, 0.15, 0.15))
  # Cas S45 : meteo 80 (abondante), yuan fort, demande 40, terrain absent, 0 article
  s <- c(meteo = rs_pre_rsi_meteo(80), devises = rs_pre_rsi_devise(-6.12), demande = 40,
         terrain = NA, geo = rs_pre_rsi_geo(0))
  r <- rs_pre_rsi(s)
  expect_true(r$publie)
  expect_equal(r$n_disponibles, 3)
  # (30 x 0.25 + 70 x 0.20 + 40 x 0.25) / 0.70 = 45
  expect_equal(r$score, 45)
  expect_equal(sum(unlist(r$poids_effectifs)), 1, tolerance = 1e-3)
  expect_equal(r$poids_effectifs$meteo, round(0.25 / 0.70, 4))
  expect_equal(r$poids_effectifs$terrain, 0)
  expect_setequal(r$composantes_absentes, c("terrain", "geo"))
  # Plus de « 63 / Legerement haussier » avec une offre abondante
  expect_false(grepl("haussier", r$signal, ignore.case = TRUE))
  # Toutes les composantes : somme ponderee simple
  t <- rs_pre_rsi(c(meteo = 50, devises = 50, demande = 50, terrain = 50, geo = 50))
  expect_equal(t$score, 50)
  expect_equal(t$poids_effectifs$meteo, 0.25)
})

test_that("Pre-RSI : non publie sous 3 composantes", {
  r <- rs_pre_rsi(c(meteo = 30, devises = NA, demande = 70, terrain = NA, geo = NA))
  expect_false(r$publie)
  expect_true(is.na(r$score))
  expect_equal(r$signal, "Non publié")
  expect_match(r$raison_non_publie, "2 composante")
  r0 <- rs_pre_rsi(c(meteo = NA, devises = NA, demande = NA, terrain = NA, geo = NA))
  expect_false(r0$publie)
  expect_equal(RS_PRE_RSI_MIN_COMPOSANTES, 3)
  expect_true(rs_pre_rsi(c(meteo = 30, devises = 50, demande = 70, terrain = NA, geo = NA))$publie)
})

test_that("Pre-RSI : libelle coherent avec le score et les composantes", {
  expect_equal(rs_pre_rsi_libelle(56), "Légèrement haussier — marché bien orienté")
  expect_equal(rs_pre_rsi_libelle(55), "Neutre — marché équilibré")
  expect_equal(rs_pre_rsi_libelle(45), "Neutre — marché équilibré")
  expect_equal(rs_pre_rsi_libelle(44), "Légèrement baissier — pression sur les prix")
  expect_equal(rs_pre_rsi_libelle(70), "Haussier — conditions favorables")
  expect_equal(rs_pre_rsi_libelle(20), "Baissier — conditions défavorables")
  # Balayage : le libelle suit toujours le sens du score
  for (sc in 0:100) {
    lib <- rs_pre_rsi_libelle(sc); sens <- rs_sens_prix(sc)
    if (sens == "hausse") expect_match(lib, "aussier")
    if (sens == "baisse") expect_match(lib, "aissier")
    if (sens == "neutre") expect_match(lib, "Neutre")
  }
  # Combinaisons aleatoires : le resultat est toujours coherent
  set.seed(1)
  for (i in 1:300) {
    s <- setNames(sample(c(NA, 0:100), 5, replace = TRUE), names(RS_PRE_RSI_POIDS))
    expect_true(rs_pre_rsi_coherent(rs_pre_rsi(s)))
  }
  # Un resultat falsifie (libelle haussier sur un score neutre) est detecte
  r <- rs_pre_rsi(c(meteo = 30, devises = 70, demande = 40, terrain = NA, geo = NA))
  r$signal <- "Légèrement haussier — marché bien orienté"
  expect_false(rs_pre_rsi_coherent(r))
})

test_that("Pre-RSI : script 04 utilise les regles communes (plus de 65 ni de 50 par defaut)", {
  s04 <- lire_script("04_collect_signals.R")
  expect_match(s04, "rs_pre_rsi\\(composantes_rsi\\)")
  expect_false(grepl("nb_articles == 0\\) 65", s04))
  expect_false(grepl("else 50\\s*\\n", s04))
  s05 <- lire_script("05_simulate.R")
  expect_false(grepl("%\\|\\|%\\s*50", s05))
  expect_false(grepl("0\\.27|DRIFT_ANNUEL_BASE|elasticite_prix\\s*=", s05))
})

# ── Devises : jamais de valeur de secours ────────────────────
test_that("Devises : API en echec ou taux manquant -> NA + statut + alerte", {
  r <- rs_devises_lire(NULL)
  for (k in names(RS_DEVISES)) expect_true(is.na(r[[k]]))
  expect_equal(r$statut, "indisponible")
  expect_match(r$alertes[1], "aucune valeur de secours")
  r2 <- rs_devises_lire(list(conversion_rates = list(CNY = 6.6654, MYR = 4.0701, XOF = 585.96, CHF = 0.8195),
                             time_last_update_unix = 1762128001))
  expect_equal(r2$USD_CNY, 6.6654)
  expect_true(is.na(r2$USD_IDR))
  expect_equal(r2$statut, "partiel")
  expect_true(any(grepl("USD_IDR", r2$alertes)))
  expect_equal(r2$date, "2025-11-03")
  r3 <- rs_devises_lire(list(rates = list(CNY = 7, MYR = 4, XOF = 600, IDR = 16000, CHF = 0.8)))
  expect_equal(r3$statut, "ok")
  expect_true(is.na(r3$date))
  # Les anciens taux figes ont disparu du script 04
  s04 <- lire_script("04_collect_signals.R")
  expect_false(grepl("7\\.24|16250|USD_XOF=614|source=\"Secours\"|REF_CNY <- 7\\.10", s04))
  expect_match(s04, "X-Api-Key")
  expect_false(grepl("apiKey=", s04, fixed = TRUE))
})

test_that("PMI manuel et fret estime retires du script 04", {
  s04 <- lire_script("04_collect_signals.R")
  expect_false(grepl("valeur=50\\.2|valeur=49\\.0|valeur=58\\.8", s04))
  expect_false(grepl("bdi_val <- 1450|tarif_ci_eu_usd_tonne = 85|delai_abidjan_rotterdam = 14", s04))
  for (id in c("BSCICP02USM460S", "BSCICP02DEM460S", "CHNBSCICP02STSAM", "JPNBSCICP02STSAQ"))
    expect_match(s04, id)
  f <- file.path(racine, "data/reference/fret.csv")
  expect_true(file.exists(f))
  expect_equal(names(utils::read.csv(f)), c("date_valeur", "indicateur", "valeur", "unite", "source", "url"))
})

# ── Licence (decision D1) ────────────────────────────────────
test_that("Licence : registre des series et reglage TRUE / FALSE", {
  for (s in c("MRB_SMR20", "SGX_TSR20", "LGM", "RTAS_SG", "JPX_TOCOM", "SHFE_RU", "INDEXMUNDI"))
    expect_false(rs_serie_publiable(s))
  for (s in c("BM_TSR20", "FMI_PRUBBUSDM", "APROMAC"))
    expect_true(rs_serie_publiable(s))
  expect_false(rs_serie_publiable("SERIE_INCONNUE"))
  # Defaut = affichage actuel conserve
  ancien <- Sys.getenv("RS_PUBLIER_SOURCES_SOUS_LICENCE", NA)
  Sys.unsetenv("RS_PUBLIER_SOURCES_SOUS_LICENCE")
  expect_true(rs_publier_sous_licence())
  Sys.setenv(RS_PUBLIER_SOURCES_SOUS_LICENCE = "FALSE"); expect_false(rs_publier_sous_licence())
  Sys.setenv(RS_PUBLIER_SOURCES_SOUS_LICENCE = "true");  expect_true(rs_publier_sous_licence())
  Sys.setenv(RS_PUBLIER_SOURCES_SOUS_LICENCE = "peut-etre"); expect_error(rs_publier_sous_licence())
  if (is.na(ancien)) Sys.unsetenv("RS_PUBLIER_SOURCES_SOUS_LICENCE") else Sys.setenv(RS_PUBLIER_SOURCES_SOUS_LICENCE = ancien)

  hebdo <- list(valeur = 2.487, date = "2026-11-03", frais = TRUE, serie = "MRB_SMR20")
  bm    <- list(valeur = 2.41, periode = "2026-09")
  lignes <- list(
    list(serie = "MRB_SMR20", libelle = "Cotation hebdomadaire (MRB_SMR20)", valeur = "2.487 USD/kg",
         date = "03/11/2026", source = "MRB via rtas.sg", nature = "O", statut = "à jour"),
    list(serie = "FMI_PRUBBUSDM", libelle = "Prix mondial FMI (mensuel)", valeur = "2.7574 USD/kg",
         date = "septembre 2026", source = "FMI via FRED", nature = "T", statut = "dernière valeur disponible"),
    list(serie = "BM_TSR20", libelle = "TSR20 Banque mondiale (mensuel)", valeur = "2.41 USD/kg",
         date = "septembre 2026", source = "Banque mondiale", nature = "T", statut = "dernière valeur disponible"))

  # TRUE : titre et tableau comme avant (cotation hebdomadaire affichee)
  t_true <- rs_titre_rapport("fr", 45, 2026, hebdo, bm, "~", TRUE)
  expect_match(t_true, "TSR20 : 2.487 USD/kg \\(03/11/2026\\)")
  tab_true <- rs_tableau_prix(lignes, "fr", TRUE)
  expect_match(tab_true, "MRB_SMR20")
  expect_match(tab_true, "\\| Nature \\|")
  expect_false(grepl("réservée", tab_true))

  # FALSE : seules les series publiables (BM, FMI) + mention licence
  t_false <- rs_titre_rapport("fr", 45, 2026, hebdo, bm, "~", FALSE)
  expect_false(grepl("2.487", t_false))
  expect_match(t_false, "Banque mondiale : 2.41 USD/kg \\(septembre 2026\\)")
  tab_false <- rs_tableau_prix(lignes, "fr", FALSE)
  expect_false(grepl("MRB|2.487|rtas", tab_false))
  expect_match(tab_false, "Banque mondiale")
  expect_match(tab_false, "FMI")
  expect_match(tab_false, "Cotation hebdomadaire réservée \\(licence\\)")
  expect_match(rs_tableau_prix(lignes, "en", FALSE), "Weekly quote reserved \\(licence\\)")
  # Une serie hebdo publiable resterait affichee meme a FALSE
  expect_match(rs_titre_rapport("fr", 45, 2026, modifyList(hebdo, list(serie = "BM_TSR20")), bm, "~", FALSE),
               "2.487")
  # Cotation non fraiche : jamais dans le titre
  expect_false(grepl("2.487", rs_titre_rapport("fr", 45, 2026, modifyList(hebdo, list(frais = FALSE)), bm, "~", TRUE)))
})

# ── IOSCO : nature et version de methode ─────────────────────
test_that("IOSCO : nature et version de methode sur les valeurs publiees", {
  x <- rs_iosco(rs_valeur(66, "%", "s", "2026-11-03", "m"), "E", "rsci")
  expect_equal(x$nature, "E")
  expect_equal(x$version_methode, "RSCI v2")
  expect_equal(x$date_effet_methode, "2026-11-03")
  expect_error(rs_iosco(list(), "X", "rsci"))
  expect_error(rs_version_methode("inconnue"))
  expect_true(all(c("prix_hebdo", "reference_mensuelle", "apromac", "rsci", "terrain", "pre_rsi", "volatilite")
                  %in% RS_METHODES$cle))
  expect_true(all(RS_REGISTRE_SERIES$nature %in% c("T", "O", "D", "E", "J")))
  expect_equal(rs_serie_info("APROMAC")$nature, "O")
  expect_equal(rs_serie_info("RSCI")$nature, "E")
  expect_equal(rs_serie_info("TERRAIN_CI")$nature, "D")
})

# ── Volatilite historique (Monte Carlo) ──────────────────────
test_that("Volatilite : estimation sur l'historique mensuel, fenetre et minimum", {
  per <- format(seq(as.Date("2020-01-01"), by = "1 month", length.out = 70), "%Y-%m")
  # Rendements alternes +a / -a : ecart-type connu
  a <- 0.05
  r <- rep(c(a, -a), length.out = 69)
  prix <- 2 * exp(cumsum(c(0, r)))
  v <- rs_volatilite_historique(per, prix, n_mois = 60)
  attendu <- sd(tail(r, 60)) * sqrt(12)
  expect_equal(v$volatilite_annuelle, round(attendu, 4))
  expect_equal(v$n_rendements, 60)
  expect_equal(v$periode_fin, "2025-10")
  expect_equal(v$periode_debut, "2020-10")
  expect_match(v$methode, "60 rendements")
  # Ordre d'entree indifferent (Pink Sheet triee du plus recent au plus ancien)
  expect_equal(rs_volatilite_historique(rev(per), rev(prix))$volatilite_annuelle, v$volatilite_annuelle)
  # Tendance constante : volatilite nulle, derive = pente
  pc <- 2 * exp(0.01 * (0:69))
  vc <- rs_volatilite_historique(per, pc)
  expect_equal(vc$volatilite_annuelle, 0)
  expect_equal(vc$derive_annuelle_estimee, 0.12)
  # Trop court -> NULL (pas de 27 % par defaut)
  expect_null(rs_volatilite_historique(per[1:10], prix[1:10]))
  expect_null(rs_volatilite_historique(character(), numeric()))
  # Trou dans la serie : les rendements a cheval sur le trou sont exclus
  vt <- rs_volatilite_historique(per[-35], prix[-35], n_mois = 100)
  expect_equal(vt$n_rendements, 67)
})

test_that("Volatilite : fourchette sans derive (mediane = prix de depart, pas de direction)", {
  expect_equal(rs_quantile_gbm(2.41, 0.25, 4, 0.5), 2.41)
  bas <- rs_quantile_gbm(2.41, 0.25, 4, 0.10); haut <- rs_quantile_gbm(2.41, 0.25, 4, 0.90)
  expect_equal(log(haut / 2.41), -log(bas / 2.41), tolerance = 1e-12)   # symetrique en log
  # Monte Carlo du script 05 (meme construction) ~ quantiles analytiques
  set.seed(42)
  sim <- 2.41 * exp(colSums(matrix(rnorm(4 * 20000, 0, 0.25 * sqrt(1 / 52)), nrow = 4)))
  expect_equal(unname(quantile(sim, 0.10)), bas, tolerance = 0.01)
  expect_equal(unname(quantile(sim, 0.90)), haut, tolerance = 0.01)
  s05 <- lire_script("05_simulate.R")
  expect_match(s05, "rs_volatilite_historique")
  expect_false(grepl("prob_hausse", s05))
})
