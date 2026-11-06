# Tests testthat de la mission 3 : base de donnees data/db/ (00_base.R) et
# RAPI-CI v1 a partir des douanes de l'UE (00_regles.R section 11, script 06).
# Usage (depuis la racine du projet) :
#   Rscript -e "testthat::test_file('tests/test_mission3.R')"
# Hors ligne : aucune requete reseau, aucune cle. Fichiers enregistres :
#   tests/fixtures/mission3/ec_europa_eu_..._DS_045409.txt : reponse reelle de
#     l'API Comext lue le 06/11/2026 (CI, ID, MY, TH, VN ; valeur + quantite ;
#     mois 2026-09 a 2026-12 sans valeur = trous)
#   tests/fixtures/mission3/data_api_ecb_..._SP00_A.txt : reponse reelle BCE
#     (EXR.M.USD.EUR.SP00.A) lue le 06/11/2026
#   tests/fixtures/mission3/comext_synthetique_trous.json : fichier SYNTHETIQUE
#     (valeurs inventees) pour les cas absents des donnees reelles : trous au
#     milieu, faible liquidite, variation > 15 %, panier Asie incomplet

library(testthat)
racine <- normalizePath(if (file.exists("scripts/00_regles.R")) "." else "..")
source(file.path(racine, "scripts/00_regles.R"))
source(file.path(racine, "scripts/00_base.R"))
FX_DIR <- file.path(racine, "tests/fixtures/mission3")
F_REEL <- file.path(FX_DIR, "ec_europa_eu_eurostat_api_comext_dissemination_statistics_1_0_data_DS_045409.txt")
F_BCE  <- file.path(FX_DIR, "data_api_ecb_europa_eu_service_data_EXR_M_USD_EUR_SP00_A.txt")
F_SYN  <- file.path(FX_DIR, "comext_synthetique_trous.json")
lire <- function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
taux_bce <- function() {
  d <- utils::read.csv(F_BCE, stringsAsFactors = FALSE)
  data.frame(periode = d$TIME_PERIOD, taux_usd_eur = d$OBS_VALUE, source = "BCE", stringsAsFactors = FALSE)
}

# ── Lecture JSON-stat avec trous ─────────────────────────────
test_that("Comext : lecture de la reponse reelle (positions, trous, date de mise a jour)", {
  long <- rs_comext_lire(lire(F_REEL))
  expect_equal(attr(long, "updated"), "2026-10-16")
  expect_setequal(unique(long$partner), c("CI", "ID", "MY", "TH", "VN"))
  w <- rs_comext_large(long)
  ci <- w[w$partenaire == "CI", ]
  # valeur CI 2026-08 = 59 119 909 EUR (meme valeur que la requete CI seule)
  expect_equal(ci$valeur_eur[ci$periode == "2026-08"], 59119909)
  # Mars 2026 : 27 768 t (preuve de concept du cahier des charges)
  expect_equal(round(ci$quantite_100kg[ci$periode == "2026-03"] / 10), 27768)
  # Trous : mois annonces sans valeur -> NA, jamais 0
  expect_true(all(is.na(w$valeur_eur[w$periode >= "2026-09"])))
  expect_true(all(is.na(w$quantite_100kg[w$periode >= "2026-09"])))
  expect_false(any(w$valeur_eur == 0, na.rm = TRUE))
})

test_that("Comext : trous au milieu de la serie (fichier synthetique)", {
  w <- rs_comext_large(rs_comext_lire(lire(F_SYN)))
  expect_equal(nrow(w), 20)
  expect_true(is.na(w$valeur_eur[w$partenaire == "CI" & w$periode == "2026-04"]))       # position 3 absente
  expect_equal(w$quantite_100kg[w$partenaire == "CI" & w$periode == "2026-04"], 300000)
  expect_true(is.na(w$valeur_eur[w$partenaire == "VN" & w$periode == "2026-02"]))       # position 33 absente
  expect_equal(w$valeur_eur[w$partenaire == "TH" & w$periode == "2026-03"], 42000000)   # position 26
  expect_error(rs_comext_lire('{"value":{}}'), "JSON-stat")
})

# ── Prix, composite, decote ──────────────────────────────────
test_that("RAPI-CI : prix CIF EUR/kg = valeur / (quantite 100 kg x 100), conversion USD (BCE)", {
  expect_equal(rs_prix_cif_eur_kg(50e6, 250000), 2)
  expect_true(is.na(rs_prix_cif_eur_kg(NA, 250000)))
  expect_true(is.na(rs_prix_cif_eur_kg(1e6, 0)))
  d <- rs_rapi_calcul(rs_comext_large(rs_comext_lire(lire(F_REEL))), taux_bce())
  m <- d[d$periode == "2026-03", ]
  expect_equal(round(m$ci_eur_kg, 3), 1.739)
  expect_equal(round(m$taux_usd_eur, 4), 1.1558)
  expect_equal(round(m$ci_usd_kg, 3), 2.010)          # preuve de concept : 2,009 (arrondi du taux)
  a <- d[d$periode == "2026-08", ]
  expect_equal(round(a$ci_usd_kg, 3), 2.412)          # preuve de concept : 2,413
  expect_equal(round(a$volume_ci_t), 28410)
  # les mois sans valeur ne produisent pas de ligne
  expect_false(any(d$periode >= "2026-09"))
})

test_that("RAPI-CI : composite Asie pondere par les volumes (somme / somme), pas une moyenne simple", {
  cp <- rs_composite_pondere(c(22e6, 11.5e6, 42e6, 10e6), c(1e5, 5e4, 2e5, 5e4), c("ID", "MY", "TH", "VN"))
  expect_true(cp$complet)
  expect_equal(cp$eur_kg, 85.5e6 / 40e6)                       # 2.1375
  expect_false(isTRUE(all.equal(cp$eur_kg, mean(c(2.2, 2.3, 2.1, 2.0)))))   # moyenne simple = 2.15
  # panier incomplet -> NA (aucune valeur de substitution)
  inc <- rs_composite_pondere(c(22e6, 11.5e6, 42e6, NA), c(1e5, 5e4, 2e5, 5e4), c("ID", "MY", "TH", "VN"))
  expect_false(inc$complet); expect_true(is.na(inc$eur_kg)); expect_equal(inc$manquants, "VN")
  # un partenaire hors panier est ignore
  expect_equal(rs_composite_pondere(c(1e6, 22e6, 11.5e6, 42e6, 10e6), c(1e4, 1e5, 5e4, 2e5, 5e4),
                                    c("BR", "ID", "MY", "TH", "VN"))$eur_kg, 2.1375)
})

test_that("RAPI-CI : decote CI / composite Asie, faible liquidite, alerte de variation (synthetique)", {
  tx <- data.frame(periode = c("2026-01", "2026-02", "2026-03", "2026-04"),
                   taux_usd_eur = c(1.17, 1.18, 1.16, 1.17), source = "BCE")
  d <- rs_rapi_calcul(rs_comext_large(rs_comext_lire(lire(F_SYN))), tx)
  expect_equal(d$periode, c("2026-01", "2026-02", "2026-03", "2026-04"))
  # Janvier : CI 2,000 EUR/kg ; Asie 2,1375 ; decote -6,43 %
  expect_equal(d$ci_eur_kg[1], 2)
  expect_equal(d$asie_eur_kg[1], 2.1375)
  expect_equal(round(d$decote_pct[1], 2), round((2 / 2.1375 - 1) * 100, 2))
  expect_equal(d$ci_usd_kg[1], 2 * 1.17)
  expect_equal(d$decote_pct[1], rs_decote_pct(d$ci_usd_kg[1], d$asie_usd_kg[1]))   # meme ratio en USD
  # Fevrier : VN sans valeur -> composite et decote NA, panier signale incomplet
  expect_false(d$asie_complet[2]); expect_equal(d$asie_manquants[2], "VN")
  expect_true(is.na(d$asie_eur_kg[2])); expect_true(is.na(d$decote_pct[2]))
  # Mars : 4 000 t -> faible liquidite ; 2,0 -> 2,4 EUR/kg = +20 % -> alerte (valeur conservee)
  expect_equal(d$volume_ci_t[3], 4000)
  expect_true(d$faible_liquidite[3]); expect_false(d$faible_liquidite[1])
  expect_equal(round(d$variation_pct[3], 6), 20)
  expect_true(d$alerte_variation[3]); expect_false(d$alerte_variation[2])
  expect_equal(d$ci_eur_kg[3], 2.4)
  # Avril : valeur CI absente -> prix NA, volume conserve, pas de variation inventee
  expect_true(is.na(d$ci_eur_kg[4])); expect_equal(d$volume_ci_t[4], 30000); expect_true(is.na(d$variation_pct[4]))
  # Taux absent pour un mois -> USD NA (EUR conserve)
  d2 <- rs_rapi_calcul(rs_comext_large(rs_comext_lire(lire(F_SYN))), tx[-1, ])
  expect_true(is.na(d2$ci_usd_kg[1])); expect_equal(d2$ci_eur_kg[1], 2)
  # Mois non consecutifs : pas de variation calculee
  w <- rs_comext_large(rs_comext_lire(lire(F_SYN)))
  d3 <- rs_rapi_calcul(w[w$periode != "2026-02", ], tx)
  expect_true(is.na(d3$variation_pct[d3$periode == "2026-03"]))
  expect_equal(RS_RAPI_SEUIL_LIQUIDITE_T, 5000); expect_equal(RS_RAPI_SEUIL_VARIATION, 0.15)
})

test_that("RAPI-CI : fraicheur mesuree depuis la mise a jour Eurostat (45 jours)", {
  f <- rs_rapi_fraicheur("2026-10-16", "2026-08", as.Date("2026-11-06"))
  expect_equal(f$statut, "derniere_valeur_disponible"); expect_equal(f$age_jours_maj, 21)
  expect_equal(f$age_jours_fin_periode, 67)
  expect_equal(rs_rapi_fraicheur("2026-10-16", "2026-08", as.Date("2026-12-15"))$statut, "perime")
  expect_equal(rs_rapi_fraicheur(NA, "2026-08", as.Date("2026-11-06"))$statut, "date_inconnue")
})

test_that("RAPI-CI : phrase de lecture factuelle, chiffres du JSON uniquement, sans jugement", {
  m <- list(periode = "2026-08", ci_usd_kg = 2.412, ci_eur_kg = 2.081, asie_usd_kg = 2.617, decote_pct = -7.8,
            bm_tsr20_usd_kg = 1.95, ecart_ci_bm_pct = 23.7)
  fr <- rs_rapi_phrase(m, "fr"); en <- rs_rapi_phrase(m, "en")
  expect_match(fr, "En août 2026, le caoutchouc ivoirien", fixed = TRUE)
  expect_match(fr, "2.412 USD/kg", fixed = TRUE); expect_match(fr, "7.8 % sous le composite asiatique", fixed = TRUE)
  expect_match(fr, "1.95 USD/kg", fixed = TRUE); expect_match(fr, "s'en écarte de +23.7 %", fixed = TRUE)
  expect_match(rs_rapi_phrase(modifyList(m, list(ci_usd_kg = 2.01)), "fr"), "2.010 USD/kg", fixed = TRUE)   # 3 decimales
  expect_equal(rs_rapi_fmt(-7.8, 1, signe = TRUE), "-7.8"); expect_equal(rs_rapi_fmt(NA, 3), "—")
  expect_match(en, "7.8% below the Asian composite", fixed = TRUE)
  nums <- regmatches(fr, gregexpr("[0-9]+(\\.[0-9]+)?", fr))[[1]]
  autorises <- c("2026", "4001", "22", "00", "20", "2.412", "2.081", "2.617", "7.8", "1.95", "23.7")
  expect_true(all(nums %in% autorises))
  for (t in c(fr, en)) expect_false(grepl("manque|perte|lose|loss|sous-pay|underpa|injust|unfair", t, ignore.case = TRUE))
  expect_match(rs_rapi_phrase(modifyList(m, list(decote_pct = 3.1)), "fr"), "3.1 % au-dessus du composite", fixed = TRUE)
  expect_match(rs_rapi_phrase(modifyList(m, list(decote_pct = NULL)), "fr"), "composite asiatique indisponible", fixed = TRUE)
  expect_match(rs_rapi_phrase(modifyList(m, list(bm_tsr20_usd_kg = NULL)), "fr"), "TSR20 Banque mondiale du même mois indisponible", fixed = TRUE)
  s03 <- paste(readLines(file.path(racine, "scripts/03_build_report.R"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  # seule occurrence admise : le commentaire qui l'interdit
  expect_false(grepl("manque à gagner", sub("« manque à gagner »", "", s03, fixed = TRUE), fixed = TRUE))
  expect_match(s03, "non publié** (aucune source de fret", fixed = TRUE)
})

test_that("IOSCO : RAPI-CI v1 au registre des methodes, series publiables, sous licence non publiables", {
  expect_equal(rs_version_methode("rapi_ci"), "RAPI-CI v1")
  expect_true(rs_serie_publiable("RAPI_CI")); expect_true(rs_serie_publiable("EUROSTAT_COMEXT"))
  s <- rs_db_catalogue_series()
  expect_equal(names(s), RS_DB_COLS_SERIES)
  expect_true(all(s$publiable[s$serie_id %in% c("MRB_SMR20", "SGX_TSR20", "INDEXMUNDI")] == "non"))
  expect_true(all(s$publiable[grepl("^RAPI_|^COMEXT_|^BM_|^FMI_|^BCE_", s$serie_id)] == "oui"))
  expect_true(all(s$nature %in% c("T", "O", "D", "E", "J")))
  expect_equal(s$nature[s$serie_id == "RAPI_CI_DECOTE_ASIE"], "E")
  expect_equal(s$nature[s$serie_id == "RAPI_CI_CIF_EUR"], "T")
  expect_false(anyDuplicated(s$serie_id) > 0)
})

# ── Base de donnees ──────────────────────────────────────────
nouvelle_base <- function() {
  dir <- file.path(tempfile("rsdb_"), "db")
  rs_db_init(dir)
  dir
}
obs_test <- function(valeur, de = "2026-11-06", date = c("2026-07", "2026-08"))
  rs_db_obs("RAPI_CI_CIF_EUR", date, valeur, "2026-10-16", de, methode = "test")

test_that("Base : initialisation (en-tetes + catalogue), relance sans effet", {
  dir <- nouvelle_base()
  f <- rs_db_fichiers(dir)
  expect_equal(readLines(f$obs), paste(RS_DB_COLS_OBS, collapse = ","))
  s <- rs_db_lire_series(dir)
  expect_equal(nrow(s), nrow(RS_DB_CATALOGUE))
  avant <- readBin(f$series, "raw", 1e6)
  rs_db_init(dir)
  expect_identical(readBin(f$series, "raw", 1e6), avant)
  expect_equal(nrow(rs_db_lire_obs(dir)), 0)
  # fin de ligne LF, UTF-8
  expect_false(any(readBin(f$series, "raw", 1e6) == as.raw(13)))
  expect_true(any(grepl("Côte d'Ivoire", rs_db_lire_series(dir)$libelle_fr, fixed = TRUE)))
})

test_that("Base : ajout idempotent (meme serie + date + valeur -> pas de doublon)", {
  dir <- nouvelle_base()
  r1 <- rs_db_ajouter(obs_test(c(1.934168, 2.080919)), dir)
  expect_equal(r1$n_ajoutees, 2); expect_equal(r1$n_revisions, 0)
  f <- rs_db_fichiers(dir)$obs
  avant <- readBin(f, "raw", 1e6)
  r2 <- rs_db_ajouter(obs_test(c(1.934168, 2.080919), de = "2026-11-13"), dir)
  expect_equal(r2$n_ajoutees, 0); expect_equal(r2$n_identiques, 2)
  expect_identical(readBin(f, "raw", 1e6), avant)
  # bruit flottant sous 12 chiffres significatifs : pas une revision
  r3 <- rs_db_ajouter(obs_test(c(1.934168 + 1e-14, 2.080919)), dir)
  expect_equal(r3$n_ajoutees, 0)
  # doublon dans le meme lot : une seule ligne ; lot contradictoire : arret
  dir2 <- nouvelle_base()
  expect_equal(rs_db_ajouter(rbind(obs_test(2, date = "2026-08"), obs_test(2, date = "2026-08")), dir2)$n_ajoutees, 1)
  expect_error(rs_db_ajouter(rbind(obs_test(2, date = "2026-07"), obs_test(3, date = "2026-07")), dir2), "contradictoires")
  # valeur absente : jamais stockee
  expect_equal(rs_db_ajouter(obs_test(c(NA, NA)), dir2)$n_ignorees, 2)
  # format de stockage : decimal, sans espaces ni notation scientifique
  expect_equal(rs_db_format_valeur(c(61.3, 100000, 59119909, -7.82271579277123, NA)),
               c("61.3", "100000", "59119909", "-7.82271579277", NA))
})

test_that("Base : revision detectee, nouvelle ligne revise = TRUE, ancienne conservee, ajout seulement", {
  dir <- nouvelle_base()
  rs_db_ajouter(obs_test(c(1.934168, 2.080919)), dir)
  f <- rs_db_fichiers(dir)$obs
  avant <- readBin(f, "raw", 1e6)
  r <- rs_db_ajouter(obs_test(c(1.95, 2.080919), de = "2026-11-20"), dir)
  expect_equal(r$n_revisions, 1); expect_equal(r$n_ajoutees, 1); expect_equal(r$n_identiques, 1)
  expect_equal(r$revisions$date_valeur, "2026-07")
  expect_equal(r$revisions$ancienne, 1.934168); expect_equal(r$revisions$nouvelle, 1.95)
  expect_equal(r$revisions$date_extraction_ancienne, "2026-11-06")
  apres <- readBin(f, "raw", 1e6)
  expect_identical(apres[seq_along(avant)], avant)          # lignes existantes intactes (ajout seulement)
  o <- rs_db_lire_obs(dir)
  expect_equal(nrow(o), 3)
  expect_equal(o$valeur[o$date_valeur == "2026-07"], c(1.934168, 1.95))
  expect_equal(o$revise[o$date_valeur == "2026-07"], c(FALSE, TRUE))
  h <- rs_db_historique(o, "RAPI_CI_CIF_EUR")
  expect_equal(h$valeur, c(1.95, 2.080919))
  expect_equal(h$deja_revise, c(TRUE, FALSE)); expect_equal(h$n_versions, c(2L, 1L))
  # la meme valeur revisee revue la semaine suivante : pas de nouvelle ligne
  expect_equal(rs_db_ajouter(obs_test(c(1.95, 2.080919), de = "2026-11-27"), dir)$n_ajoutees, 0)
  # retour a l'ancienne valeur = nouvelle revision (comparaison a la DERNIERE ligne)
  expect_equal(rs_db_ajouter(obs_test(c(1.934168, 2.080919), de = "2026-12-04"), dir)$n_revisions, 1)
})

test_that("Base : derniere valeur connue d'une serie, controles d'entree", {
  dir <- nouvelle_base()
  expect_null(rs_db_derniere(rs_db_lire_obs(dir), "RAPI_CI_CIF_EUR"))
  rs_db_ajouter(obs_test(c(1.934168, 2.080919)), dir)
  rs_db_ajouter(obs_test(2.1, de = "2026-11-20", date = "2026-08"), dir)
  der <- rs_db_derniere(rs_db_lire_obs(dir), "RAPI_CI_CIF_EUR")
  expect_equal(der$date_valeur, "2026-08"); expect_equal(der$valeur, 2.1)
  expect_equal(der$date_extraction, "2026-11-20"); expect_equal(der$date_source, "2026-10-16")
  expect_true(der$deja_revise)
  expect_error(rs_db_obs("SERIE_INCONNUE", "2026-08", 1, "2026-10-16", "2026-11-06"), "non declaree")
  expect_error(rs_db_ajouter(rs_db_obs("RAPI_CI_CIF_EUR", "08/2026", 1, "2026-10-16", "2026-11-06"), dir), "date_valeur invalide")
  # en-tete modifie -> arret (jamais d'ecriture dans un fichier inconnu)
  f <- rs_db_fichiers(dir)$obs
  l <- readLines(f); l[1] <- sub("revise", "revisee", l[1]); writeLines(l, f)
  expect_error(rs_db_lire_obs(dir), "en-tete inattendu")
  # le fichier ne peut pas rendre publiable une serie sous licence
  dir3 <- nouvelle_base(); fs <- rs_db_fichiers(dir3)$series
  s <- readLines(fs, encoding = "UTF-8"); i <- grep("^\"MRB_SMR20\"", s)
  s[i] <- sub("\"non\"", "\"oui\"", s[i]); writeLines(s, fs, useBytes = TRUE)
  expect_error(rs_db_init(dir3), "publiable")
})

test_that("Base : observations de la collecte hebdomadaire (script 01)", {
  bm  <- list(serie = data.frame(periode = c("2026-09", "2026-08"), usd_kg = c(1.9, 1.95)),
              methode = "Moyenne mensuelle", url = "https://exemple.test/cmo.xlsx")
  fmi <- list(serie = data.frame(periode = "2026-09", cents_lb = 120, usd_kg = 2.6455), methode = "FMI")
  ap  <- data.frame(mois = c("2026-05", "2026-06"), prix_fcfa = c(480, 484), type = "carreau usine",
                    source = "APROMAC", url = NA, date_publication = c("2026-05-02", NA),
                    date_releve_presse = c(NA, "2026-06-10"), origine = c("saisie", "presse_auto"), stringsAsFactors = FALSE)
  rsci <- list(rsci_pct = 66.4, mois_rsci = "2026-06", methode = "RSCI v2.", mecanisme_officiel_pct = 66,
               reference_tsr20 = list(periode = "2026-05"))
  fx  <- list(eur_usd = 1.1204, date = "2026-11-05", source = "Frankfurter (taux de reference BCE)")
  tsr <- list(candidats = list(mrb = list(usd = 2.487, indice = "MRB_SMR20", source = "MRB", date_source = "2026-11-05"),
                               sgx = list(usd = 2.45, indice = "SGX_TSR20", source = "SGX", date_source = NA)))
  o <- rs_db_obs_collecte("2026-11-06", bm, fmi, ap, rsci, fx, tsr)
  expect_setequal(unique(o$serie_id), c("BM_TSR20", "FMI_PRUBBUSDM", "APROMAC", "RSCI", "FX_USD_EUR_J", "MRB_SMR20"))
  expect_equal(o$date_source[o$serie_id == "APROMAC"], c("2026-05-02", "2026-06-10"))
  expect_equal(o$date_valeur[o$serie_id == "RSCI"], "2026-11-06")
  expect_false("SGX_TSR20" %in% o$serie_id)        # cotation sans date : non stockee
  dir <- nouvelle_base()
  expect_equal(rs_db_ajouter(o, dir)$n_ajoutees, nrow(o))
  r2 <- rs_db_ajouter(rs_db_obs_collecte("2026-11-13", bm, fmi, ap, rsci, fx, tsr), dir)
  expect_equal(r2$n_ajoutees, 1); expect_equal(r2$n_identiques, nrow(o) - 1)   # seul le RSCI du jour de collecte est nouveau
  s <- rs_db_lire_series(dir)
  expect_equal(s$publiable[s$serie_id == "MRB_SMR20"], "non")
})

# ── Script 06 de bout en bout, hors ligne (copie temporaire) ─
test_that("Script 06 : bloc rapi_ci, base idempotente, revision, API indisponible, BCE indisponible", {
  rscript <- file.path(R.home("bin"), "Rscript")
  tmp <- tempfile("rs06_"); dir.create(file.path(tmp, "scripts"), recursive = TRUE)
  for (s in c("00_regles.R", "00_sources.R", "00_base.R", "06_collect_trade.R"))
    file.copy(file.path(racine, "scripts", s), file.path(tmp, "scripts", s))
  dir.create(file.path(tmp, "data/processed"), recursive = TRUE)
  fj <- file.path(tmp, "data/processed/rubbersignal_S45_2026.json")
  jsonlite::write_json(list(meta = list(semaine = 45, annee = 2026),
                            prix = list(historique_bm = list(list(periode = "2026-08", prix_usd = 1.95)),
                                        reference_mensuelle = list(bm_tsr20 = list(source = "Banque mondiale (test)")))),
                       fj, auto_unbox = TRUE)
  fx_ok <- file.path(tmp, "fx_ok"); dir.create(fx_ok)
  file.copy(c(F_REEL, F_BCE), fx_ok)
  old <- Sys.getenv(c("RUBBERSIGNAL_FIXTURE_DIR", "RUBBERSIGNAL_TODAY", "FRED_KEY"), unset = NA)
  on.exit({ for (k in names(old)) if (is.na(old[[k]])) Sys.unsetenv(k) else do.call(Sys.setenv, as.list(old[k])) }, add = TRUE)
  lancer <- function(dir_fixtures) {
    Sys.setenv(RUBBERSIGNAL_FIXTURE_DIR = dir_fixtures, RUBBERSIGNAL_TODAY = "2026-11-06", FRED_KEY = "factice")
    out <- suppressWarnings(system2(rscript, c("--vanilla", "scripts/06_collect_trade.R"), stdout = TRUE, stderr = TRUE))
    st <- attr(out, "status") %||% 0L
    if (st != 0) cat(out, sep = "\n")
    expect_equal(st, 0L)
    list(out = out, json = jsonlite::read_json(fj), obs = rs_db_lire_obs(file.path(tmp, "data/db")))
  }
  wd <- getwd(); setwd(tmp); on.exit(setwd(wd), add = TRUE)

  # 1. Passage normal
  r1 <- lancer(fx_ok)
  b <- r1$json$rapi_ci
  expect_equal(b$statut, "ok"); expect_true(b$nouvelle_extraction)
  expect_equal(b$date_maj_eurostat, "2026-10-16"); expect_equal(b$derniere_periode, "2026-08")
  expect_equal(b$statut_fraicheur, "derniere_valeur_disponible")
  expect_equal(length(b$mois), 12)
  expect_equal(b$version_methode, "RAPI-CI v1")
  d <- b$derniere
  expect_equal(d$ci_usd_kg, 2.412); expect_equal(d$volume_ci_t, 28410); expect_equal(d$decote_pct, -7.8)
  expect_equal(d$bm_tsr20_usd_kg, 1.95); expect_equal(d$ecart_ci_bm_pct, round((2.412429 / 1.95 - 1) * 100, 1))
  expect_false(d$faible_liquidite); expect_false(d$alerte_variation); expect_false(d$revise)
  for (k in c("ci_eur_kg", "ci_usd_kg", "asie_usd_kg", "decote_ci_asie_pct")) {
    fi <- d$fiches[[k]]
    for (champ in c("valeur", "unite", "periode", "source", "date_source", "nature", "version_methode"))
      expect_false(is.null(fi[[champ]]), info = paste(k, champ))
  }
  expect_equal(d$fiches$decote_ci_asie_pct$nature, "E"); expect_equal(d$fiches$ci_eur_kg$nature, "T")
  expect_false(b$taux_change$secondaire_utilise)
  expect_equal(b$fob_abidjan$statut, "non_publie")
  expect_true(file.exists(file.path(tmp, "data/raw/comext_20261106.json")))
  n1 <- nrow(r1$obs)
  expect_gt(n1, 100)
  expect_true(all(c("COMEXT_VAL_CI", "COMEXT_QTE_VN", "BCE_USD_EUR_M", "RAPI_CI_DECOTE_ASIE") %in% r1$obs$serie_id))

  # 2. Relance identique : aucune ligne ajoutee
  r2 <- lancer(fx_ok)
  expect_equal(nrow(r2$obs), n1)

  # 3. Revision Eurostat de juillet 2026 (valeur CI modifiee) : drapeau « revise »
  fx_rev <- file.path(tmp, "fx_rev"); dir.create(fx_rev); file.copy(F_BCE, fx_rev)
  j <- jsonlite::fromJSON(F_REEL, simplifyVector = FALSE)
  pos <- as.character(17)                         # CI, VALUE_IN_EUROS, 2026-07
  expect_equal(j$value[[pos]], 59983191)
  j$value[[pos]] <- 60500000
  writeLines(jsonlite::toJSON(j, auto_unbox = TRUE, digits = NA), file.path(fx_rev, basename(F_REEL)))
  r3 <- lancer(fx_rev)
  jul <- Filter(function(m) m$periode == "2026-07", r3$json$rapi_ci$mois)[[1]]
  expect_true(jul$revise)
  expect_false(r3$json$rapi_ci$derniere$revise)
  h <- r3$obs[r3$obs$serie_id == "COMEXT_VAL_CI" & r3$obs$date_valeur == "2026-07", ]
  expect_equal(h$valeur, c(59983191, 60500000)); expect_equal(h$revise, c(FALSE, TRUE))
  expect_true(any(grepl("::warning::Revision : COMEXT_VAL_CI 2026-07", r3$out, fixed = TRUE)))

  # 4. API Comext indisponible : derniere valeur de la base, avec sa date, jamais « nouvelle »
  fx_ko <- file.path(tmp, "fx_ko"); dir.create(fx_ko); file.copy(F_BCE, fx_ko)
  n3 <- nrow(r3$obs)
  r4 <- lancer(fx_ko)
  b4 <- r4$json$rapi_ci
  expect_equal(b4$statut, "indisponible"); expect_false(b4$nouvelle_extraction)
  expect_match(b4$valeurs_issues_de, "base")
  expect_equal(b4$date_extraction_valeurs, "2026-11-06"); expect_equal(b4$date_maj_eurostat, "2026-10-16")
  expect_equal(b4$derniere$periode, "2026-08"); expect_equal(b4$derniere$ci_usd_kg, 2.412)
  expect_true(Filter(function(m) m$periode == "2026-07", b4$mois)[[1]]$revise)
  expect_true(any(grepl("::warning::Eurostat Comext indisponible", r4$out, fixed = TRUE)))
  expect_equal(nrow(r4$obs), n3)                   # rien d'ecrit dans la base

  # 5. BCE indisponible : FRED EXUSEU (source secondaire) indiquee sur la valeur
  unlink(file.path(tmp, "data/db"), recursive = TRUE)
  fx_fred <- file.path(tmp, "fx_fred"); dir.create(fx_fred); file.copy(F_REEL, fx_fred)
  writeLines(jsonlite::toJSON(list(observations = data.frame(date = sprintf("%s-01", c(paste0("2025-", sprintf("%02d", 1:12)), paste0("2026-", sprintf("%02d", 1:10)))),
                                                           value = "1.15")), auto_unbox = TRUE),
             file.path(fx_fred, "api_stlouisfed_org_fred_series_observations.txt"))
  r5 <- lancer(fx_fred)
  b5 <- r5$json$rapi_ci
  expect_true(b5$taux_change$secondaire_utilise)
  expect_true(b5$derniere$taux_source_secondaire)
  expect_match(b5$derniere$fiches$ci_usd_kg$source, "FRED EXUSEU")
  expect_equal(b5$derniere$ci_usd_kg, round(2.080919 * 1.15, 3))
  expect_true("FRED_EXUSEU" %in% r5$obs$serie_id); expect_false("BCE_USD_EUR_M" %in% r5$obs$serie_id)
})
