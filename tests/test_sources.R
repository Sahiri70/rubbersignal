# Tests hors-ligne du module de sources et du script 01 v4.0
# Usage (depuis la racine du projet) : Rscript tests/test_sources.R
# Les "fixtures" reproduisent la structure des pages reelles lues le 06/10/2026.

suppressPackageStartupMessages({ library(jsonlite) })
Sys.setenv(RUBBERSIGNAL_FIXTURE_DIR = normalizePath("tests/fixtures"),
           NEWSAPI_KEY = "factice")
source("scripts/00_sources.R")

ok <- 0; ko <- 0
check <- function(nom, cond) {
  if (isTRUE(cond)) { ok <<- ok + 1; cat("  OK  ", nom, "\n") }
  else { ko <<- ko + 1; cat("  ECHEC", nom, "\n") }
}
near <- function(a, b, tol = 1e-9) is.finite(a) && abs(a - b) < tol

cat("\n[1] Conversion des prix 'a-b'\n")
check("263-00 -> 263", near(rs_parse_dash("263-00"), 263))
check("260-4 -> 260.4", near(rs_parse_dash("260-4"), 260.4))
check("1053-50 -> 1053.5", near(rs_parse_dash("1053-50"), 1053.5))
check("texte invalide -> NA", is.na(rs_parse_dash("abc")))

cat("\n[2] Taux de change\n")
fx <- rs_fx()
check("EUR/USD = 1.1204", near(fx$eur_usd, 1.1204))
check("date 2026-10-05", fx$date == "2026-10-05")
check("JPY/CHF/CNY presents", all(is.finite(c(fx$eur_jpy, fx$eur_cny, fx$eur_chf))))

cat("\n[3] rtas.sg (MRB SMR20 + SGX)\n")
rt <- rs_fetch_rtas()
check("SMR20 = 263.00 cents", near(rt$smr20_cents, 263.00))
check("SGX front = 260.4 cents", near(rt$sgx_cents, 260.4))
check("mois SMR = Nov 2026", identical(rt$smr_mois, "Nov 2026"))
check("mois SGX = Nov 2026", identical(rt$sgx_mois, "Nov 2026"))

cat("\n[4] Chaine TSR20\n")
ts <- rs_tsr20()
check("retenu = MRB via rtas (LGM direct absent)", near(ts$usd, 2.63))
check("indice MRB_SMR20", ts$indice == "MRB_SMR20")
check("ecart MRB/SGX ~ 1 %", is.finite(ts$divergence_max_pct) && ts$divergence_max_pct < 2)
check("pas d'alerte de substitution", length(ts$alertes) == 0)

cat("\n[5] Extraction APROMAC\n")
e <- function(x) rs_extraire_apromac(x)
check("'aout 2026: 474 fcfa'", identical(e("Caoutchouc/ prix Apromac ao\u00fbt 2026: 474 fcfa/ KG"), list(mois = "2026-08", prix = 474)))
check("'484 FCFA/kg en septembre 2026'", identical(e("Prix du caoutchouc : 484 FCFA/kg en septembre 2026"), list(mois = "2026-09", prix = 484)))
check("'plus 500 FCFA octobre 2026' refuse", is.null(e("grimpe à plus 500 FCFA en octobre 2026 APROMAC")))
check("'plus de 500 FCFA' refuse", is.null(e("APROMAC octobre 2026 : plus de 500 FCFA/kg")))
check("sans mois+annee refuse", is.null(e("Le prix du Kg de caoutchouc passe de 317 f à 343 fcfa")))
check("hors sujet (cacao) refuse", is.null(e("Prix du cacao 1200 FCFA le kilo en septembre 2026")))
check("prix hors bornes refuse", is.null(e("caoutchouc septembre 2026 : 1200 FCFA/kg")))

cat("\n[6] Consensus APROMAC (aujourd'hui = 2026-10-06)\n")
cons <- rs_consensus_apromac(rs_articles_apromac(), as.Date("2026-10-06"))
check("2026-09 -> 484", nrow(cons) >= 1 && any(cons$mois == "2026-09" & cons$prix_fcfa == 484))
check("2026-08 -> 474", any(cons$mois == "2026-08" & cons$prix_fcfa == 474))
check("octobre 'plus de 500' absent", !any(cons$mois == "2026-10"))
check("plusieurs sources concordantes sur septembre", cons$n_sources[cons$mois == "2026-09"] >= 2)
check("aucun conflit", !any(cons$conflit))

cat("\n[7] Fraicheur APROMAC\n")
h <- data.frame(mois = c("2026-06", "2026-09"), prix_fcfa = c(474, 484), source = "t", url = "u",
                date_ajout = "2026-10-06", stringsAsFactors = FALSE)
check("meme mois -> ok", rs_apromac_resolu(h, as.Date("2026-09-20"))$statut == "ok")
check("mois suivant -> reporte", rs_apromac_resolu(h, as.Date("2026-10-06"))$statut == "reporte")
check("+2 mois -> perime", rs_apromac_resolu(h, as.Date("2026-11-06"))$statut == "perime")
check("historique vide -> erreur", inherits(try(rs_apromac_resolu(h[0, ], Sys.Date()), silent = TRUE), "try-error"))
check("prix du mois courant prioritaire", rs_apromac_resolu(rbind(h, data.frame(mois = "2026-10", prix_fcfa = 520, source = "t", url = "u", date_ajout = "x")), as.Date("2026-10-06"))$prix_fcfa == 520)

cat("\n[8] Regression formule RSCI (valeurs S24 de ta methodologie)\n")
apromac_usd <- round(474 / round(655.957 / 1.157, 2), 4)
sec <- round(apromac_usd / 0.60, 4)
check("prix planteur sec = 1.3935", near(sec, 1.3935))
check("RSCI S24 = 58.8", near(round(sec / 2.3685 * 100, 1), 58.8))

cat("\n", ok, "tests OK,", ko, "echecs\n")
if (ko > 0) quit(status = 1)
