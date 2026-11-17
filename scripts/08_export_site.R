# ============================================================
# RUBBERSIGNAL — Script 08 : données publiques du site et du terminal
# ============================================================
# Role    : après le rapport (03), écrire site/data/terminal.json, terminal.js
#           et observations_publiques.csv à partir de la base et du JSON de la
#           semaine. Séries publiables seulement ; la garde licence (07) passe
#           ensuite sur site/ comme sur data/ et output/.
# Usage   : Rscript scripts/08_export_site.R        (depuis la racine)
# Version : 1.0 — 16/11/2026
# ============================================================

suppressPackageStartupMessages(library(jsonlite))
src <- function(f) source(if (file.exists(file.path("scripts", f))) file.path("scripts", f) else f)
src("00_regles.R"); src("00_base.R"); src("00_site.R")
rs_db_init()   # met a jour le catalogue des series (libelles, versions) ; ne touche pas aux observations

cat("=== RUBBERSIGNAL — Export des données publiques du site ===\n")

# JSON de la semaine en cours, sinon le plus récent
f_sem <- sprintf("data/processed/rubbersignal_S%d_%d.json",
                 as.integer(format(Sys.Date(), "%V")), as.integer(format(Sys.Date(), "%G")))
if (!file.exists(f_sem)) {
  tous <- list.files("data/processed", "^rubbersignal_S[0-9]+_[0-9]+\\.json$", full.names = TRUE)
  cle <- as.integer(sub(".*_S([0-9]+)_([0-9]+)\\.json$", "\\2", tous)) * 100 + as.integer(sub(".*_S([0-9]+)_.*", "\\1", tous))
  f_sem <- if (length(tous) > 0) tous[which.max(cle)] else NA_character_
}
if (is.na(f_sem)) stop("Aucun JSON de passage dans data/processed/")
cat("   JSON de passage :", f_sem, "\n")

d <- rs_site_donnees(j = fromJSON(f_sem, simplifyVector = FALSE))
r <- rs_site_ecrire(d)
cat("   Séries publiables :", r$n_series, "| observations :", r$n_obs, "\n")
cat("   RAPI-CI :", length(d$rapi$mois), "mois | RSCI :", length(d$rsci$historique), "mois | validation :", d$meta$validation$statut, "\n")
cat("Écrit : site/data/terminal.json, terminal.js, observations_publiques.csv\n")
