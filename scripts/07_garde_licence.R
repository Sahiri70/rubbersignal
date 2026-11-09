# ============================================================
# RUBBERSIGNAL — Script 07 : garde licence avant publication
# ============================================================
# Role    : dernier controle avant le commit du robot. Le depot GitHub est
#           public : tout fichier commite est publie. Avec
#           RS_PUBLIER_SOURCES_SOUS_LICENCE = FALSE (decision D1), ce script
#           1. retire des JSON de passage les valeurs des series sous licence
#              (MRB, SGX, LGM, IndexMundi...) ;
#           2. verifie qu'aucun fichier a publier n'en contient encore
#              (JSON, base data/db, historique hebdo, brut, rapports) ;
#           3. arrete le passage (code 1) en cas d'infraction : rien n'est commite.
# Usage   : Rscript scripts/07_garde_licence.R [fichiers JSON...]
#           sans argument : JSON de passage modifies ou nouveaux (git status)
#           et JSON de la semaine en cours.
# Version : 1.0 — 09/11/2026 (regles : 00_regles.R section 8 bis)
# ============================================================

suppressPackageStartupMessages(library(jsonlite))
source(if (file.exists("scripts/00_regles.R")) "scripts/00_regles.R" else "00_regles.R")

cat("=== RUBBERSIGNAL — Garde licence avant publication ===\n")

if (rs_publier_sous_licence()) {
  cat("RS_PUBLIER_SOURCES_SOUS_LICENCE = TRUE : publication autorisee par l'evaluateur, rien a retirer.\n")
  quit(status = 0)
}

# ── 1. Fichiers a publier (modifies ou nouveaux dans data/ et output/) ──
git_changes <- tryCatch(system2("git", c("status", "--porcelain", "--untracked-files=all", "data", "output"),
                                stdout = TRUE, stderr = FALSE), error = function(e) character())
a_publier <- unique(sub("^.. ", "", git_changes))
a_publier <- gsub('^"|"$', "", sub(".* -> ", "", a_publier))
a_publier <- a_publier[!startsWith(a_publier, paste0(RS_DOSSIER_PRIVE, "/")) & file.exists(a_publier)]

args <- commandArgs(trailingOnly = TRUE)
json_semaine <- sprintf("data/processed/rubbersignal_S%d_%d.json",
                        as.integer(format(Sys.Date(), "%V")), as.integer(format(Sys.Date(), "%G")))
f_json <- if (length(args) > 0) args else
  unique(c(a_publier[grepl("^data/processed/rubbersignal_S[0-9]+_[0-9]+\\.json$", a_publier)],
           if (file.exists(json_semaine)) json_semaine))
f_json <- f_json[file.exists(f_json)]

# ── 2. Expurgation des JSON de passage ───────────────────────
lire_json <- function(f) fromJSON(f, simplifyVector = FALSE)
valeurs <- character()
for (f in f_json) {
  j <- lire_json(f)
  valeurs <- unique(c(valeurs, rs_valeurs_licence(j)))
  e <- rs_expurger_json(j, FALSE)
  if (!identical(e, j)) {
    write_json(e, f, pretty = TRUE, auto_unbox = TRUE, na = "null", null = "null", digits = NA)
    cat("   Valeurs sous licence retirees :", f, "\n")
  } else cat("   Deja conforme :", f, "\n")
}

# ── 3. Controle gardien (bloquant) ───────────────────────────
lire_csv <- function(f) if (file.exists(f))
  utils::read.csv(f, colClasses = "character", na.strings = "", encoding = "UTF-8", stringsAsFactors = FALSE) else NULL
fichiers <- unique(c(a_publier, f_json))
infractions <- rs_controle_licence(
  json     = setNames(lapply(f_json, lire_json), f_json),
  obs      = lire_csv("data/db/observations.csv"),
  series   = lire_csv("data/db/series.csv"),
  hist     = lire_csv("data/reference/tsr20_hebdo.csv"),
  fichiers = fichiers[grepl("\\.(json|csv|md|txt)$", fichiers)],
  valeurs  = valeurs,
  publier_licence = FALSE)

cat("   Fichiers inspectes :", length(fichiers), "| valeurs de cotation recherchees :", length(valeurs), "\n")
if (length(infractions) > 0) {
  for (x in infractions) cat("::error::Licence : ", x, "\n", sep = "")
  cat("Publication arretee : des valeurs sous licence figurent dans des fichiers publics.\n")
  quit(status = 1)
}
cat("Conforme : aucune valeur sous licence dans les fichiers a publier.\n")
