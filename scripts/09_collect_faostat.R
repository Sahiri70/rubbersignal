# =============================================================
# RUBBERSIGNAL TECHNOLOGIES — Script 09 : Production FAOSTAT (annuelle)
# Version : 1.0 (18/11/2026)
# Objectif: production, superficie récoltée et rendement du caoutchouc naturel
#           (FAOSTAT, produit 836) pour les producteurs africains, les grands
#           producteurs asiatiques et les totaux monde, Afrique, Asie.
#           Règles : 00_regles.R, section 11 ter. Licence CC BY 4.0 (FAO).
# Entrées : fichiers en vrac FAOSTAT (bulks-faostat.fao.org), sans clé.
# Sorties : data/raw/faostat_caoutchouc_AAAAMMJJ.csv (extrait, quelques Ko)
#           data/db/observations.csv (ajout idempotent, révisions détectées)
# Fréquence : FAO publie une fois par an. Le script lit la date de mise à jour
#           du fichier (en-tête HTTP) et ne télécharge rien si elle est déjà en base.
# Échec   : jamais bloquant pour la semaine (valeurs déjà en base conservées).
# Usage   : Rscript scripts/09_collect_faostat.R
# =============================================================

suppressPackageStartupMessages({ library(httr) })
source(if (file.exists("scripts/00_sources.R")) "scripts/00_sources.R" else "00_sources.R")   # + 00_regles.R
source(if (file.exists("scripts/00_base.R")) "scripts/00_base.R" else "00_base.R")

DATE_COLLECTE <- as.Date(Sys.getenv("RUBBERSIGNAL_TODAY", as.character(Sys.Date())))
rs_db_init()
cat("=== RUBBERSIGNAL — Production FAOSTAT du", format(DATE_COLLECTE, "%d/%m/%Y"), "===\n\n")

ua <- user_agent(RS_UA)
date_fao <- function(fichier) {
  r <- tryCatch(HEAD(sprintf(RS_FAO_URL, fichier), ua, timeout(30)), error = function(e) NULL)
  lm <- if (!is.null(r) && status_code(r) == 200) headers(r)[["last-modified"]] else NULL
  if (is.null(lm)) return(NA_character_)
  d <- suppressWarnings(as.Date(strptime(lm, "%a, %d %b %Y %H:%M:%S", tz = "GMT")))
  if (is.na(d)) { loc <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
                  d <- as.Date(strptime(lm, "%a, %d %b %Y %H:%M:%S", tz = "GMT")); Sys.setlocale("LC_TIME", loc) }
  as.character(d)
}

maj <- vapply(RS_FAO_FICHIERS, date_fao, character(1))
obs <- rs_db_lire_obs()
deja <- obs$date_source[startsWith(obs$serie_id, "FAO_")]
derniere_connue <- if (length(deja)) max(deja, na.rm = TRUE) else NA_character_
cat(">> Mise à jour FAO :", paste(names(maj), maj, sep = " = ", collapse = " ; "), "\n")
cat(">> Dernière mise à jour en base :", derniere_connue %||% "aucune", "\n")

if (all(is.na(maj))) {
  cat("::warning::FAOSTAT indisponible cette semaine : valeurs annuelles déjà en base conservées\n")
} else if (!is.na(derniere_connue) && all(maj[!is.na(maj)] <= derniere_connue)) {
  cat(">> Aucune nouvelle publication FAO : rien à télécharger.\n")
} else {
  dl <- file.path(tempdir(), "faostat"); dir.create(dl, showWarnings = FALSE)
  lus <- list()
  for (f in RS_FAO_FICHIERS) {
    z <- file.path(dl, paste0(f, ".zip"))
    ok <- tryCatch({ r <- RETRY("GET", sprintf(RS_FAO_URL, f), ua, timeout(300), write_disk(z, overwrite = TRUE), times = 3, quiet = TRUE)
                     stop_for_status(r); TRUE }, error = function(e) { cat("::warning::FAOSTAT : ", f, " non téléchargé (", conditionMessage(e), ")\n", sep = ""); FALSE })
    if (!ok) next
    csv <- grep(paste0("^", f, "\\.csv$"), utils::unzip(z, list = TRUE)$Name, value = TRUE)
    if (length(csv) != 1) { cat("::warning::FAOSTAT : fichier CSV introuvable dans ", f, "\n", sep = ""); next }
    utils::unzip(z, files = csv, exdir = dl)
    lus[[f]] <- rs_fao_lire(file.path(dl, csv))
    cat(sprintf(">> %-45s %5d valeurs (caoutchouc naturel)\n", f, nrow(lus[[f]])))
  }
  d <- do.call(rbind, lus)
  if (!is.null(d) && nrow(d) > 0) {
    d <- d[!duplicated(d[, c("code", "element", "annee")]), , drop = FALSE]
    ds <- max(maj, na.rm = TRUE)
    dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
    f_ext <- sprintf("data/raw/faostat_caoutchouc_%s.csv", gsub("-", "", ds))
    con <- file(f_ext, open = "wb"); utils::write.csv(d, con, row.names = FALSE, fileEncoding = "UTF-8"); close(con)
    lot <- rs_db_obs(rs_fao_id(d$element, d$code), d$annee, d$valeur, ds, as.character(DATE_COLLECTE),
                     methode = paste0("FAOSTAT QCL, produit 836, drapeau ", ifelse(nzchar(d$flag), d$flag, "A"), " (",
                                      ifelse(nzchar(d$flag), RS_FAO_FLAGS[d$flag], RS_FAO_FLAGS[["A"]]), ")"),
                     nature = rs_fao_nature(d$flag), url_source = RS_FAO_PAGE)
    res <- rs_db_ajouter(lot)
    cat("\n>> Base data/db/ : ", res$n_ajoutees, " ligne(s) ajoutée(s), ", res$n_identiques, " déjà connue(s), ",
        res$n_revisions, " révision(s) (historique conservé)\n", sep = "")
    cat(">> Extrait :", f_ext, "\n")
    der <- d[d$element == "PROD" & d$annee == max(d$annee), ]
    der <- der[order(-der$valeur), ]
    cat("\nProduction", max(d$annee), ":\n")
    for (i in seq_len(min(12, nrow(der)))) cat(sprintf("  %-14s %12s t  [%s]\n", RS_FAO_PAYS$nom_fr[match(der$code[i], RS_FAO_PAYS$code)],
                                                   format(round(der$valeur[i]), big.mark = " "), der$flag[i]))
  } else cat("::warning::FAOSTAT : aucune valeur lue\n")
}
cat("\n")
