# =============================================================
# RUBBERSIGNAL TECHNOLOGIES — Script 02 : Collecte des actualités
# Auteur  : Martial Sahiri
# Version : 4.0 (18/11/2026) — flux RSS gratuits à la place de NewsAPI
# Objectif: titres de la semaine sur le caoutchouc naturel (Côte d'Ivoire,
#           Afrique, Asie, EUDR, industrie), en français et en anglais.
# Sources : data/reference/sources_actualites.csv (Google News RSS, flux de
#           recherche d'éditeurs) . Aucune clé.
#           Règles : scripts/00_actualites.R.
# Sorties : data/raw/actualites_AAAA_SNN.csv (titres, éditeurs, liens, dates)
#           bloc `actualites` du JSON de la semaine (lu par 03, 04 et le site)
# Échec   : jamais bloquant. Sans article : statut « indisponible » avec la
#           raison ; la composante géopolitique du Pré-RSI est alors retirée.
# Usage   : Rscript scripts/02_collect_news.R   (après 01)
# =============================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(lubridate)
})

source(if (file.exists("scripts/00_sources.R")) "scripts/00_sources.R" else "00_sources.R")   # + 00_regles.R, rs_get
source(if (file.exists("scripts/00_actualites.R")) "scripts/00_actualites.R" else "00_actualites.R")

DATE_COLLECTE <- as.Date(Sys.getenv("RUBBERSIGNAL_TODAY", as.character(Sys.Date())))
SEMAINE       <- isoweek(DATE_COLLECTE)
ANNEE         <- year(DATE_COLLECTE)
fichier_json  <- paste0("data/processed/rubbersignal_S", SEMAINE, "_", ANNEE, ".json")

cat("=== RUBBERSIGNAL — Actualités du", format(DATE_COLLECTE, "%d/%m/%Y"), "===\n\n")


# ── 1. FLUX RSS (une source à la fois ; une panne n'arrête rien) ──

sources <- rs_actu_sources()
liste <- list(); DIAG <- list(); dernier_type <- ""
for (i in seq_len(nrow(sources))) {
  s <- sources[i, ]
  if (identical(s$type, dernier_type) && s$type == "google_news") Sys.sleep(RS_ACTU_PAUSE_S)   # politesse envers l'agrégateur
  dernier_type <- s$type
  r <- rs_actu_telecharger(s)
  DIAG[[length(DIAG) + 1]] <- r$diag
  liste[[s$id]] <- r$articles
  cat(sprintf(">> %-15s %-7s %3d titres%s\n", s$id, r$diag$statut, r$diag$items,
              if (!is.na(r$diag$message)) paste0(" (", r$diag$message, ")") else ""))
}


# NewsAPI n'est plus utilisé (18/11/2026) : son offre gratuite interdit l'usage en
# production, et les flux RSS couvrent le français et l'anglais. La clé
# NEWSAPI_KEY peut être révoquée et retirée des secrets GitHub.


# ── 2. ASSEMBLAGE : semaine, pertinence, doublons, catégories ──

res <- rs_actu_assembler(liste, DATE_COLLECTE)
art <- res$articles
n   <- if (is.null(art)) 0L else nrow(art)
cat("\n>> ", res$n_bruts, " titres reçus, ", res$n_fenetre, " de la semaine, ", n, " retenus (filtre caoutchouc, sans doublon)\n", sep = "")
if (n > 0) print(table(vapply(art$categorie, rs_actu_categorie_libelle, character(1))))

n_ok <- sum(vapply(DIAG, function(d) identical(d$statut, "ok"), logical(1)))
raison <- if (n > 0) NA_character_ else if (n_ok == 0) "aucune source d'actualites n'a repondu" else
  if (res$n_fenetre == 0) "aucun titre publie cette semaine dans les sources" else
  paste0(res$n_fenetre, " titres de la semaine recus, aucun ne concerne le caoutchouc naturel")
if (!is.na(raison)) cat("::warning::Actualites indisponibles : ", raison, "\n", sep = "")
for (d in DIAG) if (identical(d$statut, "erreur"))
  cat("::warning::Actualites : source ", d$id, " en erreur (", d$message, ")\n", sep = "")


# ── 3. ANNONCES DE PRIX APROMAC (à vérifier, jamais saisies) ──

tous_bruts <- do.call(rbind, Filter(function(d) !is.null(d) && nrow(d) > 0, liste))
recents <- if (is.null(tous_bruts)) NULL else
  tous_bruts[!is.na(tous_bruts$date) & as.Date(tous_bruts$date) > DATE_COLLECTE - RS_ACTU_FENETRE_PRIX_J, , drop = FALSE]
saisie <- tryCatch(utils::read.csv("data/reference/prix_apromac.csv", colClasses = "character", encoding = "UTF-8"),
                   error = function(e) data.frame(mois = character(), prix_fcfa_kg = character()))
historique <- tryCatch(utils::read.csv("data/reference/apromac_history.csv", stringsAsFactors = FALSE),
                       error = function(e) NULL)
annonces <- rs_actu_comparer_apromac(rs_actu_prix_apromac(recents), saisie, historique)
for (i in seq_len(nrow(annonces))) {
  a <- annonces[i, ]
  if (a$statut == "a_saisir")
    cat("::warning::APROMAC : prix de ", a$mois, " annonce dans la presse (", a$prix_fcfa, " FCFA/kg, ", a$source,
        ", ", a$date_pub, ") - a verifier puis saisir dans data/reference/prix_apromac.csv\n", sep = "")
  else if (a$statut == "ecart")
    cat("::warning::APROMAC : ", a$mois, " annonce a ", a$prix_fcfa, " FCFA/kg dans la presse, valeur connue ", a$prix_saisi,
        " FCFA/kg - a verifier\n", sep = "")
  else cat(">> APROMAC ", a$mois, " : ", a$prix_fcfa, " FCFA/kg dans la presse, conforme a la saisie\n", sep = "")
}


# ── 4. SAUVEGARDE ET JSON ────────────────────────────────────

fiche <- function(i) list(
  titre = art$titre[i], source = art$source[i], lien = art$lien[i],
  date = format(art$date[i], "%Y-%m-%d"), date_fr = format(art$date[i], "%d/%m/%Y"),
  categorie = art$categorie[i], categorie_fr = rs_actu_categorie_libelle(art$categorie[i], "fr"),
  categorie_en = rs_actu_categorie_libelle(art$categorie[i], "en"), tension = isTRUE(art$tension[i]),
  origine = art$origine[i])

if (n > 0) {
  dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
  f_csv <- sprintf("data/raw/actualites_%d_S%02d.csv", ANNEE, SEMAINE)
  sortie <- art[, c("date", "categorie", "titre", "source", "lien", "origine", "tension")]
  sortie$date <- format(sortie$date, "%Y-%m-%d %H:%M")
  con <- file(f_csv, open = "wb"); utils::write.csv(sortie, con, row.names = FALSE, fileEncoding = "UTF-8"); close(con)
  cat(">> Sauvegarde :", f_csv, "\n")
}

if (file.exists(fichier_json)) {
  jp <- read_json(fichier_json)
  jp$actualites <- list(
    date_collecte = as.character(DATE_COLLECTE),
    statut = if (n > 0) "ok" else "indisponible",
    raison = raison,
    version = "Actualités v2 (flux RSS, 18/11/2026)",
    source = "Flux RSS : Google News (titres et liens vers les éditeurs) et flux de recherche d'éditeurs",
    fenetre_jours = RS_ACTU_FENETRE_J,
    sources = DIAG,
    n_bruts = res$n_bruts, n_semaine = res$n_fenetre, total_articles = n,
    n_tension = if (n > 0) sum(art$tension) else NA_integer_,
    top_newsletter = if (n > 0) lapply(seq_len(min(5, n)), fiche) else list(),
    tous_articles = if (n > 0) lapply(seq_len(min(30, n)), fiche) else list(),
    annonces_apromac = if (nrow(annonces) > 0) lapply(seq_len(nrow(annonces)), function(i) as.list(annonces[i, ])) else list(),
    regle = "Titre, éditeur, date et lien seulement ; aucun texte d'article recopié. Filtre : caoutchouc naturel.")
  write_json(jp, fichier_json, pretty = TRUE, auto_unbox = TRUE, na = "null", null = "null")
  cat(">> JSON enrichi :", fichier_json, "\n")
} else cat("   ATTENTION : JSON du script 01 introuvable - relancer 01 d'abord.\n")


# ── 5. RÉSUMÉ ────────────────────────────────────────────────

cat("\n", strrep("=", 50), "\n", sep = "")
cat("ACTUALITÉS — Semaine", SEMAINE, "/", ANNEE, "\n")
cat("Sources qui répondent :", n_ok, "/", length(DIAG), "\n")
cat("Articles retenus      :", n, if (n > 0) paste0("(dont ", sum(art$tension), " sur des tensions)") else "", "\n")
if (n > 0) for (i in seq_len(min(5, n))) cat(sprintf("  %d. [%s] %s — %s\n", i, rs_actu_categorie_libelle(art$categorie[i]),
                                                substr(art$titre[i], 1, 80), art$source[i]))
cat("Annonces APROMAC      :", nrow(annonces), "\n")
cat(strrep("=", 50), "\n\n")
