# =============================================================
# RUBBERSIGNAL.COM — Script 02 : Collecte des actualités
# Auteur  : Martial Sahiri
# Version : 3.0 — réécrit complet
# Objectif: Récupérer les actualités caoutchouc via NewsAPI
# Prérequis: Script 01 déjà exécuté
# Usage   : source("scripts/02_collect_news.R")
# =============================================================

# ── 1. CHARGER LES PACKAGES ──────────────────────────────────

library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)


# ── 2. PARAMÈTRES GLOBAUX ────────────────────────────────────

DATE_COLLECTE <- Sys.Date()
SEMAINE       <- isoweek(DATE_COLLECTE)
ANNEE         <- year(DATE_COLLECTE)

NEWSAPI_KEY <- Sys.getenv("NEWSAPI_KEY")

if (nchar(NEWSAPI_KEY) == 0) {
  stop(paste(
    "ERREUR : Clé NewsAPI introuvable.",
    "Vérifiez votre fichier .Renviron.",
    "La ligne doit être : NEWSAPI_KEY=votre_cle_ici"
  ))
}

cat("=== RUBBERSIGNAL — Actualités du", format(DATE_COLLECTE, "%d/%m/%Y"), "===\n\n")
cat(">> Clé API détectée — OK\n\n")


# ── 3. FONCTION : APPEL NEWSAPI ──────────────────────────────
# Retourne toujours un tibble avec colonnes standardisées
# ou un tibble vide en cas d'erreur — jamais une erreur fatale

# Diagnostic de chaque appel (v3.1) : code HTTP, code d'erreur NewsAPI,
# nombre de resultats bruts. Jamais la cle (elle passe en en-tete HTTP,
# plus dans l'URL, pour ne pas apparaitre dans un message d'erreur).
DIAG_NEWS <- list()
diag_news <- function(requete, langue, http, code_api = NA, message_api = NA, total = NA, recus = 0) {
  if (!is.na(message_api)) message_api <- gsub(NEWSAPI_KEY, "****", message_api, fixed = TRUE)
  DIAG_NEWS[[length(DIAG_NEWS) + 1]] <<- list(
    requete = requete, langue = langue, http = http, code_api = code_api,
    message_api = message_api, total_resultats = total, articles_recus = recus)
}

appeler_newsapi <- function(mots_cles, langue = "en", nb_articles = 8) {

  date_debut <- format(DATE_COLLECTE - days(7), "%Y-%m-%d")

  # Requête HTTP avec gestion d'erreur (clé en en-tête X-Api-Key)
  reponse <- tryCatch(
    GET("https://newsapi.org/v2/everything",
        query = list(q = mots_cles, language = langue, from = date_debut,
                     sortBy = "relevancy", pageSize = nb_articles),
        add_headers(`X-Api-Key` = NEWSAPI_KEY),
        timeout(20)),
    error = function(e) {
      cat("   Connexion impossible :", gsub(NEWSAPI_KEY, "****", conditionMessage(e), fixed = TRUE), "\n")
      NULL
    }
  )

  # Retourner tibble vide si connexion échouée
  if (is.null(reponse)) {
    diag_news(mots_cles, langue, http = NA, code_api = "connexion_impossible")
    return(tibble())
  }

  # Parser le JSON (aussi en cas d'erreur : NewsAPI renvoie code + message)
  code_http <- status_code(reponse)
  contenu <- content(reponse, as = "text", encoding = "UTF-8")
  donnees <- tryCatch(
    fromJSON(contenu, flatten = TRUE),
    error = function(e) {
      cat("   Erreur parsing JSON :", conditionMessage(e), "\n")
      NULL
    }
  )

  # Vérifier le code HTTP
  if (code_http != 200) {
    cat("   Erreur HTTP", code_http, "-", donnees$code %||% "", "\n")
    cat("::warning::NewsAPI HTTP ", code_http, " ", donnees$code %||% "", "\n", sep = "")
    diag_news(mots_cles, langue, http = code_http, code_api = donnees$code %||% NA,
              message_api = donnees$message %||% NA)
    return(tibble())
  }

  diag_news(mots_cles, langue, http = code_http, code_api = donnees$status %||% NA,
            total = donnees$totalResults %||% NA,
            recus = if (is.data.frame(donnees$articles)) nrow(donnees$articles) else 0)

  if (is.null(donnees) || is.null(donnees$articles)) return(tibble())
  if (!is.data.frame(donnees$articles)) return(tibble())
  if (nrow(donnees$articles) == 0) return(tibble())
  
  # Construire tibble standardisé
  # Utiliser les noms de colonnes réels de NewsAPI
  articles_df <- as_tibble(donnees$articles)
  
  # Colonnes attendues de NewsAPI (flatten = TRUE)
  col_mapping <- c(
    "title"       = "titre",
    "description" = "resume",
    "url"         = "lien",
    "publishedAt" = "date_publication",
    "source.name" = "source"
  )
  
  # Sélectionner uniquement les colonnes présentes
  cols_presentes <- intersect(names(col_mapping), names(articles_df))
  
  if (length(cols_presentes) == 0) {
    cat("   Structure inattendue — colonnes :", paste(names(articles_df), collapse = ", "), "\n")
    return(tibble())
  }
  
  articles <- articles_df %>%
    select(all_of(cols_presentes)) %>%
    rename(!!!setNames(cols_presentes, col_mapping[cols_presentes])) %>%
    mutate(
      date_publication = if ("date_publication" %in% names(.))
        as_datetime(date_publication)
      else
        as_datetime(Sys.time()),
      date_fr          = format(date_publication, "%d/%m/%Y"),
      resume           = if ("resume" %in% names(.))
        str_trunc(replace_na(resume, ""), 280)
      else "",
      lien             = if ("lien" %in% names(.)) lien else "",
      source           = if ("source" %in% names(.)) source else "—"
    ) %>%
    arrange(desc(date_publication))
  
  return(articles)
}


# ── 4. FONCTION : AJOUT CATÉGORIE SÉCURISÉ ───────────────────

ajouter_categorie <- function(df, nom) {
  if (is.null(df) || nrow(df) == 0) return(tibble())
  mutate(df, categorie = nom)
}


# ── 5. FONCTION : FILTRE DE PERTINENCE ───────────────────────

filtrer_pertinence <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(tibble())
  if (!("titre" %in% names(df))) return(tibble())
  
  mots <- c("rubber", "caoutchouc", "hévéa", "hevea", "TSR",
            "latex", "RSS", "elastomer", "plantation",
            "Michelin", "Bridgestone", "ANRPC", "APROMAC", "SGX")
  
  pattern <- paste(tolower(mots), collapse = "|")
  
  # Utiliser base R pour éviter les problèmes de scope dplyr
  titres  <- tolower(df$titre)
  resumes <- tolower(replace_na(df$resume, ""))
  
  idx <- grepl(pattern, titres) | grepl(pattern, resumes)
  df[idx, ]
}


# ── 6. FONCTION : AFFICHER RÉSULTAT RECHERCHE ────────────────
# Fonction utilitaire pour éviter la répétition du code d'affichage

afficher_resultat <- function(df, label) {
  n <- nrow(df)
  if (n > 0) {
    cat("   OK —", n, "articles pertinents\n")
    if ("titre" %in% names(df)) {
      cat("   Plus récent :", str_trunc(df$titre[1], 60), "\n")
    }
  } else {
    cat("   Aucun résultat pertinent\n")
  }
}


# ── 7. RECHERCHES NEWSAPI ────────────────────────────────────

cat(">> Recherche 1 : Marché mondial (EN)...\n")
actu_marche_en <- appeler_newsapi(
  "natural rubber TSR20 price market",
  langue = "en", nb_articles = 8
) %>% filtrer_pertinence()
afficher_resultat(actu_marche_en, "Marché mondial")

cat("\n>> Recherche 2 : Côte d'Ivoire (FR)...\n")
actu_ci_fr <- appeler_newsapi(
  "caoutchouc hévéa Côte Ivoire",
  langue = "fr", nb_articles = 8
) %>% filtrer_pertinence()

if (nrow(actu_ci_fr) == 0) {
  cat("   Aucun résultat FR — tentative EN...\n")
  actu_ci_fr <- appeler_newsapi(
    "natural rubber Ivory Coast Cote Ivoire",
    langue = "en", nb_articles = 5
  ) %>% filtrer_pertinence()
}
afficher_resultat(actu_ci_fr, "Côte d'Ivoire")

cat("\n>> Recherche 3 : Afrique de l'Ouest (EN)...\n")
actu_afrique <- appeler_newsapi(
  "natural rubber Africa production export",
  langue = "en", nb_articles = 5
) %>% filtrer_pertinence()
afficher_resultat(actu_afrique, "Afrique de l'Ouest")

cat("\n>> Recherche 4 : Afrique élargie (EN)...\n")
actu_afrique_elargie <- appeler_newsapi(
  "natural rubber Ghana Nigeria Cameroon",
  langue = "en", nb_articles = 5
) %>% filtrer_pertinence()
afficher_resultat(actu_afrique_elargie, "Afrique élargie")

cat("\n>> Recherche 5 : Asie (EN)...\n")
actu_asie <- appeler_newsapi(
  "natural rubber Malaysia Thailand production",
  langue = "en", nb_articles = 5
) %>% filtrer_pertinence()
afficher_resultat(actu_asie, "Asie")

cat("\n>> Recherche 6 : Europe (FR)...\n")
actu_europe <- appeler_newsapi(
  "caoutchouc naturel Europe importation",
  langue = "fr", nb_articles = 4
) %>% filtrer_pertinence()
if (nrow(actu_europe) == 0) {
  actu_europe <- appeler_newsapi(
    "natural rubber Europe import demand",
    langue = "en", nb_articles = 4
  ) %>% filtrer_pertinence()
}
afficher_resultat(actu_europe, "Europe")

cat("\n>> Recherche 7 : Industrie pneumatiques (EN)...\n")
actu_pneus <- appeler_newsapi(
  "natural rubber tire tyre Michelin Bridgestone",
  langue = "en", nb_articles = 5
) %>% filtrer_pertinence()
afficher_resultat(actu_pneus, "Industrie aval")


# ── 8. ASSEMBLER TOUTES LES ACTUALITÉS ──────────────────────

cat("\n>> Assemblage des actualités...\n")

tous_articles <- bind_rows(
  ajouter_categorie(actu_marche_en,       "Marché mondial"),
  ajouter_categorie(actu_ci_fr,           "Côte d'Ivoire"),
  ajouter_categorie(actu_afrique,         "Afrique de l'Ouest"),
  ajouter_categorie(actu_afrique_elargie, "Afrique élargie"),
  ajouter_categorie(actu_asie,            "Asie"),
  ajouter_categorie(actu_europe,          "Europe"),
  ajouter_categorie(actu_pneus,           "Industrie aval")
)

if (nrow(tous_articles) > 0 && "lien" %in% names(tous_articles)) {
  tous_articles <- tous_articles %>%
    distinct(lien, .keep_all = TRUE) %>%
    head(25) %>%
    arrange(categorie, desc(date_publication))
  cat("   Total articles uniques :", nrow(tous_articles), "\n")
  tous_articles %>%
    count(categorie) %>%
    pwalk(~ cat("  ", ..1, ":", ..2, "articles\n"))
} else {
  cat("   Aucun article pertinent collecté cette semaine\n")
  tous_articles <- tibble()
}


# ── 9. SÉLECTION ÉDITORIALE AUTOMATIQUE ──────────────────────

mots_forts <- c("price", "prix", "market", "marché", "production",
                "export", "demand", "TSR", "rubber", "caoutchouc",
                "Ivory", "Ivoire", "ANRPC", "hévéa")

if (nrow(tous_articles) > 0 && "titre" %in% names(tous_articles)) {
  
  articles_scores <- tous_articles %>%
    mutate(
      score_titre = map_int(titre, function(t) {
        sum(str_detect(tolower(t), tolower(mots_forts)))
      }),
      score_recence = if_else(
        date_publication >= DATE_COLLECTE - days(3), 2L, 1L
      ),
      score_ci    = if_else(categorie == "Côte d'Ivoire", 3L, 0L),
      score_total = score_titre + score_recence + score_ci
    ) %>%
    arrange(desc(score_total))
  
  top_articles <- head(articles_scores, 3)
  
  cat("\n>> Top", nrow(top_articles), "articles sélectionnés :\n")
  for (i in seq_len(nrow(top_articles))) {
    cat(" ", paste0(i, ")"),
        "[", top_articles$categorie[i], "]",
        str_trunc(top_articles$titre[i], 55),
        "\n     Score :", top_articles$score_total[i],
        "| Date :", top_articles$date_fr[i], "\n\n")
  }
  
} else {
  top_articles <- tibble()
  cat("   Aucun article — note éditoriale manuelle cette semaine\n")
}


# ── 10. SAUVEGARDER ───────────────────────────────────────────

cat(">> Sauvegarde des actualités...\n")

if (nrow(tous_articles) > 0) {
  fichier_actu <- paste0("data/raw/actualites_", ANNEE, "_S", SEMAINE, ".csv")
  write_csv(tous_articles, fichier_actu)
  cat("   Sauvegardé :", fichier_actu, "\n")
}


# ── 11. ENRICHIR LE JSON DU SCRIPT 01 ────────────────────────

cat("\n>> Enrichissement du JSON...\n")

fichier_json <- paste0("data/processed/rubbersignal_S", SEMAINE, "_", ANNEE, ".json")

if (file.exists(fichier_json)) {
  
  json_existant <- read_json(fichier_json)
  
  cols_json <- c("titre", "resume", "lien", "date_fr", "source", "categorie")

  # Statut explicite : le rapport affiche « indisponible » plutot qu'un vide
  https     <- vapply(DIAG_NEWS, function(d) as.character(d$http %||% NA), character(1))
  bruts     <- sum(vapply(DIAG_NEWS, function(d) as.numeric(d$articles_recus %||% 0), numeric(1)))
  raison_actu <- if (nrow(tous_articles) > 0) NA_character_
    else if (length(https) > 0 && all(is.na(https) | https != "200"))
      paste0("NewsAPI en erreur sur toutes les requetes (HTTP ", paste(unique(https), collapse = "/"), ")")
    else if (bruts == 0) "NewsAPI repond (HTTP 200) mais aucune requete ne renvoie d'article sur la periode"
    else paste0(bruts, " articles bruts recus, aucun retenu par le filtre de pertinence")
  if (!is.na(raison_actu)) cat("::warning::Actualites indisponibles : ", raison_actu, "\n", sep = "")

  json_existant$actualites <- list(
    date_collecte  = as.character(DATE_COLLECTE),
    statut         = if (nrow(tous_articles) > 0) "ok" else "indisponible",
    raison         = raison_actu,
    source         = "NewsAPI /v2/everything (7 derniers jours)",
    diagnostic     = DIAG_NEWS,
    total_articles = nrow(tous_articles),
    top_newsletter = if (nrow(top_articles) > 0)
      top_articles %>%
      select(any_of(cols_json)) %>%
      as.list() %>% transpose()
    else list(),
    tous_articles  = if (nrow(tous_articles) > 0)
      tous_articles %>%
      select(any_of(cols_json)) %>%
      as.list() %>% transpose()
    else list()
  )
  
  write_json(json_existant, fichier_json, pretty = TRUE, auto_unbox = TRUE,
             na = "null", null = "null")
  cat("   JSON enrichi :", fichier_json, "\n")
  
} else {
  cat("   ATTENTION : JSON script 01 introuvable — relancez 01 d'abord.\n")
}


# ── 12. RÉSUMÉ FINAL ─────────────────────────────────────────

cat("\n", strrep("=", 50), "\n")
cat("RÉSUMÉ ACTUALITÉS — Semaine", SEMAINE, "/", ANNEE, "\n")
cat(strrep("=", 50), "\n")
cat("Articles pertinents :", nrow(tous_articles), "\n")
cat("Top newsletter      :", nrow(top_articles), "sélectionnés\n")
cat("JSON enrichi        :", fichier_json, "\n")
cat(strrep("=", 50), "\n")
cat("\nProchaine étape : lancer scripts/03_build_report.R\n\n")
