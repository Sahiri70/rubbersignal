# =============================================================
# RUBBERSIGNAL.COM — Script 02 : Collecte des actualités
# Auteur  : Martial Sahiri
# Version : 2.2 — corrigé
# Objectif: Récupérer chaque semaine les actualités
#           caoutchouc / Côte d'Ivoire / TSR20 via NewsAPI
# Prérequis: Script 01 déjà exécuté (JSON intermédiaire existe)
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

if (NEWSAPI_KEY == "") {
  stop(paste(
    "ERREUR : Clé NewsAPI introuvable.",
    "Vérifiez votre fichier .Renviron et redémarrez RStudio.",
    "La ligne doit être : NEWSAPI_KEY=votre_cle_ici"
  ))
}

cat("=== RUBBERSIGNAL — Actualités du", format(DATE_COLLECTE, "%d/%m/%Y"), "===\n\n")
cat(">> Clé API détectée — OK\n\n")


# ── 3. FONCTION : APPEL NEWSAPI ──────────────────────────────

appeler_newsapi <- function(mots_cles, langue = "fr", nb_articles = 10) {
  
  date_debut <- format(DATE_COLLECTE - days(7), "%Y-%m-%d")
  
  url_api <- paste0(
    "https://newsapi.org/v2/everything?",
    "q=",          URLencode(mots_cles),
    "&language=",  langue,
    "&from=",      date_debut,
    "&sortBy=relevancy",
    "&pageSize=",  nb_articles,
    "&apiKey=",    NEWSAPI_KEY
  )
  
  reponse <- tryCatch({
    GET(url_api, timeout(15))
  }, error = function(e) {
    cat("   ATTENTION : NewsAPI inaccessible :", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(reponse)) {
    cat("   ATTENTION : connexion impossible\n")
    return(tibble())
  }
  
  if (status_code(reponse) != 200) {
    cat("   ATTENTION : Erreur HTTP", status_code(reponse), "\n")
    return(tibble())
  }
  
  contenu <- content(reponse, as = "text", encoding = "UTF-8")
  donnees <- fromJSON(contenu, flatten = TRUE)
  
  articles_bruts <- donnees$articles
  if (is.null(articles_bruts)) {
    cat("   Aucun article trouvé pour :", mots_cles, "\n")
    return(tibble())
  }
  
  articles_df <- tryCatch({
    as.data.frame(articles_bruts, stringsAsFactors = FALSE)
  }, error = function(e) {
    cat("   Erreur conversion articles :", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(articles_df) || nrow(articles_df) == 0) {
    cat("   Aucun article exploitable pour :", mots_cles, "\n")
    return(tibble())
  }
  
  articles <- articles_df %>%
    as_tibble() %>%
    select(any_of(c("title", "description", "url",
                    "publishedAt", "source.name"))) %>%
    rename_with(~ case_when(
      . == "title"       ~ "titre",
      . == "description" ~ "resume",
      . == "url"         ~ "lien",
      . == "publishedAt" ~ "date_publication",
      . == "source.name" ~ "source",
      TRUE               ~ .
    )) %>%
    mutate(
      date_publication = as_datetime(date_publication),
      date_fr          = format(date_publication, "%d/%m/%Y"),
      mots_cles_query  = mots_cles,
      resume           = str_trunc(
        if_else(is.na(resume), "", resume), 300)
    ) %>%
    arrange(desc(date_publication))
  
  return(articles)
}


# ── 4. FONCTION : AJOUT CATÉGORIE SÉCURISÉ ───────────────────

ajouter_categorie <- function(df, nom) {
  if (nrow(df) > 0) {
    df %>% mutate(categorie = nom)
  } else {
    tibble()
  }
}


# ── 5. FONCTION : FILTRE DE PERTINENCE ───────────────────────
# ── CORRECTION : appliqué systématiquement sur TOUTES les recherches ──

filtrer_pertinence <- function(df) {
  if (nrow(df) == 0) return(df)
  
  mots_obligatoires <- c(
    "rubber", "caoutchouc", "hévéa", "hevea",
    "TSR", "latex", "RSS", "elastomer",
    "plantation", "Michelin", "Bridgestone",
    "ANRPC", "APROCAG", "SGX"
  )
  
  pattern <- paste(tolower(mots_obligatoires), collapse = "|")
  
  df %>%
    filter(
      str_detect(tolower(titre),  pattern) |
        str_detect(tolower(resume), pattern)
    )
}


# ── 6. RECHERCHE 1 : MARCHÉ MONDIAL ──────────────────────────

cat(">> Recherche 1 : Marché caoutchouc mondial (EN)...\n")

actu_marche_en <- appeler_newsapi(
  mots_cles   = "\"natural rubber\" OR \"TSR20\" price market 2026",
  langue      = "en",
  nb_articles = 8
) %>% filtrer_pertinence()

if (nrow(actu_marche_en) > 0) {
  cat("   OK —", nrow(actu_marche_en), "articles trouvés\n")
  cat("   Plus récent :", actu_marche_en$titre[1], "\n")
} else {
  cat("   Aucun résultat\n")
}


# ── 7. RECHERCHE 2 : CÔTE D'IVOIRE ───────────────────────────

cat("\n>> Recherche 2 : Caoutchouc Côte d'Ivoire (FR)...\n")

actu_ci_fr <- appeler_newsapi(
  mots_cles   = "\"caoutchouc\" OR \"hévéa\" \"Côte d'Ivoire\"",
  langue      = "fr",
  nb_articles = 8
) %>% filtrer_pertinence()

if (nrow(actu_ci_fr) > 0) {
  cat("   OK —", nrow(actu_ci_fr), "articles trouvés\n")
  cat("   Plus récent :", actu_ci_fr$titre[1], "\n")
} else {
  cat("   Aucun résultat FR — tentative EN...\n")
  # ── CORRECTION : filtrer_pertinence appliqué aussi sur le secours ──
  actu_ci_fr <- appeler_newsapi(
    mots_cles   = "\"natural rubber\" \"Ivory Coast\" OR \"Cote d'Ivoire\"",
    langue      = "en",
    nb_articles = 5
  ) %>% filtrer_pertinence()
  cat("   Résultats EN :", nrow(actu_ci_fr), "articles\n")
}


# ── 8. RECHERCHE 3 : AFRIQUE DE L'OUEST ──────────────────────

cat("\n>> Recherche 3 : Afrique de l'Ouest (EN)...\n")

actu_afrique <- appeler_newsapi(
  mots_cles   = "\"natural rubber\" OR \"rubber production\" Africa",
  langue      = "en",
  nb_articles = 5
) %>% filtrer_pertinence()

if (nrow(actu_afrique) > 0) {
  cat("   OK —", nrow(actu_afrique), "articles trouvés\n")
} else {
  cat("   Aucun résultat\n")
}


# ── 9. RECHERCHE 4 : AFRIQUE ÉLARGIE ─────────────────────────

cat("\n>> Recherche 4 : Afrique élargie — Ghana, Nigeria, Cameroun (EN)...\n")

actu_afrique_elargie <- appeler_newsapi(
  mots_cles   = "\"natural rubber\" Ghana Nigeria Cameroon",
  langue      = "en",
  nb_articles = 5
) %>% filtrer_pertinence()

if (nrow(actu_afrique_elargie) > 0) {
  cat("   OK —", nrow(actu_afrique_elargie), "articles trouvés\n")
} else {
  cat("   Aucun résultat\n")
}


# ── 10. RECHERCHE 5 : ASIE ───────────────────────────────────

cat("\n>> Recherche 5 : Asie — Malaisie / Thaïlande (EN)...\n")

actu_asie <- appeler_newsapi(
  mots_cles   = "\"natural rubber\" Malaysia Thailand production export",
  langue      = "en",
  nb_articles = 5
) %>% filtrer_pertinence()

if (nrow(actu_asie) > 0) {
  cat("   OK —", nrow(actu_asie), "articles trouvés\n")
} else {
  cat("   Aucun résultat\n")
}


# ── 11. RECHERCHE 6 : EUROPE ─────────────────────────────────

cat("\n>> Recherche 6 : Europe — demande importateurs (FR)...\n")

actu_europe <- appeler_newsapi(
  mots_cles   = "\"caoutchouc naturel\" Europe importation",
  langue      = "fr",
  nb_articles = 4
) %>% filtrer_pertinence()

if (nrow(actu_europe) > 0) {
  cat("   OK —", nrow(actu_europe), "articles trouvés\n")
} else {
  cat("   Tentative EN...\n")
  actu_europe <- appeler_newsapi(
    mots_cles   = "\"natural rubber\" Europe import demand",
    langue      = "en",
    nb_articles = 4
  ) %>% filtrer_pertinence()
  cat("   Résultats EN :", nrow(actu_europe), "articles\n")
}


# ── 12. RECHERCHE 7 : INDUSTRIE PNEUMATIQUES ─────────────────

cat("\n>> Recherche 7 : Industrie pneumatiques (EN)...\n")

actu_pneus <- appeler_newsapi(
  mots_cles   = "\"natural rubber\" tire tyre Michelin Bridgestone demand",
  langue      = "en",
  nb_articles = 5
) %>% filtrer_pertinence()

if (nrow(actu_pneus) > 0) {
  cat("   OK —", nrow(actu_pneus), "articles trouvés\n")
} else {
  cat("   Aucun résultat\n")
}


# ── 13. ASSEMBLER TOUTES LES ACTUALITÉS ──────────────────────

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
    pwalk(~ cat("   ", ..1, ":", ..2, "articles\n"))
} else {
  cat("   Aucun article pertinent collecté cette semaine\n")
}


# ── 14. SÉLECTION ÉDITORIALE AUTOMATIQUE ─────────────────────

mots_cles_forts <- c("price", "prix", "market", "marché", "production",
                     "export", "demand", "TSR", "rubber", "caoutchouc",
                     "Ivory", "Ivoire", "ANRPC", "hévéa")

if (nrow(tous_articles) > 0) {
  
  articles_scores <- tous_articles %>%
    mutate(
      score_titre = map_int(titre, function(t) {
        sum(str_detect(tolower(t), tolower(mots_cles_forts)))
      }),
      score_recence = if_else(
        date_publication >= DATE_COLLECTE - days(3), 2L, 1L
      ),
      score_ci    = if_else(categorie == "Côte d'Ivoire", 3L, 0L),
      score_total = score_titre + score_recence + score_ci
    ) %>%
    arrange(desc(score_total))
  
  top_articles <- head(articles_scores, 3)
  
  cat("\n>> Top 3 articles recommandés pour la newsletter :\n")
  for (i in seq_len(nrow(top_articles))) {
    cat("  ", paste0(i, ")"),
        "[", top_articles$categorie[i], "]",
        top_articles$titre[i],
        "\n     Score :", top_articles$score_total[i],
        "| Date :", top_articles$date_fr[i], "\n\n")
  }
  
} else {
  top_articles <- tibble()
  cat("   Aucun article disponible — note éditoriale manuelle cette semaine\n")
}


# ── 15. SAUVEGARDER LES ACTUALITÉS ───────────────────────────

cat(">> Sauvegarde des actualités...\n")

if (nrow(tous_articles) > 0) {
  fichier_actu <- paste0("data/raw/actualites_", ANNEE, "_S", SEMAINE, ".csv")
  write_csv(tous_articles, fichier_actu)
  cat("   Sauvegardé :", fichier_actu, "\n")
}


# ── 16. ENRICHIR LE JSON DU SCRIPT 01 ────────────────────────

cat("\n>> Enrichissement du JSON intermédiaire...\n")

fichier_json <- paste0("data/processed/rubbersignal_S", SEMAINE, "_", ANNEE, ".json")

if (file.exists(fichier_json)) {
  
  json_existant <- read_json(fichier_json)
  
  json_existant$actualites <- list(
    date_collecte  = as.character(DATE_COLLECTE),
    total_articles = nrow(tous_articles),
    top_newsletter = if (nrow(top_articles) > 0)
      top_articles %>%
      select(titre, resume, lien, date_fr,
             source, categorie) %>%
      as.list() %>% transpose()
    else list(),
    tous_articles  = if (nrow(tous_articles) > 0)
      tous_articles %>%
      select(titre, resume, lien, date_fr,
             source, categorie) %>%
      as.list() %>% transpose()
    else list()
  )
  
  write_json(json_existant, fichier_json, pretty = TRUE, auto_unbox = TRUE)
  cat("   JSON enrichi :", fichier_json, "\n")
  
} else {
  cat("   ATTENTION : JSON du script 01 introuvable !\n")
  cat("   Relancez scripts/01_collect_prices.R d'abord.\n")
}


# ── 17. RÉSUMÉ FINAL ─────────────────────────────────────────

cat("\n", strrep("=", 50), "\n")
cat("RÉSUMÉ ACTUALITÉS — Semaine", SEMAINE, "/", ANNEE, "\n")
cat(strrep("=", 50), "\n")
cat("Articles pertinents :", nrow(tous_articles), "\n")
cat("Top newsletter      :", nrow(top_articles), "articles sélectionnés\n")
cat("JSON enrichi        :", fichier_json, "\n")
cat(strrep("=", 50), "\n")
cat("\nProchaine étape : lancer scripts/03_build_report.R\n\n")
