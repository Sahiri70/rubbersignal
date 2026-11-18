# =============================================================
# RUBBERSIGNAL TECHNOLOGIES — Actualités : sources RSS, filtre, catégories
# Version : 1.0 (18/11/2026), remplace NewsAPI comme source principale
# Rôle    : fonctions pures (testables hors ligne) + téléchargement des flux.
#   - sources : data/reference/sources_actualites.csv (ajout sans code) ;
#   - chaque source est indépendante : une panne n'arrête pas les autres ;
#   - seuls le titre, l'éditeur, la date et le lien sont gardés (aucun texte
#     d'article recopié) ;
#   - filtre strict : un article doit parler du caoutchouc naturel
#     (« rubber duck », sandales, élastomères de labo… sont écartés) ;
#   - annonces de prix APROMAC détectées dans les titres : signalées « à
#     vérifier », JAMAIS saisies automatiquement (l'évaluateur saisit).
# Prérequis : 00_sources.R (rs_get, RS_UA) chargé pour le téléchargement.
# =============================================================

RS_ACTU_SOURCES <- "data/reference/sources_actualites.csv"
RS_ACTU_FENETRE_J <- 7          # articles de la semaine
RS_ACTU_FENETRE_PRIX_J <- 45    # annonces de prix : un mois et demi
RS_ACTU_PAUSE_S <- 3            # entre deux appels au même agrégateur

# ── 1. LECTURE D'UN FLUX RSS ─────────────────────────────────
# Retour : data.frame(titre, source, lien, date (POSIXct UTC), origine)
rs_actu_lire_rss <- function(txt, origine = NA_character_, nom_source = NA_character_) {
  vide <- data.frame(titre = character(), source = character(), lien = character(),
                     date = as.POSIXct(character(), tz = "UTC"), origine = character(), stringsAsFactors = FALSE)
  doc <- tryCatch(xml2::read_xml(txt), error = function(e) NULL)
  if (is.null(doc)) return(vide)
  it <- xml2::xml_find_all(doc, "//item")
  if (length(it) == 0) return(vide)
  champ <- function(x) { n <- xml2::xml_find_first(it, x); ifelse(is.na(n), NA_character_, xml2::xml_text(n)) }
  titre <- trimws(gsub("\\s+", " ", champ("title")))
  src   <- champ("source")                                 # Google News : nom de l'éditeur
  # Google News ajoute « - Éditeur » au titre : retiré quand l'éditeur est connu
  a_retirer <- !is.na(src) & nzchar(src) & endsWith(titre, paste0(" - ", src))
  titre[a_retirer] <- substr(titre[a_retirer], 1, nchar(titre[a_retirer]) - nchar(src[a_retirer]) - 3)
  src[is.na(src) | !nzchar(src)] <- nom_source
  date <- suppressWarnings(as.POSIXct(champ("pubDate"), format = "%a, %d %b %Y %H:%M:%S", tz = "UTC"))
  if (anyNA(date)) {   # noms de jours/mois anglais, quelle que soit la langue du système
    loc <- Sys.getlocale("LC_TIME"); on.exit(Sys.setlocale("LC_TIME", loc), add = TRUE)
    suppressWarnings(Sys.setlocale("LC_TIME", "C"))
    date <- suppressWarnings(as.POSIXct(champ("pubDate"), format = "%a, %d %b %Y %H:%M:%S", tz = "UTC"))
  }
  out <- data.frame(titre = titre, source = src, lien = trimws(champ("link")), date = date,
                    origine = origine, stringsAsFactors = FALSE)
  out[!is.na(out$titre) & nzchar(out$titre), , drop = FALSE]
}

# ── 2. PERTINENCE, CATÉGORIE, TENSIONS ───────────────────────
# « caoutchouc » seul est trop large en français (bottes, canards, joints…) : il
# faut un contexte de filière (naturel, prix, cours, planteurs, export…).
RS_ACTU_FORT <- paste0(
  "caoutchouc naturel|fili[èe]re (h[ée]v[ée]a|caoutchouc)|(prix|cours|march[ée]|cotations?|contrats?) (du|de) caoutchouc|",
  "(rebond|hausse|baisse|repli|recul|chute) du caoutchouc|^caoutchouc\\b|",
  "caoutchouc.*(ivoir|afrique|apromac|planteur|producteur|export|tonnes?|usine|sgx|bourse)|",
  "(ivoir|afrique|planteur|producteur|export|usine).*caoutchouc|",
  "h[ée]v[ée]a|h[ée]v[ée]icult|\\bhevea|natural rubber|",
  "rubber (price|prices|market|markets|plantation|plantations|farmer|farmers|planter|planters|tapper|tappers|",
  "export|exports|exporter|exporters|production|output|industry|sector|board|estate|estates|smallholder|smallholders|",
  "tree|trees|grower|growers|supply|futures|processing|factory|factories)|",
  "\\b(tsr ?20|tsr ?10|smr ?20|rss ?3)\\b|\\banrpc\\b|apromac|sgx[- ]sicom|",
  "\\brubber\\b.*\\b(ghana|liberia|nigeria|cameroon|ivory coast|c[ôo]te d.ivoire|thailand|indonesia|malaysia|vietnam)\\b|",
  "\\b(ghana|liberia|nigeria|cameroon|ivory coast|c[ôo]te d.ivoire|thailand|indonesia|malaysia|vietnam)\\b.*\\brubber\\b")
RS_ACTU_EXCLU <- paste0(
  "rubber (duck|ducks|stamp|stamps|bullet|bullets|band|bands|slipper|slippers|boot|boots|sole|soles|mat|mats|",
  "hose|hoses|glove|gloves|tyres? recycl|chicken)|britannica|wikipedia|epoxidi[sz]ed|nanocomposite|",
  "silicone rubber|synthetic rubber market|",
  "canards?|bottes?|gazon|semelles?|sneaker|\\bjoints?\\b|fouilles|soldats|tubeless|jantes?|",
  # communiqués payants d'« études de marché » (aucune information datée sur le marché réel)
  "market (size|growth|report|research|forecast|outlook|analysis|share)|\\bcagr\\b|openpr|einpresswire")

# Éditeurs écartés : encyclopédies et diffuseurs de communiqués payants
RS_ACTU_EDITEURS_EXCLUS <- "britannica|wikipedia|openpr|einpresswire|globenewswire|prnewswire|marketresearch"

rs_actu_pertinent <- function(titre, source = NULL) {
  x <- tolower(titre)
  ok <- grepl(RS_ACTU_FORT, x, perl = TRUE) & !grepl(RS_ACTU_EXCLU, x, perl = TRUE)
  if (!is.null(source)) ok <- ok & !grepl(RS_ACTU_EDITEURS_EXCLUS, tolower(ifelse(is.na(source), "", source)))
  ok
}

RS_ACTU_CATEGORIES <- list(
  list(cle = "eudr", fr = "Réglementation (EUDR)", en = "Regulation (EUDR)",
       motif = "\\beudr\\b|d[ée]forestation|tra[çc]abilit|traceabilit"),
  list(cle = "ci", fr = "Côte d'Ivoire", en = "Côte d'Ivoire",
       motif = "ivoir|c[ôo]te d.ivoire|ivory coast|abidjan|apromac|san[- ]p[ée]dro|\\bsaph\\b|\\bsogb\\b"),
  list(cle = "afrique", fr = "Afrique", en = "Africa",
       motif = "ghana|liberia|nigeria|cameroun|cameroon|gabon|guin[ée]e|guinea|africa|afrique|firestone"),
  list(cle = "asie", fr = "Asie", en = "Asia",
       motif = "thai|indonesi|malaysi|malaisie|vietnam|china|chine|chinese|india|\\binde\\b|anrpc|sicom|singapore|singapour|cambod"),
  list(cle = "industrie", fr = "Industrie aval", en = "Downstream industry",
       motif = "\\btyres?\\b|\\btires?\\b|pneu|michelin|bridgestone|continental|goodyear|pirelli|hankook|sumitomo"))

rs_actu_categorie <- function(titre) {
  x <- tolower(titre)
  vapply(x, function(t) {
    for (c in RS_ACTU_CATEGORIES) if (grepl(c$motif, t, perl = TRUE)) return(c$cle)
    "marche"
  }, character(1), USE.NAMES = FALSE)
}
rs_actu_categorie_libelle <- function(cle, langue = "fr") {
  for (c in RS_ACTU_CATEGORIES) if (identical(c$cle, cle)) return(if (langue == "fr") c$fr else c$en)
  if (langue == "fr") "Marché mondial" else "World market"
}

RS_ACTU_TENSION <- paste0(
  "sanction|\\bban\\b|banned|interdi|strike|gr[èe]ve|drought|s[ée]cheresse|flood|inondation|conflict|conflit|",
  "tariff|droits? de douane|export (restriction|curb|quota)|restriction|protest|manifestation|tension|\\bwar\\b|guerre|",
  "el ni[ñn]o|la ni[ñn]a|disease|maladie|leaf fall|shortage|p[ée]nurie")
rs_actu_tension <- function(titre) grepl(RS_ACTU_TENSION, tolower(titre), perl = TRUE)

# Clé de dédoublonnage : titre sans accents ni ponctuation
rs_actu_cle <- function(titre) {
  x <- tolower(iconv(titre, "UTF-8", "ASCII//TRANSLIT", sub = ""))
  gsub("[^a-z0-9]+", " ", x) |> trimws()
}

# ── 3. ANNONCES DE PRIX APROMAC DANS LES TITRES ──────────────
RS_MOIS_FR <- c("janvier", "f[ée]vrier", "mars", "avril", "mai", "juin", "juillet", "ao[ûu]t",
                "septembre", "octobre", "novembre", "d[ée]cembre")

# Retour : data.frame(mois "AAAA-MM", prix_fcfa, titre, source, lien, date_pub) ; aucune saisie
rs_actu_prix_apromac <- function(articles) {
  vide <- data.frame(mois = character(), prix_fcfa = numeric(), titre = character(), source = character(),
                     lien = character(), date_pub = character(), stringsAsFactors = FALSE)
  if (is.null(articles) || nrow(articles) == 0) return(vide)
  x <- tolower(articles$titre)
  cand <- grepl("apromac|prix (du|bord[- ]champ du|du kilogramme de) caoutchouc|prix du caoutchouc|caoutchouc naturel ?:", x) &
          grepl("[0-9]{3}\\s*(f\\s?cfa|frs|francs|f\\b)", x)
  if (!any(cand)) return(vide)
  rows <- lapply(which(cand), function(i) {
    t <- x[i]
    prix <- as.numeric(sub(".*?([0-9]{3})\\s*(f\\s?cfa|frs|francs|f\\b).*", "\\1", t, perl = TRUE))
    m <- which(vapply(RS_MOIS_FR, function(mm) grepl(paste0("\\b", mm, "\\b"), t, perl = TRUE), logical(1)))
    if (length(m) != 1 || !is.finite(prix) || prix < 100 || prix > 2000) return(NULL)
    dp <- as.Date(articles$date[i])
    an <- as.integer(format(dp, "%Y"))
    if (m == 1 && as.integer(format(dp, "%m")) == 12) an <- an + 1L     # prix de janvier annoncé fin décembre
    an_txt <- regmatches(t, regexpr("\\b20[0-9]{2}\\b", t))
    if (length(an_txt) == 1) an <- as.integer(an_txt)
    data.frame(mois = sprintf("%d-%02d", an, m), prix_fcfa = prix, titre = articles$titre[i],
               source = articles$source[i], lien = articles$lien[i], date_pub = as.character(dp), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) return(vide)
  out[!duplicated(out[, c("mois", "prix_fcfa")]), , drop = FALSE]
}

# Comparaison avec ce que la chaîne connaît déjà :
#   saisie     : data/reference/prix_apromac.csv (évaluateur) — data.frame(mois, prix_fcfa_kg)
#   historique : data/reference/apromac_history.csv (relevé presse automatique du
#                script 01, APROMAC v1) — data.frame(mois, prix_fcfa)
# Retour : annonces + prix_saisi + statut :
#   "a_saisir" (inconnu partout : jamais utilisé dans un calcul), "conforme",
#   "ecart" (diffère de la valeur connue), "releve_presse" (déjà relevé par le script 01)
rs_actu_comparer_apromac <- function(annonces, saisie, historique = NULL) {
  if (nrow(annonces) == 0) return(cbind(annonces, prix_saisi = numeric(), statut = character()))
  ps <- suppressWarnings(as.numeric(saisie$prix_fcfa_kg[match(annonces$mois, saisie$mois)]))
  ph <- if (is.null(historique) || nrow(historique) == 0) rep(NA_real_, nrow(annonces))
        else suppressWarnings(as.numeric(historique$prix_fcfa[match(annonces$mois, historique$mois)]))
  connu <- ifelse(is.na(ps), ph, ps)
  annonces$prix_saisi <- connu
  annonces$statut <- ifelse(is.na(connu), "a_saisir",
                     ifelse(abs(connu - annonces$prix_fcfa) >= 0.5, "ecart",
                     ifelse(is.na(ps), "releve_presse", "conforme")))
  annonces
}

# ── 4. SÉLECTION ─────────────────────────────────────────────
# Score : Côte d'Ivoire +3, Afrique +2, EUDR +2, prix/marché +1, récence (3 jours) +1
rs_actu_score <- function(articles, date_ref) {
  if (nrow(articles) == 0) return(numeric())
  x <- tolower(articles$titre)
  (articles$categorie == "ci") * 3 + (articles$categorie %in% c("afrique", "eudr")) * 2 +
    grepl("prix|price|march[ée]|market|export|production", x) * 1 +
    (as.Date(articles$date) >= as.Date(date_ref) - 3) * 1
}

# Assemble les articles de toutes les sources : fenêtre, pertinence, doublons, catégories
rs_actu_assembler <- function(liste, date_ref, fenetre_j = RS_ACTU_FENETRE_J) {
  a <- do.call(rbind, Filter(function(d) !is.null(d) && nrow(d) > 0, liste))
  if (is.null(a) || nrow(a) == 0) return(list(articles = NULL, n_bruts = 0L, n_fenetre = 0L))
  n_bruts <- nrow(a)
  d <- as.Date(a$date)
  a <- a[!is.na(d) & d > as.Date(date_ref) - fenetre_j & d <= as.Date(date_ref) + 1, , drop = FALSE]
  n_fenetre <- nrow(a)
  a <- a[rs_actu_pertinent(a$titre, a$source), , drop = FALSE]
  a <- a[order(a$date, decreasing = TRUE), , drop = FALSE]
  a <- a[!duplicated(rs_actu_cle(a$titre)), , drop = FALSE]
  if (nrow(a) == 0) return(list(articles = a, n_bruts = n_bruts, n_fenetre = n_fenetre))
  a$categorie <- rs_actu_categorie(a$titre)
  a$tension <- rs_actu_tension(a$titre)
  a$score <- rs_actu_score(a, date_ref)
  a <- a[order(-a$score, -as.numeric(a$date)), , drop = FALSE]
  rownames(a) <- NULL
  list(articles = a, n_bruts = n_bruts, n_fenetre = n_fenetre)
}

# ── 5. TÉLÉCHARGEMENT (réseau) ───────────────────────────────
rs_actu_sources <- function(f = RS_ACTU_SOURCES) {
  s <- utils::read.csv(f, colClasses = "character", encoding = "UTF-8", stringsAsFactors = FALSE)
  s[tolower(s$actif) %in% c("oui", "true", "1"), , drop = FALSE]
}

# Une source : list(articles, diag = list(id, http, items, message))
rs_actu_telecharger <- function(src) {
  txt <- tryCatch(rs_get(src$url, timeout_s = 25, tries = 2), error = function(e) e)
  if (inherits(txt, "error"))
    return(list(articles = NULL, diag = list(id = src$id, statut = "erreur", items = 0L,
                                             message = substr(conditionMessage(txt), 1, 160))))
  a <- rs_actu_lire_rss(txt, origine = src$id, nom_source = sub("\\s*\\([^)]*\\)\\s*$", "", src$nom))
  list(articles = a, diag = list(id = src$id, statut = if (nrow(a) > 0) "ok" else "vide", items = nrow(a), message = NA_character_))
}
