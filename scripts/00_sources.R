# =============================================================
# RUBBERSIGNAL.COM — Script 00 : Sources de donnees automatiques
# Version : 1.0 (octobre 2026)
# Role    : fournir au script 01 TOUTES ses entrees sans saisie
#           manuelle : taux de change, prix TSR20, prix APROMAC.
# Principes :
#   1. Chaque donnee a une chaine de sources (A > B > C).
#   2. Aucune valeur n'est inventee ni figee dans le code.
#   3. Une valeur trop ancienne est SIGNALEE (statut), jamais
#      reutilisee en silence ; au-dela d'un seuil, le RSCI
#      devient NA plutot que faux.
#   4. Si une donnee indispensable est introuvable (TSR20, EUR/USD),
#      le script s'arrete avec une erreur : le workflow GitHub
#      passe au rouge et t'envoie un e-mail.
# Usage   : source("scripts/00_sources.R")   (appele par le script 01)
# =============================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(rvest)
  library(xml2)
})

# Regles metier partagees (fraicheur, conversions, mecanisme CHPH, RSCI)
if (!exists("rs_fraicheur", mode = "function"))
  source(if (file.exists("scripts/00_regles.R")) "scripts/00_regles.R" else "00_regles.R")

RS_UA <- "RubberSignalBot/1.0 (+https://rubbersignal.com; github.com/Sahiri70/rubbersignal)"

# ── Outils generiques ────────────────────────────────────────

# Telechargement avec 3 essais. Si RUBBERSIGNAL_FIXTURE_DIR est defini
# (tests uniquement), lit un fichier local au lieu d'aller sur Internet.
rs_get <- function(url, query = list(), headers = NULL, timeout_s = 30, tries = 3) {
  fx <- Sys.getenv("RUBBERSIGNAL_FIXTURE_DIR", "")
  if (nzchar(fx)) {
    cle <- gsub("[^A-Za-z0-9]+", "_", sub("^https?://", "", url))
    f   <- file.path(fx, paste0(substr(cle, 1, 80), ".txt"))
    if (!file.exists(f)) stop("fixture absente : ", f)
    return(paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  }
  h <- if (is.null(headers)) character() else headers
  resp <- httr::RETRY(
    "GET", url, query = query,
    httr::add_headers(.headers = h),
    httr::user_agent(RS_UA),
    httr::timeout(timeout_s),
    times = tries, pause_base = 2, pause_cap = 20, quiet = TRUE
  )
  httr::stop_for_status(resp)
  httr::content(resp, as = "text", encoding = "UTF-8")
}

rs_msg <- function(...) cat("   ", ..., "\n", sep = "")

# Convertit "263-00" -> 263.00 ; "260-4" -> 260.4 ; "1053-50" -> 1053.5
rs_parse_dash <- function(x) {
  m <- regmatches(x, regexec("^\\s*([0-9]{2,4})-([0-9]{1,2})\\s*$", x))[[1]]
  if (length(m) < 3) return(NA_real_)
  as.numeric(m[2]) + as.numeric(m[3]) / 10^nchar(m[3])
}

rs_nettoyer <- function(x) trimws(gsub("[\\s\\x{00A0}]+", " ", enc2utf8(x), perl = TRUE))


# ── 1. TAUX DE CHANGE (BCE, via Frankfurter, repli API BCE) ──

rs_fx <- function() {
  fx <- tryCatch({
    txt <- rs_get("https://api.frankfurter.dev/v1/latest",
                  query = list(base = "EUR", symbols = "USD,JPY,CNY,CHF"))
    j <- jsonlite::fromJSON(txt)
    r <- j$rates
    stopifnot(is.numeric(r$USD), r$USD > 0.8, r$USD < 1.6)
    list(eur_usd = r$USD, eur_jpy = r$JPY, eur_cny = r$CNY, eur_chf = r$CHF,
         date = as.character(j$date),
         source = "Frankfurter (taux de reference BCE)")
  }, error = function(e) { rs_msg("Frankfurter KO : ", conditionMessage(e)); NULL })
  if (!is.null(fx)) return(fx)

  # Repli : API officielle BCE (EUR/USD seulement)
  tryCatch({
    txt <- rs_get("https://data-api.ecb.europa.eu/service/data/EXR/D.USD.EUR.SP00.A",
                  query = list(lastNObservations = 1, format = "csvdata"))
    d <- utils::read.csv(text = txt, stringsAsFactors = FALSE)
    v <- as.numeric(d$OBS_VALUE[1])
    stopifnot(is.finite(v), v > 0.8, v < 1.6)
    list(eur_usd = v, eur_jpy = NA_real_, eur_cny = NA_real_, eur_chf = NA_real_,
         date = as.character(d$TIME_PERIOD[1]),
         source = "BCE data-api (EUR/USD seul)")
  }, error = function(e) { rs_msg("BCE data-api KO : ", conditionMessage(e)); NULL })
}


# ── 2. PRIX TSR20 / SMR20 ────────────────────────────────────

# Source A bis : rtas.sg republie (a) le reglement officiel SGX du TSR20
# et (b) les prix de reference physiques de la MRB (ex-LGM) pour SMR20.
rs_fetch_rtas <- function() {
  html <- rs_get("https://www.rtas.sg/rubber-prices/")
  page <- xml2::read_html(html)

  # Toutes les lignes de tous les tableaux, cellule par cellule
  lignes <- list()
  for (t in rvest::html_elements(page, "table")) {
    for (tr in rvest::html_elements(t, "tr")) {
      cel <- rs_nettoyer(rvest::html_text(rvest::html_elements(tr, "th,td")))
      if (length(cel) >= 2) lignes[[length(lignes) + 1]] <- cel
    }
  }

  re_dash2 <- "^[0-9]{3,4}-[0-9]{2}$"
  re_mois  <- "^(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)[a-z]*( [0-9]{4})?$"

  smr20 <- NA_real_; sgx <- NA_real_; sgx_mois <- NA_character_
  for (cel in lignes) {
    if (is.na(smr20) && grepl("^(SMR ?)?20$", cel[1])) {
      d <- cel[-1][grepl(re_dash2, cel[-1])]
      if (length(d) >= 2) smr20 <- rs_parse_dash(d[length(d)])   # colonne US cents/kg
    }
    if (is.na(sgx) && grepl(re_mois, cel[1]) && grepl("^[0-9]{3}-[0-9]{1,2}$", cel[2])) {
      sgx <- rs_parse_dash(cel[2]); sgx_mois <- cel[1]
    }
  }

  # Repli texte brut si la structure des tableaux a change
  txt <- rs_nettoyer(rvest::html_text(page))
  if (is.na(smr20)) {
    m <- regmatches(txt, regexec("\\b20 ([0-9]{3,4}-[0-9]{2}) ([0-9]{3}-[0-9]{2})", txt))[[1]]
    if (length(m) == 3) smr20 <- rs_parse_dash(m[3])
  }
  m <- regmatches(txt, regexec("SMR ([A-Za-z]{3,9} [0-9]{4})", txt))[[1]]
  smr_mois <- if (length(m) == 2) m[2] else NA_character_

  # Date de la page : "Daily Price - 03 Nov 2026, Tue" (NA si absente : jamais devinee)
  m <- regmatches(txt, regexec("Daily Price\\s*-\\s*([0-9]{1,2} [A-Za-z]{3} [0-9]{4})", txt))[[1]]
  date_page <- if (length(m) == 2) rs_date_anglaise(m[2]) else NA_character_

  ok <- function(v) is.finite(v) && v > 150 && v < 500
  list(smr20_cents = if (ok(smr20)) smr20 else NA_real_, smr_mois = smr_mois,
       sgx_cents   = if (ok(sgx))   sgx   else NA_real_, sgx_mois = sgx_mois,
       date_page   = date_page)
}

# "03 Nov 2026" -> "2026-11-03" (independant de la locale)
rs_date_anglaise <- function(x) {
  p <- strsplit(trimws(x), "\\s+")[[1]]
  if (length(p) != 3) return(NA_character_)
  mo <- match(tolower(substr(p[2], 1, 3)), tolower(month.abb))
  if (is.na(mo)) return(NA_character_)
  d <- tryCatch(as.Date(sprintf("%s-%02d-%02d", p[3], mo, as.integer(p[1]))), error = function(e) NA)
  if (is.na(d)) NA_character_ else as.character(d)
}

# Source A : site officiel MRB/LGM (ancien code du script 01 v3.1)
rs_fetch_lgm <- function() {
  tryCatch({
    page  <- xml2::read_html(rs_get("https://www.lgm.gov.my"), options = "NOERROR")
    texte <- rvest::html_text2(page)
    m <- regmatches(texte, regexpr("[0-9]{2,3}\\.[0-9]{1,3}(?=\\s*\\(?US Cents)", texte, perl = TRUE))
    v <- if (length(m) > 0) as.numeric(m[1]) else NA_real_
    if (is.finite(v) && v > 150 && v < 500) v else NA_real_
  }, error = function(e) { rs_msg("LGM direct KO : ", conditionMessage(e)); NA_real_ })
}

# Source C : IndexMundi (mensuel, en retard) — dernier recours
rs_fetch_indexmundi <- function() {
  tryCatch({
    page <- xml2::read_html(rs_get("https://www.indexmundi.com/commodities/?commodity=rubber&months=12"))
    tab  <- rvest::html_table(rvest::html_element(page, "table"), header = TRUE)
    tab  <- tab[, 1:2]
    names(tab) <- c("mois", "prix")
    tab$prix <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", tab$prix)))
    tab <- tab[is.finite(tab$prix), ]
    if (nrow(tab) == 0) return(NULL)
    list(mois = tab$mois[1], usd = tab$prix[1], tableau = tab)
  }, error = function(e) { rs_msg("IndexMundi KO : ", conditionMessage(e)); NULL })
}

# Chaine de decision. Priorite : site officiel MRB/LGM > MRB via rtas.sg
# (meme serie SMR20) > SGX TSR20 (autre indice, signale) > IndexMundi.
rs_tsr20 <- function() {
  rt  <- tryCatch(rs_fetch_rtas(), error = function(e) { rs_msg("rtas.sg KO : ", conditionMessage(e)); NULL })
  lgm <- rs_fetch_lgm()
  im  <- NULL

  # Chaque candidat porte la date de sa cotation (date_source) ; NA = inconnue.
  date_rt <- if (!is.null(rt)) rt$date_page %||% NA_character_ else NA_character_
  cand <- list()
  if (is.finite(lgm))
    cand$lgm <- list(usd = lgm / 100, source = "LGM/MRB SMR20 (site officiel)", indice = "MRB_SMR20",
                     date_source = NA_character_)
  if (!is.null(rt) && is.finite(rt$smr20_cents))
    cand$mrb <- list(usd = rt$smr20_cents / 100, indice = "MRB_SMR20",
                     source = paste0("MRB SMR20 FOB ", rt$smr_mois, " (via rtas.sg)"),
                     date_source = date_rt)
  if (!is.null(rt) && is.finite(rt$sgx_cents))
    cand$sgx <- list(usd = rt$sgx_cents / 100, indice = "SGX_TSR20",
                     source = paste0("SGX TSR20 FOB ", rt$sgx_mois, " (reglement de la veille, via rtas.sg)"),
                     date_source = date_rt)
  if (length(cand) == 0) {
    im <- rs_fetch_indexmundi()
    if (!is.null(im)) {
      d_im <- tryCatch(as.character(as.Date(paste0("01-", match(tolower(substr(im$mois, 1, 3)), tolower(month.abb)),
                                                   "-", sub(".*([0-9]{4}).*", "\\1", im$mois)), "%d-%m-%Y")),
                       error = function(e) NA_character_)
      cand$im <- list(usd = im$usd, indice = "INDEXMUNDI", source = paste0("IndexMundi (", im$mois, ")"),
                      date_source = d_im)
    }
  }
  if (length(cand) == 0)
    stop("TSR20 : AUCUNE source accessible (LGM, rtas.sg, SGX, IndexMundi). ",
         "Arret volontaire : on ne publie pas de prix invente.")

  ordre   <- intersect(c("lgm", "mrb", "sgx", "im"), names(cand))
  # A priorite egale, une cotation DATEE et fraiche (<= 14 j) passe devant une cotation sans date.
  date_ref <- as.Date(Sys.getenv("RUBBERSIGNAL_TODAY", as.character(Sys.Date())))
  frais <- ordre[vapply(ordre, function(k) {
    d <- suppressWarnings(as.Date(cand[[k]]$date_source %||% NA_character_))
    !is.na(d) && as.numeric(date_ref - d) >= 0 && as.numeric(date_ref - d) <= 14
  }, logical(1))]
  if (length(frais) > 0) ordre <- c(frais, setdiff(ordre, frais))
  retenu  <- cand[[ordre[1]]]
  autres  <- setdiff(names(cand), ordre[1])
  diverg  <- if (length(autres) > 0)
    max(sapply(autres, function(k) abs(cand[[k]]$usd / retenu$usd - 1))) * 100 else NA_real_

  alertes <- character()
  if (is.finite(diverg) && diverg > 10)
    alertes <- c(alertes, sprintf("TSR20 : ecart de %.1f %% entre sources (seuil 10 %%)", diverg))
  if (retenu$indice != "MRB_SMR20")
    alertes <- c(alertes, sprintf("TSR20 : indice de substitution (%s) - non strictement comparable a la serie LGM/MRB", retenu$indice))

  list(usd = round(retenu$usd, 4), source = retenu$source, indice = retenu$indice,
       date_source = retenu$date_source %||% NA_character_,
       candidats = lapply(cand, function(x) list(usd = round(x$usd, 4), source = x$source, indice = x$indice,
                                                 date_source = x$date_source %||% NA_character_)),
       divergence_max_pct = if (is.finite(diverg)) round(diverg, 1) else NA_real_,
       sgx_cents = if (!is.null(rt)) rt$sgx_cents else NA_real_,
       sgx_mois  = if (!is.null(rt)) rt$sgx_mois  else NA_character_,
       indexmundi = if (!is.null(im)) im$tableau else NULL,
       alertes = alertes)
}


# ── 3. PRIX APROMAC (bord champ, FCFA/kg, mensuel) ──────────

RS_MOIS_FR <- c(janvier = 1, fevrier = 2, mars = 3, avril = 4, mai = 5, juin = 6,
                juillet = 7, aout = 8, septembre = 9, octobre = 10,
                novembre = 11, decembre = 12)

rs_sans_accents <- function(x) {
  x <- tolower(enc2utf8(x))
  x <- gsub("[\\x{00e9}\\x{00e8}\\x{00ea}\\x{00eb}]", "e", x, perl = TRUE)
  x <- gsub("[\\x{00e0}\\x{00e2}\\x{00e4}]", "a", x, perl = TRUE)
  x <- gsub("[\\x{00ee}\\x{00ef}]", "i", x, perl = TRUE)
  x <- gsub("[\\x{00f4}\\x{00f6}]", "o", x, perl = TRUE)
  x <- gsub("[\\x{00f9}\\x{00fb}\\x{00fc}]", "u", x, perl = TRUE)
  gsub("\\x{00e7}", "c", x, perl = TRUE)
}

# Extrait (mois "AAAA-MM", prix FCFA/kg) d'un titre/texte d'article.
# Regle stricte : il faut APROMAC ou caoutchouc, un prix suivi de FCFA,
# et un mois + annee dans le meme texte. Sinon : rien (pas de devinette).
rs_extraire_apromac <- function(texte) {
  t <- rs_sans_accents(rs_nettoyer(texte))
  if (!grepl("apromac|caoutchouc|hevea", t)) return(NULL)
  re_prix <- "([0-9]{3})(?:[.,][0-9]+)?\\s*(?:f\\s?cfa|f\\.cfa)"
  pos <- regexpr(re_prix, t, perl = TRUE)
  if (pos < 0) return(NULL)
  p <- regmatches(t, regexec(re_prix, t, perl = TRUE))[[1]]
  # Prix approximatif (« plus de 500 FCFA », « environ 480 »...) : valeur exacte
  # inconnue -> on refuse plutot que d'enregistrer un chiffre faux.
  avant <- substr(t, max(1, pos - 25), pos - 1)
  if (grepl("plus( de| d')?\\s*$|plus de|superieur|au[- ]dessus|environ|pres de|autour de|depasse|moins de|jusqu", avant))
    return(NULL)
  m <- regmatches(t, regexec(
    "(janvier|fevrier|mars|avril|mai|juin|juillet|aout|septembre|octobre|novembre|decembre)\\s+(20[0-9]{2})", t))[[1]]
  if (length(p) < 2 || length(m) < 3) return(NULL)
  prix <- as.numeric(p[2])
  if (!is.finite(prix) || prix < 250 || prix > 900) return(NULL)
  list(mois = sprintf("%s-%02d", m[3], RS_MOIS_FR[[m[2]]]), prix = prix)
}

rs_articles_apromac <- function() {
  items <- list()

  key <- Sys.getenv("NEWSAPI_KEY")
  if (nzchar(key)) {
    tryCatch({
      from <- format(Sys.Date() - 28, "%Y-%m-%d")
      for (q in c("APROMAC", "prix caoutchouc FCFA kg")) {
        txt <- rs_get("https://newsapi.org/v2/everything",
                      query = list(q = q, language = "fr", from = from,
                                   sortBy = "publishedAt", pageSize = 50),
                      headers = c("X-Api-Key" = key))
        j <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
        for (a in j$articles)
          items[[length(items) + 1]] <- list(src = "NewsAPI", url = a$url %||% "",
                                             texte = paste(a$title %||% "", a$description %||% ""))
      }
    }, error = function(e) rs_msg("NewsAPI KO : ", conditionMessage(e)))
  }

  tryCatch({
    txt <- rs_get("https://api.gdeltproject.org/api/v2/doc/doc",
                  query = list(query = "APROMAC caoutchouc sourcelang:french", mode = "artlist",
                               format = "json", maxrecords = 50, sort = "datedesc", timespan = "3months"))
    j <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
    for (a in j$articles)
      items[[length(items) + 1]] <- list(src = "GDELT", url = a$url %||% "", texte = a$title %||% "")
  }, error = function(e) rs_msg("GDELT KO : ", conditionMessage(e)))

  tryCatch({
    page <- xml2::read_html(rs_get("https://apromac.ci/"), options = "NOERROR")
    for (nd in rvest::html_elements(page, "h1,h2,h3,h4,a"))
      items[[length(items) + 1]] <- list(src = "apromac.ci", url = "https://apromac.ci/",
                                         texte = rs_nettoyer(rvest::html_text(nd)))
  }, error = function(e) rs_msg("apromac.ci KO : ", conditionMessage(e)))

  items
}

# Un prix par mois = valeur la plus frequente parmi les sources
rs_consensus_apromac <- function(items, aujourdhui = Sys.Date()) {
  recs <- list()
  for (it in items) {
    r <- rs_extraire_apromac(it$texte)
    if (!is.null(r)) recs[[length(recs) + 1]] <- c(r, list(src = it$src, url = it$url))
  }
  if (length(recs) == 0) return(data.frame())
  df <- data.frame(mois = sapply(recs, `[[`, "mois"), prix = sapply(recs, `[[`, "prix"),
                   src = sapply(recs, `[[`, "src"), url = sapply(recs, `[[`, "url"),
                   stringsAsFactors = FALSE)
  # Fenetre plausible : de -4 mois a +1 mois
  mois_ok <- format(seq(aujourdhui, by = "-1 month", length.out = 5), "%Y-%m")
  mois_ok <- c(format(seq(aujourdhui, by = "1 month", length.out = 2)[2], "%Y-%m"), mois_ok)
  df <- df[df$mois %in% mois_ok, , drop = FALSE]
  if (nrow(df) == 0) return(data.frame())
  out <- do.call(rbind, lapply(split(df, df$mois), function(g) {
    tb  <- sort(table(g$prix), decreasing = TRUE)
    val <- as.numeric(names(tb)[1])
    data.frame(mois = g$mois[1], prix_fcfa = val, n_sources = sum(g$prix == val),
               conflit = length(tb) > 1,
               source = paste(unique(g$src[g$prix == val]), collapse = "+"),
               url = g$url[g$prix == val][1], stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out
}

# Historique persistant (committe par le pipeline) : data/reference/apromac_history.csv
rs_hist_apromac_lire <- function(f = "data/reference/apromac_history.csv") {
  if (!file.exists(f)) return(data.frame(mois = character(), prix_fcfa = numeric(),
                                         source = character(), url = character(),
                                         date_ajout = character(), stringsAsFactors = FALSE))
  utils::read.csv(f, stringsAsFactors = FALSE)
}

rs_hist_apromac_maj <- function(nouveaux, f = "data/reference/apromac_history.csv",
                                aujourdhui = Sys.Date()) {
  h <- rs_hist_apromac_lire(f)
  alertes <- character()
  if (nrow(nouveaux) > 0) {
    for (i in seq_len(nrow(nouveaux))) {
      n <- nouveaux[i, ]
      if (n$mois %in% h$mois) {
        ancien <- h$prix_fcfa[h$mois == n$mois][1]
        if (ancien != n$prix_fcfa)
          alertes <- c(alertes, sprintf("APROMAC %s : %s FCFA trouve, %s FCFA deja en historique (historique conserve)",
                                        n$mois, n$prix_fcfa, ancien))
      } else {
        h <- rbind(h, data.frame(mois = n$mois, prix_fcfa = n$prix_fcfa, source = n$source,
                                 url = n$url, date_ajout = as.character(aujourdhui),
                                 stringsAsFactors = FALSE))
      }
      if (isTRUE(n$conflit))
        alertes <- c(alertes, sprintf("APROMAC %s : sources en desaccord, valeur majoritaire retenue (%s FCFA)", n$mois, n$prix_fcfa))
    }
    h <- h[order(h$mois), ]
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(h, f, row.names = FALSE)
  }
  list(historique = h, alertes = alertes)
}

# Choisit le prix a utiliser pour la date donnee et dit s'il est frais.
#   ok      : prix du mois en cours
#   reporte : prix du mois precedent (normal en debut de mois)
#   perime  : plus vieux -> le RSCI sera NA (pas de valeur fausse)
rs_apromac_resolu <- function(h, aujourdhui = Sys.Date()) {
  if (nrow(h) == 0) stop("APROMAC : historique vide et aucune source accessible.")
  cur <- format(aujourdhui, "%Y-%m")
  h   <- h[h$mois <= cur, , drop = FALSE]
  if (nrow(h) == 0) stop("APROMAC : aucun prix pour le mois courant ou avant.")
  r <- h[order(h$mois, decreasing = TRUE)[1], ]
  y1 <- as.integer(substr(cur, 1, 4)); m1 <- as.integer(substr(cur, 6, 7))
  y2 <- as.integer(substr(r$mois, 1, 4)); m2 <- as.integer(substr(r$mois, 6, 7))
  age <- (y1 - y2) * 12 + (m1 - m2)
  statut <- if (age == 0) "ok" else if (age == 1) "reporte" else "perime"
  list(prix_fcfa = r$prix_fcfa, mois = r$mois, age_mois = age, statut = statut,
       source = r$source, url = r$url)
}


# ── 3 bis. REFERENCES MENSUELLES LIBRES (FMI via FRED, Banque mondiale) ──
# Sources gratuites et redistribuables : servent de reference datee quand la
# cotation hebdomadaire est absente ou perimee, et de denominateur au RSCI.

# FMI "Global price of Rubber" (FRED PRUBBUSDM), US cents/lb, mensuel.
# Retourne NULL (avec message) si la cle manque ou si FRED ne repond pas.
rs_fred_prubbusdm <- function(n = 13, key = Sys.getenv("FRED_KEY")) {
  if (!nzchar(key)) { rs_msg("FRED PRUBBUSDM : cle FRED_KEY absente"); return(NULL) }
  tryCatch({
    txt <- rs_get("https://api.stlouisfed.org/fred/series/observations",
                  query = list(series_id = "PRUBBUSDM", api_key = key, file_type = "json",
                               sort_order = "desc", limit = n))
    o <- jsonlite::fromJSON(txt)$observations
    o <- o[o$value != ".", , drop = FALSE]
    if (nrow(o) == 0) stop("aucune observation")
    v <- as.numeric(o$value)
    stopifnot(all(is.finite(v)), v[1] > 30, v[1] < 400)      # plage plausible en cents/lb
    serie <- data.frame(periode = substr(o$date, 1, 7), cents_lb = round(v, 3),
                        usd_kg = round(rs_cents_lb_vers_usd_kg(v), 4), stringsAsFactors = FALSE)
    list(valeur = serie$usd_kg[1], unite = "USD/kg",
         valeur_orig = serie$cents_lb[1], unite_orig = "US cents/lb",
         source = "FMI, Global price of Rubber (FRED, serie PRUBBUSDM)",
         date_source = o$date[1], periode = serie$periode[1],
         methode = "Moyenne mensuelle FMI ; conversion USD/kg = cents/lb x 2.20462 / 100",
         url = "https://fred.stlouisfed.org/series/PRUBBUSDM",
         serie = serie)
  }, error = function(e) { rs_msg("FRED PRUBBUSDM KO : ", sub("api_key=[^&]+", "api_key=****", conditionMessage(e))); NULL })
}

# Banque mondiale, Pink Sheet mensuelle (CMO-Historical-Data-Monthly.xlsx) :
# colonne "Rubber, TSR20" ($/kg). Le lien change chaque annee : on le lit sur
# la page officielle. Retourne NULL (avec message) en cas d'echec.
rs_wb_pinksheet_tsr20 <- function(n = 13) {
  if (nzchar(Sys.getenv("RUBBERSIGNAL_FIXTURE_DIR", ""))) return(NULL)   # tests hors ligne
  if (!requireNamespace("readxl", quietly = TRUE)) { rs_msg("Pink Sheet : package readxl absent"); return(NULL) }
  tryCatch({
    page <- rs_get("https://www.worldbank.org/en/research/commodity-markets")
    lien <- regmatches(page, regexpr("https://thedocs\\.worldbank\\.org/[^\"' ]*CMO-Historical-Data-Monthly\\.xlsx", page))
    if (length(lien) == 0) stop("lien CMO-Historical-Data-Monthly.xlsx introuvable")
    f <- tempfile(fileext = ".xlsx")
    resp <- httr::RETRY("GET", lien, httr::user_agent(RS_UA), httr::timeout(60),
                        httr::write_disk(f, overwrite = TRUE), times = 3, quiet = TRUE)
    httr::stop_for_status(resp)
    entete <- suppressMessages(readxl::read_excel(f, sheet = "Monthly Prices", col_names = FALSE, n_max = 6))
    noms   <- as.character(unlist(entete[5, ]))
    maj    <- as.character(unlist(entete[4, ]))[1]
    col    <- which(trimws(gsub("\\*", "", noms)) == "Rubber, TSR20")
    if (length(col) != 1) stop("colonne 'Rubber, TSR20' introuvable")
    d <- suppressMessages(readxl::read_excel(f, sheet = "Monthly Prices", col_names = FALSE, skip = 6))
    per <- as.character(d[[1]]); val <- suppressWarnings(as.numeric(d[[col]]))
    ok  <- grepl("^[0-9]{4}M[0-9]{2}$", per) & is.finite(val)
    per <- per[ok]; val <- val[ok]
    if (length(val) == 0) stop("aucune valeur TSR20")
    serie <- data.frame(periode = sub("M", "-", per), usd_kg = round(val, 4), stringsAsFactors = FALSE)
    serie <- serie[order(serie$periode, decreasing = TRUE), , drop = FALSE]
    stopifnot(serie$usd_kg[1] > 0.5, serie$usd_kg[1] < 6)
    list(valeur = serie$usd_kg[1], unite = "USD/kg",
         source = "Banque mondiale, Pink Sheet mensuelle (Rubber, TSR20)",
         date_source = paste0(serie$periode[1], "-01"), periode = serie$periode[1],
         methode = paste0("Moyenne mensuelle Banque mondiale (SGX TSR20) ; fichier ", basename(lien),
                          if (!is.na(maj)) paste0(" - ", maj) else ""),
         url = lien,
         serie = utils::head(serie, n))
  }, error = function(e) { rs_msg("Pink Sheet Banque mondiale KO : ", conditionMessage(e)); NULL })
}


# ── 4. HISTORIQUE TSR20 REEL (remplace l'ancien historique code en dur) ──

rs_hist_tsr20_maj <- function(date, prix_usd, source, indice,
                              f = "data/reference/tsr20_hebdo.csv") {
  h <- if (file.exists(f)) utils::read.csv(f, stringsAsFactors = FALSE) else
    data.frame(cle = character(), date = character(), prix_usd = numeric(),
               source = character(), indice = character(), stringsAsFactors = FALSE)
  cle <- format(date, "%G-W%V")
  h <- h[h$cle != cle, , drop = FALSE]          # relance dans la meme semaine : on remplace
  h <- rbind(h, data.frame(cle = cle, date = as.character(date), prix_usd = prix_usd,
                           source = source, indice = indice, stringsAsFactors = FALSE))
  h <- h[order(h$date), ]
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(h, f, row.names = FALSE)
  h
}


# ── 5. DOUANES UE (Eurostat Comext) ET TAUX MENSUELS (mission 3) ──
# Sources publiques sans cle (sauf FRED, source secondaire). Jamais de valeur
# de secours : NULL + message si la source ne repond pas.

# Eurostat Comext DS-045409 : importations UE (declarant "EU" ; "EU27_2020"
# n'est pas accepte par l'API) de NC 40012200, valeur (EUR) et quantite
# (100 kg), un seul appel pour tous les partenaires. Renvoie le TEXTE brut
# (sauvegarde dans data/raw/ par le script 06).
rs_comext_requete <- function(partenaires = c(RS_RAPI_PARTENAIRE_CI, RS_RAPI_PARTENAIRES_ASIE), n_mois = 24) {
  c(list(format = "JSON", freq = "M", reporter = "EU"),
    stats::setNames(as.list(partenaires), rep("partner", length(partenaires))),
    list(product = RS_RAPI_PRODUIT, flow = "1", indicators = "VALUE_IN_EUROS",
         indicators = "QUANTITY_IN_100KG", lastTimePeriod = n_mois))
}
rs_comext_url <- function(q = rs_comext_requete()) httr::modify_url(RS_RAPI_URL_BASE, query = q)

rs_comext_telecharger <- function(q = rs_comext_requete()) {
  tryCatch({
    txt <- rs_get(RS_RAPI_URL_BASE, query = q, timeout_s = 90)
    if (!grepl("\"dimension\"", txt, fixed = TRUE)) stop("reponse sans donnees JSON-stat")
    txt
  }, error = function(e) { rs_msg("Eurostat Comext KO : ", conditionMessage(e)); NULL })
}

# BCE, moyenne mensuelle du taux de reference USD/EUR (serie EXR.M.USD.EUR.SP00.A)
# URL verifiee le 06/11/2026 : format csvdata, colonnes TIME_PERIOD et OBS_VALUE.
RS_URL_BCE_EXR_M <- "https://data-api.ecb.europa.eu/service/data/EXR/M.USD.EUR.SP00.A"
rs_bce_usd_eur_mensuel <- function(debut = "2024-01") {
  tryCatch({
    txt <- rs_get(RS_URL_BCE_EXR_M, query = list(format = "csvdata", startPeriod = debut))
    d <- utils::read.csv(text = txt, stringsAsFactors = FALSE)
    if (!all(c("TIME_PERIOD", "OBS_VALUE") %in% names(d))) stop("colonnes TIME_PERIOD / OBS_VALUE absentes")
    v <- suppressWarnings(as.numeric(d$OBS_VALUE))
    ok <- grepl("^[0-9]{4}-[0-9]{2}$", d$TIME_PERIOD) & is.finite(v)
    if (!any(ok)) stop("aucune moyenne mensuelle")
    stopifnot(all(v[ok] > 0.8 & v[ok] < 1.6))
    data.frame(periode = d$TIME_PERIOD[ok], taux_usd_eur = v[ok],
               source = "BCE, moyenne mensuelle du taux de reference (EXR.M.USD.EUR.SP00.A)",
               serie_id = "BCE_USD_EUR_M", stringsAsFactors = FALSE)
  }, error = function(e) { rs_msg("BCE taux mensuel KO : ", conditionMessage(e)); NULL })
}

# Source secondaire declaree (methodologie) : FRED EXUSEU, USD pour 1 EUR, mensuel.
# Appelee seulement si la BCE echoue ; son usage est indique sur chaque valeur.
rs_fred_exuseu <- function(debut = "2024-01", key = Sys.getenv("FRED_KEY")) {
  if (!nzchar(key)) { rs_msg("FRED EXUSEU : cle FRED_KEY absente"); return(NULL) }
  tryCatch({
    txt <- rs_get("https://api.stlouisfed.org/fred/series/observations",
                  query = list(series_id = "EXUSEU", api_key = key, file_type = "json",
                               observation_start = paste0(debut, "-01")))
    o <- jsonlite::fromJSON(txt)$observations
    o <- o[o$value != ".", , drop = FALSE]
    if (nrow(o) == 0) stop("aucune observation")
    v <- as.numeric(o$value)
    stopifnot(all(is.finite(v)), all(v > 0.8 & v < 1.6))
    data.frame(periode = substr(o$date, 1, 7), taux_usd_eur = v,
               source = "FRED EXUSEU (Federal Reserve), source secondaire : BCE indisponible",
               serie_id = "FRED_EXUSEU", stringsAsFactors = FALSE)
  }, error = function(e) { rs_msg("FRED EXUSEU KO : ", sub("api_key=[^&]+", "api_key=****", conditionMessage(e))); NULL })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
