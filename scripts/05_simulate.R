# =============================================================
# RUBBERSIGNAL.COM — Script 05 : Modèles Thompson
# Auteur  : Martial Sahiri
# Version : 2.0 — mission 2 (nov. 2026)
#   - volatilité ESTIMÉE sur l'historique mensuel Banque mondiale TSR20
#     (data JSON : prix$historique_bm), plus de 27 % / 3 % codés en dur ;
#   - simulation SANS DÉRIVE : fourchette de volatilité, pas une prévision
#     de hausse ou de baisse (plus d'ajustement par le Pré-RSI, plus de
#     « probabilité de hausse ») ;
#   - bootstrap branché sur le Pré-RSI v2 du script 04 (plus de %||% 50) ;
#   - scénarios météo (élasticité -0.40, chocs d'offre sans source) RETIRÉS.
#   (1.1 — sans prix de secours ni terrain par défaut ; 1.0 — Sprint 2)
# Référence : "Simulation, A Modeler's Approach" — J.R. Thompson
# Usage   : source("scripts/05_simulate.R")
# =============================================================

# ── 1. CHARGER LES PACKAGES ──────────────────────────────────

library(jsonlite)
library(tidyverse)
library(lubridate)

source(if (file.exists("scripts/00_regles.R")) "scripts/00_regles.R" else "00_regles.R")


# ── 2. PARAMÈTRES GLOBAUX ────────────────────────────────────

DATE_COLLECTE <- Sys.Date()
SEMAINE       <- isoweek(DATE_COLLECTE)
ANNEE         <- year(DATE_COLLECTE)

N_SIMULATIONS <- 10000            # trajectoires Monte Carlo
HORIZONS      <- c(4, 8, 12, 26)  # semaines
FENETRE_MOIS  <- 60               # rendements mensuels utilisés (5 ans)
MIN_RENDEMENTS <- 24              # en dessous : pas de simulation

cat("=== RUBBERSIGNAL — Modèles Thompson du",
    format(DATE_COLLECTE, "%d/%m/%Y"), "===\n\n")

fichier_json    <- paste0("data/processed/rubbersignal_S", SEMAINE, "_", ANNEE, ".json")
fichier_signaux <- paste0("data/processed/signaux_S", SEMAINE, "_", ANNEE, ".json")
fichier_mc      <- paste0("data/processed/monte_carlo_S", SEMAINE, "_", ANNEE, ".json")

if (!file.exists(fichier_json))
  stop("JSON introuvable : ", fichier_json, " - lancer le script 01 d'abord.")
donnees <- read_json(fichier_json)

num1 <- function(x) { v <- suppressWarnings(as.numeric(unlist(x %||% NA))); if (length(v) == 1) v else NA_real_ }

ecrire <- function(json_mc) {
  write_json(json_mc, fichier_mc, pretty = TRUE, auto_unbox = TRUE, na = "null", null = "null")
  jp <- read_json(fichier_json)
  jp$monte_carlo <- json_mc
  write_json(jp, fichier_json, pretty = TRUE, auto_unbox = TRUE, na = "null", null = "null")
}


# ══════════════════════════════════════════════════════════════
# MODULE 1 — FOURCHETTE DE VOLATILITÉ TSR20 (Monte Carlo, GBM sans dérive)
# ══════════════════════════════════════════════════════════════

cat(strrep("─", 55), "\n")
cat("MODULE 1 — FOURCHETTE DE VOLATILITÉ TSR20\n")
cat(strrep("─", 55), "\n\n")

# ── 1A. Volatilité estimée sur l'historique Banque mondiale ──
hb  <- donnees$prix$historique_bm %||% list()
per <- vapply(hb, function(x) as.character(x$periode %||% NA), character(1))
px  <- vapply(hb, function(x) num1(x$prix_usd), numeric(1))
vol <- rs_volatilite_historique(per, px, n_mois = FENETRE_MOIS, min_obs = MIN_RENDEMENTS)

# ── 1B. Point de départ (daté, sourcé, conforme au réglage licence) ──
publier_licence <- rs_publier_sous_licence()
syn    <- donnees$prix$synthese
hebdo  <- num1(syn$prix_actuel)                       # NA si > 14 jours
serie_hebdo <- as.character(syn$indice %||% NA)
bm_ref <- donnees$prix$reference_mensuelle$bm_tsr20
depart <- if (is.finite(hebdo) && (publier_licence || rs_serie_publiable(serie_hebdo))) {
  list(valeur = hebdo, source = as.character(syn$source), date = as.character(syn$date_source),
       serie = serie_hebdo, type = "cotation hebdomadaire")
} else if (is.finite(num1(bm_ref$valeur)) && identical(as.character(bm_ref$statut %||% ""), "derniere_valeur_disponible")) {
  list(valeur = num1(bm_ref$valeur), source = as.character(bm_ref$source),
       date = as.character(bm_ref$periode), serie = "BM_TSR20", type = "moyenne mensuelle Banque mondiale")
} else NULL

non_calcule <- function(raison) {
  cat("::warning::Monte Carlo non calcule : ", raison, "\n", sep = "")
  msg_fr <- paste0("> *Fourchette de volatilité non calculée cette semaine : ", raison, ".*\n\n")
  msg_en <- paste0("> *Volatility range not computed this week: ",
                   if (grepl("historique", raison)) "insufficient World Bank monthly history"
                   else "no dated, publishable starting price", ".*\n\n")
  list(disponible = FALSE, raison = raison, texte_fr = msg_fr, texte_en = msg_en,
       bootstrap_rsi = list(disponible = FALSE, note = "non calcule"),
       scenarios_meteo = list(publie = FALSE, statut = "retire",
                              note = "Elasticite et chocs d'offre sans source : scenarios retires du rapport (v2.0)"))
}

if (is.null(vol)) {
  json_mc <- non_calcule(sprintf("historique mensuel Banque mondiale TSR20 insuffisant (moins de %d rendements consécutifs)", MIN_RENDEMENTS))
} else if (is.null(depart)) {
  json_mc <- non_calcule("aucun prix de départ daté et publiable (cotation hebdomadaire absente, périmée ou sous licence, et référence Banque mondiale périmée)")
} else {

  VOL  <- vol$volatilite_annuelle
  dt   <- 1 / 52
  prix0 <- depart$valeur
  cat("Volatilité annuelle estimée :", round(VOL * 100, 1), "% (", vol$methode, ")\n")
  cat("Dérive estimée (non utilisée) :", round(vol$derive_annuelle_estimee * 100, 1),
      "%/an ± ", round(vol$erreur_type_derive * 100, 1), "(erreur type)\n")
  cat("Point de départ :", prix0, "USD/kg (", depart$type, ",", depart$source, ",", depart$date, ")\n\n")

  # GBM sans dérive en log : ln(S_t) = ln(S_0) + somme sigma * sqrt(dt) * eps
  # -> médiane = prix de départ ; la fourchette ne mesure que l'amplitude.
  set.seed(42)
  hmax <- max(HORIZONS)
  logret <- matrix(rnorm(N_SIMULATIONS * hmax, 0, VOL * sqrt(dt)), nrow = N_SIMULATIONS)
  cum    <- t(apply(logret, 1, cumsum))

  resultats_mc <- lapply(HORIZONS, function(h) {
    p <- prix0 * exp(cum[, h])
    q <- quantile(p, c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95), names = FALSE)
    list(horizon_sem = h, date_horizon = format(DATE_COLLECTE + weeks(h), "%Y-%m-%d"),
         prix_depart = prix0,
         p05 = round(q[1], 3), p10 = round(q[2], 3), p25 = round(q[3], 3), p50 = round(q[4], 3),
         p75 = round(q[5], 3), p90 = round(q[6], 3), p95 = round(q[7], 3),
         bas80_pct  = round((q[2] / prix0 - 1) * 100, 1),
         haut80_pct = round((q[6] / prix0 - 1) * 100, 1),
         # noms conservés pour le terminal Shiny (app.R) : bornes 10 % / 90 % et médiane
         scenario_bear = round(q[2], 3), scenario_base = round(q[4], 3), scenario_bull = round(q[6], 3))
  })
  names(resultats_mc) <- paste0("S", HORIZONS)
  for (r in resultats_mc)
    cat(sprintf("  %2d sem. : 80 %% entre %.3f et %.3f USD/kg (%+.1f %% / %+.1f %%)\n",
                r$horizon_sem, r$p10, r$p90, r$bas80_pct, r$haut80_pct))

  depart_txt_fr <- paste0(depart$valeur, " USD/kg (", depart$type, ", ", depart$source, ", ",
                          if (depart$serie == "BM_TSR20") rs_mois_libelle(depart$date, "fr") else rs_date_fr(depart$date), ")")
  depart_txt_en <- paste0(depart$valeur, " USD/kg (",
                          if (depart$serie == "BM_TSR20") "World Bank monthly average" else "weekly quote", ", ",
                          depart$source, ", ",
                          if (depart$serie == "BM_TSR20") rs_mois_libelle(depart$date, "en") else rs_date_fr(depart$date), ")")
  ligne <- function(r, lab) sprintf("| %s | %.3f USD/kg (%+.1f %%) | %.3f USD/kg | %.3f USD/kg (%+.1f %%) |\n",
                                    lab, r$p10, r$bas80_pct, r$p50, r$p90, r$haut80_pct)
  texte_mc_fr <- paste0(
    "### 📏 Fourchette de volatilité TSR20 — ", format(N_SIMULATIONS, big.mark = " "), " simulations\n\n",
    "*Ce n'est pas une prévision de hausse ou de baisse : la simulation est centrée sur le prix de départ ",
    "(sans tendance) et montre l'amplitude des variations compatible avec la volatilité historique.*\n\n",
    "| Horizon | Borne basse (10 %) | Médiane | Borne haute (90 %) |\n|---|---|---|---|\n",
    ligne(resultats_mc$S4, "4 semaines"), ligne(resultats_mc$S12, "12 semaines"), ligne(resultats_mc$S26, "26 semaines"),
    "\n> *Volatilité annuelle ", round(VOL * 100, 1), " %, estimée sur ", vol$n_rendements,
    " rendements mensuels (logarithmiques) du TSR20 Banque mondiale, de ", rs_mois_libelle(vol$periode_debut, "fr"),
    " à ", rs_mois_libelle(vol$periode_fin, "fr"), ". Point de départ : ", depart_txt_fr,
    ". Modèle : mouvement brownien géométrique sans dérive (Thompson, 2000). Nature : E (estimation calculée) — ",
    rs_version_methode("volatilite"), ". Ne constitue pas un conseil en investissement.*\n\n")
  ligne_en <- function(r, lab) sprintf("| %s | %.3f USD/kg (%+.1f%%) | %.3f USD/kg | %.3f USD/kg (%+.1f%%) |\n",
                                       lab, r$p10, r$bas80_pct, r$p50, r$p90, r$haut80_pct)
  texte_mc_en <- paste0(
    "### 📏 TSR20 volatility range — ", format(N_SIMULATIONS, big.mark = ","), " simulations\n\n",
    "*This is not a forecast of a rise or a fall: the simulation is centred on the starting price ",
    "(no trend) and shows the range of moves consistent with historical volatility.*\n\n",
    "| Horizon | Lower bound (10%) | Median | Upper bound (90%) |\n|---|---|---|---|\n",
    ligne_en(resultats_mc$S4, "4 weeks"), ligne_en(resultats_mc$S12, "12 weeks"), ligne_en(resultats_mc$S26, "26 weeks"),
    "\n> *Annual volatility ", round(VOL * 100, 1), "%, estimated on ", vol$n_rendements,
    " monthly log returns of the World Bank TSR20, from ", rs_mois_libelle(vol$periode_debut, "en"),
    " to ", rs_mois_libelle(vol$periode_fin, "en"), ". Starting point: ", depart_txt_en,
    ". Model: geometric Brownian motion without drift (Thompson, 2000). Nature: E (calculated estimate) — ",
    rs_version_methode("volatilite"), ". Not investment advice.*\n\n")

  json_mc <- rs_iosco(list(
    disponible = TRUE,
    meta = list(
      date_simulation = as.character(DATE_COLLECTE), semaine = SEMAINE, annee = ANNEE,
      n_simulations   = N_SIMULATIONS,
      modele          = "GBM sans derive (fourchette de volatilite) — Thompson (2000)",
      prix_depart     = prix0, depart = depart,
      volatilite_an   = VOL,
      volatilite_an_pct = round(VOL * 100, 1),
      volatilite_estimation = c(vol, list(serie = "Banque mondiale, Pink Sheet TSR20 (prix$historique_bm)",
                                          fenetre_mois = FENETRE_MOIS)),
      derive_utilisee = 0,
      derive_note     = "Derive estimee conservee pour information, non utilisee : resultat presente comme fourchette de volatilite, pas comme prevision directionnelle",
      publier_sources_sous_licence = publier_licence
    ),
    valeur = VOL, unite = "volatilite annuelle (fraction)", periode = paste(vol$periode_debut, vol$periode_fin, sep = "/"),
    source = "Calcul RubberSignal sur Banque mondiale Pink Sheet TSR20", date_source = as.character(DATE_COLLECTE),
    resultats = resultats_mc, texte_fr = texte_mc_fr, texte_en = texte_mc_en,
    scenarios_meteo = list(publie = FALSE, statut = "retire",
                           note = "Elasticite -0.40 et chocs d'offre sans source : scenarios retires du rapport (v2.0)")
  ), "E", "volatilite")
}


# ══════════════════════════════════════════════════════════════
# MODULE 2 — SENSIBILITÉ DU PRÉ-RSI (bootstrap, Thompson ch. 6)
# Les incertitudes par composante sont une HYPOTHÈSE DE TRAVAIL NON
# CALIBRÉE ; le texte publié le dit explicitement.
# ══════════════════════════════════════════════════════════════

cat("\n", strrep("─", 55), "\n")
cat("MODULE 2 — SENSIBILITÉ DU PRÉ-RSI\n")
cat(strrep("─", 55), "\n\n")

rsi_bootstrap <- list(disponible = FALSE, note = "Script 04 requis")
if (file.exists(fichier_signaux)) {
  pr <- read_json(fichier_signaux)$pre_rsi
  if (!isTRUE(pr$publie)) {
    rsi_bootstrap <- list(disponible = FALSE, note = "Pre-RSI non publie cette semaine (moins de 3 composantes)")
  } else {
    noms   <- names(RS_PRE_RSI_POIDS)
    scores <- vapply(noms, function(k) num1(pr$scores[[k]]), numeric(1))
    poids  <- vapply(noms, function(k) num1(pr$poids_effectifs[[k]]), numeric(1))
    dispo  <- is.finite(scores) & poids > 0
    # Hypothèse de travail (points sur 100), non calibrée
    incertitude <- c(meteo = 8, devises = 5, demande = 10, terrain = 12, geo = 8)
    N_BOOT <- 5000
    set.seed(123)
    central <- round(sum(scores[dispo] * poids[dispo]))
    boot <- replicate(N_BOOT, {
      s <- pmax(0, pmin(100, scores[dispo] + rnorm(sum(dispo), 0, incertitude[dispo])))
      sum(s * poids[dispo])
    })
    q <- round(quantile(boot, c(0.05, 0.10, 0.90, 0.95), names = FALSE), 1)
    classe_c <- rs_sens_prix(central)
    meme_classe <- round(mean(vapply(boot, rs_sens_prix, character(1)) == classe_c) * 100, 1)
    lib_fr <- c(hausse = "haussier", neutre = "neutre", baisse = "baissier")
    lib_en <- c(hausse = "bullish", neutre = "neutral", baisse = "bearish")
    texte_boot_fr <- paste0(
      "### 🎯 Sensibilité du Pré-RSI (hypothèse de travail non calibrée)\n\n",
      "| Indicateur | Valeur |\n|---|---|\n",
      "| Pré-RSI central | ", central, " / 100 |\n",
      "| Intervalle 80 % | [", q[2], " – ", q[3], "] |\n",
      "| Intervalle 90 % | [", q[1], " – ", q[4], "] |\n",
      "| Tirages gardant le même classement (", lib_fr[[classe_c]], ") | ", meme_classe, " % |\n\n",
      "> *", N_BOOT, " tirages : chaque composante disponible est perturbée d'un bruit gaussien dont l'amplitude ",
      "(météo ±8, devises ±5, demande ±10, terrain ±12, géopolitique ±8 points) est une hypothèse de travail ",
      "non calibrée. Ce n'est pas une mesure de fiabilité statistique.*\n\n")
    texte_boot_en <- paste0(
      "### 🎯 Pre-RSI sensitivity (uncalibrated working assumption)\n\n",
      "| Indicator | Value |\n|---|---|\n",
      "| Central Pre-RSI | ", central, " / 100 |\n",
      "| 80% interval | [", q[2], " – ", q[3], "] |\n",
      "| 90% interval | [", q[1], " – ", q[4], "] |\n",
      "| Draws keeping the same class (", lib_en[[classe_c]], ") | ", meme_classe, "% |\n\n",
      "> *", N_BOOT, " draws: each available component is perturbed by Gaussian noise whose size ",
      "(weather ±8, currencies ±5, demand ±10, field ±12, geopolitics ±8 points) is an uncalibrated working ",
      "assumption. This is not a measure of statistical reliability.*\n\n")
    rsi_bootstrap <- list(
      disponible = TRUE, n_bootstrap = N_BOOT, pre_rsi_central = central,
      ic80_bas = q[2], ic80_haut = q[3], ic90_bas = q[1], ic90_haut = q[4],
      classe_centrale = classe_c, pct_meme_classe = meme_classe,
      composantes = as.list(scores), poids_effectifs = as.list(poids),
      incertitudes = as.list(incertitude),
      statut_incertitudes = "hypothese_de_travail_non_calibree",
      texte_fr = texte_boot_fr, texte_en = texte_boot_en)
    cat("Pré-RSI central", central, "| IC80 [", q[2], "-", q[3], "] | même classe :", meme_classe, "%\n")
  }
} else cat(">> fichier signaux introuvable : lancer scripts/04_collect_signals.R\n")

json_mc$bootstrap_rsi <- rsi_bootstrap


# ══════════════════════════════════════════════════════════════
# MODULE 3 — SCÉNARIOS MÉTÉO : RETIRÉS (v2.0)
# L'élasticité prix/offre (-0.40) et les chocs d'offre n'avaient pas de
# source vérifiable (« calibrée sur ANRPC » non documenté). À réintroduire
# seulement avec une estimation sourcée et datée.
# ══════════════════════════════════════════════════════════════

ecrire(json_mc)
cat("\nFichier MC :", fichier_mc, "\n")
cat("Monte Carlo :", if (isTRUE(json_mc$disponible)) "fourchette de volatilité calculée" else json_mc$raison, "\n")
cat("Bootstrap   :", if (isTRUE(rsi_bootstrap$disponible)) "calculé (hypothèse non calibrée)" else rsi_bootstrap$note, "\n")
cat("Scénarios météo : retirés (élasticité et chocs sans source)\n\n")
