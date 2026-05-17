# =============================================================
# RUBBERSIGNAL.COM — Script 05 : Modèles Thompson
# Auteur  : Martial Sahiri
# Version : 1.0 — Sprint 2 : Monte Carlo TSR20
# Référence : "Simulation, A Modeler's Approach" — J.R. Thompson
# Objectif: Simuler la distribution des prix futurs TSR20
#           et produire des fourchettes probabilistes
# Usage   : source("scripts/05_simulate.R")
# =============================================================

# ── 1. CHARGER LES PACKAGES ──────────────────────────────────

library(jsonlite)
library(tidyverse)
library(lubridate)


# ── 2. PARAMÈTRES GLOBAUX ────────────────────────────────────

DATE_COLLECTE <- Sys.Date()
SEMAINE       <- isoweek(DATE_COLLECTE)
ANNEE         <- year(DATE_COLLECTE)

cat("=== RUBBERSIGNAL — Modèles Thompson du",
    format(DATE_COLLECTE, "%d/%m/%Y"), "===\n\n")


# ── 3. LIRE LE PRIX ACTUEL DEPUIS LE JSON ────────────────────

fichier_json <- paste0("data/processed/rubbersignal_S",
                       SEMAINE, "_", ANNEE, ".json")

if (file.exists(fichier_json)) {
  donnees     <- read_json(fichier_json)
  prix_actuel <- donnees$prix$synthese$prix_actuel %||% 2.29
  cat(">> Prix TSR20 actuel chargé :", prix_actuel, "USD/kg\n\n")
} else {
  prix_actuel <- 2.29
  cat(">> JSON non trouvé — prix de secours :", prix_actuel, "USD/kg\n\n")
}


# ══════════════════════════════════════════════════════════════
# MODULE 1 — MONTE CARLO TSR20
# Mouvement Brownien Géométrique (GBM)
# Référence Thompson : Chapter 4 — Stochastic Simulation
# ══════════════════════════════════════════════════════════════

cat(strrep("─", 55), "\n")
cat("MODULE 1 — MONTE CARLO TSR20 (Mouvement Brownien)\n")
cat(strrep("─", 55), "\n\n")

# ── 3A. PARAMÈTRES DU MODÈLE ─────────────────────────────────
# Calibrés sur données historiques TSR20 2015-2025
# Sources : ANRPC Statistical Bulletin, LGM Malaysia archives

# Drift annuel : tendance long terme du TSR20
# Historique : +3% à +5%/an en tendance haussière structurelle
# Ajustement avec le Pré-RSI si disponible
DRIFT_ANNUEL_BASE <- 0.03   # +3%/an tendance de base

# Ajustement drift selon le Pré-RSI chargé
pre_rsi_val <- if (file.exists(paste0(
  "data/processed/signaux_S", SEMAINE, "_", ANNEE, ".json"))) {
  sig <- read_json(paste0(
    "data/processed/signaux_S", SEMAINE, "_", ANNEE, ".json"))
  sig$pre_rsi$score %||% 50
} else 50

# Le Pré-RSI module le drift :
# RSI > 60 → drift légèrement haussier
# RSI < 40 → drift légèrement baissier
ajust_rsi <- (pre_rsi_val - 50) / 50 * 0.02
DRIFT_ANNUEL <- DRIFT_ANNUEL_BASE + ajust_rsi

# Volatilité annuelle historique du TSR20
# Matières premières agricoles : 20-35% typiquement
# TSR20 : ~25-30% selon les études ANRPC
VOLATILITE_ANNUELLE <- 0.27  # 27% — calibré sur 2015-2025

# Conversion en pas hebdomadaires (52 semaines/an)
dt               <- 1 / 52
DRIFT_HEBDO      <- DRIFT_ANNUEL * dt
VOLATILITE_HEBDO <- VOLATILITE_ANNUELLE * sqrt(dt)

# Paramètres de simulation
N_SIMULATIONS <- 10000  # Nombre de trajectoires Monte Carlo
HORIZONS      <- c(4, 8, 12, 26)  # Semaines : 1 mois, 2 mois, 3 mois, 6 mois

cat("Paramètres du modèle GBM :\n")
cat("  Prix actuel         :", prix_actuel, "USD/kg\n")
cat("  Drift annuel        :", round(DRIFT_ANNUEL * 100, 2), "%/an\n")
cat("  Ajustement Pré-RSI  :", round(ajust_rsi * 100, 2),
    "% (RSI =", pre_rsi_val, ")\n")
cat("  Volatilité annuelle :", VOLATILITE_ANNUELLE * 100, "%\n")
cat("  Volatilité hebdo    :", round(VOLATILITE_HEBDO * 100, 3), "%\n")
cat("  Simulations         :", format(N_SIMULATIONS, big.mark = " "), "\n")
cat("  Horizons            :", paste(HORIZONS, "sem.", collapse = " / "), "\n\n")


# ── 3B. SIMULATION MONTE CARLO ───────────────────────────────

cat(">> Lancement des", format(N_SIMULATIONS, big.mark=" "),
    "simulations...\n")

set.seed(42)  # Reproductibilité des résultats

# Horizon maximum pour la simulation complète
horizon_max <- max(HORIZONS)

# Matrice des trajectoires : N_SIMULATIONS lignes × (horizon_max+1) colonnes
# Chaque ligne est une trajectoire de prix simulée
trajectoires <- matrix(
  NA_real_,
  nrow = N_SIMULATIONS,
  ncol = horizon_max + 1
)
trajectoires[, 1] <- prix_actuel  # Prix initial = prix actuel TSR20

# Mouvement Brownien Géométrique (GBM) — formule d'Itô discrétisée
# ΔS = S × exp((μ - σ²/2) × Δt + σ × √Δt × ε)
# où ε ~ N(0,1) — choc aléatoire hebdomadaire

for (t in 2:(horizon_max + 1)) {
  epsilon <- rnorm(N_SIMULATIONS, mean = 0, sd = 1)
  trajectoires[, t] <- trajectoires[, t-1] * exp(
    (DRIFT_ANNUEL - 0.5 * VOLATILITE_ANNUELLE^2) * dt +
    VOLATILITE_HEBDO * epsilon
  )
}

cat("   Simulation terminée —", N_SIMULATIONS, "trajectoires générées\n\n")


# ── 3C. EXTRACTION DES RÉSULTATS PAR HORIZON ─────────────────

cat(">> Calcul des statistiques par horizon...\n\n")

resultats_mc <- map(HORIZONS, function(h) {

  # Distribution des prix à l'horizon h
  prix_horizon <- trajectoires[, h + 1]

  # Statistiques de distribution
  moy     <- round(mean(prix_horizon), 4)
  med     <- round(median(prix_horizon), 4)
  sd_prix <- round(sd(prix_horizon), 4)

  # Intervalles de confiance (percentiles)
  p05 <- round(quantile(prix_horizon, 0.05), 4)  # 5ème percentile
  p10 <- round(quantile(prix_horizon, 0.10), 4)  # 10ème percentile
  p25 <- round(quantile(prix_horizon, 0.25), 4)  # 1er quartile
  p75 <- round(quantile(prix_horizon, 0.75), 4)  # 3ème quartile
  p90 <- round(quantile(prix_horizon, 0.90), 4)  # 90ème percentile
  p95 <- round(quantile(prix_horizon, 0.95), 4)  # 95ème percentile

  # Probabilités directionnelles
  prob_hausse    <- round(mean(prix_horizon > prix_actuel) * 100, 1)
  prob_baisse    <- round(mean(prix_horizon < prix_actuel) * 100, 1)
  prob_plus5     <- round(mean(prix_horizon > prix_actuel * 1.05) * 100, 1)
  prob_moins5    <- round(mean(prix_horizon < prix_actuel * 0.95) * 100, 1)
  prob_plus10    <- round(mean(prix_horizon > prix_actuel * 1.10) * 100, 1)
  prob_moins10   <- round(mean(prix_horizon < prix_actuel * 0.90) * 100, 1)

  # Scénarios (centiles clés pour publication)
  scenario_bear  <- p10   # Scénario pessimiste
  scenario_base  <- moy   # Scénario central
  scenario_bull  <- p90   # Scénario optimiste

  cat(sprintf("Horizon %2d semaines (%s) :\n", h,
      format(DATE_COLLECTE + weeks(h), "%d/%m/%Y")))
  cat(sprintf("  Scénario pessimiste (10%%)  : %5.3f USD/kg\n", scenario_bear))
  cat(sprintf("  Scénario central (moyenne) : %5.3f USD/kg\n", scenario_base))
  cat(sprintf("  Scénario optimiste (90%%)  : %5.3f USD/kg\n", scenario_bull))
  cat(sprintf("  Fourchette 80%%            : [%5.3f – %5.3f] USD/kg\n",
              p10, p90))
  cat(sprintf("  Prob. hausse vs actuel     : %4.1f%%\n", prob_hausse))
  cat(sprintf("  Prob. +5%% ou plus         : %4.1f%%\n", prob_plus5))
  cat(sprintf("  Prob. -5%% ou plus         : %4.1f%%\n\n", prob_moins5))

  list(
    horizon_sem     = h,
    date_horizon    = format(DATE_COLLECTE + weeks(h), "%Y-%m-%d"),
    prix_actuel     = prix_actuel,
    moyenne         = moy,
    mediane         = med,
    ecart_type      = sd_prix,
    p05 = p05, p10 = p10, p25 = p25,
    p75 = p75, p90 = p90, p95 = p95,
    scenario_bear   = scenario_bear,
    scenario_base   = scenario_base,
    scenario_bull   = scenario_bull,
    prob_hausse_pct = prob_hausse,
    prob_baisse_pct = prob_baisse,
    prob_plus5_pct  = prob_plus5,
    prob_moins5_pct = prob_moins5,
    prob_plus10_pct = prob_plus10,
    prob_moins10_pct= prob_moins10
  )
})

names(resultats_mc) <- paste0("S", HORIZONS)


# ── 3D. RÉSUMÉ ÉDITORIAL POUR LE RAPPORT ─────────────────────
# Génère le texte prêt à insérer dans le rapport Substack

cat(strrep("─", 55), "\n")
cat("TEXTE ÉDITORIAL — Prévisions Monte Carlo\n")
cat(strrep("─", 55), "\n\n")

generer_texte_mc_fr <- function(res4, res12) {

  # Interprétation de la tendance
  biais <- if (res4$prob_hausse_pct > 60) "haussier"
            else if (res4$prob_hausse_pct < 40) "baissier"
            else "neutre"

  paste0(
    "### 📈 Prévisions Monte Carlo — ", N_SIMULATIONS, " simulations\n\n",
    "*Basé sur le Mouvement Brownien Géométrique (Thompson, 2000)*\n\n",
    "| Horizon | Scénario pessimiste | Scénario central | Scénario optimiste | Prob. hausse |\n",
    "|---|---|---|---|---|\n",
    sprintf("| **4 semaines** | %.3f USD/kg | **%.3f USD/kg** | %.3f USD/kg | %s%% |\n",
            res4$scenario_bear, res4$scenario_base,
            res4$scenario_bull, res4$prob_hausse_pct),
    sprintf("| **8 semaines** | %.3f USD/kg | **%.3f USD/kg** | %.3f USD/kg | %s%% |\n",
            resultats_mc$S8$scenario_bear, resultats_mc$S8$scenario_base,
            resultats_mc$S8$scenario_bull, resultats_mc$S8$prob_hausse_pct),
    sprintf("| **12 semaines** | %.3f USD/kg | **%.3f USD/kg** | %.3f USD/kg | %s%% |\n",
            res12$scenario_bear, res12$scenario_base,
            res12$scenario_bull, res12$prob_hausse_pct),
    "\n",
    "**Lecture :** Le modèle indique un biais **", biais,
    "** à 4 semaines, avec ",
    res4$prob_hausse_pct, "% de probabilité de hausse ",
    "et une fourchette 80% entre ",
    res4$p10, " et ", res4$p90, " USD/kg. ",
    "À 12 semaines, l'incertitude s'élargit : fourchette 80% entre ",
    res12$p10, " et ", res12$p90, " USD/kg.\n\n",
    "> *Ces projections sont issues d'une simulation stochastique. ",
    "Elles ne constituent pas un conseil en investissement. ",
    "La volatilité réelle peut différer des paramètres historiques.*\n\n"
  )
}

generer_texte_mc_en <- function(res4, res12) {

  biais_en <- if (res4$prob_hausse_pct > 60) "bullish"
              else if (res4$prob_hausse_pct < 40) "bearish"
              else "neutral"

  paste0(
    "### 📈 Monte Carlo Forecasts — ", N_SIMULATIONS, " simulations\n\n",
    "*Based on Geometric Brownian Motion (Thompson, 2000)*\n\n",
    "| Horizon | Bear scenario | Base scenario | Bull scenario | Upside prob. |\n",
    "|---|---|---|---|---|\n",
    sprintf("| **4 weeks** | %.3f USD/kg | **%.3f USD/kg** | %.3f USD/kg | %s%% |\n",
            res4$scenario_bear, res4$scenario_base,
            res4$scenario_bull, res4$prob_hausse_pct),
    sprintf("| **8 weeks** | %.3f USD/kg | **%.3f USD/kg** | %.3f USD/kg | %s%% |\n",
            resultats_mc$S8$scenario_bear, resultats_mc$S8$scenario_base,
            resultats_mc$S8$scenario_bull, resultats_mc$S8$prob_hausse_pct),
    sprintf("| **12 weeks** | %.3f USD/kg | **%.3f USD/kg** | %.3f USD/kg | %s%% |\n",
            res12$scenario_bear, res12$scenario_base,
            res12$scenario_bull, res12$prob_hausse_pct),
    "\n",
    "**Reading:** The model shows a **", biais_en,
    "** bias at 4 weeks, with ",
    res4$prob_hausse_pct, "% probability of upside ",
    "and an 80% confidence range of ",
    res4$p10, " to ", res4$p90, " USD/kg. ",
    "At 12 weeks, uncertainty widens: 80% range from ",
    res12$p10, " to ", res12$p90, " USD/kg.\n\n",
    "> *These projections are generated by stochastic simulation. ",
    "They do not constitute investment advice. ",
    "Actual volatility may differ from historical parameters.*\n\n"
  )
}

texte_mc_fr <- generer_texte_mc_fr(resultats_mc$S4, resultats_mc$S12)
texte_mc_en <- generer_texte_mc_en(resultats_mc$S4, resultats_mc$S12)

cat(texte_mc_fr)


# ── 3E. SAUVEGARDER LES RÉSULTATS ────────────────────────────

cat(">> Sauvegarde des résultats Monte Carlo...\n")

json_mc <- list(
  meta = list(
    date_simulation  = as.character(DATE_COLLECTE),
    semaine          = SEMAINE,
    annee            = ANNEE,
    n_simulations    = N_SIMULATIONS,
    modele           = "GBM — Thompson (2000)",
    prix_actuel      = prix_actuel,
    drift_annuel     = DRIFT_ANNUEL,
    volatilite_an    = VOLATILITE_ANNUELLE,
    pre_rsi_utilise  = pre_rsi_val
  ),
  resultats      = resultats_mc,
  texte_fr       = texte_mc_fr,
  texte_en       = texte_mc_en
)

fichier_mc <- paste0("data/processed/monte_carlo_S",
                     SEMAINE, "_", ANNEE, ".json")
write_json(json_mc, fichier_mc, pretty = TRUE, auto_unbox = TRUE)
cat("   JSON Monte Carlo :", fichier_mc, "\n")

# Enrichir le JSON principal
if (file.exists(fichier_json)) {
  jp <- read_json(fichier_json)
  jp$monte_carlo <- json_mc
  write_json(jp, fichier_json, pretty = TRUE, auto_unbox = TRUE)
  cat("   JSON principal enrichi\n")
}


# ── 3F. RÉSUMÉ FINAL ─────────────────────────────────────────

cat("\n", strrep("★", 35), "\n")
cat("MONTE CARLO TSR20 — Semaine", SEMAINE, "/", ANNEE, "\n")
cat(strrep("★", 35), "\n\n")
cat("Prix actuel   :", prix_actuel, "USD/kg\n")
cat("Drift annuel  :", round(DRIFT_ANNUEL * 100, 2), "%/an",
    "(ajusté RSI:", pre_rsi_val, ")\n\n")

for (h in HORIZONS) {
  r <- resultats_mc[[paste0("S", h)]]
  cat(sprintf("S+%2d sem. : Bear %5.3f | Base %5.3f | Bull %5.3f | P(↑) %4.1f%%\n",
      h, r$scenario_bear, r$scenario_base,
      r$scenario_bull, r$prob_hausse_pct))
}

cat("\nFichier MC :", fichier_mc, "\n\n")


# ══════════════════════════════════════════════════════════════
# MODULE 2 — BOOTSTRAP RSI
# Intervalle de confiance sur le Pré-RSI
# Référence Thompson : Chapter 6 — Bootstrap Methods
# ══════════════════════════════════════════════════════════════

cat(strrep("─", 55), "\n")
cat("MODULE 2 — BOOTSTRAP RSI (Thompson, Ch.6)\n")
cat(strrep("─", 55), "\n\n")

# ── PRINCIPE DU BOOTSTRAP (Thompson) ─────────────────────────
# Le Bootstrap répond à cette question :
# "Si on recalculait le Pré-RSI avec des données légèrement
#  différentes, dans quelle fourchette tomberait-il ?"
# On rééchantillonne les composantes du RSI avec remise
# pour estimer l'incertitude statistique du score.

cat("Principe : rééchantillonnage des composantes du Pré-RSI\n")
cat("pour estimer l'intervalle de confiance du score.\n\n")

# ── CHARGER LES COMPOSANTES DU PRÉ-RSI ───────────────────────

fichier_signaux <- paste0("data/processed/signaux_S",
                          SEMAINE, "_", ANNEE, ".json")

if (!file.exists(fichier_signaux)) {
  cat(">> ATTENTION : fichier signaux introuvable\n")
  cat("   Lancez scripts/04_collect_signals.R d'abord\n\n")
  rsi_bootstrap <- list(
    disponible = FALSE,
    note = "Script 04 requis"
  )
} else {

  signaux <- read_json(fichier_signaux)

  # Extraire les 5 composantes pondérées du Pré-RSI
  # (mêmes que dans le script 04)
  score_meteo   <- signaux$module1_meteo$score_offre_mondiale %||% 50
  score_devise  <- {
    cny <- signaux$module2_devises$USD_CNY %||% 7.10
    ecart <- (cny - 7.10) / 7.10 * 100
    if (ecart < -2) 70 else if (ecart > 2) 30 else 50
  }
  score_demande <- signaux$module3_demande_aval$score_demande %||% 50
  score_terrain <- (signaux$module6_terrain_ci$sentiment_planteurs
                    %||% 3) / 5 * 100
  score_geo     <- {
    nb <- signaux$module7_geopolitique$nb_articles %||% 0
    if (nb == 0) 65 else if (nb <= 2) 50 else if (nb <= 4) 35 else 20
  }

  # Poids des composantes (identiques au script 04)
  poids <- c(0.25, 0.20, 0.25, 0.15, 0.15)
  scores_base <- c(score_meteo, score_devise, score_demande,
                   score_terrain, score_geo)
  noms_scores <- c("Météo/Offre", "Devises", "Demande aval",
                   "Terrain CI", "Géopolitique")

  # Pré-RSI calculé (vérification)
  pre_rsi_calc <- round(sum(scores_base * poids))
  cat(">> Composantes du Pré-RSI :\n")
  for (i in 1:5) {
    cat(sprintf("   %-15s : %5.1f pts × %.2f = %4.1f pts\n",
        noms_scores[i], scores_base[i], poids[i],
        scores_base[i] * poids[i]))
  }
  cat(sprintf("   %-15s : %5.1f / 100\n", "PRÉ-RSI TOTAL", pre_rsi_calc))
  cat("\n")

  # ── BOOTSTRAP : 5000 rééchantillonnages ──────────────────────
  # Thompson recommande 1000-5000 pour les intervalles de confiance

  N_BOOT <- 5000
  cat(">> Bootstrap :", N_BOOT, "rééchantillonnages...\n")

  set.seed(123)

  # Source d'incertitude : chaque composante est connue avec une
  # incertitude estimée (bruit de mesure / subjectivité)
  # On modélise l'incertitude par composante :
  incertitude <- c(
    meteo   = 8,   # ±8 pts — données météo objectives mais extrapolées
    devise  = 5,   # ±5 pts — données précises mais interprétation subjective
    demande = 10,  # ±10 pts — PMI approximatif (manuel)
    terrain = 12,  # ±12 pts — donnée subjective (sentiment planteurs 1-5)
    geo     = 8    # ±8 pts — signal binaire (nb articles)
  )

  rsi_boot_scores <- numeric(N_BOOT)

  for (b in 1:N_BOOT) {
    # Rééchantillonner chaque composante avec bruit gaussien
    scores_perturbes <- pmax(0, pmin(100,
      scores_base + rnorm(5, mean = 0, sd = incertitude)
    ))
    rsi_boot_scores[b] <- sum(scores_perturbes * poids)
  }

  # Statistiques du Bootstrap
  rsi_moy  <- round(mean(rsi_boot_scores), 1)
  rsi_med  <- round(median(rsi_boot_scores), 1)
  rsi_sd   <- round(sd(rsi_boot_scores), 1)
  rsi_ic90_bas  <- round(quantile(rsi_boot_scores, 0.05), 1)
  rsi_ic90_haut <- round(quantile(rsi_boot_scores, 0.95), 1)
  rsi_ic80_bas  <- round(quantile(rsi_boot_scores, 0.10), 1)
  rsi_ic80_haut <- round(quantile(rsi_boot_scores, 0.90), 1)

  # Stabilité du signal
  stabilite <- if (rsi_sd < 5) "Très stable"
               else if (rsi_sd < 8) "Stable"
               else if (rsi_sd < 12) "Modérément stable"
               else "Incertain"

  # Signal robuste si les bornes IC90 restent du même côté de 50
  signal_robuste <- (rsi_ic90_bas > 50 && rsi_ic90_haut > 50) ||
                    (rsi_ic90_bas < 50 && rsi_ic90_haut < 50)

  cat("   Bootstrap terminé —", N_BOOT, "rééchantillonnages\n\n")

  cat(">> Résultats Bootstrap RSI :\n")
  cat(sprintf("   Pré-RSI central       : %5.1f / 100\n", pre_rsi_calc))
  cat(sprintf("   Moyenne bootstrap     : %5.1f / 100\n", rsi_moy))
  cat(sprintf("   Écart-type            : ± %4.1f pts\n", rsi_sd))
  cat(sprintf("   IC 80%%               : [%4.1f – %4.1f]\n",
              rsi_ic80_bas, rsi_ic80_haut))
  cat(sprintf("   IC 90%%               : [%4.1f – %4.1f]\n",
              rsi_ic90_bas, rsi_ic90_haut))
  cat(sprintf("   Stabilité du signal   : %s\n", stabilite))
  cat(sprintf("   Signal robuste (IC90) : %s\n\n",
              if (signal_robuste) "OUI — signal fiable" else
              "NON — signal incertain (zone 50)"))

  # ── TEXTE ÉDITORIAL BOOTSTRAP ────────────────────────────────

  generer_texte_boot_fr <- function() {
    paste0(
      "### 🎯 Fiabilité du Pré-RSI — Analyse Bootstrap\n\n",
      "*", N_BOOT, " rééchantillonnages (Thompson, 2000)*\n\n",
      "| Indicateur | Valeur |\n",
      "|---|---|\n",
      "| **Pré-RSI central** | **", pre_rsi_calc, " / 100** |\n",
      "| Intervalle confiance 80% | [", rsi_ic80_bas,
        " – ", rsi_ic80_haut, "] |\n",
      "| Intervalle confiance 90% | [", rsi_ic90_bas,
        " – ", rsi_ic90_haut, "] |\n",
      "| Stabilité du signal | ", stabilite, " (±", rsi_sd, " pts) |\n",
      "| Signal robuste | ",
        if (signal_robuste) "✅ Oui" else "⚠️ Zone d'incertitude",
        " |\n\n",
      "**Interprétation :** Le Pré-RSI de **", pre_rsi_calc,
      "/100** est ",
      if (signal_robuste)
        paste0("statistiquement robuste — même en faisant varier ",
               "les hypothèses, le signal reste **",
               if (pre_rsi_calc >= 55) "haussier" else
               if (pre_rsi_calc <= 45) "baissier" else "neutre", "**.")
      else
        "dans une zone d'incertitude — le signal pourrait basculer en fonction des données terrain CI de la semaine prochaine.",
      "\n\n",
      "> *Le Bootstrap estime la robustesse du score face à ",
      "l'incertitude de mesure de chaque composante. ",
      "Un IC90 étroit indique un signal fiable.*\n\n"
    )
  }

  generer_texte_boot_en <- function() {
    paste0(
      "### 🎯 Pre-RSI Reliability — Bootstrap Analysis\n\n",
      "*", N_BOOT, " resamples (Thompson, 2000)*\n\n",
      "| Indicator | Value |\n",
      "|---|---|\n",
      "| **Pre-RSI score** | **", pre_rsi_calc, " / 100** |\n",
      "| 80% confidence interval | [", rsi_ic80_bas,
        " – ", rsi_ic80_haut, "] |\n",
      "| 90% confidence interval | [", rsi_ic90_bas,
        " – ", rsi_ic90_haut, "] |\n",
      "| Signal stability | ", stabilite, " (±", rsi_sd, " pts) |\n",
      "| Robust signal | ",
        if (signal_robuste) "✅ Yes" else "⚠️ Uncertainty zone",
        " |\n\n",
      "**Interpretation:** The Pre-RSI of **", pre_rsi_calc,
      "/100** is ",
      if (signal_robuste)
        paste0("statistically robust — even when varying ",
               "the assumptions, the signal remains **",
               if (pre_rsi_calc >= 55) "bullish" else
               if (pre_rsi_calc <= 45) "bearish" else "neutral", "**.")
      else
        "in an uncertainty zone — the signal could shift depending on next week's CI field data.",
      "\n\n",
      "> *Bootstrap estimates score robustness against measurement ",
      "uncertainty in each component. ",
      "A narrow IC90 indicates a reliable signal.*\n\n"
    )
  }

  texte_boot_fr <- generer_texte_boot_fr()
  texte_boot_en <- generer_texte_boot_en()

  cat(">> Texte éditorial Bootstrap généré\n\n")
  cat(texte_boot_fr)

  # ── SAUVEGARDER ───────────────────────────────────────────────

  rsi_bootstrap <- list(
    disponible       = TRUE,
    n_bootstrap      = N_BOOT,
    pre_rsi_central  = pre_rsi_calc,
    moyenne_boot     = rsi_moy,
    ecart_type       = rsi_sd,
    ic80_bas         = rsi_ic80_bas,
    ic80_haut        = rsi_ic80_haut,
    ic90_bas         = rsi_ic90_bas,
    ic90_haut        = rsi_ic90_haut,
    stabilite        = stabilite,
    signal_robuste   = signal_robuste,
    composantes      = setNames(
      as.list(scores_base), noms_scores),
    incertitudes     = as.list(incertitude),
    texte_fr         = texte_boot_fr,
    texte_en         = texte_boot_en
  )
}

# Enrichir le JSON Monte Carlo avec le Bootstrap
json_mc$bootstrap_rsi <- rsi_bootstrap
write_json(json_mc, fichier_mc, pretty = TRUE, auto_unbox = TRUE)
cat(">> JSON enrichi avec Bootstrap RSI\n")

# Enrichir le JSON principal
if (file.exists(fichier_json)) {
  jp <- read_json(fichier_json)
  jp$monte_carlo <- json_mc
  write_json(jp, fichier_json, pretty = TRUE, auto_unbox = TRUE)
  cat(">> JSON principal mis à jour\n")
}


# ══════════════════════════════════════════════════════════════
# RÉSUMÉ FINAL SCRIPT 05
# ══════════════════════════════════════════════════════════════

cat("\n", strrep("=", 60), "\n")
cat("RÉSUMÉ SCRIPT 05 — Semaine", SEMAINE, "/", ANNEE, "\n")
cat(strrep("=", 60), "\n\n")

cat("MODULE 1 — MONTE CARLO TSR20 :\n")
for (h in HORIZONS) {
  r <- resultats_mc[[paste0("S", h)]]
  cat(sprintf("  S+%2d sem. : [%5.3f – %5.3f] | Base %5.3f | P(↑) %4.1f%%\n",
      h, r$p10, r$p90, r$scenario_base, r$prob_hausse_pct))
}

cat("\nMODULE 2 — BOOTSTRAP RSI :\n")
if (rsi_bootstrap$disponible) {
  cat(sprintf("  Pré-RSI     : %5.1f / 100\n",
      rsi_bootstrap$pre_rsi_central))
  cat(sprintf("  IC 90%%      : [%4.1f – %4.1f]\n",
      rsi_bootstrap$ic90_bas, rsi_bootstrap$ic90_haut))
  cat(sprintf("  Stabilité   : %s\n", rsi_bootstrap$stabilite))
  cat(sprintf("  Robustesse  : %s\n",
      if (rsi_bootstrap$signal_robuste) "✅ Signal fiable"
      else "⚠️ Zone d'incertitude"))
} else {
  cat("  Non disponible — relancer script 04 d'abord\n")
}

cat("\n", strrep("=", 60), "\n")
cat("Fichier MC :", fichier_mc, "\n\n")
cat("Prochaine étape : Module 3 — Scénarios météo (what-if)\n")
cat("Puis : intégrer MC + Bootstrap dans script 03\n\n")
