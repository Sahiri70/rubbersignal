# =============================================================
# RUBBERSIGNAL TERMINAL — Application Shiny R
# Auteur  : Martial Sahiri
# Version : 1.0 — Sprint 1 : Simulateur Monte Carlo interactif
# Usage   : shiny::runApp("app.R")
# Prérequis : packages shiny, shinydashboard, ggplot2,
#             jsonlite, tidyverse, lubridate, plotly
# =============================================================

# ── 1. PACKAGES ──────────────────────────────────────────────

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(jsonlite)
library(tidyverse)
library(lubridate)


# ── 2. FONCTIONS UTILITAIRES ─────────────────────────────────

# Charger le dernier JSON disponible
charger_dernier_json <- function(dossier = "data/processed") {

  fichiers <- list.files(dossier,
                         pattern = "rubbersignal_S\\d+_\\d+\\.json",
                         full.names = TRUE)

  if (length(fichiers) == 0) return(NULL)

  # Trier par date de modification — prendre le plus récent
  fichier_recent <- fichiers[which.max(file.mtime(fichiers))]
  donnees <- read_json(fichier_recent)
  cat(">> JSON chargé :", fichier_recent, "\n")
  donnees
}

# Simulation Monte Carlo GBM
simuler_monte_carlo <- function(
    prix_actuel,
    drift_annuel,
    volatilite_annuelle,
    n_simulations,
    horizon_semaines,
    seed = 42
) {
  set.seed(seed)
  dt               <- 1 / 52
  volatilite_hebdo <- volatilite_annuelle * sqrt(dt)

  # Matrice des trajectoires
  traj <- matrix(NA_real_,
                 nrow = n_simulations,
                 ncol = horizon_semaines + 1)
  traj[, 1] <- prix_actuel

  for (t in 2:(horizon_semaines + 1)) {
    epsilon   <- rnorm(n_simulations)
    traj[, t] <- traj[, t-1] * exp(
      (drift_annuel - 0.5 * volatilite_annuelle^2) * dt +
      volatilite_hebdo * epsilon
    )
  }
  traj
}

# Calculer les statistiques par horizon
calculer_stats <- function(trajectoires, prix_actuel) {
  n_sem <- ncol(trajectoires) - 1
  map_df(1:n_sem, function(h) {
    prix_h <- trajectoires[, h + 1]
    tibble(
      semaine      = h,
      p05          = quantile(prix_h, 0.05),
      p10          = quantile(prix_h, 0.10),
      p25          = quantile(prix_h, 0.25),
      mediane      = median(prix_h),
      moyenne      = mean(prix_h),
      p75          = quantile(prix_h, 0.75),
      p90          = quantile(prix_h, 0.90),
      p95          = quantile(prix_h, 0.95),
      prob_hausse  = mean(prix_h > prix_actuel) * 100,
      prob_plus5   = mean(prix_h > prix_actuel * 1.05) * 100,
      prob_moins5  = mean(prix_h < prix_actuel * 0.95) * 100
    )
  })
}


# ── 3. INTERFACE UTILISATEUR (UI) ─────────────────────────────

ui <- dashboardPage(

  skin = "black",

  # ── En-tête ──────────────────────────────────────────────────
  dashboardHeader(
    title = tags$span(
      tags$img(src = "https://via.placeholder.com/30",
               style = "margin-right:8px;"),
      "RubberSignal Terminal"
    ),
    titleWidth = 280
  ),

  # ── Barre latérale ────────────────────────────────────────────
  dashboardSidebar(
    width = 280,

    tags$div(
      style = "padding: 15px 15px 5px;",
      tags$p("Simulateur Monte Carlo",
             style = "color:#aaa; font-size:11px; margin:0;
                      text-transform:uppercase; letter-spacing:1px;")
    ),

    sidebarMenu(
      menuItem("Simulateur", tabName = "simulateur",
               icon = icon("chart-line")),
      menuItem("Dashboard", tabName = "dashboard",
               icon = icon("tachometer-alt")),
      menuItem("Corrélations", tabName = "correlations",
               icon = icon("project-diagram")),
      menuItem("Scénarios météo", tabName = "scenarios",
               icon = icon("cloud-rain"))
    ),

    tags$hr(style = "border-color:#444;"),

    # ── Paramètres du simulateur ──────────────────────────────
    tags$div(
      style = "padding: 0 15px;",

      tags$p("Paramètres de simulation",
             style = "color:#aaa; font-size:11px; margin:10px 0 5px;
                      text-transform:uppercase; letter-spacing:1px;"),

      # Prix de départ
      tags$label("Prix de départ (USD/kg)",
                 style = "color:#ddd; font-size:13px;"),
      numericInput("prix_depart", NULL,
                   value = 2.29, min = 0.5, max = 10, step = 0.01,
                   width = "100%"),

      # Drift annuel
      tags$label("Tendance annuelle (%/an)",
                 style = "color:#ddd; font-size:13px; margin-top:10px;
                          display:block;"),
      sliderInput("drift", NULL,
                  min = -10, max = 20, value = 3, step = 0.5,
                  post = "%", width = "100%"),

      # Volatilité
      tags$label("Volatilité (%/an)",
                 style = "color:#ddd; font-size:13px;
                          margin-top:10px; display:block;"),
      sliderInput("volatilite", NULL,
                  min = 5, max = 60, value = 27, step = 1,
                  post = "%", width = "100%"),

      # Horizon
      tags$label("Horizon (semaines)",
                 style = "color:#ddd; font-size:13px;
                          margin-top:10px; display:block;"),
      sliderInput("horizon", NULL,
                  min = 4, max = 52, value = 26, step = 4,
                  post = " sem.", width = "100%"),

      # Nombre de simulations
      tags$label("Simulations",
                 style = "color:#ddd; font-size:13px;
                          margin-top:10px; display:block;"),
      selectInput("n_sim", NULL,
                  choices = c("1 000" = 1000,
                              "5 000" = 5000,
                              "10 000" = 10000),
                  selected = 10000, width = "100%"),

      tags$br(),

      # Bouton lancer
      actionButton("lancer", "▶ Lancer la simulation",
                   style = "width:100%; background:#e67e22;
                            color:white; border:none; padding:10px;
                            font-weight:bold; border-radius:4px;
                            cursor:pointer;"),

      tags$br(), tags$br(),

      # Bouton reset paramètres ANRPC
      actionButton("reset_params", "↺ Paramètres ANRPC",
                   style = "width:100%; background:#333;
                            color:#aaa; border:1px solid #555;
                            padding:8px; border-radius:4px;
                            cursor:pointer;"),

      tags$br(), tags$br()
    )
  ),

  # ── Corps principal ────────────────────────────────────────────
  dashboardBody(

    # CSS personnalisé
    tags$head(
      tags$style(HTML("
        body { background-color: #1a1a2e; }
        .content-wrapper { background-color: #1a1a2e; }
        .box { background-color: #16213e; border:none;
               border-radius:8px; }
        .box-header { color: #eee; }
        .small-box { border-radius:8px; }
        .main-header .logo { background:#0f3460; }
        .main-header .navbar { background:#0f3460; }
        .main-sidebar { background:#0f3460; }
        .sidebar-menu > li > a { color:#ccc; }
        .sidebar-menu > li.active > a { background:#e67e22; color:#fff; }
        .form-control { background:#0f3460; color:#eee;
                        border-color:#334; }
        .irs-bar { background:#e67e22; }
        .irs-bar-edge { background:#e67e22; }
        .irs-slider { background:#e67e22; }
        .irs-single { background:#e67e22; }
        .select2-container .select2-selection--single {
          background:#0f3460; border-color:#334; color:#eee; }
        .info-card { background:#16213e; border-radius:8px;
                     padding:15px; margin-bottom:15px; }
        .metric-label { color:#888; font-size:12px;
                        text-transform:uppercase; letter-spacing:1px; }
        .metric-value { color:#eee; font-size:24px;
                        font-weight:bold; margin:5px 0; }
        .metric-sub { color:#aaa; font-size:13px; }
        .bull { color:#2ecc71; }
        .bear { color:#e74c3c; }
        .neutral { color:#f39c12; }
        hr { border-color:#334; }
      "))
    ),

    tabItems(

      # ══════════════════════════════════════════════════════════
      # ONGLET 1 — SIMULATEUR MONTE CARLO
      # ══════════════════════════════════════════════════════════

      tabItem(tabName = "simulateur",

        # ── Ligne 1 : Métriques clés ──────────────────────────
        fluidRow(

          valueBoxOutput("box_prix_actuel", width = 3),
          valueBoxOutput("box_scenario_base", width = 3),
          valueBoxOutput("box_prob_hausse", width = 3),
          valueBoxOutput("box_fourchette_80", width = 3)
        ),

        # ── Ligne 2 : Graphique principal ─────────────────────
        fluidRow(
          box(
            title = "Distribution des prix simulés — Monte Carlo GBM",
            status = "warning", solidHeader = FALSE,
            width = 8, height = 500,
            plotlyOutput("graphique_mc", height = "420px")
          ),

          box(
            title = "Tableau des résultats",
            status = "primary", solidHeader = FALSE,
            width = 4, height = 500,
            tableOutput("tableau_resultats"),
            tags$hr(),
            tags$p("Référence Thompson (2000)",
                   style = "color:#666; font-size:11px;
                            text-align:center;")
          )
        ),

        # ── Ligne 3 : Distribution finale + Probabilités ──────
        fluidRow(
          box(
            title = "Distribution des prix à l'horizon final",
            status = "info", solidHeader = FALSE,
            width = 6, height = 380,
            plotlyOutput("histogramme_final", height = "300px")
          ),

          box(
            title = "Probabilités directionnelles",
            status = "success", solidHeader = FALSE,
            width = 6, height = 380,
            plotlyOutput("graphique_probabilites", height = "300px")
          )
        ),

        # ── Ligne 4 : Interprétation automatique ──────────────
        fluidRow(
          box(
            title = "Interprétation — RubberSignal Analysis",
            status = "warning", solidHeader = FALSE,
            width = 12,
            uiOutput("interpretation_mc")
          )
        )
      ),

      # ══════════════════════════════════════════════════════════
      # ONGLET 2 — DASHBOARD (à développer Sprint 2)
      # ══════════════════════════════════════════════════════════

      tabItem(tabName = "dashboard",
        fluidRow(
          box(
            title = "Dashboard — Prix & Signaux",
            width = 12,
            tags$p("🚧 En construction — Sprint 2",
                   style = "color:#aaa; text-align:center;
                            padding:40px; font-size:16px;"),
            tags$p("Ce module affichera : prix TSR20 historique,
                   Pré-RSI semaine par semaine, signaux faibles
                   et Bootstrap RSI.",
                   style = "color:#666; text-align:center;")
          )
        )
      ),

      # ══════════════════════════════════════════════════════════
      # ONGLET 3 — CORRÉLATIONS (à développer Sprint 3)
      # ══════════════════════════════════════════════════════════

      tabItem(tabName = "correlations",
        fluidRow(
          box(
            title = "Corrélations interactives",
            width = 12,
            tags$p("🚧 En construction — Sprint 3",
                   style = "color:#aaa; text-align:center;
                            padding:40px; font-size:16px;"),
            tags$p("Ce module affichera : corrélation TSR20 vs
                   USD/CNY, vs pétrole WTI, vs météo zones
                   productrices. Coefficient calculé en temps réel.",
                   style = "color:#666; text-align:center;")
          )
        )
      ),

      # ══════════════════════════════════════════════════════════
      # ONGLET 4 — SCÉNARIOS MÉTÉO (à développer Sprint 4)
      # ══════════════════════════════════════════════════════════

      tabItem(tabName = "scenarios",
        fluidRow(
          box(
            title = "Scénarios météo what-if",
            width = 12,
            tags$p("🚧 En construction — Sprint 4",
                   style = "color:#aaa; text-align:center;
                            padding:40px; font-size:16px;"),
            tags$p("Ce module permettra : choisir l'intensité
                   d'une sécheresse CI, combiner plusieurs chocs
                   simultanément, voir l'impact sur le prix
                   TSR20 à 12 semaines.",
                   style = "color:#666; text-align:center;")
          )
        )
      )
    )
  )
)


# ── 4. SERVEUR (LOGIQUE) ──────────────────────────────────────

server <- function(input, output, session) {

  # ── Charger les données JSON au démarrage ──────────────────
  donnees_json <- reactive({
    charger_dernier_json()
  })

  # ── Initialiser avec les données réelles du pipeline ───────
  observe({
    donnees <- donnees_json()
    if (!is.null(donnees)) {
      prix <- donnees$prix$synthese$prix_actuel
      if (!is.null(prix)) {
        updateNumericInput(session, "prix_depart", value = round(prix, 4))
      }
    }
  })

  # ── Reset vers paramètres ANRPC calibrés ──────────────────
  observeEvent(input$reset_params, {
    updateNumericInput(session, "prix_depart", value = 2.29)
    updateSliderInput(session, "drift",       value = 3)
    updateSliderInput(session, "volatilite",  value = 27)
    updateSliderInput(session, "horizon",     value = 26)
    updateSelectInput(session, "n_sim",       selected = 10000)
  })

  # ── Simulation réactive ────────────────────────────────────
  # Se relance automatiquement quand l'utilisateur clique
  # sur "Lancer" OU quand un paramètre change

  resultats <- eventReactive(
    list(input$lancer,
         input$prix_depart,
         input$drift,
         input$volatilite,
         input$horizon),
    {
      req(input$prix_depart, input$drift,
          input$volatilite, input$horizon)

      # Paramètres
      prix      <- as.numeric(input$prix_depart)
      drift     <- as.numeric(input$drift) / 100
      vol       <- as.numeric(input$volatilite) / 100
      horizon   <- as.numeric(input$horizon)
      n_sim     <- as.numeric(input$n_sim)

      # Simulation
      withProgress(message = "Simulation en cours...",
                   value = 0, {
        incProgress(0.3, detail = paste(
          format(n_sim, big.mark=" "), "trajectoires"))
        traj  <- simuler_monte_carlo(prix, drift, vol,
                                     n_sim, horizon)
        incProgress(0.6, detail = "Calcul des statistiques")
        stats <- calculer_stats(traj, prix)
        incProgress(0.1, detail = "Finalisation")
      })

      list(
        trajectoires = traj,
        stats        = stats,
        prix_actuel  = prix,
        drift        = drift,
        volatilite   = vol,
        horizon      = horizon,
        n_sim        = n_sim
      )
    },
    ignoreNULL = FALSE
  )

  # ── Value Boxes ────────────────────────────────────────────

  output$box_prix_actuel <- renderValueBox({
    r <- resultats()
    valueBox(
      value = paste(round(r$prix_actuel, 4), "USD/kg"),
      subtitle = "Prix de départ TSR20",
      icon  = icon("tag"),
      color = "yellow"
    )
  })

  output$box_scenario_base <- renderValueBox({
    r <- resultats()
    s <- r$stats %>% filter(semaine == r$horizon)
    valueBox(
      value    = paste(round(s$moyenne, 3), "USD/kg"),
      subtitle = paste("Scénario central à", r$horizon, "semaines"),
      icon     = icon("chart-line"),
      color    = "orange"
    )
  })

  output$box_prob_hausse <- renderValueBox({
    r <- resultats()
    s <- r$stats %>% filter(semaine == 4)
    prob <- round(s$prob_hausse, 1)
    valueBox(
      value    = paste0(prob, "%"),
      subtitle = "Probabilité de hausse à 4 semaines",
      icon     = icon("arrow-up"),
      color    = if (prob > 55) "green" else if (prob < 45) "red" else "yellow"
    )
  })

  output$box_fourchette_80 <- renderValueBox({
    r <- resultats()
    s <- r$stats %>% filter(semaine == r$horizon)
    valueBox(
      value    = paste0("[", round(s$p10, 3), " – ",
                        round(s$p90, 3), "]"),
      subtitle = paste("Fourchette 80% à", r$horizon, "semaines"),
      icon     = icon("arrows-alt-h"),
      color    = "blue"
    )
  })

  # ── Graphique Monte Carlo principal ───────────────────────

  output$graphique_mc <- renderPlotly({
    r <- resultats()
    s <- r$stats

    # Créer les semaines
    semaines <- 0:r$horizon

    fig <- plot_ly() %>%

      # Zone IC 80% (p10-p90) — fond orange transparent
      add_ribbons(
        x = ~c(s$semaine, rev(s$semaine)),
        y = ~c(s$p10, rev(s$p90)),
        fillcolor = "rgba(230,126,34,0.15)",
        line = list(color = "transparent"),
        name = "Fourchette 80%",
        hoverinfo = "skip"
      ) %>%

      # Zone IC 60% (p25-p75) — fond orange plus dense
      add_ribbons(
        x = ~c(s$semaine, rev(s$semaine)),
        y = ~c(s$p25, rev(s$p75)),
        fillcolor = "rgba(230,126,34,0.25)",
        line = list(color = "transparent"),
        name = "Fourchette 60%",
        hoverinfo = "skip"
      ) %>%

      # Scénario pessimiste (p10)
      add_lines(
        x = ~s$semaine, y = ~s$p10,
        line = list(color = "#e74c3c", dash = "dash", width = 1.5),
        name = "Bear (P10)",
        hovertemplate = "Sem. %{x} | Bear: %{y:.3f} USD/kg<extra></extra>"
      ) %>%

      # Scénario central (moyenne)
      add_lines(
        x = ~s$semaine, y = ~s$moyenne,
        line = list(color = "#e67e22", width = 2.5),
        name = "Base (moyenne)",
        hovertemplate = "Sem. %{x} | Base: %{y:.3f} USD/kg<extra></extra>"
      ) %>%

      # Scénario optimiste (p90)
      add_lines(
        x = ~s$semaine, y = ~s$p90,
        line = list(color = "#2ecc71", dash = "dash", width = 1.5),
        name = "Bull (P90)",
        hovertemplate = "Sem. %{x} | Bull: %{y:.3f} USD/kg<extra></extra>"
      ) %>%

      # Prix actuel (ligne horizontale)
      add_lines(
        x = ~c(0, r$horizon),
        y = ~c(r$prix_actuel, r$prix_actuel),
        line = list(color = "#ffffff", dash = "dot", width = 1),
        name = "Prix actuel",
        hoverinfo = "skip"
      ) %>%

      layout(
        paper_bgcolor = "#16213e",
        plot_bgcolor  = "#16213e",
        font   = list(color = "#eee"),
        xaxis  = list(
          title      = "Semaines",
          gridcolor  = "#334",
          zeroline   = FALSE,
          tickcolor  = "#666"
        ),
        yaxis  = list(
          title      = "Prix TSR20 (USD/kg)",
          gridcolor  = "#334",
          zeroline   = FALSE,
          tickcolor  = "#666",
          tickformat = ".3f"
        ),
        legend = list(
          bgcolor     = "rgba(0,0,0,0.3)",
          bordercolor = "#334",
          x = 0.01, y = 0.99
        ),
        hovermode = "x unified",
        margin = list(l=60, r=20, t=20, b=50)
      )

    fig
  })

  # ── Tableau des résultats ─────────────────────────────────

  output$tableau_resultats <- renderTable({
    r <- resultats()
    s <- r$stats %>%
      filter(semaine %in% c(4, 8, 12, 26, r$horizon)) %>%
      distinct(semaine, .keep_all = TRUE) %>%
      mutate(
        Horizon    = paste(semaine, "sem."),
        Bear       = round(p10, 3),
        Base       = round(moyenne, 3),
        Bull       = round(p90, 3),
        `P(↑)%`    = round(prob_hausse, 1)
      ) %>%
      select(Horizon, Bear, Base, Bull, `P(↑)%`)
  },
  striped = TRUE,
  hover   = TRUE,
  bordered = FALSE,
  style   = "color:#eee; background:#16213e;")

  # ── Histogramme final ─────────────────────────────────────

  output$histogramme_final <- renderPlotly({
    r <- resultats()
    prix_final <- r$trajectoires[, r$horizon + 1]

    fig <- plot_ly(
      x    = ~prix_final,
      type = "histogram",
      nbinsx = 80,
      marker = list(
        color = "rgba(230,126,34,0.7)",
        line  = list(color = "rgba(230,126,34,0.3)", width = 0.5)
      ),
      name = "Distribution des prix"
    ) %>%

    # Ligne prix actuel
    add_lines(
      x = c(r$prix_actuel, r$prix_actuel),
      y = c(0, r$n_sim / 8),
      line = list(color = "#fff", dash = "dot", width = 2),
      name = "Prix actuel"
    ) %>%

    layout(
      paper_bgcolor = "#16213e",
      plot_bgcolor  = "#16213e",
      font   = list(color = "#eee"),
      xaxis  = list(title = "Prix TSR20 (USD/kg)",
                    gridcolor = "#334", tickformat = ".3f"),
      yaxis  = list(title = "Fréquence", gridcolor = "#334"),
      showlegend = FALSE,
      margin = list(l=60, r=20, t=10, b=50)
    )

    fig
  })

  # ── Graphique probabilités ────────────────────────────────

  output$graphique_probabilites <- renderPlotly({
    r <- resultats()
    s <- r$stats

    fig <- plot_ly() %>%

      add_lines(
        x = ~s$semaine, y = ~s$prob_hausse,
        line = list(color = "#3498db", width = 2),
        name = "P(hausse)",
        hovertemplate = "Sem. %{x} | P(↑): %{y:.1f}%<extra></extra>"
      ) %>%

      add_lines(
        x = ~s$semaine, y = ~s$prob_plus5,
        line = list(color = "#2ecc71", width = 1.5, dash = "dash"),
        name = "P(+5% ou plus)",
        hovertemplate = "Sem. %{x} | P(+5%): %{y:.1f}%<extra></extra>"
      ) %>%

      add_lines(
        x = ~s$semaine, y = ~s$prob_moins5,
        line = list(color = "#e74c3c", width = 1.5, dash = "dash"),
        name = "P(-5% ou plus)",
        hovertemplate = "Sem. %{x} | P(-5%): %{y:.1f}%<extra></extra>"
      ) %>%

      # Ligne 50%
      add_lines(
        x = c(0, r$horizon),
        y = c(50, 50),
        line = list(color = "#666", dash = "dot", width = 1),
        name = "Neutre (50%)",
        hoverinfo = "skip"
      ) %>%

      layout(
        paper_bgcolor = "#16213e",
        plot_bgcolor  = "#16213e",
        font   = list(color = "#eee"),
        xaxis  = list(title = "Semaines", gridcolor = "#334"),
        yaxis  = list(title = "Probabilité (%)",
                      gridcolor = "#334",
                      range = c(0, 100)),
        legend = list(
          bgcolor = "rgba(0,0,0,0.3)",
          x = 0.01, y = 0.99
        ),
        hovermode = "x unified",
        margin = list(l=60, r=20, t=10, b=50)
      )

    fig
  })

  # ── Interprétation automatique ────────────────────────────

  output$interpretation_mc <- renderUI({

    r <- resultats()
    s4  <- r$stats %>% filter(semaine == 4)
    s12 <- r$stats %>% filter(semaine == min(12, r$horizon))

    # Biais directionnel
    biais <- if (s4$prob_hausse > 60) "haussier"
             else if (s4$prob_hausse < 40) "baissier"
             else "neutre"

    couleur_biais <- if (biais == "haussier") "#2ecc71"
                     else if (biais == "baissier") "#e74c3c"
                     else "#f39c12"

    # Niveau de volatilité
    niveau_vol <- if (r$volatilite < 0.20) "faible"
                  else if (r$volatilite < 0.35) "modérée"
                  else "élevée"

    tags$div(
      style = "padding: 15px;",

      fluidRow(
        column(4,
          tags$div(
            class = "info-card",
            tags$p("Signal directionnel", class = "metric-label"),
            tags$p(toupper(biais), class = "metric-value",
                   style = paste0("color:", couleur_biais, ";")),
            tags$p(paste0("Prob. hausse à 4 sem. : ",
                          round(s4$prob_hausse, 1), "%"),
                   class = "metric-sub")
          )
        ),
        column(4,
          tags$div(
            class = "info-card",
            tags$p("Volatilité du modèle", class = "metric-label"),
            tags$p(toupper(niveau_vol), class = "metric-value",
                   style = "color:#f39c12;"),
            tags$p(paste0(r$volatilite * 100, "%/an — ",
                          format(r$n_sim, big.mark=" "),
                          " simulations"),
                   class = "metric-sub")
          )
        ),
        column(4,
          tags$div(
            class = "info-card",
            tags$p("Incertitude à 12 semaines", class = "metric-label"),
            tags$p(paste0("±",
                          round((s12$p90 - s12$p10) / 2, 3),
                          " USD/kg"),
                   class = "metric-value",
                   style = "color:#3498db;"),
            tags$p(paste0("Fourchette 80% : [",
                          round(s12$p10, 3), " – ",
                          round(s12$p90, 3), "]"),
                   class = "metric-sub")
          )
        )
      ),

      tags$hr(),

      tags$p(
        paste0(
          "Le modèle Monte Carlo (", format(r$n_sim, big.mark=" "),
          " simulations, GBM Thompson 2000) indique un biais ",
          biais, " à 4 semaines avec ",
          round(s4$prob_hausse, 1),
          "% de probabilité de hausse. ",
          "Le scénario central à ", r$horizon, " semaines est ",
          round(r$stats$moyenne[r$horizon], 3), " USD/kg. ",
          "La fourchette 80% à 12 semaines s'étend de ",
          round(s12$p10, 3), " à ", round(s12$p90, 3),
          " USD/kg, reflétant une volatilité ", niveau_vol,
          " du marché du caoutchouc naturel."
        ),
        style = "color:#aaa; font-size:13px; line-height:1.6;"
      ),

      tags$p(
        "⚠ Ces projections sont issues d'une simulation stochastique.
         Elles ne constituent pas un conseil en investissement.
         Paramètres calibrés sur données ANRPC 2015-2025.",
        style = "color:#666; font-size:11px; margin-top:10px;"
      )
    )
  })
}


# ── 5. LANCER L'APPLICATION ───────────────────────────────────

shinyApp(ui = ui, server = server)
