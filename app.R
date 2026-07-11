# RUBBERSIGNAL TERMINAL — Application Shiny R
# Auteur  : Martial Sahiri
# Version : 5.0 — Sprints 1-6 (Plantations + Manufacturiers)
# Usage   : shiny::runApp("app.R")

library(shiny)
library(shinydashboard)
library(plotly)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(leaflet)
library(leaflet.extras)
library(xml2)

# DONNÉES STATIQUES

# ── Plantations (FAO/ANRPC 2024) ─────────────────────────────
donnees_plantations <- tibble(
  pays              = c("Thailande","Indonesie","Cote d'Ivoire",
                        "Malaisie","Vietnam","Inde",
                        "Chine","Philippines","Cameroun","Myanmar"),
  continent         = c("Asie","Asie","Afrique",
                        "Asie","Asie","Asie",
                        "Asie","Asie","Afrique","Asie"),
  lat               = c(13.75,-6.21,5.36,3.14,16.04,
                        20.59,35.86,12.88,3.85,21.92),
  lon               = c(100.52,106.85,-4.01,101.69,108.28,
                        78.96,104.19,121.77,11.52,95.96),
  superficie_kha    = c(3200,3600,680,1050,970,
                        800,1150,380,420,680),
  production_kt     = c(4700,3200,1200,780,1260,
                        850,950,410,380,290),
  rendement_kg_ha   = c(1469,889,1765,743,1299,
                        1063,826,1079,905,426),
  age_moyen_ans     = c(12,15,10,18,11,14,9,13,8,16),
  part_mondiale_pct = c(34.2,23.3,8.7,5.7,9.2,
                        6.2,6.9,3.0,2.8,2.1),
  qualite_grade     = c("TSR20","TSR20/RSS","TSR20","SMR20",
                        "SVR10","ISNR20","SCR WF","TSR20",
                        "RSS","TSR20")
)

# ── Manufacturiers — Marché (Rapports annuels + IRSG 2024) ───
donnees_manuf <- tibble(
  groupe          = c("Bridgestone","Michelin","Goodyear",
                      "Continental","Sumitomo","Pirelli",
                      "Hankook","Yokohama","Maxxis","Apollo"),
  pays_origine    = c("Japon","France","USA",
                      "Allemagne","Japon","Italie",
                      "Coree du Sud","Japon","Taiwan","Inde"),
  continent       = c("Asie","Europe","Amerique",
                      "Europe","Asie","Europe",
                      "Asie","Asie","Asie","Asie"),
  ca_mrd_usd      = c(32.1,28.6,17.5,
                      15.8,9.2,6.7,
                      8.1,6.4,4.8,3.2),
  conso_nr_kt     = c(1450,1380,920,
                      780,520,480,
                      420,380,290,210),
  part_marche_pct = c(19.5,17.3,10.6,
                      9.6,5.6,4.1,
                      4.9,3.9,2.9,1.9),
  nb_usines       = c(43,68,57,
                      35,28,19,
                      14,22,31,12),
  employes_k      = c(131,132,71,
                      100,28,32,
                      35,27,60,16),
  note_durab      = c(4,5,3,4,3,4,3,3,2,3)
)

# ── Usines principales (données publiques 2024) ───────────────
donnees_usines <- tibble(
  groupe       = c("Bridgestone","Bridgestone","Michelin","Michelin",
                   "Goodyear","Goodyear","Continental","Continental",
                   "Sumitomo","Pirelli","Hankook","Yokohama",
                   "Michelin","Bridgestone","Goodyear"),
  site         = c("Tokyo HQ","Nashville USA","Clermont-Ferrand","Greenville USA",
                   "Akron USA","Luxembourg","Hanovre","Timisoara Roumanie",
                   "Shimonoseki Japon","Milan Italie","Geumsan Coree","Mishima Japon",
                   "Shenyang Chine","Nanjing Chine","Pulandian Chine"),
  pays         = c("Japon","USA","France","USA",
                   "USA","Luxembourg","Allemagne","Roumanie",
                   "Japon","Italie","Coree du Sud","Japon",
                   "Chine","Chine","Chine"),
  continent    = c("Asie","Amerique","Europe","Amerique",
                   "Amerique","Europe","Europe","Europe",
                   "Asie","Europe","Asie","Asie",
                   "Asie","Asie","Asie"),
  lat          = c(35.68,36.17,45.78,34.85,
                   41.08,49.61,52.37,45.75,
                   33.95,45.46,36.18,35.12,
                   41.80,32.06,39.42),
  lon          = c(139.69,-86.78,3.08,-82.40,
                   -81.52,6.13,9.73,21.23,
                   130.93,9.19,127.12,138.91,
                   123.43,118.78,121.98),
  capacite_kt  = c(180,320,210,280,
                   195,160,175,220,
                   145,130,185,120,
                   165,195,140),
  type_prod    = c("Siege","Tourisme/4x4","Siege","Tourisme",
                   "Tourisme/Camion","Camion","Tourisme","Tourisme",
                   "Tourisme","Tourisme/Moto","Tourisme","Tourisme",
                   "Tourisme","Camion","Tourisme")
)

# ── Données corrélation manufacturiers (simulées 2020-2026) ──
annees_corr <- 2020:2026
donnees_corr_manuf <- tibble(
  annee              = annees_corr,
  prod_pneus_mt      = c(1820,1650,1890,1950,2010,2080,2120),
  conso_nr_mondial_kt= c(13200,12100,13800,14200,14600,15100,15400),
  prix_tsr20_moy     = c(1.65,1.42,1.89,2.05,2.18,2.24,2.27),
  croissance_auto_pct= c(3.2,-12.5,8.4,5.2,4.1,3.8,2.9),
  ventes_ev_mpcs     = c(3.1,6.5,10.5,14.2,18.7,22.4,26.1)
)

# FONCTIONS UTILITAIRES

charger_dernier_json <- function(dossier = "data/processed") {
  fichiers <- list.files(dossier,
    pattern = "rubbersignal_S\\d+_\\d+\\.json", full.names = TRUE)
  if (length(fichiers) == 0) return(NULL)
  read_json(fichiers[which.max(file.mtime(fichiers))])
}

charger_historique <- function(dossier = "data/processed") {
  fichiers <- list.files(dossier,
    pattern = "rubbersignal_S\\d+_\\d+\\.json", full.names = TRUE)
  if (length(fichiers) == 0) return(NULL)
  historique <- map_df(fichiers, function(f) {
    d <- tryCatch(read_json(f), error = function(e) NULL)
    if (is.null(d)) return(NULL)
    tibble(
      semaine       = as.integer(d$meta$semaine %||% NA),
      annee         = as.integer(d$meta$annee   %||% NA),
      date_label    = paste0("S", d$meta$semaine, "/", d$meta$annee),
      prix_tsr20    = as.numeric(d$prix$synthese$prix_actuel %||% NA),
      tendance      = as.character(d$prix$synthese$tendance %||% NA),
      variation_pct = as.numeric(d$prix$synthese$variation_pct %||% NA),
      source_prix   = as.character(d$prix$synthese$source %||% NA),
      pre_rsi       = as.numeric(d$signaux_faibles$pre_rsi$score %||% NA),
      signal_rsi    = as.character(d$signaux_faibles$pre_rsi$signal %||% NA),
      score_meteo   = as.numeric(d$signaux_faibles$module1_meteo$score_offre_mondiale %||% NA),
      score_demande = as.numeric(d$signaux_faibles$module3_demande_aval$score_demande %||% NA),
      usd_cny       = as.numeric(d$signaux_faibles$module2_devises$USD_CNY %||% NA),
      usd_myr       = as.numeric(d$signaux_faibles$module2_devises$USD_MYR %||% NA),
      usd_chf       = as.numeric(d$signaux_faibles$module2_devises$USD_CHF %||% NA),
      signal_geo    = as.character(d$signaux_faibles$module7_geopolitique$signal %||% NA),
      mc_base_4sem  = as.numeric(d$monte_carlo$resultats$S4$scenario_base %||% NA),
      mc_bear_4sem  = as.numeric(d$monte_carlo$resultats$S4$scenario_bear %||% NA),
      mc_bull_4sem  = as.numeric(d$monte_carlo$resultats$S4$scenario_bull %||% NA),
      wti           = as.numeric(d$signaux_faibles$module5_shipping$wti_valeur %||% NA),
      rsci_pct         = as.numeric(d$prix$rsci$rsci_pct                          %||% NA),
      rsci_ecart_pts   = as.numeric(d$prix$rsci$ecart_pts                         %||% NA),
      sicom_usd        = as.numeric(d$prix$marches_mondiaux$sicom$prix_usd        %||% NA),
      tocom_usd        = as.numeric(d$prix$marches_mondiaux$tocom$prix_usd        %||% NA),
      shfe_usd         = as.numeric(d$prix$marches_mondiaux$shfe$prix_usd         %||% NA),
      spread_sicom_lgm = as.numeric(d$prix$marches_mondiaux$spread_sicom_lgm      %||% NA)
    )
  })
  historique %>% arrange(annee, semaine) %>% filter(!is.na(prix_tsr20))
}

simuler_mc <- function(prix, drift, vol, n, horizon, seed = 42) {
  set.seed(seed); dt <- 1/52; vol_h <- vol * sqrt(dt)
  traj <- matrix(NA_real_, nrow=n, ncol=horizon+1)
  traj[,1] <- prix
  for (t in 2:(horizon+1)) {
    eps <- rnorm(n)
    traj[,t] <- traj[,t-1] * exp((drift-0.5*vol^2)*dt + vol_h*eps)
  }
  traj
}

calc_stats <- function(traj, prix_ref) {
  n_sem <- ncol(traj) - 1
  map_df(1:n_sem, function(h) {
    px <- traj[,h+1]
    tibble(semaine=h, p05=quantile(px,0.05), p10=quantile(px,0.10),
           p25=quantile(px,0.25), mediane=median(px), moyenne=mean(px),
           p75=quantile(px,0.75), p90=quantile(px,0.90), p95=quantile(px,0.95),
           prob_hausse=mean(px>prix_ref)*100,
           prob_plus5=mean(px>prix_ref*1.05)*100,
           prob_moins5=mean(px<prix_ref*0.95)*100)
  })
}

normaliser <- function(x) {
  mn <- min(x,na.rm=TRUE); mx <- max(x,na.rm=TRUE)
  if (mx==mn) return(rep(0,length(x)))
  (x-mn)/(mx-mn)*100
}

calc_corr <- function(x, y) {
  idx <- !is.na(x) & !is.na(y)
  if (sum(idx)<3) return(list(r=NA, p=NA, n=0))
  ct <- cor.test(x[idx], y[idx], method="pearson")
  list(r=round(ct$estimate,3), p=round(ct$p.value,4), n=sum(idx))
}

# ── RUBBER NEWS — Agrégateur RSS ─────────────────────────────
charger_news <- function() {
  feeds <- list(
    list(
      url       = "https://news.google.com/rss/search?q=%22natural+rubber%22+price+TSR20+futures&hl=en-US&gl=US&ceid=US:en",
      categorie = "Prix & Marches",
      couleur   = "#f39c12"
    ),
    list(
      url       = "https://news.google.com/rss/search?q=%22natural+rubber%22+supply+plantation+Thailand+Indonesia&hl=en-US&gl=US&ceid=US:en",
      categorie = "Geopolitique & Supply",
      couleur   = "#e74c3c"
    ),
    list(
      url       = "https://news.google.com/rss/search?q=%22natural+rubber%22+tire+tyre+Bridgestone+Michelin+manufacturer&hl=en-US&gl=US&ceid=US:en",
      categorie = "Industrie & Manuf.",
      couleur   = "#3498db"
    ),
    list(
      url       = "https://news.google.com/rss/search?q=RubberTech+OR+%22Global+Polymer+Summit%22+OR+%22Tire+Technology+Expo%22+OR+%22DKT+2027%22+OR+%22rubber+conference%22+OR+%22PlastPrintPack%22+OR+%22IRC+2027%22+OR+%22rubber+expo%22&hl=en-US&gl=US&ceid=US:en",
      categorie = "Salons & Evenements",
      couleur   = "#2ecc71"
    ),
    list(
      url       = "https://news.google.com/rss/search?q=PlastPrintPack+Abidjan+OR+%22West+Africa+PlastPrintPack%22+OR+caoutchouc+salon+Afrique+OR+%22AfroPlast%22+OR+plastique+exposition+Afrique&hl=fr&gl=FR&ceid=FR:fr",
      categorie = "Salons & Evenements",
      couleur   = "#2ecc71"
    ),
    # ── Presse africaine (français) ──────────────────────────────
    list(
      url       = "https://www.agenceecofin.com/hevea?format=feed&type=rss",
      categorie = "Afrique (FR)",
      couleur   = "#9b59b6"
    ),
    list(
      url       = "https://news.google.com/rss/search?q=caoutchouc+hevea+Afrique+Ivoire&hl=fr&gl=FR&ceid=FR:fr",
      categorie = "Afrique (FR)",
      couleur   = "#9b59b6"
    ),
    # ── Presse brésilienne (português) ───────────────────────────
    list(
      url       = "https://apabor.org.br/feed",
      categorie = "Bresil (PT)",
      couleur   = "#1abc9c"
    ),
    list(
      url       = "https://news.google.com/rss/search?q=borracha+natural+Brasil+hevea&hl=pt-BR&gl=BR&ceid=BR:pt-419",
      categorie = "Bresil (PT)",
      couleur   = "#1abc9c"
    ),
    # ── Presse spécialisée européenne (english) ──────────────────
    list(
      url       = "https://www.european-rubber-journal.com/section/1/news.rss",
      categorie = "Europe — ERJ",
      couleur   = "#e67e22"
    ),
    list(
      url       = "https://whatnextrubber.com/feed",
      categorie = "Europe — ERJ",
      couleur   = "#e67e22"
    )
  )

  safe_txt <- function(node) {
    tryCatch(xml_text(node), error = function(e) NA_character_)
  }

  articles <- map_df(feeds, function(f) {
    tryCatch({
      doc   <- read_xml(f$url)
      items <- xml_find_all(doc, "//item")
      if (length(items) == 0) return(NULL)
      n <- min(10, length(items))
      map_df(items[seq_len(n)], function(item) {
        desc_raw <- safe_txt(xml_find_first(item, "description"))
        desc_clean <- if (!is.na(desc_raw) && nchar(desc_raw) > 0) {
          d <- gsub("<[^>]+>", "", desc_raw)
          d <- gsub("&amp;",  "&",  d)
          d <- gsub("&lt;",   "<",  d)
          d <- gsub("&gt;",   ">",  d)
          d <- gsub("&quot;", "\"", d)
          d <- gsub("&#39;",  "'",  d)
          substr(trimws(d), 1, 320)
        } else ""

        tibble(
          titre      = trimws(safe_txt(xml_find_first(item, "title"))   %||% ""),
          lien       = trimws(safe_txt(xml_find_first(item, "link"))    %||% ""),
          source_nom = safe_txt(xml_find_first(item, "source"))         %||% "Google News",
          date_str   = safe_txt(xml_find_first(item, "pubDate"))        %||% "",
          description= desc_clean,
          categorie  = f$categorie,
          couleur    = f$couleur
        )
      })
    }, error = function(e) NULL)
  })

  if (is.null(articles) || nrow(articles) == 0) return(NULL)

  articles %>%
    mutate(
      source_nom = if_else(is.na(source_nom) | source_nom == "", "Google News", source_nom),
      date_pub   = tryCatch(
        as.POSIXct(date_str, format = "%a, %d %b %Y %H:%M:%S %z", tz = "UTC"),
        error = function(e) NA_POSIXct_
      ),
      date_aff   = if_else(
        !is.na(date_pub),
        format(date_pub, "%d %b %Y — %H:%M UTC"),
        date_str
      )
    ) %>%
    arrange(desc(date_pub)) %>%
    distinct(titre, .keep_all = TRUE)
}

# ── Helper UI : barre de score colorée ───────────────────────
barre_score <- function(score, label = NULL) {
  s   <- if (is.null(score) || is.na(suppressWarnings(as.numeric(score)))) 0
         else min(100, max(0, as.numeric(score)))
  col <- if (s >= 65) "#2ecc71" else if (s >= 50) "#f39c12" else "#e74c3c"
  dir <- if (s >= 65) "Favorable" else if (s >= 50) "Neutre" else "Defavorable"
  tags$div(
    style = "margin:4px 0 8px 0;",
    if (!is.null(label))
      tags$div(style = "color:#888;font-size:11px;margin-bottom:2px;", label),
    tags$div(
      style = "display:flex;justify-content:space-between;margin-bottom:3px;",
      tags$span(style = "color:#eee;font-size:12px;font-weight:bold;",
                paste0(round(s), " / 100")),
      tags$span(style = paste0("color:", col, ";font-size:11px;"), dir)
    ),
    tags$div(
      style = "height:5px;background:#2a2a3e;border-radius:3px;",
      tags$div(style = paste0(
        "width:", s, "%;height:100%;background:", col,
        ";border-radius:3px;"
      ))
    )
  )
}

theme_rs <- function(fig) {
  fig %>% layout(
    paper_bgcolor="#16213e", plot_bgcolor="#16213e",
    font=list(color="#eee"),
    legend=list(bgcolor="rgba(0,0,0,0.3)", x=0.01, y=0.99),
    hovermode="x unified")
}

# UI

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title="RubberSignal Terminal", titleWidth=280),

  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem("Dashboard",          tabName="dashboard",    icon=icon("tachometer-alt")),
      menuItem("Marches Mondiaux",   tabName="marches",      icon=icon("globe")),
      menuItem("Signaux Faibles",    tabName="signaux",      icon=icon("exclamation-triangle")),
      menuItem("Correlations",       tabName="correlations", icon=icon("project-diagram")),
      menuItem("Simulateur",         tabName="simulateur",   icon=icon("chart-line")),
      menuItem("Scenarios",          tabName="scenarios",    icon=icon("cloud-rain")),
      menuItem("Plantations",        tabName="plantations",  icon=icon("tree")),
      menuItem("Manuf. — Marche",    tabName="manuf_marche", icon=icon("industry")),
      menuItem("Manuf. — Usines",    tabName="manuf_usines", icon=icon("map-marker-alt")),
      menuItem("Manuf. — Correlations", tabName="manuf_corr", icon=icon("chart-bar")),
      tags$hr(style="border-color:#444;margin:8px 15px;"),
      menuItem("Saisie Prix",            tabName="saisie",      icon=icon("edit")),
      menuItem("Rubber News",            tabName="news",        icon=icon("newspaper"))
    ),
    tags$hr(style="border-color:#444;"),
    tags$div(
      style="padding:0 15px;",
      tags$p("Correlations", style="color:#aaa;font-size:11px;margin:10px 0 5px;text-transform:uppercase;"),
      tags$label("Signal vs TSR20", style="color:#ddd;font-size:13px;"),
      selectInput("signal_x", NULL,
        choices=c("USD/CNY (Yuan)"="usd_cny","USD/MYR (Ringgit)"="usd_myr",
                  "USD/CHF (Franc)"="usd_chf","Pre-RSI"="pre_rsi",
                  "Score meteo"="score_meteo","Score demande"="score_demande",
                  "WTI (Petrole)"="wti"),
        selected="usd_cny", width="100%"),
      sliderInput("decalage","Decalage (semaines)",min=0,max=8,value=0,step=1,post=" sem.",width="100%"),
      tags$hr(style="border-color:#444;"),
      tags$p("Simulateur Monte Carlo", style="color:#aaa;font-size:11px;margin:10px 0 5px;text-transform:uppercase;"),
      numericInput("prix_depart","Prix depart (USD/kg)",value=2.29,min=0.5,max=10,step=0.01,width="100%"),
      sliderInput("drift","Tendance (%/an)",min=-10,max=20,value=3,step=0.5,post="%",width="100%"),
      sliderInput("volatilite","Volatilite (%/an)",min=5,max=60,value=27,step=1,post="%",width="100%"),
      sliderInput("horizon","Horizon (semaines)",min=4,max=52,value=26,step=4,post=" sem.",width="100%"),
      selectInput("n_sim","Simulations",choices=c("1 000"=1000,"5 000"=5000,"10 000"=10000),selected=10000,width="100%"),
      tags$br(),
      actionButton("lancer","Lancer simulation",
        style="width:100%;background:#e67e22;color:white;border:none;padding:10px;font-weight:bold;border-radius:4px;"),
      tags$br(),tags$br(),
      actionButton("reset_params","Parametres ANRPC",
        style="width:100%;background:#333;color:#aaa;border:1px solid #555;padding:8px;border-radius:4px;"),
      tags$br(),tags$br()
    )
  ),

  dashboardBody(
    tags$head(tags$style(HTML("
      body,.content-wrapper{background-color:#1a1a2e;}
      .box{background-color:#16213e;border:none;border-radius:8px;}
      .box-header{color:#eee;}
      .main-header .logo,.main-header .navbar{background:#0f3460;}
      .main-sidebar{background:#0f3460;}
      .sidebar-menu>li>a{color:#ccc;}
      .sidebar-menu>li.active>a{background:#e67e22;color:#fff;}
      .form-control{background:#0f3460;color:#eee;border-color:#334;}
      .irs-bar,.irs-bar-edge,.irs-slider,.irs-single{background:#e67e22;}
      .checkbox label,.radio label{color:#ddd !important;font-size:13px;}
      .control-label{color:#ddd !important;}
      .info-card{background:#16213e;border-radius:8px;padding:15px;margin-bottom:15px;}
      .metric-label{color:#888;font-size:12px;text-transform:uppercase;}
      .metric-value{color:#eee;font-size:22px;font-weight:bold;margin:5px 0;}
      .metric-sub{color:#aaa;font-size:13px;}
      hr{border-color:#334;}
    "))),

    tabItems(

      # DASHBOARD
      tabItem(tabName="dashboard",
        fluidRow(
          valueBoxOutput("dash_prix",width=3), valueBoxOutput("dash_rsi",width=3),
          valueBoxOutput("dash_var",width=3),  valueBoxOutput("dash_nb",width=3)
        ),
        fluidRow(
          valueBoxOutput("dash_rsci",width=6), valueBoxOutput("dash_rsci_ecart",width=6)
        ),
        fluidRow(box(title="Prix TSR20 — Historique",status="warning",width=12,height=420,
                     plotlyOutput("g_prix_hist",height="360px"))),
        fluidRow(
          box(title="RubberSignal Index — Evolution",status="primary",width=7,height=380,
              plotlyOutput("g_rsi_hist",height="320px")),
          box(title="Signaux — Derniere semaine",status="info",width=5,height=380,
              tableOutput("t_signaux"))
        ),
        fluidRow(
          box(title="Devises USD/CNY et USD/MYR",status="success",width=6,height=360,
              plotlyOutput("g_devises",height="300px")),
          box(title="Monte Carlo — Scenario central 4 sem.",status="warning",width=6,height=360,
              plotlyOutput("g_mc_hist",height="300px"))
        )
      ),

      # MARCHÉS MONDIAUX
      tabItem(tabName="marches",
        fluidRow(
          valueBoxOutput("mr_lgm",   width=3),
          valueBoxOutput("mr_sicom", width=3),
          valueBoxOutput("mr_tocom", width=3),
          valueBoxOutput("mr_shfe",  width=3)
        ),
        fluidRow(
          valueBoxOutput("mr_spread", width=6),
          valueBoxOutput("mr_date",   width=6)
        ),
        fluidRow(
          box(title="Evolution historique — 4 marches (USD/kg)",
              status="warning", width=12, height=420,
              plotlyOutput("mr_g_hist", height="360px"))
        ),
        fluidRow(
          box(title="Tableau comparatif — Semaine en cours",
              status="primary", width=7, height=340,
              tableOutput("mr_table")),
          box(title="Note methodologique",
              status="info", width=5, height=340,
              uiOutput("mr_info"))
        )
      ),

      # CORRELATIONS
      tabItem(tabName="correlations",
        fluidRow(
          valueBoxOutput("corr_r",width=3), valueBoxOutput("corr_p",width=3),
          valueBoxOutput("corr_sig",width=3), valueBoxOutput("corr_n",width=3)
        ),
        fluidRow(box(title=uiOutput("titre_temporel"),status="warning",width=12,height=450,
                     plotlyOutput("g_temporel",height="380px"))),
        fluidRow(
          box(title="Nuage de points — Regression lineaire",status="primary",width=6,height=420,
              plotlyOutput("g_scatter",height="360px")),
          box(title="Matrice de correlation — Heatmap",status="info",width=6,height=420,
              plotlyOutput("g_heatmap",height="360px"))
        ),
        fluidRow(box(title="Interpretation",status="success",width=12,uiOutput("interp_corr")))
      ),

      # SIMULATEUR
      tabItem(tabName="simulateur",
        fluidRow(
          valueBoxOutput("sim_px",width=3), valueBoxOutput("sim_base",width=3),
          valueBoxOutput("sim_prob",width=3), valueBoxOutput("sim_fourch",width=3)
        ),
        fluidRow(
          box(title="Distribution des prix — Monte Carlo GBM",status="warning",width=8,height=500,
              plotlyOutput("g_mc",height="420px")),
          box(title="Tableau des resultats",status="primary",width=4,height=500,
              tableOutput("t_mc"),tags$hr(),
              tags$p("Reference Thompson (2000)",style="color:#666;font-size:11px;text-align:center;"))
        ),
        fluidRow(
          box(title="Distribution a l'horizon final",status="info",width=6,height=380,
              plotlyOutput("g_hist_final",height="300px")),
          box(title="Probabilites directionnelles",status="success",width=6,height=380,
              plotlyOutput("g_prob",height="300px"))
        ),
        fluidRow(box(title="Interpretation — RubberSignal Analysis",status="warning",width=12,
                     uiOutput("interp_sim")))
      ),

      # SCENARIOS METEO
      tabItem(tabName="scenarios",
        fluidRow(
          valueBoxOutput("sc_base",width=3), valueBoxOutput("sc_choc",width=3),
          valueBoxOutput("sc_delta",width=3), valueBoxOutput("sc_prob",width=3)
        ),
        fluidRow(
          box(title="Configurateur de scenarios meteo",status="warning",width=5,height=540,
              tags$p("Zone geographique",style="color:#aaa;font-size:11px;text-transform:uppercase;margin-bottom:5px;"),
              checkboxGroupInput("sc_zones",NULL,
                choices=c("Cote d'Ivoire (15%)"="ci","Thailande (35%)"="th",
                          "Malaisie (25%)"="my","Indonesie (25%)"="id"),selected="ci"),
              tags$hr(style="border-color:#334;"),
              tags$p("Type de choc",style="color:#aaa;font-size:11px;text-transform:uppercase;margin-bottom:5px;"),
              radioButtons("sc_type",NULL,
                choices=c("Secheresse / Deficit hydrique"="secheresse",
                          "Exces de pluie / Inondations"="inondations",
                          "Conditions optimales"="optimal"),selected="secheresse"),
              tags$hr(style="border-color:#334;"),
              sliderInput("sc_intensite","Intensite du choc (%)",min=5,max=40,value=20,step=5,post="%",width="100%"),
              uiOutput("sc_desc"),tags$br(),
              actionButton("sc_lancer","Simuler ce scenario",
                style="width:100%;background:#e67e22;color:white;border:none;padding:10px;font-weight:bold;border-radius:4px;"),
              tags$br(),tags$br(),
              actionButton("sc_reset","Reinitialiser",
                style="width:100%;background:#333;color:#aaa;border:1px solid #555;padding:8px;border-radius:4px;")),
          box(title="Impact sur le prix TSR20 — Monte Carlo conditionnel",status="primary",width=7,height=540,
              plotlyOutput("sc_g_mc",height="460px"))
        ),
        fluidRow(
          box(title="Comparaison 4 scenarios",status="info",width=7,height=400,
              plotlyOutput("sc_g_comp",height="340px")),
          box(title="Interpretation du scenario",status="success",width=5,height=400,
              uiOutput("sc_interp"))
        )
      ),

      # PLANTATIONS
      tabItem(tabName="plantations",
        fluidRow(
          valueBoxOutput("pl_surface",width=3), valueBoxOutput("pl_prod",width=3),
          valueBoxOutput("pl_top",width=3),     valueBoxOutput("pl_ci",width=3)
        ),
        fluidRow(
          box(title="Carte mondiale des plantations d'hevea",status="success",width=8,height=520,
              leafletOutput("carte_pl",height="460px")),
          box(title="Filtres",status="warning",width=4,height=520,
              tags$p("Continent",style="color:#aaa;font-size:11px;text-transform:uppercase;margin-bottom:5px;"),
              checkboxGroupInput("pl_cont",NULL,choices=c("Asie"="Asie","Afrique"="Afrique"),
                                 selected=c("Asie","Afrique")),
              tags$hr(style="border-color:#334;"),
              tags$p("Taille des cercles",style="color:#aaa;font-size:11px;text-transform:uppercase;margin-bottom:5px;"),
              radioButtons("pl_metric",NULL,
                choices=c("Superficie (kha)"="superficie_kha","Production (kt)"="production_kt",
                          "Part mondiale (%)"="part_mondiale_pct","Rendement (kg/ha)"="rendement_kg_ha"),
                selected="superficie_kha"),
              tags$hr(style="border-color:#334;"),
              tags$p("Cliquez sur un pays pour ses details.",style="color:#888;font-size:12px;"),
              tags$hr(style="border-color:#334;"),
              tags$p("Sources : FAO FAOSTAT 2024, ANRPC 2024",style="color:#666;font-size:11px;"))
        ),
        fluidRow(
          box(title="Superficies par pays (kha)",status="primary",width=6,height=400,
              plotlyOutput("pl_g_surf",height="340px")),
          box(title="Production vs Rendement",status="info",width=6,height=400,
              plotlyOutput("pl_g_rend",height="340px"))
        ),
        fluidRow(box(title="Donnees completes — Plantations mondiales",status="warning",width=12,
                     tableOutput("pl_table")))
      ),

      # MANUFACTURIERS — MARCHÉ
      tabItem(tabName="manuf_marche",
        fluidRow(
          valueBoxOutput("mf_total_nr",  width=3),
          valueBoxOutput("mf_top_groupe",width=3),
          valueBoxOutput("mf_top_ca",    width=3),
          valueBoxOutput("mf_nb_groupes",width=3)
        ),
        fluidRow(
          box(title="Consommation caoutchouc naturel par groupe (kt/an)",
              status="warning",width=7,height=450,
              plotlyOutput("mf_g_conso",height="390px")),
          box(title="Parts de marche mondiales (%)",
              status="primary",width=5,height=450,
              plotlyOutput("mf_g_parts",height="390px"))
        ),
        fluidRow(
          box(title="Chiffre d'affaires par groupe (Mrd USD)",
              status="info",width=6,height=400,
              plotlyOutput("mf_g_ca",height="340px")),
          box(title="Consommation NR vs CA — Efficacite",
              status="success",width=6,height=400,
              plotlyOutput("mf_g_efficacite",height="340px"))
        ),
        fluidRow(
          box(title="Tableau complet — Top 10 manufacturiers",
              status="warning",width=12,
              tableOutput("mf_table"))
        )
      ),

      # MANUFACTURIERS — USINES
      tabItem(tabName="manuf_usines",
        fluidRow(
          valueBoxOutput("us_total_sites",  width=3),
          valueBoxOutput("us_total_cap",    width=3),
          valueBoxOutput("us_top_site",     width=3),
          valueBoxOutput("us_asie_pct",     width=3)
        ),
        fluidRow(
          box(title="Carte des principales usines de pneumatiques",
              status="success",width=8,height=540,
              leafletOutput("carte_usines",height="480px")),
          box(title="Filtres usines",status="warning",width=4,height=540,
              tags$p("Groupe",style="color:#aaa;font-size:11px;text-transform:uppercase;margin-bottom:5px;"),
              checkboxGroupInput("us_groupes",NULL,
                choices=c("Bridgestone","Michelin","Goodyear","Continental",
                          "Sumitomo","Pirelli","Hankook","Yokohama"),
                selected=c("Bridgestone","Michelin","Goodyear","Continental")),
              tags$hr(style="border-color:#334;"),
              tags$p("Continent",style="color:#aaa;font-size:11px;text-transform:uppercase;margin-bottom:5px;"),
              checkboxGroupInput("us_cont",NULL,
                choices=c("Asie"="Asie","Europe"="Europe","Amerique"="Amerique"),
                selected=c("Asie","Europe","Amerique")),
              tags$hr(style="border-color:#334;"),
              tags$p("Cliquez sur une usine pour ses details.",style="color:#888;font-size:12px;"),
              tags$hr(style="border-color:#334;"),
              tags$p("Sources : Rapports annuels 2024",style="color:#666;font-size:11px;"))
        ),
        fluidRow(
          box(title="Capacite de production par site (kt/an)",
              status="primary",width=7,height=400,
              plotlyOutput("us_g_cap",height="340px")),
          box(title="Repartition usines par continent",
              status="info",width=5,height=400,
              plotlyOutput("us_g_cont",height="340px"))
        )
      ),

      # MANUFACTURIERS — CORRÉLATIONS
      tabItem(tabName="manuf_corr",
        fluidRow(
          valueBoxOutput("mc_r_prod",   width=3),
          valueBoxOutput("mc_r_conso",  width=3),
          valueBoxOutput("mc_r_auto",   width=3),
          valueBoxOutput("mc_r_ev",     width=3)
        ),
        fluidRow(
          box(title="Production mondiale pneus vs Prix TSR20 (2020-2026)",
              status="warning",width=12,height=420,
              plotlyOutput("mc_g_prod_prix",height="360px"))
        ),
        fluidRow(
          box(title="Consommation NR mondiale vs Prix TSR20",
              status="primary",width=6,height=400,
              plotlyOutput("mc_g_conso_prix",height="340px")),
          box(title="Croissance automobile vs Prix TSR20",
              status="info",width=6,height=400,
              plotlyOutput("mc_g_auto_prix",height="340px"))
        ),
        fluidRow(
          box(title="Impact vehicules electriques sur demande NR",
              status="success",width=6,height=380,
              plotlyOutput("mc_g_ev",height="320px")),
          box(title="Interpretation — Signaux manufacturiers",
              status="warning",width=6,height=380,
              uiOutput("mc_interp"))
        )
      ),

      # ── SAISIE PRIX MARCHÉS MONDIAUX ─────────────────────────
      tabItem(tabName="saisie",
        fluidRow(
          box(title="Saisie hebdomadaire — Marches Mondiaux (J-1)",
              status="warning", solidHeader=TRUE, width=7,
              tags$p(style="color:#aaa;font-size:13px;",
                "Sources TradingView : ",
                tags$a("SGX-TF1!", href="https://www.tradingview.com/symbols/SGX-TF1!/",
                       target="_blank", style="color:#f39c12;"),
                " · ",
                tags$a("TOCOM-TRB1!", href="https://www.tradingview.com/symbols/TOCOM-TRB1!/",
                       target="_blank", style="color:#f39c12;"),
                " · ",
                tags$a("SHFE-RU1!", href="https://www.tradingview.com/symbols/SHFE-RU1!/",
                       target="_blank", style="color:#f39c12;"),
                " — Taux : ",
                tags$a("xe.com", href="https://www.xe.com", target="_blank",
                       style="color:#3498db;")
              ),
              dateInput("s_date", "Date des cours (J-1) :",
                        value=Sys.Date()-1, format="dd/mm/yyyy", language="fr"),
              hr(),
              tags$b("Cours des bourses", style="color:#eee;"),
              br(), br(),
              numericInput("s_sicom", "SICOM TSR20 — SGX  (USD cents/kg)",
                           value=NA, min=0, step=0.5),
              numericInput("s_tocom", "TOCOM RSS3 — JPX  (JPY/kg)",
                           value=NA, min=0, step=1),
              numericInput("s_shfe",  "SHFE RU — Shanghai  (CNY/tonne)",
                           value=NA, min=0, step=50),
              hr(),
              tags$b("Taux de change du jour", style="color:#eee;"),
              br(), br(),
              numericInput("s_jpy",
                           "1 JPY = ? USD   (ex : 0.00619 si USD/JPY = 161.6)",
                           value=NA, min=0, step=0.00001),
              numericInput("s_cny",
                           "1 CNY = ? USD   (ex : 0.1473 si USD/CNY = 6.79)",
                           value=NA, min=0, step=0.0001),
              br(),
              actionButton("s_save", "  Enregistrer dans le JSON",
                           icon=icon("save"),
                           class="btn btn-success btn-lg btn-block"),
              br(),
              uiOutput("s_status")
          ),
          box(title="Apercu des conversions",
              status="primary", solidHeader=TRUE, width=5,
              tags$p(style="color:#aaa;font-size:12px;margin-bottom:10px;",
                "Mise a jour en temps reel pendant la saisie."),
              tableOutput("s_preview"),
              hr(),
              uiOutput("s_last_values")
          )
        )
      ),

      # ── RUBBER NEWS ──────────────────────────────────────────────
      tabItem(tabName="news",
        fluidRow(
          box(width=12, background="black",
            style="padding:6px 15px 2px 15px;",
            fluidRow(
              column(4,
                selectInput("news_cat", NULL,
                  choices = list(
                    "Toutes"      = list("All categories" = "ALL"),
                    "Thematique"  = list(
                      "Prix & Marches"        = "Prix & Marches",
                      "Geopolitique & Supply" = "Geopolitique & Supply",
                      "Industrie & Manuf."    = "Industrie & Manuf.",
                      "Salons & Evenements"   = "Salons & Evenements"
                    ),
                    "Regional"    = list(
                      "Afrique (FR)"  = "Afrique (FR)",
                      "Bresil (PT)"   = "Bresil (PT)",
                      "Europe — ERJ"  = "Europe — ERJ"
                    )
                  ),
                  selected = "ALL", width = "100%"
                )
              ),
              column(3,
                actionButton("news_refresh", "Refresh",
                  icon  = icon("sync"),
                  class = "btn btn-warning btn-sm",
                  style = "margin-top:25px;"
                )
              ),
              column(5,
                tags$div(style="margin-top:30px;", uiOutput("news_last_update"))
              )
            )
          )
        ),
        fluidRow(
          column(12,
            uiOutput("news_cards")
          )
        )
      ),   # ← ferme tabItem("news")

      # ── SIGNAUX FAIBLES ──────────────────────────────────────────
      tabItem(tabName="signaux",

        # ── Ligne 1 : Pre-RSI + 4 indicateurs clés ───────────────
        fluidRow(
          valueBoxOutput("sf_pre_rsi",  width=4),
          valueBoxOutput("sf_offre",    width=2),
          valueBoxOutput("sf_demande",  width=2),
          valueBoxOutput("sf_terrain",  width=2),
          valueBoxOutput("sf_geo",      width=2)
        ),

        # ── Ligne 2 : Météo + Devises + Terrain CI ────────────────
        fluidRow(
          box(title="M1 — Météo zones productrices",
              status="info", solidHeader=TRUE, width=4,
              uiOutput("sf_meteo_detail")),
          box(title="M2 — Devises (USD/CNY · MYR · FCFA)",
              status="primary", solidHeader=TRUE, width=4,
              uiOutput("sf_devises_detail")),
          box(title="M6 — Terrain CI  ★ Exclusif RubberSignal",
              status="warning", solidHeader=TRUE, width=4,
              uiOutput("sf_terrain_detail"))
        ),

        # ── Ligne 3 : PMI + Shipping + Stocks ────────────────────
        fluidRow(
          box(title="M3 — PMI Manufacturing & Demande aval",
              status="success", solidHeader=TRUE, width=4,
              uiOutput("sf_pmi_detail")),
          box(title="M5 — Shipping / Fret maritime",
              status="danger", solidHeader=TRUE, width=4,
              uiOutput("sf_shipping_detail")),
          box(title="M4 — Stocks mondiaux",
              status="warning", solidHeader=TRUE, width=4,
              uiOutput("sf_stocks_detail"))
        )
      )
    )
  )
)

# SERVER

server <- function(input, output, session) {

  refresh_trigger <- reactiveVal(0)

  json_actuel <- reactive({
    refresh_trigger()
    charger_dernier_json()
  })
  historique  <- reactive({
    refresh_trigger()
    charger_historique()
  })

  observe({
    d <- json_actuel()
    if (!is.null(d)) {
      px <- d$prix$synthese$prix_actuel
      if (!is.null(px))
        updateNumericInput(session,"prix_depart",value=round(as.numeric(px),4))
    }
  })

  observeEvent(input$reset_params, {
    updateNumericInput(session,"prix_depart",value=2.29)
    updateSliderInput(session,"drift",value=3)
    updateSliderInput(session,"volatilite",value=27)
    updateSliderInput(session,"horizon",value=26)
    updateSelectInput(session,"n_sim",selected=10000)
  })

  # ── DASHBOARD ─────────────────────────────────────────────

  output$dash_prix <- renderValueBox({
    h <- historique()
    if (is.null(h)||nrow(h)==0) return(valueBox("N/A","Prix TSR20",icon("tag"),color="yellow"))
    d <- tail(h,1)
    valueBox(paste(round(d$prix_tsr20,4),"USD/kg"),paste0("S",d$semaine,"/",d$annee),icon("tag"),color="yellow")
  })
  output$dash_rsi <- renderValueBox({
    h <- historique()
    if (is.null(h)||nrow(h)==0) return(valueBox("N/A","Pre-RSI",icon("signal"),color="orange"))
    d <- tail(h,1); rsi <- d$pre_rsi
    valueBox(if(!is.na(rsi))paste(round(rsi),"/ 100") else "N/A",
             if(!is.na(d$signal_rsi))d$signal_rsi else "Pre-RSI",icon("signal"),
             color=if(!is.na(rsi)&&rsi>=55)"green" else if(!is.na(rsi)&&rsi<=45)"red" else "orange")
  })
  output$dash_var <- renderValueBox({
    h <- historique()
    if (is.null(h)||nrow(h)==0) return(valueBox("N/A","Variation",icon("arrows-alt-v"),color="blue"))
    d <- tail(h,1); v <- d$variation_pct
    valueBox(if(!is.na(v))paste0(if(v>0)"+" else "",round(v,2),"%") else "N/A",
             "Variation hebdomadaire",icon(if(!is.na(v)&&v>0)"arrow-up" else "arrow-down"),
             color=if(!is.na(v)&&v>0)"green" else if(!is.na(v)&&v<0)"red" else "yellow")
  })
  output$dash_nb <- renderValueBox({
    h <- historique()
    valueBox(paste(if(!is.null(h))nrow(h) else 0,"semaines"),
             "Historique disponible",icon("calendar"),color="purple")
  })
  output$dash_rsci <- renderValueBox({
    h <- historique()
    if (is.null(h)||nrow(h)==0) return(valueBox("N/A","RSCI",icon("balance-scale"),color="purple"))
    d <- tail(h,1); rsci <- d$rsci_pct; e <- d$rsci_ecart_pts
    valueBox(if(!is.na(rsci))paste0(rsci,"%") else "N/A",
             "RSCI \u2014 part planteur (DRC-corrigee)",icon("balance-scale"),
             color=if(is.na(e))"purple" else if(e>=0)"green" else if(e>=-5)"yellow" else "red")
  })
  output$dash_rsci_ecart <- renderValueBox({
    h <- historique()
    if (is.null(h)||nrow(h)==0) return(valueBox("N/A","Ecart RSCI",icon("balance-scale"),color="purple"))
    d <- tail(h,1); e <- d$rsci_ecart_pts
    valueBox(if(!is.na(e))paste0(if(e>0)"+" else "",e," pts") else "N/A",
             "vs mecanisme officiel CHPH (63%)",
             icon(if(!is.na(e)&&e>=0)"arrow-up" else "arrow-down"),
             color=if(is.na(e))"purple" else if(e>=0)"green" else if(e>=-5)"yellow" else "red")
  })
  output$g_prix_hist <- renderPlotly({
    h <- historique(); if(is.null(h)||nrow(h)==0) return(NULL)
    couleurs <- case_when(h$tendance=="hausse"~"#2ecc71",h$tendance=="baisse"~"#e74c3c",TRUE~"#f39c12")
    plot_ly() %>%
      add_lines(x=~h$date_label,y=~h$prix_tsr20,line=list(color="#e67e22",width=2.5),name="TSR20",
                hovertemplate="%{x}|%{y:.4f} USD/kg<extra></extra>") %>%
      add_markers(x=~h$date_label,y=~h$prix_tsr20,
                  marker=list(color=couleurs,size=9,line=list(color="#fff",width=1)),name="Prix hebdo",
                  hovertemplate="%{x}|%{y:.4f} USD/kg<extra></extra>") %>%
      theme_rs() %>%
      layout(xaxis=list(title="Semaine",gridcolor="#334",tickangle=-45),
             yaxis=list(title="Prix TSR20 (USD/kg)",gridcolor="#334",tickformat=".4f"),
             margin=list(l=60,r=20,t=10,b=80))
  })
  output$g_rsi_hist <- renderPlotly({
    h <- historique(); if(is.null(h)||nrow(h)==0) return(NULL)
    hr <- h %>% filter(!is.na(pre_rsi)); if(nrow(hr)==0) return(NULL)
    cr <- case_when(hr$pre_rsi>=55~"#2ecc71",hr$pre_rsi<=45~"#e74c3c",TRUE~"#f39c12")
    plot_ly() %>%
      add_lines(x=~hr$date_label,y=~hr$pre_rsi,line=list(color="#3498db",width=2.5),name="Pre-RSI",
                hovertemplate="%{x}|RSI:%{y:.0f}/100<extra></extra>") %>%
      add_markers(x=~hr$date_label,y=~hr$pre_rsi,
                  marker=list(color=cr,size=10,line=list(color="#fff",width=1.5)),name="RSI hebdo",
                  hovertemplate="%{x}|RSI:%{y:.0f}/100<extra></extra>") %>%
      add_lines(x=c(hr$date_label[1],tail(hr$date_label,1)),y=c(55,55),
                line=list(color="#2ecc71",dash="dot",width=1),name="Seuil haussier",hoverinfo="skip") %>%
      add_lines(x=c(hr$date_label[1],tail(hr$date_label,1)),y=c(45,45),
                line=list(color="#e74c3c",dash="dot",width=1),name="Seuil baissier",hoverinfo="skip") %>%
      theme_rs() %>%
      layout(xaxis=list(title="Semaine",gridcolor="#334",tickangle=-45),
             yaxis=list(title="Pre-RSI (0-100)",gridcolor="#334",range=c(0,100)),
             legend=list(bgcolor="rgba(0,0,0,0.3)",x=0.01,y=0.01),
             margin=list(l=60,r=20,t=10,b=80))
  })
  output$t_signaux <- renderTable({
    h <- historique(); if(is.null(h)||nrow(h)==0) return(NULL)
    d <- tail(h,1)
    tibble(Signal=c("Pre-RSI","Offre mondiale","Demande","USD/CNY","USD/MYR","Geopolitique"),
           Valeur=c(
             if(!is.na(d$pre_rsi))paste(round(d$pre_rsi),"/ 100") else "N/A",
             if(!is.na(d$score_meteo))paste(round(d$score_meteo),"/ 100") else "N/A",
             if(!is.na(d$score_demande))paste(round(d$score_demande),"/ 100") else "N/A",
             if(!is.na(d$usd_cny))as.character(round(d$usd_cny,4)) else "N/A",
             if(!is.na(d$usd_myr))as.character(round(d$usd_myr,4)) else "N/A",
             if(!is.na(d$signal_geo))d$signal_geo else "N/A"))
  },striped=TRUE,hover=TRUE,bordered=FALSE,style="color:#eee;background:#16213e;")
  output$g_devises <- renderPlotly({
    h <- historique(); if(is.null(h)||nrow(h)==0) return(NULL)
    hf <- h %>% filter(!is.na(usd_cny)|!is.na(usd_myr)); if(nrow(hf)==0) return(NULL)
    plot_ly() %>%
      add_lines(x=~hf$date_label,y=~hf$usd_cny,line=list(color="#e74c3c",width=2),name="USD/CNY",yaxis="y",
                hovertemplate="%{x}|CNY:%{y:.4f}<extra></extra>") %>%
      add_lines(x=~hf$date_label,y=~hf$usd_myr,line=list(color="#3498db",width=2),name="USD/MYR",yaxis="y2",
                hovertemplate="%{x}|MYR:%{y:.4f}<extra></extra>") %>%
      theme_rs() %>%
      layout(xaxis=list(title="Semaine",gridcolor="#334",tickangle=-45),
             yaxis=list(title="USD/CNY",gridcolor="#334",side="left"),
             yaxis2=list(title="USD/MYR",overlaying="y",side="right"),
             margin=list(l=60,r=60,t=10,b=80))
  })
  output$g_mc_hist <- renderPlotly({
    h <- historique(); if(is.null(h)||nrow(h)==0) return(NULL)
    hm <- h %>% filter(!is.na(mc_base_4sem)); if(nrow(hm)==0) return(NULL)
    plot_ly() %>%
      add_lines(x=~hm$date_label,y=~hm$prix_tsr20,line=list(color="#fff",dash="dot",width=1.5),name="Prix reel",
                hovertemplate="%{x}|Reel:%{y:.3f}<extra></extra>") %>%
      add_lines(x=~hm$date_label,y=~hm$mc_base_4sem,line=list(color="#e67e22",width=2),name="MC Base 4sem",
                hovertemplate="%{x}|MC:%{y:.3f}<extra></extra>") %>%
      theme_rs() %>%
      layout(xaxis=list(title="Semaine",gridcolor="#334",tickangle=-45),
             yaxis=list(title="Prix (USD/kg)",gridcolor="#334",tickformat=".3f"),
             margin=list(l=60,r=20,t=10,b=80))
  })

  # ── MARCHÉS MONDIAUX ──────────────────────────────────────

  output$mr_lgm <- renderValueBox({
    d <- json_actuel()
    if (is.null(d)) return(valueBox("N/A","LGM — TSR20 (Malaisie)",icon("tag"),color="yellow"))
    lgm <- as.numeric(d$prix$marches_mondiaux$lgm$prix_usd %||%
                      d$prix$synthese$prix_actuel            %||%
                      d$prix$prix_actuel_usd                 %||% NA)
    sem <- d$meta$semaine %||% "?"; an <- d$meta$annee %||% "?"
    valueBox(if(!is.na(lgm)) paste(round(lgm,4),"USD/kg") else "N/A",
             paste0("LGM — TSR20 | S",sem,"/",an),
             icon("tag"), color="yellow")
  })

  output$mr_sicom <- renderValueBox({
    d <- json_actuel()
    if (is.null(d)) return(valueBox("N/A","SGX/SICOM — TSR20 | Singapour",icon("chart-line"),color="blue"))
    val <- as.numeric(d$prix$marches_mondiaux$sicom$prix_usd %||% NA)
    valueBox(if(!is.na(val)) paste(val,"USD/kg") else "N/A — saisir",
             "SGX/SICOM — TSR20 | Singapour",
             icon("chart-line"), color="blue")
  })

  output$mr_tocom <- renderValueBox({
    d <- json_actuel()
    if (is.null(d)) return(valueBox("N/A","JPX/TOCOM — RSS3 | Tokyo",icon("chart-line"),color="red"))
    val <- as.numeric(d$prix$marches_mondiaux$tocom$prix_usd %||% NA)
    valueBox(if(!is.na(val)) paste(val,"USD/kg") else "N/A — saisir",
             "JPX/TOCOM — RSS3 | Tokyo",
             icon("chart-line"), color="red")
  })

  output$mr_shfe <- renderValueBox({
    d <- json_actuel()
    if (is.null(d)) return(valueBox("N/A","SHFE — SCR WF/RSS3 | Shanghai",icon("chart-line"),color="orange"))
    val <- as.numeric(d$prix$marches_mondiaux$shfe$prix_usd %||% NA)
    valueBox(if(!is.na(val)) paste(val,"USD/kg") else "N/A — saisir",
             "SHFE — SCR WF/RSS3 | Shanghai",
             icon("chart-line"), color="orange")
  })

  output$mr_spread <- renderValueBox({
    d <- json_actuel()
    if (is.null(d)) return(valueBox("N/A","Spread SICOM vs LGM",icon("arrows-alt-h"),color="purple"))
    sp <- as.numeric(d$prix$marches_mondiaux$spread_sicom_lgm %||% NA)
    valueBox(if(!is.na(sp)) paste0(if(sp>=0)"+" else "",sp," USD/kg") else "N/A",
             "Spread SICOM vs LGM (meme grade TSR20)",
             icon("arrows-alt-h"),
             color=if(is.na(sp))"purple" else if(abs(sp)<0.05)"green" else "orange")
  })

  output$mr_date <- renderValueBox({
    d <- json_actuel()
    if (is.null(d)) return(valueBox("N/A","Cours J-1",icon("calendar"),color="purple"))
    dt <- d$prix$marches_mondiaux$date_cours %||% "—"
    sem <- d$meta$semaine %||% "?"; an <- d$meta$annee %||% "?"
    valueBox(as.character(dt),
             paste0("Cours J-1 | S",sem,"/",an),
             icon("calendar"), color="purple")
  })

  output$mr_g_hist <- renderPlotly({
    h <- historique()
    if(is.null(h)||nrow(h)==0) return(NULL)
    fig <- plot_ly()
    # LGM TSR20 — toujours present
    fig <- fig %>% add_lines(x=~h$date_label, y=~h$prix_tsr20,
                              line=list(color="#f39c12",width=2.5), name="LGM TSR20 (USD/kg)",
                              hovertemplate="%{x}|LGM:%{y:.4f}<extra></extra>")
    # SICOM TSR20
    hs <- h %>% filter(!is.na(sicom_usd))
    if(nrow(hs)>0)
      fig <- fig %>% add_lines(x=~hs$date_label, y=~hs$sicom_usd,
                                line=list(color="#3498db",width=2), name="SICOM TSR20 (USD/kg)",
                                hovertemplate="%{x}|SICOM:%{y:.4f}<extra></extra>")
    # TOCOM RSS3
    ht <- h %>% filter(!is.na(tocom_usd))
    if(nrow(ht)>0)
      fig <- fig %>% add_lines(x=~ht$date_label, y=~ht$tocom_usd,
                                line=list(color="#e74c3c",width=2), name="TOCOM RSS3 (USD/kg)",
                                hovertemplate="%{x}|TOCOM:%{y:.4f}<extra></extra>")
    # SHFE RU
    hh <- h %>% filter(!is.na(shfe_usd))
    if(nrow(hh)>0)
      fig <- fig %>% add_lines(x=~hh$date_label, y=~hh$shfe_usd,
                                line=list(color="#e67e22",width=2,dash="dash"), name="SHFE RU (USD/kg)",
                                hovertemplate="%{x}|SHFE:%{y:.4f}<extra></extra>")
    fig %>% theme_rs() %>%
      layout(xaxis=list(title="Semaine",gridcolor="#334",tickangle=-45),
             yaxis=list(title="Prix (USD/kg)",gridcolor="#334",tickformat=".4f"),
             legend=list(bgcolor="rgba(0,0,0,0.3)",x=0.01,y=0.99),
             margin=list(l=60,r=20,t=10,b=80))
  })

  output$mr_table <- renderTable({
    h <- historique()
    if(is.null(h)||nrow(h)==0) return(NULL)
    d <- tail(h,1)
    tibble(
      Bourse  = c("LGM (Malaisie)","SGX/SICOM (Singapour)","JPX/TOCOM (Tokyo)","SHFE (Shanghai)"),
      Grade   = c("TSR20","TSR20","RSS3","SCR WF / RSS3"),
      Devise  = c("USD/kg","USD cts/kg → USD/kg","JPY/kg → USD/kg","CNY/t → USD/kg"),
      `Prix USD/kg` = c(
        if(!is.na(d$prix_tsr20)) round(d$prix_tsr20,4) else NA,
        if(!is.na(d$sicom_usd))  round(d$sicom_usd,4)  else NA,
        if(!is.na(d$tocom_usd))  round(d$tocom_usd,4)  else NA,
        if(!is.na(d$shfe_usd))   round(d$shfe_usd,4)   else NA
      ),
      Ticker  = c("—","TF","TRB","RU"),
      Source  = c("lgm.gov.my","SGX-TF1!","TOCOM-TRB1!","SHFE-RU1!")
    )
  }, striped=TRUE, hover=TRUE, bordered=FALSE,
     style="color:#eee;background:#16213e;", na="—")

  output$mr_info <- renderUI({
    tags$div(style="padding:15px;font-size:13px;line-height:1.7;color:#aaa;",
      tags$p(tags$b("Cours décalés J-1 — gratuits", style="color:#eee;")),
      tags$p("Les trois bourses affichent leurs cours avec un décalage de 24h à titre gratuit via TradingView."),
      tags$hr(style="border-color:#334;"),
      tags$p(tags$b("Grades et comparabilité", style="color:#eee;")),
      tags$p("Le SICOM et LGM cotent le même grade (TSR20) — le spread entre les deux reflète les frais de transport et l'arbitrage Singapour/Malaisie."),
      tags$p("Le TOCOM cote le RSS3 (feuille fumée, qualité supérieure) : naturellement plus cher que le TSR20."),
      tags$p("Le SHFE cote le SCR WF (caoutchouc chinois standard) ou RSS3 importé — libellé en CNY."),
      tags$hr(style="border-color:#334;"),
      tags$p(tags$b("Mise à jour", style="color:#eee;")),
      tags$p("Saisir les 3 cours dans script 01 chaque lundi avant de lancer le pipeline.",
             style="color:#888;font-size:12px;")
    )
  })

  # ── CORRELATIONS ─────────────────────────────────────────

  nom_signal <- reactive({
    switch(input$signal_x,
      "usd_cny"="USD/CNY","usd_myr"="USD/MYR","usd_chf"="USD/CHF",
      "pre_rsi"="Pre-RSI","score_meteo"="Score meteo",
      "score_demande"="Score demande","wti"="WTI",input$signal_x)
  })
  d_corr <- reactive({
    h <- historique(); if(is.null(h)||nrow(h)<3) return(NULL)
    sig <- input$signal_x; dec <- as.integer(input$decalage)
    y <- h$prix_tsr20; x <- h[[sig]]; if(is.null(x)) return(NULL)
    n <- nrow(h)
    x_d <- if(dec>0&&dec<n) c(rep(NA,dec),x[1:(n-dec)]) else x
    tibble(date_label=h$date_label,tsr20=y,signal=x_d,
           tsr20_norm=normaliser(y),signal_norm=normaliser(x_d)) %>% filter(!is.na(signal))
  })
  c_stats <- reactive({
    d <- d_corr(); if(is.null(d)||nrow(d)<3) return(NULL)
    calc_corr(d$tsr20,d$signal)
  })
  output$corr_r <- renderValueBox({
    cs <- c_stats()
    if(is.null(cs)||is.na(cs$r)) return(valueBox("N/A","Pearson r",icon("calculator"),color="yellow"))
    r <- cs$r
    valueBox(as.character(r),paste0("r | decalage ",input$decalage," sem."),icon("calculator"),
             color=if(abs(r)>0.7)"green" else if(abs(r)>0.4)"yellow" else "red")
  })
  output$corr_p <- renderValueBox({
    cs <- c_stats()
    if(is.null(cs)||is.na(cs$p)) return(valueBox("N/A","P-value",icon("check"),color="yellow"))
    p <- cs$p
    valueBox(as.character(p),if(p<0.01)"Tres significatif" else if(p<0.05)"Significatif" else "Non significatif",
             icon("check"),color=if(p<0.05)"green" else "red")
  })
  output$corr_sig <- renderValueBox({
    cs <- c_stats(); r <- if(!is.null(cs))cs$r else NA
    lbl <- if(is.na(r))"Insuffisant" else if(r>0.7)"Forte positive" else if(r>0.4)"Moderee positive"
           else if(r>0.1)"Faible positive" else if(r< -0.7)"Forte negative"
           else if(r< -0.4)"Moderee negative" else if(r< -0.1)"Faible negative" else "Pas de correlation"
    valueBox(if(!is.na(r))paste0(abs(round(r*100)),"%") else "N/A",lbl,icon("signal"),
             color=if(!is.na(r)&&abs(r)>0.5)"green" else if(!is.na(r)&&abs(r)>0.3)"yellow" else "red")
  })
  output$corr_n <- renderValueBox({
    d <- d_corr(); n <- if(!is.null(d))nrow(d) else 0
    valueBox(paste(n,"semaines"),"Points de donnees",icon("database"),
             color=if(n>=10)"green" else if(n>=5)"yellow" else "red")
  })
  output$titre_temporel <- renderUI({
    dec <- input$decalage
    tags$span(nom_signal()," vs TSR20",
              if(dec>0)tags$small(paste0(" — decale ",dec," sem."),style="color:#aaa;font-size:12px;"))
  })
  output$g_temporel <- renderPlotly({
    d <- d_corr()
    if(is.null(d)||nrow(d)<2) return(plot_ly() %>% theme_rs() %>% layout(
      annotations=list(list(text="Donnees insuffisantes",x=0.5,y=0.5,xref="paper",yref="paper",
                            showarrow=FALSE,font=list(color="#aaa",size=14)))))
    cs <- c_stats(); r <- if(!is.null(cs))cs$r else NA
    cc <- if(is.na(r))"#aaa" else if(r>0.5)"#2ecc71" else if(r< -0.5)"#e74c3c" else "#f39c12"
    plot_ly() %>%
      add_lines(x=~d$date_label,y=~d$tsr20_norm,line=list(color="#e67e22",width=2.5),
                name="TSR20 (norm.)",yaxis="y",hovertemplate="%{x}|TSR20:%{y:.1f}<extra></extra>") %>%
      add_lines(x=~d$date_label,y=~d$signal_norm,line=list(color=cc,width=2,dash="dash"),
                name=nom_signal(),yaxis="y2",hovertemplate="%{x}|Signal:%{y:.1f}<extra></extra>") %>%
      theme_rs() %>%
      layout(xaxis=list(title="Semaine",gridcolor="#334",tickangle=-45),
             yaxis=list(title="TSR20 norm.",gridcolor="#334",side="left",range=c(-5,110)),
             yaxis2=list(title=paste0(nom_signal()," norm."),overlaying="y",side="right",range=c(-5,110)),
             annotations=list(list(text=if(!is.na(r))paste0("r = ",r) else "r = N/A",
                                   x=0.98,y=0.05,xref="paper",yref="paper",
                                   showarrow=FALSE,font=list(color=cc,size=16))),
             margin=list(l=60,r=80,t=20,b=80))
  })
  output$g_scatter <- renderPlotly({
    d <- d_corr()
    if(is.null(d)||nrow(d)<3) return(plot_ly() %>% theme_rs() %>% layout(
      annotations=list(list(text="Donnees insuffisantes",x=0.5,y=0.5,xref="paper",yref="paper",
                            showarrow=FALSE,font=list(color="#aaa",size=14)))))
    cs <- c_stats(); r <- if(!is.null(cs))cs$r else NA
    dc <- d %>% filter(!is.na(tsr20)&!is.na(signal))
    lm_fit <- if(nrow(dc)>=2) lm(tsr20~signal,data=dc) else NULL
    xr <- if(!is.null(lm_fit)) seq(min(dc$signal),max(dc$signal),length.out=50) else NULL
    yp <- if(!is.null(lm_fit)) predict(lm_fit,newdata=data.frame(signal=xr)) else NULL
    fig <- plot_ly() %>%
      add_markers(x=~d$signal,y=~d$tsr20,text=~d$date_label,
                  marker=list(color="#e67e22",size=10,opacity=0.8,line=list(color="#fff",width=1)),
                  name="Observation",
                  hovertemplate=paste0("%{text}<br>",nom_signal(),": %{x:.4f}<br>TSR20: %{y:.4f}<extra></extra>"))
    if(!is.null(lm_fit))
      fig <- fig %>% add_lines(x=xr,y=yp,line=list(color="#3498db",width=2,dash="dash"),
                                name="Regression",hoverinfo="skip")
    fig %>% theme_rs() %>%
      layout(xaxis=list(title=nom_signal(),gridcolor="#334"),
             yaxis=list(title="TSR20 (USD/kg)",gridcolor="#334",tickformat=".4f"),
             annotations=list(list(text=if(!is.na(r))paste0("r = ",r) else "",
                                   x=0.98,y=0.05,xref="paper",yref="paper",
                                   showarrow=FALSE,font=list(color="#3498db",size=14))),
             margin=list(l=60,r=20,t=10,b=60))
  })
  output$g_heatmap <- renderPlotly({
    h <- historique()
    if(is.null(h)||nrow(h)<3) return(plot_ly() %>% theme_rs() %>% layout(
      annotations=list(list(text="Donnees insuffisantes",x=0.5,y=0.5,xref="paper",yref="paper",
                            showarrow=FALSE,font=list(color="#aaa",size=14)))))
    vars <- c("prix_tsr20","usd_cny","usd_myr","usd_chf","pre_rsi","score_meteo","score_demande","wti")
    labels <- c("TSR20","USD/CNY","USD/MYR","USD/CHF","Pre-RSI","Meteo","Demande","WTI")
    dm <- h %>% select(all_of(vars)); dm <- dm[complete.cases(dm),]
    if(nrow(dm)<3) return(plot_ly() %>% theme_rs() %>% layout(
      annotations=list(list(text="Pas assez de donnees",x=0.5,y=0.5,xref="paper",yref="paper",
                            showarrow=FALSE,font=list(color="#aaa",size=14)))))
    cm <- round(cor(dm,use="pairwise.complete.obs"),2)
    plot_ly(x=labels,y=labels,z=cm,type="heatmap",
            colorscale=list(c(0,"#e74c3c"),c(0.5,"#16213e"),c(1,"#2ecc71")),
            zmin=-1,zmax=1,text=cm,texttemplate="%{text}",
            hovertemplate="%{x} vs %{y}<br>r = %{z}<extra></extra>") %>%
      theme_rs() %>%
      layout(xaxis=list(tickangle=-45),yaxis=list(autorange="reversed"),
             margin=list(l=80,r=20,t=10,b=80))
  })
  output$interp_corr <- renderUI({
    d <- d_corr(); cs <- c_stats()
    if(is.null(d)||is.null(cs)||is.na(cs$r))
      return(tags$p("Accumulez plus de semaines (minimum 10).",style="color:#aaa;padding:15px;font-size:13px;"))
    r <- cs$r; p <- cs$p; n <- cs$n; dec <- input$decalage
    force <- if(abs(r)>0.7)"forte" else if(abs(r)>0.4)"moderee" else if(abs(r)>0.2)"faible" else "negligeable"
    direction <- if(r>0)"positive" else "negative"; sig_ok <- p<0.05
    tags$div(style="padding:15px;",
      fluidRow(
        column(4,tags$div(class="info-card",
          tags$p("Force",class="metric-label"),
          tags$p(toupper(force),class="metric-value",
                 style=paste0("color:",if(abs(r)>0.5)"#2ecc71" else if(abs(r)>0.3)"#f39c12" else "#e74c3c",";")),
          tags$p(paste0("r = ",r," (",direction,")"),class="metric-sub"))),
        column(4,tags$div(class="info-card",
          tags$p("Significativite",class="metric-label"),
          tags$p(if(sig_ok)"SIGNIFICATIF" else "NON SIGNIFICATIF",class="metric-value",
                 style=paste0("color:",if(sig_ok)"#2ecc71" else "#e74c3c",";")),
          tags$p(paste0("p = ",p," (n=",n,")"),class="metric-sub"))),
        column(4,tags$div(class="info-card",
          tags$p("Decalage",class="metric-label"),
          tags$p(paste0(dec," SEM."),class="metric-value",style="color:#3498db;"),
          tags$p(if(dec>0)paste0(nom_signal()," precede TSR20 de ",dec," sem.") else "Temps reel",
                 class="metric-sub")))
      )
    )
  })

  # ── SIMULATEUR ─────────────────────────────────────────────

  sim_res <- eventReactive(
    list(input$lancer,input$prix_depart,input$drift,input$volatilite,input$horizon), {
    req(input$prix_depart,input$drift,input$volatilite,input$horizon)
    px <- as.numeric(input$prix_depart); dr <- as.numeric(input$drift)/100
    vl <- as.numeric(input$volatilite)/100; hr <- as.numeric(input$horizon)
    ns <- as.numeric(input$n_sim)
    withProgress(message="Simulation...",value=0,{
      incProgress(0.3,detail=paste(format(ns,big.mark=" "),"trajectoires"))
      traj <- simuler_mc(px,dr,vl,ns,hr)
      incProgress(0.5,detail="Statistiques")
      stats <- calc_stats(traj,px); incProgress(0.2)
    })
    list(traj=traj,stats=stats,px=px,dr=dr,vl=vl,hr=hr,ns=ns)
  },ignoreNULL=FALSE)

  output$sim_px <- renderValueBox({
    r <- sim_res()
    valueBox(paste(round(r$px,4),"USD/kg"),"Prix de depart",icon("tag"),color="yellow")
  })
  output$sim_base <- renderValueBox({
    r <- sim_res(); s <- r$stats %>% filter(semaine==r$hr)
    valueBox(paste(round(s$moyenne,3),"USD/kg"),paste("Central a",r$hr,"sem."),icon("chart-line"),color="orange")
  })
  output$sim_prob <- renderValueBox({
    r <- sim_res(); s <- r$stats %>% filter(semaine==4); p <- round(s$prob_hausse,1)
    valueBox(paste0(p,"%"),"P(hausse) 4 semaines",icon("arrow-up"),
             color=if(p>55)"green" else if(p<45)"red" else "yellow")
  })
  output$sim_fourch <- renderValueBox({
    r <- sim_res(); s <- r$stats %>% filter(semaine==r$hr)
    valueBox(paste0("[",round(s$p10,3)," - ",round(s$p90,3),"]"),
             paste("IC80 a",r$hr,"sem."),icon("arrows-alt-h"),color="blue")
  })
  output$g_mc <- renderPlotly({
    r <- sim_res(); s <- r$stats
    plot_ly() %>%
      add_lines(x=~s$semaine,y=~s$p10,line=list(color="#e74c3c",dash="dash",width=1.5),name="Bear (P10)",
                hovertemplate="S%{x}|Bear:%{y:.3f}<extra></extra>") %>%
      add_lines(x=~s$semaine,y=~s$moyenne,line=list(color="#e67e22",width=2.5),name="Base",
                hovertemplate="S%{x}|Base:%{y:.3f}<extra></extra>") %>%
      add_lines(x=~s$semaine,y=~s$p90,line=list(color="#2ecc71",dash="dash",width=1.5),name="Bull (P90)",
                hovertemplate="S%{x}|Bull:%{y:.3f}<extra></extra>") %>%
      add_lines(x=c(0,r$hr),y=c(r$px,r$px),line=list(color="#fff",dash="dot",width=1),
                name="Prix actuel",hoverinfo="skip") %>%
      theme_rs() %>%
      layout(xaxis=list(title="Semaines",gridcolor="#334",zeroline=FALSE),
             yaxis=list(title="Prix TSR20 (USD/kg)",gridcolor="#334",zeroline=FALSE,tickformat=".3f",
                        range=list(round(min(s$p10)*0.99,3),round(max(s$p90)*1.01,3))),
             margin=list(l=60,r=20,t=20,b=50))
  })
  output$t_mc <- renderTable({
    r <- sim_res()
    r$stats %>% filter(semaine %in% c(4,8,12,26,r$hr)) %>% distinct(semaine,.keep_all=TRUE) %>%
      mutate(Horizon=paste(semaine,"sem."),Bear=round(p10,3),Base=round(moyenne,3),
             Bull=round(p90,3),`P(up)%`=round(prob_hausse,1)) %>% select(Horizon,Bear,Base,Bull,`P(up)%`)
  },striped=TRUE,hover=TRUE,bordered=FALSE,style="color:#eee;background:#16213e;")
  output$g_hist_final <- renderPlotly({
    r <- sim_res(); pf <- r$traj[,r$hr+1]
    plot_ly(x=~pf,type="histogram",nbinsx=80,
            marker=list(color="rgba(230,126,34,0.7)",line=list(color="rgba(230,126,34,0.3)",width=0.5))) %>%
      add_lines(x=c(r$px,r$px),y=c(0,r$ns/8),line=list(color="#fff",dash="dot",width=2),name="Prix actuel") %>%
      theme_rs() %>%
      layout(xaxis=list(title="Prix TSR20 (USD/kg)",gridcolor="#334",tickformat=".3f"),
             yaxis=list(title="Frequence",gridcolor="#334"),showlegend=FALSE,
             margin=list(l=60,r=20,t=10,b=50))
  })
  output$g_prob <- renderPlotly({
    r <- sim_res(); s <- r$stats
    plot_ly() %>%
      add_lines(x=~s$semaine,y=~s$prob_hausse,line=list(color="#3498db",width=2),name="P(hausse)",
                hovertemplate="S%{x}|P(up):%{y:.1f}%<extra></extra>") %>%
      add_lines(x=~s$semaine,y=~s$prob_plus5,line=list(color="#2ecc71",width=1.5,dash="dash"),name="P(+5%)",
                hovertemplate="S%{x}|P(+5%):%{y:.1f}%<extra></extra>") %>%
      add_lines(x=~s$semaine,y=~s$prob_moins5,line=list(color="#e74c3c",width=1.5,dash="dash"),name="P(-5%)",
                hovertemplate="S%{x}|P(-5%):%{y:.1f}%<extra></extra>") %>%
      add_lines(x=c(0,r$hr),y=c(50,50),line=list(color="#666",dash="dot",width=1),name="Neutre",hoverinfo="skip") %>%
      theme_rs() %>%
      layout(xaxis=list(title="Semaines",gridcolor="#334"),
             yaxis=list(title="Probabilite (%)",gridcolor="#334",range=c(0,100)),
             margin=list(l=60,r=20,t=10,b=50))
  })
  output$interp_sim <- renderUI({
    r <- sim_res(); s4 <- r$stats %>% filter(semaine==4); s12 <- r$stats %>% filter(semaine==min(12,r$hr))
    biais <- if(s4$prob_hausse>60)"haussier" else if(s4$prob_hausse<40)"baissier" else "neutre"
    cb <- if(biais=="haussier")"#2ecc71" else if(biais=="baissier")"#e74c3c" else "#f39c12"
    nv <- if(r$vl<0.20)"faible" else if(r$vl<0.35)"moderee" else "elevee"
    tags$div(style="padding:15px;",
      fluidRow(
        column(4,tags$div(class="info-card",
          tags$p("Signal",class="metric-label"),
          tags$p(toupper(biais),class="metric-value",style=paste0("color:",cb,";")),
          tags$p(paste0("P(hausse) 4sem : ",round(s4$prob_hausse,1),"%"),class="metric-sub"))),
        column(4,tags$div(class="info-card",
          tags$p("Volatilite",class="metric-label"),
          tags$p(toupper(nv),class="metric-value",style="color:#f39c12;"),
          tags$p(paste0(r$vl*100,"%/an — ",format(r$ns,big.mark=" ")," sim."),class="metric-sub"))),
        column(4,tags$div(class="info-card",
          tags$p("IC80 a 12 semaines",class="metric-label"),
          tags$p(paste0("+-",round((s12$p90-s12$p10)/2,3)," USD/kg"),
                 class="metric-value",style="color:#3498db;"),
          tags$p(paste0("[",round(s12$p10,3)," - ",round(s12$p90,3),"]"),class="metric-sub")))
      ),
      tags$hr(),
      tags$p(paste0("Monte Carlo (",format(r$ns,big.mark=" ")," sim.) : biais ",biais,
                    " — P(hausse) ",round(s4$prob_hausse,1),"%. Central a ",r$hr," sem. : ",
                    round(r$stats$moyenne[r$hr],3)," USD/kg."),
             style="color:#aaa;font-size:13px;line-height:1.6;"),
      tags$p("Ces projections ne constituent pas un conseil en investissement.",
             style="color:#666;font-size:11px;margin-top:8px;")
    )
  })

  # ── SCENARIOS METEO ────────────────────────────────────────

  output$sc_desc <- renderUI({
    zones <- input$sc_zones; type <- input$sc_type; intensite <- input$sc_intensite
    poids <- c(ci=0.15,th=0.35,my=0.25,id=0.25)
    pt <- sum(poids[zones],na.rm=TRUE)
    dir <- if(type=="optimal") 1 else -1
    choc <- round(pt*intensite/100*dir*100,1)
    tags$p(paste0("Choc offre mondiale : ",choc,"% | ",paste(toupper(zones),collapse="+"),
                  " (",round(pt*100),"% prod. mondiale)"),style="color:#888;font-size:12px;margin-top:5px;")
  })
  sc_res <- eventReactive(
    list(input$sc_lancer,input$sc_zones,input$sc_type,input$sc_intensite), {
    req(input$sc_zones)
    h <- historique(); px <- if(!is.null(h)&&nrow(h)>0) tail(h$prix_tsr20,1) else 2.29
    zones <- input$sc_zones; type <- input$sc_type; intensite <- as.numeric(input$sc_intensite)/100
    poids <- c(ci=0.15,th=0.35,my=0.25,id=0.25); pt <- sum(poids[zones],na.rm=TRUE)
    dir <- if(type=="optimal") 1 else -1
    choc_offre <- pt*intensite*dir*(-1); ajust <- choc_offre*(-0.40)
    rsi_v <- if(!is.null(h)&&nrow(h)>0&&!is.na(tail(h$pre_rsi,1))) tail(h$pre_rsi,1) else 50
    d_base <- 0.03+(rsi_v-50)/50*0.02; d_choc <- d_base+ajust
    VOL <- 0.27; N <- 10000
    tb <- simuler_mc(px,d_base,VOL,N,12,seed=42); tc <- simuler_mc(px,d_choc,VOL,N,12,seed=43)
    sb <- calc_stats(tb,px); sc <- calc_stats(tc,px)
    delta12 <- round(sc$moyenne[12]-sb$moyenne[12],4)
    zn <- c(ci="CI",th="Thailand",my="Malaisie",id="Indonesie")
    tn <- c(secheresse="Secheresse",inondations="Inondations",optimal="Optimal")
    nom <- paste0(tn[type]," ",paste(zn[zones],collapse="+")," ",round(intensite*100),"%")
    list(sb=sb,sc=sc,px=px,d_base=d_base,d_choc=d_choc,choc_offre=choc_offre,
         ajust=ajust,delta12=delta12,zones=zones,type=type,intensite=intensite,pt=pt,nom=nom)
  },ignoreNULL=FALSE)
  observeEvent(input$sc_reset,{
    updateCheckboxGroupInput(session,"sc_zones",selected="ci")
    updateRadioButtons(session,"sc_type",selected="secheresse")
    updateSliderInput(session,"sc_intensite",value=20)
  })
  output$sc_base <- renderValueBox({r <- sc_res(); valueBox(paste(round(r$px,4),"USD/kg"),"Prix TSR20 base",icon("tag"),color="yellow")})
  output$sc_choc <- renderValueBox({
    r <- sc_res(); sc <- r$sc %>% filter(semaine==12)
    valueBox(paste(round(sc$moyenne,3),"USD/kg"),"Scenario central 12 sem.",icon("chart-line"),color=if(r$delta12>0)"red" else "green")
  })
  output$sc_delta <- renderValueBox({
    r <- sc_res(); d <- r$delta12
    valueBox(paste0(if(d>0)"+" else "",d," USD/kg"),"Impact vs base",icon(if(d>0)"arrow-up" else "arrow-down"),
             color=if(d>0.02)"red" else if(d< -0.02)"green" else "yellow")
  })
  output$sc_prob <- renderValueBox({
    r <- sc_res(); s <- r$sc %>% filter(semaine==4); p <- round(s$prob_hausse,1)
    valueBox(paste0(p,"%"),"P(hausse) 4 semaines",icon("percent"),
             color=if(p>55)"green" else if(p<45)"red" else "yellow")
  })
  output$sc_g_mc <- renderPlotly({
    r <- sc_res(); sb <- r$sb; sc <- r$sc
    ymin <- round(min(sc$p10,sb$p10)*0.993,3); ymax <- round(max(sc$p90,sb$p90)*1.007,3)
    plot_ly() %>%
      add_lines(x=~sb$semaine,y=~sb$moyenne,line=list(color="#aaa",dash="dot",width=1.5),name="Base (sans choc)",
                hovertemplate="S%{x}|Base:%{y:.4f}<extra></extra>") %>%
      add_lines(x=~sc$semaine,y=~sc$p10,line=list(color="#e74c3c",dash="dash",width=1.5),name="Bear choc",
                hovertemplate="S%{x}|Bear:%{y:.4f}<extra></extra>") %>%
      add_lines(x=~sc$semaine,y=~sc$moyenne,line=list(color="#e67e22",width=2.5),name=r$nom,
                hovertemplate="S%{x}|Central:%{y:.4f}<extra></extra>") %>%
      add_lines(x=~sc$semaine,y=~sc$p90,line=list(color="#2ecc71",dash="dash",width=1.5),name="Bull choc",
                hovertemplate="S%{x}|Bull:%{y:.4f}<extra></extra>") %>%
      add_lines(x=c(0,12),y=c(r$px,r$px),line=list(color="#fff",dash="dot",width=1),name="Prix actuel",hoverinfo="skip") %>%
      theme_rs() %>%
      layout(xaxis=list(title="Semaines",gridcolor="#334",zeroline=FALSE),
             yaxis=list(title="Prix TSR20 (USD/kg)",gridcolor="#334",tickformat=".4f",range=list(ymin,ymax)),
             margin=list(l=60,r=20,t=20,b=50))
  })
  output$sc_g_comp <- renderPlotly({
    r <- sc_res()
    sc_list <- list(
      list(nom="Secheresse CI severe",drift=0.25,col="#e74c3c"),
      list(nom="Inondations Asie -35%",drift=0.45,col="#e67e22"),
      list(nom="Scenario actuel",drift=r$d_choc,col="#f39c12"),
      list(nom="Conditions optimales",drift=-0.10,col="#2ecc71"))
    fig <- plot_ly()
    for (i in seq_along(sc_list)) {
      sc <- sc_list[[i]]; tr <- simuler_mc(r$px,sc$drift,0.27,3000,12,seed=40+i)
      st <- calc_stats(tr,r$px)
      fig <- fig %>% add_lines(x=~st$semaine,y=~st$moyenne,line=list(color=sc$col,width=2),name=sc$nom,
                                hovertemplate=paste0(sc$nom," S%{x}|%{y:.4f}<extra></extra>"))
    }
    fig %>% theme_rs() %>%
      layout(xaxis=list(title="Semaines",gridcolor="#334"),
             yaxis=list(title="Prix TSR20 (USD/kg)",gridcolor="#334",tickformat=".4f",
                        range=list(round(r$px*0.93,3),round(r$px*1.18,3))),
             annotations=list(list(text=paste0("Delta : ",round(r$delta12,4)," USD/kg"),
                                   x=0.98,y=0.05,xref="paper",yref="paper",
                                   showarrow=FALSE,font=list(color="#f39c12",size=13))),
             margin=list(l=60,r=20,t=10,b=50))
  })
  output$sc_interp <- renderUI({
    r <- sc_res(); d <- r$delta12
    tn <- c(secheresse="secheresse",inondations="inondations",optimal="conditions optimales")
    zn <- c(ci="Cote d'Ivoire",th="Thailande",my="Malaisie",id="Indonesie")
    impact <- if(abs(d)<0.005)"negligeable" else if(abs(d)<0.02) if(d>0)"modere haussier" else "modere baissier"
              else if(abs(d)<0.05) if(d>0)"significatif haussier" else "significatif baissier"
              else if(d>0)"fort impact haussier" else "fort impact baissier"
    cc <- if(d>0.02)"#e74c3c" else if(d< -0.02)"#2ecc71" else "#f39c12"
    tags$div(style="padding:10px;",
      tags$div(class="info-card",
        tags$p("Impact sur les prix",class="metric-label"),
        tags$p(toupper(impact),class="metric-value",style=paste0("color:",cc,";")),
        tags$p(paste0(if(d>0)"+" else "",d," USD/kg a 12 semaines"),class="metric-sub")),
      tags$hr(),
      tags$p(paste0("Scenario : ",tn[r$type]," affectant ",paste(zn[r$zones],collapse=" + "),
                    " (",round(r$pt*100),"% prod. mondiale). Choc : ",round(r$choc_offre*100,1),"%. ",
                    "Impact : ",impact," de ",abs(d)," USD/kg."),
             style="color:#aaa;font-size:12px;line-height:1.6;"),
      tags$p("Elasticite prix/offre : -0.40 (ANRPC 2015-2025)",style="color:#666;font-size:11px;margin-top:8px;")
    )
  })

  # ── PLANTATIONS ────────────────────────────────────────────

  pl_data <- reactive({ donnees_plantations %>% filter(continent %in% input$pl_cont) })

  output$pl_surface <- renderValueBox({
    valueBox(paste(sum(donnees_plantations$superficie_kha),"kha"),"Superficie totale mondiale",icon("globe"),color="green")
  })
  output$pl_prod <- renderValueBox({
    valueBox(paste(sum(donnees_plantations$production_kt),"kt"),"Production totale mondiale",icon("industry"),color="blue")
  })
  output$pl_top <- renderValueBox({
    d <- donnees_plantations %>% arrange(desc(production_kt))
    valueBox(d$pays[1],paste0("1er producteur — ",d$production_kt[1]," kt"),icon("trophy"),color="yellow")
  })
  output$pl_ci <- renderValueBox({
    ci <- donnees_plantations %>% filter(pays=="Cote d'Ivoire")
    rk <- which(donnees_plantations$pays[order(-donnees_plantations$production_kt)]=="Cote d'Ivoire")
    valueBox(paste0("#",rk),paste0("CI — ",ci$production_kt," kt — ",ci$part_mondiale_pct,"% mondial"),
             icon("flag"),color="orange")
  })
  output$carte_pl <- renderLeaflet({
    d <- pl_data(); if(nrow(d)==0) return(leaflet() %>% addTiles())
    metric <- input$pl_metric; valeurs <- d[[metric]]
    rayons <- sqrt(valeurs/max(valeurs,na.rm=TRUE))*60
    couleurs <- colorFactor(palette=c("#e67e22","#3498db"),domain=c("Asie","Afrique"))
    lu <- switch(metric,superficie_kha="kha",production_kt="kt",part_mondiale_pct="%",rendement_kg_ha="kg/ha")
    leaflet(d) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addCircleMarkers(lng=~lon,lat=~lat,radius=rayons,color=~couleurs(continent),
                       fillColor=~couleurs(continent),fillOpacity=0.7,stroke=TRUE,weight=2,
                       popup=~paste0("<b>",pays,"</b><br>Superficie : ",superficie_kha," kha<br>",
                                     "Production : ",production_kt," kt<br>Rendement : ",rendement_kg_ha," kg/ha<br>",
                                     "Age moyen : ",age_moyen_ans," ans<br>Grade : ",qualite_grade,"<br>",
                                     "Part mondiale : ",part_mondiale_pct,"%"),
                       label=~paste0(pays," — ",valeurs," ",lu)) %>%
      addLegend(pal=couleurs,values=~continent,title="Continent",position="bottomright") %>%
      setView(lng=80,lat=10,zoom=2)
  })
  output$pl_g_surf <- renderPlotly({
    d <- donnees_plantations %>% arrange(desc(superficie_kha))
    cc <- ifelse(d$continent=="Afrique","#e67e22","#3498db")
    plot_ly(d,x=~reorder(pays,superficie_kha),y=~superficie_kha,type="bar",
            marker=list(color=cc,line=list(color="#fff",width=0.5)),
            hovertemplate="%{x}<br>%{y} kha<extra></extra>") %>%
      theme_rs() %>%
      layout(xaxis=list(title="",tickangle=-45,gridcolor="#334"),
             yaxis=list(title="Superficie (kha)",gridcolor="#334"),
             margin=list(l=60,r=20,t=10,b=100))
  })
  output$pl_g_rend <- renderPlotly({
    d <- donnees_plantations %>% arrange(desc(rendement_kg_ha))
    plot_ly() %>%
      add_bars(x=~reorder(d$pays,d$rendement_kg_ha),y=~d$rendement_kg_ha,name="Rendement (kg/ha)",
               marker=list(color="#2ecc71",line=list(color="#fff",width=0.5)),
               hovertemplate="%{x}<br>%{y} kg/ha<extra></extra>") %>%
      add_lines(x=~reorder(d$pays,d$rendement_kg_ha),
                y=~d$production_kt/max(d$production_kt)*max(d$rendement_kg_ha),
                name="Production (norm.)",line=list(color="#e67e22",width=2),yaxis="y2",
                hovertemplate="%{x}<br>Prod. norm.: %{y:.0f}<extra></extra>") %>%
      theme_rs() %>%
      layout(xaxis=list(title="",tickangle=-45,gridcolor="#334"),
             yaxis=list(title="Rendement (kg/ha)",gridcolor="#334",side="left"),
             yaxis2=list(title="Production norm.",overlaying="y",side="right"),
             margin=list(l=60,r=60,t=10,b=100))
  })
  output$pl_table <- renderTable({
    donnees_plantations %>% arrange(desc(production_kt)) %>%
      mutate(Rang=row_number(),Pays=pays,Continent=continent,
             `Superficie kha`=superficie_kha,`Production kt`=production_kt,
             `Rendement kg/ha`=rendement_kg_ha,`Age moyen`=age_moyen_ans,
             `Part mondiale %`=part_mondiale_pct,Grade=qualite_grade) %>%
      select(Rang,Pays,Continent,`Superficie kha`,`Production kt`,`Rendement kg/ha`,`Age moyen`,`Part mondiale %`,Grade)
  },striped=TRUE,hover=TRUE,bordered=FALSE,style="color:#eee;background:#16213e;")

  # ── MANUFACTURIERS — MARCHÉ ─────────────────────────────────

  output$mf_total_nr <- renderValueBox({
    valueBox(paste(sum(donnees_manuf$conso_nr_kt),"kt"),"Conso. NR totale top 10",icon("leaf"),color="green")
  })
  output$mf_top_groupe <- renderValueBox({
    d <- donnees_manuf %>% arrange(desc(conso_nr_kt))
    valueBox(d$groupe[1],paste0("1er consommateur NR — ",d$conso_nr_kt[1]," kt"),icon("trophy"),color="yellow")
  })
  output$mf_top_ca <- renderValueBox({
    d <- donnees_manuf %>% arrange(desc(ca_mrd_usd))
    valueBox(paste(d$ca_mrd_usd[1],"Mrd USD"),paste0("Plus grand CA — ",d$groupe[1]),icon("dollar-sign"),color="blue")
  })
  output$mf_nb_groupes <- renderValueBox({
    valueBox("10 groupes","Top 10 manufacturiers mondiaux",icon("industry"),color="orange")
  })
  output$mf_g_conso <- renderPlotly({
    d <- donnees_manuf %>% arrange(desc(conso_nr_kt))
    cc <- case_when(d$continent=="Europe"~"#3498db",d$continent=="Amerique"~"#e74c3c",TRUE~"#2ecc71")
    plot_ly(d,x=~reorder(groupe,conso_nr_kt),y=~conso_nr_kt,type="bar",
            marker=list(color=cc,line=list(color="#fff",width=0.5)),
            hovertemplate=paste0("%{x}<br>Conso NR : %{y} kt<br>",
                                 "Pays : ",d$pays_origine,"<extra></extra>")) %>%
      theme_rs() %>%
      layout(xaxis=list(title="",tickangle=-45,gridcolor="#334"),
             yaxis=list(title="Consommation NR (kt/an)",gridcolor="#334"),
             annotations=list(list(text="Vert=Asie | Bleu=Europe | Rouge=Amerique",
                                   x=0.5,y=-0.35,xref="paper",yref="paper",
                                   showarrow=FALSE,font=list(color="#888",size=11))),
             margin=list(l=60,r=20,t=10,b=100))
  })
  output$mf_g_parts <- renderPlotly({
    d <- donnees_manuf %>% arrange(desc(part_marche_pct))
    couleurs_pie <- c("#e67e22","#3498db","#e74c3c","#2ecc71","#9b59b6",
                      "#1abc9c","#f39c12","#e91e63","#00bcd4","#8bc34a")
    plot_ly(d,labels=~groupe,values=~part_marche_pct,type="pie",
            marker=list(colors=couleurs_pie,line=list(color="#16213e",width=2)),
            textinfo="label+percent",
            hovertemplate="%{label}<br>Part marche : %{value}%<extra></extra>") %>%
      theme_rs() %>%
      layout(margin=list(l=20,r=20,t=10,b=10),showlegend=FALSE)
  })
  output$mf_g_ca <- renderPlotly({
    d <- donnees_manuf %>% arrange(desc(ca_mrd_usd))
    cc <- case_when(d$continent=="Europe"~"#3498db",d$continent=="Amerique"~"#e74c3c",TRUE~"#2ecc71")
    plot_ly(d,x=~reorder(groupe,ca_mrd_usd),y=~ca_mrd_usd,type="bar",
            marker=list(color=cc,line=list(color="#fff",width=0.5)),
            hovertemplate="%{x}<br>CA : %{y} Mrd USD<extra></extra>") %>%
      theme_rs() %>%
      layout(xaxis=list(title="",tickangle=-45,gridcolor="#334"),
             yaxis=list(title="CA (Mrd USD)",gridcolor="#334"),
             margin=list(l=60,r=20,t=10,b=100))
  })
  output$mf_g_efficacite <- renderPlotly({
    d <- donnees_manuf %>% mutate(nr_par_mrd_ca=round(conso_nr_kt/ca_mrd_usd,1))
    plot_ly() %>%
      add_markers(x=~d$ca_mrd_usd,y=~d$conso_nr_kt,text=~d$groupe,
                  marker=list(color="#e67e22",size=12,opacity=0.8,line=list(color="#fff",width=1)),

                  hovertemplate="%{text}<br>CA : %{x} Mrd USD<br>Conso NR : %{y} kt<extra></extra>") %>%
      theme_rs() %>%
      layout(xaxis=list(title="CA (Mrd USD)",gridcolor="#334"),
             yaxis=list(title="Consommation NR (kt)",gridcolor="#334"),
             margin=list(l=60,r=20,t=10,b=60))
  })
  output$mf_table <- renderTable({
    donnees_manuf %>% arrange(desc(conso_nr_kt)) %>%
      mutate(Rang=row_number(),Groupe=groupe,Pays=pays_origine,
             CA=ca_mrd_usd, NR_kt=conso_nr_kt,
             Part_pct=part_marche_pct, Usines=nb_usines) %>%
      select(Rang,Groupe,Pays,CA,NR_kt,Part_pct,Usines)
  },striped=TRUE,hover=TRUE,bordered=FALSE,style="color:#eee;background:#16213e;")
  us_data <- reactive({
    donnees_usines %>%
      filter(groupe %in% input$us_groupes) %>%
      filter(continent %in% input$us_cont)
  })
  output$us_total_sites <- renderValueBox({
    valueBox(paste(nrow(donnees_usines),"sites"),"Usines",icon("map-marker-alt"),color="green")
  })
  output$us_total_cap <- renderValueBox({
    valueBox(paste(sum(donnees_usines$capacite_kt),"kt"),"Capacite totale",icon("industry"),color="blue")
  })
  output$us_top_site <- renderValueBox({
    d <- donnees_usines %>% arrange(desc(capacite_kt))
    valueBox(d$site[1],paste0(d$groupe[1]," ",d$capacite_kt[1]," kt/an"),icon("trophy"),color="yellow")
  })
  output$us_asie_pct <- renderValueBox({
    pct <- round(sum(donnees_usines$continent=="Asie")/nrow(donnees_usines)*100)
    valueBox(paste0(pct,"%"),"Usines en Asie",icon("globe-asia"),color="orange")
  })
  output$carte_usines <- renderLeaflet({
    d <- us_data()
    if(nrow(d)==0) return(leaflet() %>% addTiles())
    pal <- c("#e67e22","#3498db","#e74c3c","#2ecc71","#9b59b6","#1abc9c","#f39c12","#e91e63")
    names(pal) <- c("Bridgestone","Michelin","Goodyear","Continental","Sumitomo","Pirelli","Hankook","Yokohama")
    cg <- colorFactor(palette=pal, domain=names(pal))
    r  <- sqrt(d$capacite_kt/max(d$capacite_kt))*35
    leaflet(d) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
      addCircleMarkers(lng=~lon,lat=~lat,radius=r,
                       color=~cg(groupe),fillColor=~cg(groupe),fillOpacity=0.8,stroke=TRUE,weight=2,
                       popup=~paste0("<b>",groupe,"</b> ",site,"<br>",pays,"<br>",capacite_kt," kt/an"),
                       label=~paste0(groupe," ",site)) %>%
      addLegend(pal=cg,values=~groupe,title="Groupe",position="bottomright") %>%
      setView(lng=50,lat=30,zoom=2)
  })
  output$us_g_cap <- renderPlotly({
    d <- us_data() %>% arrange(desc(capacite_kt))
    cc <- case_when(d$continent=="Europe"~"#3498db",d$continent=="Amerique"~"#e74c3c",TRUE~"#2ecc71")
    plot_ly(d,x=~reorder(paste0(groupe," ",site),capacite_kt),y=~capacite_kt,type="bar",
            marker=list(color=cc,line=list(color="#fff",width=0.5)),
            hovertemplate="%{x}<br>%{y} kt/an<extra></extra>") %>%
      theme_rs() %>%
      layout(xaxis=list(title="",tickangle=-45,gridcolor="#334"),
             yaxis=list(title="Capacite (kt/an)",gridcolor="#334"),
             margin=list(l=60,r=20,t=10,b=120))
  })
  output$us_g_cont <- renderPlotly({
    d <- donnees_usines %>% count(continent) %>% arrange(desc(n))
    plot_ly(d,labels=~continent,values=~n,type="pie",
            marker=list(colors=c("#2ecc71","#3498db","#e74c3c"),line=list(color="#16213e",width=2)),
            textinfo="label+percent",
            hovertemplate="%{label}<br>%{value} usines<extra></extra>") %>%
      theme_rs() %>% layout(margin=list(l=20,r=20,t=10,b=10),showlegend=FALSE)
  })
  output$mc_r_prod <- renderValueBox({
    cs <- calc_corr(donnees_corr_manuf$prod_pneus_mt,donnees_corr_manuf$prix_tsr20_moy)
    valueBox(if(!is.na(cs$r))as.character(cs$r) else "N/A","r Prod pneus/TSR20",icon("calculator"),
             color=if(!is.na(cs$r)&&abs(cs$r)>0.7)"green" else "yellow")
  })
  output$mc_r_conso <- renderValueBox({
    cs <- calc_corr(donnees_corr_manuf$conso_nr_mondial_kt,donnees_corr_manuf$prix_tsr20_moy)
    valueBox(if(!is.na(cs$r))as.character(cs$r) else "N/A","r Conso NR/TSR20",icon("calculator"),
             color=if(!is.na(cs$r)&&abs(cs$r)>0.7)"green" else "yellow")
  })
  output$mc_r_auto <- renderValueBox({
    cs <- calc_corr(donnees_corr_manuf$croissance_auto_pct,donnees_corr_manuf$prix_tsr20_moy)
    valueBox(if(!is.na(cs$r))as.character(cs$r) else "N/A","r Croissance auto/TSR20",icon("calculator"),
             color=if(!is.na(cs$r)&&abs(cs$r)>0.5)"green" else "yellow")
  })
  output$mc_r_ev <- renderValueBox({
    cs <- calc_corr(donnees_corr_manuf$ventes_ev_mpcs,donnees_corr_manuf$prix_tsr20_moy)
    valueBox(if(!is.na(cs$r))as.character(cs$r) else "N/A","r Ventes EV/TSR20",icon("calculator"),
             color=if(!is.na(cs$r)&&abs(cs$r)>0.5)"green" else "yellow")
  })
  output$mc_g_prod_prix <- renderPlotly({
    d <- donnees_corr_manuf
    r_val <- calc_corr(d$prod_pneus_mt,d$prix_tsr20_moy)$r
    plot_ly() %>%
      add_lines(x=~d$annee,y=~normaliser(d$prod_pneus_mt),
                line=list(color="#3498db",width=2.5),name="Production pneus",yaxis="y") %>%
      add_lines(x=~d$annee,y=~normaliser(d$prix_tsr20_moy),
                line=list(color="#e67e22",width=2.5),name="Prix TSR20",yaxis="y2") %>%
      theme_rs() %>%
      layout(xaxis=list(title="Annee",gridcolor="#334"),
             yaxis=list(title="Prod. pneus (norm.)",gridcolor="#334",side="left"),
             yaxis2=list(title="Prix TSR20 (norm.)",overlaying="y",side="right"),
             annotations=list(list(text=paste0("r = ",r_val),x=0.98,y=0.05,
                                   xref="paper",yref="paper",showarrow=FALSE,
                                   font=list(color="#e67e22",size=16))),
             margin=list(l=60,r=80,t=20,b=50))
  })
  output$mc_g_conso_prix <- renderPlotly({
    d <- donnees_corr_manuf
    r_val <- calc_corr(d$conso_nr_mondial_kt,d$prix_tsr20_moy)$r
    plot_ly() %>%
      add_markers(x=~d$conso_nr_mondial_kt,y=~d$prix_tsr20_moy,text=~d$annee,
                  marker=list(color="#2ecc71",size=12,line=list(color="#fff",width=1))) %>%
      theme_rs() %>%
      layout(xaxis=list(title="Conso NR mondiale (kt)",gridcolor="#334"),
             yaxis=list(title="Prix TSR20 (USD/kg)",gridcolor="#334"),
             annotations=list(list(text=paste0("r = ",r_val),x=0.98,y=0.05,
                                   xref="paper",yref="paper",showarrow=FALSE,
                                   font=list(color="#2ecc71",size=14))),
             margin=list(l=60,r=20,t=10,b=60))
  })
  output$mc_g_auto_prix <- renderPlotly({
    d <- donnees_corr_manuf
    r_val <- calc_corr(d$croissance_auto_pct,d$prix_tsr20_moy)$r
    plot_ly() %>%
      add_markers(x=~d$croissance_auto_pct,y=~d$prix_tsr20_moy,text=~d$annee,
                  marker=list(color="#e74c3c",size=12,line=list(color="#fff",width=1))) %>%
      theme_rs() %>%
      layout(xaxis=list(title="Croissance auto (%)",gridcolor="#334"),
             yaxis=list(title="Prix TSR20 (USD/kg)",gridcolor="#334"),
             annotations=list(list(text=paste0("r = ",r_val),x=0.98,y=0.05,
                                   xref="paper",yref="paper",showarrow=FALSE,
                                   font=list(color="#e74c3c",size=14))),
             margin=list(l=60,r=20,t=10,b=60))
  })
  output$mc_g_ev <- renderPlotly({
    d <- donnees_corr_manuf
    plot_ly() %>%
      add_lines(x=~d$annee,y=~d$ventes_ev_mpcs,
                line=list(color="#9b59b6",width=2.5),name="Ventes EV (Mpc)") %>%
      add_lines(x=~d$annee,y=~d$conso_nr_mondial_kt/max(d$conso_nr_mondial_kt)*max(d$ventes_ev_mpcs),
                line=list(color="#e67e22",width=2,dash="dash"),name="Conso NR norm.",yaxis="y2") %>%
      theme_rs() %>%
      layout(xaxis=list(title="Annee",gridcolor="#334"),
             yaxis=list(title="Ventes EV (millions)",gridcolor="#334",side="left"),
             yaxis2=list(title="Conso NR norm.",overlaying="y",side="right"),
             margin=list(l=60,r=80,t=10,b=50))
  })
  output$mc_interp <- renderUI({
    d <- donnees_corr_manuf
    r1 <- calc_corr(d$prod_pneus_mt,d$prix_tsr20_moy)$r
    r2 <- calc_corr(d$conso_nr_mondial_kt,d$prix_tsr20_moy)$r
    r3 <- calc_corr(d$ventes_ev_mpcs,d$prix_tsr20_moy)$r
    tags$div(style="padding:15px;",
      tags$div(class="info-card",
        tags$p("Signal cle",class="metric-label"),
        tags$p("DEMANDE AVAL",class="metric-value",style="color:#e67e22;"),
        tags$p("Conso. NR mondiale — meilleur predicateur TSR20",class="metric-sub")),
      tags$hr(),
      tags$p(paste0("Prod. pneus r=",r1," | Conso NR r=",r2," | EV r=",r3),
             style="color:#aaa;font-size:12px;"),
      tags$p("Sources : IRSG 2024, Rapports annuels, IEA 2024",
             style="color:#666;font-size:11px;margin-top:8px;")
    )
  })
  # ── SAISIE PRIX — Etat du message de retour ──────────────────
  saisie_msg  <- reactiveVal(NULL)
  saisie_type <- reactiveVal("success")

  output$s_status <- renderUI({
    msg <- saisie_msg()
    if (is.null(msg)) return(NULL)
    cls <- switch(saisie_type(),
      success = "alert alert-success",
      warning = "alert alert-warning",
      danger  = "alert alert-danger",
      "alert alert-info")
    tags$div(class=cls, style="margin-top:12px;", HTML(msg))
  })

  # ── SAISIE PRIX — Pre-remplissage depuis le JSON actuel ──────
  observeEvent(json_actuel(), {
    d <- json_actuel()
    if (is.null(d)) return()
    mm <- d$prix$marches_mondiaux
    if (is.null(mm)) return()
    dt <- tryCatch(as.Date(mm$date_cours), error=function(e) Sys.Date()-1)
    updateDateInput(session, "s_date", value=dt)
    if (!is.null(mm$sicom$prix_orig))
      updateNumericInput(session, "s_sicom", value=as.numeric(mm$sicom$prix_orig))
    if (!is.null(mm$tocom$prix_orig))
      updateNumericInput(session, "s_tocom", value=as.numeric(mm$tocom$prix_orig))
    if (!is.null(mm$shfe$prix_orig))
      updateNumericInput(session, "s_shfe",  value=as.numeric(mm$shfe$prix_orig))
    if (!is.null(mm$tocom$taux_jpy_usd))
      updateNumericInput(session, "s_jpy",   value=as.numeric(mm$tocom$taux_jpy_usd))
    if (!is.null(mm$shfe$taux_cny_usd))
      updateNumericInput(session, "s_cny",   value=as.numeric(mm$shfe$taux_cny_usd))
  }, ignoreNULL=TRUE)

  # ── SAISIE PRIX — Apercu temps reel ──────────────────────────
  output$s_preview <- renderTable({
    sicom <- input$s_sicom; tocom <- input$s_tocom
    shfe  <- input$s_shfe;  jpy   <- input$s_jpy; cny <- input$s_cny
    if (any(is.na(c(sicom, tocom, shfe, jpy, cny)))) return(NULL)
    sicom_usd <- round(sicom / 100, 4)
    tocom_usd <- round(tocom * jpy, 4)
    shfe_usd  <- round((shfe / 1000) * cny, 4)
    d   <- isolate(json_actuel())
    lgm <- if (!is.null(d)) as.numeric(d$prix$marches_mondiaux$lgm$prix_usd %||% NA) else NA
    data.frame(
      Marche  = c("LGM TSR20 (ref)", "SICOM TSR20", "TOCOM RSS3", "SHFE RU"),
      Saisie  = c(ifelse(is.na(lgm), "—", paste(round(lgm,4), "USD/kg (auto)")),
                  paste(sicom, "cts/kg"),
                  paste(tocom, "JPY/kg"),
                  paste(shfe, "CNY/t")),
      USD_kg  = c(ifelse(is.na(lgm), "—", format(lgm, nsmall=4)),
                  format(sicom_usd, nsmall=4),
                  format(tocom_usd, nsmall=4),
                  format(shfe_usd,  nsmall=4))
    )
  }, striped=TRUE, hover=TRUE, na="—")

  output$s_last_values <- renderUI({
    d <- json_actuel()
    if (is.null(d) || is.null(d$prix$marches_mondiaux)) return(NULL)
    mm <- d$prix$marches_mondiaux
    tags$div(style="color:#aaa;font-size:12px;margin-top:5px;",
      tags$b("Valeurs actuellement en base :", style="color:#ddd;"),
      tags$ul(style="margin-top:6px;",
        tags$li(paste("SICOM :", mm$sicom$prix_orig %||% "—", "cts/kg")),
        tags$li(paste("TOCOM :", mm$tocom$prix_orig %||% "—", "JPY/kg")),
        tags$li(paste("SHFE  :", mm$shfe$prix_orig  %||% "—", "CNY/t")),
        tags$li(paste("JPY/USD :", mm$tocom$taux_jpy_usd %||% "—")),
        tags$li(paste("CNY/USD :", mm$shfe$taux_cny_usd  %||% "—")),
        tags$li(paste("Date  :", mm$date_cours %||% "—"))
      )
    )
  })

  # ── SAISIE PRIX — Sauvegarde dans le JSON ────────────────────
  observeEvent(input$s_save, {
    sicom <- input$s_sicom; tocom <- input$s_tocom
    shfe  <- input$s_shfe;  jpy   <- input$s_jpy; cny <- input$s_cny

    if (any(is.na(c(sicom, tocom, shfe, jpy, cny)))) {
      saisie_type("warning")
      saisie_msg("&#9888; Remplissez tous les champs avant d'enregistrer.")
      return()
    }

    fichiers <- list.files("data/processed",
      pattern="rubbersignal_S\\d+_\\d+\\.json", full.names=TRUE)
    if (length(fichiers) == 0) {
      saisie_type("danger")
      saisie_msg("&#10007; Aucun fichier JSON trouve dans data/processed/")
      return()
    }
    fichier_json <- fichiers[which.max(file.mtime(fichiers))]

    tryCatch({
      d <- read_json(fichier_json, simplifyVector=FALSE)

      sicom_usd <- round(sicom / 100, 4)
      tocom_usd <- round(tocom * jpy, 4)
      shfe_usd  <- round((shfe / 1000) * cny, 4)
      lgm_usd   <- as.numeric(d$prix$marches_mondiaux$lgm$prix_usd %||% NA)
      spread    <- if (!is.na(lgm_usd)) round(sicom_usd - lgm_usd, 4) else NULL

      d$prix$marches_mondiaux$date_cours       <- as.character(input$s_date)
      d$prix$marches_mondiaux$spread_sicom_lgm <- spread
      d$prix$marches_mondiaux$sicom <- list(
        grade="TSR20", bourse="SGX/SICOM (Singapour)", ticker="TF",
        prix_orig=sicom, unite_orig="USD cts/kg", prix_usd=sicom_usd
      )
      d$prix$marches_mondiaux$tocom <- list(
        grade="RSS3", bourse="JPX/TOCOM (Tokyo)", ticker="TRB",
        prix_orig=tocom, unite_orig="JPY/kg", taux_jpy_usd=jpy, prix_usd=tocom_usd
      )
      d$prix$marches_mondiaux$shfe <- list(
        grade="SCR WF / RSS3", bourse="SHFE (Shanghai)", ticker="RU",
        prix_orig=shfe, unite_orig="CNY/tonne", taux_cny_usd=cny, prix_usd=shfe_usd
      )

      write_json(d, fichier_json, auto_unbox=TRUE, pretty=TRUE)
      refresh_trigger(refresh_trigger() + 1)

      saisie_type("success")
      saisie_msg(paste0("&#10003; Sauvegarde dans <b>", basename(fichier_json),
                        "</b> a ", format(Sys.time(), "%H:%M:%S")))
      showNotification("Cours enregistres avec succes !", type="message", duration=4)

    }, error=function(e) {
      saisie_type("danger")
      saisie_msg(paste0("&#10007; Erreur : ", conditionMessage(e)))
    })
  })

  # ── SIGNAUX FAIBLES ───────────────────────────────────────────

  # Helper interne : couleur selon score
  sf_col <- function(score) {
    s <- suppressWarnings(as.numeric(score))
    if (is.na(s)) return("orange")
    if (s >= 65) "green" else if (s >= 50) "yellow" else "red"
  }

  # Pre-RSI — score composite
  output$sf_pre_rsi <- renderValueBox({
    d <- json_actuel(); sf <- d$signaux_faibles
    if (is.null(d) || is.null(sf))
      return(valueBox("N/A", "Pre-RSI", icon("tachometer-alt"), color="orange"))
    score  <- as.numeric(sf$pre_rsi$score  %||% NA)
    signal <- sf$pre_rsi$signal %||% "?"
    valueBox(
      if (!is.na(score)) paste0(score, " / 100") else "N/A",
      paste0("Pre-RSI — ", signal),
      icon("tachometer-alt"), color = sf_col(score)
    )
  })

  output$sf_offre <- renderValueBox({
    d <- json_actuel(); sf <- d$signaux_faibles
    if (is.null(d) || is.null(sf))
      return(valueBox("N/A", "M1 Offre mondiale", icon("cloud-rain"), color="orange"))
    score <- as.numeric(sf$module1_meteo$score_offre_mondiale %||% NA)
    valueBox(
      if (!is.na(score)) paste0(score, "/100") else "N/A",
      "M1 Offre mondiale", icon("cloud-rain"), color = sf_col(score)
    )
  })

  output$sf_demande <- renderValueBox({
    d <- json_actuel(); sf <- d$signaux_faibles
    if (is.null(d) || is.null(sf))
      return(valueBox("N/A", "M3 Demande aval", icon("chart-line"), color="orange"))
    score <- as.numeric(sf$module3_demande_aval$score_demande %||% NA)
    valueBox(
      if (!is.na(score)) paste0(score, "/100") else "N/A",
      "M3 Demande aval", icon("chart-line"), color = sf_col(score)
    )
  })

  output$sf_terrain <- renderValueBox({
    d <- json_actuel(); sf <- d$signaux_faibles
    if (is.null(d) || is.null(sf))
      return(valueBox("N/A", "M6 Terrain CI", icon("map-marker-alt"), color="orange"))
    tc   <- sf$module6_terrain_ci
    prix <- as.numeric(tc$prix_bord_champ_fcfa %||% NA)
    sent <- as.numeric(tc$sentiment_planteurs   %||% NA)
    col  <- if (!is.na(sent) && sent >= 4) "green" else if (!is.na(sent) && sent >= 3) "yellow" else "red"
    valueBox(
      if (!is.na(prix)) paste0(round(prix), " FCFA/kg") else "N/A",
      "M6 Terrain CI", icon("map-marker-alt"), color = col
    )
  })

  output$sf_geo <- renderValueBox({
    d <- json_actuel(); sf <- d$signaux_faibles
    if (is.null(d) || is.null(sf))
      return(valueBox("N/A", "M7 Geopolitique", icon("globe-africa"), color="orange"))
    nb  <- as.integer(sf$module7_geopolitique$nb_articles %||% 0)
    col <- if (nb == 0) "green" else if (nb <= 2) "yellow" else "red"
    valueBox(
      paste0(nb, " article", if (nb != 1) "s" else ""),
      "M7 Geopolitique", icon("globe-africa"), color = col
    )
  })

  # M1 — Météo détail
  output$sf_meteo_detail <- renderUI({
    d <- json_actuel(); sf <- d$signaux_faibles
    if (is.null(d) || is.null(sf))
      return(tags$p(style="color:#555;", "Donnees indisponibles"))
    zones <- sf$module1_meteo$zones

    couleur_sig <- function(s) switch(s %||% "",
      "Favorable"        = "#2ecc71",
      "Neutre"           = "#f39c12",
      "Defavorable"      = "#e74c3c",
      "Tres defavorable" = "#c0392b", "#888")

    drapeaux <- c("Cote d'Ivoire"="#1F=CI 🇨🇮", "Thaïlande"="🇹🇭",
                  "Malaisie"="🇲🇾", "Indonésie"="🇮🇩")
    emoji <- c("Côte d'Ivoire"="🇨🇮","Thaïlande"="🇹🇭",
               "Malaisie"="🇲🇾","Indonésie"="🇮🇩")

    rows <- imap(emoji, function(flag, pays) {
      z <- zones[[pays]]; if (is.null(z)) return(NULL)
      score <- as.numeric(z$score_production %||% 0)
      sig   <- z$signal_production %||% "?"
      pluie <- as.numeric(z$pluie_30j_mm   %||% NA)
      temp  <- as.numeric(z$temp_moyenne_c  %||% NA)
      tags$div(style = "margin-bottom:11px;",
        tags$div(style = "display:flex;justify-content:space-between;",
          tags$span(style = "color:#eee;font-size:12px;font-weight:bold;",
                    paste(flag, pays)),
          tags$span(style = paste0("color:", couleur_sig(sig), ";font-size:11px;"), sig)
        ),
        tags$div(style = "color:#555;font-size:10px;margin:1px 0 2px 0;",
          paste0(if (!is.na(pluie)) paste0(pluie, " mm/30j") else "",
                 if (!is.na(temp))  paste0("  |  ", temp, " °C") else "")
        ),
        barre_score(score)
      )
    })

    score_g <- as.numeric(sf$module1_meteo$score_offre_mondiale %||% NA)
    sig_g   <- sf$module1_meteo$signal_offre %||% ""

    tags$div(
      rows,
      tags$hr(style = "border-color:#2a2a3e;margin:6px 0;"),
      tags$div(style = "color:#aaa;font-size:11px;margin-bottom:4px;", sig_g),
      barre_score(score_g, "Score offre mondiale pondere")
    )
  })

  # M2 — Devises
  output$sf_devises_detail <- renderUI({
    d <- json_actuel(); sf <- d$signaux_faibles
    if (is.null(d) || is.null(sf))
      return(tags$p(style="color:#555;", "Donnees indisponibles"))
    dev <- sf$module2_devises

    ligne_dev <- function(libelle, valeur, signal, ref_val) {
      val_n <- suppressWarnings(as.numeric(valeur))
      ecart <- if (!is.na(val_n) && !is.null(ref_val))
        round((val_n - ref_val) / ref_val * 100, 2) else NA
      col <- if (!is.na(ecart) && ecart < -2) "#2ecc71"
             else if (!is.na(ecart) && ecart > 2) "#e74c3c"
             else "#f39c12"
      tags$div(style = "margin-bottom:13px;",
        tags$div(style = "display:flex;justify-content:space-between;",
          tags$span(style = "color:#888;font-size:12px;", libelle),
          tags$span(style = "color:#eee;font-size:14px;font-weight:bold;",
                    as.character(valeur %||% "N/A"))
        ),
        tags$div(style = paste0("color:", col, ";font-size:11px;margin-top:2px;"),
          signal %||% "")
      )
    }

    tags$div(
      ligne_dev("USD / CNY", dev$USD_CNY, dev$signal_cny, 7.10),
      ligne_dev("USD / MYR", dev$USD_MYR, dev$signal_myr, 4.40),
      tags$hr(style = "border-color:#2a2a3e;"),
      tags$div(style = "display:flex;justify-content:space-between;margin-bottom:6px;",
        tags$span(style = "color:#888;font-size:12px;", "USD / XOF (FCFA)"),
        tags$span(style = "color:#eee;font-size:13px;",
                  as.character(dev$USD_XOF %||% "N/A"))
      ),
      tags$div(style = "display:flex;justify-content:space-between;",
        tags$span(style = "color:#888;font-size:12px;", "USD / IDR"),
        tags$span(style = "color:#eee;font-size:13px;",
                  as.character(dev$USD_IDR %||% "N/A"))
      ),
      tags$p(style = "color:#444;font-size:10px;margin-top:8px;",
             paste0("Source : ", dev$source %||% "?",
                    "  |  ", dev$date %||% ""))
    )
  })

  # M6 — Terrain CI
  output$sf_terrain_detail <- renderUI({
    d <- json_actuel(); sf <- d$signaux_faibles
    if (is.null(d) || is.null(sf))
      return(tags$p(style="color:#555;", "Donnees indisponibles"))
    tc <- sf$module6_terrain_ci

    sent <- suppressWarnings(as.numeric(tc$sentiment_planteurs %||% 0))
    etoiles <- paste0(
      paste(rep("★", min(5, max(0, round(sent)))), collapse=""),
      paste(rep("☆", 5 - min(5, max(0, round(sent)))), collapse="")
    )

    ligne <- function(lbl, val, suf="") {
      tags$div(style="display:flex;justify-content:space-between;margin-bottom:5px;",
        tags$span(style="color:#888;font-size:12px;", lbl),
        tags$span(style="color:#eee;font-size:12px;font-weight:bold;",
                  paste0(as.character(val %||% "N/A"), suf))
      )
    }

    tags$div(
      ligne("Prix bord champ APROMAC", tc$prix_bord_champ_fcfa, " FCFA/kg"),
      if (!is.null(tc$prix_marche_reel_fcfa) && !is.na(tc$prix_marche_reel_fcfa))
        ligne("Prix marche reel", tc$prix_marche_reel_fcfa, " FCFA/kg"),
      ligne("Disponibilite latex",   tc$statut_disponibilite),
      ligne("Cooperatives",          tc$activite_cooperatives),
      ligne("Saison de saignee",     tc$saison_saignee),
      ligne("Tension marche local",  tc$tension_marche_local),
      tags$div(style="margin:8px 0 4px 0;",
        tags$span(style="color:#888;font-size:12px;", "Sentiment planteurs  "),
        tags$span(style="color:#f39c12;font-size:16px;letter-spacing:2px;", etoiles),
        tags$span(style="color:#555;font-size:11px;", paste0("  ", round(sent), "/5"))
      ),
      if (!is.null(tc$note_terrain) && nchar(tc$note_terrain %||% "") > 3)
        tags$p(style="color:#666;font-size:11px;font-style:italic;margin-top:6px;border-left:2px solid #f39c12;padding-left:6px;",
               tc$note_terrain)
    )
  })

  # M3 — PMI & Demande
  output$sf_pmi_detail <- renderUI({
    d <- json_actuel(); sf <- d$signaux_faibles
    if (is.null(d) || is.null(sf))
      return(tags$p(style="color:#555;", "Donnees indisponibles"))
    pmi   <- sf$module3_demande_aval$pmi
    score <- as.numeric(sf$module3_demande_aval$score_demande %||% 50)
    sig   <- sf$module3_demande_aval$signal_demande %||% ""

    flags_pmi <- c(USA="🇺🇸", Europe="🇪🇺", Chine="🇨🇳", Inde="🇮🇳", Japon="🇯🇵")

    pmi_rows <- imap(flags_pmi, function(flag, pays) {
      p <- pmi[[pays]]; if (is.null(p)) return(NULL)
      val <- suppressWarnings(as.numeric(p$valeur %||% NA))
      col <- if (!is.na(val) && val >= 52) "#2ecc71"
             else if (!is.na(val) && val >= 49) "#f39c12"
             else "#e74c3c"
      tags$div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:5px;",
        tags$span(style="color:#aaa;font-size:12px;min-width:80px;",
                  paste(flag, pays)),
        tags$span(style=paste0("color:",col,";font-size:13px;font-weight:bold;"),
                  if (!is.na(val)) as.character(val) else "N/A"),
        tags$span(style="color:#555;font-size:10px;",
                  p$signal %||% "")
      )
    })

    tags$div(
      tags$p(style="color:#555;font-size:10px;margin-bottom:6px;",
             "PMI Manufacturing — seuil neutre = 50"),
      pmi_rows,
      tags$hr(style="border-color:#2a2a3e;margin:8px 0;"),
      barre_score(score, "Score demande composite"),
      tags$p(style="color:#aaa;font-size:11px;margin-top:4px;", sig)
    )
  })

  # M5 — Shipping
  output$sf_shipping_detail <- renderUI({
    d <- json_actuel(); sf <- d$signaux_faibles
    if (is.null(d) || is.null(sf))
      return(tags$p(style="color:#555;", "Donnees indisponibles"))
    sh <- sf$module5_shipping

    ligne <- function(lbl, val, suf="") {
      tags$div(style="display:flex;justify-content:space-between;margin-bottom:8px;",
        tags$span(style="color:#888;font-size:12px;", lbl),
        tags$span(style="color:#eee;font-size:14px;font-weight:bold;",
                  paste0(as.character(val %||% "N/A"), suf))
      )
    }

    tags$div(
      ligne("Petrole WTI",           sh$wti_valeur, " USD/baril"),
      ligne("Baltic Dry Index (BDI)", sh$bdi_valeur, " pts"),
      tags$hr(style="border-color:#2a2a3e;margin:4px 0 8px 0;"),
      ligne("Fret CI → Europe",      sh$tarif_ci_eu_usd_tonne, " USD/tonne"),
      ligne("Delai Abidjan → Rotterdam", sh$delai_abidjan_rotterdam, " jours"),
      tags$hr(style="border-color:#2a2a3e;margin:4px 0 8px 0;"),
      tags$p(style="color:#aaa;font-size:11px;", sh$signal_fret %||% ""),
      tags$p(style="color:#444;font-size:10px;", sh$source %||% "")
    )
  })

  # M4 — Stocks
  output$sf_stocks_detail <- renderUI({
    d <- json_actuel(); sf <- d$signaux_faibles
    if (is.null(d) || is.null(sf))
      return(tags$p(style="color:#555;", "Donnees indisponibles"))
    st <- sf$module4_stocks

    pays_list <- list(
      list(flag="🇲🇾", nom="Malaisie",      t=st$malaisie_tonnes,   m=st$malaisie_mois,   td=st$malaisie_tendance),
      list(flag="🇹🇭", nom="Thaïlande",     t=st$thailande_tonnes,  m=st$thailande_mois,  td=st$thailande_tendance),
      list(flag="🇮🇩", nom="Indonesie",     t=st$indonesie_tonnes,  m=st$indonesie_mois,  td=st$indonesie_tendance),
      list(flag="🇨🇮", nom="Cote d'Ivoire", t=st$ci_tonnes,         m=st$ci_mois,         td=st$ci_tendance)
    )

    rows <- map(pays_list, function(p) {
      val_txt <- if (is.null(p$t) || is.na(p$t)) "A saisir"
                 else paste0(format(as.integer(p$t), big.mark=" "), " t")
      col     <- if (is.null(p$t) || is.na(p$t)) "#444" else "#eee"
      tags$div(style="display:flex;justify-content:space-between;margin-bottom:6px;",
        tags$span(style="color:#aaa;font-size:12px;",
                  paste(p$flag, p$nom, "—", p$m %||% "")),
        tags$span(style=paste0("color:",col,";font-size:12px;font-weight:bold;"), val_txt)
      )
    })

    tags$div(
      rows,
      tags$hr(style="border-color:#2a2a3e;margin:6px 0;"),
      tags$div(style="display:flex;justify-content:space-between;",
        tags$span(style="color:#888;font-size:12px;", "Port Abidjan"),
        tags$span(style="color:#eee;font-size:12px;",
          paste0(st$abidjan_statut %||% "?", " — ", st$abidjan_delai_jours %||% "?", "j"))
      ),
      tags$p(style="color:#444;font-size:10px;margin-top:8px;",
             st$note %||% "Donnees a completer mensuellement")
    )
  })

  # ── RUBBER NEWS ───────────────────────────────────────────────
  news_data <- reactiveVal(NULL)
  news_ts   <- reactiveVal(NULL)

  # Auto-load au démarrage (après le premier rendu UI)
  session$onFlushed(function() {
    tryCatch({
      d <- charger_news()
      news_data(d)
      news_ts(Sys.time())
    }, error = function(e) NULL)
  }, once = TRUE)

  # Rafraîchissement manuel
  observeEvent(input$news_refresh, {
    showNotification("Loading rubber news...", id = "nload",
                     duration = NULL, type = "message")
    tryCatch({
      d <- charger_news()
      news_data(d)
      news_ts(Sys.time())
      removeNotification("nload")
      showNotification(
        paste(if (!is.null(d)) nrow(d) else 0, "articles loaded"),
        type = "message", duration = 3
      )
    }, error = function(e) {
      removeNotification("nload")
      showNotification(paste("Error:", conditionMessage(e)),
                       type = "error", duration = 8)
    })
  }, ignoreInit = TRUE)

  output$news_last_update <- renderUI({
    t <- news_ts()
    if (is.null(t))
      return(tags$span(style = "color:#666;font-size:12px;",
                       "Not loaded — click Refresh"))
    tags$span(style = "color:#666;font-size:12px;",
              paste("Updated:", format(t, "%d/%m/%Y %H:%M")))
  })

  output$news_cards <- renderUI({
    d <- news_data()

    if (is.null(d) || nrow(d) == 0) {
      return(tags$div(
        style = "padding:60px;text-align:center;color:#555;",
        tags$div(style = "font-size:40px;margin-bottom:14px;",
                 icon("newspaper")),
        tags$p(style = "font-size:14px;color:#777;",
               "Click 'Refresh' to load rubber industry news."),
        tags$p(style = "font-size:11px;color:#444;",
               "Sources: Google News RSS — TSR20 prices, plantations, manufacturers, trade events")
      ))
    }

    cat_sel <- input$news_cat
    if (!is.null(cat_sel) && cat_sel != "ALL")
      d <- d %>% filter(categorie == cat_sel)

    if (nrow(d) == 0)
      return(tags$div(
        style = "padding:30px;text-align:center;color:#555;",
        "No articles found for this category."
      ))

    cards <- map(seq_len(nrow(d)), function(i) {
      r <- d[i, ]
      tags$div(
        style = paste0(
          "background:#1a1a2e;border-left:4px solid ", r$couleur, ";",
          "padding:14px 16px;margin:6px 0;border-radius:4px;"
        ),
        # Badge + source + date
        tags$div(
          style = "margin-bottom:7px;",
          tags$span(
            style = paste0(
              "background:", r$couleur, ";color:#000;",
              "font-size:10px;font-weight:bold;",
              "padding:2px 8px;border-radius:10px;margin-right:8px;"
            ),
            r$categorie
          ),
          tags$span(
            style = "color:#666;font-size:11px;",
            paste0(r$source_nom, " · ", r$date_aff)
          )
        ),
        # Title (clickable)
        tags$a(
          href   = r$lien,
          target = "_blank",
          style  = paste0(
            "color:#eee;font-weight:600;font-size:14px;",
            "text-decoration:none;line-height:1.4;display:block;margin-bottom:5px;"
          ),
          r$titre
        ),
        # Description
        if (nchar(trimws(r$description)) > 10)
          tags$p(
            style = "color:#999;font-size:12px;margin:0;line-height:1.6;",
            r$description
          )
      )
    })

    tags$div(
      tags$div(
        style = "color:#555;font-size:11px;margin-bottom:10px;padding:2px 4px;",
        paste0(nrow(d), " articles — click any title to read on source site")
      ),
      tags$div(
        style = "max-height:74vh;overflow-y:auto;padding-right:4px;",
        cards
      )
    )
  })

}
shinyApp(ui = ui, server = server)

