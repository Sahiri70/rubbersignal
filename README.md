# RubberSignal

Plateforme d'intelligence de marché sur le caoutchouc naturel (TSR20).  
Objectif : devenir la référence d'information prix pour les marchés africains du caoutchouc naturel.

**Terminal :** https://rubbersignal.shinyapps.io/rubbersignal-terminal/  
**GitHub :** https://github.com/Sahiri70/rubbersignal  
**Substack :** rubbersignal.substack.com

---

## Architecture des scripts

Le pipeline est composé de 5 scripts R exécutés dans cet ordre :

**`01_collect_prices.R`** (v3.1) — Collecte des prix  
Récupère le prix TSR20 depuis LGM Malaysia (scraping), avec repli sur IndexMundi puis World Bank Pink Sheet. Intègre le prix bord-champ APROMAC (FCFA) et calcule le **RSCI** (RubberSignal Chain Index) : part de la valeur internationale captée par le planteur ivoirien. Produit un JSON enrichi dans `data/processed/`.

**`02_collect_news.R`** (v3.0) — Actualités  
Interroge NewsAPI pour collecter les nouvelles caoutchouc de la semaine. Enrichit le JSON produit par le script 01. Requiert la variable d'environnement `NEWSAPI_KEY`.

**`03_build_report.R`** (v4.1) — Rapport bilingue  
Assemble les données des scripts 01, 02 et 04 pour générer deux rapports Markdown FR + EN à destination de Substack. Intègre le RSCI et le Pré-RSI. **Doit être exécuté en dernier.**

**`04_collect_signals.R`** (v2.1) — Signaux faibles  
Collecte 7 modules de données : météo des zones productrices (Open-Meteo), devises (ExchangeRate API), demande aval (FRED + PMI manuel), stocks mondiaux (manuel), fret maritime (BDI + WTI via FRED), terrain CI exclusif (manuel hebdomadaire), tensions géopolitiques (NewsAPI). Calcule le **Pré-RSI composite 0–100**. Requiert `EXCHANGERATE_KEY`, `FRED_KEY`, `NEWSAPI_KEY`.

**`05_simulate.R`** (v1.0) — Modèles Thompson  
Simulation Monte Carlo GBM (mouvement brownien géométrique) sur le prix TSR20. Produit des fourchettes probabilistes de prix futurs. Référence : *Simulation, A Modeler's Approach* — J.R. Thompson.

---

## Pipeline hebdomadaire

Le pipeline tourne automatiquement via **GitHub Actions chaque lundi à 5h00 UTC**.

Ordre d'exécution :

```
01_collect_prices.R
       ↓
02_collect_news.R
       ↓
04_collect_signals.R
       ↓
05_simulate.R
       ↓
03_build_report.R   ← lit les sorties des 4 scripts précédents
```

Pour lancer manuellement depuis RStudio :

```r
source("scripts/01_collect_prices.R")
source("scripts/02_collect_news.R")
source("scripts/04_collect_signals.R")
source("scripts/05_simulate.R")
source("scripts/03_build_report.R")
```

Variables d'environnement requises dans `.Renviron` :

```
NEWSAPI_KEY=...
EXCHANGERATE_KEY=...
FRED_KEY=...
```

---

## Méthodologie RSCI

Le **RubberSignal Chain Index (RSCI)** mesure la part de la valeur internationale effectivement captée par le planteur ivoirien :

```
RSCI (%) = (Prix_APROMAC_FCFA / DRC_officiel) / FCFA_par_USD
           / Prix_LGM_TSR20_USD × 100
```

Documentation complète : [`docs/architecture/RSCI_methodologie_v1.md`](docs/architecture/RSCI_methodologie_v1.md)

Résultat S24 2026 : **RSCI = 58.8%** (mécanisme légal CHPH : 63%, écart = −4.2 pts)

---

## Structure du projet

```
rubbersignal/
├── scripts/          # Pipeline R (01 → 02 → 04 → 05 → 03)
├── data/
│   ├── raw/          # Données brutes hebdomadaires
│   └── processed/    # JSON enrichis (rubbersignal_SXX_YYYY.json)
└── docs/
    ├── architecture/ # Méthodologie RSCI
    ├── strategy/     # Document de stratégie produit
    └── rapports/     # Rapports générés
```
