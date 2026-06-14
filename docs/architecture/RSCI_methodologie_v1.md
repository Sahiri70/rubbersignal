# RubberSignal — RSCI v1 : Méthodologie et intégration
**Session du 13-14/06/2026 — Script 01 v3.1**

---

## 1. Objectif de la session

Construire un premier indicateur public et défendable mesurant la part
de la valeur internationale (LGM TSR20) effectivement captée par le
planteur ivoirien — **RSCI : RubberSignal Chain Index**.

Point de départ : une première tentative de calcul de prix FOB Abidjan
(RAPI) donnait **+40% vs LGM** — rejetée comme incohérente (extrapolation
linéaire erronée à partir d'un seul point historique). Pivot vers un
indicateur de **part de chaîne de valeur** plutôt qu'un prix absolu.

---

## 2. RSCI — Formule et logique

```
RSCI (%) = (Prix_APROMAC_FCFA / DRC_officiel) / FCFA_par_USD
           / Prix_LGM_TSR20_USD × 100
```

**Logique** : le prix APROMAC (fonds de tasse, humide) est converti en
équivalent sec via le DRC officiel, puis exprimé en USD, puis comparé au
prix international de référence (LGM TSR20). Le résultat est comparé au
mécanisme légal de répartition CHPH (63% planteurs).

---

## 3. Paramètres sourcés

| Paramètre | Valeur | Source |
|---|---|---|
| `PRIX_APROMAC_FCFA` | 474 FCFA/kg (juin 2026) | FratMat, 01/06/2026 (Didier Assoumou) |
| `DRC_OFFICIEL` | 60% | Décision CHPH n°0037, mai 2022 |
| `MECANISME_OFFICIEL` | 63% planteurs / 37% transformateurs | CHPH, depuis mai 2022 (61%/39% avant) |
| Cadre légal | Loi N°2017-540 du 03/08/2017 | Création du CHPH |
| `XOF_PAR_EUR` | 655.957 (parité fixe, ne change jamais) | Arrimage CFA/EUR |
| `TAUX_EUR_USD` | 1.157 (vérifié 12/06/2026) | xe.com / Reuters |
| `FCFA_PAR_USD` (dérivé) | 566.95 | = XOF_PAR_EUR / TAUX_EUR_USD |
| Prix LGM TSR20 | 2.3685 USD/kg | Donnée manuelle (03/06/2026) |

---

## 4. Corrections apportées au script 01 (v3.0 → v3.1)

1. **APROMAC** : 359 → 474 FCFA/kg, mois "2026-05" → "2026-06"
2. **Taux de change** : `TAUX_FCFA_USD <- 0.00158` (≈633 FCFA/USD, non
   sourcé) remplacé par une dérivation sourcée : parité fixe XOF/EUR
   (655.957) ÷ EUR/USD (1.157, à vérifier chaque semaine) = 566.95
   FCFA/USD. **Impact** : ce changement seul fait passer le RSCI de
   56.1% (estimation initiale non sourcée) à 58.8%.
3. **Bug FRED corrigé** : la série `PRUBBUSDM` (corrigée de
   `PNRGQUSDM` plus tôt dans la session) répond HTTP 200 mais retourne
   un **indice général base 100 (~110.4)**, pas un prix TSR20 en
   USD/kg. Si elle était restée active, la prochaine fois que LGM +
   IndexMundi échouent (= chaque semaine actuellement), le script
   aurait fixé `prix_actuel_usd <- 110.39`. Retirée de la chaîne de
   décision active, conservée en commentaire de référence.
4. **Ajout bloc RSCI** (section 7B) : `DRC_OFFICIEL`,
   `MECANISME_OFFICIEL`, `prix_planteur_sec_usd`, `rsci_pct`,
   `rsci_ecart_pts`, avec sources légales en commentaires.
5. **JSON enrichi** : nouveau bloc `prix$rsci` exploitable par script
   03 et le terminal.

---

## 5. Résultat S24 validé (14/06/2026)

| Indicateur | Valeur |
|---|---|
| Prix planteur (DRC-corrigé) | 1.3935 USD/kg |
| **RSCI** | **58.8%** |
| Mécanisme officiel CHPH | 63% |
| **Écart** | **-4.2 points** |
| Spread export-planteur (V2, confidentiel) | 1.532 USD/kg (64.7%) |

**Angle éditorial possible** : *"RSCI 58.8% : le planteur ivoirien sous
le seuil légal de 63% — un écart de 4.2 points malgré la hausse du prix
APROMAC à 474 FCFA"*. Sourcé, défendable, correspond aux tensions
documentées dans la presse (CHPH n°0037 vs revendication BNEDT à 75%).

---

## 6. État Git / déploiement

- Script 01 v3.1 (394 lignes, 15.7 KB) poussé sur GitHub :
  commit `2dafe3b` (`a6b74a3..2dafe3b master -> master`)
- Conflit rebase résolu sur `data/processed/rubbersignal_S24_2026.json`
  et `data/raw/prix_bm_2026_S24.csv` (le pipeline GitHub Actions avait
  tourné lundi avec l'ancien v3.0 et produit ses propres fichiers S24)
- **⚠ EN SUSPENS** : `git checkout --theirs` a renvoyé "Updated 0 paths
  from the index" pour ces 2 fichiers — signal ambigu. Vérification du
  contenu (JSON valide ? marqueurs de conflit `<<<<<<<` dans le CSV ?)
  demandée mais **résultat jamais reçu** (fin de session).

---

## 7. Points en suspens — prochaines étapes

1. **PRIORITAIRE** : vérifier l'intégrité de
   `rubbersignal_S24_2026.json` et `prix_bm_2026_S24.csv` post-rebase
2. Intégrer RSCI dans script 03 (rapport hebdo FR/EN Substack)
3. Intégrer RSCI dans le terminal (value box Dashboard) — reporté
   depuis plusieurs sessions
4. Housekeeping git : supprimer `fin_app.R` (temporaire), ajouter
   `rsconnect/` au `.gitignore`, commiter `docs/`
5. **RSCI v2** : prime TSR10 vs TSR20 (usines CI majoritairement
   TSR10) — non quantifiée, nécessite une vraie source avant
   intégration
6. **Spread V2** : même limite méthodologique DRC que RSCI — formule à
   revoir avant toute publication

---

## 8. Roadmap RubberSignal globale (rappel, hors RSCI)

- Traduction multilingue FR/EN/DE/ZH/MS
- Accès payant Substack
- RAPI (RubberSignal Africa Price Index) — FOB Abidjan + Douala + fret
- RSPI (RubberSignal Price Index) — synthèse SICOM/TOCOM/SHFE
- Correction erreur "argument 3" plotly (non bloquante)

