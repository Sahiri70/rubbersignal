# RubberSignal — Document de stratégie
**Auteur :** Martial Sahiri  
**Localisation :** Le Locle, Suisse  
**Date :** Juin 2026

---

## Vision

RubberSignal est une plateforme d'intelligence de marché sur le caoutchouc naturel (TSR20).
Objectif : devenir la référence d'information prix pour les marchés africains du caoutchouc naturel.

---

## État actuel du Terminal (V5)

**URL :** https://rubbersignal.shinyapps.io/rubbersignal-terminal/  
**GitHub :** https://github.com/Sahiri70/rubbersignal  
**Substack :** rubbersignal.substack.com

### 8 onglets opérationnels
1. Dashboard — Prix TSR20 historique + Pré-RSI
2. Corrélations — TSR20 vs signaux faibles
3. Simulateur — Monte Carlo GBM interactif
4. Scénarios météo — What-if conditionnel
5. Plantations — Carte mondiale hévéa (FAO/ANRPC 2024)
6. Manufacturiers — Marché (Top 10 consommateurs NR)
7. Manufacturiers — Usines (Carte 15 usines mondiales)
8. Manufacturiers — Corrélations (Demande aval vs TSR20)

### Pipeline automatisé (GitHub Actions — lundi 5h00 UTC)
- Script 01 : Collecte prix (LGM + World Bank Pink Sheet secours)
- Script 02 : Collecte news (NewsAPI)
- Script 04 : Signaux faibles (Pré-RSI 0-100)
- Script 05 : Monte Carlo GBM
- Script 03 : Rapport FR/EN (Substack)

---

## Architecture de prix — Réflexion stratégique

### Problème fondamental identifié
Il n'existe pas UN prix mondial du caoutchouc naturel.
Il existe une constellation de prix locaux :

| Zone | Prix référence | Statut |
|---|---|---|
| Asie (FOB Malaysia) | LGM TSR20 | Manuel hebdomadaire |
| Afrique Ouest (FOB Abidjan) | APROMAC + spread | Calculé ✅ |
| Europe (CIF Rotterdam) | LGM + fret | À construire |
| Amériques (CIF Houston) | LGM + fret | À construire |
| Chine (CIF Shanghai) | SHFE ajusté | À construire |

### Avantage compétitif clé
Spread export-planteur CI : ~1.802 USD/kg (76.1% valeur export) — confidentiel V2.
**Personne d'autre ne calcule et ne publie ce prix chaque semaine.**

### Sprint 7 planifié : RubberSignal African Price Index (RAPI)
Construction d'un indice de prix africain basé sur :
- Prix FOB Abidjan (APROMAC)
- Prix FOB Douala (Cameroun)
- Indices de fret maritime (Freightos Baltic Index — gratuit)
- Pondération par volumes de production

**C'est le vrai 0→1 de RubberSignal — pas une copie des bourses asiatiques.**

---

## Sources de données

| Source | Type | Délai | Accès |
|---|---|---|---|
| LGM Malaysia | TSR20 FOB Klang | Temps réel | Manuel (scraping instable) |
| World Bank Pink Sheet | TSR20 mensuel | 2-3 mois | Automatique ✅ |
| APROMAC CI | Prix bord-champ FCFA | Hebdomadaire | Manuel |
| SIPRI | Volumes production | Annuel | Gratuit |
| Freightos Baltic Index | Fret maritime | Hebdomadaire | Gratuit |

---

## Roadmap restante

### Court terme (prochaines sessions)
- [ ] Traduction multilingue FR/EN/DE/ZH/MS
- [ ] Accès payant Substack intégration
- [ ] Correction erreur argument 3 plotly
- [ ] Automatisation prix (World Bank Pink Sheet comme secours)

### Moyen terme (Sprint 7)
- [ ] RAPI — RubberSignal African Price Index
- [ ] Prix FOB Abidjan publié chaque semaine
- [ ] Extension prix FOB Douala + Lagos

### Long terme
- [ ] API RubberSignal (accès données pour tiers)
- [ ] Partenariat APROMAC officiel
- [ ] Version mobile

---

## Modèle économique cible

- Substack payant : 29-49 USD/mois (professionnels)
- API accès données : 200-500 USD/mois (industriels)
- Rapports personnalisés : 500-2000 USD (one-shot)

