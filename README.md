# ⚽ Football Transfers Analysis — Big 5 European Leagues

Analyse et visualisation du marché des transferts dans les 5 grands championnats européens (Premier League, La Liga, Ligue 1, Serie A, Bundesliga) de 2002 à 2025.

> **Le marché des transferts du Big 5 européen : une inflation incontrôlée ?**

## 🔗 Liens

- 📊 [Rapport d'analyse](https://frantrab08.github.io/football-transfers-analysis/projet.html)
- 🚀 [Application Shiny interactive](https://frantrab08.shinyapps.io/football-transfers-analysis/)

---

## 📌 Questions de recherche

1. Comment les dépenses en transferts ont-elles évolué depuis 2002 ?
2. Quels sont les 25 transferts les plus chers de l'histoire ?
3. Les clubs paient-ils les joueurs à leur juste valeur ?
4. La valeur marchande des joueurs a-t-elle explosé dans toutes les ligues ?
5. Qui vend le plus — et à qui ? Balance nette par championnat.
6. Combien coûte un joueur selon son poste ?
7. Les championnats vendent-ils surtout entre eux ou à l'étranger ?

---

## 📊 Visualisations

| # | Titre | Type |
|---|-------|------|
| 1 | Dépenses par ligue au fil des saisons | Ligne interactive |
| 2 | Top 25 transferts les plus chers | Bar chart |
| 3 | Prix payé vs valeur marchande | Scatter plot |
| 4 | Inflation des valeurs des joueurs | Ligne par ligue |
| 5 | Balance nette achat/vente par ligue | Bar chart |
| 6 | Coût médian par poste | Bar chart horizontal |
| 7 | Ventes internes vs externes | Bar chart empilé |

---

## 🛠️ Technologies

- **R** — langage principal
- **Shiny / shinydashboard** — application interactive
- **ggplot2 / plotly** — visualisations
- **tidyverse** — manipulation des données
- **RMarkdown** — rapport statique

---

## 📁 Structure

```
football-transfers-analysis/
│
├── data/
│   ├── transfers.csv          # 157 186 transferts
│   ├── players.csv            # 47 703 joueurs
│   ├── clubs.csv              # 797 clubs
│   └── player_valuations.csv  # 616 378 estimations de valeur
│
├── app-shiny.R
├── projet.Rmd
└── projet.html
```

---

## 📦 Source des données

Données issues du projet open source [transfermarkt-datasets](https://github.com/dcaribou/transfermarkt-datasets), maintenu par David Cariboo — scraping automatisé de transfermarkt.com, mis à jour chaque semaine.  
Licence : Creative Commons Attribution 4.0 (CC BY 4.0)

> ⚠️ **Note** : les fichiers CSV utilisés dans ce projet sont une snapshot statique des données, non synchronisée avec le dépôt source. Ils reflètent l'état du dataset au moment de l'analyse (avril 2025).
>
>
---

*Projet réalisé dans le cadre du cours de Datavisualisation — SIAD 2025-2026*
