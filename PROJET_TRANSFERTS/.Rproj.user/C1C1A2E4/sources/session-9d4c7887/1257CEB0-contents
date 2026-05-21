# ============================================================
#  Application Shiny — Marché des transferts du Big 5
#  Datavisualisation SIAD 2025-2026
# ============================================================

# --- Installation
# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("scales")
# install.packages("ggrepel")
# install.packages("plotly")
# install.packages("lubridate")

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(scales)
library(ggrepel)
library(plotly)
library(lubridate)

# ============================================================
# 0. CONSTANTES
# ============================================================

BIG5_CODES <- c("GB1", "ES1", "FR1", "IT1", "L1")

LEAGUE_LABELS <- c(
  GB1 = "Premier League",
  ES1 = "La Liga",
  FR1 = "Ligue 1",
  IT1 = "Serie A",
  L1  = "Bundesliga"
)

LEAGUE_COLORS <- c(
  "Premier League" = "#3d85c8",
  "La Liga"        = "#e06c44",
  "Ligue 1"        = "#8e44ad",
  "Serie A"        = "#27ae60",
  "Bundesliga"     = "#e74c3c"
)

POSITION_COLORS <- c(
  "Attack"     = "#e06c44",
  "Midfield"   = "#3d85c8",
  "Defender"   = "#27ae60",
  "Goalkeeper" = "#8e44ad"
)

# Postes en francais pour les checkboxes (valeur = anglais, label = francais)
POSITIONS_FR <- c(
  "Attaque"  = "Attack",
  "Milieu"   = "Midfield",
  "Defense"  = "Defender",
  "Gardien"  = "Goalkeeper"
)

TRADUCTION_POSTES <- c(
  "Centre-Forward"     = "Avant-centre",
  "Second Striker"     = "Deuxieme attaquant",
  "Left Winger"        = "Ailier gauche",
  "Right Winger"       = "Ailier droit",
  "Attacking Midfield" = "Milieu offensif",
  "Central Midfield"   = "Milieu central",
  "Defensive Midfield" = "Milieu defensif",
  "Left Midfield"      = "Milieu gauche",
  "Right Midfield"     = "Milieu droit",
  "Centre-Back"        = "Defenseur central",
  "Left-Back"          = "Lateral gauche",
  "Right-Back"         = "Lateral droit",
  "Goalkeeper"         = "Gardien"
)

# Liste des saisons disponibles
SAISONS <- c(
  "Toutes",
  "02/03","03/04","04/05","05/06","06/07","07/08","08/09","09/10",
  "10/11","11/12","12/13","13/14","14/15","15/16","16/17","17/18",
  "18/19","19/20","20/21","21/22","22/23","23/24","24/25","25/26"
)

# ============================================================
# 1. CHARGEMENT & PREPARATION DES DONNEES
# ============================================================

season_to_year <- function(s) {
  yr <- as.integer(substr(s, 1, 2))
  ifelse(yr > 50, 1900L + yr, 2000L + yr)
}

# Chargement des CSV (meme dossier que app.R)
transfers  <- read_csv("data/transfers.csv",         show_col_types = FALSE)
players    <- read_csv("data/players.csv",           show_col_types = FALSE)
clubs      <- read_csv("data/clubs.csv",             show_col_types = FALSE)
valuations <- read_csv("data/player_valuations.csv", show_col_types = FALSE)

# Correspondance club -> ligue
clubs_big5 <- clubs %>%
  filter(domestic_competition_id %in% BIG5_CODES) %>%
  select(club_id, domestic_competition_id) %>%
  mutate(league_name = LEAGUE_LABELS[domestic_competition_id])

# Fonction pour determiner la fenetre de mercato selon le mois
get_mercato <- function(date_col) {
  m <- month(as.Date(date_col))
  case_when(
    m %in% c(6, 7, 8, 9) ~ "Ete",
    m %in% c(1, 2)       ~ "Hiver",
    TRUE                  ~ "Autre"
  )
}

# Table enrichie avec ligue acheteur + saison + mercato
transfers_clean <- transfers %>%
  mutate(
    year           = season_to_year(transfer_season),
    transfer_fee_m = transfer_fee / 1e6,
    market_value_m = market_value_in_eur / 1e6,
    mercato        = get_mercato(transfer_date)
  ) %>%
  filter(!is.na(year)) %>%
  left_join(clubs_big5 %>% select(club_id, league_name),
            by = c("to_club_id" = "club_id"))

# Table avec ligue vendeur ET acheteur (viz5 et viz7)
transfers_full <- transfers %>%
  mutate(
    year           = season_to_year(transfer_season),
    transfer_fee_m = transfer_fee / 1e6,
    mercato        = get_mercato(transfer_date)
  ) %>%
  filter(!is.na(year)) %>%
  left_join(clubs_big5 %>% select(club_id, league_name) %>% rename(to_league   = league_name),
            by = c("to_club_id"   = "club_id")) %>%
  left_join(clubs_big5 %>% select(club_id, league_name) %>% rename(from_league = league_name),
            by = c("from_club_id" = "club_id"))

# Postes traduits en francais (viz6)
players_postes <- players %>%
  select(player_id, position, sub_position) %>%
  mutate(
    sub_position_fr = TRADUCTION_POSTES[sub_position],
    sub_position_fr = if_else(is.na(sub_position_fr), sub_position, sub_position_fr)
  )

# ============================================================
# 2. UI
# ============================================================

# Bloc de filtres réutilisable par viz
# Contient : slider période, sélecteur saison précise, checkboxes mercato
filtres_saison_mercato <- function(id_years, id_saison, id_mercato,
                                   year_min = 2002, year_max = 2025) {
  tagList(
    # Slider pour choisir une période large (ex: 2010 - 2020)
    sliderInput(id_years, "Periode :",
                min = year_min, max = year_max,
                value = c(year_min, year_max), sep = "", step = 1),
    # Selecteur pour zoomer sur une saison precise (ex: "23/24")
    selectInput(id_saison, "Saison precise (optionnel) :",
                choices  = SAISONS,
                selected = "Toutes"),
    helpText("Si une saison est selectionnee, le slider est ignore."),
    # Mercato ete ou hiver
    checkboxGroupInput(id_mercato, "Fenetre de mercato :",
                       choices  = c("Mercato d'ete"   = "Ete",
                                    "Mercato d'hiver" = "Hiver"),
                       selected = c("Ete", "Hiver"))
  )
}

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Transferts Big 5", titleWidth = 230),
  
  dashboardSidebar(
    width = 230,
    sidebarMenu(
      menuItem("Accueil",               tabName = "accueil", icon = icon("house")),
      menuItem("Depenses par ligue",    tabName = "viz1",    icon = icon("chart-line")),
      menuItem("Top transferts",        tabName = "viz2",    icon = icon("ranking-star")),
      menuItem("Prix vs Valeur",        tabName = "viz3",    icon = icon("scale-balanced")),
      menuItem("Inflation des valeurs", tabName = "viz4",    icon = icon("arrow-trend-up")),
      menuItem("Balance nette",         tabName = "viz5",    icon = icon("right-left")),
      menuItem("Cout par poste",        tabName = "viz6",    icon = icon("shirt")),
      menuItem("Ventes int. vs ext.",   tabName = "viz7",    icon = icon("globe"))
    )
  ),
  
  dashboardBody(
    
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f4f6f9; }
      .box { border-radius: 6px; }
      .info-box { border-radius: 6px; }
      .description-box {
        background: #fff;
        border-left: 4px solid #3d85c8;
        padding: 12px 16px;
        margin-bottom: 14px;
        border-radius: 0 6px 6px 0;
        font-size: 13px;
        color: #444;
        line-height: 1.6;
      }
    "))),
    
    tabItems(
      
      # -------------------------------------------------------
      # ACCUEIL
      # -------------------------------------------------------
      tabItem(tabName = "accueil",
              fluidRow(
                box(width = 12, status = "primary",
                    h2(icon("futbol"), " Marche des transferts du Big 5 europeen"),
                    h4("Une inflation incontrolee ?", style = "color:#666; margin-top:-8px;"),
                    hr(),
                    p("Ce tableau de bord explore l'economie des transferts dans les cinq grands
              championnats europeens (Premier League, La Liga, Ligue 1, Serie A, Bundesliga)
              a partir des donnees open source de Transfermarkt, couvrant plus de 150 000
              transferts depuis 2002."),
                    p("Naviguez entre les 7 visualisations via le menu a gauche.
              Chaque page dispose de ses propres filtres : saison precise, fenetre de mercato
              (ete ou hiver), ligues, etc.")
                )
              ),
              fluidRow(
                infoBox("Transferts recenses", "157 186",
                        icon = icon("exchange-alt"), color = "blue",   width = 3),
                infoBox("Record absolu", "222 M€ (Neymar, 2017)",
                        icon = icon("euro-sign"),   color = "red",    width = 3),
                infoBox("Periode couverte", "2002 - 2025",
                        icon = icon("calendar"),    color = "green",  width = 3),
                infoBox("Championnats", "5 grandes ligues",
                        icon = icon("trophy"),      color = "yellow", width = 3)
              )
      ),
      
      # -------------------------------------------------------
      # VIZ 1 — Depenses par ligue
      # -------------------------------------------------------
      tabItem(tabName = "viz1",
              div(class = "description-box",
                  strong("Depenses en transferts par championnat."),
                  " Montants cumules des transferts entrants payants. La Premier League domine
          largement depuis 2010, avec une acceleration nette apres 2017 (Neymar, 222M€).
          En 2023-2024, les clubs anglais depensent autant que les quatre autres ligues reunies."
              ),
              fluidRow(
                box(width = 3, title = "Filtres", status = "primary", solidHeader = TRUE,
                    filtres_saison_mercato("v1_years", "v1_saison", "v1_mercato"),
                    hr(),
                    checkboxGroupInput("v1_ligues", "Championnats :",
                                       choices  = names(LEAGUE_COLORS),
                                       selected = names(LEAGUE_COLORS)),
                    radioButtons("v1_metric", "Afficher :",
                                 choices  = c("Depenses totales (M€)" = "total_depenses_m",
                                              "Nombre de transferts"   = "nb_transferts"),
                                 selected = "total_depenses_m"),
                    checkboxInput("v1_neymar", "Annoter 2017 (Neymar)", value = TRUE)
                ),
                box(width = 9, title = "Evolution des depenses en transferts",
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("plot_v1", height = "460px")
                )
              )
      ),
      
      # -------------------------------------------------------
      # VIZ 2 — Top transferts
      # -------------------------------------------------------
      tabItem(tabName = "viz2",
              div(class = "description-box",
                  strong("Les transferts les plus chers de l'histoire."),
                  " Presque tous les records ont ete etablis apres 2016. Premier League et La Liga
          concentrent la majorite des plus grosses transactions. Montants en euros courants,
          non corriges de l'inflation."
              ),
              fluidRow(
                box(width = 3, title = "Filtres", status = "warning", solidHeader = TRUE,
                    filtres_saison_mercato("v2_years", "v2_saison", "v2_mercato"),
                    hr(),
                    checkboxGroupInput("v2_ligues", "Championnat d'arrivee :",
                                       choices  = c(names(LEAGUE_COLORS), "Autre"),
                                       selected = c(names(LEAGUE_COLORS), "Autre")),
                    sliderInput("v2_n", "Nombre de transferts affiches :",
                                min = 5, max = 40, value = 25, step = 5)
                ),
                box(width = 9, title = "Top transferts les plus chers",
                    status = "warning", solidHeader = TRUE,
                    plotlyOutput("plot_v2", height = "520px")
                )
              )
      ),
      
      # -------------------------------------------------------
      # VIZ 3 — Prix vs valeur marchande
      # -------------------------------------------------------
      tabItem(tabName = "viz3",
              div(class = "description-box",
                  strong("Prix paye vs valeur marchande Transfermarkt."),
                  " Le ratio median est proche de 1 (juste prix), mais certains clubs —
          notamment anglais — paient regulierement une prime. La valeur Transfermarkt
          est elle-meme influencee par le marche, ce qui cree une circularite partielle."
              ),
              fluidRow(
                box(width = 3, title = "Filtres", status = "success", solidHeader = TRUE,
                    filtres_saison_mercato("v3_years", "v3_saison", "v3_mercato"),
                    hr(),
                    checkboxGroupInput("v3_ligues", "Championnats :",
                                       choices  = names(LEAGUE_COLORS),
                                       selected = names(LEAGUE_COLORS)),
                    sliderInput("v3_ratio_max", "Ratio maximum affiche :",
                                min = 1, max = 10, value = 4, step = 0.5),
                    radioButtons("v3_type", "Type de graphique :",
                                 choices  = c("Boxplot par ligue"     = "box",
                                              "Scatter prix / valeur" = "scatter"),
                                 selected = "box")
                ),
                box(width = 9, title = "Ratio prix paye / valeur marchande",
                    status = "success", solidHeader = TRUE,
                    plotlyOutput("plot_v3", height = "460px")
                )
              )
      ),
      
      # -------------------------------------------------------
      # VIZ 4 — Inflation des valeurs
      # -------------------------------------------------------
      tabItem(tabName = "viz4",
              div(class = "description-box",
                  strong("Valeur marchande mediane par championnat."),
                  " La Premier League se detache nettement a partir de 2013-2014. En 2024,
          un joueur median de PL vaut 2 a 3 fois plus qu'un joueur de Serie A ou Bundesliga."
              ),
              fluidRow(
                box(width = 3, title = "Filtres", status = "danger", solidHeader = TRUE,
                    # Viz4 utilise les valuations, pas les transferts -> filtre annee simple
                    sliderInput("v4_years", "Annees :",
                                min = 2000, max = 2026, value = c(2005, 2026), sep = "", step = 1),
                    checkboxGroupInput("v4_ligues", "Championnats :",
                                       choices  = names(LEAGUE_COLORS),
                                       selected = names(LEAGUE_COLORS)),
                    checkboxInput("v4_log", "Echelle logarithmique", value = FALSE),
                    helpText("Les valuations ne sont pas liees a une fenetre de mercato.")
                ),
                box(width = 9, title = "Valeur marchande mediane (M€)",
                    status = "danger", solidHeader = TRUE,
                    plotlyOutput("plot_v4", height = "460px")
                )
              )
      ),
      
      # -------------------------------------------------------
      # VIZ 5 — Balance nette
      # -------------------------------------------------------
      tabItem(tabName = "viz5",
              div(class = "description-box",
                  strong("Balance nette des transferts (ventes - achats)."),
                  " La Premier League est massivement deficitaire. Ligue 1 et Bundesliga sont
          excedentaires : elles vendent plus qu'elles n'achetent. La balance est
          recalculee dynamiquement sur la periode et le mercato selectionnes."
              ),
              fluidRow(
                box(width = 3, title = "Filtres", status = "primary", solidHeader = TRUE,
                    filtres_saison_mercato("v5_years", "v5_saison", "v5_mercato"),
                    helpText("La balance est recalculee sur la selection.")
                ),
                box(width = 9, title = "Balance nette par championnat (M€)",
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("plot_v5", height = "420px")
                )
              )
      ),
      
      # -------------------------------------------------------
      # VIZ 6 — Cout par poste
      # -------------------------------------------------------
      tabItem(tabName = "viz6",
              div(class = "description-box",
                  strong("Cout median d'un transfert selon le poste."),
                  " Les postes offensifs sont les plus chers. Les milieux defensifs ont vu
          leur valeur progresser. Les gardiens restent en bas malgre quelques exceptions."
              ),
              fluidRow(
                box(width = 3, title = "Filtres", status = "warning", solidHeader = TRUE,
                    filtres_saison_mercato("v6_years", "v6_saison", "v6_mercato"),
                    hr(),
                    # Checkboxes en francais — la valeur retournee est en anglais (Attack, etc.)
                    checkboxGroupInput("v6_positions", "Familles de postes :",
                                       choices  = POSITIONS_FR,
                                       selected = POSITIONS_FR),
                    sliderInput("v6_min_obs", "Nb min. d'observations :",
                                min = 10, max = 100, value = 30, step = 10)
                ),
                box(width = 9, title = "Frais de transfert median par poste (M€)",
                    status = "warning", solidHeader = TRUE,
                    plotlyOutput("plot_v6", height = "460px")
                )
              )
      ),
      
      # -------------------------------------------------------
      # VIZ 7 — Ventes internes vs externes
      # -------------------------------------------------------
      tabItem(tabName = "viz7",
              div(class = "description-box",
                  strong("Ventes internes vs ventes a l'etranger."),
                  " Zone foncee = ventes hors du championnat. Zone claire = ventes internes.
          La Premier League vend surtout en interne. Bundesliga et Ligue 1 exportent
          quasi exclusivement vers d'autres championnats."
              ),
              fluidRow(
                box(width = 3, title = "Filtres", status = "success", solidHeader = TRUE,
                    filtres_saison_mercato("v7_years", "v7_saison", "v7_mercato"),
                    hr(),
                    checkboxGroupInput("v7_ligues", "Championnats vendeurs :",
                                       choices  = names(LEAGUE_COLORS),
                                       selected = names(LEAGUE_COLORS))
                ),
                box(width = 9, title = "Recettes de ventes : internes vs externes (M€)",
                    status = "success", solidHeader = TRUE,
                    plotlyOutput("plot_v7", height = "480px")
                )
              )
      )
      
    ) # fin tabItems
  ) # fin dashboardBody
) # fin dashboardPage

# ============================================================
# 3. SERVER
# ============================================================

# Fonction utilitaire : applique le filtre période + saison + mercato
# Si une saison précise est sélectionnée, le slider est ignoré
filtre_saison <- function(df, years_input, saison_input, mercato_input) {
  df %>%
    filter(
      if (saison_input == "Toutes") {
        year >= years_input[1] & year <= years_input[2]
      } else {
        transfer_season == saison_input
      },
      mercato %in% mercato_input
    )
}

server <- function(input, output, session) {
  
  # ---- VIZ 1 : evolution des depenses ----
  output$plot_v1 <- renderPlotly({
    metric <- input$v1_metric
    ylabel <- if (metric == "total_depenses_m") "Depenses (M€)" else "Nb de transferts"
    
    df <- transfers_clean %>%
      filter(transfer_fee > 0, !is.na(league_name),
             league_name %in% input$v1_ligues) %>%
      filtre_saison(input$v1_years, input$v1_saison, input$v1_mercato) %>%
      group_by(year, league_name) %>%
      summarise(total_depenses_m = sum(transfer_fee_m, na.rm = TRUE),
                nb_transferts    = n(), .groups = "drop")
    
    validate(need(nrow(df) > 0, "Aucune donnee pour ces filtres."))
    
    p <- ggplot(df, aes(x = year, y = .data[[metric]],
                        color = league_name, group = league_name,
                        text = paste0("<b>", league_name, "</b><br>",
                                      year, " : ", round(.data[[metric]], 1),
                                      if (metric == "total_depenses_m") "M€" else " transferts"))) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2) +
      { if (input$v1_neymar && metric == "total_depenses_m" &&
            input$v1_saison == "Toutes" && 2017 %in% df$year)
        list(
          geom_vline(xintercept = 2017, linetype = "dashed", color = "grey50"),
          annotate("text", x = 2017.2,
                   y = max(df$total_depenses_m, na.rm = TRUE) * 0.88,
                   label = "Neymar\n222M€", hjust = 0, size = 3,
                   color = "grey40", fontface = "italic")
        )
      } +
      scale_color_manual(values = LEAGUE_COLORS) +
      scale_y_continuous(labels = comma_format(
        suffix = if (metric == "total_depenses_m") "M€" else "")) +
      labs(x = NULL, y = ylabel, color = NULL) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom", panel.grid.minor = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })
  
  # ---- VIZ 2 : top transferts ----
  output$plot_v2 <- renderPlotly({
    df <- transfers_clean %>%
      filter(transfer_fee > 0) %>%
      filtre_saison(input$v2_years, input$v2_saison, input$v2_mercato) %>%
      mutate(league_name = if_else(is.na(league_name), "Autre", league_name)) %>%
      filter(league_name %in% input$v2_ligues) %>%
      slice_max(transfer_fee, n = input$v2_n, with_ties = FALSE) %>%
      mutate(
        label_unique = paste0(player_name, " -> ", to_club_name, " (", year, ")"),
        label_unique = fct_reorder(label_unique, transfer_fee_m)
      )
    
    validate(need(nrow(df) > 0, "Aucune donnee pour ces filtres."))
    
    p <- ggplot(df, aes(x = label_unique, y = transfer_fee_m, fill = league_name,
                        text = paste0("<b>", player_name, "</b><br>",
                                      from_club_name, " -> ", to_club_name, "<br>",
                                      round(transfer_fee_m), "M€ — ", year))) +
      geom_col(alpha = 0.9) +
      # Montant à l'intérieur, décalé à gauche pour rester dans la barre
      geom_text(aes(label = paste0(round(transfer_fee_m), "M€"),
                    x = label_unique,
                    y = transfer_fee_m * 0.72),
                hjust = 0.5, size = 3, color = "white", fontface = "bold") +
      coord_flip() +
      scale_fill_manual(values = c(LEAGUE_COLORS, "Autre" = "#95a5a6")) +
      scale_y_continuous(labels = label_dollar(prefix = "", suffix = "M€"),
                         expand = expansion(mult = c(0, 0.18))) +
      labs(x = NULL, y = "Montant (M€)", fill = "Championnat") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom", panel.grid.major.y = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.1))
  })
  
  # ---- VIZ 3 : ratio prix / valeur ----
  output$plot_v3 <- renderPlotly({
    df <- transfers_clean %>%
      filter(transfer_fee > 0, market_value_in_eur > 0,
             !is.na(league_name), league_name %in% input$v3_ligues) %>%
      filtre_saison(input$v3_years, input$v3_saison, input$v3_mercato) %>%
      mutate(ratio = transfer_fee / market_value_in_eur) %>%
      filter(ratio <= input$v3_ratio_max)
    
    validate(need(nrow(df) > 0, "Aucune donnee pour ces filtres."))
    
    if (input$v3_type == "box") {
      df_box <- df %>%
        mutate(league_ordered = fct_reorder(league_name, ratio, median))
      
      p <- ggplot(df_box, aes(x = league_ordered, y = ratio, fill = league_name,
                              text = league_name)) +
        geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
        geom_boxplot(alpha = 0.75, outlier.alpha = 0.2) +
        scale_fill_manual(values = LEAGUE_COLORS) +
        coord_cartesian(ylim = c(0, input$v3_ratio_max)) +
        labs(x = NULL, y = "Ratio prix / valeur marchande") +
        theme_minimal(base_size = 12) +
        theme(legend.position = "none", panel.grid.minor = element_blank())
      
    } else {
      p <- ggplot(df, aes(x = market_value_m, y = transfer_fee_m,
                          color = league_name,
                          text = paste0("<b>", player_name, "</b><br>",
                                        "Valeur : ", round(market_value_m, 1), "M€<br>",
                                        "Prix : ",   round(transfer_fee_m, 1), "M€<br>",
                                        "Ratio : ",  round(transfer_fee / market_value_in_eur, 2)))) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
        geom_point(alpha = 0.35, size = 1.8) +
        scale_color_manual(values = LEAGUE_COLORS) +
        labs(x = "Valeur marchande (M€)", y = "Prix paye (M€)", color = NULL) +
        theme_minimal(base_size = 12) +
        theme(panel.grid.minor = element_blank())
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })
  
  # ---- VIZ 4 : inflation valeurs (pas de filtre mercato, donnees de valuations) ----
  output$plot_v4 <- renderPlotly({
    df <- valuations %>%
      mutate(year = year(as.Date(date))) %>%
      filter(player_club_domestic_competition_id %in% BIG5_CODES,
             year >= input$v4_years[1], year <= input$v4_years[2]) %>%
      mutate(league_name = LEAGUE_LABELS[player_club_domestic_competition_id]) %>%
      filter(league_name %in% input$v4_ligues) %>%
      group_by(year, league_name) %>%
      summarise(median_value_m = median(market_value_in_eur, na.rm = TRUE) / 1e6,
                .groups = "drop")
    
    validate(need(nrow(df) > 0, "Aucune donnee pour ces filtres."))
    
    p <- ggplot(df, aes(x = year, y = median_value_m,
                        color = league_name, group = league_name,
                        text = paste0("<b>", league_name, "</b><br>",
                                      year, " : ", round(median_value_m, 2), "M€"))) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(values = LEAGUE_COLORS) +
      { if (input$v4_log)
        scale_y_log10(labels = label_dollar(prefix = "", suffix = "M€"))
        else
          scale_y_continuous(labels = label_dollar(prefix = "", suffix = "M€"))
      } +
      labs(x = NULL, y = "Valeur mediane (M€)", color = NULL) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })
  
  # ---- VIZ 5 : balance nette ----
  output$plot_v5 <- renderPlotly({
    achats <- transfers_clean %>%
      filter(transfer_fee > 0, !is.na(league_name)) %>%
      filtre_saison(input$v5_years, input$v5_saison, input$v5_mercato) %>%
      group_by(league_name) %>%
      summarise(achats_m = sum(transfer_fee_m, na.rm = TRUE), .groups = "drop")
    
    ventes <- transfers_full %>%
      filter(transfer_fee > 0, !is.na(from_league)) %>%
      filtre_saison(input$v5_years, input$v5_saison, input$v5_mercato) %>%
      group_by(from_league) %>%
      summarise(ventes_m = sum(transfer_fee_m, na.rm = TRUE), .groups = "drop") %>%
      rename(league_name = from_league)
    
    df <- ventes %>%
      left_join(achats, by = "league_name") %>%
      mutate(
        achats_m    = replace_na(achats_m, 0),
        balance_m   = ventes_m - achats_m,
        statut      = if_else(balance_m >= 0, "Excedentaire", "Deficitaire"),
        league_name = fct_reorder(league_name, balance_m),
        label       = paste0(if_else(balance_m > 0, "+", ""),
                             round(balance_m / 1000, 1), " Mds€")
      )
    
    validate(need(nrow(df) > 0, "Aucune donnee pour ces filtres."))
    
    p <- ggplot(df, aes(x = league_name, y = balance_m, fill = statut,
                        text = paste0("<b>", league_name, "</b><br>",
                                      "Ventes : ", round(ventes_m / 1000, 1), " Mds€<br>",
                                      "Achats : ", round(achats_m / 1000, 1), " Mds€<br>",
                                      "Balance : ", label))) +
      geom_col(width = 0.6, alpha = 0.9) +
      geom_hline(yintercept = 0, color = "grey30", linewidth = 0.8) +
      geom_text(aes(label = label, vjust = if_else(balance_m > 0, -0.5, 1.3)),
                size = 3.5, fontface = "bold", color = "grey20") +
      scale_fill_manual(values = c("Excedentaire" = "#27ae60", "Deficitaire" = "#e74c3c"),
                        name = NULL) +
      scale_y_continuous(labels = label_dollar(prefix = "", suffix = "M€", big.mark = " "),
                         expand = expansion(mult = c(0.2, 0.2))) +
      labs(x = NULL, y = "Balance nette (M€)") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom", panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.3, y = -0.15))
  })
  
  # ---- VIZ 6 : cout par poste ----
  output$plot_v6 <- renderPlotly({
    df <- transfers_clean %>%
      filter(transfer_fee > 0, !is.na(league_name)) %>%
      filtre_saison(input$v6_years, input$v6_saison, input$v6_mercato) %>%
      left_join(players_postes, by = "player_id") %>%
      filter(!is.na(sub_position), position %in% input$v6_positions) %>%
      group_by(position, sub_position_fr) %>%
      summarise(median_fee_m = median(transfer_fee_m, na.rm = TRUE),
                nb = n(), .groups = "drop") %>%
      filter(nb >= input$v6_min_obs) %>%
      mutate(sub_position_fr = fct_reorder(sub_position_fr, median_fee_m))
    
    validate(need(nrow(df) > 0, "Aucune donnee pour ces filtres."))
    
    p <- ggplot(df, aes(x = sub_position_fr, y = median_fee_m, fill = position,
                        text = paste0("<b>", sub_position_fr, "</b><br>",
                                      "Mediane : ", round(median_fee_m, 1), "M€<br>",
                                      "Nb de transferts : ", nb))) +
      geom_col(alpha = 0.88) +
      coord_flip() +
      scale_fill_manual(values = POSITION_COLORS,
                        labels = c("Attack" = "Attaque", "Midfield" = "Milieu",
                                   "Defender" = "Defense", "Goalkeeper" = "Gardien"),
                        name = "Famille") +
      scale_y_continuous(labels = label_dollar(prefix = "", suffix = "M€"),
                         expand = expansion(mult = c(0, 0.15))) +
      labs(x = NULL, y = "Frais median (M€)") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.15))
  })
  
  # ---- VIZ 7 : ventes internes vs externes ----
  output$plot_v7 <- renderPlotly({
    
    df_all <- transfers_full %>%
      filter(transfer_fee > 0, !is.na(from_league),
             from_league %in% input$v7_ligues) %>%
      filtre_saison(input$v7_years, input$v7_saison, input$v7_mercato) %>%
      mutate(type_vente = factor(
        if_else(
          !is.na(to_league) & from_league == to_league,
          "Ventes internes au championnat",
          "Ventes hors championnat"   # inclut ventes vers hors Big5
        ),
        levels = c("Ventes hors championnat", "Ventes internes au championnat")
      )) %>%
      group_by(year, from_league, type_vente) %>%
      summarise(total_m = sum(transfer_fee_m, na.rm = TRUE), .groups = "drop")
    
    validate(need(nrow(df_all) > 0, "Aucune donnee pour ces filtres."))
    
    df_all <- df_all %>%
      mutate(
        fill_color = case_when(
          from_league == "Premier League" & type_vente == "Ventes hors championnat"        ~ "#3d85c8",
          from_league == "Premier League" & type_vente == "Ventes internes au championnat" ~ "#a8c8e8",
          from_league == "La Liga"        & type_vente == "Ventes hors championnat"        ~ "#e06c44",
          from_league == "La Liga"        & type_vente == "Ventes internes au championnat" ~ "#f0b49a",
          from_league == "Ligue 1"        & type_vente == "Ventes hors championnat"        ~ "#8e44ad",
          from_league == "Ligue 1"        & type_vente == "Ventes internes au championnat" ~ "#c9a0dc",
          from_league == "Serie A"        & type_vente == "Ventes hors championnat"        ~ "#27ae60",
          from_league == "Serie A"        & type_vente == "Ventes internes au championnat" ~ "#a0d8b0",
          from_league == "Bundesliga"     & type_vente == "Ventes hors championnat"        ~ "#e74c3c",
          from_league == "Bundesliga"     & type_vente == "Ventes internes au championnat" ~ "#f0a09a",
          TRUE ~ "#3d85c8"  # ne devrait jamais arriver
        )
      )
    
    # Palette nommée pour scale_fill_identity
    p <- ggplot(df_all,
                aes(x = year, y = total_m, fill = fill_color,
                    text = paste0("<b>", from_league, "</b><br>",
                                  type_vente, " — ", year, "<br>",
                                  round(total_m), "M€"))) +
      geom_col(position = "stack", width = 0.8) +
      scale_fill_identity(
        guide  = "legend",
        labels = c(
          "#3d85c8" = "PL — Ext.", "#a8c8e8" = "PL — Int.",
          "#e06c44" = "Liga — Ext.", "#f0b49a" = "Liga — Int.",
          "#8e44ad" = "L1 — Ext.", "#c9a0dc" = "L1 — Int.",
          "#27ae60" = "SA — Ext.", "#a0d8b0" = "SA — Int.",
          "#e74c3c" = "BL — Ext.", "#f0a09a" = "BL — Int."
        )
      ) +
      scale_x_continuous(breaks = seq(2002, 2025, by = 3)) +
      scale_y_continuous(labels = label_dollar(prefix = "", suffix = "M€")) +
      facet_wrap(~ from_league, nrow = 2, scales = "fixed") +
      labs(x = NULL, y = "Recettes (M€)", fill = NULL,
           caption = "Couleur foncee = ventes hors championnat  |  Couleur claire = ventes internes") +
      theme_minimal(base_size = 11) +
      theme(strip.text         = element_text(face = "bold"),
            legend.position    = "none",
            panel.grid.minor   = element_blank(),
            panel.grid.major.x = element_blank(),
            plot.caption       = element_text(color = "grey50", size = 9))
    
    ggplotly(p, tooltip = "text") %>%
      layout(showlegend = FALSE)
  })
  
}

# ============================================================
# 4. LANCEMENT
# ============================================================
shinyApp(ui = ui, server = server)
