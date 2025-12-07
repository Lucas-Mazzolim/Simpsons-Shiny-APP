library(shiny)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(plotly)
library(DT)

#DADOS

file_path <- "./Data_01/tabela_mestre_final.csv"

if (!file.exists(file_path)) {
  stop("ERRO: Arquivo não encontrado. Verifique a pasta Data_01.")
}

dados <- read_csv(file_path)
if (ncol(dados) <= 1) dados <- read_csv2(file_path)

# Tratamento Geral
dados <- dados  |> 
  mutate(
    season = as.numeric(season),
    original_air_date = as.Date(original_air_date),
    us_viewers_in_millions = as.numeric(us_viewers_in_millions),
    imdb_rating = as.numeric(imdb_rating),
    imdb_votes = as.numeric(imdb_votes),
    # rótulos de gênero
    gender = case_when(
      gender %in% c("m", "M") ~ "Masculino",
      gender %in% c("f", "F") ~ "Feminino",
      TRUE ~ "Desconhecido"
    )
  ) |>
  filter(!is.na(season))

# sliders
min_season <- min(dados$season, na.rm = TRUE)
max_season <- max(dados$season, na.rm = TRUE)
if (!is.finite(min_season)) min_season <- 1
if (!is.finite(max_season)) max_season <- 30

# lista de personagens
top_personagens_lista <- dados |>
  filter(!is.na(raw_character_text)) |>
  count(raw_character_text, sort = TRUE) |>
  slice_head(n = 50) |>
  pull(raw_character_text)

data("stop_words")

ui <- fluidPage(

  
  # CSS para dar estilo ao app
  tags$head(tags$style(HTML("
    .shiny-output-error { visibility: hidden; }
    h4 { color: #009DDC; font-weight: bold; border-bottom: 2px solid #FED90F; padding-bottom: 5px; }
    .nav-tabs>li.active>a, .nav-tabs>li.active>a:focus, .nav-tabs>li.active>a:hover {
      background-color: #FED90F; font-weight: bold;
    }
  "))),
  
  titlePanel(
    span("Os Simpsons Analytics", 
         style = "color: #337AB7; font-weight: bold; font-family: sans-serif;") 

  ),
  
  sidebarLayout(
    sidebarPanel(
      h3("Painel de Controle"),
      p("Utilize os filtros abaixo para interagir com todas as abas."),
      hr(),
      sliderInput("range_temporadas", "Intervalo de Temporadas:",
                  min = min_season, max = max_season,
                  value = c(min_season, max_season), step = 1, sep = ""),
      hr(),
      selectInput("personagem_selecionado", "Personagem Destaque (Aba 1):",
                  choices = top_personagens_lista, selected = "Homer Simpson"),
      hr(),
      radioButtons("tipo_tendencia", "Tendência (Aba 3):",
                   choices = c("Sem Linha"="none", "Linear"="lm", "Suave"="loess"),
                   selected = "loess"),
      br(),
      img(src = "https://upload.wikimedia.org/wikipedia/commons/9/98/The_Simpsons_yellow_logo.svg", width = "80%")
    ),
    
    mainPanel(
      tabsetPanel(
        
        #ABA 1: PERSONAGENS
        tabPanel("1. Personagens",
                 br(),
                 h4("Quem fala mais?"),
                 plotlyOutput("plot_barras", height = "350px"),
                 br(),
                 fluidRow(
                   column(6, 
                          h4("Vocabulário do Personagem"), 
                          plotOutput("plot_nuvem", height = "400px")),
                   column(6, 
                          h4("Tabela de Falas"),
                          p("Top personagens no período selecionado."),
                          DTOutput("tabela_personagens"))
                 )
        ),
        
        # ABA 2: LOCAIS
        tabPanel("2. Locais (Geo)",
                 br(),
                 h4("Onde as coisas acontecem?"),
                 p("Os locais mais frequentes nas cenas dos Simpsons."),
                 plotlyOutput("plot_locais", height = "450px"),
                 hr(),
                 h4("Dados dos Locais"),
                 DTOutput("tabela_locais")
        ),
        
        #ABA 3: DEMOGRAFIA (gênero)
        tabPanel("3. Demografia",
                 br(),
                 h4("Distribuição de Gênero por Temporada"),
                 p("Comparativo do volume de falas entre personagens masculinos e femininos."),
                 plotlyOutput("plot_genero", height = "450px"),
                 hr(),
                 h4("Resumo Demográfico"),
                 DTOutput("tabela_genero")
        ),
        
        #ABA 4: AUDIÊNCIA (Tendências)
        tabPanel("4. Audiência",
                 br(),
                 h4("Evolução da Audiência (Milhões)"),
                 plotlyOutput("plot_audiencia_season", height = "300px"),
                 br(),
                 h4("Detalhe por Episódio"),
                 plotlyOutput("plot_audiencia_episodes", height = "350px"),
                 hr(),
                 h4("Dados Brutos"),
                 DTOutput("tabela_audiencia")
        ),
        
        # --- ABA 5: RELAÇÃO VIEW x NOTA (Correlações)
        tabPanel("5. View vs Nota",
                 br(),
                 h4("A Audiência dita a Qualidade?"),
                 plotlyOutput("plot_scatter_corr", height = "450px"),
                 hr(),
                 fluidRow(
                   column(6, 
                          h4("Heatmap de Categorias"),
                          plotOutput("plot_heatmap", height = "400px")),
                   column(6, 
                          h4("Episódios 'Polêmicos'"),
                          p("Episódios com alta audiência mas nota baixa (ou vice-versa)."),
                          DTOutput("tabela_polemicos"))
                 )
        ),
        
        #ABA 6: STATS TEMPORADA (Comparativo)
        tabPanel("6. Stats da Temporada",
                 br(),
                 fluidRow(
                   column(6, 
                          h4("Distribuição de Notas (Boxplot)"),
                          plotlyOutput("plot_boxplot", height = "400px")),
                   column(6, 
                          h4("Evolução da Correlação"),
                          plotlyOutput("plot_corr_line", height = "400px"))
                 ),
                 hr(),
                 h4("Resumo Estatístico Consolidado"),
                 DTOutput("tabela_resumo_season")
        )
      )
    )
  )
)