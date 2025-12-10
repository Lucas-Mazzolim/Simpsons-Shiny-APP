library(shiny)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(plotly)
 
getwd()
setwd('C:/Users/Eri/Documents/Simpsons-Shiny-APP/')

dados <- read_csv("./Data_01/tabela_mestre_final.csv")

# dropdown com top 50
top_personagens_lista <- dados |>
  filter(!is.na(raw_character_text)) |> #null's
  count(raw_character_text, sort = TRUE) |>
  slice_head(n = 50) |>
  pull(raw_character_text)

# Ajuste prévio: Garantir que temporada seja numérico
dados <- dados |> mutate(season = as.numeric(season))

# Definir limites para o Slider (Min e Max temporadas)
min_season <- min(dados$season, na.rm = TRUE)
max_season <- max(dados$season, na.rm = TRUE)

# Carregando lista de "stopwords" (palavras como "the", "and", "is", etc)
data("stop_words")

ui <- fluidPage(
  
  titlePanel("Dashboard Interativo: Os Simpsons"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filtros Globais"),
      
      # 1. Slider de Temporadas
      sliderInput("range_temporadas",
                  "Selecione o Intervalo de Temporadas:",
                  min = min_season,
                  max = max_season,
                  value = c(min_season, max_season), # Começa selecionando tudo
                  step = 1,
                  sep = ""), # Remove a vírgula de milhar se houver
      
      hr(),
      
      # dropdown
      selectInput("personagem_selecionado",
                  "Escolha um Personagem (Destaque):",
                  choices = top_personagens_lista,
                  selected = "Homer Simpson"),
      
      helpText("O Slider afeta todos os gráficos. O Personagem afeta a Nuvem de palavras.")
    ),
    
    mainPanel(
      # Usando Abas para organizar melhor os gráficos
      tabsetPanel(
        tabPanel("Ranking de Falas", plotlyOutput("plot_barras", height = "500px")),
        tabPanel("Nuvem de Palavras", plotOutput("plot_nuvem", height = "600px"))
        
      )
    )
  )
)


# SERVER

server <- function(input, output) {
  
  #DADOS REATIVOS (filtros)
  
  # Dados filtrados APENAS pelo Slider de Temporada
  dados_por_temporada <- reactive({
    dados |>
      filter(season >= input$range_temporadas[1],
             season <= input$range_temporadas[2])
  })
  
  # GRÁFICO 2 - (Bar Chart)
  output$plot_barras <- renderPlotly({
    
    df_ranking <- dados_por_temporada() |>
      filter(!is.na(raw_character_text)) |>
      count(raw_character_text, name = "total_falas") |>
      arrange(desc(total_falas)) |>
      slice_head(n = 15)
    
    p1 <- ggplot(df_ranking, aes(x = reorder(raw_character_text, total_falas), 
                                 y = total_falas,
                                 text = paste("Personagem:", raw_character_text, 
                                              "<br>Falas:", total_falas))) +
      
      geom_col(fill = "#FED90F", color = "black") + 
      coord_flip() +
      labs(title = paste("Top 15 Personagens (Temp.", input$range_temporadas[1], 
                         "até", input$range_temporadas[2], ")"),
           x = "", y = "Total de Linhas de Diálogo") +
      theme_minimal() +
      theme(panel.grid.major.y = element_blank())
    
    ggplotly(p1, tooltip = "text")
  })
  
  # -GRÁFICO 3 - (Word Cloud)
  output$plot_nuvem <- renderPlot({
    
    req(input$personagem_selecionado)
    
    dados_filtrados <- dados_por_temporada() |>
      filter(raw_character_text == input$personagem_selecionado)
    
    if(nrow(dados_filtrados) == 0) return(NULL)
    
    df_palavras <- dados_filtrados |>
      unnest_tokens(word, normalized_text) |>
      anti_join(stop_words, by = "word") |>
      count(word, sort = TRUE) |>
      slice_head(n = 80)
    
    wordcloud(words = df_palavras$word, 
              freq = df_palavras$n, 
              min.freq = 1,
              max.words = 80, 
              random.order = FALSE, 
              rot.per = 0.35, 
              colors = brewer.pal(8, "Dark2"),
              
              scale = c(8, 1.5), 

              family = "sans", 
              font = 2) # font=2, negrito
  })
}
shinyApp(ui, server)
