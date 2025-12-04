library(shiny)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(RColorBrewer)

dados <- read_csv("./Data_01/tabela_mestre_final.csv")

# dropdown com top 50
top_personagens <- dados |>
  filter(!is.na(raw_character_text)) |> # Remove nulos
  count(raw_character_text, sort = TRUE) |>
  slice_head(n = 50) |>
  pull(raw_character_text)

# Carregando lista de "stopwords" (palavras como "the", "and", "is", etc)
data("stop_words")

# =========================================================
# 2. INTERFACE DO USUÁRIO (UI)
# =========================================================
ui <- fluidPage(
  
  # Título do App
  titlePanel("Análise de Diálogos: Os Simpsons"),
  
  # Layout com barra lateral e painel principal
  sidebarLayout(
    
    sidebarPanel(
      h3("Filtros"),
      
      selectInput(inputId = "personagem_selecionado",
                  label = "Escolha um Personagem:",
                  choices = top_personagens,
                  selected = "Homer Simpson"), #valor default
      
      hr(), # Linha divisória
      helpText("A nuvem de palavras atualizará automaticamente ao mudar o personagem.")
    ),
    
    mainPanel(
      # Onde o gráfico vai aparecer
      h4("Nuvem de Palavras (Word Cloud)"),
      plotOutput(outputId = "plot_nuvem", height = "800px")
    )
  )
)


####### SERVER
server <- function(input, output) {
  
  #roda quando o input$personagem_selecionado muda
  dados_palavras <- reactive({
    
    req(input$personagem_selecionado) # Garante que algo foi selecionado
    
    dados |>
      # 1. Filtra pelo personagem escolhido no dropdown
      filter(raw_character_text == input$personagem_selecionado) |>
      
      # 2. Tokenização: Quebra as frases em palavras
      # A coluna 'normalized_text' é a melhor para isso pois já está limpa
      unnest_tokens(word, normalized_text) |>
      
      # 3. Remove "Stop Words" (artigos, preposições)
      anti_join(stop_words, by = "word") |>
      
      # 4. Conta a frequência das palavras
      count(word, sort = TRUE) |>
      
      # 5. Pega apenas as 100 mais faladas para a nuvem não ficar ilegível
      slice_head(n = 100)
  })
  
  # Renderização do Gráfico
  output$plot_nuvem <- renderPlot({
    
    # Pegamos os dados processados acima
    df_nuvem <- dados_palavras()
    
    # Geramos a nuvem
    wordcloud(words = df_nuvem$word, 
              freq = df_nuvem$n, 
              min.freq = 2,
              max.words = 80, 
              random.order = FALSE, 
              rot.per = 0.35, 
              colors = brewer.pal(8, "Dark2"),
              scale = c(4, 0.5)) # Ajusta o tamanho da maior e menor palavra
  })
}

# Roda o aplicativo
shinyApp(ui = ui, server = server)
