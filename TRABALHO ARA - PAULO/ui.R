# install.packages(c("shiny", "tidyverse", "tm", "wordcloud", "readr", "RColorBrewer","plotly"))

library(shiny)
library(tidyverse)
library(tm)
library(wordcloud)
library(readr)
library(scales)
library(RColorBrewer)
library(magrittr)
library(data.table)
library(plotly)

# # Carregar conjuntos de dados
dados_personagens <- read_csv('https://raw.githubusercontent.com/paulogermano2025/CE302/refs/heads/main/simpsons_characters.csv')
dados_episodios <- read_csv('https://raw.githubusercontent.com/paulogermano2025/CE302/refs/heads/main/simpsons_episodes.csv')
dados_locais <- read_csv('https://raw.githubusercontent.com/paulogermano2025/CE302/refs/heads/main/simpsons_locations.csv')
dados_dialogos <- read_csv('https://raw.githubusercontent.com/paulogermano2025/CE302/refs/heads/main/simpsons_script_lines.csv', col_select = c(id,episode_id,number,timestamp_in_ms,speaking_line,character_id,location_id,raw_character_text,raw_location_text,normalized_text,word_count))
#
# Carregar palavras excluídas
outwords <- read_lines('https://raw.githubusercontent.com/paulogermano2025/CE302/refs/heads/main/words.txt')

# Excluir linhas sem diálogos e com erros na contagem de palavras
dados_dialogos <- dados_dialogos %>%
  filter(speaking_line == TRUE) %>%
  mutate(word_count = ifelse(word_count > 200,
                             trunc(word_count/1000),
                             word_count)) %>%
  filter(word_count < 130)

# Unir tabelas em conjunto completo
dados_dialogos_episodios <- dados_dialogos %>%

  left_join(dados_episodios, by = c("episode_id" = "id")) %>%
  left_join(dados_personagens, by = c('character_id' = 'id')) %>%
  select(-c('name','normalized_name')) %>%
  mutate(
    nome_personagem = str_to_title(trimws(raw_character_text)),
    ano_exibicao = as.numeric(original_air_year),
    locais = str_to_title(trimws(raw_location_text)),
    gender = ifelse(gender == "f", "Female","Male")
  ) %>%
  filter(speaking_line == TRUE, word_count > 0, !is.na(normalized_text))

lista_personagens <- dados_dialogos_episodios%>%
  group_by(nome_personagem) %>%
  summarise(total_falas = n()) %>%
  filter(total_falas > 50, !nome_personagem %in% c("Unknown", "Narrator", "Man", "Woman", "Kids", "Children","Producer","Salesman","Executive","Female Executive","Announcer")) %>%
  pull(nome_personagem)


lista_anos <- sort(unique(dados_dialogos_episodios$ano_exibicao[!is.na(dados_dialogos_episodios$ano_exibicao)]))
lista_locais <- dados_dialogos_episodios %>%
  group_by(locais) %>%
  summarise(n = n()) %>%
  filter(n > 80) %>% # Filtra locais com poucas falas
  pull(locais)
lista_locais <- sort(lista_locais)
# # 
# # 

ui <- fluidPage(title = "Os Simpsons", style = "background-color: #FFFBAD;",
                
  # Título do aplicativo
  titlePanel(
    tags$div(
      tags$h1("Como se Fala em Springfield!", style = "font-family: verdana; color: #FFF200; font-weight: bold; border-width: 3px, border-coloer: #3C3E3E; background-color: #37A0FB; padding-left: 4px; padding-top: 15px; padding-bottom: 15px"),
      tags$h3("Uma análise dos diálogos em Os Simpsons", style = "font-family: verdana; color: #3C3E3E; padding-bottom: 15px")
    )
  ),

  # Layout com barra lateral e painel principal
  sidebarLayout(
    # --- BARRA LATERAL (FILTROS) ---
    sidebarPanel(style = "height: 1000px; background-color: #D9596C;",
      width = 2, # Largura da barra lateral
      tags$h3("Filtros de Dados", style = "color: #FFF200;"),
      
      # Filtro de Personagem (Dropdown/Select)
      selectizeInput(
        inputId = "filtro_personagem",
        label = tags$strong("Escolha um Personagem:", style = "color: #FFF200; padding-top: 30px;"),
        choices = c("All" = "all", lista_personagens),
        selected = "all",
        options = list(maxOptions = 5000)
      ),
      
      # Filtro Genero
      radioButtons(
        inputId = "filtro_genero",
        label = tags$strong("Gênero do Personagem", style = "color: #FFF200; padding-top: 30px;"),
        choices = c("All" = "all", "Female","Male"),
        selected = "all"
      ),
      
      # Filtro de Ano (Slider de Intervalo)
      sliderInput(
        inputId = "ano_selecionado",
        label = tags$strong("Período", style = "color: #FFF200; padding-top: 30px;"),
        min = min(lista_anos),
        max = max(lista_anos),
        value = c(min(lista_anos), max(lista_anos)),
        sep = "" # Remove o separador de milhares
      ),
      
      # Filtro de Local (Dropdown/Select)
      selectizeInput(
        inputId = "local_selecionado",
        label = tags$strong("Selecione o Local:", style = "color: #FFF200; padding-top: 80px;"),
        choices = c("All" = "all", lista_locais),
        selected = "all"
      ),
      
      
      
      # Número máximo de palavras na nuvem
      # sliderInput(
      #   inputId = "max_palavras",
      #   label = tags$strong("Máximo de Palavras na Nuvem:"),
      #   min = 10, max = 200, value = 100, step = 10
      # ),
      # 
      # # Frequência mínima
      # sliderInput(
      #   inputId = "min_freq",
      #   label = tags$strong("Frequência Mínima (para Aparecer):"),
      #   min = 1, max = 50, value = 5, step = 1
      # )
    ),
    
    # --- PAINEL PRINCIPAL (OUTPUT) ---
    mainPanel(width = 10, style = "height: 1000px;background-color: #37A0FB;",
      # Título Dinâmico

      fluidRow(
        column(4,
               # Nuvem de Palavras
               tags$h4(style = "margin-top: 25px; color: #333; background-color: #D7ECFE"),
               plotOutput("nuvem_palavras", height = "400px")
               ),
        column(4,
               # Gráfico de Barras
               tags$h4(style = "margin-top: 25px; color = #333;"),
               plotOutput("grafico_media_palavras", height = "400px")
               ),
        column(4,
               # Gráfico de Áreas
               tags$h4(style = "margin-top: 25px; color: #333;"),
               plotOutput("linear_area_chart", height = "400px")
               )
        ),
      fluidRow(
        column(6,
               # Heatmap
               tags$h4(style = "margin-top: 30px; color: #333;"),
               plotOutput("heatmap_imdb", height = "400px")
               ),
        column(6,
               # Dispersão Palavras x Rating
               tags$h4(style = "margin-top: 30px; color:#333;"),
               plotOutput("scatter_imdb", height = "400px")
               )
      )
    )
  )
)