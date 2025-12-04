library(shiny)
library(ggplot2)
library(tidyverse)
library(bslib)
library(shinydashboard)
library(shinyWidgets)


ui <- fluidPage(
  tags$head(includeCSS('www/custom.css')),
  tags$head(tags$link(rel = "shortcut icon", href = "www/favicon.ico")),
  
  # Injeção de JS para evitar digitação no input de datas
  tags$script(HTML("
      $(document).ready(function() {
        // Encontra todos os inputs de data e adiciona o atributo 'readonly'
        // Isso impede a digitação, mas mantém o calendário clicável.
        $('#data_inicial_filter_KPIs, #data_final_filter_KPIs').find('input').attr('readonly', 'readonly');
      });
    ")),
  dashboardPage(
    tagAppendAttributes(
      dashboardHeader(title = "The Simpsons",
                      tags$li(class = "dropdown",
                              tags$a(href = "https://github.com/Lucas-Mazzolim/Simpsons-Shiny-APP", 
                                     target = "_blank", # Abre em nova aba
                                     icon("github", class = "fa-lg"),
                                     "Repositório", # Texto opcional ao lado do ícone
                                     style = "font-size: 20px ; padding-top: 15px; padding-bottom: 15px; color: white; font-weight: bold; text-shadow: 1px 2px 10px rgba(000, 000, 000, 0.75)"
                              )
                      )
      ),
      class = 'header'
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem(text = "Panorama Geral", tabName = "panorama", icon = icon('chart-area')),
        menuItem(text = "Principais Correlações", tabName = "correlacoes", icon = icon('arrows-alt-h')),
        menuItem(text = "Análise dos Episódios", tabName = "episodios", icon = icon('magnifying-glass')),
        menuItem(text = "Aspecto Textual", tabName = "texto", icon = icon('file-lines')),
        menuItem(text = "Evolução Temporal", tabName = "tempo", icon = icon('clock')),
        menuItem(text = "Comparações Categóricas", tabName = "categorias", icon = icon('layer-group')),
        menuItem(text = "Sobre", tabName = "sobre", icon = icon('circle-question'))
      )
    ),
    dashboardBody(
       tabItems(
         tabItem(
           tabName = 'correlacoes',
           h1('Análise vai aqui')
         ),
         tabItem(
           tabName = 'episodios',
           h1('Análise vai aqui')
         ),
         tabItem(
           tabName = 'texto',
           h1('Análise vai aqui')
         ),
         tabItem(
           tabName = 'tempo',
           h1('Análise vai aqui')
         ),
         tabItem(
           tabName = 'categorias',
           h1('Análise vai aqui')
         ),
         tabItem(
           tabName = 'panorama',
           h1('Análise vai aqui')
         ),
         tabItem(
           tabName = 'sobre',
           Sobre('Sobre')
         )
       )
      )
    )
  )


