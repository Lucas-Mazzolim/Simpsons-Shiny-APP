# Ficheiro: ui.R
library(shiny)
library(bslib)
library(shinydashboard)

meu_tema <- bs_theme(
  yellow = "#FED90F",
)

ui <- fluidPage(
    dashboardPage(
      tagAppendAttributes(
        dashboardHeader(title = "The Simpsons",
                        tags$li(class = "dropdown",
                                tags$a(href = "https://github.com/Lucas-Mazzolim/Simpsons-Shiny-APP.git", 
                                       target = "_blank", # Abre em nova aba
                                       icon("github", class = "fa-lg"),
                                       "Repositório", # Texto opcional ao lado do ícone
                                       style = "font-size: 20px ; padding-top: 15px; padding-bottom: 15px; color: #333; font-weight: bold;"
                                )
                        )
                        ),
        class = 'header'
      ),
      dashboardSidebar(
        sidebarMenu(
          menuItem(text = "Análise 1", tabName = "analise_1", icon = icon('database')),
          menuItem(text = "Análise 2", tabName = "analise_2", icon = icon('chart-line')),
          menuItem(text = "Análise 3", tabName = "analise_3", icon = icon('chart-column')),
          menuItem(text = "Análise 4", tabName = "analise_4", icon = icon('chart-bar')),
          menuItem(text = "Análise 5", tabName = "analise_5", icon = icon('chart-area')),
          menuItem(text = "Análise 6", tabName = "analise_6", icon = icon('chart-pie')),
          menuItem(text = "Sobre", tabName = "integrantes", icon = icon('circle-question'))
        )
      ),
      dashboardBody(
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
        tabItems(
          tabItem(tabName = "analise_1",
                  h2("Análise 1")
          ),
          tabItem(tabName = "analise_2",
                  h2("Análise 2")
          ),
          tabItem(tabName = "analise_3",
                  h2("Análise 3")
          ),
          tabItem(tabName = "analise_4",
                  h2("Análise 4")
          ),
          tabItem(tabName = "analise_5",
                  h2("Análise 5")
          ),
          tabItem(tabName = "analise_6",
                  h2("Análise 6")
          ),
          
          tabItem(tabName = "integrantes",
                  h2("Sobre", class = 'title_sobre'),
                  p('O presente projeto consiste no desenvolvimento de uma aplicação web interativa voltada para a análise exploratória de 
                    dados da série televisiva Os Simpsons. Desenvolvido colaborativamente por uma equipe de quatro integrantes, o trabalho 
                    utiliza integralmente o ecossistema da linguagem R, fundamentando-se no uso do pacote Shiny para a construção da interface 
                    gráfica e do framework Tidyverse para a manipulação e limpeza da base de dados. A aplicação está estruturada em seis módulos 
                    analíticos distintos, permitindo ao usuário investigar dinamicamente tendências de audiência, a evolução das avaliações da crítica, 
                    padrões de diálogo e a densidade de participação dos personagens ao longo das temporadas. Visando assegurar o rigor metodológico e a
                    transparência científica, o projeto integra a ferramenta Quarto para a geração automatizada de relatórios estáticos, garantindo a plena
                    reprodutibilidade das rotinas de código e das visualizações geradas. O resultado é uma ferramenta que une a interatividade do dashboard 
                    à formalidade da documentação técnica, oferecendo uma perspectiva quantitativa robusta sobre a trajetória da série.', class = 'texto_sobre')
          )
        )
      )
    )
  )
