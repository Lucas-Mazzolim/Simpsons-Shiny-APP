# Ficheiro: ui.R
library(shiny)
library(bslib)
library(shinydashboard)

meu_tema <- bs_theme(
  yellow = "#FED90F",
)

ui <- fluidPage(
    tags$head(
      tags$style(HTML('
                      .container-fluid {
                        padding: 0px;
                        margin-right: auto;
                        margin-left: auto;
                      }
                      '))
    ),
    dashboardPage(
      dashboardHeader(title = "Simpsons Dashboard"),
      dashboardSidebar(
        sidebarMenu(
          menuItem(text = "Análise 1", tabName = "analise_1"),
          menuItem(text = "Análise 2", tabName = "analise_2"),
          menuItem(text = "Análise 3", tabName = "analise_3"),
          menuItem(text = "Análise 4", tabName = "analise_4"),
          menuItem(text = "Análise 5", tabName = "analise_5"),
          menuItem(text = "Análise 6", tabName = "analise_6"),
          menuItem(text = "Colaboradores", tabName = "integrantes")
        )
      ),
      dashboardBody(
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
                  h2("Integrantes"),
                  color = 'red'
          )
        )
      )
    )
  )
  