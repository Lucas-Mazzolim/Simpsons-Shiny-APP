# Carrega todas as bibliotecas necessárias para o app inteiro
library(shiny)
library(shinydashboard)
library(tidyverse)
library(magrittr)

# Carrega todos os módulos de UI da pasta R/
lapply(list.files("R", full.names = TRUE, recursive = TRUE), source, encoding = "UTF-8")

# Carregamento de Dados Brutos