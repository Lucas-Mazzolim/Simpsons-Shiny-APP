server <- function(input, output) {
  
  #DADOS REATIVOS
  dados_filtrados <- reactive({
    req(input$range_temporadas)
    dados |> filter(season >= input$range_temporadas[1], season <= input$range_temporadas[2])
  })
  
  dados_eps_unicos <- reactive({
    dados_filtrados() |> distinct(episode_id, .keep_all = TRUE) |>
      filter(!is.na(us_viewers_in_millions), !is.na(imdb_rating))
  })
  
  
  # ABA 1: PERSONAGENS
  output$plot_barras <- renderPlotly({
    df <- dados_filtrados() |> count(raw_character_text, name="n") |> arrange(desc(n)) |> slice_head(n=15)
    max_val <- max(df$n) #escalas intervalar 500
    limite_eixo <- ceiling(max_val / 500) * 500  #escalas intervalar 500
    p <- ggplot(df, aes(x = n, 
                        y = reorder(raw_character_text, n),
                        # TOOLTIP CUSTOMIZADO
                        text = paste("Personagem:", raw_character_text, "<br>Falas:", n))) + 
      geom_col(fill="#FED90F", color="black") +
      scale_x_continuous(breaks = seq(0, limite_eixo, by = 500), 
                         expand = expansion(mult = c(0, 0.05))
      ) +
      labs(x="Linhas de Fala", y="") + 
      theme_minimal() + 
      theme(
        panel.grid.major.y = element_blank(), # Remove linhas horizontais
        panel.grid.minor.x = element_blank(), # Remove grades menores
        axis.text = element_text(size=10, color="black")
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$plot_nuvem <- renderPlot({
    req(input$personagem_selecionado)
    df <- dados_filtrados() |> filter(raw_character_text == input$personagem_selecionado) |>
      unnest_tokens(word, normalized_text) |> anti_join(stop_words, by="word") |> count(word, sort=TRUE) |> slice_head(n=60)
    if(nrow(df)==0) return(NULL)
    wordcloud(df$word, df$n, max.words=60, colors=brewer.pal(8,"Dark2"), scale=c(4,0.5), family="sans", font=2)
  })
  
  output$tabela_personagens <- renderDT({
    dados_filtrados() |> count(Personagem = raw_character_text, name = "Falas") |> arrange(desc(Falas)) |>
      datatable(options = list(pageLength = 5, scrollY = "300px"), rownames=FALSE)
  })
  
  # ABA 2: LOCAIS
  output$plot_locais <- renderPlotly({
    df <- dados_filtrados() |> count(raw_location_text, name="n") |> 
      arrange(desc(n)) |> slice_head(n=15)
    max_val <- max(df$n)
    limite_eixo <- ceiling(max_val / 500) * 500

    p <- ggplot(df, aes(x = n, 
                        y = reorder(raw_location_text, n),
                        text = paste("Local:", raw_location_text, "<br>Cenas:", n))) + 
      
      geom_col(fill="#009DDC", color="black", width = 0.75) + 
      
      # Ajuste da escala de 500 em 500
      scale_x_continuous(
        breaks = seq(0, limite_eixo, by = 500),
        expand = expansion(mult = c(0, 0.05)) # Tira o espaço vazio no começo
      ) +
      
      labs(x="Aparições em Cena", y="") + 
      theme_minimal() +
      theme(
        panel.grid.major.y = element_blank(), # Limpa linhas horizontais
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(size=10, color="black")
      )
    ggplotly(p, tooltip = "text")
  })
  
  output$tabela_locais <- renderDT({
    dados_filtrados() |> count(Local = raw_location_text, name = "Cenas") |> arrange(desc(Cenas)) |>
      datatable(options = list(pageLength = 5), rownames=FALSE)
  })
  
  #ABA 3: DEMOGRAFIA
  output$plot_genero <- renderPlotly({
    df <- dados_filtrados() |> filter(gender != "Desconhecido") |> count(season, gender)
    p <- ggplot(df, aes(x=season, y=n, fill=gender)) + 
      geom_bar(stat="identity", position="dodge", color="black") +
      scale_fill_manual(values = c("Feminino"="#FF69B4", "Masculino"="#4682B4")) +
      labs(x="Temporada", y="Total de Falas", fill="Gênero") + theme_minimal()
    ggplotly(p)
  })
  
  output$tabela_genero <- renderDT({
    dados_filtrados() |> filter(gender != "Desconhecido") |> group_by(Temporada=season, Genero=gender) |>
      summarise(Falas = n(), .groups="drop") |> pivot_wider(names_from = Genero, values_from = Falas) |>
      datatable(options = list(pageLength = 5), rownames=FALSE)
  })
  
  #ABA 4: AUDIÊNCIA
  output$plot_audiencia_season <- renderPlotly({
    df <- dados_eps_unicos() |> group_by(season) |> summarise(m = mean(us_viewers_in_millions))
    p <- ggplot(df, aes(season, m)) + geom_line(color="#009DDC", size=1) + 
      geom_point(fill="#FED90F", color="black", shape=21, size=3) + theme_minimal() + labs(y="Média (Mi)", x="Temp.")
    ggplotly(p)
  })
  
  output$plot_audiencia_episodes <- renderPlotly({
    p <- ggplot(dados_eps_unicos(), aes(original_air_date, us_viewers_in_millions)) +
      geom_point(aes(text=paste("Ep:", title)), fill="#FED90F", color="black", shape=21, alpha=0.6) + 
      geom_smooth(color="#CB181D", se=FALSE) + theme_minimal() + labs(y="Audiência", x="Data")
    ggplotly(p, tooltip="text")
  })
  
  output$tabela_audiencia <- renderDT({
    dados_eps_unicos() |> select(Temp=season, Titulo=title, Data=original_air_date, Views=us_viewers_in_millions) |>
      datatable(options = list(pageLength = 5), rownames=FALSE)
  })
  
  #ABA 5: VIEW X NOTA
  
  output$plot_scatter_corr <- renderPlotly({
    p <- dados_eps_unicos() %>% 
      ggplot(aes(x = us_viewers_in_millions, y = imdb_rating)) +
      geom_point(aes(color = season, text = paste("Título:", title)), alpha = 0.7, size = 2) +
      scale_color_viridis_c(option = "plasma", name = "Temporada", direction = -1) +
      scale_x_continuous(labels = scales::label_number(suffix = " mi")) +
      labs(
        title = "Evolução de 'The Simpsons': Audiência vs. Qualidade",
        x = "Audiência nos EUA (Milhões)",
        y = "Nota IMDb"
      ) +

      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", color = "#2c3e50"),
        plot.subtitle = element_text(size = 12, color = "#7f8c8d", margin = margin(b = 10)),
        legend.position = "right",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey90"),
        axis.text = element_text(size = 11, color = "black"),
        axis.title = element_text(face = "bold")
      )

    if (!is.null(input$tipo_tendencia)) {
      
      if (input$tipo_tendencia == "lm") {
        p <- p + geom_smooth(method = "lm", se = FALSE, color = "#CB181D", linewidth = 1)
      }
      
      if (input$tipo_tendencia == "loess") {
        p <- p + geom_smooth(method = "loess", se = FALSE, color = "#009DDC", linewidth = 1)
      }
    }
    ggplotly(p, tooltip = "text")
  })
  
  output$tabela_polemicos <- renderDT({
    # Episódios com alta view e nota baixa (Ex: Quartil 4 de view e Quartil 1 de nota)
    dados_eps_unicos() |> 
      filter(us_viewers_in_millions > quantile(us_viewers_in_millions, 0.75), 
             imdb_rating < quantile(imdb_rating, 0.25)) |>
      select(Titulo=title, Temp=season, Views=us_viewers_in_millions, Nota=imdb_rating) |>
      datatable(caption = "Episódios com Alta Audiência mas Baixa Nota", options=list(pageLength=5), rownames=FALSE)
  })
  
  #6: STATS TEMPORADA
  output$plot_boxplot <- renderPlotly({
    p <- ggplot(dados_eps_unicos(), aes(factor(season), imdb_rating)) + 
      geom_jitter(width = 0.2, color = "#333333", size = 1, alpha = 0.4) +
      geom_boxplot(fill = "#FED90F", color = "black", alpha = 0.6, outlier.shape = NA) + 
      
      theme_minimal() + 
      labs(x = "Temporada", y = "Nota")
    
    ggplotly(p) %>% layout(boxmode = "group")
  })
  
  # ABA 6: GRÁFICO DE CORRELAÇÃO
  output$plot_corr_line <- renderPlotly({
    
    corr_by_season_votes <- dados_eps_unicos() %>%
      group_by(season) %>%
      summarise(
        n = n(),
        corr = cor(us_viewers_in_millions, imdb_votes, use = "complete.obs", method = "pearson")
      ) %>%
      ungroup() %>%
      filter(!is.na(corr)) # Remove temporadas onde não foi possível calcular
    
    p <- ggplot(corr_by_season_votes, aes(x = season, y = corr)) +
      
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
      geom_segment(aes(xend = season, yend = 0, color = corr), linewidth = 1) +
      geom_point(aes(color = corr, 
                     text = paste("Temporada:", season, 
                                  "<br>Correlação (r):", round(corr, 3))), 
                 size = 4) +
      geom_text(aes(label = round(corr, 2)), 
                nudge_y = 0.05, # Empurra o texto 0.05 unidades para cima
                size = 3, 
                color = "#333333") +
      
      # Escalas e Eixos
      scale_x_continuous(breaks = seq(min(corr_by_season_votes$season), 
                                      max(corr_by_season_votes$season), 
                                      by = 1)) +
      
      scale_color_gradient(low = "#e0aeb6", high = "#8b0000", name = "Força") +
      scale_y_continuous(limits = c(min(0, min(corr_by_season_votes$corr) - 0.1), 
                                    max(corr_by_season_votes$corr) + 0.25)) +
      
      labs(x = "Temporada", y = "Correlação de Pearson (r)") +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 9, angle = 0),
        legend.position = "none"
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$tabela_resumo_season <- renderDT({
    dados_eps_unicos() |> group_by(Temporada=season) |> 
      summarise(Qtd=n(), Media_Nota=round(mean(imdb_rating),2), Media_View=round(mean(us_viewers_in_millions),2)) |>
      datatable(options=list(pageLength=5), rownames=FALSE)
  })
}
 
shinyApp(ui, server)
