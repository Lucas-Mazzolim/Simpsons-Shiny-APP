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
                         expand = expansion(mult = c(0, 0.05))   # Remove o espaço vazio antes do 0
      ) +
      labs(x="Linhas de Fala", y="") + 
      theme_minimal() + 
      theme(
        panel.grid.major.y = element_blank(), # Remove linhas horizontais (desnecessárias em gráfico de barra)
        panel.grid.minor.x = element_blank(), # Remove grades menores para não poluir
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
    # p <- ggplot(df, aes(reorder(raw_location_text, n), n)) + geom_col(fill="#009DDC", color="black") + # Azul Marge
    #   coord_flip() + labs(x="", y="Aparições em Cena") + theme_minimal()
    # ggplotly(p)
    p <- ggplot(df, aes(x = n, 
                        y = reorder(raw_location_text, n),
                        # AQUI ESTÁ A MÁGICA DO TOOLTIP LIMPO:
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
    
    # 4. Renderiza com o tooltip forçado apenas para o texto que criamos
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
    p <- ggplot(dados_eps_unicos(), aes(us_viewers_in_millions, imdb_rating)) +
      geom_point(aes(text=paste(title)), color="#444", alpha=0.5) + labs(x="Audiência", y="Nota") + theme_minimal()
    if(input$tipo_tendencia=="lm") p <- p + geom_smooth(method="lm", se=F, color="#CB181D")
    if(input$tipo_tendencia=="loess") p <- p + geom_smooth(method="loess", se=F, color="#009DDC")
    ggplotly(p, tooltip="text")
  })
  
  # output$plot_heatmap <- renderPlot({
  #   df <- dados_eps_unicos() |> mutate(
  #     v_grp = cut(us_viewers_in_millions, breaks=quantile(us_viewers_in_millions,probs=0:4/4), include.lowest=T, labels=c("Baixa","Média-","Média+","Alta")),
  #     r_grp = cut(imdb_rating, breaks=quantile(imdb_rating,probs=0:4/4), include.lowest=T, labels=c("Baixa","Média-","Média+","Alta"))
  #   )
  #   if(nrow(df)<5) return(NULL)
  #   ggplot(df %>% count(v_grp, r_grp), aes(v_grp, r_grp, fill=n)) + geom_tile(color="white") + geom_text(aes(label=n), size=5) +
  #     scale_fill_gradient(low="#FFF5F0", high="#CB181D") + labs(x="Audiência", y="Nota") + theme_minimal()
  # })
  
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
      geom_boxplot(fill="#FED90F", outlier.color="red") + theme_minimal() + labs(x="Temporada", y="Nota")
    ggplotly(p) |> layout(boxmode="group")
  })
  
  output$plot_corr_line <- renderPlotly({
    df <- dados_eps_unicos() |> group_by(season) |> summarise(c = cor(us_viewers_in_millions, imdb_rating))
    p <- ggplot(df, aes(season, c)) + geom_line(color="#009DDC") + geom_point(aes(text=round(c,3)), color="black") +
      geom_hline(yintercept=0, linetype="dashed") + theme_minimal() + labs(x="Temporada", y="Correlação")
    ggplotly(p)
  })
  
  output$tabela_resumo_season <- renderDT({
    dados_eps_unicos() |> group_by(Temporada=season) |> 
      summarise(Qtd=n(), Media_Nota=round(mean(imdb_rating),2), Media_View=round(mean(us_viewers_in_millions),2)) |>
      datatable(options=list(pageLength=5), rownames=FALSE)
  })
}
 
shinyApp(ui, server)
