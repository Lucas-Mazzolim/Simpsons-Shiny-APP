server <- function(input, output, session) {
  
########### BARPLOT - MÉDIA DE PALAVRAS #########
  # Filtrar dados e calcular média
  contagem_palavras <- reactive({
    
    # Filtro de ano
    dados_palavras <- dados_dialogos_episodios %>%
      filter(ano_exibicao >= input$ano_selecionado[1] & ano_exibicao <= input$ano_selecionado[2])
    
    # Filtro local
    if (input$local_selecionado !='all') {
      dados_palavras <- dados_palavras %>%
        filter(locais == input$local_selecionado)
    }
    
    # Filtro gênero
    if (input$filtro_genero !='all') {
      dados_palavras <- dados_palavras %>%
        filter(gender == input$filtro_genero)
    }
    

    # Total palavras personagem por episódio
    palavras_episodio_personagem <- dados_palavras %>%
      group_by(episode_id, nome_personagem) %>%
      summarise(
        total_palavras_por_episodio = sum(as.numeric(word_count), na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Média de palavras por episódio por personagem
    media_palavras_personagem <- palavras_episodio_personagem %>%
      group_by(nome_personagem) %>%
      summarise(
        media_palavras_por_episodio = mean(total_palavras_por_episodio, na.rm = TRUE),
        # Contamos o número de episódios em que o personagem aparece (para garantir validade)
        episodes_n = n_distinct(episode_id),
        .groups = 'drop'
      ) %>%
      # Filtramos personagens que apareceram em pelo menos 10 episódios para maior relevância
      filter(episodes_n >= 5) %>%
      # Ordenar e selecionar os 10 primeiros
      arrange(desc(media_palavras_por_episodio)) %>%
      slice_head(n = 10)
    
    # Garantir que o nome do personagem seja um fator ordenado para o gráfico
    media_palavras_personagem$nome_personagem <- factor(
      media_palavras_personagem$nome_personagem, 
      levels = rev(media_palavras_personagem$nome_personagem)
    )
    
    return(media_palavras_personagem)
  })
  
  # Renderizar Gráfico de Barras
  output$grafico_media_palavras <- renderPlot({
    plot_data <- contagem_palavras()
    
    if (nrow(plot_data) == 0) {
      plot(NA, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", bty="n", xaxt="n", yaxt="n")
      text(0.5, 0.5, "Filtro retornou dados insuficientes", cex = 1.2)
      return(NULL)
    }
    
        # Criação do gráfico de barras horizontais usando ggplot2
    ggplot(plot_data, aes(x = media_palavras_por_episodio, y = nome_personagem)) +
      geom_bar(stat = "identity", color = "#B8283E", fill = "#E58A98", alpha = 0.6) +
      # Adicionar o valor da média na barra
      geom_text(aes(label = round(media_palavras_por_episodio, 0)), hjust = -0.1, size = 4) +
      scale_fill_brewer(palette = "Set3") + # Paleta de cores vibrantes
      scale_x_continuous(expand = expansion(mult = c(0, 0.2))) + # Expande o eixo X para acomodar os rótulos
      labs(
        title = "Quem fala mais?",
        subtitle = "Top 10 personagens com mais palavras por episódio",
        x = "Média de Palavras por Episódio",
        y = NULL
      ) +
      theme_bw() +
      theme(
        plot.background = element_rect(fill = 'lightblue'),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 11),
        panel.background = element_rect(fill = "#FFFBAD"),
        axis.text = element_text(face = "bold", size = 11),
        legend.position = "none"
      )
  })
  
########### MAPA DE CALOR ############
  
  heatmap_data <- reactive({
    
    # Personagens da família Simpson
    familia <- c("Homer Simpson", "Marge Simpson", "Bart Simpson", "Lisa Simpson")
    
    # Filtro ano
    data_filtered_by_year <- dados_dialogos_episodios %>%
      filter(ano_exibicao >= input$ano_selecionado[1] & ano_exibicao <= input$ano_selecionado[2])

    # Filtrar família total palavras por episódio e personagem
    palavras_episodio_personagem <- data_filtered_by_year %>%
      filter(nome_personagem %in% familia) %>%
      group_by(episode_id, nome_personagem, imdb_rating) %>%
      summarise(
        total_words = sum(as.numeric(word_count), na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Remover NaNs e episódios sem classificação
    data_for_heatmap <- palavras_episodio_personagem %>%
      filter(!is.na(imdb_rating), !is.na(total_words), total_words > 0)
    
    # Definier bins
    binned_data <- data_for_heatmap %>%
      group_by(nome_personagem) %>%
      mutate(
        # Cria 5 grupos de igual tamanho com base na distribuição de total_words
        word_count_bin_num = ntile(total_words, 5),
        # Cria um rótulo descritivo para cada bin
        word_count_bin = factor(word_count_bin_num, 
                                levels = 1:5,
                                labels = c("Muito Baixo", "Baixo", "Médio", "Alto", "Muito Alto"))
      ) %>%
      ungroup()
    
    # Média IMDb por bin
    mean_imdb_by_bin <- binned_data %>%
      group_by(nome_personagem, word_count_bin) %>%
      summarise(
        mean_imdb = mean(imdb_rating, na.rm = TRUE),
        episodes_n = n(),
        .groups = 'drop'
      ) %>%
      # Garante a ordem correta para o plot (vertical)
      mutate(nome_personagem = factor(nome_personagem, levels = c("Homer Simpson", "Marge Simpson", "Bart Simpson", "Lisa Simpson")))
    
    return(mean_imdb_by_bin)
  })
  
  # Renderização do Mapa de Calor
  output$heatmap_imdb <- renderPlot({
    plot_data <- heatmap_data()
    
    if (nrow(plot_data) == 0) {
      plot(NA, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", bty="n", xaxt="n", yaxt="n")
      text(0.5, 0.5, "Dados insuficientes para gerar o Mapa de Calor.", cex = 1.2)
      return(NULL)
    }
    
    # Título dinâmico
    
    # Criação do mapa de calor usando ggplot2
    ggplot(plot_data, aes(x = word_count_bin, y = nome_personagem, fill = mean_imdb)) +
      geom_tile(color = "white", linewidth = 0.5) + 
      geom_text(aes(label = round(mean_imdb, 2)), color = "#323434", size = 4) + # Adiciona o valor dentro do bloco
      scale_fill_gradient(
        low = "#F8DEE1",  
        high = "#CA2B3D", 
        name = "Média IMDb"
      ) +
      labs(
        title = "Média IMDb x Palavras da Família",
        x = "Total de Palavras Faladas",
        y = NULL
      ) +
      theme_bw() +
      theme(
        plot.background = element_rect(fill = "lightblue", colour = "#3C3E3E"),
        plot.title = element_text(face = "bold", size = 16),
        panel.background = element_rect(fill = "#FFFBAD"),
        axis.text = element_text(face = "bold", size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.background = element_rect(fill = "lightblue", colour = "#3C3E3E"),
        legend.position = "right"
      )
  })
  
########### GRAFICO DE ÁREAS ###########
  
  
  dados_area<- reactive({
    familia <- c("Homer Simpson", "Marge Simpson", "Bart Simpson", "Lisa Simpson")
    
    # Filtrar membros da família
    dados_familia <- dados_dialogos_episodios %>%
      filter(nome_personagem %in% familia)
    
    # Filtrar período
    dados_familia <- dados_familia %>%
      filter(ano_exibicao >= input$ano_selecionado[1] & ano_exibicao <= input$ano_selecionado[2])
    
    
    # Filtrar locais
    if (input$local_selecionado != 'all') {
      dados_familia <- dados_familia %>%
        filter(locais == input$local_selecionado)
    }
    
    # Total de palavras por episódio, personagem e ano
    palavras_personagem_ano <- dados_familia %>%
      group_by(episode_id, ano_exibicao, nome_personagem) %>%
      summarise(
        total_palavras_por_episodio = sum(word_count, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Média de palavras por episódio ano para cada personagem
    media_palavras_personagem_ano <- palavras_personagem_ano %>%
      group_by(ano_exibicao, nome_personagem) %>%
      summarise(
        media_palavras_por_episodio = mean(total_palavras_por_episodio, na.rm = TRUE),
        episodes_n = n_distinct(episode_id),
        .groups = 'drop'
      ) %>%
      # Filtra anos/personagens com menos de 5 episódios para maior estabilidade
      filter(episodes_n >= 2, !is.na(ano_exibicao)) %>%
      # Garante a ordem e o fator para o gráfico
      mutate(nome_personagem = factor(nome_personagem, levels = familia))
    
    return(media_palavras_personagem_ano)
  })
  
  # NOVO: Renderização do Diagrama Linear de Áreas
  output$linear_area_chart <- renderPlot({
    plot_data <- dados_area() # Reutilizando a preparação de dados
    
    if (nrow(plot_data) == 0) {
      plot(NA, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", bty="n", xaxt="n", yaxt="n")
      text(0.5, 0.5, "Filtro retornou dados insuficientes", cex = 1.2)
      return(NULL)
    }
    
    # Criação do gráfico base
    p <- ggplot(plot_data, aes(x = ano_exibicao, y = media_palavras_por_episodio, fill = nome_personagem)) +
      # Use geom_area para o preenchimento da área e defina transparência
      # position="identity" garante sobreposição em vez de empilhamento
      geom_area(alpha = 0.35, position = "identity") +
      # Linhas para destacar a variação
      geom_line(aes(group = nome_personagem, color = nome_personagem), linewidth = 1) + 
      
      # Configurações de cores (Cores temáticas dos Simpsons)
      scale_fill_manual(values = c("Homer Simpson" = "#ff9900", "Marge Simpson" = "#0066cc", 
                                   "Bart Simpson" = "#cc4c02", "Lisa Simpson" = "#339933")) +
      scale_color_manual(values = c("Homer Simpson" = "#ff9900", "Marge Simpson" = "#0066cc", 
                                    "Bart Simpson" = "#cc4c02", "Lisa Simpson" = "#339933")) +
      # Configura o eixo X para ser discreto e exibir todos os anos
      scale_x_continuous(breaks = unique(plot_data$ano_exibicao),
                         labels = unique(plot_data$ano_exibicao)) +
      labs(
        title = "Média de Palavras por Episódio - Apenas Família Simpson",
        subtitle = "A família mudou com os anos?",
        x = "Ano",
        y = "Média de Palavras",
        fill = "Personagem",
        color = "Personagem"
      ) +
      # 3. Tema
      theme_bw() +
      theme(
        plot.background = element_rect(fill = "lightblue", colour = "#3C3E3E"),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5),
        panel.background = element_rect(fill = "#FFFBAD"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(size = 11),
        axis.text.x = element_text(angle = 60, vjust = 0.5, size = 9), # Rotaciona rótulos do ano
        axis.title = element_text(size = 11, face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.background = element_rect(fill = "lightblue", colour = "#3C3E3E")
      )
    
    print(p)
  })
  
  
########### WORDCLOUD ############

  # Expressão reativa para filtrar os dados (usada pela Nuvem de Palavras)
  dados_filtrados_nuvem <- reactive({
    dados_nuvem <- dados_dialogos_episodios %>%
      select(ano_exibicao,nome_personagem,raw_location_text,normalized_text,gender)
    
#    Filtro por Personagem
    if (input$filtro_personagem != "all") {
      dados_nuvem <- dados_nuvem %>%
        filter(nome_personagem == input$filtro_personagem)
    }
    
    # Filtro por Ano
    dados_nuvem <- dados_nuvem %>%
      filter(ano_exibicao >= input$ano_selecionado[1] & ano_exibicao <= input$ano_selecionado[2])

    # Filtro por Local
    if (input$local_selecionado != "all") {
      dados_nuvem <- dados_nuvem %>%
        filter(raw_location_text == input$local_selecionado)
    }
    
    # Filtro gênero
    if (input$filtro_genero !='all') {
      dados_nuvem <- dados_nuvem %>%
        filter(gender == input$filtro_genero)
    }
    
    # Selecionar apenas a coluna de texto para processamento
    dados_nuvem$normalized_text
  })
  
  texto_processado <- reactive({
    text_vector <- dados_filtrados_nuvem()
    
    # Criar um Corpus a partir do vetor de texto
    corpus <- Corpus(VectorSource(text_vector))
    # Remover números
    corpus <- tm_map(corpus, removeNumbers)
    # Remover stopwords 
    corpus <- tm_map(corpus, removeWords, c(stopwords("english"),outwords))
  })
  
  # Renderização do Título Dinâmico
  output$titulo_nuvem <- renderText({
    personagem <- if (input$filtro_personagem == "all") "Todos os Personagens" else input$filtro_personagem
    local <- if (input$local_selecionado == "all") "em Todos os Locais" else paste("em", input$local_selecionado)
    ano_inicio <- input$ano_selecionado[1]
    ano_fim <- input$ano_selecionado[2]
    
    paste("Nuvem de Palavras para", personagem, local, " (", ano_inicio, "-", ano_fim, ")")
  })
  
  # Renderização da Nuvem de Palavras
  output$nuvem_palavras <- renderPlot({
    cloud_data <- texto_processado()
    
    # Criar a nuvem de palavras
    wordcloud(cloud_data,
      #min.freq = 10,                      # Frequência mínima
      scale = c(5,.4),
      max.words = 70,                    # Máximo de palavras
      random.order = FALSE,               # Palavras mais frequentes no centro
      rot.per = 0.15,                     # Percentual de palavras rotacionadas
      colors = colorRampPalette(c('#F0BCC4','#B72A3F'))(15)
    )
  })
  
  
########### DISPERSÃO IMDB ############
  # Expressão reativa para preparar os dados para o Gráfico de Dispersão por Gênero
  imdb_genero <- reactive({
    
    # 1. Aplicar filtro de ano
    dados_scatter <- dados_dialogos_episodios %>%
      filter(ano_exibicao >= input$ano_selecionado[1] & ano_exibicao <= input$ano_selecionado[2])
    
    # 2. Agrupar por episódio e personagem para obter o total de palavras por fala
    palavras_personagem_por_episodio <- dados_scatter %>%
      group_by(episode_id, imdb_rating, ano_exibicao, gender) %>%
      summarise(
        total_words_in_episode = sum(word_count, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      # 3. Filtrar episódios com classificação e personagens com gênero definido (f/m)
      filter(!is.na(imdb_rating), !is.na(gender), gender %in% c("Female", "Male"))
    
    return(palavras_personagem_por_episodio)
  })
  
  # Renderização do Gráfico de Dispersão IMDb vs. Palavras por Gênero
  output$scatter_imdb <- renderPlot({
    plot_data <- imdb_genero()
    
    # if (nrow(plot_data) < 50) { # Precisa de um número razoável de pontos
    #   plot(NA, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", bty="n", xaxt="n", yaxt="n")
    #   text(0.5, 0.5, "Dados insuficientes para gerar o Gráfico de Dispersão por Gênero.", cex = 1.2)
    #   return(NULL)
    # }
    
    # Criação do gráfico de dispersão usando ggplot2 com facet
    ggplot(plot_data, aes(x = total_words_in_episode, y = imdb_rating)) +
      geom_point(alpha = 0.5, colour = "#0F8CFA") + # Pontos de dispersão
      #geom_smooth(method = "lm", se = FALSE, color = "#cc4c02", linewidth = 1.5) + # Linha de regressão
      facet_wrap(~ gender) + # Separar por Gênero (Facets)
      #scale_color_gradient(low = "yellow", high = "#0066cc", name = "Ano do Episódio") +
      scale_x_continuous(labels = comma, trans = "log10") + # Formatar o eixo X
      labs(
        title = "Volume de Palavras x Avaliação IMDb",
        subtitle = "A quantidade de palavras influencia o gosto popular?",
        x = "Total de Palavras do Personagem no Episódio",
        y = "Avaliação IMDb do Episódio"
      ) +
      theme_bw() +
      theme(
        axis.text = element_text(size = 11),
        axis.title = element_text(face = "bold"),
        plot.background = element_rect(fill = 'lightblue'),
        plot.margin = margin(5,8,5,8),
        panel.background = element_rect(fill = "#F4CDD3"),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5),
        strip.text = element_text(face = "bold", size = 12),
        strip.background = element_rect(fill = '#E17A89', colour = '#E17A89'),
        legend.position = "bottom"
      )
  })

}