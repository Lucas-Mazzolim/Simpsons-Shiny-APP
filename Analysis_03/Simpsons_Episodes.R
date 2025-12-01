install.packages("tidytuesdayR")
install.packages("tidyverse")
library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2025-02-04')

simpsons_characters <- tuesdata$simpsons_characters
simpsons_episodes <- tuesdata$simpsons_episodes
simpsons_locations <- tuesdata$simpsons_locations
simpsons_script_lines <- tuesdata$simpsons_script_lines

episodes_clean <- simpsons_episodes %>% 
  filter(!is.na(us_viewers_in_millions),
         !is.na(imdb_rating)) #Limpeza da base de dados


###AUDIÊNCIA COM O PASSAR DO TEMPO
views_by_season <- episodes_clean %>%
  group_by(season) %>%
  summarise(
    total_viewers = sum(us_viewers_in_millions, na.rm = TRUE),
    mean_viewers = mean(us_viewers_in_millions, na.rm = TRUE),
    n = n()
  )
views_by_season

ggplot(views_by_season, aes(season, mean_viewers)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Audiência média por temporada",
    x = "Temporada",
    y = "Audiência média (milhões)"
  ) +
  theme_minimal() #Grafico ilustrando a audiência da série ao longo das temporadas

ggplot(episodes_clean, aes(original_air_year, us_viewers_in_millions, color = factor(season))) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Audiência por episódio ao longo dos anos",
    x = "Ano",
    y = "Audiência (milhões)",
    color = "Temporada"
  ) +
  theme_minimal() #Grafico ilustrando a audiência da série com o passar do tempo

###RELAÇÃO ENTRE AUDIÊNCIA E NOTAS IMDB
episodes_clean %>% 
  ggplot(aes(x = us_viewers_in_millions, y = imdb_rating)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Audiência vs IMDb Rating — The Simpsons",
    x = "Audiência nos EUA (milhões)",
    y = "Nota no IMDb"
  )

ggplot(episodes_clean, aes(x = us_viewers_in_millions, y = imdb_rating)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Correlação entre audiência e nota no IMDB – The Simpsons",
    x = "Audiência (milhões)",
    y = "Nota no IMDB"
  ) +
  theme_minimal() #Grafico de dispersao ilustrando relacao audiência x notas IMDB

correlation <- cor(episodes_clean$us_viewers_in_millions,
                   episodes_clean$imdb_rating,
                   method = "pearson") 
correlation #Força de correlação da relacao audiência x nota IMDB

model <- lm(imdb_rating ~ us_viewers_in_millions, data = episodes_clean)
summary(model) #Regressão aplicada à relacao audiência x nota IMDB

ggplot(episodes_clean, aes(x = us_viewers_in_millions, y = imdb_rating)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ season, scales = "free_x") +
  labs(
    title = "Relação entre audiência e nota por temporada – The Simpsons",
    x = "Audiência (milhões)",
    y = "Nota no IMDB"
  ) +
  theme_minimal() #Graficos indicando a relacao audiencia x nota POR TEMPORADA

corr_by_season_rating <- episodes_clean %>%
  group_by(season) %>%
  summarise(
    n = n(),
    corr = cor(us_viewers_in_millions, imdb_rating, use = "complete.obs", method = "pearson")
  )
corr_by_season_rating #Força de correlação da relacao audiência x nota POR TEMPORADA

episodes_flags <- episodes_clean %>%
  mutate(
    low_views  = us_viewers_in_millions < quantile(us_viewers_in_millions, 0.25, na.rm = TRUE),
    high_views = us_viewers_in_millions > quantile(us_viewers_in_millions, 0.75, na.rm = TRUE),
    low_rating  = imdb_rating < quantile(imdb_rating, 0.25, na.rm = TRUE),
    high_rating = imdb_rating > quantile(imdb_rating, 0.75, na.rm = TRUE),
    low_votes  = imdb_votes < quantile(imdb_votes, 0.25, na.rm = TRUE),
    high_votes = imdb_votes > quantile(imdb_votes, 0.75, na.rm = TRUE)
  )
episodes_flags

episodes_flags %>%
  mutate(
    views_group = ifelse(low_views, "Baixa audiência",
                         ifelse(high_views, "Alta audiência", "Média")),
    rating_group = ifelse(low_rating, "Baixa nota",
                          ifelse(high_rating, "Alta nota", "Média"))
  ) %>%
  count(views_group, rating_group) %>%
  ggplot(aes(views_group, rating_group, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), size = 5) +
  scale_fill_gradient(low = "lightyellow", high = "red") +
  labs(
    title = "Relação entre audiência e nota – categorias por quartis",
    x = "Grupo de audiência",
    y = "Grupo de nota",
    fill = "Episódios"
  ) +
  theme_minimal() #Heatmap indicando relacao audiencia x nota

###RELAÇÃO ENTRE AUDÊNCIA E NÚMERO DE VOTOS
corr_by_season_votes <- episodes_clean %>%
  group_by(season) %>%
  summarise(
    n = n(),
    corr = cor(us_viewers_in_millions, imdb_votes, use = "complete.obs", method = "pearson")
  )
corr_by_season_votes

ggplot(corr_by_season_votes, aes(x = season, y = corr)) +
  geom_line(size = 1.1, color = "darkred") +
  geom_point(size = 3, color = "darkred") +
  labs(
    title = "Correlação entre audiência e votos IMDb por temporada",
    x = "Temporada",
    y = "Correlação de Pearson"
  ) +
  theme_minimal() #Grafico ilustrando a correlacao audiencia x numero de votos no IMDB

model_votes <- lm(imdb_votes ~ us_viewers_in_millions, data = episodes_clean)
summary(model_votes) #Regressão da correlacao audiencia x numero de votos no IMDB

ggplot(episodes_clean, aes(us_viewers_in_millions, imdb_votes)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Audiência vs Número de Votos no IMDb",
    x = "Audiência (milhões)",
    y = "Número de votos no IMDb"
  ) +
  theme_minimal()


###VISUALIZAÇÃO AUDIÊNCIA X NOTAS X VOTOS
ggplot(episodes_clean, aes(us_viewers_in_millions, imdb_rating,
                           color = factor(season), size = imdb_votes)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(2, 10)) +
  labs(
    title = "Audiência x Nota x Número de Votos – The Simpsons",
    x = "Audiência (milhões)",
    y = "Nota IMDb",
    color = "Temporada",
    size = "Votos IMDb"
  ) +
  theme_minimal()


###NOTAS IMDB POR TEMPORADA
imdb_summary <- episodes_clean %>%
  group_by(season) %>%
  summarise(
    n_episodios = n(),
    media = mean(imdb_rating, na.rm = TRUE),
    mediana = median(imdb_rating, na.rm = TRUE),
    desvio = sd(imdb_rating, na.rm = TRUE),
    minimo = min(imdb_rating, na.rm = TRUE),
    maximo = max(imdb_rating, na.rm = TRUE)
  ) %>%
  arrange(media)
imdb_summary

ggplot(episodes_clean, aes(x = factor(season), y = imdb_rating)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  labs(
    title = "Distribuição das notas do IMDB por temporada – The Simpsons",
    x = "Temporada",
    y = "Nota IMDB"
  ) +
  theme_minimal() #boxplots indicando notas por temporada


### VERIFICANDO OUTLIERS
outliers_per_season <- episodes_clean %>%
  group_by(season) %>%
  mutate(
    Q1 = quantile(imdb_rating, 0.25, na.rm = TRUE),
    Q3 = quantile(imdb_rating, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower = Q1 - 1.5 * IQR,
    upper = Q3 + 1.5 * IQR,
    is_outlier = imdb_rating < lower | imdb_rating > upper
  ) %>%
  filter(is_outlier) %>%
  select(season, title, imdb_rating)
outliers_per_season #Por temporada

global_outliers <- episodes_clean %>%
  mutate(
    z = as.numeric(scale(imdb_rating)),
    outlier_global = abs(z) > 3
  ) %>%
  filter(outlier_global) %>%
  select(season, title, imdb_rating, z)
global_outliers #Outliers globais
