library(tidyverse)
library(tidytext)

script <- simpsons_script_lines %>%
  left_join(simpsons_episodes %>% select(id, season), 
            by = c("episode_id" = "id"))

frases_com_personagem <- script %>%
  left_join(simpsons_characters, by = c("character_id" = "id")) %>%
  filter(!is.na(spoken_words), spoken_words != "") %>%
  count(name, spoken_words, sort = TRUE)

### PERSONAGENS QUE MAIS FALAM
top_speakers <- script %>%
  filter(speaking_line == TRUE, !is.na(character_id)) %>%
  group_by(raw_character_text) %>%
  summarise(
    n_lines = n(),
    total_words = sum(word_count, na.rm = TRUE)
  ) %>%
  arrange(desc(n_lines)) %>%
  slice_head(n = 15)
top_speakers

ggplot(top_speakers, aes(x = reorder(raw_character_text, n_lines), y = n_lines)) +
  geom_col(fill = "gold") +
  coord_flip() +
  labs(
    title = "Personagens com mais falas (2010–2016)",
    x = "Personagem",
    y = "Número de falas"
  ) +
  theme_minimal()


### EVOLUÇÃO DAS FALAS POR TEMPORADA
trend <- script %>%
  filter(speaking_line == TRUE) %>%
  group_by(season, raw_character_text) %>%
  summarise(n_lines = n(), .groups = "drop") %>%
  group_by(raw_character_text) %>%
  filter(sum(n_lines) > 150)
trend

ggplot(trend, aes(x = season, y = n_lines, color = raw_character_text)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Evolução das falas por temporada",
    x = "Temporada",
    y = "Número de falas"
  ) +
  theme_minimal()


### TAMANHO DAS FALAS POR PERSONAGEM
avg_words <- script %>%
  filter(speaking_line == TRUE) %>%
  group_by(raw_character_text) %>%
  summarise(
    mean_words = mean(word_count, na.rm = TRUE),
    n_lines = n()
  ) %>%
  filter(n_lines > 100)
avg_words

ggplot(avg_words, aes(x = reorder(raw_character_text, mean_words), y = mean_words)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Tamanho médio das falas por personagem",
    x = "Personagem",
    y = "Palavras por fala"
  ) +
  theme_minimal()

### FRASES MAIS FALADAS
frases_mais_faladas <- script %>%
  filter(!is.na(spoken_words), spoken_words != "") %>%
  mutate(spoken_norm = spoken_words %>%
           str_to_lower() %>%
           str_trim()) %>%
  count(spoken_norm, sort = TRUE) %>%
  slice_head(n = 10) %>%
  mutate(spoken_norm = fct_reorder(spoken_norm, n))
frases_mais_faladas

ggplot(frases_mais_faladas, aes(x = spoken_norm, y = n)) +
  geom_col(fill = "#FFC20A") +
  coord_flip() +
  labs(title = "10 Frases Mais Faladas em The Simpsons (2010–2016)",
       x = "Frase",
       y = "Número de repetições") +
  theme_minimal(base_size = 13)

frases_personagens <- script %>%
  left_join(simpsons_characters, by = c("character_id" = "id")) %>%
  filter(!is.na(spoken_words), spoken_words != "", !is.na(name)) %>%
  mutate(spoken_norm = spoken_words %>%
           str_to_lower() %>%
           str_trim()) %>%
  count(name, spoken_norm, sort = TRUE) %>%
  group_by(name) %>%
  slice_max(n, n = 3) %>%
  ungroup() %>%
  mutate(spoken_norm = fct_reorder(spoken_norm, n))
frases_personagens