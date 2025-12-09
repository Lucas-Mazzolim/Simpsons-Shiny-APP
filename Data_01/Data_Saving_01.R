# Ler dados do github e salvar como .csv localmente
library(tidytuesdayR)
library(tidyverse)
library(tidytext)

tuesdata <- tidytuesdayR::tt_load('2025-02-04')

# Lendo dados
simpsons_characters <- tuesdata$simpsons_characters
simpsons_episodes <- tuesdata$simpsons_episodes
simpsons_locations <- tuesdata$simpsons_locations
simpsons_script_lines <- tuesdata$simpsons_script_lines


# Limpando os dados dos episodios (NA e Season 28)
episodes_clean <- simpsons_episodes %>% 
  filter(!is.na(us_viewers_in_millions),!is.na(imdb_rating),season != 28)  %>%
  select(-image_url, -video_url) 


# #Limpando os dados das falas
script_lines_clean <- simpsons_script_lines %>%
  filter(!is.na(raw_character_text),raw_character_text != "",
         !is.na(spoken_words),spoken_words != "",
         str_trim(spoken_words) != "") %>%
          select(-timestamp_in_ms)

# joins
tabela_mestre <- script_lines_clean %>%
  inner_join(episodes_clean, by = c("episode_id" = "id")) %>% 
  left_join(simpsons_characters, by = c("character_id" = "id")) %>%
  left_join(simpsons_locations, by = c("location_id" = "id")) 

# Salvando em .csv
write_csv(simpsons_characters, './Data_01/characters.csv')
write_csv(episodes_clean, './Data_01/episodes.csv')
write_csv(simpsons_locations, './Data_01/locations.csv')
write_csv(simpsons_script_lines, './Data_01/script_lines.csv')
write_csv(tabela_mestre, './Data_01/tabela_mestre.csv')

#criando metadados
# dialogos totais e diversidade de locais por episódio
metadados_episodio <- tabela_mestre |>
  group_by(episode_id) |>
  summarise(
    total_dialogos = n(),  # Conta quantas linhas existem neste episódio
    location_diversity = n_distinct(location_id, na.rm = TRUE)) # Conta locais únicos

tabela_mestre_final <- tabela_mestre |>
  left_join(metadados_episodio, by = "episode_id")

write_csv(tabela_mestre_final, './Data_01/tabela_mestre_final.csv')


