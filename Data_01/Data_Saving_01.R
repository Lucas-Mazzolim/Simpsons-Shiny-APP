# Ler dados do github e salvar como .csv localmente
library(tidyverse)

# Lendo dados
simpsons_characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_characters.csv')
simpsons_episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_episodes.csv')
simpsons_locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_locations.csv')
simpsons_script_lines <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_script_lines.csv')

# Salvando em .csv
write_csv(simpsons_characters, './Data_01/characters.csv')
write_csv(simpsons_episodes, './Data_01/episodes.csv')
write_csv(simpsons_locations, './Data_01/locations.csv')
write_csv(simpsons_script_lines, './Data_01/script_lines.csv')

# Salvando dados em tabela central
simpsons_characters |>
  glimpse()

simpsons_episodes |>
  glimpse()

simpsons_script_lines |>
  glimpse()

tabela_mestre = simpsons_script_lines |>
  left_join(
    simpsons_episodes,
    by = c('episode_id' = 'id')
  ) |>
  left_join(
    simpsons_characters,
    by = c('character_id' = 'id')
  ) |>
  left_join(
    simpsons_locations,
    by = c('location_id' = 'id')
  )

write_csv(tabela_mestre, './Data_01/tabela_mestre.csv')
