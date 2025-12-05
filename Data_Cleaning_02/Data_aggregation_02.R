# criação de metadados

# diálogos totais e diversidade de locais por episódio

metadados_episodio <- tabela_mestre |>
  group_by(episode_id) |>
  summarise(
    total_dialogos = n(),  # Conta quantas linhas existem neste episódio
    location_diversity = n_distinct(location_id, na.rm = TRUE) # Conta locais únicos
  )

tabela_mestre_final <- tabela_mestre |>
  left_join(metadados_episodio, by = "episode_id")

write_csv(tabela_mestre_final, './Data_01/tabela_mestre_final.csv')