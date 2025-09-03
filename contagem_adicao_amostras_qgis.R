
# Contagem de novas amostras adicionadas às amostras originais --------------------------------------------------------------------------------------------

# Carregar pacotes ----------------------------------------------------------------------------------------------------------------------------------------

library(sf)

# Tile 035016 ---------------------------------------------------------------------------------------------------------------------------------------------

# Carregar amostras

amostras_originais <- sf::read_sf("amostras_originais_tile_035016.shp")
amostras_totais <- sf::read_sf("amostras_adicionais_035016.shp")

View(amostras_originais)
View(amostras_totais)

# Extrair coordenadas e renomear

totais_df <- amostras_totais %>%
  mutate(longitude = st_coordinates(.)[,1],
         latitude  = st_coordinates(.)[,2]) %>%
  st_set_geometry(NULL)  # remove a geometria para virar data.frame

originais_df <- amostras_originais %>%
  mutate(longitude = st_coordinates(.)[,1],
         latitude  = st_coordinates(.)[,2]) %>%
  st_set_geometry(NULL)

# Verificar as amostras que não estão contidas nas duas tabelas

pontos_novos_df <- anti_join(totais_df, originais_df, by = c("longitude", "latitude"))

View(pontos_novos_df)

unique(pontos_novos_df$label)

# Verificar novas amostras adicionadas por classes

pontos_novos_df_ar <- pontos_novos_df |>
  filter(label == "aflor_rocha")

View(pontos_novos_df_ar)
  
pontos_novos_df_sup <- pontos_novos_df |>
  filter(label == "supressao")
  
View(pontos_novos_df_sup)
  
pontos_novos_df_agua <- pontos_novos_df |>
  filter(label == "agua")

View(pontos_novos_df_agua)
