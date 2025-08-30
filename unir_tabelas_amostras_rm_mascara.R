# Carregar pacote

library(sf)

# ler as camadas

pontos <- st_read("Amostras_rm5_29_08_2025.shp")
poligonos <- st_read("poligono_rm_5.shp")
mascara <- st_read("mascara_rm5.shp")

st_crs(pontos)
st_crs(poligonos)
st_crs(mascara)

# unir atributos por localização

pontos_com_regiao <- st_join(pontos, poligonos)

sf_use_s2(FALSE)

pontos_final <- st_join(pontos_com_regiao, mascara)

# visualizar

head(pontos_final)
view(pontos_final)

# Resumir tabela

pontos_final <- pontos_final |>
  dplyr::select(-c(8:24))

# Verificar se existe NA nas colunas mais importantes

view(pontos_final)
unique(is.na(pontos_final$longitude))
unique(is.na(pontos_final$latitude))
unique(is.na(pontos_final$label))

# Teste da máscara por tile

mascara_tile_034018 <- pontos_final |>
  filter(tile == "034018")

view(mascara_tile_034018)
