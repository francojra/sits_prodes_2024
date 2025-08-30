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

# Verificar se existe NA nas colunas mais importantes

view(pontos_com_regiao)
unique(is.na(pontos_com_regiao$longitude))
unique(is.na(pontos_com_regiao$latitude))
unique(is.na(pontos_com_regiao$label))

# unir tabela com tile e geometria da máscara

sf_use_s2(FALSE)

mascara_tiles <- st_join(poligonos, mascara)

# visualizar

view(mascara_tiles)

# Resumir tabela

mascara_tiles <- mascara_tiles |>
  dplyr::select(-c(3:19))

view(mascara_tiles)

# Teste da máscara por tile

mascara_tile_034018 <- mascara_tiles[5, 3]

view(mascara_tile_034018)

st_geometry(mascara_tile_034018)

plot(st_geometry(mascara_tile_034018), col = "lightblue", border = "darkblue")



