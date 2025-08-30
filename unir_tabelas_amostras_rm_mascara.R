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

st_geometry_type(mascara)
st_geometry_type(poligonos)

mascara_tiles <- st_join(poligonos, mascara)

# visualizar

view(mascara_tiles)

ggplot(mascara_tiles) +
  geom_sf(fill = "darkblue", color = "darkblue") +
  theme_minimal()

# Resumir tabela

# mascara_tiles <- mascara_tiles |>
#   dplyr::select(-c(3:19))
# 
# view(mascara_tiles)

# Teste da máscara por tile

mascara_tile_034018 <- mascara_tiles[5, 3]

view(mascara_tile_034018)

# Verificar validade da máscara

mask_sf_034018 <- sf::st_make_valid(mascara_tile_034018) # Validade da topologia
sf::st_geometry_type(mask_sf_034018) # Verificar tipo de geometria, deve ser multipoligon ou poligon

# Salvar shp

sf::st_write(mask_sf_034018, "mask_sf_034018.shp")

mask_sf_034018 <- read_sf("mask_sf_034018.shp")

library(ggplot2)

ggplot(mascara) +
  geom_sf(fill = "darkblue", color = "darkblue") +
  theme_minimal()


