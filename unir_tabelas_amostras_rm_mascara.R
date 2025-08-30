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

class(pontos_com_regiao)

# Verificar se existe NA nas colunas mais importantes

view(pontos_com_regiao)
unique(is.na(pontos_com_regiao$longitude))
unique(is.na(pontos_com_regiao$latitude))
unique(is.na(pontos_com_regiao$label))

pontos_com_regiao <- sf::st_write(pontos_com_regiao, "pontos_com_regiao.shp")

st_read("pontos_com_regiao.shp")
