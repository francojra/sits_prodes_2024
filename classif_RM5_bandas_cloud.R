# Script para Classificação do Bioma Caatinga --------------------------------------------------------------------------------------------------------------
# Classificação por Região de Mapeamento (RM) --------------------------------------------------------------------------------------------------------------
# Região de mapeamento: 5 ----------------------------------------------------------------------------------------------------------------------------------
# Tiles: 33016, 33018, 34016, 34017, 34018, 35015, 35016, 35017 --------------------------------------------------------------------------------------------
# Bandas: B01,B02,B03,B04,B05,B06,B07,B08,B09,B11, B12, B8A ------------------------------------------------------------------------------------------------
# Classes: água, vegetação natural, abiótico, queimada, supressao ------------------------------------------------------------------------------------------
# Número total de amostras: xxx ----------------------------------------------------------------------------------------------------------------------------

# Carregar pacotes -----------------------------------------------------------------------------------------------------------------------------------------

library(tibble) # Pacote para visualizar tabelas
library(sits) # Pacote para análises de séries temporais de imagens de satélite
library(sitsdata) # Pacote para obter conjunto de dados de amostras do sits
library(kohonen) # Pacote para plotar o mapa SOM
library(randomForestExplainer) # Pacote para treinar modelo de classificação
library(luz) # Pacote para facilitar criação, treino e avaliação de modelos no Torch
library(torch) # Pacote para criar modelos deep learning e treinar redes neurais
torch::install_torch()
library(tidyverse) # Pacote para manipulação de tabelas e gráficos
library(terra) # Pacote para manipular dados espaciais (imagens raster, dados de satélite)
library(raster) # Pacote mais antigo para manipulação de dados raster
library(sf) # Pacote para manipulação de dados vetoriais (pontos, linhas, polígonos)

# Criar e ler cubo de dados -------------------------------------------------------------------------------------------------------------------------------

# cubo <- sits_cube(
#   source     = "BDC", # Fonte dos cubos de dados
#   collection = "SENTINEL-2-16D", # Coleção de imagens
#   tiles      = c("033016", "033018", "034016", "034017",
#                  "034018", "035015", "035016", "035017"), # Tiles/Regiões de ineteresse
#   start_date = "2024-01-01", # Data inicial 
#   end_date   = "2024-12-31") # Data final 

## Verificar bandas, tempos e outras informações do cubo 

sits_bands(cubo)
sits_timeline(cubo)
view(cubo)
view(cubo$file_info)

## Salvar e ler cubo criado

# saveRDS(cubo, file = "cubo.rds") 
cubo <- readRDS("cubo.rds")

# Ler arquivo .shp com amostras por classes ---------------------------------------------------------------------------------------------------------------

amostras_classes <- sf::read_sf("Amostras_RM5_29_08_2025.shp")

amostras_classes <- amostras_classes |>
  mutate(label = case_when(label == "aflor_rocha" ~ "abiotico",
                           label == "aflor_rcoha" ~ "abiotico",
                           TRUE ~ label)) |>
  dplyr::select(- start_date, - end_date)

view(amostras_classes)
unique(amostras_classes$label)
unique(is.na(amostras_classes))

# Adicionar amostras ao cubo de dados criado --------------------------------------------------------------------------------------------------------------

cubo_amostras <- sits_get_data(
  cubo, 
  samples = amostras_classes, # Arquivo shapefile do tile 034018
  label_attr = "label", # Coluna que indica as classes das amostras (pontos)
  bands = c("B01",   "B02",   "B03",   "B04",   "B05",   
            "B06",   "B07",   "B08",   "B09",   "B11",   
            "B12", "B8A", "CLOUD"), # Seleção de bandas e índices
  memsize = 85, # consumo de memória
  multicores = 20, # Número de núcleos usados. Quanto maior, mais rápido o processamento
  progress = TRUE) # Acompanhar carregamento

## Verificar informações do cubo com amostras

view(cubo_amostras)

## Salvar e ler cubo com amostras

saveRDS(cubo_amostras, file = "cubo_amostras.rds") 
cubo_amostras <- readRDS("cubo_amostras.rds")
view(cubo_amostras)
sits_bands(cubo_amostras)
sits_labels(cubo_amostras)

# Visualizar padrões de séries temporais por classe -------------------------------------------------------------------------------------------------------

padroes_tempo_amostras <- sits_patterns(cubo_amostras) # Média harmônica das séries temporais 
view(padroes_tempo_amostras$time_series[[1]])
view(padroes_tempo_amostras)

## Gráfico

p <- plot(padroes_tempo_amostras)
p + theme_bw() +
  theme(legend.position = "top",
        axis.text = element_text(color = "black"))

## Verificar proporção e nº de amostras balanceadas e não balanceadas

summary(cubo_amostras) # Nº de amostras não balanceadas

# Análise SOM ---------------------------------------------------------------------------------------------------------------------------------------------

## Definir cores das classes

sits_colors_set(tibble(name = c("abiotico", "queimada", "supressao", 
                                "veg_natural", "agua"),
                       color = c("#1A1A1A", "#D60C00", "#FAE9A0",
                                 "#A6D96A", "#A1DDEF"))
)

som_cluster <- sits_som_map(
  data = cubo_amostras, # SOM feito com grupo de amostras 
  grid_xdim = 20, # Grade eixo x. Aqui é 10 x 10 para gerar 100 neurônios
  grid_ydim = 20, # Grade eixo y
  distance = "dtw", # Método de calcular a distância,
  mode = "pbatch", # Gera o mesmo mapa SOM a cada run
  rlen = 20) # Número de iterações (quantidade de vezes que o mapa é gerado)

## Visualizar mapa SOM

plot(som_cluster, band = "B01") 
plot(som_cluster, band = "B02")
plot(som_cluster, band = "B03")
plot(som_cluster, band = "B04") 
plot(som_cluster, band = "B05")
plot(som_cluster, band = "B06")
plot(som_cluster, band = "B07") 
plot(som_cluster, band = "B08")
plot(som_cluster, band = "B09")
plot(som_cluster, band = "B11")
plot(som_cluster, band = "B12")
plot(som_cluster, band = "B8A")

# Seleção de neurônios no SOM -----------------------------------------------------------------------------------------------------------------------------

view(som_cluster$data)

## Identificar amostras outliers de queimada

amostras_filt_neuro2 <- som_cluster$data[som_cluster$data$id_neuron == 204, ]
view(amostras_filt_neuro2)

amostras_filt_neuro2 <- som_cluster$data[som_cluster$data$id_neuron == 308, ]
view(amostras_filt_neuro2)

# Detectar ruídos das amostras ----------------------------------------------------------------------------------------------------------------------------

all_samples <- sits_som_clean_samples(som_map = som_cluster,
                                      keep = c("clean", "analyze", "remove"))

view(all_samples)

## Visualizar gráfico

p1 <- plot(all_samples)
p1 + theme_bw() +
  theme(axis.text = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "top")

summary(all_samples) # Número de amostras (mesma quantidade das originais ou balanceadas)

# Avaliar matriz de confusão das amostras sem filtragem ------------------------------------------------------------------------------------------

## Função de avaliação

avaliacao_som <- sits_som_evaluate_cluster(som_cluster)
# avaliacao_som_limpo <- sits_som_evaluate_cluster(som_cluster_limpo)

## Gráficos

p3 <- plot(avaliacao_som) 
p3 + theme(axis.text = element_text(color = "black"))

# p3.1 <- plot(avaliacao_som_limpo)
# p3.1 + theme(axis.text = element_text(color = "black"))

## Resultados das avaliações

avaliacao_som 
print(avaliacao_som, n = 23)

# Classificações ------------------------------------------------------------------------------------------------------------------------------------------

## Leitura do cubo criado com bandas e índices

cubo <- readRDS("cubo.rds")

view(cubo)

# Treinar modelo Random Forest ----------------------------------------------------------------------------------------------------------------------------

## Treinar modelo com amostras limpas (VERIFICAR AMOSTRAS LIMPAS OU NÃO)

set.seed(5207418)

rf_model <- sits_train(
  samples = all_samples, # Se precisar de amostras originais --> samples_clean
  ml_method = sits_rfor()) # Modelo Random Forest

## Gráfico com as variávies mais importantes do modelo

plot(rf_model)

# Validação do modelo Random Forest -----------------------------------------------------------------------------------------------------------------------

set.seed(520)

rfor_valid <- sits_kfold_validate(
  samples    = all_samples,
  folds      = 5, 
  ml_method  = sits_rfor(),
  multicores = 5)

rfor_valid 

# Preparar máscara ----------------------------------------------------------------------------

# Ler cubo e CRS dos tiles

view(cubo)
class(cubo)
crs(cubo[1, ]$crs) # CRS do primeiro tile

# Ler máscara

mask_sf <- read_sf("PRODES_MARCO_2000_2023_RM5_Dissolvido.shp")

class(mask_sf)
view(mask_sf)
crs(mask_sf) # CRS da máscara

# Verificar validade da máscara

mask_sf <- sf::st_make_valid(mask_sf) # Validade da topologia
sf::st_geometry_type(mask_sf) # Verificar tipo de geometria, deve ser multipoligon ou poligon

# Tornar máscara para o CRS dos tiles 

mask_sf <- sf::st_transform(mask_sf, sf::st_crs(cubo$crs[1]))
view(mask_sf)
mask_sf <- st_geometry(mask_sf)
plot(mask_sf)

# Salvar shp

sf::st_write(mask_sf, "mask_sf.shp")

mask_sf <- read_sf("mask_sf.shp")

plot(mask_sf)

# # Fazer o dissolve da máscara
# 
# sf::sf_use_s2(FALSE)  # evita enrosco do S2 durante a limpeza/dissolve
# 
# v <- terra::vect(mask_sf)
# v <- terra::makeValid(v)
# 
# # dissolve todas as feições (sem precisar de campo, funde tudo em uma só)
# 
# v <- terra::aggregate(v)
# 
# # limpar microproblemas
# v <- terra::buffer(v, 0)
# 
# # voltar para sf
# mask <- sf::st_as_sf(v) |>
#   sf::st_make_valid()
# 
# plot(mask)
# 
# sf::st_write(mask, "Prodes_2024/Mascaras/RM5/Mascara_corrigida/mask_rm5_final.shp")
# 
# #Ao abrir em nova sessão
# 
# mascara_rm5 <- sf::st_read("Prodes_2024/Mascaras/RM5/Mascara_corrigida/mask_rm5_final.shp")
# 
# plot(mascara_rm5)

# Produzir mapas de probabilidades por classes ------------------------------------------------------------------------------------------------------------

tempdir_r <- "mapa_prob_rm5_bandas_cloud"
dir.create(tempdir_r, showWarnings = FALSE, recursive = TRUE)

probs_class <- sits_classify(
  data = cubo, 
  ml_model = rf_model,
  exclusion_mask = mask_sf,
  multicores = 20,
  memsize = 85,
  output_dir = tempdir_r)

## Salvar dados dos mapas de probabilidades

saveRDS(probs_class, file = "probs_class.rds")
probs_class <- readRDS("probs_class.rds")

view(probs_class)
view(probs_class[1, ]) # Visualizar dados do primeiro tile
view(probs_class[1:2, ]) # Visualizar dados do primeiro e segundo tile

## Mapa

plot(probs_class[1, ])
plot(probs_class[2, ])
plot(probs_class[3, ])
plot(probs_class[4, ])
plot(probs_class[5, ])
plot(probs_class[6, ])
plot(probs_class[7, ])
plot(probs_class[8, ])

# Suavização dos mapas de probabilidades ------------------------------------------------------------------------------------------------------------------

tempdir_r <- "prob_suav_rm5_bandas_cloud"
dir.create(tempdir_r, showWarnings = FALSE, recursive = TRUE)

smooth_probs_rm5 <- sits_smooth(
  cube = probs_class,
  exclusion_mask = mask_sf,
  multicores = 20,
  memsize = 85,
  output_dir = tempdir_r)

plot(smooth_probs_rm5, labels = "supressao", palette = "YlOrBr")
plot(smooth_probs_rm5, labels = "veg_natural", palette = "Greens")
plot(smooth_probs_rm5, labels = "queimada", palette = "Reds")
plot(smooth_probs_rm5, labels = "aflor_rocha", palette = "Greys")
plot(smooth_probs_rm5, labels = "agua", palette = "Blues")

plot(smooth_probs_rm5[1,])
plot(smooth_probs_rm5[2,])
plot(smooth_probs_rm5[3,])
plot(smooth_probs_rm5[8,])

## Salvar dados do cubo suavizado

saveRDS(smooth_probs_rm5, file = "smooth_probs_rm5.rds")
smooth_probs_rm5 <- readRDS("smooth_probs_rm5.rds")

# Rotulando o cubo de probabilidades - Classificação do mapa final ----------------------------------------------------------------------------------------

tempdir_r <- "map_classificado_bandas_cloud"
dir.create(tempdir_r, showWarnings = FALSE, recursive = TRUE)

map_class <- sits_label_classification(
  cube = smooth_probs_rm5, # mosaico_probs
  output_dir = tempdir_r, 
  memsize = 85,
  multicores = 20)

## Salvar dados do cubo classificado

saveRDS(map_class, file = "map_class.rds")
map_class <- readRDS("map_class.rds")
view(map_class)
class(map_class)

## Visualizar mapa classificado

### Definir cores das classes

sits_colors_set(tibble(name = c("abiotico", "queimada", "supressao", 
                                "veg_natural", "agua"),
                       color = c("#1A1A1A", "#D60C00", "#FAE9A0",
                                 "#A6D96A", "#A1DDEF"))
)

plot(map_class[8,],
     legend_position = "outside",
     scale = 1.0)

# Unir tiles com sits_mosaic() ----------------------------------------------------------------------------------------------------------------------------

tempdir_r <- "mosaico_bandas_cloud"
dir.create(tempdir_r, showWarnings = FALSE, recursive = TRUE)

mosaico_probs <- sits_mosaic(map_class,
                             output_dir = tempdir_r,
                             multicores = 20, 
                             progress   = TRUE)

view(mosaico_probs)

## Salvar dados do mosaico de probabilidades

saveRDS(mosaico_probs, file = "mosaico_probs.rds")
mosaico_probs <- readRDS("mosaico_probs.rds")
view(mosaico_probs)
plot(mosaico_probs)

# Mapa de incerteza ---------------------------------------------------------------------------------------------------------------------------------------

tempdir_r <- "mapa_incerteza_bandas_cloud"
dir.create(tempdir_r, showWarnings = FALSE, recursive = TRUE)

map_incerteza <- sits_uncertainty(
  cube = mosaico_probs, # Arquivo do cubo de probabilidades com mosaico
  type = "margin",
  output_dir = tempdir_r,
  memsize = 75,
  multicores = 20,
  progress = TRUE)

## Visualizar mapa de incerteza

plot(map_incerteza, 
     legend_position = "outside") # rev = FALSE

# Adicionar máscara ao mapa de incerteza ------------------------------------------------------------------------------------------------------------------

## Ler raster do mapa de incerteza e mapa da mascara

mask_sf

mapa_incert_final <- rast("SENTINEL-2_MSI_MOSAIC_2024-01-01_2024-12-18_margin_v1.tif")
class(mapa_incert_final)
unique(values(mapa_incert_final))
unique(is.na(values(mapa_incert_final))) # Mapa não tem valor NA

# Definir maior valor de incerteza que é na área externa do mapa como NA

mapa_incert_final[mapa_incert_final == 10000] <- NA

library(tidyterra) # Pacote para inserir raster no ggplot2
library(RColorBrewer)

# Mapa ggplot2 da máscara

ggplot(mask_sf) +
  geom_sf(fill = "black", color = "black") 

# Mapa da máscara com mapa de incerteza

ggplot() +
  geom_spatraster(data = mapa_incert_final) +
  scale_fill_gradient(na.value = "white", 
                      low  = "#7fbc41",  # cor mais clara
                      high = "red"   # cor mais escura
  ) +
  geom_sf(data = mask_sf, fill = "black", color = "black") +
  theme_bw() +
  labs(title = "Mapa de Incerteza")

# Exemplo da documentação do pacote

# ggplot(cyl_sf) +
# geom_spatraster_rgb(data = tile) +
# geom_sf(aes(fill = iso2)) +
# coord_sf(crs = 3857) +
# scale_fill_viridis_d(alpha = 0.7)
