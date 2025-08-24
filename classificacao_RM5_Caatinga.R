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

cubo <- sits_cube(
  source     = "BDC", # Fonte dos cubos de dados
  collection = "SENTINEL-2-16D", # Coleção de imagens
  tiles      = c("033016", "033018", "034016", "034017",
                 "034018", "035015", "035016", "035017"), # Tiles/Regiões de ineteresse
  start_date = "2024-01-01", # Data inicial 
  end_date   = "2024-12-31") # Data final 

## Verificar bandas, tempos e outras informações do cubo 

sits_bands(cubo)
sits_timeline(cubo)
view(cubo)
view(cubo$file_info)

## Salvar e ler cubo criado

saveRDS(cubo, file = "cubo.rds") 
cubo <- readRDS("cubo.rds")

# Ler arquivo .shp com amostras por classes ---------------------------------------------------------------------------------------------------------------

amostras_classes <- sf::read_sf("amostras_RM_5_caatinga.shp")
view(amostras_classes)

# Adicionar amostras ao cubo de dados criado --------------------------------------------------------------------------------------------------------------

cubo_amostras <- sits_get_data(
  cubo, # Cubo geral com bandas e índices
  samples = "amostras_RM_5_caatinga.shp", # Arquivo shapefile do tile 034018
  label_attr = "label", # Coluna que indica as classes das amostras (pontos)
  bands = c("B01",   "B02",   "B03",   "B04",   "B05",   
            "B06",   "B07",   "B08",   "B09",   "B11",   
            "B12", "B8A", "EVI", "NBR", "NDVI"), # Seleção de bandas e índices
  memsize = 64, # consumo de memória
  multicores = 20, # Número de núcleos usados. Quanto maior, mais rápido o processamento
  progress = TRUE) # Acompanhar carregamento

## Verificar informações do cubo com amostras

view(cubo_amostras)
sits_bands(cubo_amostras)
sits_labels(cubo_amostras)

## Salvar e ler cubo com amostras

saveRDS(cubo_amostras, file = "cubo_amostras.rds") 
cubo_amostras <- readRDS("cubo_amostras.rds")

# Visualizar padrões de séries temporais por classe -------------------------------------------------------------------------------------------------------

padroes_tempo_amostras <- sits_patterns(cubo_amostras) # Média harmônica das séries temporais 
view(padroes_tempo_amostras$time_series[[1]])
view(padroes_tempo_amostras)

## Gráfico

p <- plot(padroes_tempo_amostras)
p + theme_bw() +
  theme(legend.position = "right",
        axis.text = element_text(color = "black"))

# Balanceamento de amostras -------------------------------------------------------------------------------------------------------------------------------

# cubo_amostras_bal <- sits_reduce_imbalance(
#   cubo_amostras,
#   n_samples_over = 100,
#   n_samples_under = 100)
# 
# ## Verificar proporção e nº de amostras balanceadas e não balanceadas
# 
# summary(cubo_amostras) # Nº de amostras não balanceadas
# summary(cubo_amostras_bal) # Nº amostras balanceadas

# Análise SOM ---------------------------------------------------------------------------------------------------------------------------------------------

## Definir cores das classes

sits_colors_set(tibble(name = c("aflor_rocha", "queimada", "supressao", 
                                "veg_natural", "agua"),
                       color = c("#1A1A1A", "#D60C00", "#FAE9A0",
                                 "#A6D96A", "#A1DDEF"))
)

som_cluster <- sits_som_map(
  data = cubo_amostras, # SOM feito com grupo de amostras balanceadas (VERIFICAR!)
  grid_xdim = 12, # Grade eixo x. Aqui é 10 x 10 para gerar 100 neurônios
  grid_ydim = 12, # Grade eixo y
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

amostras_filt_neuro <- som_cluster$data[som_cluster$data$id_neuron == 25, ]
view(amostras_filt_neuro)

amostras_filt_neuro1 <- som_cluster$data[som_cluster$data$id_neuron == 2, ]
view(amostras_filt_neuro1)

amostras_filt_neuro2 <- som_cluster$data[som_cluster$data$id_neuron == 45, ]
view(amostras_filt_neuro2)

# Detectar ruídos das amostras ----------------------------------------------------------------------------------------------------------------------------

all_samples <- sits_som_clean_samples(som_map = som_cluster,
                                      keep = c("clean", "analyze", "remove"))

## Visualizar gráfico

p1 <- plot(all_samples)
p1 + theme_bw() +
  theme(axis.text = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "bottom")

summary(all_samples) # Número de amostras (mesma quantidade das originais ou balanceadas)

# Remover amostras ruidosas -------------------------------------------------------------------------------------------------------------------------------

samples_clean <- sits_som_clean_samples(som_cluster,
                                        keep = c("clean", "analyze"))

## Visualizar gráfico

p2 <- plot(samples_clean)
p2 + theme_bw() +
  theme(axis.text = element_text(color = "black"),
        axis.title = element_blank(),
        legend.position = "bottom")

summary(samples_clean) # Número de amostras após filtro

# Ver diferenças na quantidade de amostras antes e após filtragem -----------------------------------------------------------------------------------------

summary(all_samples)
summary(samples_clean)

# Gerar SOM dos dados sem ruídos --------------------------------------------------------------------------------------------------------------------------

# som_cluster_limpo <- sits_som_map(
#   data = samples_clean, # SOM feito com o nosso grupo de amostras 
#   grid_xdim = 10, # Aqui é 10 x 10 para gerar 100 neurônios
#   grid_ydim = 10,
#   mode = "pbatch", # Gera o mesmo mapa SOM a cada run
#   distance = "dtw", # Método para calcular a distância
#   rlen = 20) # Número de iterações
# 
# ## Visualizar mapa SOM limpo
# 
# windows(width = 9, height = 7)
# plot(som_cluster_limpo, band = "DBSI")
# plot(som_cluster_limpo, band = "NDVI")
# plot(som_cluster_limpo, band = "B11")

# Avaliar matriz de confusão das amostras antes e após filtragem ------------------------------------------------------------------------------------------

## Função de avaliação

avaliacao_som <- sits_som_evaluate_cluster(som_cluster)
# avaliacao_som_limpo <- sits_som_evaluate_cluster(som_cluster_limpo)

## Gráficos

p3 <- plot(avaliacao_som) 
p3 + theme(axis.text = element_text(color = "black"))

# plot(avaliacao_som_limpo)

## Resultados das avaliações

avaliacao_som 
# avaliacao_som_limpo

# Classificações ------------------------------------------------------------------------------------------------------------------------------------------

## Leitura do cubo criado com bandas e índices

cubo <- readRDS("cubo.rds")

view(cubo)

# Treinar modelo Random Forest ----------------------------------------------------------------------------------------------------------------------------

## Treinar modelo com amostras limpas (VERIFICAR AMOSTRAS LIMPAS OU NÃO)

set.seed(03024)

rf_model <- sits_train(
  samples = all_samples, # Se precisar de amostras originais --> all_samples 
  ml_method = sits_rfor()) # Modelo Random Forest

## Gráfico com as variávies mais importantes do modelo

plot(rf_model)

# Validação do modelo Random Forest -----------------------------------------------------------------------------------------------------------------------

set.seed(333)

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

mask_sf <- read_sf("PRODES_2000_2023_+_RESIDUOS_RM5.shp")

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

tempdir_r <- "mapa_prob_rm5"
dir.create(tempdir_r, showWarnings = FALSE, recursive = TRUE)

probs_class1 <- sits_classify(
  data = cubo, 
  ml_model = rf_model,
  exclusion_mask = mask_sf,
  multicores = 20,
  memsize = 70,
  output_dir = tempdir_r)

## Salvar dados dos mapas de probabilidades

saveRDS(probs_class1, file = "probs_class1.rds")
probs_class1 <- readRDS("probs_class1.rds")

view(probs_class1)
view(probs_class1[1, ]) # Visualizar dados do primeiro tile
view(probs_class1[1:2, ]) # Visualizar dados do primeiro e segundo tile

## Mapa

plot(probs_class1[1, ])
plot(probs_class1[2, ])
plot(probs_class1[3, ])
plot(probs_class1[4, ])
plot(probs_class1[5, ])
plot(probs_class1[6, ])
plot(probs_class1[7, ])
plot(probs_class1[8, ])

# Unir tiles com sits_mosaic() ----------------------------------------------------------------------------------------------------------------------------

tempdir_r <- "mosaico_probs1"
dir.create(tempdir_r, showWarnings = FALSE, recursive = TRUE)

mosaico_probs <- sits_mosaic(probs_class1,
                             output_dir = tempdir_r,
                             multicores = 30, 
                             progress   = TRUE)

view(mosaico_probs)

## Visualizar tiles juntos em único mapa

plot(mosaico_probs, labels = "agua", palette = "Blues")
plot(mosaico_probs, labels = "aflor_rocha", palette = "Greys")
plot(mosaico_probs, labels = "veg_natural", palette = "BuGn")
plot(mosaico_probs, labels = "supressao", palette = "YlOrBr")
plot(mosaico_probs, labels = "queimada", palette = "Reds")

## Salvar dados do mosaico de probabilidades

saveRDS(mosaico_probs, file = "mosaico_probs.rds")
mosaico_probs <- readRDS("mosaico_probs.rds")

# Suavização dos mapas de probabilidades ------------------------------------------------------------------------------------------------------------------

tempdir_r <- "mosaico_prob_suav_rm5"
dir.create(tempdir_r, showWarnings = FALSE, recursive = TRUE)

smooth_probs_rm5 <- sits_smooth(
  cube = mosaico_probs,
  multicores = 20,
  memsize = 70,
  output_dir = tempdir_r)

plot(smooth_probs_rm5)
plot(smooth_probs_rm5, labels = "supressao", palette = "YlOrBr")
plot(smooth_probs_rm5, labels = "veg_natural", palette = "Greens")
plot(smooth_probs_rm5, labels = "queimada", palette = "Reds")
plot(smooth_probs_rm5, labels = "aflor_rocha", palette = "Greys")
plot(smooth_probs_rm5, labels = "agua", palette = "Blues")

## Salvar dados do cubo suavizado

saveRDS(smooth_probs_rm5, file = "smooth_probs_rm5.rds")
smooth_probs_rm5 <- readRDS("smooth_probs_rm5.rds")

# Rotulando o cubo de probabilidades - Classificação do mapa final ----------------------------------------------------------------------------------------

tempdir_r <- "map_classificado"
dir.create(tempdir_r, showWarnings = FALSE, recursive = TRUE)

map_class <- sits_label_classification(
  cube = smooth_probs_rm5, # mosaico_probs
  output_dir = tempdir_r, 
  memsize = 64,
  multicores = 20)

## Salvar dados do cubo classificado

saveRDS(map_class, file = "map_class.rds")
map_class <- readRDS("map_class.rds")
view(map_class)
class(map_class)

map_class <- map_class %>%
  mutate(labels = map(labels, ~ {
    .x[.x == "aflor_rocha"] <- "abiotico"
    .x
  }))

## Visualizar mapa classificado

### Definir cores das classes

sits_colors_set(tibble(name = c("abiotico", "queimada", "supressao", 
                                "veg_natural", "agua"),
                       color = c("#1A1A1A", "#D60C00", "#FAE9A0",
                                 "#A6D96A", "#A1DDEF"))
)

plot(map_class,
     legend_position = "outside",
     scale = 1.0)

# Mapa de incerteza ---------------------------------------------------------------------------------------------------------------------------------------

tempdir_r <- "mapa_incerteza"
dir.create(tempdir_r, showWarnings = FALSE, recursive = TRUE)

map_incerteza <- sits_uncertainty(
  cube = mosaico_probs, # Arquivo do cubo de probabilidades com mosaico
  type = "margin",
  output_dir = tempdir_r,
  memsize = 64,
  multicores = 20,
  progress = TRUE)

## Visualizar mapa de incerteza

plot(map_incerteza, legend_position = "outside") 

# Classificação por tiles ---------------------------------------------------------------------

tempdir_r <- "mosaico_prob_suav1"
dir.create(tempdir_r, showWarnings = FALSE, recursive = TRUE)

smooth_probs1 <- sits_smooth(
  cube = probs_class,
  multicores = 20,
  memsize = 64,
  output_dir = tempdir_r)

plot(smooth_probs1)
