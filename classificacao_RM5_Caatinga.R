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
  tiles      = c("", "", ""), # Tiles/Regiões de ineteresse
  start_date = "0", # Data inicial 
  end_date   = "") # Data final 

## Verificar bandas, tempos e outras informações do cubo 

sits_bands(cubo)
sits_timeline(cubo)
view(cubo)
view(cubo$file_info)

## Salvar e ler cubo criado

saveRDS(cubo, file = "cubo.rds") 
cubo <- readRDS("cubo.rds")

# Ler arquivo .shp com amostras por classes ---------------------------------------------------------------------------------------------------------------

amostras_classes <- sf::read_sf("amostras_classes.shp")

# Adicionar amostras ao cubo de dados criado --------------------------------------------------------------------------------------------------------------

cubo_amostras <- sits_get_data(
  cubo_tile034018_entorno_g4_2b, # Cubo geral com bandas e índices
  samples = "amostras_classes.shp", # Arquivo shapefile do tile 034018
  label_attr = "", # Coluna que indica as classes das amostras (pontos)
  bands = c("", "", "", ""), 
  memsize = 8, # consumo de memória
  multicores = 2, # Número de núcleos usados. Quanto maior, mais rápido o processamento
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

## Gráfico

plot(padroes_tempo_amostras)

# Balanceamento de amostras -------------------------------------------------------------------------------------------------------------------------------

cubo_amostras_bal <- sits_reduce_imbalance(
  cubo_amostras,
  n_samples_over = 100, 
  n_samples_under = 100) 

## Verificar proporção e nº de amostras balanceadas e não balanceadas

summary(cubo_amostras) # Nº de amostras não balanceadas
summary(cubo_amostras_bal) # Nº amostras balanceadas

# Análise SOM ---------------------------------------------------------------------------------------------------------------------------------------------

## Definir cores das classes

sits_colors_set(tibble(
  name = c("supressao", "veg_natural", "", "","", "", "", ""),
  color = c("#bf812d", "#01665e", "", "", "", "", "", "")))

## Com balanceamento

som_cluster <- sits_som_map(
  data = cubo_amostras_bal, # SOM feito com grupo de amostras balanceadas (VERIFICAR!)
  grid_xdim = 10, # Grade eixo x. Aqui é 10 x 10 para gerar 100 neurônios
  grid_ydim = 10, # Grade eixo y
  distance = "dtw", # Método de calcular a distância,
  mode = "pbatch", # Gera o mesmo mapa SOM a cada run
  rlen = 20) # Número de iterações (quantidade de vezes que o mapa é gerado)

## Visualizar mapa SOM

windows(width = 9, height = 7)
plot(som_cluster, band = "DBSI") 
plot(som_cluster, band = "NDII")
plot(som_cluster, band = "B11")

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

plot(all_samples)
summary(all_samples) # Número de amostras (mesma quantidade das originais ou balanceadas)

# Remover amostras ruidosas -------------------------------------------------------------------------------------------------------------------------------

samples_clean <- sits_som_clean_samples(som_cluster,
                                        keep = c("clean", "analyze"))

## Visualizar gráfico

plot(samples_clean)
summary(samples_clean) # Número de amostras após filtro

# Ver diferenças na quantidade de amostras antes e após filtragem -----------------------------------------------------------------------------------------

summary(all_samples)
summary(samples_clean) 

# Gerar SOM dos dados sem ruídos --------------------------------------------------------------------------------------------------------------------------

som_cluster_limpo <- sits_som_map(
  data = samples_clean, # SOM feito com o nosso grupo de amostras 
  grid_xdim = 10, # Aqui é 10 x 10 para gerar 100 neurônios
  grid_ydim = 10,
  mode = "pbatch", # Gera o mesmo mapa SOM a cada run
  distance = "dtw", # Método para calcular a distância
  rlen = 20) # Número de iterações

## Visualizar mapa SOM limpo

windows(width = 9, height = 7)
plot(som_cluster_limpo, band = "DBSI")
plot(som_cluster_limpo, band = "NDVI")
plot(som_cluster_limpo, band = "B11")

# Avaliar matriz de confusão das amostras antes e após filtragem ------------------------------------------------------------------------------------------

## Função de avaliação

avaliacao_som <- sits_som_evaluate_cluster(som_cluster)
avaliacao_som_limpo <- sits_som_evaluate_cluster(som_cluster_limpo)

## Gráficos

plot(avaliacao_som)
plot(avaliacao_som_limpo)

## Resultados das avaliações

avaliacao_som 
avaliacao_som_limpo

# Classificações ------------------------------------------------------------------------------------------------------------------------------------------

## Leitura do cubo criado com bandas e índices

cubo_indices_bandas <- readRDS("cubo_indices_bandas.rds")

view(cubo_indices_bandas)

# Treinar modelo Random Forest ----------------------------------------------------------------------------------------------------------------------------

## Treinar modelo com amostras limpas (VERIFICAR AMOSTRAS LIMPAS OU NÃO)

set.seed(03024)

rf_model <- sits_train(
  samples = samples_clean, # Se precisar de amostras originais --> all_samples 
  ml_method = sits_rfor()) # Modelo Random Forest

## Gráfico com as variávies mais importantes do modelo

plot(rf_model)

# Validação do modelo Random Forest -----------------------------------------------------------------------------------------------------------------------

set.seed(333)

rfor_valid <- sits_kfold_validate(
  samples    = samples_clean,
  folds      = 5, 
  ml_method  = sits_rfor(),
  multicores = 5)

rfor_valid 

# Produzir mapas de probabilidades por classes ------------------------------------------------------------------------------------------------------------

tempdir_r <- "mapa_prob"
dir.create(tempdir_r, showWarnings = FALSE, recursive = TRUE)

probs_class <- sits_classify(
  data = cubo_indices_bandas, 
  ml_model = rf_model,
  multicores = 3,
  memsize = 8,
  output_dir = tempdir_r)

## Salvar dados dos mapas de probabilidades

saveRDS(probs_class, file = "probs_class.rds")
probs_class <- readRDS("probs_class.rds")

# Unir tiles com sits_mosaic() ----------------------------------------------------------------------------------------------------------------------------

tempdir_r <- "mosaico_probs"
dir.create(tempdir_r, showWarnings = FALSE, recursive = TRUE)

mosaico_probs <- sits_mosaic(probs_class,
                             output_dir = tempdir_r,
                             multicores = 7,
                             progress   = TRUE)

view(mosaico_probs)

## Visualizar tiles juntos em único mapa

plot(mosaico_probs)

## Salvar dados do mosaico de probabilidades

saveRDS(mosaico_probs, file = "mosaico_probs.rds")
mosaico_probs <- readRDS("mosaico_probs.rds")

# Suavização dos mapas de probabilidades ------------------------------------------------------------------------------------------------------------------

tempdir_r <- "mosaico_prob_suav"
dir.create(tempdir_r, showWarnings = FALSE, recursive = TRUE)

smooth_probs <- sits_smooth(
  cube = mosaico_probs,
  multicores = 7,
  memsize = 15,
  output_dir = tempdir_r)

plot(smooth_probs)

## Salvar dados do cubo suavizado

saveRDS(smooth_probs, file = "smooth_probs.rds")
smooth_probs <- readRDS("smooth_probs.rds")

# Rotulando o cubo de probabilidades - Classificação do mapa final ----------------------------------------------------------------------------------------

tempdir_r <- "map_classificado"
dir.create(tempdir_r, showWarnings = FALSE, recursive = TRUE)

map_class <- sits_label_classification(
  cube = smooth_probs, 
  output_dir = tempdir_r, 
  memsize = 15,
  multicores = 7)

## Salvar dados do cubo classificado

saveRDS(map_class, file = "map_class.rds")
map_class <- readRDS("map_class.rds")

## Visualizar mapa classificado

### Definir cores das classes

sits_colors_set(tibble(
  name = c("supressao", "veg_natural", "", "","", "", "", ""),
  color = c("#bf812d", "#01665e", "", "", "", "", "", "")))

plot(map_class)
class(map_class)

# Mapa de incerteza ---------------------------------------------------------------------------------------------------------------------------------------

tempdir_r <- "mapa_incerteza"
dir.create(tempdir_r, showWarnings = FALSE, recursive = TRUE)

map_incerteza <- sits_uncertainty(
  cube = mosaico_probs, # Arquivo do cubo de probabilidades com mosaico
  type = "margin",
  output_dir = tempdir_r,
  memsize = 12,
  multicores = 4,
  progress = TRUE)

## Visualizar mapa de incerteza

plot(map_incerteza) 

# Adicionar máscara com reclassificação do SITS -----------------------------------------------------------------------------------------------------------

tempdir_r <- "map_final_classificado"
dir.create(tempdir_r, showWarnings = FALSE)

## Gerar cubo do mapa classificado 

cubo_map_class <- sits_cube(
  source = "BDC",
  collection = "SENTINEL-2-16D",
  data_dir = tempdir_r, # A imagem classificada deve estar nesta pasta
  parse_info = c("satellite", "sensor", 
                 "tile", "start_date", "end_date",
                 "band", "version"),
  bands = "class",
  labels = c("1" = "supressao", # Definir os pixels da imagem
             "2" = "veg_natural"))

view(cubo_map_class)

## Visualizar mapa do cubo

plot(cubo_map_class)

## Gerar cubo da máscara PRODES

tempdir_r <- "cl_reclassification"
dir.create(tempdir_r, showWarnings = FALSE)

## A imagem em formato .tif da máscara deve estar na pasta 'cl_reclassification'
## e conter as informações citadas em "parse_info" do cubo abaixo:

masc_prodes <- sits_cube(
  source = "BDC",
  collection = "SENTINEL-2-16D",
  tiles      = "034018",
  data_dir = tempdir_r,
  parse_info = c("product", "sensor", 
                 "tile", "start_date", "end_date",
                 "band", "version"),
  bands = "class",
  version = "v22", # Versão do mapa PRODES para não confundir com mapa classificado
  labels = c("1" = "mascara", "2" = "NA")) # Verificar pixel da máscara, talvez reclassificar pixels

view(masc_prodes)

plot(masc_prodes)

## Junção mapa classificado com máscara PRODES - Reclassificação

tempdir_r <- "cl_reclassification1"
dir.create(tempdir_r, showWarnings = FALSE)

reclas_masc_map_class <- sits_reclassify(
  cube = cubo_map_class, # Cubo do mapa classificado
  mask = prodes_2020_2B, # Cubo da máscara PRODES
  rules = list("Mascara_PRODES_2000-2019" = mask == "mascara",
               "Supressao" = cube == "supressao",
               "Vegetacao_natural" = cube == "veg_natural"), # VERIFICAR
  multicores = 7,
  output_dir = tempdir_r,
  version = "reclass")

sits_colors_set(tibble(
  name = c("Mascara_PRODES", "Supressao","Vegetacao_natural","", "", "", "", ""),
  color = c("white", "#bf812d", "#01665e","", "", "", "", "")))

plot(reclas_masc_map_class,
     legend_text_size = 0.7, 
     legend_position = "outside",
     scale = 1.0)