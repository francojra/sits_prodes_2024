# Histograma do mapa de incerteza -------------------------------------------------------------------------------------------------------------------------
# Classificação RM 5 --------------------------------------------------------------------------------------------------------------------------------------
# Classificação com banda de nuvem, sem filtro e sem balanceamento ----------------------------------------------------------------------------------------

# Carregar pacotes -----------------------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(readxl)
library(scales)

# Carregar tabela com valores de incerteza ----------------------------------------------------------------------------------------------------------------

v_inc <- readxl::read_excel("valores_incerteza.xlsx")

view(v_inc)

v_inc <- v_inc |>
  janitor::clean_names()

# Plotar histograma com ggplot2

ggplot(v_inc, aes(x = valor, y = contagem_de_pixel)) +
  geom_col(col = "white", fill = "#016450") +
  scale_y_continuous(
    labels = label_number(scale = 1e-6, suffix = "M", accuracy = 0.1)) +
  labs(x = "Valores de incerteza", 
       y = "Número de pixels") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.03))) +
  theme_bw()
