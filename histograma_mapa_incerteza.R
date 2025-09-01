
# Histograma do mapa de incerteza -------------------------------------------------------------------------------------------------------------------------
# Classificação RM 5 --------------------------------------------------------------------------------------------------------------------------------------
# Classificação com banda de nuvem, sem filtro e sem balanceamento ----------------------------------------------------------------------------------------

# Carregar pacotes -----------------------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(readxl)

# Carregar tabela com valores de incerteza ----------------------------------------------------------------------------------------------------------------

v_inc <- readxl::read_excel("valores_incerteza.xlsx")

view(v_inc)

v_inc <- v_inc |>
  janitor::clean_names()

# Plotar histograma com ggplot2

ggplot(df, aes(x = valor)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(title = "Histograma da Incerteza",
       x = "Incerteza (0–1)",
       y = "Frequência") +
  theme_minimal()
