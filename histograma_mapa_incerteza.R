# Histograma do mapa de incerteza -------------------------------------------------------------------------------------------------------------------------
# Classificação RM 5 --------------------------------------------------------------------------------------------------------------------------------------
# Classificação com banda de nuvem, sem filtro e sem balanceamento ----------------------------------------------------------------------------------------

# Carregar pacotes -----------------------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(readxl)
library(scales)

# Carregar tabela com valores de incerteza ----------------------------------------------------------------------------------------------------------------

v_inc <- readxl::read_excel("tabela_values_incert.xls")
v_inc1 <- readxl::read_excel("tab_mask2.xls")
v_inc2 <- readxl::read_excel("tab_mask3.xls")
v_inc3 <- readxl::read_excel("tab_mask4.xls")
v_inc4 <- readxl::read_excel("tab_mask5.xls")
v_inc5 <- readxl::read_excel("tab_mask6.xls")
v_inc6 <- readxl::read_excel("tab_mask7.xls")
v_inc7 <- readxl::read_excel("tab_mask8.xls")
v_inc_t <- readxl::read_excel("tab_mask_t.xls")

# v_inc <- v_inc |>
#   janitor::clean_names()

# Ajustar valores da tabela -------------------------------------------------------------------------------------------------------------------------------

# Exemplo: tabela freq com colunas 'value' (valores de pixel) e 'count'

# Defina número de classes (bins)
n_bins <- 60  

# Cria intervalos automáticos
v_inc_t$bin <- cut(v_inc_t$value, breaks = n_bins)

# Soma frequência por intervalo
freq_bins <- aggregate(count ~ bin, data = v_inc_t, sum)

# Gráfico de linhas com classes

ggplot(freq_bins, aes(x = bin, y = count, group = 1)) +
  geom_line(color = "darkblue", linewidth = 1.4) +
  geom_point(size = 2, color = "darkblue") +
  scale_y_continuous(
    labels = label_number(scale = 1e-6, suffix = "M", accuracy = 0.1)) +
  scale_x_discrete(breaks = levels(freq_bins$bin)[seq(1, length(levels(freq_bins$bin)), by = 5)]) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Valores de incerteza (intervalo)", y = "Frequência de pixels")

