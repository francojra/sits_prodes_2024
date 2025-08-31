library(terra)
library(ggplot2)

# Carregar o raster

incerteza <- rast("SENTINEL-2_MSI_MOSAIC_2020-01-01_2020-12-18_margin_v1.tif")

values(incerteza)

# Normalizar (0 a 1)

incerteza_norm <- incerteza / 10000

# Extrair valores em um dataframe

df <- data.frame(valor = values(incerteza_norm))

# Remover NA

df <- na.omit(df)

# Plotar histograma com ggplot2

ggplot(df, aes(x = valor)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(title = "Histograma da Incerteza",
       x = "Incerteza (0–1)",
       y = "Frequência") +
  theme_minimal()
