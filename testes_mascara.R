cubo[1, ]

view(cubo[1,])
class(cubo)

mask_sf <- read_sf("033016.shp")

class(mask_sf)
view(mask_sf)
sf::st_geometry_type(mask_sf)

crs(cubo[1,]$crs)
crs(mask_sf)

mask_sf <- sf::st_transform(mask_sf, sf::st_crs(cubo[1,]$crs))
mask_sf <- sf::st_make_valid(mask_sf)


# ti <- ti |>
#   dplyr::select(-def_cloud)


probs_class27 <- sits_classify(
  data = cubo[1, ], 
  ml_model = rf_model,
  exclusion_mask = mask_sf,
  multicores = 20,
  memsize = 64,
  output_dir = tempdir_r,
  version = "v27")

plot(probs_class27, labels = "veg_natural")

smot <- sits_smooth(
  probs_class8,
  exclusion_mask = NULL,
  multicores = 20,
  memsize = 64,
  output_dir = tempdir_r,
  version = "vv7"
)

plot(smot)

# map_rst <- rast("SENTINEL-2_MSI_033016_2024-01-01_2024-12-18_probs_v8.tif")
# plot(map_rst)



# remotes::install_version("sits", version = "1.5.2")
# 
# getwd()
