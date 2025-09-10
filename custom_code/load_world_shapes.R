library(sf)
library(rnaturalearth)
library(rmapshaper)

# 1. Get country boundaries and simplify 
world <- ne_countries(scale = "medium", returnclass = "sf")[, c("admin", "iso_a3")]
world_simplified <- rmapshaper::ms_simplify(world, keep = 0.05, keep_shapes = TRUE)
saveRDS(world_simplified, "data/world_simple.rds")


