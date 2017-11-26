library(tidyverse)
library(sf)

# Download d_sf from https://github.com/abjur/abjMaps
d_sf <- read_rds("d_sf.rds")
d_sf_min <- d_sf[-1, ]
write_rds(d_sf_min, "../data/d_sf_min.rds")

