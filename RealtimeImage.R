# ============================
# SATELLITE AREA + TIME VIEWER
# NASA GIBS + Leaflet + Snapshot
# ============================

# Install packages (run once)
packages <- c("leaflet", "sf", "terra", "mapview", "htmlwidgets")
new_pkgs <- packages[!(packages %in% installed.packages()[, "Package"])]
if(length(new_pkgs)) install.packages(new_pkgs)

library(leaflet)
library(sf)
library(terra)
library(mapview)
library(htmlwidgets)

# ----------------------------
# 1. Define AREA (change this)
# ----------------------------
bbox <- st_bbox(c(
  xmin = 49.5, xmax = 50.5,
  ymin = 40.0, ymax = 41.0
), crs = st_crs(4326))

area <- st_as_sfc(bbox)

# ----------------------------
# 2. Define DATE (important)
# ----------------------------
date <- "2026-06-08"

gibs_base <- paste0(
  "https://gibs.earthdata.nasa.gov/wms/epsg4326/best/wms.cgi?TIME=",
  date
)

# ----------------------------
# 3. Satellite layers
# ----------------------------
true_color <- "MODIS_Terra_CorrectedReflectance_TrueColor"
ndvi_layer  <- "MODIS_Terra_NDVI"
thermal     <- "MODIS_Terra_Thermal_Anomalies"

# ----------------------------
# 4. Create interactive map
# ----------------------------
map <- leaflet() |>

  addTiles(group = "Base Map") |>

  addWMSTiles(
    baseUrl = gibs_base,
    layers = true_color,
    group = "True Color (Natural)"
  ) |>

  addWMSTiles(
    baseUrl = gibs_base,
    layers = ndvi_layer,
    group = "Vegetation (NDVI Filter)"
  ) |>

  addWMSTiles(
    baseUrl = gibs_base,
    layers = thermal,
    group = "Heat / Fire Detection"
  ) |>

  fitBounds(
    lng1 = bbox["xmin"], lat1 = bbox["ymin"],
    lng2 = bbox["xmax"], lat2 = bbox["ymax"]
  ) |>

  addLayersControl(
    baseGroups = c("Base Map"),
    overlayGroups = c(
      "True Color (Natural)",
      "Vegetation (NDVI Filter)",
      "Heat / Fire Detection"
    ),
    options = layersControlOptions(collapsed = FALSE)
  )

# ----------------------------
# 5. SHOW MAP
# ----------------------------
map

# ----------------------------
# 6. SAVE SNAPSHOT (HTML + PNG)
# ----------------------------
saveWidget(map, "satellite_map.html", selfcontained = TRUE)

mapshot(map, file = "satellite_snapshot.png")