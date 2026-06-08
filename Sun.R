library(rgee)

ee_Initialize()

# -----------------------------
# Area of interest
# -----------------------------
aoi <- ee$Geometry$Point(c(49.9, 40.4))$buffer(20000)

# -----------------------------
# Single satellite snapshot
# -----------------------------
img <- ee$ImageCollection("COPERNICUS/S2_SR")$
  filterBounds(aoi)$
  filterDate("2026-01-01", "2026-01-10")$
  sort("CLOUDY_PIXEL_PERCENTAGE")$
  first()$
  clip(aoi)

# -----------------------------
# SUN POSITION (metadata)
# -----------------------------
sun_azimuth <- img$get("MEAN_SOLAR_AZIMUTH_ANGLE")
sun_zenith  <- img$get("MEAN_SOLAR_ZENITH_ANGLE")

print("Sun Azimuth:")
print(sun_azimuth)

print("Sun Zenith:")
print(sun_zenith)

# -----------------------------
# 1. NDVI (vegetation / surface)
# -----------------------------
ndvi <- img$normalizedDifference(c("B8", "B4"))$rename("NDVI")

# -----------------------------
# 2. Edge detection (Sobel)
# -----------------------------
edges <- img$select("B4")$convolve(
  ee$Kernel$sobel()
)$rename("Edges")

# -----------------------------
# 3. Gaussian smoothing
# -----------------------------
smooth <- ndvi$convolve(
  ee$Kernel$gaussian(radius = 3, sigma = 1, units = "pixels")
)$rename("Smooth")

# -----------------------------
# 4. Contrast enhancement
# -----------------------------
contrast <- ndvi$unitScale(0, 1)$multiply(2)$subtract(1)

# -----------------------------
# 5. Illumination / shadow proxy
# -----------------------------
illumination <- img$select("B8")$convolve(
  ee$Kernel$sobel()
)$rename("Sun_Shadow_Gradient")

# -----------------------------
# VISUAL SETTINGS
# -----------------------------
ndvi_vis <- list(min = 0, max = 1, palette = c("blue", "white", "green"))
edge_vis <- list(min = 0, max = 0.4, palette = c("black", "white"))
smooth_vis <- list(min = 0, max = 1, palette = c("purple", "yellow", "green"))
contrast_vis <- list(min = -1, max = 1, palette = c("red", "white", "blue"))
illum_vis <- list(min = 0, max = 0.3, palette = c("black", "yellow", "white"))

# -----------------------------
# MAP OUTPUT
# -----------------------------
Map$centerObject(aoi, 10)

Map$addLayer(ndvi, ndvi_vis, "NDVI (Surface)")
Map$addLayer(smooth, smooth_vis, "Smoothed NDVI")
Map$addLayer(edges, edge_vis, "Edges (Structure)")
Map$addLayer(contrast, contrast_vis, "Contrast")
Map$addLayer(illumination, illum_vis, "Sun/Shadow Gradient")