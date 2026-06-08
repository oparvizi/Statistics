# -----------------------------
# Install packages (run once)
# -----------------------------
install.packages(c("rgee"))

library(rgee)

# -----------------------------
# Initialize Earth Engine
# -----------------------------
ee_Initialize()

# -----------------------------
# Area of interest (Azerbaijan example)
# -----------------------------
aoi <- ee$Geometry$Point(c(49.9, 40.4))$buffer(20000)

# -----------------------------
# Get ONE satellite image (single moment)
# -----------------------------
img <- ee$ImageCollection("COPERNICUS/S2_SR")$
  filterBounds(aoi)$
  filterDate("2026-01-01", "2026-01-10")$
  sort("CLOUDY_PIXEL_PERCENTAGE")$
  first()$
  clip(aoi)

# -----------------------------
# 1. NDVI (vegetation / surface structure)
# -----------------------------
ndvi <- img$normalizedDifference(c("B8", "B4"))$rename("NDVI")

# -----------------------------
# 2. Edge detection (Sobel-like gradient)
# -----------------------------
edges <- img$select("B4")$convolve(
  ee$Kernel$sobel()
)$rename("Edges")

# -----------------------------
# 3. Smoothing filter (reduce noise)
# -----------------------------
smooth <- ndvi$convolve(
  ee$Kernel$gaussian(radius = 3, sigma = 1, units = "pixels")
)$rename("Smooth_NDVI")

# -----------------------------
# 4. Contrast enhancement (stretch-like)
# -----------------------------
contrast <- ndvi$unitScale(0, 1)$multiply(2)$subtract(1)$rename("Contrast")

# -----------------------------
# Visualization settings
# -----------------------------
ndvi_vis <- list(min = 0, max = 1, palette = c("blue", "white", "green"))
edge_vis <- list(min = 0, max = 0.5, palette = c("black", "white"))
smooth_vis <- list(min = 0, max = 1, palette = c("purple", "yellow", "green"))
contrast_vis <- list(min = -1, max = 1, palette = c("red", "white", "blue"))

# -----------------------------
# Map output
# -----------------------------
Map$centerObject(aoi, 10)

Map$addLayer(ndvi, ndvi_vis, "NDVI (Vegetation)")
Map$addLayer(smooth, smooth_vis, "Smoothed NDVI")
Map$addLayer(edges, edge_vis, "Edge Detection")
Map$addLayer(contrast, contrast_vis, "Contrast Enhanced NDVI")