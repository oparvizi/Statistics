library(rgee)

ee_Initialize()

# =========================
# 1. AREA OF INTEREST
# =========================
aoi <- ee$Geometry$Point(c(49.9, 40.4))$buffer(20000)

# =========================
# 2. CLOUD MASK FUNCTION (Sentinel-2)
# =========================
maskS2 <- function(img) {
  qa <- img$select("QA60")

  cloud_mask <- qa$bitwiseAnd(1 << 10)$eq(0)$
    And(qa$bitwiseAnd(1 << 11)$eq(0))

  img$updateMask(cloud_mask)
}

# =========================
# 3. SENTINEL-2 COLLECTION (OPTICAL)
# =========================
s2 <- ee$ImageCollection("COPERNICUS/S2_SR")$
  filterBounds(aoi)$
  filterDate("2025-12-01", "2026-01-10")$
  map(maskS2)

# Baseline (historical mean)
baseline <- s2$mean()$clip(aoi)

# Latest image
latest <- s2$sort("system:time_start", FALSE)$first()$clip(aoi)

# =========================
# 4. SENTINEL-1 (RADAR - ALL WEATHER)
# =========================
s1 <- ee$ImageCollection("COPERNICUS/S1_GRD")$
  filterBounds(aoi)$
  filterDate("2025-12-01", "2026-01-10")$
  select("VV")

s1_base <- s1$mean()$clip(aoi)
s1_latest <- s1$sort("system:time_start", FALSE)$first()$clip(aoi)

# =========================
# 5. FEATURE ENGINEERING
# =========================

# NDVI
ndvi_latest <- latest$normalizedDifference(c("B8","B4"))
ndvi_base <- baseline$normalizedDifference(c("B8","B4"))

# Brightness
bright_latest <- latest$expression(
  "(B2+B3+B4)/3",
  list(B2=latest$select("B2"),
       B3=latest$select("B3"),
       B4=latest$select("B4"))
)

bright_base <- baseline$expression(
  "(B2+B3+B4)/3",
  list(B2=baseline$select("B2"),
       B3=baseline$select("B3"),
       B4=baseline$select("B4"))
)

# Radar backscatter change (Sentinel-1)
radar_change <- s1_latest$subtract(s1_base)

# =========================
# 6. CHANGE DETECTION
# =========================

ndvi_diff <- ndvi_latest$subtract(ndvi_base)
bright_diff <- bright_latest$subtract(bright_base)

# Edge detection (structure change)
edges <- latest$select("B4")$convolve(ee$Kernel$sobel())

# =========================
# 7. Z-SCORE ANOMALY SCORING
# =========================

zscore <- function(img) {
  mean <- img$reduceRegion(
    ee$Reducer$mean(), aoi, 1000
  )$getInfo()

  std <- img$reduceRegion(
    ee$Reducer$stdDev(), aoi, 1000
  )$getInfo()

  img$subtract(mean)$divide(std)
}

# Apply Z-score (simplified EE-safe version)
ndvi_z <- ndvi_diff$abs()
bright_z <- bright_diff$abs()
radar_z <- radar_change$abs()
edge_z <- edges$abs()

# =========================
# 8. FINAL ANOMALY SCORE
# =========================

anomaly <- ndvi_z$
  add(bright_z)$
  add(radar_z)$
  add(edge_z)$
  rename("ANOMALY_SCORE")

# =========================
# 9. VISUALIZATION
# =========================

ndvi_vis <- list(min=-1, max=1, palette=c("blue","white","green"))
radar_vis <- list(min=-2, max=2, palette=c("black","yellow","red"))
edge_vis <- list(min=0, max=0.5, palette=c("black","white"))
anom_vis <- list(min=0, max=5, palette=c("blue","yellow","red"))

# =========================
# 10. MAP OUTPUT
# =========================

Map$centerObject(aoi, 10)

Map$addLayer(ndvi_diff, ndvi_vis, "NDVI CHANGE")
Map$addLayer(radar_change, radar_vis, "RADAR CHANGE (S1)")
Map$addLayer(edges, edge_vis, "STRUCTURE CHANGE")
Map$addLayer(anomaly, anom_vis, "FINAL ANOMALY MAP")