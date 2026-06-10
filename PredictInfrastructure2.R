###############################################################
# ADVANCED HEALTHCARE FACILITY LOCATION OPTIMIZATION
# Self-contained simulation example
#
# Features:
# - Simulated population centers
# - Future population growth
# - Elderly population weighting
# - Disease burden weighting
# - Existing hospitals
# - Healthcare accessibility analysis
# - DBSCAN hotspot detection
# - Weighted p-median optimization
# - Facility classification
# - Interactive Leaflet visualization
###############################################################

# ============================================================
# INSTALL PACKAGES
# ============================================================

pkgs <- c(
  "dplyr",
  "sf",
  "dbscan",
  "leaflet",
  "geosphere",
  "ggplot2",
  "cluster"
)

new_pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]

if(length(new_pkgs))
  install.packages(new_pkgs)

lapply(pkgs, library, character.only = TRUE)

# ============================================================
# SIMULATE POPULATION CENTERS
# ============================================================

set.seed(123)

n <- 500

cities <- data.frame(
  id = 1:n,

  lon = c(
    rnorm(200, 49.85, 0.4),
    rnorm(150, 48.8, 0.5),
    rnorm(150, 47.5, 0.6)
  ),

  lat = c(
    rnorm(200, 40.40, 0.3),
    rnorm(150, 40.7, 0.4),
    rnorm(150, 39.9, 0.5)
  ),

  population = sample(
    1000:250000,
    n,
    replace = TRUE
  )
)

# ============================================================
# DEMOGRAPHICS
# ============================================================

cities$elderly_pct <- runif(n,5,35)

cities$chronic_disease_pct <- runif(n,2,25)

cities$poverty_pct <- runif(n,1,30)

# ============================================================
# FUTURE POPULATION FORECAST
# ============================================================

annual_growth <- runif(n,0.005,0.035)

forecast_years <- 15

cities$population_future <-
  cities$population *
  (1 + annual_growth)^forecast_years

# ============================================================
# HEALTHCARE DEMAND MODEL
# ============================================================

cities$age_factor <-
  1 + cities$elderly_pct / 100

cities$disease_factor <-
  1 + cities$chronic_disease_pct / 100

cities$poverty_factor <-
  1 + cities$poverty_pct / 100

cities$healthcare_demand <-
  cities$population_future *
  cities$age_factor *
  cities$disease_factor *
  cities$poverty_factor

# ============================================================
# EXISTING HEALTH FACILITIES
# ============================================================

existing_hospitals <- cities %>%
  sample_n(20)

# ============================================================
# ACCESSIBILITY ANALYSIS
# ============================================================

hospital_coords <- existing_hospitals[,c("lon","lat")]

distance_matrix_existing <- distm(
  cities[,c("lon","lat")],
  hospital_coords,
  fun = distHaversine
)/1000

cities$nearest_hospital_km <-
  apply(
    distance_matrix_existing,
    1,
    min
  )

# ============================================================
# HEALTHCARE NEED INDEX
# ============================================================

cities$need_index <-
  scale(log(cities$healthcare_demand + 1)) * 0.7 +
  scale(cities$nearest_hospital_km) * 0.3

cities$need_index <-
  as.numeric(cities$need_index)

# ============================================================
# DBSCAN HOTSPOT DETECTION
# ============================================================

db <- dbscan(
  cities[,c("lon","lat")],
  eps = 0.25,
  minPts = 8
)

cities$dbscan_cluster <- db$cluster

# ============================================================
# CANDIDATE DEMAND POINTS
# ============================================================

candidate_points <- cities %>%
  arrange(desc(need_index)) %>%
  slice(1:150)

# ============================================================
# WEIGHTED K-MEDOIDS
# (Approximation of p-median)
# ============================================================

k <- 12

pam_fit <- pam(
  candidate_points[,c("lon","lat")],
  k = k
)

medoid_ids <- pam_fit$id.med

new_facilities <-
  candidate_points[medoid_ids,]

# ============================================================
# ASSIGN POPULATION TO FACILITIES
# ============================================================

facility_distance <- distm(
  cities[,c("lon","lat")],
  new_facilities[,c("lon","lat")],
  fun = distHaversine
)/1000

cities$facility_id <-
  apply(
    facility_distance,
    1,
    which.min
  )

cities$travel_km_new <-
  apply(
    facility_distance,
    1,
    min
  )

# ============================================================
# FACILITY LOAD
# ============================================================

facility_load <- cities %>%
  group_by(facility_id) %>%
  summarise(
    served_population =
      sum(population_future),

    demand =
      sum(healthcare_demand),

    avg_distance =
      mean(travel_km_new)
  )

new_facilities$served_population <-
  facility_load$served_population

new_facilities$demand <-
  facility_load$demand

# ============================================================
# FACILITY CLASSIFICATION
# ============================================================

new_facilities$type <-
  ifelse(
    new_facilities$served_population < 50000,
    "Primary Clinic",

    ifelse(
      new_facilities$served_population < 250000,
      "Community Hospital",

      ifelse(
        new_facilities$served_population < 750000,
        "Regional Hospital",
        "Medical Center"
      )
    )
  )

# ============================================================
# PERFORMANCE METRICS
# ============================================================

coverage_15km <-
  mean(cities$travel_km_new <= 15) * 100

coverage_30km <-
  mean(cities$travel_km_new <= 30) * 100

coverage_60km <-
  mean(cities$travel_km_new <= 60) * 100

# ============================================================
# RESULTS TABLE
# ============================================================

results <- new_facilities %>%
  select(
    lon,
    lat,
    type,
    served_population,
    demand
  )

print(results)

# ============================================================
# LEAFLET VISUALIZATION
# ============================================================

pal <- colorFactor(
  c(
    "green",
    "blue",
    "orange",
    "red"
  ),
  domain = c(
    "Primary Clinic",
    "Community Hospital",
    "Regional Hospital",
    "Medical Center"
  )
)

leaflet() %>%

  addProviderTiles(
    providers$CartoDB.Positron
  ) %>%

  addCircleMarkers(
    data = cities,
    lng = ~lon,
    lat = ~lat,
    radius =
      sqrt(population)/120,
    stroke = FALSE,
    fillOpacity = 0.55,
    color = "gray",
    popup = ~paste(
      "Population:",
      round(population),
      "<br>Future:",
      round(population_future),
      "<br>Need:",
      round(need_index,2),
      "<br>Distance:",
      round(travel_km_new,1)," km"
    )
  ) %>%

  addCircleMarkers(
    data = existing_hospitals,
    lng = ~lon,
    lat = ~lat,
    radius = 7,
    color = "green",
    fillColor = "green",
    fillOpacity = 1,
    popup = "Existing Hospital"
  ) %>%

  addCircleMarkers(
    data = new_facilities,
    lng = ~lon,
    lat = ~lat,
    radius = 12,
    color = ~pal(type),
    fillColor = ~pal(type),
    fillOpacity = 1,
    popup = ~paste(
      "<b>",type,"</b>",
      "<br>Served:",
      round(served_population),
      "<br>Demand:",
      round(demand)
    )
  )

# ============================================================
# SUMMARY
# ============================================================

cat("\n")
cat("=========================================\n")
cat("HEALTHCARE PLANNING SUMMARY\n")
cat("=========================================\n")
cat("Population Centers :", nrow(cities), "\n")
cat("Existing Hospitals :", nrow(existing_hospitals), "\n")
cat("New Facilities     :", nrow(new_facilities), "\n")
cat("Coverage <=15 km   :", round(coverage_15km,1), "%\n")
cat("Coverage <=30 km   :", round(coverage_30km,1), "%\n")
cat("Coverage <=60 km   :", round(coverage_60km,1), "%\n")
cat("=========================================\n")

# ============================================================
# EXPORT
# ============================================================

write.csv(
  results,
  "optimized_healthcare_facilities.csv",
  row.names = FALSE
)