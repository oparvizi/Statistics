Population Data
        ↓
Population Growth Forecast
        ↓
Healthcare Accessibility Analysis
        ↓
Healthcare Need Score
        ↓
DBSCAN (identify underserved regions)
        ↓
Weighted p-Median
        ↓
Optimal Clinic Locations
        ↓
Optimal Hospital Locations
        ↓
Map Visualization

Below is a single integrated R script that combines:
Population demand
Population growth prediction
Existing health facilities
Healthcare accessibility analysis
Healthcare need scoring
DBSCAN underserved-area detection
Weighted p-Median facility optimization
Automatic facility classification
Interactive Leaflet visualization

############################################################
# HEALTHCARE INFRASTRUCTURE OPTIMIZATION SYSTEM
# Population Forecast + Accessibility + DBSCAN +
# Weighted p-Median + Facility Classification
############################################################

# ----------------------------------------------------------
# INSTALL / LOAD PACKAGES
# ----------------------------------------------------------

packages <- c(
  "dplyr",
  "sf",
  "dbscan",
  "leaflet",
  "geosphere",
  "ompr",
  "ompr.roi",
  "ROI.plugin.glpk",
  "ggplot2"
)

new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]

if(length(new_packages))
  install.packages(new_packages)

lapply(packages, library, character.only = TRUE)

# ----------------------------------------------------------
# INPUT DATA
# Replace with real population dataset
# ----------------------------------------------------------

set.seed(123)

n <- 300

cities <- data.frame(
  id = 1:n,
  lon = runif(n,72,88),
  lat = runif(n,18,30),
  population = sample(
    1000:1000000,
    n,
    replace = TRUE
  )
)

# ----------------------------------------------------------
# POPULATION FORECAST
# ----------------------------------------------------------

growth_rate <- 0.02
forecast_years <- 10

cities$population_2035 <-
  cities$population *
  (1 + growth_rate)^forecast_years

# ----------------------------------------------------------
# EXISTING HEALTH FACILITIES
# ----------------------------------------------------------

existing_facilities <- cities %>%
  sample_n(20)

facility_coords <- existing_facilities %>%
  select(lon,lat)

# ----------------------------------------------------------
# ACCESSIBILITY ANALYSIS
# ----------------------------------------------------------

distance_to_health <- apply(
  distm(
    cities[,c("lon","lat")],
    facility_coords,
    fun = distHaversine
  ) / 1000,
  1,
  min
)

cities$nearest_health_km <-
  distance_to_health

# ----------------------------------------------------------
# HEALTHCARE NEED INDEX
# ----------------------------------------------------------

cities$health_need <-
  0.6 * as.numeric(scale(log(
    cities$population_2035 + 1
  ))) +
  0.4 * as.numeric(scale(
    cities$nearest_health_km
  ))

cities$health_need <-
  cities$health_need -
  min(cities$health_need)

# ----------------------------------------------------------
# FUTURE DEMAND WEIGHT
# ----------------------------------------------------------

cities$demand_weight <-
  cities$population_2035 *
  (1 + cities$health_need)

# ----------------------------------------------------------
# DBSCAN
# IDENTIFY UNDERSERVED REGIONS
# ----------------------------------------------------------

db <- dbscan(
  cities[,c("lon","lat")],
  eps = 0.8,
  minPts = 5
)

cities$dbscan_cluster <- db$cluster

# ----------------------------------------------------------
# DISTANCE MATRIX
# ----------------------------------------------------------

coords <- cities[,c("lon","lat")]

dist_mat <- distm(
  coords,
  coords,
  fun = distHaversine
) / 1000

# ----------------------------------------------------------
# P-MEDIAN OPTIMIZATION
# ----------------------------------------------------------

N <- nrow(cities)

new_facilities <- 10

model <- MIPModel() %>%

  add_variable(
    y[j],
    j = 1:N,
    type = "binary"
  ) %>%

  add_variable(
    x[i,j],
    i = 1:N,
    j = 1:N,
    type = "binary"
  ) %>%

  set_objective(

    sum_expr(
      cities$demand_weight[i] *
      dist_mat[i,j] *
      x[i,j],
      i = 1:N,
      j = 1:N
    ),

    "min"

  ) %>%

  add_constraint(
    sum_expr(
      x[i,j],
      j = 1:N
    ) == 1,
    i = 1:N
  ) %>%

  add_constraint(
    x[i,j] <= y[j],
    i = 1:N,
    j = 1:N
  ) %>%

  add_constraint(
    sum_expr(
      y[j],
      j = 1:N
    ) == new_facilities
  )

result <- solve_model(
  model,
  with_ROI(
    solver = "glpk"
  )
)

selected <- get_solution(
  result,
  y[j]
)

selected <- selected %>%
  filter(value > 0.9)

predicted_facilities <-
  cities[selected$j, ]

# ----------------------------------------------------------
# ASSIGN POPULATION TO FACILITY
# ----------------------------------------------------------

facility_dist <- distm(
  cities[,c("lon","lat")],
  predicted_facilities[,c("lon","lat")],
  fun = distHaversine
) / 1000

cities$facility_id <-
  apply(
    facility_dist,
    1,
    which.min
  )

# ----------------------------------------------------------
# FACILITY LOAD
# ----------------------------------------------------------

facility_load <- cities %>%
  group_by(facility_id) %>%
  summarise(
    served_population =
      sum(population_2035),
    avg_distance =
      mean(nearest_health_km)
  )

predicted_facilities$served_population <-
  facility_load$served_population

# ----------------------------------------------------------
# CLASSIFICATION
# ----------------------------------------------------------

predicted_facilities$type <-
  ifelse(
    predicted_facilities$served_population < 20000,
    "Primary Clinic",
    ifelse(
      predicted_facilities$served_population < 150000,
      "Community Hospital",
      "Regional Medical Center"
    )
  )

# ----------------------------------------------------------
# FUTURE HEALTHCARE GAP
# ----------------------------------------------------------

cities$future_gap <-
  cities$population_2035 /
  (cities$nearest_health_km + 1)

# ----------------------------------------------------------
# MAP VISUALIZATION
# ----------------------------------------------------------

pal <- colorFactor(
  rainbow(length(
    unique(cities$facility_id)
  )),
  cities$facility_id
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
      sqrt(population)/150,
    color =
      ~pal(facility_id),
    stroke = FALSE,
    fillOpacity = 0.6,
    popup =
      ~paste(
        "Population:",
        round(population),
        "<br>2035:",
        round(population_2035),
        "<br>Need:",
        round(health_need,2),
        "<br>Gap:",
        round(future_gap,1)
      )
  ) %>%

  addCircleMarkers(
    data = existing_facilities,
    lng = ~lon,
    lat = ~lat,
    radius = 8,
    color = "green",
    fillColor = "green",
    fillOpacity = 1,
    popup = "Existing Facility"
  ) %>%

  addCircleMarkers(
    data = predicted_facilities,
    lng = ~lon,
    lat = ~lat,
    radius = 12,
    color = "red",
    fillColor = "red",
    fillOpacity = 1,
    popup =
      ~paste(
        type,
        "<br>Served:",
        round(served_population)
      )
  )

# ----------------------------------------------------------
# SUMMARY
# ----------------------------------------------------------

cat("\n")
cat("=====================================\n")
cat("HEALTHCARE OPTIMIZATION SUMMARY\n")
cat("=====================================\n")
cat(
  "Population Points:",
  N,"\n"
)
cat(
  "Existing Facilities:",
  nrow(existing_facilities),"\n"
)
cat(
  "New Facilities:",
  nrow(predicted_facilities),"\n"
)
cat("\n")

print(
  predicted_facilities %>%
  select(
    lon,
    lat,
    type,
    served_population
  )
)

# ----------------------------------------------------------
# EXPORT RESULTS
# ----------------------------------------------------------

write.csv(
  predicted_facilities,
  "predicted_health_facilities.csv",
  row.names = FALSE
)

############################################################
# END
############################################################