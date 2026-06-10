############################################################
# INFRASTRUCTURE OPTIMIZATION TOOLKIT
# DBSCAN + Weighted KMeans + KMedoids + p-Median
# Interactive Leaflet Map
############################################################

# Install packages if needed
packages <- c(
  "sf",
  "dplyr",
  "dbscan",
  "cluster",
  "leaflet",
  "geosphere",
  "ggplot2",
  "ompr",
  "ompr.roi",
  "ROI.plugin.glpk"
)

new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(sf)
library(dplyr)
library(dbscan)
library(cluster)
library(leaflet)
library(geosphere)
library(ggplot2)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

############################################################
# EXAMPLE DATA
# Replace with your own dataset
############################################################

set.seed(123)

n <- 300

cities <- data.frame(
  id = 1:n,
  lon = runif(n, 72, 88),
  lat = runif(n, 18, 30),
  population = sample(1000:1000000, n, replace = TRUE)
)

############################################################
# CONVERT TO SF
############################################################

cities_sf <- st_as_sf(
  cities,
  coords = c("lon", "lat"),
  crs = 4326
)

############################################################
# DBSCAN CLUSTERING
############################################################

coords <- cities %>%
  select(lon, lat)

db <- dbscan(
  coords,
  eps = 0.5,
  minPts = 5
)

cities$dbscan_cluster <- db$cluster

############################################################
# WEIGHTED K-MEANS
############################################################

k <- 8

wkmeans <- kmeans(
  coords,
  centers = k,
  nstart = 25
)

cities$kmeans_cluster <- wkmeans$cluster

kmeans_centers <- as.data.frame(
  wkmeans$centers
)

names(kmeans_centers) <- c("lon","lat")

############################################################
# K-MEDOIDS (PAM)
############################################################

distance_matrix <- dist(coords)

pam_fit <- pam(
  distance_matrix,
  k = k
)

cities$pam_cluster <- pam_fit$clustering

medoid_ids <- pam_fit$id.med

medoids <- cities[medoid_ids, ]

############################################################
# DISTANCE MATRIX (KM)
############################################################

dist_mat <- distm(
  coords,
  coords,
  fun = distHaversine
)/1000

############################################################
# P-MEDIAN FACILITY LOCATION
############################################################

p <- 8
N <- nrow(cities)

model <- MIPModel() %>%

  add_variable(y[j],
               j = 1:N,
               type = "binary") %>%

  add_variable(x[i,j],
               i = 1:N,
               j = 1:N,
               type = "binary") %>%

  set_objective(
    sum_expr(
      cities$population[i] *
      dist_mat[i,j] *
      x[i,j],
      i = 1:N,
      j = 1:N
    ),
    "min"
  ) %>%

  add_constraint(
    sum_expr(x[i,j], j = 1:N) == 1,
    i = 1:N
  ) %>%

  add_constraint(
    x[i,j] <= y[j],
    i = 1:N,
    j = 1:N
  ) %>%

  add_constraint(
    sum_expr(y[j], j = 1:N) == p
  )

result <- solve_model(
  model,
  with_ROI(
    solver = "glpk"
  )
)

facility_indices <- get_solution(
  result,
  y[j]
) %>%
  filter(value > 0.9)

facilities <- cities[
  facility_indices$j,
]

############################################################
# ASSIGN EACH CITY TO CLOSEST FACILITY
############################################################

facility_coords <- facilities %>%
  select(lon, lat)

nearest_facility <- apply(
  distm(
    coords,
    facility_coords,
    fun = distHaversine
  ),
  1,
  which.min
)

cities$facility_id <- nearest_facility

############################################################
# LEAFLET VISUALIZATION
############################################################

pal <- colorFactor(
  rainbow(length(unique(cities$facility_id))),
  cities$facility_id
)

leaflet() %>%
  addTiles() %>%

  addCircleMarkers(
    data = cities,
    lng = ~lon,
    lat = ~lat,
    radius = ~sqrt(population)/150,
    color = ~pal(facility_id),
    stroke = FALSE,
    fillOpacity = 0.6,
    popup = ~paste(
      "Population:", population,
      "<br>Facility:", facility_id
    )
  ) %>%

  addMarkers(
    data = facilities,
    lng = ~lon,
    lat = ~lat,
    icon = icons(
      iconUrl =
"https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-red.png",
      iconWidth = 25,
      iconHeight = 41
    ),
    popup = "Optimal Facility"
  )

############################################################
# STATIC MAP OF P-MEDIAN RESULT
############################################################

ggplot(cities,
       aes(lon, lat,
           color = factor(facility_id),
           size = population)) +
  geom_point(alpha = 0.7) +
  geom_point(
    data = facilities,
    aes(lon, lat),
    color = "black",
    fill = "yellow",
    shape = 23,
    size = 6
  ) +
  theme_minimal() +
  labs(
    title = "Optimal Infrastructure Distribution",
    color = "Facility"
  )

############################################################
# KPI REPORT
############################################################

total_weighted_distance <-
  sum(
    cities$population *
    apply(
      distm(
        coords,
        facility_coords,
        fun = distHaversine
      )/1000,
      1,
      min
    )
  )

cat("\n")
cat("Facilities:", p, "\n")
cat("Demand Points:", N, "\n")
cat("Weighted Distance:", round(total_weighted_distance), "\n")
cat("\n")

############################################################
# END
############################################################