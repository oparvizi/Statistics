# =========================================================
# 1. PACKAGES
# =========================================================
install.packages(c("tidyverse"))
library(tidyverse)

# =========================================================
# 2. LOAD REAL NASA EXOPLANET DATA
# =========================================================
url <- "https://exoplanetarchive.ipac.caltech.edu/TAP/sync?query=select+pl_name,pl_rade,pl_bmasse,pl_orbper,st_teff,st_rad,st_mass,pl_eqt+from+pscomppars&format=csv"

df <- read.csv(url)
df <- na.omit(df)

# =========================================================
# 3. CONSTANTS
# =========================================================
earth_temp <- 288
earth_flux <- 1  # Earth receives 1 unit solar flux

# =========================================================
# 4. PHYSICS: ORBITAL DISTANCE (AU)
# Kepler’s 3rd law approximation
# =========================================================
orbital_distance <- function(period, star_mass) {
  (period / 365.25)^(2/3) * (star_mass)^(1/3)
}

# =========================================================
# 5. STELLAR FLUX (INSOLATION)
# =========================================================
stellar_flux <- function(st_rad, st_teff, distance) {
  luminosity <- (st_rad^2) * (st_teff / 5778)^4
  luminosity / (distance^2)
}

# =========================================================
# 6. EQUILIBRIUM TEMPERATURE MODEL
# =========================================================
eq_temp <- function(star_flux) {
  278 * (star_flux)^(1/4)
}

# =========================================================
# 7. EARTH SIMILARITY INDEX (IMPROVED)
# =========================================================
esi <- function(radius, mass, temp) {

  r <- exp(-((radius - 1)/0.57)^2)
  m <- exp(-((mass - 1)/1.5)^2)
  t <- exp(-((temp - earth_temp)/75)^2)

  (r * m * t)^(1/3)
}

# =========================================================
# 8. HABITABLE ZONE SCORE
# (1 = ideal Earth-like flux)
# =========================================================
hz_score <- function(flux) {
  exp(-((flux - earth_flux)^2))
}

# =========================================================
# 9. PROCESS DATA
# =========================================================
results <- df %>%
  mutate(

    # orbital distance (AU)
    distance_au = orbital_distance(pl_orbper, st_mass),

    # stellar flux
    flux = stellar_flux(st_rad, st_teff, distance_au),

    # equilibrium temperature
    temp_model = eq_temp(flux),

    # Earth Similarity Index
    ESI = esi(pl_rade, pl_bmasse, pl_eqt),

    # habitability zone score
    HZ = hz_score(flux),

    # temperature suitability
    TempScore = exp(-abs(temp_model - earth_temp)/80),

    # gravity factor (simplified)
    GravityScore = exp(-abs(pl_bmasse/pl_rade - 1)/2),

    # FINAL HABITABILITY INDEX (weighted physics model)
    HabitabilityScore =
      (ESI * 0.25) +
      (HZ * 0.25) +
      (TempScore * 0.25) +
      (GravityScore * 0.25)
  ) %>%
  arrange(desc(HabitabilityScore))

# =========================================================
# 10. RESULTS
# =========================================================
cat("\n🌍 TOP POTENTIALLY HABITABLE PLANETS (IMPROVED MODEL):\n\n")

print(results %>%
        select(pl_name, ESI, HZ, TempScore, GravityScore, HabitabilityScore) %>%
        head(15))

# =========================================================
# 11. VISUALIZATION
# =========================================================
library(ggplot2)

ggplot(results, aes(x = HZ, y = HabitabilityScore)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  theme_minimal() +
  labs(
    title = "Exoplanet Habitability (Physics-Based Model)",
    x = "Habitable Zone Score",
    y = "Habitability Index"
  )