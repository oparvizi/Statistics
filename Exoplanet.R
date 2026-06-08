import pandas as pd
import numpy as np
import requests

# =========================================================
# 1. LOAD REAL EXOPLANET DATA (NASA Exoplanet Archive)
# =========================================================
url = "https://exoplanetarchive.ipac.caltech.edu/TAP/sync?query=select+pl_name,pl_rade,pl_bmasse,pl_orbper,pl_eqt,st_teff,st_rad,st_mass+from+pscomppars&format=csv"

df = pd.read_csv(url)

# Clean data
df = df.dropna()

# =========================================================
# 2. CONSTANTS
# =========================================================
EARTH_RADIUS = 1
EARTH_MASS = 1
EARTH_TEMP = 288  # Kelvin

# =========================================================
# 3. HABITABLE ZONE ESTIMATION
# =========================================================
def habitable_zone(teff, radius):
    # simplified stellar luminosity approximation
    luminosity = (radius ** 2) * (teff / 5778) ** 4
    return luminosity

# =========================================================
# 4. TEMPERATURE ESTIMATE (simplified equilibrium model)
# =========================================================
def estimate_temp(st_teff, st_rad, distance_au):
    lum = (st_rad ** 2) * (st_teff / 5778) ** 4
    temp = 278 * (lum ** 0.25) / np.sqrt(distance_au)
    return temp

# =========================================================
# 5. EARTH SIMILARITY INDEX (ESI)
# =========================================================
def esi(radius, mass, temp):
    r = np.exp(-((radius - 1) / 0.57) ** 2)
    m = np.exp(-((mass - 1) / 1.5) ** 2)
    t = np.exp(-((temp - EARTH_TEMP) / 75) ** 2)
    return (r * m * t) ** (1/3)

# =========================================================
# 6. PROCESS DATA
# =========================================================
results = []

for _, row in df.iterrows():
    try:
        radius = row["pl_rade"]
        mass = row["pl_bmasse"]
        period = row["pl_orbper"]
        temp = row["pl_eqt"]
        star_temp = row["st_teff"]
        star_radius = row["st_rad"]

        # approximate orbital distance (AU)
        distance = (period / 365.25) ** (2/3) * (star_mass := row["st_mass"]) ** (1/3)

        # habitability score
        esi_score = esi(radius, mass, temp)

        # temperature check
        temp_score = np.exp(-abs(temp - EARTH_TEMP) / 100)

        # final habitability index
        hsi = (esi_score * 0.5) + (temp_score * 0.5)

        results.append({
            "planet": row["pl_name"],
            "ESI": esi_score,
            "TempScore": temp_score,
            "HabitabilityScore": hsi
        })

    except:
        continue

# =========================================================
# 7. RESULTS TABLE
# =========================================================
res = pd.DataFrame(results)
res = res.sort_values("HabitabilityScore", ascending=False)

print("\n🌍 TOP POTENTIALLY HABITABLE PLANETS:\n")
print(res.head(10))

# =========================================================
# 8. VISUALIZATION (optional simple plot)
# =========================================================
import matplotlib.pyplot as plt

plt.figure(figsize=(10,5))
plt.scatter(res["ESI"], res["HabitabilityScore"], alpha=0.6)
plt.xlabel("Earth Similarity Index (ESI)")
plt.ylabel("Habitability Score")
plt.title("Exoplanet Habitability Analysis")
plt.grid()
plt.show()