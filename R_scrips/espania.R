# Instalar paquetes si es necesario
#install.packages(c("rnaturalearth", "rnaturalearthdata", "sf", "dplyr", "ggplot2", "devtools"))
#devtools::install_github("ropensci/rnaturalearthhires")


# Cargar paquetes
library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# 1. Obtener mapa de España a nivel de regiones administrativas (comunidades autónomas)
# La función ne_states() descarga un mapa en formato sf desde Natural Earth
espana_mapa <- ne_states(country = "Spain", returnclass = "sf")

# 2. Crear una tabla con datos demográficos: población por comunidad autónoma
# Esta tabla se usará para unirse al mapa, por el nombre de la región (columna "name")
poblacion <- data.frame(
  name = c("Andalucía", "Aragón", "Asturias", "Cantabria", "Castilla y León",
           "Castilla-La Mancha", "Cataluña", "Comunidad Valenciana",
           "Extremadura", "Galicia", "Madrid", "Murcia",
           "Navarra", "País Vasco", "La Rioja", "Islas Baleares", "Islas Canarias", "Ceuta", "Melilla"),
  poblacion = c(8472407,	1326261,	1011792,	1173008,	2172944,	584507,	2383139,
                2049562,	7763362,	5058138,	1059501,	2695645,	6751251,	1518486,
                661537,	2213993,	319796,	83517,	86261)
)

# 3. Unir el mapa con la tabla de población
# La unión se hace por la columna "name", que identifica cada región en el mapa
mapa_completo <- espana_mapa %>%
  left_join(poblacion, by = "name")

# 4. Crear el gráfico con ggplot2
ggplot(data = mapa_completo) +
  # geom_sf dibuja los polígonos geográficos usando la geometría incluida en el objeto sf
  geom_sf(aes(fill = poblacion)) +
  # Usamos una escala de color continua "plasma" (de viridis) para representar la población
  scale_fill_viridis_c(option = "magma", direction = -1, name = "Población") +
  # Títulos y leyenda del gráfico
  labs(title = "Población por Comunidad Autónoma en España",
       subtitle = "Datos aproximados para el año 2021",
       fill = "Población") +
  # Tema minimalista para una apariencia limpia
  theme_minimal()
