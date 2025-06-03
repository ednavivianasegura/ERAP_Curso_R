# Cargar paquetes necesarios
library(ggplot2)
library(dplyr)
library(nycflights13)
install.packages("maps")
library(maps)

# 1. Filtrar vuelos desde NYC en enero de 2013
vuelos_ny <- flights %>%
  filter(month == 1) %>%         # Filtrar solo vuelos de enero
  select(origin, dest) %>%       # Seleccionar columnas de origen y destino
  distinct()                     # Eliminar duplicados para tener rutas únicas

# 2. Obtener coordenadas de los aeropuertos de origen
origenes <- vuelos_ny %>%
  select(faa = origin) %>%       # Renombrar columna para hacer join
  distinct() %>%                 # Asegurar que no haya duplicados
  left_join(airports, by = "faa") %>%  # Unir con tabla de aeropuertos
  select(faa,                   # Seleccionar identificador del aeropuerto
         lon_origen = lon,     # Renombrar longitud del origen
         lat_origen = lat)     # Renombrar latitud del origen

# 2b. Obtener coordenadas de los aeropuertos de destino
destinos <- vuelos_ny %>%
  select(faa = dest) %>%        # Renombrar columna para hacer join
  distinct() %>%
  left_join(airports, by = "faa") %>%
  select(faa,                   # Seleccionar identificador del aeropuerto
         lon_dest = lon,       # Renombrar longitud del destino
         lat_dest = lat)       # Renombrar latitud del destino

# 2c. Unir coordenadas de origen y destino a cada ruta
rutas <- vuelos_ny %>%
  left_join(origenes, by = c("origin" = "faa")) %>%  # Añadir coordenadas de origen
  left_join(destinos, by = c("dest" = "faa"))        # Añadir coordenadas de destino

# 3. Crear mapa base de los Estados Unidos
usa <- map_data("state")  # Datos del contorno de los estados de EE.UU.

# 4. Dibujar el mapa con las rutas de vuelo
ggplot() +
  # Mapa base en gris claro, con bordes blancos
  geom_polygon(data = usa, aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white") +
  # Rutas de vuelo como líneas desde origen hasta destino
  geom_segment(data = rutas,
               aes(x = lon_origen, y = lat_origen,    # punto de inicio
                   xend = lon_dest, yend = lat_dest), # punto final
               color = "blue", alpha = 0.3) +         # líneas azules semitransparentes
  # Mantener proporción entre latitud y longitud
  coord_fixed(1.3) +
  # Títulos y etiquetas
  labs(title = "Rutas de aviones desde NYC - Enero 2013",
       subtitle = "Basado en el dataset nycflights13",
       x = "", y = "") +
  # Tema visual minimalista
  theme_minimal()

