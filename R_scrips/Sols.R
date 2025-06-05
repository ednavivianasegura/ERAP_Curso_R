# Paquetes necesarios

# Función para instalar si no está instalado
instalar_si_no <- function(paquete) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    install.packages(paquete, repos = "https://cloud.r-project.org")
  }
  library(paquete, character.only = TRUE)
}

# Lista de paquetes a verificar
paquetes <- c("forcats","VIM","knitr","e1071","tidyr","dplyr","ggplot2","e1071", "visdat","reshape2", "janitor","mlbench")


# Instalar y cargar todos
invisible(lapply(paquetes, instalar_si_no))

# Instalar librería empire
remotes::install_github("davidbiol/empire")


# SOLUCIÓN COMPLETA DEL EJERCICIO FINAL

# 1. Cargar paquetes necesarios


# 2. Cargar base de datos
data(PimaIndiansDiabetes)
df <- as.data.frame(PimaIndiansDiabetes)

glimpse(df)

# 3. Limpieza de datos: reemplazamos los ceros por NA
df <- df %>%
  mutate(
    glucose = ifelse(glucose == 0, NA, glucose),
    pressure = ifelse(pressure == 0, NA, pressure),
    mass = ifelse(mass == 0, NA, mass)
  )

# 4. Imputación con ridge
(df_numerico <- df %>%
    select_if(is.numeric))

summary(df_numerico)

(df_imputado <- empire::estimate_ridge(data = df_numerico, diff = 10, ridge_alpha = 0)$new_data)

summary(df_imputado)

# 5. Filtrar mujeres embarazadas (al menos 1 embarazo)
glimpse(df_embarazadas <- df %>%
  filter(pregnant >= 1)) 
  
  
# 6. Crear una nueva variable estandarizada de BMI
df_embarazadas <- df_embarazadas %>%
  mutate(mass_z = scale(mass))

# 7. Estadística descriptiva de glucosa
df_embarazadas %>%
  summarise(
    media = mean(glucose, na.rm=T),
    mediana = median(glucose, na.rm=T),
    sd = sd(glucose, na.rm=T),
    min = min(glucose, na.rm=T),
    max = max(glucose, na.rm=T)
  )

# 8. Histograma de glucosa
ggplot(df_embarazadas, aes(x = glucose)) +
  geom_histogram(aes(y = ..density..), fill = "skyblue", bins = 30, color = "black") +
  stat_function(fun = dnorm,
                args = list(mean = mean(df_embarazadas$glucose),
                            sd = sd(df_embarazadas$glucose)),
                color = "red") +
  labs(title = "Distribución de glucosa en embarazadas")

# 9. Correlación entre glucosa y masa corporal
cor(df_embarazadas$glucose, df_embarazadas$mass, use = "complete.obs")

# 10. Tabla cruzada: diabetes vs tramos de edad
df_embarazadas <- df_embarazadas %>%
  mutate(grupo_edad = cut(age, breaks = c(20, 30, 40, 50, 60), right = FALSE))

table(df_embarazadas$diabetes, df_embarazadas$grupo_edad)


#11 Con la siguiente tabla de recomendaciones por grupo de edad y diagnóstico
# agrega a cada una de las pacientes con. más de un embarazo dicha recomendación
recomendaciones <- tibble::tibble(
  grupo_edad = c("[20,30)", "[30,40)", "[40,50)", "[50,60)", "[60,90)"),
  diabetes = c("pos", "neg", "pos", "neg", "pos"),
  recomendacion = c(
    "Revisión semestral",
    "Promover actividad física",
    "Controles nutricionales",
    "Revisión anual",
    "Seguimiento médico mensual"
  )
)

#union
df_embarazadas <- df_embarazadas %>%
  left_join(recomendaciones, by = c("grupo_edad", "diabetes"))

View(df_embarazadas)

