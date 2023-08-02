install.packages("learnr") # solo una vez#·install.packages("devtools") # solo una vez
devtools::install_github("dgonxalex80/paqueteMET")
library(paqueteMET)
data(vivienda_faltantes)


# instalar paquetes
library(RColorBrewer)
library(ggplot2)
# Crear una tabla de frecuencias de la variable "zona"
tabla_zonas <- table(vivienda_faltantes$zona)
# Calcular el porcentaje de cada categoría
porcentaje <- prop.table(tabla_zonas) * 100

# Crear el dataframe para el gráfico
df_zonas <- data.frame(zona = names(tabla_zonas),
                       frecuencia_absoluta = tabla_zonas,
                       porcentaje = porcentaje)

# Paleta de colores tipo pastel
colores_pastel <- brewer.pal(length(df_zonas$zona), "Pastel1")

# Crear el gráfico de pastel con colores tipo pastel
ggplot(data = df_zonas, aes(x = "", y = porcentaje, fill = zona)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste(round(porcentaje, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Distribución de inmuebles de acuerdo a la zona",
       x = NULL, y = NULL) +
  scale_fill_manual(values = colores_pastel) +
  theme_minimal()

png("Distribución de inmuebles de acuerdo a la zona.png")
ggplot(data = df_zonas, aes(x = "", y = porcentaje, fill = zona)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste(round(porcentaje, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Distribución de inmuebles de acuerdo a la zona",
       x = NULL, y = NULL) +
  scale_fill_manual(values = colores_pastel) +
  theme_minimal()
dev.off()

# Función para estandarizar los valores en la columna "tipo"
estandarizar_tipo <- function(x) {
  x <- gsub("(?i)APARTAMENTO", "Apartamento", x, perl = TRUE)  # (?i) ignora mayúsculas/minúsculas
  x <- gsub("(?i)apto", "Apartamento", x, perl = TRUE)
  return(x)
}

# Aplicar la función a la columna "tipo" del data frame
vivienda_faltantes$tipo <- estandarizar_tipo(vivienda_faltantes$tipo)

# Resultado
print(vivienda_faltantes)

# Función para estandarizar los valores en la columna "tipo"
estandarizar_tipo <- function(x) {
  x <- gsub("(?i)APARTAMENTO", "Apartamento", x, perl = TRUE)  # (?i) ignora mayúsculas/minúsculas
  x <- gsub("(?i)apto", "Apartamento", x, perl = TRUE)
  x <- gsub("(?i)CASA", "Casa", x, perl = TRUE)
  return(x)
}

# Aplicar la función a la columna "tipo" del data frame
vivienda_faltantes$tipo <- estandarizar_tipo(vivienda_faltantes$tipo)

print(vivienda_faltantes)

# Crear una tabla de frecuencias de la variable "tipo"
tabla_tipos <- table(vivienda_faltantes$tipo)

# Calcular el porcentaje de cada categoría
porcentaje <- prop.table(tabla_tipos) * 100

# Crear el dataframe para el gráfico
df_tipos <- data.frame(tipo = names(tabla_tipos),
                       frecuencia_absoluta = tabla_tipos,
                       porcentaje = porcentaje)

# Paleta de colores tipo pastel
colores_pastel <- brewer.pal(length(df_tipos$tipo), "Pastel1")

# Crear el gráfico de pastel con colores tipo pastel
ggplot(data = df_tipos, aes(x = "", y = porcentaje, fill = tipo)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste(round(porcentaje, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Distribución del tipo de inmueble",
       x = NULL, y = NULL) +
  scale_fill_manual(values = colores_pastel) +
  theme_minimal()

png("Distribución del tipo de inmueble.png")
ggplot(data = df_tipos, aes(x = "", y = porcentaje, fill = tipo)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste(round(porcentaje, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Distribución del tipo de inmueble",
       x = NULL, y = NULL) +
  scale_fill_manual(values = colores_pastel) +
  theme_minimal()
dev.off()

# Convertir la variable "estrato" en factor y especificar el orden
vivienda_faltantes$estrato <- factor(vivienda_faltantes$estrato, levels = c(3, 4, 5, 6))

# Eliminar las filas con valores NA en la variable "estrato"
vivienda_faltantes <- vivienda_faltantes[!is.na(vivienda_faltantes$estrato), ]

# Describir las frecuencias de cada estrato
summary(vivienda_faltantes$estrato)
# Crear el gráfico de barras
ggplot(vivienda_faltantes, aes(x = estrato, fill = estrato)) +
  geom_bar() +
  labs(title = "Distribución de viviendas de acuerdo al estrato",
       x = "Estrato", y = "Frecuencia") +
  theme_minimal()

png("Distribución de viviendas de acuerdo al estrato.png")

ggplot(vivienda_faltantes, aes(x = estrato, fill = estrato)) +
  geom_bar() +
  labs(title = "Distribución de viviendas de acuerdo al estrato",
       x = "Estrato", y = "Frecuencia") +
  theme_minimal()

dev.off()

# Calcula el promedio del precio
promedio_precio <- mean(vivienda_faltantes$preciom, na.rm = TRUE)

# Crea el gráfico de caja
ggplot(data = vivienda_faltantes, aes(x = "", y = preciom)) +
  geom_boxplot(color = "#8D80AD", fill = "#F9EBB2") +
  geom_hline(yintercept = quantile(vivienda_faltantes$preciom, 0.25, na.rm = TRUE), linetype = "dashed", color = "#5D6161") +
  geom_hline(yintercept = median(vivienda_faltantes$preciom, na.rm = TRUE), linetype = "dotted", color = "#5D6161") +
  geom_hline(yintercept = quantile(vivienda_faltantes$preciom, 0.75, na.rm = TRUE), linetype = "dashed", color = "#5D6161") +
  geom_hline(yintercept = promedio_precio, linetype = "solid", color = "red", size = 1) +
  labs(title = "Media, mediana y cuartiles del precio en millones",
       y = "Precio (m)",
       x = "",
       subtitle = paste0("Q1: ", round(quantile(vivienda_faltantes$preciom, 0.25, na.rm = TRUE), 2), ", ",
                         "Mediana: ", round(median(vivienda_faltantes$preciom, na.rm = TRUE), 2), ", ",
                         "Q3: ", round(quantile(vivienda_faltantes$preciom, 0.75, na.rm = TRUE), 2), ", ",
                         "Promedio: ", round(promedio_precio, 2))) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank())
png("Media, mediana y cuartiles del precio en millones.png")

ggplot(data = vivienda_faltantes, aes(x = "", y = preciom)) +
  geom_boxplot(color = "#8D80AD", fill = "#F9EBB2") +
  geom_hline(yintercept = quantile(vivienda_faltantes$preciom, 0.25, na.rm = TRUE), linetype = "dashed", color = "#5D6161") +
  geom_hline(yintercept = median(vivienda_faltantes$preciom, na.rm = TRUE), linetype = "dotted", color = "#5D6161") +
  geom_hline(yintercept = quantile(vivienda_faltantes$preciom, 0.75, na.rm = TRUE), linetype = "dashed", color = "#5D6161") +
  geom_hline(yintercept = promedio_precio, linetype = "solid", color = "red", size = 1) +
  labs(title = "Media, mediana y cuartiles del precio en millones",
       y = "Precio (m)",
       x = "",
       subtitle = paste0("Q1: ", round(quantile(vivienda_faltantes$preciom, 0.25, na.rm = TRUE), 2), ", ",
                         "Mediana: ", round(median(vivienda_faltantes$preciom, na.rm = TRUE), 2), ", ",
                         "Q3: ", round(quantile(vivienda_faltantes$preciom, 0.75, na.rm = TRUE), 2), ", ",
                         "Promedio: ", round(promedio_precio, 2))) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank())

dev.off()

# Construir el gráfico lineal entre las variables "preciom" y "areaconst"
ggplot(data = vivienda_faltantes, aes(x = preciom, y = areaconst)) +
  geom_point(color = "#8D80AD", alpha = 0.7) +
  geom_smooth(method = "lm", color = "#FF5733", se = FALSE) +
  labs(title = "Gráfico Lineal de 'preciom' y 'areaconst'",
       x = "Precio (m)",
       y = "Área Construida (m²)") +
  theme_minimal()
# Construir el gráfico lineal entre las variables "preciom" y "areaconst"
ggplot(data = vivienda_faltantes, aes(x = preciom, y = areaconst)) +
  geom_point(color = "#8D80AD", alpha = 0.7) +
  geom_smooth(method = "lm", color = "#FF5733", se = FALSE) +
  labs(title = "Relación entre área construida y precio",
       x = "Precio (m)",
       y = "Área Construida (m²)") +
  theme_minimal()

png("Relación entre área construida y precio.png")
ggplot(data = vivienda_faltantes, aes(x = preciom, y = areaconst)) +
  geom_point(color = "#8D80AD", alpha = 0.7) +
  geom_smooth(method = "lm", color = "#FF5733", se = FALSE) +
  labs(title = "Relación entre área construida y precio",
       x = "Precio (m)",
       y = "Área Construida (m²)") +
  theme_minimal()
dev.off()


# Suponiendo que tienes el dataframe "vivienda_faltantes" con las variables "preciom" y "habitac".
# Convertir la variable "habitac" en un factor y definir el orden de los niveles
vivienda_faltantes$habitac <- factor(vivienda_faltantes$habitac, levels = 1:10)

# Filtrar filas con valores no NA en las columnas "preciom" y "habitac"
vivienda_filtrada <- vivienda_faltantes[complete.cases(vivienda_faltantes[c("preciom", "habitac")]), ]

# Cargar la librería necesaria
library(ggplot2)

# Construir el gráfico de barras para relacionar "preciom" y "habitac"
ggplot(data = vivienda_filtrada, aes(x = habitac, y = preciom)) +
  geom_bar(stat = "summary", fun = "mean", fill = "#8D80AD") +
  labs(title = "Relación entre número de habitaciones y precio",
       x = "Número de Habitaciones",
       y = "Precio Promedio (m)") +
  theme_minimal()

png("Relación entre número de habitaciones y precio.png")

ggplot(data = vivienda_filtrada, aes(x = habitac, y = preciom)) +
  geom_bar(stat = "summary", fun = "mean", fill = "#8D80AD") +
  labs(title = "Relación entre número de habitaciones y precio",
       x = "Número de Habitaciones",
       y = "Precio Promedio (m)") +
  theme_minimal()

dev.off()

# Crear la variable ordinal "parquea_ordinal" a partir de la variable continua "parquea"
vivienda_faltantes$parquea_ordinal <- cut(vivienda_faltantes$parquea,
                                          breaks = c(-Inf, 3, 6, 9, Inf),
                                          labels = c("menos de 3", "3 - 6", "7 a 9", ">9"),
                                          right = FALSE)

# Convertir "parquea_ordinal" en un factor para que sea ordinal
vivienda_faltantes$parquea_ordinal <- factor(vivienda_faltantes$parquea_ordinal,
                                             levels = c("menos de 3", "3 - 6", "7 a 9", ">9"))

# Cargar la librería necesaria
library(ggplot2)

# Filtrar filas con valores no NA en las columnas "parquea_ordinal" y "preciom"
vivienda_filtrada <- vivienda_faltantes[complete.cases(vivienda_faltantes[c("parquea_ordinal", "preciom")]), ]
# Construir el gráfico de barras para relacionar "parquea_ordinal" y "preciom"
ggplot(data = vivienda_filtrada, aes(x = parquea_ordinal, y = preciom)) +
  geom_bar(stat = "summary", fun = "mean", fill = "#8D80AD") +
  labs(title = "Relación entre número de parqueaderos y precio",
       x = "Parqueaderos",
       y = "Precio Promedio (m)") +
  theme_minimal()

png("Relación entre número de parqueaderos y precio.png")

ggplot(data = vivienda_filtrada, aes(x = parquea_ordinal, y = preciom)) +
  geom_bar(stat = "summary", fun = "mean", fill = "#8D80AD") +
  labs(title = "Relación entre número de parqueaderos y precio",
       x = "Parqueaderos",
       y = "Precio Promedio (m)") +
  theme_minimal()
dev.off()

# Cargar las librerías necesarias
library(dplyr)
library(knitr)
library(kableExtra)

# Suponiendo que tienes el dataframe "vivienda_faltantes" con las variables "zona", "preciom", "areaconst", "parquea", "banios" y "habitac".

# Convertir la columna "habitac" a numérica
vivienda_faltantes$habitac <- as.numeric(vivienda_faltantes$habitac)

# Calcular el promedio y la desviación estándar utilizando dplyr::group_by() y dplyr::summarise()
resultados_promedio_sd <- vivienda_faltantes %>%
  group_by(zona) %>%
  summarise(
    "Promedio precio" = mean(preciom, na.rm = TRUE),
    "DE precio" = sd(preciom, na.rm = TRUE),
    "Promedio área construida" = mean(areaconst, na.rm = TRUE),
    "DE área construida" = sd(areaconst, na.rm = TRUE),
    "Promedio número de parqueaderos" = mean(parquea, na.rm = TRUE),
    "DE número de parqueaderos" = sd(parquea, na.rm = TRUE),
    "Promedio número de baños" = mean(banios, na.rm = TRUE),
    "DE número de baños" = sd(banios, na.rm = TRUE),
    "Promedio número de habitaciones" = mean(habitac, na.rm = TRUE),
    "DE número de habitaciones" = sd(habitac, na.rm = TRUE)
  )

# Formatear la tabla utilizando knitr::kable() y kableExtra
tabla_formateada <- kable(resultados_promedio_sd, format = "html", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# Mostrar la tabla formateada
print(tabla_formateada)

# Función para estandarizar los valores en la columna "tipo"
estandarizar_tipo <- function(x) {
  x <- gsub("(?i)APARTAMENTO", "Apartamento", x, perl = TRUE)  # (?i) ignora mayúsculas/minúsculas
  x <- gsub("(?i)apto", "Apartamento", x, perl = TRUE)
  return(x)
}

# Aplicar la función a la columna "tipo" del data frame
vivienda_faltantes$tipo <- estandarizar_tipo(vivienda_faltantes$tipo)

# Resultado
print(vivienda_faltantes)

# Función para estandarizar los valores en la columna "tipo"
estandarizar_tipo <- function(x) {
  x <- gsub("(?i)APARTAMENTO", "Apartamento", x, perl = TRUE)  # (?i) ignora mayúsculas/minúsculas
  x <- gsub("(?i)apto", "Apartamento", x, perl = TRUE)
  x <- gsub("(?i)CASA", "Casa", x, perl = TRUE)
  return(x)
}

# Aplicar la función a la columna "tipo" del data frame
vivienda_faltantes$tipo <- estandarizar_tipo(vivienda_faltantes$tipo)

# Resultado
print(vivienda_faltantes)

# Suponiendo que tienes el dataframe "vivienda_faltantes" con las variables "tipo", "preciom", "areaconst", "parquea", "banios" y "habitac".

# Calcular el promedio y la desviación estándar para cada grupo de la variable nominal "tipo"
resultados_promedio_sd <- vivienda_faltantes %>%
  group_by(tipo) %>%
  summarise(
    "Promedio precio" = mean(preciom, na.rm = TRUE),
    "DE precio" = sd(preciom, na.rm = TRUE),
    "Promedio área construida" = mean(areaconst, na.rm = TRUE),
    "DE área construida" = sd(areaconst, na.rm = TRUE),
    "Promedio número de parqueaderos" = mean(parquea, na.rm = TRUE),
    "DE número de parqueaderos" = sd(parquea, na.rm = TRUE),
    "Promedio número de baños" = mean(banios, na.rm = TRUE),
    "DE número de baños" = sd(banios, na.rm = TRUE),
    "Promedio número de habitaciones" = mean(habitac, na.rm = TRUE),
    "DE número de habitaciones" = sd(habitac, na.rm = TRUE)
  )

# Formatear la tabla utilizando knitr::kable() y kableExtra
tabla_formateada <- kable(resultados_promedio_sd, format = "html", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# Mostrar la tabla formateada
print(tabla_formateada)



# Calcular el promedio y la desviación estándar para cada grupo de la variable ordinal "estrato"
resultados_promedio_sd_estrato <- vivienda_faltantes %>%
  group_by(estrato) %>%
  summarise(
    "Promedio precio" = mean(preciom, na.rm = TRUE),
    "DE precio" = sd(preciom, na.rm = TRUE),
    "Promedio área construida" = mean(areaconst, na.rm = TRUE),
    "DE área construida" = sd(areaconst, na.rm = TRUE),
    "Promedio número de parqueaderos" = mean(parquea, na.rm = TRUE),
    "DE número de parqueaderos" = sd(parquea, na.rm = TRUE),
    "Promedio número de baños" = mean(banios, na.rm = TRUE),
    "DE número de baños" = sd(banios, na.rm = TRUE),
    "Promedio número de habitaciones" = mean(habitac, na.rm = TRUE),
    "DE número de habitaciones" = sd(habitac, na.rm = TRUE)
  )

# Formatear la tabla utilizando knitr::kable() y kableExtra
tabla_formateada_estrato <- kable(resultados_promedio_sd_estrato, format = "html", align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# Mostrar la tabla formateada
print(tabla_formateada_estrato)

