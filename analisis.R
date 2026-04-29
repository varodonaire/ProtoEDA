library(tidyverse)
library(readxl)
library(patchwork)

# Cargar y preparar datos ----
datos <- read_excel("Dudas(1-27).xlsx") |>
  select(
    sueno      = `¿Cuántas horas dormiste anoche?`,
    traslado   = `¿Cuánto tardas en llegar a la U? (En minutos)`,
    apps       = `¿Cuántas apps tienes instaladas en tu celular aprox?`,
    estatura   = `¿Cuál es tu estatura? (En centímetros)`,
    redes      = `¿Cuántas horas al día usas redes sociales en promedio?`,
    estudio    = `¿Cuántas horas a la semana en promedio estudias fuera de clases?`,
    transporte = `¿Cómo llegas a la U? (Último tramo)`,
    trabaja    = `¿Tienes trabajo remunerado además de estudiar?`
  ) |>
  mutate(trabaja = factor(trabaja), transporte = factor(transporte)) |>
  mutate(sueno = as.numeric(sueno), traslado=as.numeric(traslado), apps = as.numeric(apps), estatura = as.numeric(estatura), redes = as.numeric(redes), estudio=as.numeric(estudio))

# Helpers ----
# Calcula media y mediana para usar en geom_vline
marcas <- function(var) {
  datos |>
    summarise(
      media   = mean({{ var }}, na.rm = TRUE),
      mediana = median({{ var }}, na.rm = TRUE)
    )
}


# == 1. VARIABLES CATEGÓRICAS ================================================

# Frecuencia: medio de transporte
ggplot(datos, aes(x = fct_infreq(transporte), fill = transporte)) +
  geom_bar() +
  labs(title = "Medio de transporte", x = NULL, y = "Frecuencia") +
  theme_minimal() +
  theme(legend.position = "none")

# Frecuencia: trabaja
ggplot(datos, aes(x = trabaja, fill = trabaja)) +
  geom_bar() +
  labs(title = "¿Trabaja?", x = NULL, y = "Frecuencia") +
  theme_minimal() +
  theme(legend.position = "none")


# == 2. HISTOGRAMAS (sin marcas) =============================================

hist_simple <- function(var, etiqueta) {
  ggplot(datos, aes(x = {{ var }})) +
    geom_histogram(bins = 10, fill = "steelblue", color = "white") +
    labs(title = etiqueta, x = etiqueta, y = "Frecuencia") +
    theme_minimal()
}

datos <- datos |> filter(estatura>100)
ggplot(datos, aes(x=estatura))+
  geom_density() +
  theme_minimal()

mean(datos$traslado, na.rm=TRUE)

hist_simple(sueno,    "Horas de sueño")
hist_simple(traslado, "Traslado (min)")
hist_simple(apps,     "Apps instaladas")
hist_simple(estatura, "Estatura (cm)")
hist_simple(redes,    "Horas en redes")
hist_simple(estudio,  "Horas de estudio semanal")

# Todos juntos en una grilla
datos |>
  select(where(is.numeric)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "valor") |>
  ggplot(aes(x = valor)) +
  geom_histogram(bins = 10, fill = "steelblue", color = "white") +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Histogramas — todas las variables") +
  theme_minimal()


# == 3. HISTOGRAMAS (con media y mediana) ====================================

hist_marcas <- function(var, etiqueta) {
  m <- marcas({{ var }})
  ggplot(datos, aes(x = {{ var }})) +
    geom_histogram(bins = 10, fill = "steelblue", color = "white") +
    geom_vline(xintercept = m$media,   color = "red",    linewidth = 1, linetype = "solid") +
    geom_vline(xintercept = m$mediana, color = "orange", linewidth = 1, linetype = "dashed") +
    annotate("text", x = m$media,   y = Inf, label = "media",   color = "red",
             vjust = 2, hjust = -0.1, size = 3.5) +
    annotate("text", x = m$mediana, y = Inf, label = "mediana", color = "orange",
             vjust = 2, hjust = -0.1, size = 3.5) +
    labs(title = etiqueta, x = etiqueta, y = "Frecuencia") +
    theme_minimal()
}

hist_marcas(sueno,    "Horas de sueño")
hist_marcas(traslado, "Traslado (min)")
hist_marcas(apps,     "Apps instaladas")
hist_marcas(estatura, "Estatura (cm)")
hist_marcas(redes,    "Horas en redes")
hist_marcas(estudio,  "Horas de estudio semanal")


# == 4. DENSIDADES ===========================================================

# Sin marcas
densidad_simple <- function(var, etiqueta) {
  ggplot(datos, aes(x = {{ var }})) +
    geom_density(fill = "steelblue", alpha = 0.4) +
    labs(title = etiqueta, x = etiqueta, y = "Densidad") +
    theme_minimal()
}

# Con marcas
densidad_marcas <- function(var, etiqueta) {
  m <- marcas({{ var }})
  ggplot(datos, aes(x = {{ var }})) +
    geom_density(fill = "steelblue", alpha = 0.4) +
    geom_vline(xintercept = m$media,   color = "red",    linewidth = 1) +
    geom_vline(xintercept = m$mediana, color = "orange", linewidth = 1, linetype = "dashed") +
    labs(title = etiqueta, x = etiqueta, y = "Densidad") +
    theme_minimal()
}

densidad_simple(sueno,    "Horas de sueño")
densidad_marcas(sueno,    "Horas de sueño")
densidad_simple(traslado, "Traslado (min)")
densidad_marcas(traslado, "Traslado (min)")


# == 5. BOXPLOTS INDIVIDUALES ================================================

box_simple <- function(var, etiqueta) {
  ggplot(datos, aes(x = {{ var }})) +
    geom_boxplot(fill = "steelblue", width = 0.4) +
    labs(title = etiqueta, x = etiqueta) +
    theme_minimal() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
}

# Con media superpuesta (punto rojo)
box_con_media <- function(var, etiqueta) {
  m <- marcas({{ var }})
  ggplot(datos, aes(x = {{ var }})) +
    geom_boxplot(fill = "steelblue", width = 0.4) +
    annotate("point", y = 0, x = m$media, color = "red", size = 3, shape = 18) +
    labs(title = etiqueta, subtitle = "◆ media", x = etiqueta) +
    theme_minimal() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
}


box_simple(sueno,    "Horas de sueño")
box_con_media(sueno, "Horas de sueño")
box_simple(traslado, "Traslado (min)")
box_con_media(traslado, "Traslado (min)")

# Todos juntos
datos |>
  select(where(is.numeric)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "valor") |>
  ggplot(aes(x = variable, y = valor)) +
  geom_boxplot(fill = "steelblue") +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_blank())


# == 6. BOXPLOTS POR GRUPO ===================================================

# Sueño según trabaja o no
ggplot(datos, aes(x = trabaja, y = sueno, fill = trabaja)) +
  geom_boxplot() +
  labs(title = "Sueño según trabaja", x = NULL, y = "Horas de sueño") +
  theme_minimal() + theme(legend.position = "none")

# Estudio según trabaja o no
ggplot(datos, aes(x = trabaja, y = estudio, fill = trabaja)) +
  geom_boxplot() +
  labs(title = "Horas de estudio según trabaja", x = NULL, y = "Horas semanales") +
  theme_minimal() + theme(legend.position = "none")

# Traslado según transporte
ggplot(datos, aes(x = transporte, y = traslado, fill = transporte)) +
  geom_boxplot() +
  labs(title = "Traslado según medio de transporte", x = NULL, y = "Minutos") +
  theme_minimal() + theme(legend.position = "none")

# Redes según trabaja
ggplot(datos, aes(x = trabaja, y = redes, fill = trabaja)) +
  geom_boxplot() +
  labs(title = "Redes sociales según trabaja", x = NULL, y = "Horas diarias") +
  theme_minimal() + theme(legend.position = "none")


# == 7. COMPARACIÓN LADO A LADO (patchwork) ==================================
# Útil para mostrar en clase: mismo gráfico con y sin marcas

p1 <- hist_simple(sueno, "Sin marcas")
p2 <- hist_marcas(sueno, "Con media y mediana")
p1 + p2 + plot_annotation(title = "Horas de sueño")

p3 <- hist_simple(traslado, "Sin marcas")
p4 <- hist_marcas(traslado, "Con media y mediana")
p3 + p4 + plot_annotation(title = "Traslado (min)")


# == 8. SCATTERPLOTS =========================================================

# Redes vs sueño
ggplot(datos, aes(x = redes, y = sueno)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  labs(title = "Redes sociales vs sueño",
       x = "Horas en redes", y = "Horas de sueño") +
  theme_minimal()

# Estudio vs sueño
ggplot(datos, aes(x = estudio, y = sueno)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue") +
  labs(title = "Horas de estudio vs sueño",
       x = "Horas de estudio semanal", y = "Horas de sueño") +
  theme_minimal()

# Traslado vs estudio, coloreado por trabaja
ggplot(datos, aes(x = traslado, y = estudio, color = trabaja)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Traslado vs horas de estudio",
       x = "Traslado (min)", y = "Horas de estudio semanal",
       color = "¿Trabaja?") +
  theme_minimal()
