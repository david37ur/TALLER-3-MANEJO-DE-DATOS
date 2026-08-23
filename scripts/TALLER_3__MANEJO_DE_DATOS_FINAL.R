# ==============================================================================
# SCRIPT FINAL: TALLER 3 - HACIENDO ECONOMÍA
# Tareas 1-4 (limpieza/cruce) + Tarea 5 (bases finales + VISUALIZACIÓN)
#
# AUTORES:
# - David Alejandro Suárez Escorcia
# - Joan Santiago Cortés Gomez
# - Santiago Gomez Ibague
#
# NOTA DE REPRODUCIBILIDAD:
# Reemplace las rutas de la Sección 0 con la ubicación de sus archivos.
#
# NOTA SOBRE TerriData_Dim2.xlsx:
# Este libro de Excel tiene ~4.1 millones de filas (proyecciones de
# población 2018-2070 para todos los municipios de Colombia) y cada hoja
# pesa más de 600 MB al descomprimirse. Leerlo completo con
# readxl::read_excel() agota la memoria RAM de un computador estándar.
# Por eso se hizo una extracción "streaming" previa
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. PREPARACIÓN DEL ENTORNO Y CARGA DE DATOS
# ------------------------------------------------------------------------------
# INSTRUCCIÓN: Si no tiene alguna de estas librerías, instálelas primero con:
# install.packages(c("dplyr","tidyr","haven","readxl","ggplot2","scales","ggrepel","forcats"))

# Fijamos el locale a UTF-8 para que tildes y eñes se guarden bien en los PNG
# (en Windows use "Spanish_Colombia.utf8"; en Mac/Linux "es_CO.UTF-8" o "C.utf8")
try(Sys.setlocale("LC_ALL", "Spanish_Colombia.utf8"), silent = TRUE)

library(dplyr)
library(tidyr)
library(haven)
library(readxl)
library(ggplot2)
library(scales)
library(ggrepel)
library(forcats)

# INSTRUCCIÓN: Reemplace estas rutas por la ubicación de sus archivos.
RUTA_TENDEROS   <- "C:/Users/pc/Downloads/Taller Manejo De Datos/Data/TenderosFU03_Publica.dta"
RUTA_POBLACION  <- "C:/Users/pc/Downloads/Taller Manejo De Datos/Data/poblacion_terridata_2022.csv"
RUTA_GRAFICOS   <- "C:/Users/pc/Downloads/Taller Manejo De Datos/graficos"                       # carpeta de salida

dir.create(RUTA_GRAFICOS, showWarnings = FALSE)

TenderosFU03_Publica <- read_dta(RUTA_TENDEROS)

# ------------------------------------------------------------------------------
# PALETA DE COLOR Y TEMA GRÁFICO (consistente en toda la presentación)
# ------------------------------------------------------------------------------
COLOR_PRIMARIO   <- "#1B3A4B"  # azul petróleo (dominante)
COLOR_SECUNDARIO <- "#3E92CC"  # azul medio (apoyo)
COLOR_ACENTO     <- "#F2542D"  # naranja/coral (para resaltar UN elemento)
COLOR_GRIS       <- "#B8C4CC"  # gris azulado (elementos no protagonistas)

tema_taller <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#0D1B24", margin = margin(b = 4)),
    plot.subtitle = element_text(size = 11.5, color = "#4A5A63", margin = margin(b = 12)),
    plot.caption = element_text(size = 8.5, color = "#8A97A0", hjust = 0, margin = margin(t = 10)),
    axis.title = element_text(size = 10.5, color = "#4A5A63"),
    axis.text = element_text(size = 10, color = "#4A5A63"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#E7ECEF", linewidth = 0.4),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    legend.title = element_text(size = 10, face = "bold"),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )
theme_set(tema_taller)


# ------------------------------------------------------------------------------
# TAREA 1: PENETRACIÓN DE INTERNET POR CIUDAD
# ------------------------------------------------------------------------------
base_ciudad <- TenderosFU03_Publica %>%
  group_by(Munic_Dept) %>%
  summarise(penetracion_internet = mean(uso_internet, na.rm = TRUE), .groups = "drop")

print("Resultado Tarea 1: Penetración por ciudad")
print(head(base_ciudad))


# ------------------------------------------------------------------------------
# TAREA 2: PENETRACIÓN DE INTERNET POR SECTOR COMERCIAL
# ------------------------------------------------------------------------------
# Diccionario con el nombre real de cada sector (etiquetas del cuestionario)
etiquetas_actividad <- c(
  actG1 = "Tienda", actG2 = "Comida preparada", actG3 = "Peluquería y belleza",
  actG4 = "Ropa", actG5 = "Otras variedades", actG6 = "Papelería y comunicaciones",
  actG7 = "Vida nocturna", actG8 = "Productos bajo inventario", actG9 = "Salud",
  actG10 = "Servicios", actG11 = "Ferretería y afines"
)

base_actividad <- TenderosFU03_Publica %>%
  pivot_longer(cols = actG1:actG11, names_to = "Actividad", values_to = "Realiza_Actividad") %>%
  filter(Realiza_Actividad == 1) %>%
  group_by(Actividad) %>%
  summarise(penetracion_internet = mean(uso_internet, na.rm = TRUE),
            n_negocios = n(), .groups = "drop") %>%
  mutate(Actividad_label = recode(Actividad, !!!etiquetas_actividad)) %>%
  arrange(desc(penetracion_internet))

print("Resultado Tarea 2: Penetración por actividad comercial")
print(head(base_actividad))


# ------------------------------------------------------------------------------
# TAREA 3: SECTORES QUE MÁS USAN INTERNET POR CIUDAD
# ------------------------------------------------------------------------------
base_actividad_ciudad <- TenderosFU03_Publica %>%
  pivot_longer(cols = actG1:actG11, names_to = "Actividad", values_to = "Realiza_Actividad") %>%
  filter(Realiza_Actividad == 1) %>%
  group_by(Munic_Dept, Actividad) %>%
  summarise(penetracion_internet = mean(uso_internet, na.rm = TRUE), .groups = "drop") %>%
  mutate(Actividad_label = recode(Actividad, !!!etiquetas_actividad)) %>%
  arrange(Munic_Dept, desc(penetracion_internet))

print("Resultado Tarea 3: Penetración por actividad cruzada con ciudad")
print(head(base_actividad_ciudad))


# ------------------------------------------------------------------------------
# TAREA 4: RELACIÓN CON LA POBLACIÓN DE LA CIUDAD
# ------------------------------------------------------------------------------
base_poblacion_limpia <- read.csv(RUTA_POBLACION, encoding = "UTF-8") %>%
  transmute(
    Codigo_Entidad = as.numeric(Codigo_Entidad),
    Entidad,
    poblacion_total
  )
# ---------------------------------------------------------------------------

base_final <- base_ciudad %>%
  left_join(base_poblacion_limpia, by = c("Munic_Dept" = "Codigo_Entidad"))

print("Resultado Tarea 4: Penetración de internet vs Población Municipal")
print(head(base_final))


# ------------------------------------------------------------------------------
# TAREA 5: BASES DE DATOS PARA ANÁLISIS ESTADÍSTICO Y VISUALIZACIÓN
# ------------------------------------------------------------------------------

# --- 5A. BASE LARGA (para PowerBI) ---
base_larga <- base_actividad_ciudad %>%
  left_join(base_poblacion_limpia, by = c("Munic_Dept" = "Codigo_Entidad")) %>%
  select(
    divipola = Munic_Dept,
    municipio = Entidad,
    Actividad = Actividad_label,
    penetracion_internet,
    poblacion_total
  )

print("Resultado Tarea 5A: Base Larga (para PowerBI)")
print(head(base_larga))
write.csv(base_larga, file.path(RUTA_GRAFICOS, "base_larga.csv"), row.names = FALSE)

# --- 5B. BASE EXTENSA (para gráficos de dispersión) ---
base_extensa <- base_larga %>%
  pivot_wider(
    names_from = Actividad,
    values_from = penetracion_internet,
    names_prefix = "internet_"
  )

print("Resultado Tarea 5B: Base Extensa (para gráficos de dispersión)")
print(head(base_extensa))
write.csv(base_extensa, file.path(RUTA_GRAFICOS, "base_extensa.csv"), row.names = FALSE)


# ==============================================================================
# VISUALIZACIÓN — PUNTO 5
# Cada gráfico transmite UN solo mensaje, siguiendo las reglas vistas en clase:
# tipo de gráfico según el objetivo, colores con jerarquía, etiquetado directo,
# eliminación de "chart-junk" y foco en el hallazgo, no en la decoración.
# ==============================================================================

# ------------------------------------------------------------------------------
# GRÁFICO 1 — Barras horizontales: ¿qué sectores usan más internet?
# Mensaje: hay una brecha clara entre sectores "digitalizados" y rezagados.
# ------------------------------------------------------------------------------
g1_datos <- base_actividad %>%
  mutate(
    resaltar = Actividad_label == Actividad_label[which.max(penetracion_internet)],
    Actividad_label = fct_reorder(Actividad_label, penetracion_internet)
  )

g1 <- ggplot(g1_datos, aes(x = penetracion_internet, y = Actividad_label, fill = resaltar)) +
  geom_col(width = 0.68) +
  geom_text(aes(label = percent(penetracion_internet, accuracy = 1)),
            hjust = -0.15, size = 3.7, color = "#0D1B24", fontface = "bold") +
  scale_fill_manual(values = c(`TRUE` = COLOR_ACENTO, `FALSE` = COLOR_PRIMARIO), guide = "none") +
  scale_x_continuous(labels = percent, limits = c(0, max(g1_datos$penetracion_internet) * 1.18),
                      expand = expansion(mult = c(0, 0))) +
  labs(
    title = "Papelería y comunicaciones lidera el uso de internet",
    subtitle = "Penetración de internet por sector comercial de los tenderos encuestados",
    x = NULL, y = NULL,
    caption = "Fuente: Encuesta a Tenderos FU03 (2022). Elaboración propia."
  )

ggsave(file.path(RUTA_GRAFICOS, "01_penetracion_por_sector.png"), g1,
       width = 9, height = 5.5, dpi = 300, bg = "white", type = "cairo")


# ------------------------------------------------------------------------------
# GRÁFICO 2 — Dispersión: penetración de internet vs. población municipal
# Mensaje: las ciudades más grandes no necesariamente tienen mayor penetración;
# la relación es débil y hay outliers relevantes.
# ------------------------------------------------------------------------------
g2_datos <- base_final %>%
  filter(!is.na(poblacion_total), poblacion_total > 0) %>%
  mutate(destacar = Entidad %in% c("Bogotá", "Barranquilla", "Bucaramanga", "Soacha"))

g2 <- ggplot(g2_datos, aes(x = poblacion_total, y = penetracion_internet)) +
  geom_smooth(method = "lm", se = TRUE, color = COLOR_SECUNDARIO,
              fill = COLOR_SECUNDARIO, alpha = 0.12, linewidth = 0.8, linetype = "dashed") +
  geom_point(aes(size = poblacion_total), color = COLOR_PRIMARIO, alpha = 0.55) +
  geom_point(data = filter(g2_datos, destacar),
             aes(size = poblacion_total), color = COLOR_ACENTO, alpha = 0.9) +
  geom_text_repel(data = filter(g2_datos, destacar), aes(label = Entidad),
                   size = 3.6, fontface = "bold", color = "#0D1B24",
                   min.segment.length = 0, seed = 1, box.padding = 0.6) +
  scale_x_log10(labels = label_number(scale_cut = cut_short_scale())) +
  scale_y_continuous(labels = percent) +
  scale_size_continuous(range = c(1.5, 11), guide = "none") +
  labs(
    title = "El tamaño de la ciudad no garantiza mayor uso de internet",
    subtitle = "Penetración de internet vs. población municipal (escala logarítmica)",
    x = "Población municipal (2022, escala log)", y = "Penetración de internet",
    caption = "Fuente: Encuesta a Tenderos FU03 (2022) y TerriData - DNP. Elaboración propia."
  )

ggsave(file.path(RUTA_GRAFICOS, "02_penetracion_vs_poblacion.png"), g2,
       width = 9, height = 6, dpi = 300, bg = "white", type = "cairo")


# ------------------------------------------------------------------------------
# GRÁFICO 3 — Dispersión con tercera dimensión: comparación entre dos sectores
# Mensaje: los negocios de Tienda y de Comida preparada tienden a moverse juntos
# en su adopción de internet ciudad por ciudad; el tamaño del punto = población.
# ------------------------------------------------------------------------------
g3_datos <- base_extensa %>%
  filter(!is.na(internet_Tienda), !is.na(`internet_Comida preparada`), !is.na(poblacion_total)) %>%
  mutate(destacar = municipio %in% c("Bogotá", "Barranquilla", "Bucaramanga", "Pereira"))

g3 <- ggplot(g3_datos, aes(x = internet_Tienda, y = `internet_Comida preparada`)) +
  geom_abline(slope = 1, intercept = 0, color = COLOR_GRIS, linetype = "dashed", linewidth = 0.6) +
  geom_point(aes(size = poblacion_total), color = COLOR_SECUNDARIO, alpha = 0.6) +
  geom_point(data = filter(g3_datos, destacar), aes(size = poblacion_total),
             color = COLOR_ACENTO, alpha = 0.9) +
  geom_text_repel(data = filter(g3_datos, destacar), aes(label = municipio),
                   size = 3.6, fontface = "bold", color = "#0D1B24",
                   min.segment.length = 0, seed = 2) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  scale_size_continuous(range = c(1.5, 11), guide = "none") +
  labs(
    title = "Comida preparada usa más internet que las tiendas en casi toda ciudad",
    subtitle = "Penetración de internet por municipio: Tienda vs. Comida preparada (correlación = 0.70)\nPor encima de la línea punteada: el sector de comida preparada supera a las tiendas (tamaño = población)",
    x = "Penetración de internet — Tienda", y = "Penetración de internet — Comida preparada",
    caption = "Fuente: Encuesta a Tenderos FU03 (2022) y TerriData - DNP. Elaboración propia."
  )

ggsave(file.path(RUTA_GRAFICOS, "03_scatter_tienda_vs_comida.png"), g3,
       width = 9, height = 6.2, dpi = 300, bg = "white", type = "cairo")


# ------------------------------------------------------------------------------
# GRÁFICO 4 — Top ciudades: penetración de internet en el sector líder (Tienda)
# Mensaje: entre las ciudades con más tenderos encuestados, ¿quién usa más internet?
# ------------------------------------------------------------------------------
g4_datos <- base_larga %>%
  filter(Actividad == "Tienda") %>%
  filter(!is.na(municipio)) %>%
  mutate(
    resaltar = municipio == municipio[which.max(penetracion_internet)],
    municipio = fct_reorder(municipio, penetracion_internet)
  )

g4 <- ggplot(g4_datos, aes(x = penetracion_internet, y = municipio, fill = resaltar)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = percent(penetracion_internet, accuracy = 1)),
            hjust = -0.15, size = 3.7, color = "#0D1B24", fontface = "bold") +
  scale_fill_manual(values = c(`TRUE` = COLOR_ACENTO, `FALSE` = COLOR_PRIMARIO), guide = "none") +
  scale_x_continuous(labels = percent, limits = c(0, 1.08), expand = expansion(mult = c(0,0))) +
  labs(
    title = "Zipaquirá lidera la penetración de internet entre tiendas",
    subtitle = "Penetración de internet en el sector 'Tienda', por municipio encuestado",
    x = NULL, y = NULL,
    caption = "Fuente: Encuesta a Tenderos FU03 (2022). Elaboración propia."
  )

ggsave(file.path(RUTA_GRAFICOS, "04_top10_ciudades_tienda.png"), g4,
       width = 9, height = 5.5, dpi = 300, bg = "white", type = "cairo")
