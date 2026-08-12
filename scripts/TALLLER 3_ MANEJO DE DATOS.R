# ==============================================================================
# SCRIPT FINAL: TALLER 3 - HACIENDO ECONOMÍA
# 
# AUTORES:
# - Joan Santiago Cortés Gomez
# - Santiago Gomez Ibague
# - David Alejandro Suárez Escorcia
#
# NOTA DE REPRODUCIBILIDAD:
# Este código contiene las instrucciones paso a paso para que cualquier persona 
# pueda ejecutar este taller en su sistema operativo de preferencia (macOS, Windows 
# o Linux). Para que funcione correctamente, asegúrese de instalar las librerías 
# necesarias y reemplazar las rutas de los archivos locales en las Secciones 0 y 4 
# con la ubicación exacta dentro de su computador.
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. PREPARACIÓN DEL ENTORNO Y CARGA DE DATOS
# ------------------------------------------------------------------------------
# INSTRUCCIÓN: Si no tiene alguna de estas librerías, instálelas primero ejecutando:
# install.packages(c("dplyr", "tidyr", "haven", "readxl"))

# Cargamos las librerías necesarias para la limpieza, manipulación y cruce de datos
library(dplyr)
library(tidyr)
library(haven) 
library(readxl)

# INSTRUCCIÓN: Asigne la ruta de su computador donde guardó la base de tenderos.
# Reemplace "ruta/a/su/archivo.dta" con su ruta local.
# TenderosFU03_Publica <- read_dta("ruta/a/su/archivo.dta")


# ------------------------------------------------------------------------------
# TAREA 1: PENETRACIÓN DE INTERNET POR CIUDAD
# Objetivo: Pasar de la base inicial (nivel de tienda) a la base final (nivel de ciudad)
# Operación: Collapse / Aggregate
# ------------------------------------------------------------------------------

base_ciudad <- TenderosFU03_Publica %>%
  
  # 1. Agrupamos utilizando la variable identificadora de la ciudad (Munic_Dept)
  group_by(Munic_Dept) %>%
  
  # 2. Calculamos el porcentaje de uso de internet (promedio de la variable binaria)
  # Usamos na.rm = TRUE para omitir valores nulos y evitar errores en el cálculo
  summarise(penetracion_internet = mean(uso_internet, na.rm = TRUE))

# Visualizamos el resultado de la Tarea 1
print("Resultado Tarea 1: Penetración por ciudad")
head(base_ciudad)


# ------------------------------------------------------------------------------
# TAREA 2: PENETRACIÓN DE INTERNET POR SECTOR COMERCIAL
# Objetivo: Pasar de la base inicial a una base a nivel de actividad
# Operación: Reshape Long (Melt) + Collapse (Group By)
# ------------------------------------------------------------------------------

base_actividad <- TenderosFU03_Publica %>%
  
  # 1. Reshape Long (Melt): Pasamos las 11 columnas de actividades a filas
  pivot_longer(
    cols = actG1:actG11, 
    names_to = "Actividad",
    values_to = "Realiza_Actividad"
  ) %>%
  
  # 2. Filtramos para quedarnos solo con los registros donde sí realizan la actividad
  # (Asumiendo que la variable toma el valor de 1 cuando pertenece a ese sector)
  filter(Realiza_Actividad == 1) %>%
  
  # 3. Collapse: Agrupamos por la nueva columna única de Actividad
  group_by(Actividad) %>%
  
  # 4. Calculamos el promedio de penetración de internet para cada sector
  summarise(penetracion_internet = mean(uso_internet, na.rm = TRUE)) %>%
  
  # Opcional: Ordenamos de mayor a menor penetración
  arrange(desc(penetracion_internet))

# Visualizamos el resultado de la Tarea 2
print("Resultado Tarea 2: Penetración por actividad comercial")
head(base_actividad)


# ------------------------------------------------------------------------------
# TAREA 3: SECTORES QUE MÁS USAN INTERNET POR CIUDAD
# Objetivo: Pasar de la base inicial a una base a nivel de actividad x ciudad
# Operación: Reshape Long (Melt) + Collapse (Group By) múltiple
# ------------------------------------------------------------------------------

base_actividad_ciudad <- TenderosFU03_Publica %>%
  
  # 1. Reshape Long (Melt): Pasamos las columnas de actividades a filas
  pivot_longer(
    cols = actG1:actG11, 
    names_to = "Actividad",
    values_to = "Realiza_Actividad"
  ) %>%
  
  # 2. Filtramos para quedarnos con los negocios que sí realizan la actividad
  filter(Realiza_Actividad == 1) %>%
  
  # 3. Collapse: Agrupamos por CIUDAD y por ACTIVIDAD simultáneamente
  group_by(Munic_Dept, Actividad) %>%
  
  # 4. Calculamos el promedio de penetración de internet
  # (.groups = 'drop' evita un mensaje de advertencia común en R al hacer grupos múltiples)
  summarise(penetracion_internet = mean(uso_internet, na.rm = TRUE), .groups = 'drop') %>%
  
  # Opcional: Ordenamos por municipio, y dentro de cada municipio, por la mayor penetración
  arrange(Munic_Dept, desc(penetracion_internet))

# Visualizamos el resultado de la Tarea 3
print("Resultado Tarea 3: Penetración por actividad cruzada con ciudad")
head(base_actividad_ciudad)


# ------------------------------------------------------------------------------
# TAREA 4: RELACIÓN CON LA POBLACIÓN DE LA CIUDAD
# Objetivo: Unir la base de penetración por ciudad con datos de población (ej. DANE/TerriData)
# Operación: Joins (Left Join)
# ------------------------------------------------------------------------------

# 1. Importar el archivo descargado de TerriData
# INSTRUCCIÓN: Reemplace la ruta de abajo con la ubicación de su archivo de Excel.
# En Mac suele verse como: "/Users/su_usuario/Downloads/TerriData_Dim2.xlsx"
# En Windows suele verse como: "C:/Users/su_usuario/Downloads/TerriData_Dim2.xlsx"
base_terridata <- read_excel("/Users/davidsuarez/Downloads/TerriData_Dim2.xlsx")

# 2. Preparar y limpiar la base de población antes del cruce
base_poblacion_limpia <- base_terridata %>%
  
  # LIMPIEZA DE FORMATO: Aseguramos que sea texto, quitamos puntos de miles y cambiamos comas por puntos decimales.
  mutate(`Dato Numérico` = as.character(`Dato Numérico`)) %>%
  mutate(`Dato Numérico` = gsub("\\.", "", `Dato Numérico`)) %>%
  mutate(`Dato Numérico` = gsub(",", ".", `Dato Numérico`)) %>%
  mutate(`Dato Numérico` = as.numeric(`Dato Numérico`)) %>%
  
  # Como los datos vienen desagregados por edad/género, agrupamos por el municipio
  group_by(`Código Entidad`, Entidad) %>%
  
  # Sumamos toda la población de ese municipio en un solo gran total
  summarise(poblacion_total = sum(`Dato Numérico`, na.rm = TRUE), .groups = 'drop') %>%
  
  # Convertimos el código de texto a número para que encaje perfectamente con la base de ciudad
  mutate(`Código Entidad` = as.numeric(`Código Entidad`))

# 3. Join: Usamos la base de la TAREA 1 (base_ciudad) y le pegamos la población limpia
base_final <- base_ciudad %>%
  
  # Hacemos el left_join indicando los nombres de las columnas llave en ambas tablas
  left_join(base_poblacion_limpia, by = c("Munic_Dept" = "Código Entidad"))

# Visualizamos el resultado definitivo de la Tarea 4
print("Resultado Tarea 4: Penetración de internet vs Población Municipal")
head(base_final)


# ------------------------------------------------------------------------------
# TAREA 5: BASES DE DATOS PARA ANÁLISIS ESTADÍSTICO Y VISUALIZACIÓN
# Objetivo: Crear una base "Larga" (PowerBI) y una base "Extensa" (Gráficos de dispersión)
# Operaciones: Merge (Join) y Reshape Wide (Cast / pivot_wider)
# ------------------------------------------------------------------------------

# --- 5A. BASE LARGA (Para visualizar en PowerBI) ---
# Tomamos la base de Tarea 3 (actividad por ciudad) y le hacemos Merge con la población limpia
base_larga <- base_actividad_ciudad %>%
  left_join(base_poblacion_limpia, by = c("Munic_Dept" = "Código Entidad")) %>%
  
  # Renombramos y organizamos las columnas para que queden idénticas al ejemplo de la clase
  select(
    divipola = Munic_Dept,
    municipio = Entidad,
    Actividad,
    penetracion_internet,
    poblacion_total
  )

print("Resultado Tarea 5A: Base Larga (Para PowerBI)")
head(base_larga)


# --- 5B. BASE EXTENSA (Para hacer gráficos de dispersión) ---
# Tomamos la base larga y aplicamos un 'Cast' (pivot_wider) para poner las actividades en múltiples columnas
base_extensa <- base_larga %>%
  pivot_wider(
    names_from = Actividad,
    values_from = penetracion_internet,
    names_prefix = "internet_" # Agrega el prefijo 'internet_' a las nuevas columnas
  )

print("Resultado Tarea 5B: Base Extensa (Para gráficos de dispersión)")
head(base_extensa)
