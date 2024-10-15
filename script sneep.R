data <- read.csv(file.choose(), header = TRUE, sep = ",")
names(data)
rm(list = ls()) #para remover todos los objetos que estÃ¡n en el "Global Environment"
options(scipen = 999) # para evitar tener caracteres científicos

# Install the package if necessary
install.packages("ggthemes")

####################

# Cargar librerías necesarias
library(dplyr)
library(ggplot2)

# Filtrar solo a los hombres
data_hombres <- data %>%
  filter(genero_descripcion == "Varón")
# Tomar una muestra del 10% de los datos
muestra_data_hombres <- data_hombres %>%
  sample_frac(0.1)

# Calcular la duración promedio de la condena por nivel de instrucción en la muestra
duracion_condena_instruccion_muestra <- muestra_data_hombres %>%
  group_by(nivel_instruccion_descripcion) %>%
  summarise(duracion_promedio_anios = mean(duracion_condena_anios, na.rm = TRUE))

# Mostrar la tabla de resultados
print(duracion_condena_instruccion_muestra)

# Graficar la duración promedio de la condena por nivel de instrucción en la muestra
ggplot(duracion_condena_instruccion_muestra, aes(x = nivel_instruccion_descripcion, y = duracion_promedio_anios)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  labs(title = "Duración Promedio de la Condena por Nivel de Instrucción (Muestra)", 
       x = "Nivel de Instrucción", 
       y = "Duración Promedio (años)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Crear una nueva columna con las categorías agrupadas de nivel de instrucción
muestra_data_hombres <- muestra_data_hombres %>%
  mutate(nivel_instruccion_agrupado = case_when(
    grepl("Primario", nivel_instruccion_descripcion) ~ "Primario",
    grepl("Secundario", nivel_instruccion_descripcion) | grepl("Polimodal", nivel_instruccion_descripcion) ~ "Secundario",
    grepl("Terciario", nivel_instruccion_descripcion) ~ "Terciario",
    grepl("Universitario", nivel_instruccion_descripcion) ~ "Universitario",
    grepl("EGB", nivel_instruccion_descripcion) ~ "Educación General Básica",
    TRUE ~ NA_character_  # Asignar NA a los casos sin información
  ))

# Eliminar los casos con "Sin información"
muestra_data_hombres_filtrada <- muestra_data_hombres %>%
  filter(!is.na(nivel_instruccion_agrupado))

# Calcular la duración promedio de la condena por nivel de instrucción agrupado en la muestra
duracion_condena_instruccion_agrupada <- muestra_data_hombres_filtrada %>%
  group_by(nivel_instruccion_agrupado) %>%
  summarise(duracion_promedio_anios = mean(duracion_condena_anios, na.rm = TRUE))

# Mostrar la tabla de resultados
print(duracion_condena_instruccion_agrupada)

# Graficar la duración promedio de la condena por nivel de instrucción agrupado en la muestra
ggplot(duracion_condena_instruccion_agrupada, aes(x = nivel_instruccion_agrupado, y = duracion_promedio_anios)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  labs(title = "Duración Promedio de la Condena por Nivel de Instrucción Agrupado (Muestra)", 
       x = "Nivel de Instrucción Agrupado", 
       y = "Duración Promedio (años)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Contar la cantidad de individuos por género
distribucion_genero <- data %>%
  group_by(genero_descripcion) %>%
  summarise(cantidad = n())

# Mostrar la tabla de distribución por género
print(distribucion_genero)
# Graficar la distribución por género mejorada
ggplot(distribucion_genero, aes(x = genero_descripcion, y = cantidad, fill = genero_descripcion)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = cantidad), vjust = -0.3) +  # Agregar etiquetas con las cantidades
  scale_y_continuous(labels = scales::comma) +  # Escala del eje Y con formato de miles
  theme_minimal() +
  labs(title = "Distribución por Género", x = "Género", y = "Cantidad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Girar texto del eje X
        legend.position = "none")  # Ocultar leyenda


# Agrupar los datos por la categoría de delito y contar la cantidad de casos por cada categoría
distribucion_delitos <- data %>%
  group_by(delito1_descripcion) %>%
  summarise(cantidad = n()) %>%
  arrange(desc(cantidad))  # Ordenar de mayor a menor cantidad

# Mostrar la tabla de distribución de delitos por categoría
print(distribucion_delitos)

# Graficar la distribución de delitos por categoría
ggplot(distribucion_delitos, aes(x = reorder(delito1_descripcion, -cantidad), y = cantidad, fill = delito1_descripcion)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = cantidad), vjust = -0.3, size = 3) +  # Agregar etiquetas con las cantidades
  theme_minimal() +
  labs(title = "Distribución de Delitos por Categoría", x = "Delito", y = "Cantidad") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),  # Rotar el texto para que sea legible
        legend.position = "none")  # Ocultar leyenda


# Definir un umbral para los delitos más frecuentes (por ejemplo, solo delitos con más de 10,000 casos)
umbral_delitos <- 10000

# Agrupar las categorías menos comunes bajo "Otros"
distribucion_delitos_agrupada <- distribucion_delitos %>%
  mutate(delito1_descripcion = ifelse(cantidad < umbral_delitos, "Otros", delito1_descripcion)) %>%
  group_by(delito1_descripcion) %>%
  summarise(cantidad = sum(cantidad)) %>%
  arrange(desc(cantidad))  # Ordenar de mayor a menor

# Graficar la distribución de delitos agrupada
ggplot(distribucion_delitos_agrupada, aes(x = reorder(delito1_descripcion, -cantidad), y = cantidad, fill = delito1_descripcion)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::comma(cantidad)), vjust = -0.3, size = 3) +  # Etiquetas con formato de miles
  theme_minimal() +
  labs(title = "Distribución de Delitos por Categoría (Agrupada)", x = "Delito", y = "Cantidad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Girar texto para mejor legibilidad
        legend.position = "none")  # Ocultar leyenda


# Graficar un histograma de la edad de los presos
ggplot(data, aes(x = edad)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribución de la Edad de los Presos", x = "Edad", y = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Histograma de la duración de la condena en años
ggplot(data, aes(x = duracion_condena_anios)) +
  geom_histogram(binwidth = 2, fill = "darkgreen", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribución de la Duración de la Condena", x = "Duración de la Condena (años)", y = "Frecuencia")

# Gráfico de cajas de la edad según nivel de instrucción
ggplot(data, aes(x = nivel_instruccion_descripcion, y = edad, fill = nivel_instruccion_descripcion)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribución de la Edad según Nivel de Instrucción", x = "Nivel de Instrucción", y = "Edad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# Gráfico de densidad de la duración de la condena por nivel de instrucción
ggplot(data, aes(x = duracion_condena_anios, fill = nivel_instruccion_descripcion)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Densidad de la Duración de la Condena por Nivel de Instrucción", x = "Duración de la Condena (años)", y = "Densidad") +
  theme(legend.position = "bottom")

# Gráfico de barras de participación en programas educativos
ggplot(data, aes(x = participacion_programa_educativo_descripcion)) +
  geom_bar(fill = "purple", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Participación en Programas Educativos", x = "Participación en Programas Educativos", y = "Cantidad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Gráfico de barras de reincidencia por nivel de instrucción
ggplot(data, aes(x = nivel_instruccion_descripcion, fill = factor(reincidencia_simple))) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Reincidencia por Nivel de Instrucción", x = "Nivel de Instrucción", y = "Cantidad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")


# Agrupar niveles de instrucción en categorías más generales
data_hombres <- data_hombres %>%
  mutate(nivel_instruccion_agrupado = case_when(
    grepl("Ninguno", nivel_instruccion_descripcion, ignore.case = TRUE) ~ "Sin instrucción",
    grepl("Primario|EGB 1|EGB 2", nivel_instruccion_descripcion, ignore.case = TRUE) ~ "Primaria",
    grepl("Secundario|EGB 3|Polimodal", nivel_instruccion_descripcion, ignore.case = TRUE) ~ "Secundaria",
    grepl("Terciario", nivel_instruccion_descripcion, ignore.case = TRUE) ~ "Terciaria",
    grepl("Universitario", nivel_instruccion_descripcion, ignore.case = TRUE) ~ "Universitaria",
    TRUE ~ "Sin información"
  ))

# Gráfico de barras de reincidencia por nivel de instrucción agrupado
ggplot(data_hombres, aes(x = nivel_instruccion_agrupado, fill = factor(reincidencia_simple))) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Reincidencia por Nivel de Instrucción (Agrupado)", x = "Nivel de Instrucción", y = "Cantidad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

ggplot(data_hombres, aes(x = nivel_instruccion_agrupado, fill = factor(es_reincidente_id))) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Reincidencia por Nivel de Instrucción (Agrupado)", x = "Nivel de Instrucción", y = "Cantidad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
# Filtrar los datos para eliminar los valores "Sin información" y los que no reincidieron (es_reincidente_id == 0)
data_hombres_filtrado <- data_hombres %>%
  filter(nivel_instruccion_agrupado != "Sin información" & es_reincidente_id != 0)

# Crear el gráfico filtrado
ggplot(data_hombres_filtrado, aes(x = nivel_instruccion_agrupado, fill = factor(es_reincidente_id))) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Reincidencia por Nivel de Instrucción (Agrupado)", x = "Nivel de Instrucción", y = "Cantidad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")


ggplot(data %>% filter(es_reincidente_id != 0), aes(x = edad, fill = factor(es_reincidente_id))) +
  geom_histogram(bins = 30, alpha = 0.7, position = "dodge") +
  theme_minimal() +
  labs(title = "Distribución de la Edad según Reincidencia", x = "Edad", y = "Frecuencia") +
  scale_fill_manual(values = c("green", "red"), name = "Reincidencia")

ggplot(data %>% filter(es_reincidente_id != 0), aes(x = estado_civil_descripcion, fill = factor(es_reincidente_id))) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Reincidencia según Estado Civil", x = "Estado Civil", y = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Agrupamos los delitos en categorías
data_delitos_agrupados <- data %>%
  filter(es_reincidente_id != 0) %>%
  mutate(delito_agrupado = case_when(
    delito1_descripcion %in% c("Robo", "Hurto", "Robo y/o tentativa de robo") ~ "Robo/Hurto",
    delito1_descripcion %in% c("Homicidios dolosos", "Homicidios culposos") ~ "Homicidios",
    delito1_descripcion %in% c("Violaciones/Abuso sexual") ~ "Delitos sexuales",
    delito1_descripcion %in% c("Lesiones", "Amenazas") ~ "Delitos contra la integridad",
    TRUE ~ "Otros delitos"
  ))

# Gráfico de reincidencia por tipo de delito agrupado
ggplot(data_delitos_agrupados, aes(x = delito_agrupado, fill = factor(es_reincidente_id))) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Reincidencia por Tipo de Delito (Agrupado)", x = "Tipo de Delito", y = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Agrupamos el nivel de instrucción
data_instruccion_agrupada <- data %>%
  filter(es_reincidente_id != 0) %>%
  mutate(instruccion_agrupada = case_when(
    nivel_instruccion_descripcion %in% c("Primario incompleto", "Primario completo") ~ "Primaria",
    nivel_instruccion_descripcion %in% c("Secundario incompleto", "Secundario completo", "Polimodal Completo", "Polimodal Incompleto") ~ "Secundaria",
    nivel_instruccion_descripcion %in% c("Terciario incompleto", "Terciario completo") ~ "Terciaria",
    nivel_instruccion_descripcion %in% c("Universitario incompleto", "Universitario completo") ~ "Universitaria",
    nivel_instruccion_descripcion == "Ninguno" ~ "Sin Instrucción",
    TRUE ~ "Sin información"
  ))

# Filtrar para eliminar "Sin información"
data_instruccion_agrupada_filtered <- data_instruccion_agrupada %>%
  filter(instruccion_agrupada != "Sin información")



# Gráfico de reincidencia por participación en programas educativos
ggplot(data %>% filter(es_reincidente_id != 0), aes(x = participacion_programa_educativo_descripcion, fill = factor(es_reincidente_id))) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Reincidencia por Participación en Programas Educativos", x = "Participación en Programas Educativos", y = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# Gráfico de reincidencia por participación en programas laborales
ggplot(data %>% filter(es_reincidente_id != 0), aes(x = participacion_programa_laboral, fill = factor(es_reincidente_id))) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Reincidencia por Participación en Programas Laborales", x = "Participación en Programas Laborales", y = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
# Mejorar visualización del gráfico
ggplot(data_hombres_reincidentes, aes(x = duracion_condena_anios, fill = factor(es_reincidente_id))) +
  geom_bar(position = "dodge", alpha = 0.7) +  # Aumentar transparencia
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 2)) +  # Limitar el rango a 20 años
  scale_fill_brewer(palette = "Set1") +  # Cambiar la paleta de colores
  labs(title = "Reincidencia por Duración de la Condena", x = "Años de Condena", y = "Frecuencia", fill = "Reincidencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Filtrar solo los reincidentes
data_hombres_reincidentes <- data_hombres %>% filter(es_reincidente_id != 0)

# Crear un gráfico para ver la relación entre reincidencia y si tenían trabajo antes de ingresar a prisión
ggplot(data_hombres_reincidentes, aes(x = ultima_situacion_laboral_descripcion, fill = factor(es_reincidente_id))) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Relación entre Reincidencia y Situación Laboral Anterior (Agrupado)", x = "Situación Laboral Anterior", y = "Cantidad") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

