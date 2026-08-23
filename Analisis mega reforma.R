library(readxl)
library(tidyverse)

df_raw <- read_excel("Personal disponible en el sector publico.xlsx")

# 1. Cargar la hoja desde el archivo Excel
# Ajusta 'ruta_de_tu_archivo.xlsx' y el nombre de la hoja (sheet) si aplica


# 2. Limpieza y desanidación de categorías
df_clean <- df_raw %>%
  # Renombrar la primera columna
  rename(Categoria = 1) %>% 
  
  # Eliminar los puntos de miles y convertir a número
  mutate(across(`2021`:`2025`, ~ as.numeric(gsub("\\.", "", .x)))) %>%
  
  # Identificar los nombres de las Dependencias y rellenar hacia abajo
  mutate(
    Dependencia = if_else(!Categoria %in% c("Permanente", "Transitorio"), Categoria, NA_character_)
  ) %>%
  fill(Dependencia, .direction = "down") %>%
  
  # Clasificar si la fila es el 'Total', 'Permanente' o 'Transitorio'
  mutate(
    Tipo_Personal = if_else(Categoria == Dependencia, "Total", Categoria)
  ) %>%
  
  # Reordenar y descartar la columna original anidada
  select(Dependencia, Tipo_Personal, `2021`:`2025`)

# 3. Transformación a formato Tidy (ideal para análisis y gráficos)
df_tidy <- df_clean %>%
  pivot_longer(
    cols = `2021`:`2025`,
    names_to = "Anio",
    values_to = "Cargos"
  ) %>%
  mutate(Anio = as.numeric(Anio))





library(ggplot2)
library(dplyr)
library(scales)
library(ggrepel) # Cargar ggrepel para ubicar los textos ordenadamente

# 1. Preparar datos y crear etiquetas formateadas (ej: "510.160")
df_grafico <- df_tidy %>%
  filter(
    Dependencia != "Sector Público",
    Tipo_Personal == "Total"
  ) %>%
  mutate(
    Etiqueta = number(Cargos, big.mark = ".", accuracy = 1)
  )

# 2. Paleta de colores
colores_foco <- c(
  "Administración Central"       = "#1a365d",
  "Municipalidades"              = "#dd6b20",
  "Universidades y CFT Estatales"= "#4a5568",
  "Empresas Públicas"            = "#718096",
  "Organismos Autónomos"         = "#a0aec0",
  "Otras Instituciones Públicas" = "#cbd5e0"
)

# 3. Construcción del gráfico refinado
p2 <- ggplot(df_grafico, aes(x = Anio, y = Cargos, color = Dependencia, group = Dependencia)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  
  # AÑADIR NÚMEROS SOBRE LOS PUNTOS
  geom_text_repel(
    aes(label = Etiqueta),
    size = 3.2,
    fontface = "bold",
    show.legend = FALSE,
    box.padding = 0.35,
    point.padding = 0.2,
    segment.color = NA # Oculta la línea guía para que sea más limpio
  ) +
  
  # EJE Y MÁS DETALLADO (saltos cada 50.000 con formato numérico completo)
  scale_y_continuous(
    labels = label_number(big.mark = "."),
    breaks = seq(0, 550000, by = 50000),
    limits = c(0, 560000)
  ) +
  
  scale_x_continuous(breaks = 2021:2025) +
  scale_color_manual(values = colores_foco) +
  labs(
    title = "¿Dónde creció el empleo público en Chile? (2021-2025)",
    subtitle = "Evolución detallada de cargos prom. anual por dependencia",
    x = NULL,
    y = "Número de Cargos",
    color = NULL,
    caption = "Fuente: DIPRES - Distribución por Dependencia del Sector Público"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#1a202c"),
    plot.subtitle = element_text(size = 10, color = "#4a5568", margin = margin(b = 15)),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    axis.text.y = element_text(size = 8, color = "#4a5568")
  )

# Renderizar y guardar
print(p2)
ggsave("grafico_empleo_detalle.png", plot = p2, width = 9, height = 6, dpi = 300)

library(knitr)

