# =============================================================================
# Clase 21 - Practica: Graficos editorializados en R (estilo Our World in Data)
# Ciencia de Datos para Economia y Negocios - FCE/UBA
# -----------------------------------------------------------------------------
# Plantilla GENERICA y reutilizable. Cada bloque del grafico esta comentado con
# el elemento de storytelling que aporta. Para aplicarla a una base de Argendata:
#   (1) reemplaza el bloque "DATOS" por tu base,
#   (2) definis quien es "protagonista" (color) y quien es "contexto" (gris),
#   (3) reescribis titulo/subtitulo/anotaciones y ajustas las coordenadas.
# Todo lo demas (tema, escalas, etiquetas directas) es reutilizable tal cual.
#
# CHECKLIST MINIMO (version larga, en el chat / la slide):
#   1. Un solo mensaje. El TITULO afirma la conclusion (no describe la variable).
#   2. Subtitulo = metrica + unidad + ajustes (real/nominal, per capita, PPA).
#   3. Jerarquia: resalta 1-2 series; manda el resto a gris.
#   4. Color = significado y CONSISTENTE entre titulo, linea y etiqueta.
#   5. Etiquetas directas al final en lugar de leyenda.
#   6. Anotaciones (texto + flecha + recuadro) que guien la lectura.
#   7. Eje honesto (?arranca en cero?), fuente citada, sin chartjunk.
# =============================================================================

library(tidyverse)   # ggplot2, dplyr, etc.
library(ggtext)      # titulos/subtitulos con color (markdown + HTML)
library(scales)      # formato de ejes y etiquetas
library(gapminder)   # datos de ejemplo (reemplazar por base de Argendata)

# (Opcional) tipografia tipo OWID. Requiere sysfonts + showtext:
# library(showtext); font_add_google("Lato", "lato"); showtext_auto()
# y luego usar base_family = "lato" dentro de theme_owid().


# -----------------------------------------------------------------------------
# 0) PALETA Y TEMA REUTILIZABLES  <- esto es lo que mas vas a reusar
# -----------------------------------------------------------------------------
owid_azul   <- "#4C6A9C"
owid_rojo   <- "#B13507"
owid_verde  <- "#578145"
owid_morado <- "#6D3E91"
owid_gris   <- "#BDBDBD"   # para todo lo que NO es protagonista

theme_owid <- function(base_size = 13, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_markdown(face = "bold", size = rel(1.35),
                                       colour = "#1d1d1d", lineheight = 1.2,
                                       margin = margin(b = 4)),
      plot.subtitle = element_markdown(size = rel(1.0), colour = "#5b5b5b",
                                       margin = margin(b = 16)),
      plot.caption  = element_markdown(hjust = 0, size = rel(0.72),
                                       colour = "#8a8a8a", margin = margin(t = 14)),
      axis.title    = element_blank(),         # OWID casi no usa titulos de eje
      axis.text     = element_text(colour = "#5b5b5b"),
      axis.ticks    = element_blank(),
      panel.grid.major.y = element_line(colour = "#e6e6e6", linewidth = 0.4),
      panel.grid.major.x = element_blank(),    # solo grilla horizontal
      panel.grid.minor   = element_blank(),
      legend.position    = "none",             # usamos etiquetas directas
      plot.margin = margin(t = 14, r = 110, b = 10, l = 16)  # der.: espacio etiquetas
    )
}


# -----------------------------------------------------------------------------
# 1) DATOS  <- REEMPLAZAR por tu base de Argendata
#    Patron: una columna "grupo" que distingue protagonistas vs "Otros".
# -----------------------------------------------------------------------------
protagonistas <- c("Argentina", "Korea, Rep.")
contexto      <- c("Brazil", "Chile", "Mexico")

datos <- gapminder %>%
  filter(country %in% c(protagonistas, contexto)) %>%
  mutate(grupo = if_else(country %in% protagonistas,
                         as.character(country), "Otros"))

# Ultimo punto de cada protagonista -> para la etiqueta directa al final
fin <- datos %>%
  filter(grupo != "Otros", year == max(year)) %>%
  mutate(etiqueta = recode(country, "Korea, Rep." = "Corea del Sur"))

# Colores asignados a cada protagonista: los MISMOS que usa el titulo
colores <- c("Argentina" = owid_azul, "Korea, Rep." = owid_rojo)


# -----------------------------------------------------------------------------
# 2) TITULO Y SUBTITULO  (el color del nombre = color de la linea)
#    ggtext lee markdown (**negrita**) y HTML (<span style='color:...'>).
# -----------------------------------------------------------------------------
titulo <- sprintf(
  "La <span style='color:%s'>**Argentina**</span> era mucho más rica que <span style='color:%s'>**Corea del Sur**</span>... y quedo muy atras",
  owid_azul, owid_rojo)

subtitulo <- "PBI per capita, en dolares internacionales (ajustados por poder de compra)"


# -----------------------------------------------------------------------------
# 3) GRAFICO  (cada capa = un elemento de storytelling)
# -----------------------------------------------------------------------------
g <- ggplot(datos, aes(year, gdpPercap)) +
  
  # (a) RECUADRO: resaltar un periodo de interes (ej. crisis 2001-2002) --------
annotate("rect", xmin = 1998, xmax = 2002, ymin = -Inf, ymax = Inf,
         fill = owid_azul, alpha = 0.07) +
  annotate("text", x = 2000, y = 1500, label = "Crisis\n2001-2002",
           colour = owid_azul, size = 3, lineheight = 0.9, fontface = "bold") +
  
  # (b) CONTEXTO: el resto de las series, en gris y finas (no compiten) --------
geom_line(data = filter(datos, grupo == "Otros"),
          aes(group = country), colour = owid_gris, linewidth = 0.6) +
  
  # (c) PROTAGONISTAS: lineas resaltadas, gruesas y con color ------------------
geom_line(data = filter(datos, grupo != "Otros"),
          aes(colour = grupo), linewidth = 1.2) +
  geom_point(data = fin, aes(colour = grupo), size = 2.4) +
  
  # (d) ETIQUETAS DIRECTAS al final (reemplazan la leyenda) --------------------
geom_text(data = fin, aes(label = etiqueta, colour = grupo),
          hjust = 0, nudge_x = 1.2, fontface = "bold", size = 4) +
  
  # (e) ANOTACION + FLECHA hacia el punto clave (el cruce, ~1990) --------------
annotate("text", x = 1974, y = 19500,
         label = "Hacia 1990 Corea\nsupera a la Argentina",
         hjust = 0, colour = "#555555", size = 3.5, lineheight = 0.95) +
  annotate("segment", x = 1980, y = 18800, xend = 1987, yend = 10200,
           linewidth = 0.5, colour = "#555555",
           arrow = arrow(length = unit(2.4, "mm"), type = "closed")) +

  # (e2) ANOTACION + FLECHA hacia el punto clave (cambia la tendencia, ~1975) --------------
annotate("text", x = 1965, y = 15000,
         label = "Cambia de tendencia en los '70",
         hjust = 0, colour = "#555555", size = 3.5, lineheight = 0.95) +
  annotate("segment", x = 1970, y = 14500, xend = 1977, yend = 10200,
           linewidth = 0.5, colour = "#555555",
           arrow = arrow(length = unit(2.4, "mm"), type = "closed")) +
  
  # (f) ESCALAS, COLORES y RECORTE ---------------------------------------------
scale_colour_manual(values = colores) +
  scale_y_continuous(limits = c(0, NA),
                     labels = label_dollar(prefix = "US$ ",
                                           big.mark = ".", decimal.mark = ",")) +
  scale_x_continuous(breaks = seq(1950, 2010, 10),
                     expand = expansion(mult = c(0.01, 0.04))) +
  coord_cartesian(clip = "off") +   # deja dibujar las etiquetas fuera del panel
  
  # (g) TITULO, SUBTITULO, FUENTE y TEMA ---------------------------------------
labs(title   = titulo,
     subtitle = subtitulo,
     caption = "Datos: Gapminder (Maddison Project) - Replica con fines didacticos del estilo *Our World in Data*") +
  theme_owid()

print(g)


# -----------------------------------------------------------------------------
# 4) GUARDAR  (proporcion apaisada, tipo OWID)
# -----------------------------------------------------------------------------
ggsave("clase21_grafico_storytelling.png", g,
       width = 9, height = 5.5, dpi = 300, bg = "white")


# =============================================================================
# BLOQUES OPCIONALES - sumalos al objeto g segun lo que quieras contar
# =============================================================================

# --- Marcar un EVENTO puntual con linea vertical + etiqueta ------------------
g +
  geom_vline(xintercept = 1976, linetype = "dashed",
             colour = "#bbbbbb", linewidth = 0.4) +
  annotate("text", x = 1976, y = 23000, label = "Evento", angle = 90,
           vjust = -0.4, hjust = 1, colour = "#9a9a9a", size = 3)

# --- Resaltar UN punto con circulo abierto + etiqueta de valor ---------------
pico <- filter(datos, country == "Korea, Rep.", year == 2007)
g +
  geom_point(data = pico, shape = 21, size = 6, stroke = 1,
             colour = owid_rojo, fill = NA) +
  geom_text(data = pico,
            aes(label = label_dollar(prefix = "US$ ", big.mark = ".",decimal.mark = ',')(gdpPercap)),
            nudge_y = 1800, colour = owid_rojo, fontface = "bold", size = 3.5)

# --- Sombrear el area bajo una serie protagonista ----------------------------
g +
  geom_area(data = filter(datos, country == "Argentina"),
            aes(y = gdpPercap), fill = owid_azul, alpha = 0.08)

# --- Etiquetar TODAS las series sin solaparlas (alternativa con ggrepel) -----
library(ggrepel)
g + geom_text_repel(
      data = datos %>% filter( !country %in% c('Korea, Rep.','Argentina'),
                    year == max(year)),
      aes(label = country, colour = grupo),
      hjust = 0, direction = "y", nudge_x = 1.5, segment.size = 0.2)
