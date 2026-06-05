# =============================================================================
# Clase 21 - Practica: Graficos editorializados en R (estilo Our World in Data)
# EJEMPLOS ADICIONALES: (1) barras editorializadas  (2) scatter editorializado
# Ciencia de Datos para Economia y Negocios - FCE/UBA
# -----------------------------------------------------------------------------
# Archivo autocontenido (redefine paleta + theme_owid, corre solo).
# Mismo patron que la plantilla de lineas: protagonista en color, resto en gris,
# titulo con palabras coloreadas, etiquetas directas, anotaciones que guian.
# =============================================================================

library(tidyverse)
library(ggtext)
library(scales)
library(gapminder)

# --- Paleta y tema reutilizables (idem plantilla de lineas) ------------------
owid_azul   <- "#4C6A9C"
owid_rojo   <- "#B13507"
owid_verde  <- "#578145"
owid_gris   <- "#C9C9C9"

theme_owid <- function(base_size = 13, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_markdown(face = "bold", size = rel(1.3),
                                       colour = "#1d1d1d", lineheight = 1.2,
                                       margin = margin(b = 4)),
      plot.subtitle = element_markdown(size = rel(0.98), colour = "#5b5b5b",
                                       margin = margin(b = 16)),
      plot.caption  = element_markdown(hjust = 0, size = rel(0.72),
                                       colour = "#8a8a8a", margin = margin(t = 14)),
      axis.title    = element_blank(),
      axis.text     = element_text(colour = "#5b5b5b"),
      axis.ticks    = element_blank(),
      panel.grid.major.y = element_line(colour = "#e6e6e6", linewidth = 0.4),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      legend.position    = "none",
      plot.margin = margin(t = 14, r = 24, b = 10, l = 16)
    )
}


# =============================================================================
# EJEMPLO 1 - BARRAS EDITORIALIZADAS
# Historia: ranking de PBI per capita en America (2007), con la Argentina
# resaltada y el promedio regional como referencia.
# Claves del tipo: barras ORDENADAS, arrancan en CERO, etiqueta de valor al
# final (no hace falta eje x), una sola barra protagonista.
# =============================================================================

barras <- gapminder %>%
  filter(continent == "Americas", year == 2007) %>%
  mutate(
    pais = recode(country, "United States" = "Estados Unidos"),
    destacar = if_else(country == "Argentina", "Argentina", "Otros"),
    pais = fct_reorder(pais, gdpPercap)          # ordenar de menor a mayor
  )

prom_region <- mean(barras$gdpPercap)            # linea de referencia

col_fill <- c("Argentina" = owid_azul, "Otros" = owid_gris)
col_text <- c("Argentina" = owid_azul, "Otros" = "#7a7a7a")

titulo_barras <- sprintf(
  "En ingreso por habitante, la <span style='color:%s'>**Argentina**</span> quedo en mitad de tabla en America",
  owid_azul)

g_barras <- ggplot(barras, aes(gdpPercap, pais, fill = destacar)) +
  
  # (a) BARRAS: protagonista en color, resto en gris --------------------------
geom_col(width = 0.72) +
  
  # (b) ETIQUETA DE VALOR al final de cada barra (reemplaza al eje x) ----------
geom_text(aes(label = label_dollar(prefix = "US$ ", big.mark = ".",
                                   decimal.mark = ',',
                                   accuracy = 1)(gdpPercap),
              colour = destacar),
          hjust = -0.12, size = 3, fontface = "bold") +
  
  # (c) LINEA DE REFERENCIA: promedio regional + anotacion --------------------
geom_vline(xintercept = prom_region, linetype = "dashed",
           colour = "#6b6b6b", linewidth = 0.4) +
  annotate("text", x = prom_region, y = Inf, vjust = 1.4, hjust = -0.06,
           label = "Promedio regional", colour = "#6b6b6b", size = 3) +
  
  # (d) ESCALAS Y COLORES -----------------------------------------------------
scale_fill_manual(values = col_fill) +
  scale_colour_manual(values = col_text) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.14))) +  # aire p/ etiquetas
  
  # (e) TITULO, SUBTITULO, FUENTE Y TEMA --------------------------------------
labs(title = titulo_barras,
     subtitle = "PBI per capita, dolares internacionales (PPA), 2007",
     caption = "Datos: Gapminder (Maddison Project) - Replica didactica del estilo *Our World in Data*") +
  theme_owid() +
  # en barras horizontales: grilla vertical (eje de valores), SIN grilla horizontal
  theme(panel.grid.major.x = element_line(colour = "#e6e6e6", linewidth = 0.4),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_blank())

print(g_barras)

ggsave("clase21_barras_editorializado.png", g_barras,
       width = 9, height = 7.5, dpi = 300, bg = "white")


# =============================================================================
# EJEMPLO 2 - SCATTER EDITORIALIZADO  (curva de Preston / estilo Rosling)
# Historia: a mayor ingreso, mayor esperanza de vida... salvo excepciones.
# Costa Rica rinde de mas; Guinea Ecuatorial, de menos. Argentina, de referencia.
# Claves del tipo: escala log en ingreso, burbujas por poblacion, resaltar pocos
# puntos con etiqueta, y una FLECHA anclada a un dato real (no a coordenadas
# sueltas) para no romper la curva.
# =============================================================================

sc <- gapminder %>%
  filter(year == 2007) %>%
  mutate(grupo = if_else(country %in% c("Argentina", "Costa Rica",
                                        "Equatorial Guinea"),
                         as.character(country), "Otros"))

etiquetas_sc <- sc %>%
  filter(grupo != "Otros") %>%
  mutate(pais = recode(country, "Equatorial Guinea" = "Guinea Ecuatorial"))

# destino de la flecha = un punto REAL (asi nunca cae fuera de rango)
outlier <- filter(sc, country == "Equatorial Guinea")

col_sc <- c("Argentina" = owid_azul, "Costa Rica" = owid_verde,
            "Equatorial Guinea" = owid_rojo, "Otros" = owid_gris)

titulo_sc <- sprintf(
  "El dinero alarga la vida, pero <span style='color:%s'>**Costa Rica**</span> y <span style='color:%s'>**Guinea Ecuatorial**</span> rompen la regla",
  owid_verde, owid_rojo)

g_scatter <- ggplot(sc, aes(gdpPercap, lifeExp)) +
  
  # (a) CONTEXTO: el resto de los paises, en gris -----------------------------
geom_point(data = filter(sc, grupo == "Otros"),
           aes(size = pop), colour = owid_gris, alpha = 0.6) +
  
  # (a2) Por encima y por debajo: regresion 
  geom_smooth(method='lm',se = FALSE,color='grey50',alpha=0.7 ) + 
  
  # (b) PROTAGONISTAS: pocos puntos resaltados con color ----------------------
geom_point(data = filter(sc, grupo != "Otros"),
           aes(size = pop, colour = grupo), alpha = 0.9) +
  
  # (c) ETIQUETAS de los puntos resaltados ------------------------------------
geom_text(data = etiquetas_sc, aes(label = pais, colour = grupo),
          vjust = -1.2, fontface = "bold", size = 3.3) +
  
  # (d) ANOTACION + FLECHA al outlier (anclada a su punto real) ---------------
annotate("text", x = 2700, y = 53, hjust = 0,
         label = "Petroleo sin desarrollo:\nmucho ingreso,\npoca expectativa de vida",
         colour = "#555555", size = 3, lineheight = 0.95) +
  annotate("curve", x = 4200, y = 53, xend = outlier$gdpPercap * 0.92,
           yend = outlier$lifeExp, curvature = -0.2, linewidth = 0.45,
           colour = "#555555", arrow = arrow(length = unit(2.2, "mm"),
                                             type = "closed")) +
  
  # (d) ANOTACION 2 + FLECHA a regresion ---------------
annotate("text", x = 350, y = 60, hjust = 0,
         label = "Por encima de la linea, los países tienen\nuna esperanza de vida \nmayor a la esperada",
         colour = "#555555", size = 3, lineheight = 0.95) +
  annotate("curve", x = 400, y = 58, xend = 800,yend = 53, 
           curvature = -0.2, linewidth = 0.45,
           colour = "#555555", arrow = arrow(length = unit(2.2, "mm"),
                                             type = "closed")) +
  
  # (e) ESCALAS: ingreso en log, burbujas por poblacion -----------------------
scale_colour_manual(values = col_sc) +
  scale_size_area(max_size = 14, guide = "none") +
  scale_x_log10(labels = label_dollar(prefix = "US$ ", big.mark = ".")) +
  scale_y_continuous(breaks = seq(40, 85, 10)) +
  
  # (f) TITULO, SUBTITULO, FUENTE Y TEMA --------------------------------------
labs(title = titulo_sc,
     subtitle = "Esperanza de vida segun PBI per capita (escala logaritmica), 2007. El tamano representa la poblacion.",
     caption = "Datos: Gapminder (Maddison Project) - Replica didactica del estilo *Our World in Data*") +
  theme_owid() +
  # en scatter conviene grilla en AMBOS ejes (suave)
  theme(panel.grid.major.x = element_line(colour = "#e6e6e6", linewidth = 0.4))

print(g_scatter)

ggsave("clase21_scatter_editorializado.png", g_scatter,
       width = 9, height = 5.8, dpi = 300, bg = "white")