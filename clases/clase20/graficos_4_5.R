# =============================================================================
# Clase 21 - Practica: Graficos editorializados en R (estilo Our World in Data)
# EJEMPLOS DE DISTRIBUCION: (1) boxplot editorializado  (2) histograma / densidad
# Ciencia de Datos para Economia y Negocios - FCE/UBA
# -----------------------------------------------------------------------------
# Archivo autocontenido (redefine paleta + theme_owid, corre solo).
# Idea para distribuciones: el titulo cuenta la FORMA o el contraste; el grafico
# muestra los datos crudos (puntos en el boxplot) y marca las referencias
# (mediana, media) que sostienen el mensaje.
# =============================================================================

library(tidyverse)
library(ggtext)
library(scales)
library(gapminder)

# --- Paleta y tema reutilizables ---------------------------------------------
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
# EJEMPLO 1 - BOXPLOT EDITORIALIZADO
# Historia: la esperanza de vida no solo es mas baja en Africa, tambien es la
# mas desigual entre paises.
# Claves del tipo: resaltar el grupo que carga el mensaje, MOSTRAR los puntos
# (cada uno = un pais) porque el box solo esconde la n y la dispersion, ordenar
# los grupos por mediana, anotar un caso extremo.
# =============================================================================

caja <- gapminder %>%
  filter(year == 2007) %>%
  mutate(
    continente = recode(continent,
                        "Africa" = "Africa", "Americas" = "America",
                        "Asia" = "Asia", "Europe" = "Europa",
                        "Oceania" = "Oceania"),
    destacar = if_else(continent == "Africa", "Africa", "Otros"),
    continente = fct_reorder(continente, lifeExp, .fun = median)  # ordenar x mediana
  )

# caso extremo a anotar (dato real -> la flecha nunca cae fuera de rango)
extremo <- caja %>% filter(country == "Swaziland")

col_caja <- c("Africa" = owid_rojo, "Otros" = owid_gris)

titulo_caja <- sprintf(
  "<span style='color:%s'>**Africa**</span> tiene la esperanza de vida mas baja y mas desigual del mundo",
  owid_rojo)

g_box <- ggplot(caja, aes(lifeExp, continente)) +
  
  # (a) BOXES tenues: protagonista en color, resto en gris --------------------
geom_boxplot(aes(fill = destacar), colour = "#666666",
             width = 0.55, alpha = 0.25, outlier.shape = NA) +  # sin outliers dobles
  
  # (b) LOS DATOS: cada punto = un pais (lo que el box esconde) ----------------
geom_jitter(aes(colour = destacar),
            position = position_jitter(height = 0.15, width = 0, seed = 123),
            size = 1.6, alpha = 0.6) +
  
  # (c) ANOTACION + FLECHA a un caso extremo (anclada a su punto real) --------
annotate("text", x = 41, y = 0.62, hjust = 0, vjust = 0,
         label = "Suazilandia: 39,6 años,\nel valor mas bajo del mundo",
         colour = "#555555", size = 3, lineheight = 0.95) +
  annotate("curve", x = 41, y = 0.7, xend = extremo$lifeExp + 0.4,
           yend = 1, curvature = 0.2, linewidth = 0.45, colour = "#555555",
           arrow = arrow(length = unit(2.2, "mm"), type = "closed")) +
  
  # (d) ESCALAS Y COLORES -----------------------------------------------------
scale_fill_manual(values = col_caja) +
  scale_colour_manual(values = col_caja) +
  scale_x_continuous(breaks = seq(40, 85, 10)) +
  
  # (e) TITULO, SUBTITULO, FUENTE Y TEMA --------------------------------------
labs(title = titulo_caja,
     subtitle = "Esperanza de vida al nacer por continente. Cada punto es un pais, 2007.",
     caption = "Datos: Gapminder (Maddison Project) - Replica didactica del estilo *Our World in Data*") +
  theme_owid() +
  # boxplot horizontal: grilla vertical (eje de valores), sin grilla horizontal
  theme(panel.grid.major.x = element_line(colour = "#e6e6e6", linewidth = 0.4),
        panel.grid.major.y = element_blank())

print(g_box)

ggsave("clase21_boxplot_editorializado.png", g_box,
       width = 9, height = 5.6, dpi = 300, bg = "white")


# =============================================================================
# EJEMPLO 2 - HISTOGRAMA EDITORIALIZADO
# Historia: el ingreso mundial esta sesgado a la derecha: la media queda muy por
# encima de la mediana porque unos pocos paises ricos estiran el promedio.
# Claves del tipo: un solo color suave, lineas de referencia (media y mediana)
# con etiquetas, y -truco de consistencia- las palabras del titulo pintadas con
# el color de cada linea.
# =============================================================================

hist_df <- gapminder %>% filter(year == 2007)

mediana_y <- median(hist_df$gdpPercap)
media_y   <- mean(hist_df$gdpPercap)
fmt <- label_dollar(prefix = "US$ ", big.mark = ".", accuracy = 1)

titulo_hist <- sprintf(
  "La <span style='color:%s'>**media**</span> queda muy por encima de la <span style='color:%s'>**mediana**</span>: el ingreso mundial esta sesgado",
  owid_rojo, owid_azul)

g_hist <- ggplot(hist_df, aes(gdpPercap)) +
  
  # (a) HISTOGRAMA en un solo color suave -------------------------------------
geom_histogram(binwidth = 2500, boundary = 0,
               fill = owid_azul, alpha = 0.55,
               colour = "white", linewidth = 0.2) +
  
  # (b) LINEAS DE REFERENCIA: mediana (solida) y media (punteada) -------------
geom_vline(xintercept = mediana_y, colour = owid_azul, linewidth = 0.6) +
  geom_vline(xintercept = media_y, colour = owid_rojo,
             linewidth = 0.6, linetype = "dashed") +
  annotate("text", x = mediana_y, y = Inf, vjust = 1.6, hjust = 1.08,
           label = paste0("Mediana\n", fmt(mediana_y)),
           colour = owid_azul, size = 3, lineheight = 0.9, fontface = "bold") +
  annotate("text", x = media_y, y = Inf, vjust = 1.6, hjust = -0.08,
           label = paste0("Media\n", fmt(media_y)),
           colour = owid_rojo, size = 3, lineheight = 0.9, fontface = "bold") +
  
  # (c) ANOTACION + FLECHA a la cola larga (los pocos paises ricos) -----------
annotate("text", x = 38000, y = 14, hjust = 0,
         label = "La cola larga:\npocos paises muy ricos\nestiran el promedio",
         colour = "#555555", size = 3, lineheight = 0.95) +
  annotate("curve", x = 41000, y = 11, xend = 45000, yend = 2,
           curvature = 0.25, linewidth = 0.45, colour = "#555555",
           arrow = arrow(length = unit(2.2, "mm"), type = "closed")) +
  
  # (d) ESCALAS ---------------------------------------------------------------
scale_x_continuous(labels = label_dollar(prefix = "US$ ", big.mark = ".")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  
  # (e) TITULO, SUBTITULO, FUENTE Y TEMA --------------------------------------
labs(title = titulo_hist,
     subtitle = "Cantidad de paises segun su PBI per capita (PPA), 2007",
     caption = "Datos: Gapminder (Maddison Project) - Replica didactica del estilo *Our World in Data*") +
  theme_owid()   # histograma: la grilla horizontal de theme_owid ya sirve

print(g_hist)

ggsave("clase21_histograma_editorializado.png", g_hist,
       width = 9, height = 5.6, dpi = 300, bg = "white")


# =============================================================================
# VARIANTE: GRAFICO DE DENSIDAD (misma historia, forma suavizada)
# Reemplaza el histograma por una curva. Ojo: el eje y pasa a ser densidad, no
# conteo, asi que las etiquetas con y = Inf se reubican solas (siguen arriba).
# =============================================================================
g_dens <- ggplot(hist_df, aes(gdpPercap)) +
  geom_density(fill = owid_azul, alpha = 0.4, colour = owid_azul, linewidth = 0.6) +
  geom_vline(xintercept = mediana_y, colour = owid_azul, linewidth = 0.6) +
  geom_vline(xintercept = media_y, colour = owid_rojo,
             linewidth = 0.6, linetype = "dashed") +
  scale_x_continuous(labels = label_dollar(prefix = "US$ ", big.mark = ".")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = titulo_hist,
       subtitle = "Densidad del PBI per capita entre paises (PPA), 2007",
       caption = "Datos: Gapminder - Replica didactica del estilo *Our World in Data*") +
  theme_owid()
print(g_dens)