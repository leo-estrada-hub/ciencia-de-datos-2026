# =============================================================================
# Clase 21 - Practica: Graficos editorializados en R (estilo Our World in Data)
# (1) slopechart  (2) grafico de brechas (dumbbell)  (3) ridgeline
# Ciencia de Datos para Economia y Negocios - FCE/UBA
# -----------------------------------------------------------------------------
# Archivo autocontenido (paleta + theme_owid). El ridgeline usa el paquete
# ggridges: install.packages("ggridges").
# =============================================================================

library(tidyverse)
library(ggtext)
library(scales)
library(gapminder)
library(ggridges)    # para el ridgeline (ejemplo 3)

# --- Paleta y tema reutilizables ---------------------------------------------
owid_azul   <- "#4C6A9C"
owid_rojo   <- "#B13507"
owid_verde  <- "#578145"
owid_gris   <- "#C9C9C9"

cap <- "Datos: Gapminder (Maddison Project) - Replica didactica del estilo *Our World in Data*"

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
# EJEMPLO 1 - SLOPECHART
# Historia: el reordenamiento entre dos momentos. La Argentina partia mucho mas
# rica en 1952; en 2007 varios la pasaron.
# Claves del tipo: solo DOS posiciones en x, una linea por caso, etiquetas en
# ambos extremos (reemplazan los ejes), protagonistas en color y resto en gris.
# =============================================================================

paises_slope <- c("Argentina", "Korea, Rep.", "Brazil", "China", "Japan")

slope <- gapminder %>%
  filter(year %in% c(1952, 2007), country %in% paises_slope) %>%
  mutate(
    pais = recode(country, "Korea, Rep." = "Corea del Sur"),
    destacar = case_when(country == "Argentina"    ~ "Argentina",
                         country == "Korea, Rep."  ~ "Corea del Sur",
                         TRUE                       ~ "Otros"),
    anio = factor(year, levels = c(1952, 2007))
  )

col_slope <- c("Argentina" = owid_azul, "Corea del Sur" = owid_rojo, "Otros" = owid_gris)
fmt0 <- label_dollar(prefix = "US$ ", big.mark = ".", accuracy = 1)
izq <- filter(slope, year == 1952)
der <- filter(slope, year == 2007)

titulo_slope <- sprintf(
  "En 1952 la <span style='color:%s'>**Argentina**</span> quintuplicaba a <span style='color:%s'>**Corea del Sur**</span>; en 2007 Corea ya la superaba",
  owid_azul, owid_rojo)

g_slope <- ggplot(slope, aes(anio, gdpPercap, group = country, colour = destacar)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.6) +
  # etiquetas a izquierda (solo nombre) y a derecha (nombre + valor)
  geom_text(data = izq, aes(label = pais), hjust = 1, nudge_x = -0.06,
            size = 3.2, fontface = "bold") +
  geom_text(data = der, aes(label = paste0(pais, "  ", fmt0(gdpPercap))),
            hjust = 0, nudge_x = 0.06, size = 3.2, fontface = "bold") +
  scale_colour_manual(values = col_slope) +
  scale_x_discrete(expand = expansion(mult = c(0.5, 0.85))) +
  coord_cartesian(clip = "off") +
  labs(title = titulo_slope,
       subtitle = "PBI per capita, dolares internacionales (PPA)",
       caption = cap) +
  theme_owid() +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = margin(t = 14, r = 150, b = 10, l = 90))

print(g_slope)
ggsave("clase21_slopechart_editorializado.png", g_slope,
       width = 9, height = 5.6, dpi = 300, bg = "white")


# =============================================================================
# EJEMPLO 2 - GRAFICO DE BRECHAS (dumbbell)
# Historia: cuanto avanzo cada pais en esperanza de vida entre 1952 y 2007. La
# "brecha" es el largo del segmento; se resalta el mayor avance.
# Claves del tipo: una fila por caso, dos puntos (los dos momentos) con colores
# consistentes, el segmento ES la brecha, ordenar por valor final.
# =============================================================================

paises_gap <- c("China", "Korea, Rep.", "Brazil", "Indonesia",
                "Argentina", "Japan", "United States", "Egypt")

gap <- gapminder %>%
  filter(year %in% c(1952, 2007), country %in% paises_gap) %>%
  select(country, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp, names_prefix = "a") %>%
  mutate(
    pais   = recode(country, "Korea, Rep." = "Corea del Sur",
                    "United States" = "Estados Unidos"),
    brecha = a2007 - a1952,
    pais   = fct_reorder(pais, a2007)
  )

top_gap <- gap %>% slice_max(brecha, n = 1)   # mayor avance, para resaltar

col_anios <- c("1952" = "#9a9a9a", "2007" = owid_azul)

titulo_gap <- sprintf(
  "Cuanto avanzo la esperanza de vida entre <span style='color:%s'>**1952**</span> y <span style='color:%s'>**2007**</span>",
  "#9a9a9a", owid_azul)

g_gap <- ggplot(gap, aes(y = pais)) +
  # segmento = la brecha (gris para todos)
  geom_segment(aes(x = a1952, xend = a2007, yend = pais),
               colour = "#dadada", linewidth = 1.8) +
  # resaltar el mayor avance (segmento + etiqueta en rojo)
  geom_segment(data = top_gap, aes(x = a1952, xend = a2007, yend = pais),
               colour = owid_rojo, linewidth = 1.8) +
  # los dos momentos, con colores consistentes con el titulo
  geom_point(aes(x = a1952, colour = "1952"), size = 3.2) +
  geom_point(aes(x = a2007, colour = "2007"), size = 3.2) +
  # etiqueta de la brecha al final de cada fila
  geom_text(aes(x = a2007, label = paste0("+", round(brecha), " anios")),
            hjust = -0.22, size = 2.9, colour = "#5b5b5b") +
  geom_text(data = top_gap,
            aes(x = a2007, label = paste0("+", round(brecha), " anios")),
            hjust = -0.22, size = 2.9, colour = owid_rojo, fontface = "bold") +
  scale_colour_manual(values = col_anios) +
  scale_x_continuous(expand = expansion(mult = c(0.04, 0.18))) +
  labs(title = titulo_gap,
       subtitle = "Esperanza de vida al nacer. El segmento muestra la brecha entre ambos anios.",
       caption = cap) +
  theme_owid() +
  theme(panel.grid.major.x = element_line(colour = "#e6e6e6", linewidth = 0.4),
        panel.grid.major.y = element_blank())

print(g_gap)
ggsave("clase21_brechas_dumbbell_editorializado.png", g_gap,
       width = 9, height = 5.6, dpi = 300, bg = "white")


# =============================================================================
# EJEMPLO 3 - RIDGELINE
# Historia: la distribucion mundial de la esperanza de vida se corrio a la
# derecha decada tras decada (y paso de bimodal a unimodal).
# Claves del tipo: una densidad por grupo (aca, por anio), apiladas; el orden
# del eje y cuenta el tiempo; una linea de referencia ancla el "antes".
# =============================================================================

med_1952 <- median(filter(gapminder, year == 1952)$lifeExp)
med_2007 <- median(filter(gapminder, year == 2007)$lifeExp)
g_ridge <- ggplot(gapminder, aes(x = lifeExp, y = factor(year))) +
  
  # densidades apiladas, con relleno graduado segun el valor (refuerza el eje x)
  geom_density_ridges_gradient(aes(fill = after_stat(x)),
                               scale = 2.4, colour = "white", linewidth = 0.25,
                               rel_min_height = 0.01) +
  
  # linea de referencia: mediana mundial de 1952 (se ve como todo se corre a su derecha)
  geom_vline(xintercept = med_1952, linetype = "dashed",
             colour = "#6b6b6b", linewidth = 0.4) +
  annotate("text", x = med_1952, y = Inf, vjust = 1.5, hjust = -0.05,
           label = "Mediana mundial\nen 1952", colour = "#6b6b6b",
           size = 2.9, lineheight = 0.9) +
# Linea de referencia en 2007
  geom_vline(xintercept = med_2007, linetype = "dashed",
             colour = "#6b6b6b", linewidth = 0.4) +
  annotate("text", x = med_2007, y = Inf, vjust = 1.5, hjust = -0.05,
           label = "Mediana mundial\nen 2007", colour = "#6b6b6b",
           size = 2.9, lineheight = 0.9) +
  
    
  scale_fill_gradient(low = "#d3e1f0", high = owid_azul, guide = "none") +
  scale_y_discrete(expand = expansion(add = c(0.2, 2.4))) +
  scale_x_continuous(breaks = seq(30, 90, 15)) +
  labs(title = "El mundo vive cada vez mas: la esperanza de vida se corrio a la derecha, decada tras decada",
       subtitle = "Distribucion de la esperanza de vida entre paises, por anio",
       caption = cap) +
  theme_owid() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#e6e6e6", linewidth = 0.4))

print(g_ridge)
# ggsave("clase21_ridgeline_editorializado.png", g_ridge,
#        width = 9, height = 6.2, dpi = 300, bg = "white")