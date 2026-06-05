# =============================================================================
# Clase 21 - Practica: Graficos editorializados en R (estilo Our World in Data)
# MAPA COROPLETICO PROVINCIAL (Argentina)
# Ciencia de Datos para Economia y Negocios - FCE/UBA
# -----------------------------------------------------------------------------
# Necesita geometria provincial. Aca uso el paquete geoAr (lo descarga la
# primera vez, requiere internet). Alternativa sin internet: leer un shapefile
# o geojson local del IGN con sf::st_read("provincias.geojson").
#   install.packages(c("sf", "geoAr"))
# =============================================================================

library(tidyverse)
library(sf)
library(geoAr)
library(ggtext)
library(scales)

# --- Paleta y tema de MAPA (theme_void con estetica OWID) --------------------
owid_azul <- "#4C6A9C"
cap <- "Datos: simulados con fines didacticos - Replica del estilo *Our World in Data*"

theme_owid_map <- function(base_size = 13) {
  theme_void(base_size = base_size) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_markdown(face = "bold", size = rel(1.3),
                                       colour = "#1d1d1d", lineheight = 1.2,
                                       margin = margin(b = 4)),
      plot.subtitle = element_markdown(size = rel(0.98), colour = "#5b5b5b",
                                       margin = margin(b = 14)),
      plot.caption  = element_markdown(hjust = 0, size = rel(0.72),
                                       colour = "#8a8a8a", margin = margin(t = 12)),
      legend.position = "bottom",
      legend.title    = element_text(size = rel(0.8), colour = "#5b5b5b"),
      legend.text     = element_text(size = rel(0.72), colour = "#5b5b5b"),
      plot.margin     = margin(14, 16, 10, 16)
    )
}


# -----------------------------------------------------------------------------
# 1) GEOMETRIA PROVINCIAL  (geoAr + recorte para sacar la Antartida y
#    conservar el continente, Malvinas y Tierra del Fuego)
# -----------------------------------------------------------------------------
arg <- get_geo("ARGENTINA", level = "provincia") %>%
  add_geo_codes() %>%
  st_make_valid()

arg <- st_crop(arg, st_bbox(c(xmin = -74, xmax = -52, ymin = -56, ymax = -21),
                            crs = st_crs(arg)))

# -----------------------------------------------------------------------------
# 2) DATOS POR PROVINCIA
#    Aca SIMULO un indicador para que el mapa corra. En vivo, reemplazalo por
#    tu base de Argendata uniendo por la columna de nombres ISO (name_iso):
#
#    mapa_datos <- arg %>%
#      left_join(tus_datos, by = c("name_iso" = "provincia"))
#
#    Ojo con la clave de union: name_iso trae el nombre completo
#    ("Ciudad Autonoma de Buenos Aires", "Tierra del Fuego..."). Si tu base usa
#    codigos INDEC, arma un diccionario tribble(~codigo, ~name_iso, ...) y uni.
# -----------------------------------------------------------------------------
set.seed(2026)
mapa_datos <- arg %>%
  mutate(indicador = round(runif(n(), 20, 85), 1))

# provincia con el valor mas alto, para anotarla directamente sobre el mapa
destacada <- mapa_datos %>% slice_max(indicador, n = 1)


# -----------------------------------------------------------------------------
# 3) MAPA EDITORIALIZADO
# -----------------------------------------------------------------------------
titulo_mapa <- "Titular del mapa: la conclusion que queres que se lea"  # <- reescribir

g_mapa <- ggplot(mapa_datos) +
  
  # (a) coropletas: una variable continua por provincia -----------------------
geom_sf(aes(fill = indicador), colour = "white", linewidth = 0.2) +
  
  # (b) etiqueta directa sobre la provincia que mas importa --------------------
geom_sf_text(data = destacada, aes(label = name_iso),
             size = 3, fontface = "bold", colour = "#1d1d1d") +
  
  # (c) escala en TRAMOS (binned), tipo OWID, en vez de gradiente continuo -----
scale_fill_fermenter(palette = "Blues", direction = 1, n.breaks = 5,
                     name = "Indicador (unidad)") +
  
  coord_sf(expand = FALSE) +
  labs(title = titulo_mapa,
       subtitle = "Que mide el indicador, unidad y periodo",
       caption = cap) +
  theme_owid_map() +
  # leyenda como barra horizontal escalonada abajo
  guides(fill = guide_colorsteps(barwidth = 14, barheight = 0.5,
                                 title.position = "top", title.hjust = 0))

print(g_mapa)

# Para mapas conviene formato vertical
ggsave("clase21_mapa_coropletico_provincial.png", g_mapa,
       width = 7, height = 10, dpi = 300, bg = "white")


# =============================================================================
# NOTA: si en lugar de tramos preferis un gradiente continuo, cambia el bloque
# (c) por:
#   scale_fill_distiller(palette = "Blues", direction = 1, name = "Indicador (unidad)") +
#   ... y guides(fill = guide_colourbar(barwidth = 14, barheight = 0.5,
#                                       title.position = "top"))
# =============================================================================