# =============================================================================
# Gapminder interactivo al estilo OWID
# Basado en el gráfico de la clase 19 de Visualización con ggplot2
# Requiere: plotly, tidyverse, WDI
# =============================================================================

library(tidyverse)
library(WDI)
library(plotly)
library(scales)

# -----------------------------------------------------------------------------
# 1. DATOS (igual que en la clase)
# -----------------------------------------------------------------------------

wdi_raw <- WDI(
  indicator = c(
    gdppc     = "NY.GDP.PCAP.PP.KD",
    lifeExp   = "SP.DYN.LE00.IN",
    poblacion = "SP.POP.TOTL"
  ),
  country = "all",
  start   = 2000,
  end     = 2022,
  extra   = TRUE
)

wdi <- wdi_raw |>
  filter(region != "Aggregates", !is.na(gdppc), !is.na(lifeExp)) |>
  select(pais = country, anio = year, gdppc, lifeExp, poblacion,
         region, income)

wdi_2019 <- wdi |> filter(anio == 2019)

promedio_mundial <- mean(wdi_2019$lifeExp, na.rm = TRUE)

# -----------------------------------------------------------------------------
# 2. PALETA DE COLORES (misma que la clase)
# -----------------------------------------------------------------------------

paleta_regiones <- c(
  "East Asia & Pacific"        = "#d15573",
  "Europe & Central Asia"      = "#e8c468",
  "Latin America & Caribbean"  = "#f5b88c",
  "Middle East & North Africa" = "#b89bc7",
  "North America"              = "#8a3a52",
  "South Asia"                 = "#6c9ed0",
  "Sub-Saharan Africa"         = "#9cccd8"
)

# -----------------------------------------------------------------------------
# 3. PREPARACIÓN DEL DATAFRAME PARA PLOTLY
# -----------------------------------------------------------------------------

# Función auxiliar para formatear población (evita problemas con label_number)
format_pop <- function(x) {
  case_when(
    x >= 1e9  ~ paste0(round(x / 1e9, 2), " mil millones"),
    x >= 1e6  ~ paste0(round(x / 1e6, 1), " millones"),
    x >= 1e3  ~ paste0(round(x / 1e3, 1), " mil"),
    TRUE      ~ as.character(x)
  )
}

# Escala de tamaño proporcional a la población (como en OWID)
# Plotly usa el área del círculo ~ sqrt(pop), escalamos a un rango visible
pop_min <- min(wdi_2019$poblacion, na.rm = TRUE)
pop_max <- max(wdi_2019$poblacion, na.rm = TRUE)

wdi_2019 <- wdi_2019 |>
  mutate(
    # Tamaño visual: raíz cuadrada escalada a rango [4, 40]
    size_viz = 4 + 36 * (sqrt(poblacion) - sqrt(pop_min)) /
      (sqrt(pop_max) - sqrt(pop_min)),
    
    # Texto del tooltip al estilo OWID
    tooltip = paste0(
      "<b>", pais, "</b><br>",
      "Año: 2019<br>",
      "<br>",
      "PBI per cápita, PPP (USD constantes 2017)<br>",
      "<b>", dollar(gdppc, accuracy = 1, big.mark = ".", decimal.mark = ","), "</b><br>",
      "<br>",
      "Esperanza de vida al nacer<br>",
      "<b>", round(lifeExp, 1), " años</b><br>",
      "<br>",
      "Población<br>",
      "<b>", format_pop(poblacion), "</b>"
    ),
    
    # Color según paleta manual
    color_hex = paleta_regiones[region]
  )

# -----------------------------------------------------------------------------
# 4. CONSTRUCCIÓN DEL GRÁFICO PLOTLY
# -----------------------------------------------------------------------------

# Construimos una traza por región para que la leyenda permita
# highlight/hide individual por región (como el ejemplo de OWID)

regiones <- unique(wdi_2019$region) |> sort()

fig <- plot_ly()

# Línea de referencia: promedio mundial
fig <- fig |>
  add_segments(
    x    = ~min(wdi_2019$gdppc, na.rm = TRUE) * 0.8,
    xend = ~max(wdi_2019$gdppc, na.rm = TRUE) * 1.2,
    y    = promedio_mundial,
    yend = promedio_mundial,
    line       = list(color = "gray", width = 1, dash = "dash"),
    showlegend = FALSE,
    hoverinfo  = "none"
  ) |>
  # Anotación del promedio mundial
  add_annotations(
    x         = log10(800),   # posición en escala log
    y         = promedio_mundial + 0.8,
    text      = paste0("Promedio mundial: ", round(promedio_mundial, 1), " años"),
    showarrow = FALSE,
    font      = list(size = 10, color = "gray50"),
    xref      = "x",
    yref      = "y"
  )

# Una traza por región (permite leyenda interactiva con highlight)
for (reg in regiones) {
  df_reg <- wdi_2019 |> filter(region == reg)
  color_reg <- paleta_regiones[reg]
  
  fig <- fig |>
    add_trace(
      data        = df_reg,
      x           = ~gdppc,
      y           = ~lifeExp,
      type        = "scatter",
      mode        = "markers",
      name        = reg,
      marker      = list(
        size    = ~size_viz,
        color   = color_reg,
        opacity = 0.7,
        line    = list(color = "white", width = 0.5)
      ),
      text        = ~tooltip,
      hoverinfo   = "text",
      hoverlabel  = list(
        bgcolor   = "white",
        bordercolor = color_reg,
        font      = list(size = 12, color = "black")
      )
    )
}

# -----------------------------------------------------------------------------
# 5. LAYOUT: ejes, título, tema OWID-like
# -----------------------------------------------------------------------------

fig <- fig |>
  layout(
    title = list(
      text = paste0(
        "<b>Más rico, más vida: PBI per cápita y esperanza de vida</b><br>",
        "<span style='font-size:11px;color:gray'>",
        "Cada círculo es un país. El tamaño es proporcional a la población. ",
        "Datos para 2019. PBI en USD PPP constantes 2017.</span>"
      ),
      x    = 0.02,
      xref = "paper",
      font = list(size = 15)
    ),
    xaxis = list(
      title      = "PBI per cápita, PPP (USD constantes 2017, escala logarítmica)",
      type       = "log",
      tickformat = "$,.0f",
      tickvals   = c(500, 1000, 2000, 5000, 10000, 20000, 50000, 100000),
      ticktext   = c("$500", "$1.000", "$2.000", "$5.000",
                     "$10.000", "$20.000", "$50.000", "$100.000"),
      showgrid   = TRUE,
      gridcolor  = "#ebebeb",
      zeroline   = FALSE,
      tickfont   = list(size = 10)
    ),
    yaxis = list(
      title    = "Esperanza de vida al nacer (años)",
      range    = c(50, 90),
      showgrid = TRUE,
      gridcolor = "#ebebeb",
      zeroline  = FALSE,
      tickfont  = list(size = 10)
    ),
    legend = list(
      title       = list(text = "<b>Región</b>"),
      orientation = "v",
      x           = 1.01,
      y           = 0.95,
      bgcolor     = "rgba(255,255,255,0.8)",
      bordercolor = "#cccccc",
      borderwidth = 1,
      font        = list(size = 11),
      # Clicking en leyenda: isolate (solo esa región) / double-click: volver a todo
      itemclick       = "toggleothers",
      itemdoubleclick = "toggle"
    ),
    paper_bgcolor = "white",
    plot_bgcolor  = "white",
    font          = list(family = "Arial, sans-serif"),
    margin        = list(l = 70, r = 180, t = 80, b = 70),
    annotations   = list(
      list(
        text      = "Fuente: Banco Mundial, World Development Indicators (WDI).",
        x         = 0,
        y         = -0.12,
        xref      = "paper",
        yref      = "paper",
        showarrow = FALSE,
        font      = list(size = 9, color = "gray60"),
        align     = "left"
      )
    ),
    hoverdistance = 5   # sensibilidad del hover
  ) |>
  config(
    displaylogo  = FALSE,
    modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
    toImageButtonOptions = list(
      format   = "svg",
      filename = "gapminder_2019",
      width    = 900,
      height   = 600
    )
  )

# -----------------------------------------------------------------------------
# 6. Ver el grafico
# -----------------------------------------------------------------------------

# Para guardar como HTML autocontenido:
htmlwidgets::saveWidget(fig, "gapminder_interactivo.html", selfcontained = TRUE)
# Luego abrirlo localmente 