# -----------------------------------------------------------------------------
# 0) PALETA Y TEMA REUTILIZABLES  <- esto es lo que mas vas a reusar
# -----------------------------------------------------------------------------
owid_azul   <- "#4A8A9C"
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