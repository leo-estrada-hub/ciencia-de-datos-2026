# ============================================================================
# Clase 16 - Práctica 4: Índices compuestos - IDH y variante contra EEUU
# ----------------------------------------------------------------------------
# Autor: Nicolás Sidicaro
# Curso: Ciencia de Datos para Economía y Negocios | FCE-UBA
#
# Objetivos:
#   (A) Reconstruir el IDH oficial paso a paso
#       - Sub-índice de educación a partir de DOS dimensiones ponderadas
#         (años esperados de escolaridad y años promedio de escolaridad)
#       - Normalización min-max con cotas fijas del PNUD
#       - Agregación geométrica vs aritmética
#       - Análisis de sensibilidad a la ponderación intra-educación
#
#   (B) Variante: índice tipo IDH pero indexado contra EEUU
#       - Cada componente como ratio respecto al valor de EEUU
#       - EEUU = 1 por construcción
#       - Permite leer directamente "X país equivale al Y% del nivel de EEUU
#         en cada dimensión"
#
# Datos: HDR25 - Statistical Annex - Table 1 (HDI), PNUD
#        Variables:
#          - Esperanza de vida al nacer (años)
#          - Años esperados de escolaridad
#          - Años promedio de escolaridad
#          - INB per cápita (PPP 2021 USD)
# ============================================================================

# ---- 0. Paquetes y rutas ---------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(stringr)

f_idh <- "datos/HDR25_Statistical_Annex_HDI_Table.xlsx"


# ---- 1. Lectura y limpieza -------------------------------------------------

idh_raw <- read_excel(f_idh, sheet = "Table 1. HDI", skip = 7)

# La estructura del archivo: columnas relevantes son 1, 2, 3, 5, 7, 9, 11
# (entre medio hay columnas de notas/letras)
idh <- idh_raw |>
  select(1, 2, 3, 5, 7, 9, 11) |>
  setNames(c("rank", "pais", "idh_oficial",
             "esp_vida", "anios_esp_educ", "anios_prom_educ",
             "inb_pc")) |>
  filter(!is.na(as.numeric(idh_oficial))) |>     # filtra encabezados intermedios
  mutate(across(c(idh_oficial, esp_vida, anios_esp_educ,
                  anios_prom_educ, inb_pc), as.numeric)) |>
  filter(!is.na(esp_vida), !is.na(inb_pc),
         !is.na(anios_esp_educ), !is.na(anios_prom_educ))
View(idh)
# ============================================================================
# (A) RECONSTRUCCIÓN DEL IDH OFICIAL
# ============================================================================
#
# El IDH combina tres dimensiones:
#   1. Salud (esperanza de vida al nacer)
#   2. Educación (combinación de años esperados y años promedio)
#   3. Ingreso (INB per cápita, PPP)
#
# Cada dimensión se normaliza min-max con cotas teóricas FIJAS (PNUD):
#   - Esperanza de vida:        mín = 20,  máx = 85
#   - Años esperados educ.:     mín = 0,   máx = 18
#   - Años promedio educ.:      mín = 0,   máx = 15
#   - INB per cápita (log):     mín = 100, máx = 75.000
#
# Sub-índice de educación: combinación de las DOS dimensiones educativas.
# IDH final: media geométrica de los tres sub-índices.
# ----------------------------------------------------------------------------

# ---- A.1 Cotas oficiales PNUD ----------------------------------------------

COTAS <- list(
  esp_vida        = c(min = 20,  max = 85),
  anios_esp_educ  = c(min = 0,   max = 18),
  anios_prom_educ = c(min = 0,   max = 15),
  inb_pc          = c(min = 100, max = 75000)
)


# ---- A.2 Función de normalización min-max ---------------------------------

normalizar_minmax <- function(x, min_val, max_val) {
  z <- (x - min_val) / (max_val - min_val)
  pmin(pmax(z, 0), 1)   # clipear al rango [0, 1]
}


# ---- A.3 Función para construir el SUB-ÍNDICE DE EDUCACIÓN -----------------
#
# Lo armamos como combinación de las dos sub-variables normalizadas, con
# ponderación CONFIGURABLE. Por default 50-50, pero se puede cambiar.
#
# Argumentos:
#   - educ_esp_norm: años esperados normalizados
#   - educ_prom_norm: años promedio normalizados
#   - w_esp: peso de años esperados (default 0.5)
#   - w_prom: peso de años promedio (default 0.5)
#   - metodo: "geometrica" (oficial post-2010) o "aritmetica"
# ----------------------------------------------------------------------------

construir_indice_educacion <- function(educ_esp_norm, educ_prom_norm,
                                        w_esp = 0.5, w_prom = 0.5,
                                        metodo = "geometrica") {
  # Re-normalizar los pesos para que sumen 1
  total_w <- w_esp + w_prom
  w_esp <- w_esp / total_w
  w_prom <- w_prom / total_w

  if (metodo == "geometrica") {
    (educ_esp_norm^w_esp) * (educ_prom_norm^w_prom)
  } else if (metodo == "aritmetica") {
    w_esp * educ_esp_norm + w_prom * educ_prom_norm
  } else {
    stop("metodo debe ser 'geometrica' o 'aritmetica'")
  }
}


# ---- A.4 Función general para construir el IDH ----------------------------
#
# Toma los tres sub-índices ya normalizados y los agrega.
# Permite cambiar pesos y método de agregación.
# ----------------------------------------------------------------------------

construir_idh <- function(salud_norm, educ_norm, ingreso_norm,
                          w_s = 1, w_e = 1, w_i = 1,
                          metodo = "geometrica") {
  total <- w_s + w_e + w_i
  w_s <- w_s / total
  w_e <- w_e / total
  w_i <- w_i / total

  if (metodo == "geometrica") {
    (salud_norm^w_s) * (educ_norm^w_e) * (ingreso_norm^w_i)
  } else if (metodo == "aritmetica") {
    w_s * salud_norm + w_e * educ_norm + w_i * ingreso_norm
  } else {
    stop("metodo debe ser 'geometrica' o 'aritmetica'")
  }
}


# ---- A.5 Aplicación: normalizar las tres dimensiones -----------------------

idh_norm <- idh |>
  mutate(
    # Salud
    salud_norm = normalizar_minmax(esp_vida,
                                    COTAS$esp_vida["min"],
                                    COTAS$esp_vida["max"]),
    # Educación: dos sub-variables normalizadas
    educ_esp_norm  = normalizar_minmax(anios_esp_educ,
                                        COTAS$anios_esp_educ["min"],
                                        COTAS$anios_esp_educ["max"]),
    educ_prom_norm = normalizar_minmax(anios_prom_educ,
                                        COTAS$anios_prom_educ["min"],
                                        COTAS$anios_prom_educ["max"]),
    # Educación: índice combinado (50-50, geométrica = método PNUD)
    educ_norm = construir_indice_educacion(educ_esp_norm, educ_prom_norm,
                                            w_esp = 0.5, w_prom = 0.5,
                                            metodo = "geometrica"),
    # Ingreso (en log, como el PNUD)
    ingreso_norm = normalizar_minmax(log(pmax(inb_pc, COTAS$inb_pc["min"])),
                                      log(COTAS$inb_pc["min"]),
                                      log(COTAS$inb_pc["max"]))
  )


# ---- A.6 Calcular el IDH reconstruido y comparar con el oficial ------------

idh_norm <- idh_norm |>
  mutate(
    idh_reconstruido = construir_idh(salud_norm, educ_norm, ingreso_norm,
                                      w_s = 1, w_e = 1, w_i = 1,
                                      metodo = "geometrica")
  )

# Comparación
idh_norm |>
  select(pais, idh_oficial, idh_reconstruido) |>
  mutate(dif = idh_reconstruido - idh_oficial) |>
  arrange(desc(abs(dif))) |>
  head(20)

# Scatter
ggplot(idh_norm, aes(x = idh_oficial, y = idh_reconstruido)) +
  geom_abline(slope = 1, intercept = 0, color = "tomato", linetype = "dashed") +
  geom_point(alpha = 0.6, color = "steelblue") +
  labs(x = "IDH oficial PNUD", y = "IDH reconstruido",
       title = "Validación: nuestro IDH replica al oficial",
       subtitle = "Las diferencias pequeñas se deben a redondeos en los datos publicados") +
  theme_minimal(base_size = 11)


# ---- A.7 Sensibilidad a la ponderación intra-educación ---------------------
#
# Pregunta: ¿qué pasa si dentro del sub-índice de educación pondero más a
# los años promedio (que reflejan stock acumulado) que a los años esperados
# (que reflejan futuro)?
# ----------------------------------------------------------------------------

escenarios_educ <- tibble(
  escenario = c("50-50 (PNUD)", "70-30 (esp dominante)",
                 "30-70 (prom dominante)", "100-0 (solo esp)",
                 "0-100 (solo prom)"),
  w_esp =  c(0.5, 0.7, 0.3, 1.0, 0.0),
  w_prom = c(0.5, 0.3, 0.7, 0.0, 1.0)
)

sensibilidad_educ <- escenarios_educ |>
  rowwise() |>
  mutate(
    datos = list(
      idh_norm |>
        mutate(
          educ_alt = construir_indice_educacion(
            educ_esp_norm, educ_prom_norm,
            w_esp = w_esp, w_prom = w_prom,
            metodo = "geometrica"
          ),
          idh_alt = construir_idh(salud_norm, educ_alt, ingreso_norm,
                                   metodo = "geometrica"),
          rank_alt = rank(-idh_alt, ties.method = "min")
        ) |>
        select(pais, idh_alt, rank_alt)
    )
  ) |>
  unnest(datos) |>
  ungroup()

# Top 15 países según el escenario base
top15 <- sensibilidad_educ |>
  filter(escenario == "50-50 (PNUD)") |>
  arrange(desc(idh_alt)) |>
  slice_head(n = 15) |>
  pull(pais)

sensibilidad_educ |>
  filter(pais %in% top15) |>
  ggplot(aes(x = reorder(pais, -rank_alt),
             y = rank_alt,
             color = escenario, group = escenario)) +
  geom_line(linewidth = 0.5, alpha = 0.7) +
  geom_point(size = 2) +
  scale_y_reverse(breaks = 1:25) +
  coord_flip() +
  labs(x = NULL, y = "Ranking",
       color = "Pesos (esp - prom)",
       title = "Sensibilidad del ranking IDH a la ponderación intra-educación",
       subtitle = "Top 15 países según escenario PNUD oficial",
       caption = "Fuente: HDR 2025, PNUD") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8))


# ============================================================================
# (B) ÍNDICE TIPO IDH INDEXADO CONTRA EEUU
# ============================================================================
#
# Idea: en vez de normalizar con cotas teóricas del PNUD, normalizar cada
# dimensión usando EEUU como referente. Cada país queda como ratio EEUU=1.
#
#   ratio_dimension = valor_pais / valor_eeuu
#
# Lectura:
#   - ratio > 1: el país supera a EEUU en esa dimensión
#   - ratio = 1: el país iguala a EEUU
#   - ratio < 1: el país está por debajo de EEUU
#
# Para el INGRESO usamos log(INB), manteniendo la lógica del IDH oficial
# (donde el ingreso entra en log porque la utilidad marginal del ingreso
# es decreciente). Para educación, agregamos las dos sub-variables igual
# que en el IDH oficial.
# ----------------------------------------------------------------------------

# ---- B.1 Identificar los valores de EEUU -----------------------------------

eeuu <- idh |> filter(pais == "United States")
eeuu

# Crear vector de referencia
ref_eeuu <- list(
  esp_vida        = eeuu$esp_vida,
  anios_esp_educ  = eeuu$anios_esp_educ,
  anios_prom_educ = eeuu$anios_prom_educ,
  log_inb_pc      = log(eeuu$inb_pc)
)
ref_eeuu


# ---- B.2 Construir ratios contra EEUU --------------------------------------

idh_eeuu <- idh |>
  mutate(
    # Ratios directos (cada dimensión / EEUU)
    ratio_salud         = esp_vida / ref_eeuu$esp_vida,
    ratio_educ_esp      = anios_esp_educ / ref_eeuu$anios_esp_educ,
    ratio_educ_prom     = anios_prom_educ / ref_eeuu$anios_prom_educ,
    ratio_ingreso       = log(pmax(inb_pc, 100)) / ref_eeuu$log_inb_pc,

    # Sub-índice de educación: media geométrica de los dos ratios educativos
    # (manteniendo la lógica del IDH oficial)
    ratio_educ = sqrt(ratio_educ_esp * ratio_educ_prom),

    # Índice contra EEUU: media geométrica de los tres componentes
    indice_vs_eeuu = (ratio_salud * ratio_educ * ratio_ingreso)^(1/3)
  )


# ---- B.3 Verificación: EEUU debe dar exactamente 1 ------------------------

idh_eeuu |>
  filter(pais == "United States") |>
  select(pais, ratio_salud, ratio_educ, ratio_ingreso, indice_vs_eeuu)


# ---- B.4 Ranking contra EEUU ----------------------------------------------

# Países que SUPERAN a EEUU (índice > 1)
superan_eeuu <- idh_eeuu |>
  filter(indice_vs_eeuu > 1) |>
  arrange(desc(indice_vs_eeuu)) |>
  select(pais, ratio_salud, ratio_educ, ratio_ingreso, indice_vs_eeuu)

superan_eeuu

# Argentina y comparación regional
seleccion <- c("United States", "Norway", "Iceland", "Switzerland",
                "Germany", "Japan", "Korea (Republic of)",
                "Argentina", "Chile", "Uruguay", "Brazil", "Mexico",
                "China", "India", "South Africa", "Nigeria")

idh_eeuu |>
  filter(pais %in% seleccion) |>
  select(pais, ratio_salud, ratio_educ, ratio_ingreso, indice_vs_eeuu) |>
  arrange(desc(indice_vs_eeuu))


# ---- B.5 Visualización: descomposición de la brecha contra EEUU -----------

datos_grafico <- idh_eeuu |>
  filter(pais %in% seleccion) |>
  select(pais, ratio_salud, ratio_educ, ratio_ingreso) |>
  pivot_longer(-pais, names_to = "dimension", values_to = "ratio") |>
  mutate(
    dimension = recode(dimension,
                       "ratio_salud"   = "Salud",
                       "ratio_educ"    = "Educación",
                       "ratio_ingreso" = "Ingreso (log)"),
    dimension = factor(dimension, levels = c("Salud", "Educación",
                                              "Ingreso (log)"))
  )

# Ordenar países según el índice agregado
orden_paises <- idh_eeuu |>
  filter(pais %in% seleccion) |>
  arrange(indice_vs_eeuu) |>
  pull(pais)

datos_grafico |>
  mutate(pais = factor(pais, levels = orden_paises)) |>
  ggplot(aes(x = pais, y = ratio, fill = dimension)) +
  geom_col(position = "dodge", width = 0.8) +
  geom_hline(yintercept = 1, color = "tomato",
             linetype = "dashed", linewidth = 0.7) +
  coord_flip() +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_manual(values = c("Salud" = "steelblue",
                                "Educación" = "#2ca02c",
                                "Ingreso (log)" = "tomato")) +
  labs(x = NULL, y = "Ratio contra EEUU (EEUU = 100%)",
       fill = "Dimensión",
       title = "Países seleccionados vs Estados Unidos por dimensión",
       subtitle = "Cada barra muestra qué porcentaje del nivel de EEUU alcanza el país",
       caption = "Fuente: HDR 2025, PNUD") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")


# ---- B.7 IDH oficial vs índice contra EEUU: ¿cambia mucho el ranking? ------

comparacion_indices <- idh_eeuu |>
  select(pais, idh_oficial, indice_vs_eeuu) |>
  mutate(
    rank_oficial = rank(-idh_oficial, ties.method = "min"),
    rank_eeuu    = rank(-indice_vs_eeuu, ties.method = "min"),
    cambio_rank  = rank_oficial - rank_eeuu
  ) |>
  arrange(desc(abs(cambio_rank)))

head(comparacion_indices, 20)