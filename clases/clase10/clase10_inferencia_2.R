# =============================================================================
# Clase 9 - Práctica: Tests de hipótesis en R
# Ciencia de Datos para Economía y Negocios | FCE-UBA
# -----------------------------------------------------------------------------
# Contenidos:
#   1) t-test: diferencia de ingreso total promedio varones vs mujeres
#   2) t-test: diferencia de ingreso HORARIO promedio varones vs mujeres
#   3) Versiones ponderadas de (1) y (2) con el paquete {survey}
#   4) Test pareado: ¿cambió la esperanza de vida en los últimos 10 años?
#   5) ANOVA: ¿la región del mundo afecta el PBI per cápita?
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Paquetes y opciones
# -----------------------------------------------------------------------------
# {tidyverse} para manipulación, {survey} para inferencia con ponderadores,
# {WDI} para descargar los indicadores del Banco Mundial.
library(tidyverse)
library(survey)
library(WDI)
library(eph)
options(scipen = 999)  # evita notación científica en los prints


# =============================================================================
# BLOQUE A: EPH - preparación de la base
# =============================================================================
# Trabajamos con asalariados registrados del sector privado, con ingreso de
# ocupación principal (P21) positivo y horas de trabajo razonables.
# P47T es el ingreso TOTAL individual (incluye no laborales). Para los puntos
# 1 y 2 usamos P47T (total) e ing_horario (P21 / horas) respectivamente.
# -----------------------------------------------------------------------------

# Cargamos la EPH individual del T3 2025
eph <- get_microdata(year=2025,period = 3,'individual')
# Filtros y variables derivadas
# - ESTADO == 1     : ocupados
# - CAT_OCUP == 3   : asalariados
# - P21 > 0         : ingreso laboral positivo
# - PP3E_TOT        : horas semanales en la ocupación principal
# - CH04            : género (1 = varón, 2 = mujer)
# - P47T            : ingreso total individual (mensual)
base <- eph %>%
  filter(ESTADO == 1, CAT_OCUP == 3,
         !is.na(P21), P21 > 0,
         !is.na(PP3E_TOT), PP3E_TOT > 0, PP3E_TOT <= 84,
         !is.na(CH04),
         !is.na(P47T), P47T > 0) %>%
  mutate(
    genero      = factor(CH04, levels = c(1, 2),
                         labels = c("Varón", "Mujer")),
    horas_mes   = PP3E_TOT * 4.33,        # de semanales a mensuales
    ing_horario = P21 / horas_mes,        # ingreso laboral por hora
    ing_total   = P47T                    # ingreso total mensual
  ) %>%
  filter(ing_horario > 0)

# Revisamos tamaños muestrales por grupo
base %>% count(genero)


# =============================================================================
# 1) Diferencia de medias: ingreso TOTAL de varones vs mujeres
# =============================================================================
# H0: mu_varones = mu_mujeres     (no hay brecha en la población)
# H1: mu_varones != mu_mujeres    (bilateral; hay brecha, en cualquier sentido)
#
# Usamos Welch t-test (var.equal = FALSE, default en R): no asume varianzas
# iguales entre grupos. Por TCL, con n grande la media muestral es ~ Normal,
# así que el estadístico t es válido aunque la distribución de ingresos sea
# asimétrica.
# -----------------------------------------------------------------------------

# Medias y desvíos por grupo (foto descriptiva previa al test)
base %>%
  group_by(genero) %>%
  summarise(n        = n(),
            media    = mean(ing_total),
            sd       = sd(ing_total),
            mediana  = median(ing_total))

# Test t de dos muestras (Welch)
t_total <- t.test(ing_total ~ genero, data = base)
t_total

# Interpretación de los elementos clave del output:
# - t, df         : estadístico de prueba y grados de libertad (Welch ajusta df)
# - p-value       : probabilidad de observar una diferencia al menos tan grande
#                   como la muestral si H0 fuese cierta
# - conf.int      : IC 95% para la diferencia de medias (Varón - Mujer)
# - sample estimates: medias muestrales por grupo
#
# Regla de decisión: si p-value < 0.05 rechazamos H0 al 5%.


# =============================================================================
# 2) Diferencia de medias: ingreso HORARIO de varones vs mujeres
# =============================================================================
# Mismo esquema que (1), pero con ing_horario. Esto "limpia" el efecto de la
# cantidad de horas trabajadas: compara cuánto se paga cada hora de trabajo,
# no cuánto se cobra a fin de mes.
#
# H0: mu_varones = mu_mujeres  (en ingreso por hora)
# H1: mu_varones != mu_mujeres
# -----------------------------------------------------------------------------

# Descriptivos por grupo
base %>%
  group_by(genero) %>%
  summarise(n        = n(),
            media_h  = mean(ing_horario),
            sd_h     = sd(ing_horario),
            mediana_h= median(ing_horario))

# Test t (Welch) sobre ingreso horario
t_horario <- t.test(ing_horario ~ genero, data = base)
t_horario

# Interpretación: la comparación con el test (1) permite ver cuánto de la
# brecha en ingreso total viene por diferencia horaria y cuánto por diferencias
# en horas trabajadas.


# =============================================================================
# 3) Tests ponderados con {survey}
# =============================================================================
# La EPH es una muestra con diseño complejo: cada persona tiene un ponderador
# (PONDERA = cuántas personas de la población representa). Ignorar el
# ponderador sesga tanto el punto estimado como los errores estándar.
#
# Pasos:
#   (a) Definir el objeto de diseño con svydesign() indicando los pesos.
#   (b) Usar svyttest() en lugar de t.test().
#
# Nota práctica: svydesign admite también estratos y conglomerados. Para la
# EPH los usuarios avanzados usan CODUSU/NRO_HOGAR como PSU y aglomerado como
# estrato. Acá trabajamos con un diseño simplificado (solo pesos) para que la
# comparación con los tests no ponderados sea directa.
# -----------------------------------------------------------------------------

# Base para el diseño: agregamos PONDERA (ponderador individual)
base_svy <- eph %>%
  filter(ESTADO == 1, CAT_OCUP == 3,
         !is.na(P21), P21 > 0,
         !is.na(PP3E_TOT), PP3E_TOT > 0, PP3E_TOT <= 84,
         !is.na(CH04),
         !is.na(P47T), P47T > 0,
         !is.na(PONDERA), PONDERA > 0) %>%
  mutate(
    genero      = factor(CH04, levels = c(1, 2),
                         labels = c("Varón", "Mujer")),
    horas_mes   = PP3E_TOT * 4.33,
    ing_horario = P21 / horas_mes,
    ing_total   = P47T
  ) %>%
  filter(ing_horario > 0)

# Definimos el objeto de diseño muestral:
#   ids = ~1  -> no declaramos clusters (diseño simple con pesos)
#   weights = ~PONDERA -> el ponderador de la EPH
#   data = base_svy
dis_eph <- svydesign(ids = ~1, weights = ~PONDERA, data = base_svy)

# --- 3.a) Ingreso total, ponderado -------------------------------------------
# Medias ponderadas por grupo (para contrastar con las no ponderadas)
svyby(~ing_total, ~genero, design = dis_eph, FUN = svymean)

# Test t ponderado. La sintaxis es la misma que t.test pero con design.
t_total_w <- svyttest(ing_total ~ genero, design = dis_eph)
t_total_w

# --- 3.b) Ingreso horario, ponderado -----------------------------------------
svyby(~ing_horario, ~genero, design = dis_eph, FUN = svymean)

t_horario_w <- svyttest(ing_horario ~ genero, design = dis_eph)
t_horario_w

# Ejercicio---
# comparar el p-valor y el IC de las versiones ponderadas con las no
# ponderadas. ¿Que sucede?


# =============================================================================
# BLOQUE B: WDI - preparación de la base de países
# =============================================================================
# Vamos a usar dos indicadores del Banco Mundial:
#   - SP.DYN.LE00.IN : esperanza de vida al nacer (años)
#   - NY.GDP.PCAP.KD : PBI per cápita a precios constantes de 2015 (USD)
# Pedimos un rango de años amplio y después elegimos los años que queremos.
# -----------------------------------------------------------------------------

# Descarga
wdi_raw <- WDI(country   = "all",
                 indicator = c("esp_vida" = "SP.DYN.LE00.IN",
                               "pbi_pc"   = "NY.GDP.PCAP.KD"),
                 start     = 2013, end = 2023,
                 extra     = TRUE)

# Nos quedamos con países reales (no agregados regionales del BM)
wdi_paises <- wdi_raw %>%
  filter(region != "Aggregates")


# =============================================================================
# 4) Test pareado: ¿cambió la esperanza de vida en los últimos 10 años?
# =============================================================================
# Planteo: comparamos esperanza de vida del país i en t vs t-10. Cada país
# aparece DOS veces (una por año), y las observaciones NO son independientes
# entre sí (Japón en 2013 y Japón en 2023 están correlacionados). Por eso
# corresponde un test PAREADO (paired = TRUE): equivalente a un t de una
# muestra sobre las diferencias d_i = X_{i, t} - X_{i, t-10}.
#
# H0: media de las diferencias = 0   (no hubo cambio en promedio)
# H1: media de las diferencias != 0  (sí hubo cambio)
#
# Elegimos 2013 y 2023 como extremos del período (10 años completos).
# -----------------------------------------------------------------------------

# Armamos la base ancha: una fila por país, columnas para cada año
esp_vida <- wdi_paises %>%
  filter(year %in% c(2013, 2023), !is.na(esp_vida)) %>%
  select(iso3c, country, region, year, esp_vida) %>%
  pivot_wider(names_from = year, values_from = esp_vida,
              names_prefix = "ev_") %>%
  filter(!is.na(ev_2013), !is.na(ev_2023)) %>%   # países con ambos años
  mutate(diff = ev_2023 - ev_2013)

# Descriptivos de la diferencia (es sobre esto que corre el test pareado)
esp_vida %>%
  summarise(n           = n(),
            media_diff  = mean(diff),
            sd_diff     = sd(diff),
            min_diff    = min(diff),
            max_diff    = max(diff))

# Test pareado. Dos formas equivalentes:
# (a) Pasando los dos vectores con paired = TRUE
t_pareado <- t.test(esp_vida$ev_2023, esp_vida$ev_2013, paired = TRUE)
t_pareado

# (b) Equivalentemente, un t de una muestra sobre las diferencias
t.test(esp_vida$diff, mu = 0)

# Interpretación:
# - mean of the differences: cambio promedio en años de esperanza de vida.
# - p-value: probabilidad de observar un cambio al menos tan grande si H0
#   (no hubo cambio) fuese cierta.
# - conf.int: IC 95% para el cambio promedio de esperanza de vida por país.

# =============================================================================
# 5) ANOVA: ¿la región afecta al PBI per cápita?
# =============================================================================
# Comparamos medias de PBI per cápita entre las regiones definidas por el
# Banco Mundial. Con más de dos grupos NO usamos t-test (inflaría el error
# tipo I por múltiples comparaciones): usamos ANOVA de un factor.
#
# H0: mu_region1 = mu_region2 = ... = mu_regionK  (todas iguales)
# H1: al menos una media regional difiere del resto
#
# El estadístico F compara varianza ENTRE grupos vs DENTRO de grupos:
#   F grande -> los grupos son muy distintos entre sí respecto de su
#               dispersión interna -> tiende a rechazar H0.
#
# Elegimos el último año disponible (2023) para tener una foto transversal.
# -----------------------------------------------------------------------------

# Base para ANOVA: un país por fila, en 2023, con PBI per cápita y región
df_anova <- wdi_paises %>%
  filter(year == 2023, !is.na(pbi_pc), !is.na(region)) %>%
  select(iso3c, country, region, pbi_pc) %>%
  mutate(region = factor(region))

# Descriptivos por región (foto previa)
df_anova %>%
  group_by(region) %>%
  summarise(n     = n(),
            media = mean(pbi_pc),
            sd    = sd(pbi_pc),
            min   = min(pbi_pc),
            max   = max(pbi_pc)) %>%
  arrange(desc(media))

# Ajuste del ANOVA
modelo_anova <- aov(pbi_pc ~ region, data = df_anova)
summary(modelo_anova)

# Interpretación del summary():
# - Df              : grados de libertad (entre = k-1, residuales = n-k)
# - Sum Sq, Mean Sq : sumas y cuadrados medios
# - F value         : estadístico F
# - Pr(>F)          : p-valor. Si < 0.05 rechazamos H0 al 5%.
#
# Si rechazamos H0, sabemos que HAY diferencias, pero no ENTRE CUÁLES regiones.
# Para eso hacemos comparaciones múltiples post-hoc con Tukey HSD, que
# controla el error tipo I familywise.
tukey <- TukeyHSD(modelo_anova)
tukey

# Cada fila del Tukey muestra:
# - diff   : diferencia de medias entre dos regiones
# - lwr/upr: IC 95% ajustado para esa diferencia
# - p adj  : p-valor ajustado por múltiples comparaciones
# Si el IC no contiene 0 (o p adj < 0.05), esas dos regiones difieren
# significativamente.

# Chequeo de supuestos:
# - Homocedasticidad: ANOVA clásico asume varianzas iguales entre grupos.
#   Si las varianzas son muy distintas (mirar los sd por grupo), conviene
#   oneway.test(pbi_pc ~ region, data = df_anova) que es la versión Welch
#   y no asume igualdad de varianzas.
oneway.test(pbi_pc ~ region, data = df_anova)   # Welch-ANOVA
# Para Tukey
library(rstatix)
df_anova %>%
  games_howell_test(pbi_pc ~ region)
