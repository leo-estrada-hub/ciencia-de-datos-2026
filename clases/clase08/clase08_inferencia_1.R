# ============================================================================
# Ciencia de Datos para Economía y Negocios - FCE-UBA
# Clase grabada: Inferencia estadística aplicada
# ----------------------------------------------------------------------------
# Temas:
#   1. Estadísticas descriptivas con datos reales
#   2. Simulación de muestreo y Teorema Central del Límite (TCL)
#   3. Construcción de intervalos de confianza
#   4. Código integrador: loops + bind_rows
# ----------------------------------------------------------------------------
# Datos: nycflights13 (todos los vuelos que salieron de NYC en 2013,
#        provenientes de los tres aeropuertos de la ciudad: JFK, LGA y EWR).
# ============================================================================


# ----------------------------------------------------------------------------
# 0. Preparación del entorno
# ----------------------------------------------------------------------------

# Librerías que vamos a usar. Si no tienen alguna, la instalan con install.packages().

library(tidyverse)     # dplyr, ggplot2, tidyr, etc.
library(nycflights13)  # los datos que vamos a usar como "población"
library(scales)        # para formatear ejes de los gráficos

# Fijamos una semilla para que las simulaciones sean reproducibles.
# Esto es MUY importante: si cambian la semilla, los resultados cambian,
# pero la lógica es la misma. Hoy usamos 1234 para que todos obtengamos
# lo mismo si corren el código en sus computadoras.

set.seed(1234)

# Una cosa sobre las seed que puede ser un tanto confusa. 
# Si ejecutamos un codigo que nos devuelve un valor aleatorio: 
runif(1,0,5)
# Obtenemos un valor (en mi caso 0.568 porque es la primera aleatorizacion que hice
# luego de definir el seed)
# Pero qué pasa si lo vuelvo a ejecutar 
runif(1,0,5)
# Obtuve 3.111 ¿por que? Lo que hace el seed es definir una "suerte"
# Entonces cada aleatorización que realice, paso a la siguiente tirada del dado. 
# Pero si vuelvo a correr set.seed(1234) y runif(1,0,5) voy a volver a obtener 0.568
set.seed(1234)
runif(1,0,5)
# Taran. Es decir, el orden importa. 

# ============================================================================
# 1. LA PREGUNTA QUE GUÍA LA CLASE
# ============================================================================
#
# Imaginen que somos analistas de una aerolínea y queremos entender
# cuánto se demoran, en promedio, los vuelos que salen de los aeropuertos
# de Nueva York. La pregunta aplicada es:
#
#   ¿Cuál es la demora promedio de salida (en minutos) de los vuelos
#    que partieron de NYC en 2013?
#
# Para responderla con certeza, necesitaríamos los datos de TODOS los vuelos.
# Por suerte, hoy los tenemos: el paquete nycflights13 contiene la población
# completa de ese año. Esto es raro en la vida real (casi nunca tenemos
# toda la población), pero es justo lo que necesitamos para aprender
# inferencia: vamos a hacer de cuenta que NO conocemos la población,
# sacar muestras, y ver qué tan cerca estamos del valor verdadero.
#
# ============================================================================


# ----------------------------------------------------------------------------
# 1.1 Exploración inicial de los datos
# ----------------------------------------------------------------------------

# Veamos primero qué tiene la base flights.
flights

# ¿Cuántas observaciones y variables?
dim(flights)

# ¿Qué variables tenemos?
glimpse(flights)

# Para esta clase nos vamos a concentrar en la demora de salida: dep_delay.
# Valores positivos = el vuelo salió tarde; negativos = salió antes de lo programado;
# NA = el vuelo fue cancelado y no tiene demora registrada.

# Tenemos que decidir qué hacer con los NA. Como no podemos calcular la demora
# de un vuelo que no voló, vamos a sacarlos de la base. Esta decisión la
# hacemos UNA VEZ al principio y queda documentada.
# La idea es que este tipo de decisiones las tomen tambien en el TP y las dejen documentadas, 
# no solamente en el codigo, tambien en la PPT 

vuelos <- flights |>
  filter(!is.na(dep_delay))

# ¿Cuántos quedaron? Esta es nuestra "población" para el resto de la clase.
nrow(vuelos)


# ============================================================================
# 2. ESTADÍSTICAS DESCRIPTIVAS DE LA POBLACIÓN
# ============================================================================
#
# Antes de simular muestreo, calculemos los parámetros poblacionales.
# Son los valores "verdaderos" que normalmente NO conocemos.
# Los usamos hoy como referencia para evaluar cuán bien funcionan
# nuestras estimaciones muestrales.
# ============================================================================


# ----------------------------------------------------------------------------
# 2.1 Resumen general: las seis medidas clásicas
# ----------------------------------------------------------------------------

# Con summarise() calculamos todo de una vez.

parametros_pob <- vuelos |>
  summarise(
    N           = n(),
    media       = mean(dep_delay),
    mediana     = median(dep_delay),
    desvio      = sd(dep_delay),
    q1          = quantile(dep_delay, 0.25),
    q3          = quantile(dep_delay, 0.75),
    iqr         = IQR(dep_delay),
    minimo      = min(dep_delay),
    maximo      = max(dep_delay),
    cv          = desvio / media * 100
  )

parametros_pob

# Guardemos la media y el desvío poblacionales en objetos sueltos, porque los
# vamos a usar mucho cuando evaluemos nuestras muestras.

mu_pob    <- parametros_pob$media
sigma_pob <- parametros_pob$desvio

mu_pob
sigma_pob

# Observación importante: la media (~12.6 min) es bastante mayor que la
# mediana (~-2 min). Esto nos dice que la distribución es asimétrica a la
# derecha: la mayoría de los vuelos salen casi en horario (o antes), pero
# hay algunos pocos con demoras enormes que "tiran" la media para arriba.
# El TCL va a funcionar igual, como veremos. Ese es el punto.


# ----------------------------------------------------------------------------
# 2.2 Descriptivas por grupo
# ----------------------------------------------------------------------------

# ¿Las demoras son iguales en los tres aeropuertos? Agrupemos.

descriptivas_por_origen <- vuelos |>
  group_by(origin) |>
  summarise(
    n_vuelos = n(),
    media    = mean(dep_delay),
    mediana  = median(dep_delay),
    desvio   = sd(dep_delay),
    .groups  = "drop"
  )

descriptivas_por_origen

# ¿Y por mes? A veces la estacionalidad importa.

descriptivas_por_mes <- vuelos |>
  group_by(month) |>
  summarise(
    n_vuelos = n(),
    media    = mean(dep_delay),
    mediana  = median(dep_delay),
    desvio   = sd(dep_delay),
    .groups  = "drop"
  )

descriptivas_por_mes

# Viendo estos dos resúmenes podemos armar hipótesis: los meses de verano 
# de EEUU (junio, julio) tienden a tener más demoras. 
# Habria que ver si hay algun factor climatico o si son las vacaciones que lo hacen colapsar
# EWR (Newark) parece tener peores demoras que JFK o LGA. Pero estas son
# afirmaciones descriptivas: no dicen nada sobre incertidumbre todavía.


# ----------------------------------------------------------------------------
# 2.3 Visualizamos la distribución poblacional
# ----------------------------------------------------------------------------

# La distribución original de dep_delay está super sesgada y tiene colas muy
# largas (algunos vuelos se demoraron más de 20 horas). Para el gráfico
# recortamos el eje X entre -30 y 180 minutos para que se vea bien el grueso
# de los datos. Los casos extremos existen, pero no los necesitamos visibles
# para el argumento pedagógico.

ggplot(vuelos, aes(x = dep_delay)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  geom_vline(xintercept = mu_pob, color = "red", linewidth = 1) +
  coord_cartesian(xlim = c(-30, 180)) +
  labs(
    title    = "Distribución de la demora de salida (población)",
    subtitle = "Vuelos desde NYC en 2013. Línea roja = media poblacional",
    x        = "Demora de salida (minutos)",
    y        = "Cantidad de vuelos"
  ) +
  theme_minimal()

# Observen: la distribución es claramente asimétrica a la derecha. Nada
# parecido a una normal.


# ============================================================================
# 3. SIMULACIÓN DE MUESTREO Y TEOREMA CENTRAL DEL LÍMITE
# ============================================================================
#
# Ahora bien, supongamos que no conocemos la poblacion, por lo que necesitamos
# tomar muestras, como en una encuesta real.
# Por ejemplo, esto podria pasar en el ejemplo si con algun software (cof R cof)
# levantan la informacion una vez por dia o algunas veces a la semana de alguna 
# web. Es complicado tener un relevamiento detallado, pero pueden hacer una muestra 
# por su cuenta de los datos web (de demoras, de precios, de tantas cosas)
# ============================================================================


# ----------------------------------------------------------------------------
# 3.1 Una sola muestra: la lógica del muestreo
# ----------------------------------------------------------------------------

# Tomemos una muestra aleatoria de 100 vuelos. slice_sample() de dplyr
# hace exactamente eso: elige filas al azar.

muestra_1 <- vuelos |>
  slice_sample(n = 100)

# Calculemos la media de esa muestra: la llamamos "x barra" o media muestral.

media_muestral_1 <- mean(muestra_1$dep_delay)
media_muestral_1

# Comparémosla con la verdadera:
mu_pob

# No dio exactamente igual. Esto es NORMAL: cada muestra es distinta.
# Si tomamos otra, va a dar otro número.

muestra_2 <- vuelos |> slice_sample(n = 100)
mean(muestra_2$dep_delay)

muestra_3 <- vuelos |> slice_sample(n = 100)
mean(muestra_3$dep_delay)

# Cada número es una estimación puntual de la media poblacional. Están cerca,
# pero no son iguales. ¿Qué pasa si repetimos esto muchas veces? ¿Se puede
# anticipar cómo van a distribuirse las medias muestrales?


# ----------------------------------------------------------------------------
# 3.2 El TCL en acción: simulación con un loop
# ----------------------------------------------------------------------------

# Vamos a simular tomar muchas muestras (digamos 1000) de tamaño n = 100 y
# guardar la media de cada una. Para eso usamos un loop. Un loop es una
# estructura que repite un pedazo de código un número fijo de veces.

# Estrategia 1: un loop que va guardando en un vector.

n_muestras <- 1000     # vamos a repetir el muestreo 1000 veces
tam_muestra <- 100     # cada muestra tendrá 100 vuelos

# Creamos un vector vacío para ir guardando los resultados.
medias_simuladas <- numeric(n_muestras)

# Ahora el loop. Para cada i de 1 a 1000:
#   1) tomamos una muestra de 100 vuelos,
#   2) calculamos su media,
#   3) la guardamos en la posición i del vector.

for (i in 1:n_muestras) {
  muestra_i <- vuelos |> slice_sample(n = tam_muestra)
  medias_simuladas[i] <- mean(muestra_i$dep_delay)
}

# Listo: tenemos 1000 medias muestrales.
length(medias_simuladas)
head(medias_simuladas)

# Resumamos la distribución de esas 1000 medias.
summary(medias_simuladas)

# La media de las medias muestrales debería estar muy cerca de la media
# poblacional. Comparemos:
mean(medias_simuladas)
mu_pob


# Podriamos aumentar la cantidad de observaciones de cada muestra 
# O la cantidad de muestras. En ambos casos vamos a mejorar nuestra estimacion
# Pero en la vida real, es mas facil tener una muestra mas grande que muchas muestras. 

# ----------------------------------------------------------------------------
# 3.3 El TCL: ¿qué forma tiene la distribución de las medias?
# ----------------------------------------------------------------------------

# La distribución ORIGINAL de dep_delay era muy asimétrica.
# Y eso no va a cambiar nunca, sin importar el tamaño de la muestra
# Pero la distribución de las MEDIAS MUESTRALES...

# Armemos un tibble para graficarlo con ggplot.
medias_tbl <- tibble(media_muestral = medias_simuladas)

ggplot(medias_tbl, aes(x = media_muestral)) +
  geom_histogram(bins = 30, fill = "darkorange", color = "white") +
  geom_vline(xintercept = mu_pob, color = "red", linewidth = 1) +
  labs(
    title    = "Distribución de 1000 medias muestrales (n = 100)",
    subtitle = "Aun cuando la población es asimétrica, las medias se aproximan a una normal",
    x        = "Media muestral de dep_delay (minutos)",
    y        = "Cantidad de muestras"
  ) +
  theme_minimal()

# La distribución de las medias es aproximadamente SIMÉTRICA y con forma de
# campana. Esto es el Teorema Central del Límite:
#
#   Independientemente de la forma de la población, la distribución de las
#   medias muestrales se aproxima a una normal cuando n es "grande"
#   (regla práctica: n >= 30, aunque depende de cuán asimétrica sea la población).
#
# El TCL nos permite razonar con reglas conocidas (la normal) incluso cuando
# los datos originales no tienen nada de normales.
# En este caso, incluso podemos ver que sigue siendo un poco asimetrica aun

# ----------------------------------------------------------------------------
# 3.4 El error estándar: ¿cuánto varían las medias muestrales?
# ----------------------------------------------------------------------------

# El TCL dice algo más: la desviación estándar de la distribución de las
# medias muestrales (llamada ERROR ESTÁNDAR) tiene una fórmula conocida:
#
#     EE = sigma / sqrt(n)
#
# Verifiquemos empíricamente con nuestra simulación.

ee_teorico <- sigma_pob / sqrt(tam_muestra)
ee_teorico

ee_empirico <- sd(medias_simuladas)
ee_empirico

# Son parecidos. Es decir, la fórmula funciona. Esto es
# clave porque, en la vida real, no vamos a poder repetir el muestreo
# 1000 veces: vamos a tener UNA sola muestra. Y aun así, vamos a poder
# estimar la variabilidad de nuestra estimación usando la fórmula.


# ----------------------------------------------------------------------------
# 3.5 ¿Qué pasa si cambiamos el tamaño de la muestra?
# ----------------------------------------------------------------------------

# El error estándar depende de n. A mayor n, menor error. Vamos a verificar
# esto con una simulación más ambiciosa: repetir el ejercicio para varios
# tamaños de muestra y juntar todo con bind_rows().

# Esta es una técnica muy útil: cuando queremos comparar varios escenarios,
# generamos un data frame por escenario y al final los "apilamos" con bind_rows.

# Esta funcion nos permite hacer una especie de join, pero sin agregar columnas, sino filas. 
# Para ello, los nombres de las columnas deben coincidir, sino se nos van a generar columnas con 
# NULLs en los casos en los que no coincidian los nombres de las variables
# Una de las cosas buenas de la funcion es que no trabaja por posicion de la variable 
# sino que se fija en el nombre. Entonces, si las columnas tienen el mismo nombre, 
# pero distinta posicion entre cada tabla, no pasa nada, la tabla final va a tener el 
# orden de la primera tabla que se pone en el bind_rows
# Lo "malo" es que no es una funcion que controle la repitencia de valores, 
# entonces una buena practica -dependiendo del caso- es usar distinct() una vez que hace el 
# bind_rows

tamaños_muestra <- c(10, 30, 100, 500)
lista_resultados <- list()   # lista vacía para ir guardando los tibbles

for (n in tamaños_muestra) {
  
  # Vector para las medias de este tamaño.
  medias_n <- numeric(n_muestras)
  
  for (i in 1:n_muestras) {
    muestra_i <- vuelos |> slice_sample(n = n)
    medias_n[i] <- mean(muestra_i$dep_delay)
  }
  
  # Armamos un tibble con los resultados y el tamaño de muestra como
  # columna identificadora.
  lista_resultados[[as.character(n)]] <- tibble(
    tam_muestra    = n,
    media_muestral = medias_n
  )
}

# bind_rows apila todos los tibbles de la lista en uno solo.
resultados_tcl <- bind_rows(lista_resultados)

# Chequeamos qué quedó.
resultados_tcl
count(resultados_tcl, tam_muestra)

# Graficamos las cuatro distribuciones en paneles con facet_wrap.
# Fíjense cómo a medida que aumenta n, la distribución se concentra más
# alrededor de la media poblacional: ese es el efecto de sqrt(n) en el
# denominador del error estándar.

ggplot(resultados_tcl, aes(x = media_muestral)) +
  geom_histogram(bins = 30, fill = "darkorange", color = "white") +
  geom_vline(xintercept = mu_pob, color = "red", linewidth = 1) +
  facet_wrap(~ tam_muestra, scales = "free_y",
             labeller = labeller(tam_muestra = function(x) paste("n =", x))) +
  labs(
    title    = "Distribución de medias muestrales para distintos n",
    subtitle = "A mayor tamaño de muestra, menor variabilidad de la media muestral",
    x        = "Media muestral de dep_delay (minutos)",
    y        = "Cantidad de muestras"
  ) +
  theme_minimal()

# Además, comparemos el error estándar empírico para cada n con el teórico.

comparacion_ee <- resultados_tcl |>
  group_by(tam_muestra) |>
  summarise(
    media_de_medias = mean(media_muestral),
    ee_empirico     = sd(media_muestral),
    .groups         = "drop"
  ) |>
  mutate(
    ee_teorico = sigma_pob / sqrt(tam_muestra),
    diferencia = round(ee_empirico - ee_teorico, 3)
  )

comparacion_ee

# La fórmula del TCL predice con precisión cómo se contrae la distribución
# de la media muestral cuando crece n. Esto no es una coincidencia:
# es el resultado matemático más importante de la estadística clásica.


# ============================================================================
# 4. INTERVALOS DE CONFIANZA
# ============================================================================
#
# Hasta acá vimos que la media muestral es una estimación con error. Entonces, la
# pregunta es si ¿podemos cuantificar ese error? Un intervalo de
# confianza (IC) es justamente eso: un rango de valores dentro del cual
# es "plausible" que esté el parámetro poblacional.
# ============================================================================


# ----------------------------------------------------------------------------
# 4.1 Construcción manual de un IC al 95%
# ----------------------------------------------------------------------------

# Tenemos UNA muestra (no la población). Queremos estimar mu. La fórmula
# del IC del 95% para la media, cuando el desvío poblacional es desconocido
# y usamos el desvío muestral, es:
#
#     IC = x_barra  ±  t_{alfa/2, n-1} * (s / sqrt(n))
#
# donde s es el desvío muestral de la variable en la muestra 
# y t es el cuantil de la distribución
# t-Student con n-1 grados de libertad. Para n grande (como 100), t es
# prácticamente igual al z de la normal (1.96), pero usamos t por
# convención y porque es correcto también para n pequeños.

# Tomamos una nueva muestra:

muestra <- vuelos |> slice_sample(n = 100)

n      <- nrow(muestra)
x_bar  <- mean(muestra$dep_delay)
s      <- sd(muestra$dep_delay)
ee     <- s / sqrt(n)
t_crit <- qt(0.975, df = n - 1)  # 0.975 porque queremos 2.5% a cada lado

li <- x_bar - t_crit * ee
ls <- x_bar + t_crit * ee

tibble(x_bar, s, ee, t_crit, li, ls)

# Interpretación correcta: si repitiéramos este procedimiento muchas veces
# (tomar muestras y construir intervalos), aproximadamente el 95% de los
# intervalos contendrían la verdadera media poblacional. NO es que haya un
# 95% de probabilidad de que mu esté en ESTE intervalo particular: mu está
# fijo (no es aleatorio), lo que varía es el intervalo. 
# Esto muchas veces se confunde.


# ----------------------------------------------------------------------------
# 4.2 El atajo: t.test()
# ----------------------------------------------------------------------------

# R tiene una función que hace todo este cálculo en un paso.

resultado_t <- t.test(muestra$dep_delay, conf.level = 0.95)
resultado_t

# La salida incluye el IC. Para extraerlo:
resultado_t$conf.int

# Chequeen que coincide con nuestro cálculo manual. Dentro de los redondeos,
# deberían dar lo mismo.


# ----------------------------------------------------------------------------
# 4.3 Verificamos empíricamente la cobertura del 95%
# ----------------------------------------------------------------------------

# Si construimos 1000 intervalos, cada uno desde una muestra distinta, 
#¿cuántos contienen a mu? Debería ser aproximadamente el 95%.

n_muestras  <- 1000
tam_muestra <- 1000 # Vamos a subir un poco el tamaño muestral, para que de mejor 

# En vez de usar vectores sueltos, usamos una lista de tibbles + bind_rows:
# es más prolijo, y nos deja ver bien qué pasa con cada intervalo.

lista_ic <- list()

for (i in 1:n_muestras) {
  
  muestra_i <- vuelos |> slice_sample(n = tam_muestra)
  x_bar_i   <- mean(muestra_i$dep_delay)
  s_i       <- sd(muestra_i$dep_delay)
  ee_i      <- s_i / sqrt(tam_muestra)
  t_crit    <- qt(0.975, df = tam_muestra - 1)
  
  lista_ic[[i]] <- tibble(
    rep    = i,
    x_bar  = x_bar_i,
    li     = x_bar_i - t_crit * ee_i,
    ls     = x_bar_i + t_crit * ee_i
  )
}

# Apilamos todos los intervalos en un único tibble.
ics <- bind_rows(lista_ic)

# Marcamos cuáles contienen a mu.
ics <- ics |>
  mutate(contiene_mu = if_else(li <= mu_pob & mu_pob <= ls, "Sí", "No"))

# ¿Qué proporción contiene a mu?
ics |>
  count(contiene_mu) |>
  mutate(prop = n / sum(n))

# Debería estar muy cerca del 95%. Si no corrieran las 1000 repeticiones,
# la proporción podría desviarse un poco, pero siempre va a rondar esa
# cifra. Esa es justamente la definición operativa del "95% de confianza".

# Visualicemos los primeros 100 intervalos para ver el concepto en acción.
# Los que NO contienen a mu se destacan en rojo.

ics |>
  slice(1:100) |>
  ggplot(aes(x = rep, y = x_bar, color = contiene_mu)) +
  geom_errorbar(aes(ymin = li, ymax = ls), width = 0) +
  geom_point(size = 1) +
  geom_hline(yintercept = mu_pob, color = "black", linewidth = 0.8) +
  scale_color_manual(values = c("Sí" = "steelblue", "No" = "red")) +
  labs(
    title    = "100 intervalos de confianza del 95%",
    subtitle = "La línea horizontal es la media poblacional. Los rojos no la contienen.",
    x        = "Repetición",
    y        = "Media muestral ± margen de error",
    color    = "¿Contiene μ?"
  ) +
  theme_minimal()


# ============================================================================
# 5. CÓDIGO INTEGRADOR
# ============================================================================
#
# Pongamos todo junto en un ejercicio realista. Supongan que nos piden
# estimar, con un IC del 95%, la demora promedio de salida para cada uno
# de los tres aeropuertos de NYC (JFK, LGA y EWR), pero a partir de una
# muestra de 200 vuelos por aeropuerto.
#
# Vamos a hacerlo con un loop que itera sobre los aeropuertos, calcula el
# IC para cada uno y guarda todo en una lista. Al final usamos bind_rows
# para tener una tabla prolija.
# ============================================================================


# ----------------------------------------------------------------------------
# 5.1 Una función que encapsula el cálculo del IC
# ----------------------------------------------------------------------------

# Antes del loop, definamos una función. Así el código queda más limpio
# y, si queremos cambiar el nivel de confianza, cambiamos una sola línea.

calcular_ic <- function(x, conf = 0.95) {
  
  n      <- length(x)
  x_bar  <- mean(x)
  s      <- sd(x)
  ee     <- s / sqrt(n)
  alfa   <- 1 - conf
  t_crit <- qt(1 - alfa / 2, df = n - 1)
  
  tibble(
    n       = n,
    media   = x_bar,
    desvio  = s,
    ee      = ee,
    li      = x_bar - t_crit * ee,
    ls      = x_bar + t_crit * ee,
    ancho   = ls - li
  )
}

# Probamos la función con una muestra cualquiera.
calcular_ic(muestra$dep_delay)

# Le cambiamos el nivel a 99%: el intervalo se ensancha. Mayor confianza,
# mayor ancho. Este trade-off es inevitable.
calcular_ic(muestra$dep_delay, conf = 0.99)


# ----------------------------------------------------------------------------
# 5.2 Loop sobre los tres aeropuertos
# ----------------------------------------------------------------------------

aeropuertos <- c("JFK", "LGA", "EWR")
lista_aeropuertos <- list()

for (aero in aeropuertos) {
  
  # Tomamos una muestra de 200 vuelos del aeropuerto correspondiente.
  muestra_aero <- vuelos |>
    filter(origin == aero) |>
    slice_sample(n = 200)
  
  # Calculamos el IC con la función que definimos antes y le agregamos
  # una columna identificatoria para saber a qué aeropuerto corresponde.
  ic_aero <- calcular_ic(muestra_aero$dep_delay) |>
    mutate(aeropuerto = aero, .before = 1)
  
  lista_aeropuertos[[aero]] <- ic_aero
}

# Apilamos todo en una única tabla de resultados.
tabla_ics <- bind_rows(lista_aeropuertos)

# Comparamos con la verdad (la media poblacional por aeropuerto), que en la
# vida real no tendríamos.

verdad_por_aero <- vuelos |>
  group_by(origin) |>
  summarise(media_poblacional = mean(dep_delay), .groups = "drop") |>
  rename(aeropuerto = origin)

tabla_final <- tabla_ics |>
  left_join(verdad_por_aero, by = "aeropuerto") |>
  mutate(
    cubre_verdad = if_else(li <= media_poblacional & media_poblacional <= ls,
                           "Sí", "No")
  )

tabla_final


# ----------------------------------------------------------------------------
# 5.3 Visualización final: comparamos los tres IC
# ----------------------------------------------------------------------------

ggplot(tabla_final, aes(x = aeropuerto, y = media, color = cubre_verdad)) +
  geom_errorbar(aes(ymin = li, ymax = ls), width = 0.2, linewidth = 1) +
  geom_point(size = 3) +
  geom_point(aes(y = media_poblacional), shape = 4, size = 4,
             color = "black", stroke = 1) +
  scale_color_manual(values = c("Sí" = "steelblue", "No" = "red")) +
  labs(
    title    = "Demora de salida promedio: IC 95% por aeropuerto",
    subtitle = "Muestras de n = 200. La X negra es la media poblacional verdadera.",
    x        = "Aeropuerto de origen",
    y        = "Demora de salida (minutos)",
    color    = "¿Cubre la verdad?"
  ) +
  theme_minimal()


# ============================================================================
# 6. CIERRE Y PREGUNTAS ANÁLOGAS
# ============================================================================
#
# Volvamos a la pregunta del inicio:
#
#   ¿Cuál es la demora promedio de salida de los vuelos que partieron de NYC?
#
# Respuesta: con una muestra de 100 vuelos y un IC del 95%, estimamos que
# la demora promedio está en [ ... , ... ] minutos. Y verificamos que este
# procedimiento, si lo repetimos muchas veces, acierta aproximadamente el
# 95% de las veces.
#
# ----------------------------------------------------------------------------
# Preguntas para pensar y hacer en casa:
# ----------------------------------------------------------------------------
#
#   1. ¿Qué pasa con el ancho del IC si aumentamos el nivel de confianza
#      del 95% al 99%? ¿Y si lo bajamos al 90%?
#
#   2. ¿Qué pasa con el ancho si aumentamos el tamaño de la muestra de 100
#      a 1000? Prueben modificar el código de la sección 4.1.
#
#   3. Repitan el ejercicio integrador de la sección 5 usando como variable
#      arr_delay (demora de llegada) en vez de dep_delay. ¿Cambian las
#      conclusiones sobre los aeropuertos?
#
#   4. En la simulación de la sección 3.5, probamos con n = 10, 30, 100 y
#      500. ¿Qué forma tiene la distribución de las medias cuando n = 10?
#      ¿Ya se parece a una normal?
#
#   5. Intentar en casa: modifiquen el loop de la sección 5.2 para calcular un IC
#      para cada mes del año en lugar de para cada aeropuerto.
#      ¿En qué meses tenemos peores demoras?
#
# ----------------------------------------------------------------------------
# Ideas clave para llevarse de esta clase:
# ----------------------------------------------------------------------------
#
#   - La media muestral es una variable aleatoria: cambia con cada muestra.
#   - El TCL garantiza que, para n grande, la distribución de las medias
#     muestrales se aproxima a una normal, sin importar la forma de la
#     población.
#   - El error estándar (sigma / sqrt(n)) cuantifica la variabilidad de la
#     media muestral y se achica con la raíz de n.
#   - Un IC del 95% es un procedimiento que, repetido muchas veces, captura
#     el parámetro el 95% de las veces. No habla de probabilidad en una
#     realización particular.
#   - Los loops + bind_rows son una herramienta versátil para repetir
#     análisis sobre grupos y consolidar resultados en tablas únicas.
#
# ============================================================================