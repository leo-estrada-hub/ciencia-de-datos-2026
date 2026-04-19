# Ejercicios prácticos: Tests de hipótesis con WDI

**Ciencia de Datos para Economía y Negocios | FCE-UBA**
**Clase 9 — Práctica domiciliaria (opcional)**

---

Estos ejercicios son para que puedan practicar los tests de hipótesis vistos en clase. No son de entrega obligatoria. La idea es que los desarrollen en sus hogares, con tiempo, y traigan dudas a la siguiente clase.

Ambos ejercicios usan datos del **Banco Mundial** a través del paquete `WDI` de R. Para descargar los datos van a necesitar conexión a internet la primera vez; después pueden guardarlos localmente con `saveRDS()`.

Paquetes a instalar previamente:

```r
install.packages(c("tidyverse", "WDI"))
```

---

## Ejercicio A — Desigualdad en el tiempo y entre regiones

### Pregunta central

*¿Cambió la desigualdad de ingresos en la última década? ¿Difiere la desigualdad entre regiones del mundo?*

### Datos

- **Indicador**: índice de Gini del Banco Mundial.
- **Código WDI**: `SI.POV.GINI`.
- **Fuente original**: PovcalNet / World Bank Poverty and Inequality Platform.
- **Años sugeridos**: 2010 a 2022.

Descarga:

```r
library(WDI)
gini_raw <- WDI(country   = "all",
                indicator = c("gini" = "SI.POV.GINI"),
                start     = 2010, end = 2022,
                extra     = TRUE)
```

**Advertencia importante**: el Gini no está disponible para todos los países todos los años. Algunos países tienen 2 o 3 mediciones en la década, otros tienen más. Esto va a requerir que piensen bien cómo armar la base antes de testear.

### Preguntas guía

**1. Exploración inicial**

- ¿Cuántos países tienen al menos una observación de Gini en el período?
- ¿Cuántos países tienen observaciones en al menos dos años distintos?
- Hagan un gráfico rápido de la distribución del Gini en el último año disponible.

**2. Test pareado: ¿cambió el Gini en la última década?**

- Para cada país, quédense con el **primer año disponible** y el **último año disponible** de Gini. Piensen: ¿conviene usar `group_by` + `filter(year == min(year))`? ¿Cómo lo armarían?
- Filtren países que tengan al menos dos observaciones (una para el inicio y otra para el final).
- Planteen H0 y H1 del test pareado sobre la diferencia de Gini entre el último y el primer año.
- Corran el test en R.
- ¿Rechazan H0? ¿En qué dirección va el cambio promedio?
- **Crítica al diseño**: el "primer año disponible" no es el mismo para todos los países. ¿Eso es un problema? ¿Qué supuesto implícito estamos haciendo al compararlos así?

**3. ANOVA: ¿el Gini difiere entre regiones?**

- Tomen el **último año disponible por país** (el más reciente de cada uno).
- La variable `region` ya viene en el dataset cuando usan `extra = TRUE` en `WDI()`.
- Descriptivos por región: media, desvío, cantidad de países.
- Planteen H0 y H1 del ANOVA.
- Corran el ANOVA (`aov()`) y el test post-hoc de Tukey (`TukeyHSD()`).
- ¿Qué regiones difieren significativamente entre sí? ¿Qué región tiene el Gini más alto en promedio?

**4. Reflexión final**

- ¿Qué nos dice el test pareado sobre la evolución global de la desigualdad?
- ¿Qué nos dice el ANOVA sobre la distribución espacial de la desigualdad?
- ¿Son resultados contradictorios o complementarios?

### Pistas técnicas

- Si `SI.POV.GINI` les da muy pocas observaciones, pueden probar con `SI.POV.GINI` completo o con el Gini de la base SEDLAC para Latinoamérica.
- Para quedarse con el primer y último año por país:

```r
gini_extremos <- gini_raw %>%
  filter(region != "Aggregates", !is.na(gini)) %>%
  group_by(iso3c) %>%
  filter(year == min(year) | year == max(year)) %>%
  # ... sigan ustedes
```

- Para el test pareado necesitan **formato ancho**: una fila por país, una columna para el primer año y otra para el último. Miren `pivot_wider()`.

---

## Ejercicio B — Comercio internacional y grupo de ingreso

### Pregunta central

*¿Los países de ingreso alto comercian proporcionalmente más (en relación a su PBI) que los países de ingreso medio-alto?*

### Datos

- **Indicador**: comercio como porcentaje del PBI (exportaciones + importaciones de bienes y servicios / PBI).
- **Código WDI**: `NE.TRD.GNFS.ZS`.
- **Fuente original**: World Development Indicators, Banco Mundial.
- **Año sugerido**: 2022 (o el último disponible).

Descarga:

```r
library(WDI)
comercio_raw <- WDI(country   = "all",
                    indicator = c("comercio_pbi" = "NE.TRD.GNFS.ZS"),
                    start     = 2022, end = 2022,
                    extra     = TRUE)
```

La variable `income` (que viene con `extra = TRUE`) clasifica a los países en: `High income`, `Upper middle income`, `Lower middle income`, `Low income`.

### Preguntas guía

**1. Exploración inicial**

- Filtren agregados regionales (`region != "Aggregates"`).
- Descriptivos de comercio/PBI por grupo de ingreso: media, mediana, desvío, cantidad de países.
- Hagan un boxplot de `comercio_pbi` por `income`. ¿Qué patrón ven?

**2. Test de diferencia de medias: High income vs Upper middle income**

- Filtren solo esos dos grupos.
- Planteen H0 y H1 del t-test de dos muestras (bilateral).
- Corran el test con `t.test()`. Por default R usa el Welch t-test, que no asume varianzas iguales — está bien que lo dejen así.
- ¿Cuál es el p-valor? ¿Rechazan H0?
- Interpreten el IC 95% de la diferencia de medias.

**3. Comparación adicional: Low income vs High income**

- Repitan el test comparando `Low income` vs `High income`.
- ¿El resultado es más o menos fuerte que la comparación anterior? ¿Por qué podría ser?
- **Discusión**: si los países chicos tienden a comerciar más (porque no pueden producir todo internamente) y los países chicos están desigualmente distribuidos entre grupos de ingreso, ¿estamos midiendo "efecto ingreso" o "efecto tamaño"?

**4. Extensión opcional: controlar por tamaño**

- Descarguen también población (`SP.POP.TOTL`) para el mismo año.
- Clasifiquen los países en "chicos" (menos de 10 millones) y "grandes" (más de 10 millones).
- Repitan el t-test de High vs Upper middle **solo entre países grandes**. ¿Cambia la conclusión?
- Esto no es todavía un análisis causal riguroso, pero muestra por qué un solo test rara vez alcanza para responder una pregunta económica.

**5. Reflexión final**

- ¿Qué diferencia hay entre "los países ricos comercian más" y "ser rico causa que un país comercie más"?
- ¿Un test de hipótesis puede distinguir entre esas dos afirmaciones? ¿Por qué sí o por qué no?

### Pistas técnicas

- Para filtrar dos grupos específicos de `income`:

```r
comercio_raw %>%
  filter(income %in% c("High income", "Upper middle income"))
```

- Si el t-test les devuelve un warning sobre pocos datos, chequeen cuántos países tienen dato de comercio en el año elegido (algunos faltan todos los años, como Venezuela).

---

## Cómo presentar el trabajo (si quieren entregarlo)

No es obligatorio, pero si quieren feedback pueden enviar:

- Un script R (`.R`) o un R Markdown (`.Rmd`) con el código comentado.
- Las respuestas a las preguntas guía en comentarios del código o en el texto del Rmd.
- Un párrafo corto de reflexión final por cada ejercicio.

El objetivo no es que el código sea perfecto sino que el razonamiento sea claro: qué H0 plantean, qué test eligen, por qué, y cómo interpretan el resultado.
