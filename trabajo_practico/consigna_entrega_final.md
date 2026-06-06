# Entrega final — Ciencia de Datos para Economía y Negocios

## Forma de entrega

La entrega consiste en un **enlace a un repositorio de GitHub** que contenga todo el material del trabajo: presentación, bases de datos, códigos y cualquier otro insumo necesario para su evaluación. Todo debe estar alojado en el repositorio, salvo excepciones habilitadas expresamente por el docente.

El repositorio debe incluir un **README** y respetar la estructura de carpetas vista en el curso.

### Estructura de carpetas

El repositorio debe organizarse con las siguientes carpetas:

- **raw/**: datos crudos, tal como fueron descargados. No se modifican.
- **input/**: datos procesados y listos para el análisis. Son el producto de los scripts de limpieza y transformación.
- **output/**: resultados del análisis (tablas, gráficos exportados, objetos guardados).
- **auxiliar/**: bases de datos complementarias o auxiliares utilizadas en el trabajo.
- **utils/**: funciones personalizadas. Si una función se usa en más de un script, debe estar guardada en esta carpeta y ser llamada desde los scripts con `source()`.
- **scripts/**: todos los códigos del trabajo.

### Compartimentación de los scripts

Cada script debe ser una unidad autónoma: **guardar sus resultados** (en `output/` o `input/`, según corresponda) y el script siguiente debe **leer esos resultados guardados**, no depender de que el script anterior haya corrido en la misma sesión. Esto permite correr cualquier script en cualquier orden sin que se rompa nada, siempre que los archivos intermedios estén generados.

Por ejemplo: si un script limpia la base y genera una tabla limpia, debe guardarla como archivo. El script de análisis posterior debe leer ese archivo, no asumir que el objeto está en el entorno de R.

### Nomenclatura de los scripts

Los archivos de código deben nombrarse con un prefijo numérico que indique el orden de ejecución, seguido de una descripción breve de lo que hace el script. Por ejemplo: `01_limpieza.R`, `02_analisis_descriptivo.R`, `03_regresion.R`. El nombre debe ser suficiente para entender qué hace cada script sin necesidad de abrirlo.

### Reproducibilidad

**Este punto es fundamental y será evaluado con especial atención, por más que parezca de menor relevancia.** El código debe poder ejecutarse sin intervención manual y sin errores. Esto implica:

- Rutas relativas (nunca absolutas).
- Todos los archivos de datos necesarios deben estar en el repositorio.
- El README debe indicar claramente qué scripts correr y en qué orden, describir brevemente qué hace cada uno y qué archivos genera.
- El código debe seguir las buenas prácticas de escritura vistas en el curso: nombres claros de objetos y variables, comentarios que expliquen las decisiones relevantes, estructura ordenada y legible.
- Las librerías utilizadas deben estar cargadas al inicio de cada script.

Cuentan con todos los elementos necesarios para cumplir con estos criterios: la estructura de carpetas, el armado del README y las buenas prácticas de código fueron temas del curso. La reproducibilidad es una competencia central de la materia y será evaluada con ese peso.

---

## Presentación

La presentación puede estar en el formato que prefieran (Beamer, PowerPoint, Google Slides, PDF, etc.) siempre y cuando sean **diapositivas**. Debe pensarse como una presentación laboral o académica: visual, sintética y bien estructurada. Eviten bloques extensos de texto; las diapositivas acompañan un argumento, no lo reemplazan.

El docente evaluará si es necesaria una instancia de **presentación oral virtual de 10 minutos**.

---

## Contenido esperado

### 1. Hipótesis de trabajo

Presentar con claridad la o las hipótesis que guían el análisis. Deben ser específicas y contrastables con los datos disponibles. Toda la estructura del trabajo —desde la elección de variables hasta la metodología— debe estar orientada a evaluar estas hipótesis.

### 2. Descripción de la base de datos

- ¿De qué trata la base de datos? ¿Quién la produce? ¿Qué unidad de observación tiene (personas, empresas, países, meses)?
- ¿Cuántas filas tiene? ¿Cuántas variables?
- ¿Qué período temporal cubre?
- ¿Tiene alguna limitación conocida?

### 3. Descripción de las variables utilizadas

No necesariamente todas las variables de la base, sino las que efectivamente se usan en el trabajo. Para cada una indicar:

- Nombre en la base de datos.
- Qué representa.
- Tipo de variable (numérica, categórica, fecha, etc.).

### 4. Bases de datos auxiliares y complementarias

- Describir cada base auxiliar y sus variables relevantes.
- Explicar cómo y para qué se utilizaron (por ejemplo: para hacer un join, para construir un índice, para contextualizar).

### 5. Estadísticas descriptivas

Presentar estadísticas descriptivas de la tabla con los datos que van a utilizar en el análisis. Incluir medidas de tendencia central, dispersión y, si corresponde, distribución. Estas estadísticas deben permitir al lector hacerse una idea de los datos antes de ver los resultados del análisis.

### 6. Datos faltantes y outliers

- Identificar si existen observaciones faltantes y/o outliers. Mostrar cómo se detectaron (métodos, gráficos, criterios numéricos).
- Explicar el criterio utilizado para tratarlos (imputación, eliminación, transformación, etc.) y **justificar** la decisión.
- **Si no hay datos faltantes ni outliers, igualmente deben mostrar cómo llegaron a esa conclusión.** No alcanza con decir "no hay"; hay que mostrar la evidencia.

### 7. Estadísticas descriptivas post-limpieza

Presentar las estadísticas descriptivas luego del tratamiento de datos faltantes y outliers.

Si decidieron no realizar ninguna limpieza para evitar alterar las variables, deben mostrar que la inclusión o exclusión de esas observaciones no genera un cambio considerable en las estadísticas descriptivas, o bien justificar por qué ese cambio, aun siendo relevante, no debe tenerse en cuenta.

### 8. Metodología y resultados

Presentar las herramientas metodológicas utilizadas, ordenadas jerárquicamente para construir bien el argumento. El orden importa: cada herramienta debe apoyarse en lo anterior y preparar lo que sigue. Como orientación general:

- Los tests de hipótesis van antes que un ANOVA o un test Chi-cuadrado.
- ANOVA / Chi-cuadrado van antes que una regresión.
- Si el trabajo incluye análisis de series de tiempo, conviene que sea lo primero.
- Índices como RCA, HHI u otros similares se ubican según su rol en el argumento.

Para cada herramienta utilizada:

- Explicar brevemente **por qué se eligió** esa herramienta y qué pregunta responde dentro del análisis.
- Plantear los **supuestos clave** que requiere y evaluar si se cumplen. Si no se cumplen, explicar cómo se abordó el problema (por ejemplo: usar errores robustos, transformar la variable, elegir un test alternativo). No todas las herramientas tienen los mismos supuestos: lo que se pide es que identifiquen los relevantes para cada caso.
- Presentar los resultados con su **interpretación**. Todo resultado mostrado debe estar interpretado en el contexto del trabajo. Un coeficiente, un p-valor o un estadístico no hablan solos: deben decir qué implican para la hipótesis.

### 9. Conclusión

¿Se comprueban las hipótesis planteadas? Responder con base en la evidencia presentada a lo largo del trabajo. La conclusión no debe introducir resultados nuevos, sino sintetizar lo ya mostrado.

### 10. Próximos pasos

¿Qué cosas creen que sería útil hacer para mejorar o profundizar el trabajo? Pueden mencionar limitaciones de los datos, herramientas que no pudieron aplicar, variables que sería interesante incorporar, o análisis complementarios que fortalecerían las conclusiones.

---

## Visualizaciones

El trabajo debe incluir las **dos visualizaciones realizadas en la tercera instancia**, articuladas con el flujo del análisis. No pueden aparecer desconectadas del argumento.

Además de esas dos, pueden incluir todas las visualizaciones adicionales que consideren necesarias. En todos los casos deben cumplir con los criterios vistos en el curso:

- **Si son comunicacionales**: respetar los criterios estéticos, de storytelling y de editorialización.
- **Si son exploratorias**: ser claras, estéticas y comprensibles.

### Requisitos de formato para todos los gráficos

- Todos los elementos deben estar **en castellano**.
- Los labels deben ser legibles y descriptivos. Si la variable es "PIB per cápita (USD)", el gráfico debe mostrar exactamente eso, no `gdp_per_cap` ni ningún otro nombre de variable crudo.

---

## Referencia

Se compartieron oportunamente ejemplos de trabajos que cumplen con los criterios estéticos y de contenido esperados. Úsenlos como referencia. Tengan en cuenta que en esos trabajos prácticos se esperaba un alcance menor al de esta entrega final, ya que no eran el producto de un cuatrimestre completo. El nivel de profundidad, rigurosidad y calidad de esta entrega debe ser acorde al recorrido hecho a lo largo de todo el curso.
