# Segunda entrega del TP de Ciencia de Datos para Economía y Negocios

**Prof. Nicolás Sidicaro**

## Consigna

Esta entrega tiene un doble objetivo:

1. **Incorporar los comentarios** realizados en la primera entrega sobre la hipótesis de trabajo (tanto principal como complementaria), así como también la posibilidad de reformular las variables y la base de datos escogida.

2. **(Parte obligatoria)** Proponer distintos métodos de inferencia y herramientas cuantitativas para tratar los datos con los que ustedes trabajan, de forma tal de poder responder a las hipótesis planteadas.

Recuerden que esta etapa tiene un carácter **propositivo**: en caso de tener una respuesta favorable por parte del docente, deberán implementar dicha estrategia con los datos en el TP final y deberán interpretar los resultados. En esta instancia no hace falta entregar código ni resultados — no se precipiten.

Toda propuesta que realicen deberá ser **factible** y comprensible para ustedes como alumnos. El ejercicio busca que muestren apropiación de las herramientas vistas en clase: propuestas demasiado simples que no reflejen ese trabajo van a ser revisadas por el docente y eso impactará en la nota de este ítem. Propuestas demasiado complejas pueden resultar difíciles de implementar en el TP final. Por eso, **busquen el equilibrio entre algo interesante y algo abordable**.

---

## Ayuda y consejo

El tipo de herramientas que propongan dependerá del tipo de base de datos con la que cuentan. La metodología recomendable varía según la estructura de los datos:

### Series de tiempo

*Pocos individuos y variables, muchos períodos.*

- Análisis descriptivo con indexaciones y tasas de variación (con intervalos cuando corresponda).
- Deflactar variables nominales cuando sea necesario.
- Identificar tendencia, estacionalidad y residuo (descomposición clásica o STL).
- Comparaciones inferenciales entre subperíodos o entre series (tests de medias, por ejemplo, si tienen varios países y quieren analizar si difieren significativamente de algún grupo de interés).

### Corte transversal

*Muchos individuos para un único momento.*

- Tests de hipótesis.
- Regresión lineal (con interpretación de coeficientes, dummies, transformaciones logarítmicas).
- Índices de concentración, desigualdad y ventajas comparativas reveladas (Gini, HHI, etc.).
- Estadísticas descriptivas con desagregaciones por grupos.

### Panel corto

*Varios individuos para pocos períodos (más de uno, pero no suficientes para hacer series de tiempo).*

- Las herramientas de corte transversal son aplicables.
- Pueden agregar comparaciones entre períodos y análisis de variación intra/entre individuos.
- En este caso podrán incorporar también test pareados por ejemplo
- No corresponde aplicar herramientas propiamente de series de tiempo.

### Índices complejos (aplicable a cualquier estructura)

Pueden proponer la construcción de un **índice compuesto** (al estilo del IDH, el Índice de Pobreza Multidimensional o índices de estabilidad financiera). En ese caso, deberán:

- Justificar la elección de las variables que integran el índice.
- Fundamentar los pesos asignados a cada componente.
- Discutir cómo cambia el índice si se modifican esos pesos (análisis de sensibilidad básico).

---

## Formato de respuesta

Para responder esta segunda instancia, deberán completar el siguiente formulario [link](https://docs.google.com/forms/d/e/1FAIpQLSedXTDrxCpBUKc2iIuw2DE6rVcPT740OzIu8gPcl4SHSqcDAg/viewform?usp=publish-editor). El formulario contiene dos secciones:

### 1. Reformulación de hipótesis

Espacio para reformular la hipótesis (en caso de ser necesario) o volver a escribirla tal como la habían planteado en la primera entrega.

### 2. Propuesta metodológica

Deberán proponer **tres métodos** para abordar el problema. Estos pueden ser complementarios entre sí (cada uno aporta evidencia distinta a la misma hipótesis) o alternativos (formas distintas de evaluar lo mismo) — en cualquier caso, explíciten cuál es la lógica que articula los tres.

Para **cada uno de los tres métodos**, indiquen:

- **(a)** Qué técnica proponen.
- **(b)** Qué variables utilizarían.
- **(c)** Cómo se conecta el método con la hipótesis (principal o complementaria) que buscan responder.
- **(d)** Qué esperan obtener y cómo interpretarían el resultado.

Recuerden que **todos los ejercicios deben ayudar a pensar y responder la hipótesis planteada**.

---

## Devolución del docente

En función del tema, la complejidad interpretativa y de cálculo, el docente podrá:

- Quedarse con dos de los tres métodos propuestos (en caso de estar bien planteados).
- Proponer un tercer método (o más) si la propuesta resulta escasa respecto de lo visto en clase.
- Sugerir ajustes sobre las técnicas propuestas.

---

## Adelanto del trabajo final

Como anticipo, en el TP final deberán incorporar — además de los ejercicios mencionados — **estadísticas descriptivas** que ayuden a transmitir el mensaje central del trabajo y a comprender el porqué de la hipótesis. Esto **no entra en esta instancia**, pero conviene tenerlo presente al diseñar la propuesta.
