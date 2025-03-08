# Proyecto Final de ML

## Requerimientos

El proyecto final consistirá en proponer un problema relevante y diseñar, desarrollar y evaluar un sistema basado en aprendizaje automático para resolver dicho problema. Los proyectos pueden ser realizados de forma individual o en equipo de 2 personas.

La propuesta debe ser de máximo 3 cuartillas y debe incluir:

- Título
- Descripción y delimitación del problema
- Objetivos
- Justificación
- Conjunto de datos a utilizar (o estrategia para recopilarla)
- Análisis exploratorio del conjunto de datos
- Conformación del equipo (en su caso)

> Sugerencias: Piensa en problemas donde aplicar aprendizaje automatizado y elige aquellos que puedas realizar durante el semestre y en los que existan suficientes datos disponibles. Puedes tomar ideas de competencias (por ej. en Kaggle,  CLEF, Iberlef, SEMEVAL) o de problemas existentes en otras áreas que puedas abordar con aprendizaje automatizado.

---

## Propuesta

### TW-ICE (Time Windown for Ice Flow Prediction)

### Descripción y delimitación del problema

Debido al gran avance que ha habido en la tecnología, se ha facilitado la tarea de obtención y almacenamiento de grandes volúmenes de datos. Particularmente en el ámbito de (clima o finanzas) estos datos se almacenan de manera cronológica de acuerdo a cierta medida de tiempo (días, semanas, meses) formando así series de tiempo. Al realizar un análisis de estos datos se puede obtener información que no solo describe el comportamiento actual del fenómeno sino el comportamiento futuro, permitiendo tomar medidas o acciones de acuerdo al valor futuro.

El cambio climático es uno de los problemas más importantes que enfrenta la humanidad en la actualidad. Uno de los efectos más visibles de este fenómeno es el derretimiento de los glaciares, lo que tiene un impacto directo en la elevación del nivel del mar. Por lo tanto, es importante poder predecir el flujo de hielo en los glaciares para poder tomar medidas preventivas y mitigar los efectos del cambio climático.

En el caso de los glaciares, se puede obtener una serie de tiempo sobre de la velocidad de flujo del hielo que se desplaza en un glaciar. Esta serie de tiempo se puede obtener a partir de datos de satélite y de sensores instalados en los glaciares. 

### Objetivos

El objetivo de este proyecto es desarrollar un sistema basado en aprendizaje automático que pueda predecir el flujo de hielo en los glaciares. Para ello, se utilizará una serie de tiempo elaborada a partir de datos de satélite y de sensores instalados en los glaciares del proyecto ITS_LIVE de la NASA.

### Justificación

El derretimiento de los glaciares es un problema que afecta a todo el planeta y que tiene consecuencias graves para la humanidad. Por lo tanto, es importante poder predecir el flujo de hielo en los glaciares para poder tomar medidas preventivas y mitigar los efectos del cambio climático.

### Conjunto de datos a utilizar

Los datos a utilizar serán los datos de satélite y de sensores instalados en los glaciares del proyecto ITS_LIVE de la NASA. Estos datos contienen información sobre la velocidad de flujo del hielo en los glaciares, así como sobre las condiciones climáticas y geográficas de los mismos.

Estos datos estan disponibles en el siguiente enlace: [ITS_LIVE](https://its-live.jpl.nasa.gov/) o en su visualizador: https://mappin.itsliveiceflow.science

### Análisis exploratorio del conjunto de datos

Los datos de ITS_LIVE contienen información sobre la velocidad de flujo del hielo en los glaciares, así como sobre las condiciones climáticas y geográficas de los mismos. Estos datos se presentan en forma de series de tiempo, lo que permite analizar su comportamiento a lo largo del tiempo.

En el notebook adjunto se presenta un análisis exploratorio de los datos de ITS_LIVE, en el que se muestra la distribución de la velocidad de flujo del hielo en los glaciares, así como su comportamiento a lo largo del tiempo.

### Conformación del equipo

- Luis Vicente Ruiz Hernández
- Rodrigo Sebastián Cortez Madrigal