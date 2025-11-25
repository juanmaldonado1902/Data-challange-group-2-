# Data-challange-group-2-
Repositorio para el data challange
Dentro del repositorio se encuentra el siguiente contenido:

1.- Un link de un Drive que lleva a la base de datos "final" que se ocupo para la realización del modelo econométrico. En dicha base se realiza la unificación de: la búsqueda de datos manual de valor promedio del metro cuadrado por colonia, la creación de la estimación de los metros cuadrados por airbnb dado sus cuartos y baños (explicado en el apéndice), dummies para la pertenencia a cada colonia a partir de datos encontrados en "datos abiertos de la CDMX" y del uso de la latitud y longitud para ubicar a cada airbnb, las dummies para las 20 ameninades que más corrrelación tienen con la variable rendimiento y por último la columna que representa el cálculo del "rendimiento_real" (explicada en el apéndice).

2.- Un conjunto de imágenes que representan gráficas necesarias para darnos idea sobre la estructuración del modelo, entre ellas se encuentran: Top 20 ameninades que se relacionan con la variable de rendimiento, Top 20 ameninades con mayor frecuencia en los airbnbs, Top 20 colonias con mayor número de lisntigs, matríz de correlación entre las amenidades, box- plot log(rendimiento) por room type, distribución log(rendimiento) con tratamiento y sin tratamiento, scatterplot (rendimiento- review score).

3.- Conjunto de mapas del log(rendimiento) con restricciones (número de lintings mínimo) y con distintas configuraciones (Z-score y regular).

4.- Estadística descriptiva de la variable log(rendimiento)

5.- Apéndice que únifica en un word los resultados "extra" del modelo y las configuraciones de las variables.

6.- Modelo final restringido a más de 25 listings por colonia en formato html.

7.- Apéndice estadístico que unifica toda la información generada.

8.- Código principal del modelo contiene: generación variable rendimiento, modelo principal, generación del mapa rendimientos por colonia y cálculo de los metro cuadrado por departamento (Funciona con la base de datos final).

9.- Código generación análisis espacial, unión de distancias y cálculo del modelo para determinar relación con la variable rendimiento.


El uso de la IA dentro del modelo fue ocupada para la codificación y el manejo de las bases de datos. Especialmente fue particularmente útil en la creación de los mapas y en la posibilidad de crear unificación geo-espaciales por colonia. La inteligencia artificial ocupada fue CHATGPT.

Trabajo realizado por cada participante

Juan Pablo Maldonado Rojas 000202145 : Configuración de la base de datos final, generación tabla de rendimientos por colonia, planteamiento del modelo econométrico por colonia y   creación del repositorio y el readme

Alberto Reyes : Realización análisis distancias (Búsqueda y match de la información geoespacial), estudio correlaciones con el rendimiento real y realización modelo respecto ditancias.

Alex Cano : Realización del apéndice estadístico, estudio de correlaciones y organización del texto final.
