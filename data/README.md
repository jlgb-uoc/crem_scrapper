La base de datos creada con todas las tablas disponibles ocupa mas de 75MB, por lo que no se puede usar GitHub para almacenarla y compartirla.
Hemos optado por compartir los datos mediante Zenodo en el siguiente dataset:

<a href="https://doi.org/10.5281/zenodo.4262216"><img src="https://zenodo.org/badge/DOI/10.5281/zenodo.4262216.svg" alt="DOI"></a>

Además de las tablas originales, cuyo contenido tambien se pude visualizar mediante la [aplicacion Shiny](https://alonsarp.shinyapps.io/aplicacion/), incluimos 3 tablas CSV que contienen:

1. estructura.csv - Estructura del fichero JSON con los nombres de los temas, subtemas y tablas segun el CREM
2. tablasMunicipales.csv - Subset de las tablas conteniendo unicamente aquellas referidas a los municipios, con una fila por municipio y todas las columnas de todas las tablas. La descripcion de las columnas se encuentra en el fichero clavesColumnas.csv
3. clavesColumnas.csv - Recopilación de todas las columnas del fichero tablasMunicipales.csv incluyendo el tema, subtema tabla y columna donde estan recogidas en la web del CREM


