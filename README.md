# CREM Scrapper
## Motivación
Este repositorio se crea para la Práctica 1 de la asignatura _Tipología y ciclo de vida de los datos_ del Master of Data Science de la UOC y pretende a la vez demostrar las capacidades de Web Scrapping y recopilar datos útiles de la base de datos del Centro Regional de Estadistica de Murcia
## Miembros
Este repositorio ha sido creado por Jose Luis Garcia Bravo y Francisco Alonso Sarria
## Organización del repositorio
En el directorio _/src_ se encuentran los ficheros fuentes usados para recopilar los datos.

En el directorio _/data_ se encuentra la base de datos final (subida a Zenodo por motivos de espacio)

En el directorio _/tables_ se encuentran las tablas resumen de los datos.

## Ultima actualización de los datos
8 de Noviembre de 2020
## Formato del dataset
El dataset se compone de un solo fichero JSON.
En el directorio _/data_ hay información mas específica de su formato

## Fuentes
Todo el dataset está recopilado desde la web del Centro Regional de Estadística de Murcia, concretamente desde este link:
https://econet.carm.es/web/crem/informacion-de-la-a-z

## Utilización
El fichero CREM_scraper.R tiene el codigo necesario para crear todos los ficheros de salida (JSON con las tablas y los 3 archivos .csv)
Hemos detectado que si la web realiza cambios en su estructura o se añaden ficheos, los datos incluidos en el fichero getnc.R dejan de ser válidos y deben modificarse adecuadamente. Se está trabajando en una versión automatizada de esta función.

## Licencia
La licencia escogida para este repositorio es [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/) con la intención de que pueda ser usado libremente siempre que no sea usado con fines comerciales y los trabajos derivados sigan manteniendo el mismo tipo de licencia, de manera que se siga comparitiendo el conocimiento.
Si lo deseas puedes acceder a los [Terminos Legales](https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode.es)

