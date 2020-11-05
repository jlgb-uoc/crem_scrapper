# Crea un data.frame con los nombres y los links presentes en obj vía el xpath
extraeLinks <- function(obj, xpath, urlBase=NULL) {
nodos = rvest::html_nodes(obj, xpath=xpath)
nombres = rvest::html_text(nodos)
enlaces = rvest::html_attr(nodos, "href")
if (!is.null(urlBase)) enlaces = paste0(urlBase,"/",enlaces)
return(data.frame(nombres=nombres, enlaces=enlaces))
}
7
# Reemplaza NA por val en un data.frame.
replace_na <- function(tabla,val) {
for (c in 1:ncol(tabla)) {
tabla[which(is.na(tabla[,c])),c] = val
}
return(tabla)
}
espera = 2 # Tiempo en segundos antes de cada llamada
# Compruebo en primer lugar que puedo proceder:
robotstxt::get_robotstxt(domain= "https://econet.carm.es")
#Leo la página base que es la que contiene los enlaces a los grandes temas.
url0 <- "https://econet.carm.es/web/crem/informacion-de-la-a-z"
html0 <- xml2::read_html(url0)
# Los enlaces a grandes temas aparecen en dos columnas y a cada columna se accede
# con un xpath diferente. Así que accedo a cada columna por separado y luego las
# integro
xpath1 = "//div/div/div[1]/div/div/div/div/div/div/div/div[1]/ul/li/a"
xpath2 = "//div[2]/ul/li/a"
nivel1 = rbind(extraeLinks(html0, xpath1),
extraeLinks(html0, xpath2))
# Para cada uno de los grandes temas leo el segundo nivel. Guardo los enlaces
# junto con los nombres en nivel2. El vector temas1 contiene el número del tema
# principal de cada entrada.
for (n1 in 1:nrow(nivel1)) {
cat(n1, "/", nrow(nivel1), "\n")
url1 = nivel1[n1,2]
html1 <- xml2::read_html(url1)
urlBase=url1
if(grepl("html",url1)) {
urlBase=paste0(head(strsplit(url1, "/", fixed=TRUE)[[1]],-1), collapse="/")
}
xpath = "//td/table/tbody/tr/td/a"
niv2 = extraeLinks(html1, xpath=xpath, urlBase=urlBase)
niv2 = niv2[grep("Indice",niv2[,2]),] # Me quedo con enlaces a datos
if (n1==1) {
nivel2 = niv2
temas1 = rep(n1, nrow(niv2))
8
} else {
nivel2=rbind(nivel2, niv2)
temas1 = c(temas1, rep(n1, nrow(niv2)))
}
Sys.sleep(espera)
}
#Acceso a las páginas de subtemas temas
# El siguiente paso es repetir el proceso anterior, pero ahora para descubrir y
# extraer las entradas de nivel 3 que hay en las de nivel 2. En la mayoría de
# los casos solo se llega al nivel 3, pero en algunos se alcanza el nivel 4.
# Cuando el nivel 3 contiene tablas, los nombres de los ficheros html contienen
# las palabras clave sec o pagina. Cuando contiene las entradas de nivel 4,
# la palabra clave es indice.
# El problema es que en el nivel 4 los xpath no son sistemáticos como en los
# niveles anteriores, por lo que ha habido que analizar cada caso por separado.
# El conocimiento adquirido se integra en una función llamada getnt que en función
# de la entrada de nivel 2 en la que estemos devuelve un número con el que formar
# la cadena correcta para el xpath.
#Para simplificar la estructura de datos final, todas las tablas de nivel 4 se
# almacenan como tablas de nivel 3 ya que los captions correspondientes son
# distintos:
source("getnc.R")
for (n2 in 1:nrow(nivel2)) {
cat(n2, "/", nrow(nivel2), "\n")
if (n2==88) next # excepción
url2 = nivel2[n2,2]
urlBase=url2
if(grepl("html",url2)) {
urlBase=paste0(head(strsplit(url2, "/", fixed=TRUE)[[1]],-1), collapse="/")
}
html2 <- xml2::read_html(url2)
xpath = "//td[2]/table/tbody/tr[2]/td/table/tbody/tr/td/table/tbody/tr/td/a"
niv3 = extraeLinks(html2, xpath=xpath, urlBase=urlBase)
# Pero que pasa cuando hay un enlace al cuarto nivel????
# Cuales tienen 4o nivel pero no graficos?
w = which(grepl("Indice",niv3[,2]) & !(grepl("GRÁFICOS",niv3[,1])))
# para todos ellos, ábrelo y extrae las tablas.
for (ww in w) {
9
html3 <- xml2::read_html(niv3[ww,2])
nt=getnt(n2,ww)
xpath=paste0("//tr[",nt,"]/td[2]/table/tbody/tr/td/table/tbody/tr/td/a")
if (n2==35 & ww==1) {
xpath="//td/table/tbody/tr[2]/td[2]/table/tbody/tr/td/table/tbody/tr/td/a"
}
niv4 = extraeLinks(html3, xpath=xpath, urlBase=urlBase)
niv3 = rbind(niv3, niv4)
}
niv3 = niv3[grep("sec|pagina",niv3[,2]),] # Me quedo con enlaces a tablas
if (n2==1) {
nivel3 = niv3
temas2 = rep(n2, nrow(niv3))
} else {
nivel3=rbind(nivel3, niv3)
temas2 = c(temas2, rep(n2, nrow(niv3)))
}
Sys.sleep(espera)
}
# Acceso a las tablas
# El último paso del scraping es leer las tablas. Este proceso es algo más sencillo
# ya que, aunque todas las páginas que contienen una tabla tienen en realidad
# varias, siempre es la número 4 o la número 6 la que contiene la información,
# con lo que es fácil extraerla combinando las funciones html_table y html_nodes
# del paquete rvest:
tablas = list()
nt = 0
for (n3 in 1:nrow(nivel3)) {
cat(n3, "/", nrow(nivel3), "\n")
url3 = nivel3[n3,2]
html3 <- xml2::read_html(url3)
nt = nt+1
tbs = rvest::html_nodes(html3, "table")
if(length(tbs)==0) next
if (length(tbs)>=6) {
tablas[[nt]] <- rvest::html_table(tbs[6])[[1]]
} else {
tablas[[nt]] <- rvest::html_table(tbs[4],fill = TRUE)[[1]]
}
Sys.sleep(espera)
}
10
# Un poco de data cleaning en las tablas
# En este momento hacemos un poco de data cleaning en las tablas. Consiste en
# convertir los “.” en "" y luego las “,” en “.”. El mismo script detecta tablas
# nulas que hemos comprobado que son enlaces rotos.
for (tb in 1:length(tablas)) {
if (is.null(tablas[[tb]])) {
cat(tb, "/", length(tablas), "\n")
} else {
# Convierto en numérico las celdillas que cumplen esta regExp
rexpReal = "^(\\+|-)?((\\d*).?)*\\,?(\\d*)$"
for (r in 1:nrow(tablas[[tb]])) { for (c in 1:ncol(tablas[[tb]])) {
if (grepl(rexpReal, tablas[[tb]][r,c])) {
tablas[[tb]][r,c] = gsub(",", ".",
gsub(".","",tablas[[tb]][r,c], fixed=TRUE),
fixed=TRUE)
}
}}
}
}
# Meterlo todo en una lista. La figura incluida en el punto 4 representa
# la estructura de esta lista.
lista_temas1 = list()
nt = 0
for (n1 in 1:nrow(nivel1)) {
lista_temas1[[n1]] = list(tema = nivel1[n1,1], subtemas=list())
w = which(temas1==n1)
for (n2 in 1:length(w)) {
ll = list(tema=nivel2[w[n2],1], subtemas=list())
lista_temas1[[n1]]$subtemas[[n2]] = ll
w2 = which(temas2==w[n2])
if (length(w2)>0) {
for (n3 in 1:length(w2)) {
nt = nt+1
if (nt %in% c(1321,2021,2293)) {
ll = list(tema=nivel3[w2[n3],1], tabla=NULL)
} else {
ll = list(tema=nivel3[w2[n3],1], tabla=tablas[[nt]])
ll$tabla = replace_na(ll$tabla,-9999)
}
lista_temas1[[n1]]$subtemas[[n2]]$subtemas[[n3]] = ll
11
}
}
}
}
# Y guardarla en un fichero JSON
jsonlite::write_json(lista_temas1, "CREM.json")
