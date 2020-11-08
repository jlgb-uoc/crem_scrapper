# Funcion que crea un data.frame con los nombres y los links presentes en obj vía el xpath
extraeLinks <- function(obj, xpath, urlBase=NULL) {
    nodos    = rvest::html_nodes(obj, xpath=xpath)
    nombres  = rvest::html_text(nodos)
    enlaces  = rvest::html_attr(nodos, "href")
    if (!is.null(urlBase)) enlaces = paste0(urlBase,"/",enlaces)
    return(data.frame(nombres=nombres, enlaces=enlaces))
}

# Funcion que reemplaza NA por val en un data.frame.
replace_na <- function(tabla,val) {
     for (c in 1:ncol(tabla)) {
        tabla[which(is.na(tabla[,c])),c] = val
     }
     return(tabla)
}

# Comprobamos en primer lugar que puedo proceder:

robotstxt::get_robotstxt(domain= "https://econet.carm.es")

#Leemos la página base que es la que contiene los enlaces a los grandes temas.

url0 <- "https://econet.carm.es/web/crem/informacion-de-la-a-z"
html0 <- xml2::read_html(url0)

#Los enlaces a grandes temas aparecen en dos columnas y a cada columna se accede con un xpath diferente. Así que accedemos a cada columna por separado y luego las integramos

xpath1 = "//div/div/div[1]/div/div/div/div/div/div/div/div[1]/ul/li/a"
xpath2 = "//div[2]/ul/li/a"
nivel1 = rbind(extraeLinks(html0, xpath1), 
               extraeLinks(html0, xpath2))

# Para cada uno de los grandes temas leemos el segundo nivel. Guardamos los enlaces junto con los nombres en nivel2. El vector temas1 contiene el número del tema principal de cada entrada.

for (n1 in 1:nrow(nivel1)) {
    cat("Progreso:",n1, "/", nrow(nivel1), "\n")
    url1 = nivel1[n1,2]
    html1 <- xml2::read_html(url1)

    urlBase=url1
    if(grepl("html",url1)) { 
      urlBase=paste0(head(strsplit(url1, "/", fixed=TRUE)[[1]],-1), collapse="/")
    }

    xpath = "//td/table/tbody/tr/td/a"
    niv2 = extraeLinks(html1, xpath=xpath, urlBase=urlBase)
    niv2 = niv2[grep("Indice",niv2[,2]),]  # Nos quedamos con enlaces a datos
    if (n1==1) {
        nivel2 = niv2 
        temas1 = rep(n1, nrow(niv2))
    } else {
        nivel2=rbind(nivel2, niv2)
        temas1 = c(temas1, rep(n1, nrow(niv2)))
    }
    Sys.sleep(runif(1, min = 0.5, max = 3))
}

# Importamos los datos que hemos recopilado a mano con las particularidades de los distintos indices
source("getnc.R")

# Extraemos el nivel 3 y el nivel 4 (que trataremos como un nivel 3)
for (n2 in 1:nrow(nivel2)) {
  
    cat("Progreso:",n2, "/", nrow(nivel2), "\n")
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
    # Cuales tienen 4º nivel pero no graficos?
    w = which(grepl("Indice",niv3[,2]) & !(grepl("GRÁFICOS",niv3[,1]))) 
    # para todos ellos, ábrelo y extrae las tablas.

    for (ww in w) {
       html3 <- xml2::read_html(niv3[ww,2])

       nt=getnt(n2,ww) 
       
       xpath=paste0("//tr[",nt,"]/td[2]/table/tbody/tr/td/table/tbody/tr/td/a")
       if (n2==35 & ww==1) {
          xpath="//td/table/tbody/tr[2]/td[2]/table/tbody/tr/td/table/tbody/tr/td/a"
       }

       niv4 = extraeLinks(html3, xpath=xpath, urlBase=urlBase)
       niv3 = rbind(niv3, niv4)
    }
    niv3 = niv3[grep("sec|pagina",niv3[,2]),]  # Nos quedamos con enlaces a tablas

    if (n2==1) {
        nivel3 = niv3 
        temas2 = rep(n2, nrow(niv3))
    } else {
        nivel3=rbind(nivel3, niv3)
        temas2 = c(temas2, rep(n2, nrow(niv3)))
    }
    Sys.sleep(runif(1, min = 0.5, max = 3))
}

tablas = list()
nt = 0

# Solo nos falta leer las tablas de datos, que se situan siempre en la tabla 4 o 6, y las almacenamos
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
    Sys.sleep(runif(1, min = 0.5, max = 3))
}

# Un poco de data cleaning en las tablas

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


#  Lo introducimos en un JSON

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

           }
        }
     }
}
 
# Las escribimos en un fichero

jsonlite::write_json(lista_temas1, "CREM.json")

# Creamos una supertabla con todos los datos para crear los ficheros de estructura y municipal

estructura = data.frame(nivel1 = nivel1[temas1[temas2],1],
                        nivel2 = nivel2[temas2,1],
                        nivel3 = nivel3[,1])

# Para arreglar inconsistencias en los nombres de algunos municipios.
tablas[[listaTab[34]]][17,1]="Caravaca de la Cruz"
tablas[[listaTab[35]]][17,1]="Caravaca de la Cruz"
tablas[[listaTab[39]]][17,1]="Caravaca de la Cruz"
tablas[[listaTab[39]]][40,1]="Torre-Pacheco"
for (k in c(45:47, 52:54,122)) {
   tablas[[listaTab[k]]][23,1]="Fuente Ãlamo"
   tablas[[listaTab[k]]][35,1]="Puerto Lumbreras"
}

# Creo la tabla de municipios
k=0
listaTab = c()
rm("supertabla")
for (nt in 1:length(tablas)) {
   cond1 = any(grep("olina",tablas[[nt]][,1]))
   cond2 = !is.null(nrow(tablas[[nt]]))
   if (cond1 & cond2) {
       if (nrow(tablas[[nt]])==46 & !(nt %in% c(1885:1886,2578,3587))) {
          k=k+1
          listaTab[k] = nt
          if (exists("supertabla")) {
              supertabla = merge(supertabla,tablas[[nt]],
                                 by.x=names(supertabla)[1],
                                 by.y=names(tablas[[nt]])[1])
          } else {
              supertabla = tablas[[nt]]
          }
          cat(k,nt,dim(tablas[[nt]]),dim(supertabla),"\n")
          if (nrow(supertabla)<45) break
       }
   }
}

# Pongo nombres a las columnas y preparo el dataframe con los nombres detallados
# de las columnas 
ncols = 0
daf = data.frame(numcol="municipio", namecol="municipio")
for (nt in listaTab) {
    numcols = (ncols + 1):(ncols + ncol(tablas[[nt]]) -1)
    columnas = paste0( paste0(estructura[nt,],collapse=";"),
                       colnames(tablas[[nt]])[-1], sep=";")
    daf = rbind(daf,
                data.frame(numcol=paste0("columna_", numcols),
                           namecol=columnas))
    ncols = ncols + ncol(tablas[[nt]]) -1
}
names(supertabla) = c("municipio", paste("columna_",2:3260))

# Convierto valores numÃ©ricos
regexp = "^(\\d+)\\.?(\\d+)?$"
for (cl in 2:ncol(supertabla)) {
    if(all(grepl(regexp,supertabla[,cl]))) {
        supertabla[,cl] = as.numeric(supertabla[,cl])
    }
}

# Escribo los ficheros.
write.table(supertabla, "tablasMunicipales.csv", quote=FALSE, row.names=FALSE, sep=";")
write.table(daf, "clavesColumnas.csv", quote=FALSE, row.names=FALSE, sep=";")
write.table(estructura, "estructura.csv", quote=FALSE, row.names=FALSE, sep=";")