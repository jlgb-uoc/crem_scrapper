
estructura = data.frame(nivel1 = nivel1[temas1[temas2],1],
                        nivel2 = nivel2[temas2,1],
                        nivel3 = nivel3[,1])

# Para arreglar inconsistencias en los nombres de algunos municipios.
tablas[[listaTab[34]]][17,1]="Caravaca de la Cruz"
tablas[[listaTab[35]]][17,1]="Caravaca de la Cruz"
tablas[[listaTab[39]]][17,1]="Caravaca de la Cruz"
tablas[[listaTab[39]]][40,1]="Torre-Pacheco"
for (k in c(45:47, 52:54,122)) {
   tablas[[listaTab[k]]][23,1]="Fuente Álamo"
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

# Convierto valores numéricos
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

