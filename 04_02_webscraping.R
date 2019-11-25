#----------------------------------------------------------------------------
# Web scraping y manipulación básica de texto
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
## Web scraping Tecnica para bajar tablas de la web
#----------------------------------------------------------------------------
install.packages("rvest")
library(rvest)

url.ibex <- "http://www.bolsamadrid.es/esp/aspx/Mercados/Precios.aspx?indice=ESI100000000"

tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table")

length(tmp)
sapply(tmp, class)
sapply(tmp, function(x) dim(html_table(x, fill = TRUE)))
ibex <- html_table(tmp[[5]])

# LIMPIAR LA TABLA RENOMBRANDO LAS COLUMNAS
ibex[,colnames(ibex)]
colnames(ibex) <- c("nombre","ultimo","diferencia","maximo","minimo","volumen","efectivo","fecha","hora")
# REEMPLAZAR COMAS POR PUNTOS Y DEJAR FORMATEADOS LOS CAMPOS A NUMERICOS CON PUNTO PARA SER ANALIZADOS
vec <- 2:5
poblar <- function(v){
  ibex[v] <- as.numeric(gsub(",",".",ibex[,v]))
  
}
ibex[vec] <- sapply(vec, poblar)
# formatear las fechas
# foo
ibex$fecha
ibex$hora
# GPL no olvidar lo de compartir el código después de hecho (condición para poder entrar en la comunidad)
# 
# quita los puntos
# reemplza coma por punto y luego le aplicas numeri
# strptime("2013/08/19 13:34:25", "%Y/%m/%d %H:%M:%S") -. ejemplo --
# strptime(paste(ibex$Fecha, ibex$Hora, sep = " "),"%Y/%m/%d %H:%M:%S")
# xpath para sacar elementos de una pagina web
#//*[@id="proposal_27211_votes"]/div/span/text()

#=============================================================================
# solucion del compañero 
url.ibex <- "http://www.bolsamadrid.es/esp/aspx/Mercados/Precios.aspx?indice=ESI100000000"
tmp <- read_html(url.ibex)
tmp <- html_nodes(tmp, "table") #Extrae las tablas
tmp
sapply(tmp, function(x) dim(html_table(x, fill = TRUE)))
ibex <- html_table(tmp[[5]])
colnames(ibex) <- c("Nombre","Ult","Dif","Max","Min","Vol","Efec","Fecha","Hora")
dec <- function(x){
  x<-as.numeric(gsub(",",".",x))
}
ibex[2:5]<- lapply(ibex[2:5],dec)
head(ibex)
dec2<- function(x){
  x<- gsub("\\.","",x)
}
ibex[6:7] <- lapply(ibex[6:7],dec2)
ibex$Efec<-dec(ibex$Efec)
ibex$Vol<- as.numeric(ibex$Vol)
ibex$Fecha<-lapply(ibex$Fecha,dmy)
ibex$Tiempo <- paste(ibex$Fecha,ibex$Hora)
ibex$Fecha <- NULL
ibex$Hora <- NULL

as.Date(ibex$fecha,format("%d/%m/%Y"))

#==========================================================================================
# LIBRARY RVEST
#==========================================================================================
library(rvest)
ids <- 1:3000
#ids <- 319:321
res <- sapply(ids, function(i){
  print(i)
  url <- paste("https://decide.madrid.es/proposals/", i, sep = "")
  out <- try(kk <- read_html(url), silent = T)
  if (inherits(out, "try-error")) 
    return(NA)
  
  apoyos <- html_node(kk, xpath = '//*[@id="proposal_27211_votes"]/div/span/text()')
  apoyos <- html_text(apoyos)
})

save(res, file ="/home/carlos/visitas_madrid_decide_20150924.Rdat")
kk <- sapply(res, function(x) gsub("apoyo.*", "", x))
kk <- sapply(kk, function(x) as.numeric(gsub("\\.", "", x)))
names(kk) <- NULL
#==========================================================================================
# EJERCICIO EN CLASE
#==========================================================================================
tmp <- read_html("https://decide.madrid.es/proposals/")
apoyos <- html_node(tmp, xpath = '//*[@id="proposal_27211_votes"]/div/span/text()')
apoyos <- html_text(apoyos)
apoyos
#==========================================================================================
# TEST 8 preguntas y proceso de resolucion
#==========================================================================================
#==========================================================================================
# QUESTION 1
#==========================================================================================
#los paquetes de R se actualizan continuamente. Existe una función en R que permite actualizar todos los paquetes instalados en el sistema (y que conviene ejecutar periódicamente). Dicha función es…

#a. system.upgrade
#b. install.packages
#c. update.packages  # es esta
#d. upgrade.all.packages
#e. upgrade.packages
#==========================================================================================
# QUESTION 2
#==========================================================================================
#Usando la tabla que aparece en 
#https://en.wikipedia.org/wiki/List_of_countries_by_population_%28United_Nations%29,
#¿cuál es el incremento porcentual de la población
#en la “región estadística de la ONU” donde mayor fue tal incremento?
  
#a. 4.3544
#b. 2.8048
#c. 3.0667 # es esta
#d. 2.6866
#e. 2.6903

#======================================================================================
tmp <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_population_%28United_Nations%29")
traer_todo <- html_nodes(tmp,'table') # se trae todos los nodos del html
tablas <- html_table(traer_todo,dec = ",") # se trae las tablas
estadisticas <- tablas[[2]]


cuadrado.raro <- function(x) if(x < 5) x^2 else -x^2
sapply(1:10, cuadrado.raro)
#======================================================================================
ids <- 1:2
urls <- sapply(ids, function(i){
  url <- paste("http://www.sensacine.com/peliculas/mejores/nota-espectadores/?page=",i,sep = "")
})

divs_peli <- sapply(urls,function(url){
   pagina_html <- read_html(url)
   peliculas <- html_nodes(pagina_html,xpath = '//div[@class="data_box"]/div/div')
   
})


matriz_pelis <- sapply(divs_peli,function(pelicula){
  # Trae todos los elementos div que estan dentro de un elemento li
   
    atributo_lis <- html_nodes(pelicula,'li div')
    titulo <- html_node(pelicula,'h2 a')  # trae el unico elemento a que esta dentro de un elemento h2

    # Datos de mi nuevo Data Frame
    titulo <- html_text(titulo)
    
    ano <- html_text(atributo_lis[1])
    director <- html_text(atributo_lis[2])
    reparto <- html_text(atributo_lis[3])
    genero <- html_text(atributo_lis[4])
    
    # Limpiar Datos
    titulo  <-  gsub("\n","",titulo)
    ano <- gsub("\n","",ano)
    director <- gsub("\n","",director)
    reparto <-  gsub("\n","",reparto)
    genero <-  gsub("\n","",genero)
    
    lista <- c(titulo,ano,director,reparto,genero)
    
})

tabla <- as.data.frame(matriz_pelis)

#======================================================================================
pagina <- 'http://www.sensacine.com/peliculas/mejores/nota-espectadores/?page='
iteraciones <- 1:10
tmp <- read_html(paste(pagina,iteraciones))
tmp

#======================================================================================
# Usadas para web Scrcaping
#html_name() obtiene los atributos html
#html_text() extrae el texto html --> el value que me interesa
#html_attr() regresa los atributos específicos html
#html_attrs() obtiene los atributos html

#=================================================================================
# LIMPIAR LAS COLUMNAS PARA PODER OPERARLAS (POBLACION)
colnames(estadisticas) <- c('id',"pais","region_con","region_es","po_2018","po_2019","cambio")
head(estadisticas)
#


dec <- function(x){
  x<-as.numeric(gsub(",",".",x))
}
ibex[2:5]<- lapply(ibex[2:5],dec)
#==========================================================================================
# QUESTION 3
#==========================================================================================
#¿Cuál es el comando de formato adecuado de strptime para leer el timestamp que aparece en http://www.zaragoza.es/trafico/estado/estado.json ?
  
#a. “%Y%m%d-%H:%M:%S”
#b. “%Y%m%d %H:%M:%S”
#c. “%y%m%d %H%M%S”
#d. “%Y%m%d-%H%M%S” # es esta
#e. “%Y%m%d-%H:%M:%SZ”
trafico <- read_html("http://www.zaragoza.es/trafico/estado/estado.json", encoding = "UTF-8")

#trafico <- html_node(tmp, xpath = '//*[@id="proposal_27211_votes"]/div/span/text()')
#trafico <- html_text(apoyos)

strftime(fecha,"%Y%m%d-%H%M%S")

#==========================================================================================
# QUESTION 4
#==========================================================================================
#¿Cuál es un XPath válido para extraer mi foto de https://www.datanalytics.com/bio/?
  
#a. //*[@id="post-2"]/div/p[1]/a/img    # es esta
#b. //*[@id="post-1"]/p[1]/a/img
#c. //*[@id="post-1"]/div/p[2]/a/img
#d. //*[@id="post-2"]/p[1]/a/img
#e. //*[@id="post-2"]/div/p[2]/img

#/html/body/div/div/div/main/div/div[1]/article/div/p[1]/a/img
#//*[@id="post-2"]/div/p[1]/a/img

#==========================================================================================
# QUESTION 5
#==========================================================================================





