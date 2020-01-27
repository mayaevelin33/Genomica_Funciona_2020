##genimica funcional
##Tarea 1
##Ejercicio 1 
##A partir de las redes de la figura 1. Calcula con igraph, las siguientes propiedades:
##a) Vecinos.
##b) La distribuci´on de conectividades
##c) El nodo m´as conectado.
##d) El di´ametro
##e) La matriz de distancias y el heatmap asociado.
##Primera figura 
##primero se carga la libreria igraph para poder realizar el ejercicio
library(igraph)
##se utilizo make_empty_graph para realizar el grafico
fig1 <- make_empty_graph(n=10, directed = FALSE )
V(fig1)$color="darkgoldenrod"
plot(fig1)
fig1 <- add.edges(fig1,c(1,10, 1,2, 1,3, 1,4, 1,5, 1,6, 1,7, 1,8, 1,9))
plot(fig1)
##a) Vecinos.
vecinos1 <- degree(fig1)
vecinos1
##b) La distribuci´on de conectividades
transitivity(fig1)
##c) El nodo m´as conectado.
sort(degree(fig1))
head(sort(degree(fig1),decreasing=TRUE),1)
##d) El di´ametro
diameter(fig1)
##e) La matriz de distancias y el heatmap asociado.
distMatrix <- shortest.paths(fig1)
distMatrix
##figura 2
eve2<-make_empty_graph(n=10, directed = FALSE)
V(eve2)$color="firebrick1"
plot(eve2)
eve2<-add.edges(eve2,c(10,7, 10,9, 10,2, 10,1, 1,8, 1,2, 1,9, 1,4,
                       9,8, 9,2, 9,4, 7,3, 7,5, 7,4, 7,2, 
                       7,9, 3,5, 3,8, 8,4, 8,6, 6,2, 6,5, 6,4, 5,8,
                       5,4, 9,6))
plot(eve2)
##a) Vecinos.
vecinos2<-degree(eve2)
vecinos2
##b) La distribuci´on de conectividades
transitivity(eve2)
##c) El nodo m´as conectado.
sort(degree(eve2))
head(sort(degree(eve2),decreasing=TRUE),1)
##d) El di´ametro
diameter(eve2)
##e) La matriz de distancias y el heatmap asociado.
distMatrix <- shortest.paths(eve2)
distMatrix
##figura 3 
eve3 <- make_empty_graph(n = 10, directed = FALSE)
V(eve3)$color = "orangered"
plot(eve3)
eve3 <- add.edges(eve3,c(1,2, 2,3, 3,4, 4,5, 5,6, 6,7, 7,8, 8,9, 9,10, 10,1))
plot(eve3)
##a) Vecinos.
vecinos3<-degree(eve3)
vecinos3
##b) La distribuci´on de conectividades
transitivity(eve3)
##c) El nodo m´as conectado.
sort(degree(eve3))
head(sort(degree(eve3),decreasing=TRUE),1)
##d) El di´ametro
diameter(eve3)
##e) La matriz de distancias y el heatmap asociado.
distMatrix <- shortest.paths(eve3)
distMatrix
##figura 4
eve4 <- make_empty_graph(n = 10, directed = FALSE)
V(eve4)$color = "steelblue4"
plot(eve4)
eve4 <- add.edges(eve4,c(1,2, 2,9, 1,8, 1,3, 3,4, 1,6, 1,5, 5,7, 5,10))
plot(eve4)
##a) Vecinos.
vecinos4<- degree(eve4)
vecinos4
##b) La distribuci´on de conectividades
transitivity(eve4)
##c) El nodo m´as conectado.
sort(degree(eve4))
head(sort(degree(eve4),decreasing=TRUE),1)
##d) El di´ametro
diameter(eve4)
##e) La matriz de distancias y el heatmap asociado.
distMatrix <- shortest.paths(eve4)
distMatrix
##Ejercicio 2 
##Elabora un programa en R que utilice un ciclo for para a partir del vector
##v siguiente
##v <-sample(100)
##imprima los cuadrados de los numeros impares.
v <- sample(100)
v
##se utilizo el cilco for para poder realizar los cuadrados de los numeros impares, asiganadole que los numeros sean divisibles entre dos y asigando que sean los impares con el 1 

for (i in v) {
  if ( i%% 2 ==1) 
    print(i^2)
  }
##se utiliza el ciclo for para realizar los impares 
v<-sample(100)
for (i in v) {
  if((i%%2==1)){
    print(i)
  }
}

##Ejercicio 3
## Elabora un programa en R que a partir del archivo de amistades del grupo.
##a) Cargue el archivo
##Para cargar el archivo se utiliza la funcion read.cvs y se asigna en un vector 
dani <- read.csv("Red_de_Amigas_GF_2020 - Sheet1.csv")
View(dani)
##ahora quitamos una columna 
row.names <- dani[,1]
dani <- dani[,-1]
dani <- as.matrix(dani)
daniela <- graph_from_adjacency_matrix(dani,mode ="directed")
plot(daniela)
##b) Genere el vector de nombres de todos tus amigos (los tuyos)
##se asigno otro nombre al vector y se utilizo which para poder extraer solo a los aque considero mis amigos y el parametro para decir que son amigos es que sea >=1 
amigos_evelin <-which(daniela["DanielaM",]>=1)
amigos_evelin
##c) Genere el vector de nombres de todos los que se consideren tus amigos.
##e asigno otro nombre al vector y se utilizo which para poder extraer solo a los que me consideran su amiga  y el parametro para decir que son amigos es que sea >=1 
amigos<-as.matrix(daniela)
amigos_daniela <-which(daniela[,"DanielaM"]>=1)
amigos_daniela 
names(amigos_daniela)
##d) Imprima el texto: “Hola amigo1”, en donde amigo1 es el nombre de cada uno de tus amigos.
##Primero se utilizo el vector amigos_evelin con wich para extraer solo a los que considero a mis amigos con el parametro >=1
amigos_evelin <-which(daniela["DanielaM",]>=1)
##se utilizo el ciclo for asigandole la longitud de todos mis amigos y peguen los nombres de mis amigos 
for (i in 1:length(amigos_evelin)) {
  print(paste("Hola amigo", names(amigos_evelin)[i]))
}
##despues para comprobar realize un vector con amigos para ver si el parametro de >=1 cumplia de que solo resultaran mis amigos 
amigos<-as.matrix(daniela)
amigos
