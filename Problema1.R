### Proyecto de Probabilidad 1 ###
# Demian Alejandro Monterrubio Acosta
# Rodrigo García Padilla

# Cargamos una tabla de datos.


# Problema 1:
# Dada una base de datos de canciones de un usuario, queremos un que programa las reproduce al azar,
# pero no completamente al azar; las canciones que más le gustan deben salir más probablemente que las que no 
# (usando retroalimentación dada por el usuario). Tampoco deben salir las mismas canciones demasiado seguidas.


# Número de canciones con las que se va a realizar la prueba.
numCanciones <- 1000
# Vector que contiene en orden las calificaciones con las que el usurario calificó cada canción, en orden.
califPorCancion <- sample(6:10, numCanciones,  replace = TRUE, prob = c(1/10, 2.5/10, 3/10, 2.5/10, 1/10))
# Creamos una matriz de dos columnas, una matriz con las canciones (presentadas por un código numérico),
# y la otra columna para la calificación de cada canción.
canciones <- cbind(canción = 1:numCanciones, califPorCancion)
View(canciones)
# En cada vector urna se guardan los índices que ocuparan las urnas dentro del arreglo.
# Urna 1, todas las canciones que aún no se han escuchado.
urna1 <- c(0, 0)
# Número de canciones actuales en la urna 1.
n1 <- 0
# Urna 2, las canciones que ya se escucharon pero al usuario le gustan lo suficiente para que se vuelvan a escuchar.
# El índice superior de la urna 2 representará la urna 4, (solo puede haber una canción al mismo tiempo en la urna 4).
urna2 <- c(0, 0)
# Número de canciones actuales en la urna 2.
n2 <- 0
# Urna 3, las canciones que ya se escucharon y al usuario no le gustan lo suficiente para que se vuelvan a escuchar.
urna3 <- c(0, 0)
# Número de canciones actuales en la urna 3.
n3 <- 0

# Función para inicializar las urnas.
initUrnas <- function(){
  urna1 <<- c(1, numCanciones)
  n1 <<- numCanciones
  # El primer índice de la urna se sale del arreglo, esto solo es un truco para que el algoritmo de reacomodo de índices funcione.
  urna2 <<- c(numCanciones + 1, numCanciones)
  n2 <<- 0
  # El segundo índice de esta urna se sale del arreglo, esto solo es un truco para que el algoritmo de reacomodo de índices funcione.
  urna3 <<- c(1, 1 - 1)
  n3 <<- 0
}

reproduceAleatoria <- function(){
  probabilidad <- runif(1)
  # Si n1 == 0 significa que ya se han reproducido todas las canciones de la lista, entonces vaciamos todas las canciones dentro de la urna 1
  # para empezar el proceso de nuevo.
  if(n1 == 0){
    initUrnas()
  }
  # Se elige una urna al azar de donde escoger canción; P(urna1) = (n1+n3)/numCanciones; P(urna2) = n2/numCanciones; P(urna3) = 0
  if(probabilidad >= n2/numCanciones){
    print(paste("Canción a reproducir: ",  caso1()))
  }else{
    # Nos aseguramos que n2 sea diferente de 0, aunque teóricamente no sea necesario, lo hacemos para darle robustez al programa.
    if(n2 < 1){
      print(paste("Canción a reproducir: ",  caso1()))
    }else{
      print(paste("Canción a reproducir: ",  caso2()))
    }
  }
}

# Caso 1, se saca una canción de la urna 1, todas las canciones tienen probabilidad equiprobable de salir.
caso1 <- function(){
  c <- sample(urna1[1]:urna2[2], 1)
  
  print(urna1)
  
  cancion <- canciones[c,1]
  
  print(c)
  
  calif <- canciones[c,2]
  vectorIntercambio <- canciones[c,]
  if(calif == 10){
  # Caso en el que la canción le gusta mucho al usuario, se debe guardar la canción en la segunda urna.
    # Intercambiamos la canción elegida con la del último índice de la urna 1, luego decrementemos el índice superior de la urna 1
    # y decrementamos el índice inferior de la urna 2, así la urna 2 ahora tiene una canción más y la urna 1 una menos.
    canciones[c,] <<- canciones[urna1[2],]
    canciones[urna1[2],] <<- vectorIntercambio
    urna1[2] <<- urna1[2] - 1
    urna2[1] <<- urna2[1] - 1
    # Luego colocamos la canción elegida en el último índice del arreglo, lo que sería correspondiente a la urna 4, para asegurarnos
    # que no se repita inmediatamente después.
    vectorIntercambio <- canciones[urna2[1],]
    canciones[urna2[1],] <<- canciones[urna2[2],]
    canciones[urna2[2],] <<- vectorIntercambio
    # Se actualiza el número de canciones en cada urna
    n1 <<- n1 - 1
    n2 <<- n2 + 1
  }else{
  # Caso en el que la canción no le gusta tanto al usuario, se debe guardar en la tercer urna.
    # Intercambiamos la canción elegida con la del primer índice de la urna 1, luego aumentamos el índice inferior de la urna 1
    # y aumentamos el índice superior de la urna 3, así la urna 3 ahora tiene una canción más y la urna 1 una menos.
    canciones[c,] <<- canciones[urna1[1],]
    canciones[urna1[1],] <<- vectorIntercambio
    urna1[1] <<- urna1[1] + 1
    urna3[2] <<- urna3[2] + 1
    # Se actualiza el número de canciones en cada urna
    n1 <<- n1 - 1
    n3 <<- n3 + 1
  }
  return(cancion)
}

# Caso 2, se va a sacar una canción de la urna 2. Si solo hay una canción en la urna 2 se elige esa canción.
# Si hay más de una canción, cada canción, excepto por la última que haya sido reproducida de esta urna, tiene probabilidad
# equiprobable de salir, la última canción reproducida de esta urna tiene probabilidad 0 de salir.
caso2 <- function(){
  # Verificamos si hay más de una canción en esta urna.
  if(n2 <= 1){
    # Si solo hay una canción se reproduce esa canción.
    return(canciones[urna2[2], 1])
  }
  # Nos aseguramos que no pueda salir la última canción que ha salido de la urna 2 y se elige una canción al azar de entre las demás.
  c <- sample(urna2[1]:(urna2[2]-1), 1)
  cancion <- canciones[c,1]
  # Una vez que se eligió canción, actualizamos la urna 4 donde estará la última canción de la urna 2 que se haya reproducido.
  # Si había una canción en la urna 4, la devolvemos a la urna 2.
  vectorIntercambio <- canciones[c,]
  canciones[c,] <<- canciones[numCanciones,]
  canciones[numCanciones,] <<- vectorIntercambio
  return(cancion)
}

# Función para actualizar las urnas en caso que el usuario haga una elección específica.
# Se busca en que posición del arreglo estaba esa canción y se mueve a la urna correspondiente.
eligeCancion <- function(numeroCancion){
  indiceCancion <- 0
  for(i in 1:numeroCancion){
    if(canciones[i,1] == numeroCancion){
      indiceCancion <- i
      break
    }
  }
  vectorIntercambio <- canciones[indiceCancion,]
  if(indiceCancion < urna1[1] | urna1[2] < indiceCancion){
    return()
  }
  if(canciones[indiceCancion,2] == 10){
    canciones[indiceCancion,] <<- canciones[urna1[2],]
    canciones[urna1[2],] <<- vectorIntercambio
    urna1[2] <<- urna1[2] - 1
    urna2[1] <<- urna2[1] - 1
    vectorIntercambio <- canciones[urna2[1],]
    canciones[urna2[1],] <<- canciones[urna2[2],]
    canciones[urna2[2],] <<- vectorIntercambio
    n1 <<- n1 - 1
    n2 <<- n2 + 1
  }else{
    canciones[indiceCancion,] <<- canciones[urna1[1],]
    canciones[urna1[1],] <<- vectorIntercambio
    urna1[1] <<- urna1[1] + 1
    urna3[2] <<- urna2[2] + 1
    n1 <<- n1 - 1
    n3 <<- n3 + 1
  }
}
