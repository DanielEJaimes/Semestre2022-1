arreglo = c(9,15,24,69,15)

orden <- function(vector){
  n <- length(vector)
  algo <- 0
  while(algo!=1){
    vector_old <- vector
    for(i in 1:(n-1)){
      if(vector[i]>vector[i+1]){
        vector[i:(i+1)] <- vector[(i+1):i]
      }
    }
    algo <- prod(vector_old==vector)
  }
  vector
}

arregloOrdenado = orden(arreglo)

# MODA

contador = 0
nmax = 0
moda = 0
aux = arregloOrdenado[1]

arregloOrdenado[1]
for (i in arregloOrdenado) {
  if (i == aux) {
    contador = contador + 1
  }else{
    contador = 1
    aux = i
  }
  if (contador > nmax) {
    nmax = contador
    moda = i
  }
}

print(paste('La moda es:',moda))

# MEDIA

media = 0

for (i in arreglo){
  media = media + i
}

media = media/length(arreglo)

print(paste('La media es:',media))

# MEDIANA

mediana = 0
mitad = length(arreglo)/2

if (mitad %% 2 != 0){
  mediana = arregloOrdenado[mitad+1]
}else {
  mediana = (arregloOrdenado[floor(mitad)] + arregloOrdenado[floor(mitad)+1])/2
}

print(paste('La mediana es:',mediana))