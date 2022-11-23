valoresx = c(12,15,23,30.9)
valoresy = c(429,675,1595,2880.33)
orden = length(valoresx)
n = orden

diferencias_a = valoresy
diferencias_d = c()

diferenciasfinales = c(valoresy[1])

while (n != 1) { #Primeras diferencias (f[x1,x0],f[x2,x1]...)
  diferencia = (diferencias_a[n]-diferencias_a[n-1])/(valoresx[n]-valoresx[n-1])
  diferencias_d = c(diferencia,diferencias_d)
  n = n - 1
}

diferenciasfinales = c(diferenciasfinales, diferencias_d[1])

orden = orden - 1
n = orden
ite = 1

diferencias_a = diferencias_d
diferencias_d = c()

while (orden != 1) { #Siguientes diferencias (f[x2,x1,x0],f[x3,x2,x1,x0]...)
  
  while (n != 1) {
    diferencia = (diferencias_a[n]-diferencias_a[n-1])/(valoresx[n + ite]-valoresx[n - 1]) #Se calcula la diferencia
    diferencias_d = c(diferencia,diferencias_d) # Se guarda el valor en un vector
    n = n - 1 # Se avanza al siguiente elemento
  }
  # Se guardan los valores obtenidos para usarlos en el siguiente cálculo
  diferencias_a = diferencias_d
  diferencias_d = c()
  orden = orden - 1
  ite = ite + 1
  n = orden
  diferenciasfinales = c(diferenciasfinales, diferencias_a[1])
}



newton <- function(constantes, x, valoresx){
  sumatoria = 0
  orden = length(diferenciasfinales)
  for (ii in 1:length(diferenciasfinales)) {
    n = orden-1
    producto = 1
    while(n >= 1){
      producto = producto * (x-valoresx[n])
      n = n - 1
    }
    sumatoria = sumatoria + diferenciasfinales[orden]*producto
    orden = orden - 1
  }
  return(sumatoria)
}


lnlagrange <- function(vectx,index,x){
  
  
  #vectx : puntos donde se conoce la funcion
  #index : numero que indica que termino estamos calculando
  #x : punto en el que se evalua el polinomio
  
  ln = 1
  
  for (ii in 1:length(vectx)) {
    if (ii != index) {
      ln = ln * ((x-vectx[ii])/(vectx[index]-vectx[ii]))
    }
  }
  return (ln)
}

polinomio_lagrange <- function(dataX,dataY,x){
  f_aprox <- 0.         #inicializamos el polinomio
  for(i in 1:length(dataX))
  {
    #calculamos iterativamente el polinomio de Laprange
    f_aprox <- f_aprox + dataY[i]*lnlagrange(dataX, i, x)
  }
  return(f_aprox) 
}


primery = newton(diferenciasfinales,17,valoresx)
segundoy = newton(diferenciasfinales,20,valoresx)

tercery = polinomio_lagrange(valoresx,valoresy,25.5)
cuartoy = polinomio_lagrange(valoresx,valoresy,28.2)

print(paste("Primer y:",primery," Segundo y:",segundoy," Tercer y:",tercery," Cuarto y:",cuartoy))
