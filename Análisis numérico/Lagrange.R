valoresx = c(0,1,2)
valoresy = c(1,3,0)

ln_lagrange <- function(vectx,index,x){
  
  #vectx (vector-double): puntos donde se conoce la funcion
  #index (int): numero que indica que termino estamos calculando
  #x (double): punto en el que se evalua el polinomio

  ln = 1
  
  for (ii in 1:length(vectx) {
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
    f_aprox <- f_aprox + dataY[i]*oneLagrange_pol(dataX, i, x)
  }
  return(f_aprox) 
}