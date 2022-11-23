valoresx = c(1,4,6,5)
valoresy = c(0,1.386294,1.791759,1.6094381)
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
sumatoria = 0
orden = length(diferenciasfinales)
