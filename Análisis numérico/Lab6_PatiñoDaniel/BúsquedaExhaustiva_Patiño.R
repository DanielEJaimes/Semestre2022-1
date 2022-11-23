valoresx = c(-3,-2,0,5,7,9,17,20)
valoresy = c(11,10,-11,50,11,15,-23,50)
n = 1

coeficiente = 0

rr = 0
mejor = 0
nmejor = n
n1 = n
busqueda = 0

while (n <= 3 && rr != 1) {
  
  
  m = matrix(0, nrow = n + 1, ncol = n + 2)#Genera la matriz para orden n
  ii = 1
  
  while (ii <= n + 1) {
    #Genera los valores de x^q en la matriz
    
    jj = 1
    
    while (jj < n + 2) {
      sumatoria = 0
      potencia = (n * 2 - ii + 1) - jj + 1 #Hace la iteracion de q cambiando según el orden
      
      for (kk in valoresx) {
        #Hace la sumatora de los valores x^q
        x = kk ^ potencia
        sumatoria = sumatoria + x
      }
      
      m[ii, jj] = sumatoria
      jj = jj + 1
      
    }
    ii = ii + 1
  }
  
  ll = 1
  
  while (ll < n+2) {
    potencia = (n - ll + 1) #Genera las potencias del orden para abajo
    sumatoria = 0
    ite = 1
    while(ite <= length(valoresx)){
      x = (valoresx[ite]^potencia)*valoresy[ite] #Hace la sumatoria de x^potencia * y
      sumatoria = sumatoria + x
      ite = ite + 1
    }
    m[ll, n+2] = sumatoria
    ll = ll + 1 
  }
  print(m)
  
  gaussjordan = function(m){
    
    p = nrow(m)
    
    m[1,] = m[1,]/m[1,1]
    
    for (i in 2:p){
      for (j in i:p) {
        m[j, ] = m[j, ] - m[i-1, ] * m[j, i-1]
      }
      m[i,] = m[i,]/m[i,i]
    }
    
    for (i in p:2){
      for (j in i:2-1) {
        m[j, ] = m[j, ] - m[i, ] * m[j, i]
      }
    }
    m
  }
  
  m = gaussjordan(m)
  
  # Se calcula el valor de 'a'
  
  yprom = sum(valoresy)/length(valoresy)
  
  ra = 0
  for (aa in valoresy) {
    ra = ra + (aa-yprom)^2 
  }
  
  rb = 0
  
  valoresye = c()
  
  # Se genera la matriz 'valoresye' con los valores del y estimado
  
  for (bb in valoresx) {
    ye = 0 
    ite = 1
    orden = n
    while (ite < ncol(m)) {
      ye = m[ite,ncol(m)] * bb^orden + ye
      ite = ite + 1
      orden = orden - 1
    }
    valoresye = c(valoresye,ye)
  }
  
  # Se calcula el valor de 'b'
  
  ite = 1
  
  while (ite < length(valoresy)) {
    rb = rb + (valoresy[ite] - valoresye[ite])^2
    ite = ite + 1
  }
  
  # Se calcula el coeficiente de determinación (r^2)
  
  rr = (ra-rb)/ra
  
  if (rr >= 0.83 && busqueda == 0) {
    busqueda = 1
    coeficiente = rr
    n1 = n
  }
  
  if (rr > mejor) {
    mejor = rr
    nmejor = n
  }
  
  n = n + 1
  
}

print(paste("La regresión de grado ",n1," genera un r^2 de:",coeficiente))
print(paste("La mejor regresión se dió en n=",nmejor))
