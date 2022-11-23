fx = sin((4*2.5)+6)
fxi = fx

es = 0.03
ea = 1

corchete = ((4*2.5)+6)
diferencia = 2.9-2.5

cont = 1

signo = 1

derivada <- function(orden, u, h){
  if (orden %% 2 == 0) {
    fxd =sin(u)
  }
  else{
    fxd = cos(u)
  }
  fxd = fxd * 4 ** orden
  fxd = (fxd / factorial(orden))*(h**orden)
}

while (ea >= es) {
  if (cont %% 2 == 0) {
    signo = signo * -1
  }
  der = derivada(cont,corchete, diferencia) * signo
  fxi = fxi + der
  ea = abs(((fxi-fx)/fxi))
  cont = cont + 1
  fx = fxi
}

print(paste('La cantidad de iteraciones fue:',cont-1))