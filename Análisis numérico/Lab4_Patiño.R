#Raices = -0.9212, 0.1923, 1.1289
# El es lo da el usuario
# El xi lo da el usuario
# Máximo 20 iteraciones, mostrar mensaje de que no se pudo hallar la raíz
# ea = abs(((fxi-fx)/fxi))*100

cont = 0

print("Ingrese el Xi")
x = scan()

print("Ingrese el es")
es = scan()

ea = 100

while (ea >= es && cont < 20) {
  fx = 5*(x**3) - 2*(x**2) - 5*(x) + 1
  fxd = 15*(x**2) - 4*(x) - 5
  
  xi = x - (fx/fxd)
  
  ea = abs(((xi-x)/xi))*100
  
  x = xi
  
  cont = cont + 1
}

if (cont < 20) {
  print(paste0("La raíz es de " , x ))
}else{
  print("No se pudo hallar la raíz, límite de iteraciones alcanzado") 
}
