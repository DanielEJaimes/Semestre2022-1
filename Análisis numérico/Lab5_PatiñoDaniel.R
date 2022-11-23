x = -5
xi = 10000
es = 0.5
ea = 100
cont = 0

while (ea >= es && cont < 40) {
  xii = xi - (((sin(xi) + cos(1+(xi**2))-1)*(x-xi))/
                ((sin(x) + cos(1+(x**2))-1)-(sin(xi) + cos(1+(xi**2))-1)))
  ea = abs(((xii-xi)/xii))*100
  x = xi
  xi = xii
  cont = cont + 1
}

