# f(x)=2x**3 + 8x**2 + 4x - 8

valores = c(-4,-3.6,-3.2,-2.8,-2.4,-2,-1.6,-1.2,-0.8,-0.4,0,0.4,0.8,1.2)
es = 0.7

biseccion <- function(l,u){
  ea = 100
  xl = l
  xu = u
  xrtemp = 0
  while (ea >= es) {
    
    xr = (xl+xu)/2
    
    fxl = 2*(xl^3) + 8*(xl^2) + 4*xl - 8
    fxu = 2*(xu^3) + 8*(xu^2) + 4*xu - 8
    fxr = 2*(xr^3) + 8*(xr^2) + 4*xr - 8
    
    if (fxl*fxr > 0) {
      xl = xr
    }else{
      if (fxl*fxr < 0) {
        xu = xr
      }else{
        if (fxl*fxr == 0){
          ea = 0
        }
      }
    }
    
    ea = abs((xr - xrtemp)/xr)*100
    xrtemp = xr
    
  }
  return(xr)
}


fx = fx = 2*(valores[1]^3) + 8*(valores[1]^2) + 4*valores[1] - 8

ite = 1
temp = 0

for(i in valores){
  fxi = 2*(i^3) + 8*(i^2) + 4*i - 8
  if (fxi == 0) {
    print(i)
  }
  if (fx * fxi < 0) {
    root = biseccion(valores[temp],valores[ite])
    print(paste(root))
  }
  fx = fxi
  ite = ite + 1
  temp = temp + 1
}

