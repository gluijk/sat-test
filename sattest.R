# Animation of a SAT test exercise with R
# www.overfitting.net
# https://www.overfitting.net/2022/11/test-geometrico-y-su-simulacion-con-r.html


# LIBRERÍA GRÁFICA BITMAP

NewBitmap = function(dimx, dimy, val=0) {
  # Crea bitmap de dimensiones dimx y dimy
  return(array(val,c(dimx,dimy)))
}

# Por Carlos Gil Bellosta
indices.drawline = function(x0, y0, x1, y1) {
    x0=round(x0)
    x1=round(x1)
    y0=round(y0)
    y1=round(y1)
    
    if (y0 == y1) return(cbind(x0:x1, y0)) # Recta de m=0 o un punto
    if (abs(x1 - x0) >= abs(y1 - y0)) { # Recta de 0 < |m| <= 1
        m = (y1 - y0) / (x1 - x0)
        cbind(x0:x1, round(y0 + m * ((x0:x1) - x0)))
    } else indices.drawline(y0, x0, y1, x1)[, 2:1]  # Recta de |m| > 1
    # Llamada traspuesta recursiva y traspuesta
}

DrawLine = function(img, x0, y0, x1, y1, inc=T, val=1) {
  # Dibuja recta desde (x0,y0)-(x1,y1)
  # Por defecto método no destructivo y con valor=1
  indices=indices.drawline(x0, y0, x1, y1)
  if (inc) img[indices]=img[indices]+val
  else img[indices]=val
  
  return(img)
}

DrawPoint = function(img, x0, y0, inc=T, val=1) {
    # Dibuja punto en (x0,y0)
    # Por defecto método no destructivo y con valor=1
    img=DrawLine(img, x0, y0, x0, y0, inc, val)
    
    return(img)
}

DrawEllip = function(img, x0, y0, a, b, inc=T, val=1, fill=F, thick=1) {
    # Dibuja elipse de centro (x0,y0) y radios a y b
    # Por defecto método no destructivo, con valor=1 y sin relleno
    # Puede elegirse el grosor si no se rellena
    # Aquí no redondeamos para tener más precisión en la división
    if (fill) {
        indices=which( ((row(img)-x0)/a)^2 + ((col(img)-y0)/b)^2 < 1 )
    } else {
        indices=which( ((row(img)-x0)/(a+thick/2))^2 + ((col(img)-y0)/(b+thick/2))^2 <  1 &
                           ((row(img)-x0)/(a-thick/2))^2 + ((col(img)-y0)/(b-thick/2))^2 >= 1 )
    }
    if (inc) img[indices]=img[indices]+val
    else img[indices]=val
    
    return(img)
}

DrawCircle = function(img, x0, y0, r, inc=T, val=1, fill=F, thick=1) {
  # Dibuja círculo de centro (x0,y0) y radio r
  # Por defecto método no destructivo, con valor=1 y sin relleno
  # Puede elegirse el grosor si no se rellena
  img=DrawEllip(img, x0, y0, r, r, inc, val, fill, thick)
  
  return(img)
}

SaveBitmap = function(img, name, trunc=T, gamma=1) {
  # Guarda bitmap en formato PNG
  # Solo si trunc=F y la imagen excede de 1 se reescala a 1
  library(png)
  img[img<0]=0
  if (trunc) img[img>1]=1
  if (tolower(substr(name, nchar(name)-3, nchar(name))) != ".png") name=paste0(name,".png")
  writePNG(t(img[,ncol(img):1] / max(max(img),1))^(1/gamma), name)
}



# SAT TEST ANIMATION

ANCHO=800
ALTO=ANCHO
MARGEN=round(min(ANCHO,ALTO)/20)
CENTROX=round(ANCHO*0.5)
CENTROY=round(ALTO*0.5)

RATIO=3  # size ratio between circles A and B
RA=round((min(ANCHO,ALTO)-2*MARGEN) / (2*(2+RATIO)))
RB=RA*RATIO

frm=NewBitmap(ANCHO, ALTO)
frm=DrawCircle(frm, CENTROX, CENTROY, RB, val=0.3, fill=TRUE)  # A

N=400
for (t in 0:(N-1)) {
    alpha=2*pi*t/N
    x0=CENTROX+(RA+RB)*sin(alpha)
    y0=CENTROY+(RA+RB)*cos(alpha)
    frm=DrawCircle(frm, round(x0), round(y0), RA, val=0.3, fill=TRUE)  # B
    
    # Diameter
    x1=round(x0-RA*sin(alpha*(RATIO+1)))
    y1=round(y0-RA*cos(alpha*(RATIO+1)))
    x2=round(x0+RA*sin(alpha*(RATIO+1)))
    y2=round(y0+RA*cos(alpha*(RATIO+1)))
    frm=DrawLine(frm, x1, y1, x2, y2, val=0.5)
    frm=DrawPoint(frm, x2, y2, val=1)

    SaveBitmap(frm, paste0("frm", ifelse(t<10, "00", ifelse(t<100, "0", "")), t))
    # Delete
    frm=DrawCircle(frm, round(x0), round(y0), RA, val=-0.3, fill=TRUE)
    frm=DrawLine(frm, x1, y1, x2, y2, val=-0.5)
}
