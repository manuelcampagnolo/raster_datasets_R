# interactividade
plot(pan, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores)  
box <- drawExtent() # clicar em 2 cantos
# definir a extensão usando box 
plot(pan, ext=box, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores)  
plot(pan, ext=box/2, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores)  

# zoom
plot(pan, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores)  
zoom(pan, new=FALSE, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores)

# click
vals<-click(pan)

# locator
pts <- locator() # devolve uma lista de coordenadas; parar com <ESC>
polygon(cbind(pts$x,pts$y),border="red") # adiciona à imagem o polígono formado pelos pontos digitalizados

# crop
pan.crop <- crop(x=pan,y=pts)
plot(pan.crop)
