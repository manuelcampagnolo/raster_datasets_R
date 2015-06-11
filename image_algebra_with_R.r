# algebra de imagens
ndvi <- (b$banda4 - b$banda3)/(b$banda4 + b$banda3)

ncell(ndvi[is.na(ndvi)])/ncell(ndvi) #ou length 
ndvi[ndvi<0] <- 0 # substituição de valor
hist(ndvi)

#reclassificar
tabela <- cbind(c(-1,0,.5),c(0,.5,1),1:3)
ndvi.c <- reclassify(ndvi,rcl=tabela,right=TRUE)

# funções que se aplicam pixel a pixel de um astack
min(b) # devolve o mínimo nas 6 bandas
range(b) # devolve RasterBrick com min e max 

# funções que se aplicam a cada banda
cellStats(b,"mean") # devolve um vector 

# operações sobre vários objectos RasterLayer
median(b) #dá erro
b.mediana<-overlay(b,fun=median,rm.na=TRUE) # lento, mas funciona
