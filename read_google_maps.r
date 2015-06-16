# dados alta resolução google maps para a mesma região
#install.packages("dismo")
library(dismo)
library(proj4)
library(sp)
# se rgb=FALSE devolve um RasterLayer, 
# se rgb=TRUE devolve um RasterBrick, com 3 layers
# scale pode ser 1 ou 2. scale=2 devolve um maior número de pixels (duplica resolução)
gm.wgs<-gmap(zona.central,type="satellite",lonlat=TRUE,rgb=TRUE,scale=2) 

# re-projectar para ETRS-PT-TM06
etrs<-"+proj=tmerc +lat_0=39.6682583 +lon_0=-8.1331083 +k=1 +x_0=0 +y_0=0 +ellps=GRS80 +units=m"
gm.etrs<-projectRaster(gm.wgs,crs=etrs)
landsat.etrs<-projectRaster(landsat,crs=etrs)

# sobrepor as duas imagens usando ptransparência 
if (export)  png(paste(aulas,"ribatejo_rgb_432_sobre_google.png",sep="\\"), width=800, height=600, res=120)
par(mfrow=c(1,1))
plotRGB(gm.etrs,r=3,g=2,b=1,stretch="lin") 
# composição colorida Landsat RGB=432
plotRGB(landsat.etrs,r=4,g=3,b=2,stretch="lin",ext=gm.etrs@extent,alpha=100,add=TRUE) #RGB
if (export) graphics.off()

# nova figura
plotRGB(gm.etrs,r=3,g=2,b=1,stretch="lin") #RGB
# composição colorida Landsat RGB=742
plotRGB(landsat.etrs,r=6,g=4,b=2,stretch="lin",ext=gm.etrs@extent,alpha=100,add=TRUE) #RGB
