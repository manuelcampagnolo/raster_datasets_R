#############################################################################################
#
# Sessão 4: 5 de Fevereiro de 2015
#
# dados matriciais: leitura de dados (GeoTIFF), manipulação de conjuntos de dados
# geográficos matriciais em R, package "raster", sistema de coordenadas, re-projecção
#
#########################################################################################

library(raster)
library(rgdal)
library(sp)
library(rgeos)
#library("gdalUtils") # função remove_file_extension

library(grDevices)
export<-FALSE

# pasta de trabalho
wd<-"Y:\\Aulas\\sigs_com_R\\dados_aulas"
aulas<-"Y:\\Aulas\\sigs_com_R"
setwd(wd)

# LER COM RASTER
# ler geotiff com raster: devolve raster object
# raster atribui CRS ao objecto
fich<-"landsat8pan.tif" # 4.5 Mb
pan<-raster(fich) # cria um objecto RasterLayer com slots @file, @data,... @crs,...
str(pan)
print(pan@crs)
cores<-gray(seq(0,1,length.out=100))
plot(pan,col=cores)
if (export)  png(paste(aulas,"imagem_pan_tons_cinzento_legenda_horizontal.png",sep="\\"), width=800, height=800, res=120)
plot(pan,  xaxt="n", yaxt="n", box=FALSE, axes=FALSE, col=cores,horizontal=TRUE)
if (export) graphics.off()

# re-projectar
igeoe<- "+proj=tmerc +lat_0=39.66666666666666  +towgs84=-304.046,-60.576,103.64,0,0,0,0 +lon_0=1 +k=1 +x_0=200000 +y_0=300000 +ellps=intl  +pm=lisbon +units=m" # Coordenadas ``militares''
m <- projectRaster(pan, crs=igeoe) #demora...

# comparar coordenadas e projecção
if (export)  png(paste(aulas,"imagem_pan_utm_vs_igeoe.png",sep="\\"), width=800, height=500, res=120)
par(mfrow=c(1,2))
# dados originais (coordenadas UTM zona 29) 
plot(pan, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores,zlim=c(0,25000))
xy<-as.vector(pan@extent) 
# xpd para poder escrever fora da imagem
text(xy[c(1,2,1,1)],xy[c(3,3,3,4)],round(xy),pos=c(1,1,2,2),xpd=TRUE)
text(x=mean(xy[1:2]),y=xy[4],"UTM zona 29",col="yellow",pos=1)

# dados transformados (coordenadas militares) 
plot(m, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores,zlim=c(0,25000))
xy<-as.vector(m@extent) 
text(xy[c(1,2,1,1)],xy[c(3,3,3,4)],round(xy),pos=c(1,1,2,2),xpd=TRUE)
text(x=mean(xy[1:2]),y=xy[4],"Coord. Militares",col="yellow",pos=1)
if (export) graphics.off()

# nadgrid
#Colocar ptLX_e89.gsb na pasta dada por system.file("proj", package = "rgdal")
igeoe.grid <- CRS("+proj=tmerc +lat_0=39.66666666666666  +nadgrids=ptLX_e89.gsb +lon_0=1 +k=1 +x_0=200000 +y_0=300000 +ellps=intl  +pm=lisbon +units=m")
m.grid <- projectRaster(pan, crs=igeoe.grid)
# dados transformados (coordenadas militares) 3 coefs. 
if (export)  png(paste(aulas,"imagem_pan_bursa_wolf_vs_nadgrid.png",sep="\\"), width=800, height=500, res=120)
par(mfrow=c(1,2))
plot(m, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores,zlim=c(0,25000))
xy<-as.vector(m@extent) 
text(xy[c(1,2,1,1)],xy[c(3,3,3,4)],round(xy),pos=c(1,1,2,2),xpd=TRUE)
text(x=mean(xy[1:2]),y=xy[4],"3 parâmetros",col="yellow",pos=1)

# dados transformados (coordenadas militares) nadgrid
plot(m.grid, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores,zlim=c(0,25000))
xy<-as.vector(m.grid@extent)
text(xy[c(1,2,1,1)],xy[c(3,3,3,4)],round(xy),pos=c(1,1,2,2),xpd=TRUE)
text(x=mean(xy[1:2]),y=xy[4],"método das grelhas",col="yellow",pos=1)
if (export) graphics.off()

# exercicio (a) determinar a gama lat/long correspondente a pan
wgs84<-"+proj=longlat +ellps=WGS84 +datum=WGS84"
w <- projectRaster(pan, crs=wgs84)
plot(w, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores,zlim=c(0,25000))
xy<-as.vector(w@extent) 
text(xy[c(1,2,1,1)],xy[c(3,3,3,4)],round(xy,4),pos=c(1,1,2,2),xpd=TRUE)

# exercicio (b) determinar a localização (lat/long) do parede da barragem de Montargil
zoom(w, new=FALSE, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores)
locator()

