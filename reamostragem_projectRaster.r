#################################################################################################
#
#

tile<-"N40W008"
# descarregar dados de altimetria SRTM dessa região
URL<-paste("http://dds.cr.usgs.gov/srtm/version2_1/SRTM3/Eurasia/", tile, ".hgt.zip",sep="") # SE
download.file(url=URL,destfile=paste(tile,".hgt.zip",sep=""),mode="wb") # faz download para pasta de trabalho
unzip(zipfile=paste(tile,".hgt.zip",sep="")) # ficheiro .hgt
srtm<-raster(paste(tile,".hgt",sep=""))

# recortar na região das Penhas Douradas
srtm.crop<-crop(srtm,extent(c(-7.62,-7.54,40.32,40.4)))
plot(srtm.crop,col=topo.colors(100))

# converter MDE para coordenadas ETRS
etrs<-"+proj=tmerc +lat_0=39.6682583 +lon_0=-8.1331083 +k=1 +x_0=0 +y_0=0 +ellps=GRS80 +units=m"
mde<-projectRaster(srtm.crop,crs=etrs,method="bilinear")
# observar o MDE apenas sobre a zona central do concelho de Manteigas
plot(mde,col=topo.colors(100))

# re-amostragem bi-linear para uma resolução mais fina 
mde20<-projectRaster(srtm.crop,res=20,crs=etrs,method="bilinear")
# nota: as extensões são diferentes:
mde@extent; mde20@extent
# para ficar exactamente com a mesma extensão
vazio<-raster(ext=mde@extent,crs=mde@crs,resolution=20)
mde20<-projectRaster(from=srtm.crop,to=vazio,method="bilinear")

export<-TRUE
locs<-as.vector(mde@extent)
par(mfrow=c(1,1),mar=rep(1,4))
if (export)  png(paste(aulas,"mde_serra_estrela_original.png",sep="\\"), width=900, height=900, res=120)
plot(mde,col=topo.colors(100),box=FALSE,axes=FALSE,legend=FALSE)
contour(mde,add=TRUE)
text(x=mean(locs[1:2]),y=quantile(locs[3:4],.9),paste("resolução=",paste(round(res(mde)),collapse=",")),cex=0.7)
if (export) graphics.off()
if (export)  png(paste(aulas,"mde_serra_estrela_20m.png",sep="\\"), width=900, height=900, res=120)
plot(mde20,col=topo.colors(100),box=FALSE,axes=FALSE,legend=FALSE)
contour(mde,add=TRUE)
text(x=mean(locs[1:2]),y=quantile(locs[3:4],.9),paste("resolução=",paste(round(res(mde20)),collapse=",")),cex=0.7)
if (export) graphics.off()

#library(maptools)
persp(mde,  ext=mde@extent,exp=0.2,phi=55, xlab="x", ylab="y", zlab="Elevation", shade=0.25, col="green")
