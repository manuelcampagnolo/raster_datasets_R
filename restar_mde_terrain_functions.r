# descarregar srtm
# dados SE Portugal 
HTTP<-"http://dds.cr.usgs.gov/srtm/version2_1/SRTM3/Eurasia/N37W008.hgt.zip" # SE
download.file(url=HTTP,destfile="N37W008.hgt.zip",mode="wb") # faz download para pasta de trabalho
unzip(zipfile="N37W008.hgt.zip") # ficheiro .hgt
srtm8<-raster("N37W008.hgt")

HTTP<-"http://dds.cr.usgs.gov/srtm/version2_1/SRTM3/Eurasia/N37W009.hgt.zip" # SW
download.file(url=HTTP,destfile="N37W009.hgt.zip",mode="wb") # faz download para pasta de trabalho
unzip(zipfile="N37W009.hgt.zip") # ficheiro .hgt
srtm9<-raster("N37W009.hgt")

# merge
srtm <- merge(srtm8,srtm9)
srtm[srtm<=0]<-NA

# imagem do mosaico
if (export)  png(paste(aulas,"srtm_algarve_tons_cinzento.png",sep="\\"), width=800, height=500, res=120)
par(mar=rep(0,4))
plot(srtm, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores)
if (export) graphics.off()


# dar mais cor à imagem:
#install.packages("RColorBrewer")
library(RColorBrewer) # para criar paletas
display.brewer.all() # ver possíveis paletas de cores
if (export)  png(paste(aulas,"colorbrewer_spectral.png",sep="\\"), width=800, height=500, res=120)
par(mar=rep(0,4))
N<-12; pal<-"Spectral";pie(rep(1,N), col=brewer.pal(n=N,pal))
if (export) graphics.off()

if (export)  png(paste(aulas,"srtm_algarve_brewer_spectral.png",sep="\\"), width=800, height=600, res=120)
par(mar=rep(0.5,4))
plot(srtm, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, horizontal=TRUE, col=brewer.pal(n=12, "Spectral"))

# adicionar a localização e nome de Lagos na imagem
points(y=37.095753,x=-8.683319,col="black",pch=16)
text(y=37.095753,x=-8.683319,"Lagos",col="black",pos=3,cex=1.2)
if (export) graphics.off()

# declive, orientação, etc, com função terrain
# pode aplicar-se a coordenadas geográficas se a elevação for em metros
inc<-terrain(srtm,opt="slope", units="radians", neighbors=8)
declives<-100*tan(values(inc)) # em %
range(declives,na.rm=TRUE)

# qual é a localização do declive máximo?
aux<-cbind(coordinates(srtm),declives)
colnames(aux)<-c("long","lat","declive")
loc.max<-aux[which(aux[,3]==max(declives,na.rm=TRUE)),]

# localizar no mapa
if (export)  png(paste(aulas,"srtm_algarve_maximo_declive.png",sep="\\"), width=800, height=600, res=120)
par(mar=rep(0,4))
plot(srtm, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores)
points(x=loc.max["long"],y=loc.max["lat"],col="red",cex=2,lwd=2)
if (export) graphics.off()

# ver Google map para esse local
if (export)  png(paste(aulas,"google_map_local_maximo_declive.png",sep="\\"), width=800, height=600, res=120)
par(mar=rep(0,4))
ponto.sp<-SpatialPoints(matrix(c(loc.max["long"],loc.max["lat"]),ncol=2))
buffer.sp<-gBuffer(ponto.sp,width=0.003) # em graus
plot(gmap(buffer.sp,type="hybrid",lonlat=TRUE,rgb=FALSE,scale=2) )
if (export) graphics.off()
