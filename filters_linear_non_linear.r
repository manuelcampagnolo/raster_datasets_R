# filtro de moda (lento)
srtm.crop<-crop(srtm,extent(c(-8.1,-8,37.4,37.5)))
statmod <- function(x,...) {z <- table(as.vector(x[-(length(x)+1)/2])); names(z)[z == max(z)]}
srtm.moda <- focal(srtm.crop,w=matrix(1,nrow=3,ncol=3),fun=statmod)
par(mfrow=c(2,1))
plot(srtm.crop, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores)
plot(srtm.moda, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores)

# com filtros lineares (rápido)
# Latitude: 37.318, Longitude: -8.55602 (Monchique)
if (export)  png(paste(aulas,"srtm_monchique_declives.png",sep="\\"), width=800, height=700, res=120)
srtm.crop<-crop(srtm,extent(c(-8.7,-8.4,37.1,37.4)))
srtm.etrs<- projectRaster(srtm.crop,crs=etrs) # para obter coordenadas em metros
Sx <- focal(srtm.etrs,w=matrix(c(-1,-2,-1,0,0,0,1,2,1)/(8*res(srtm.etrs)[1]),ncol=3))
Sy <- focal(srtm.etrs,w=matrix(c(1,0,-1,2,0,-2,1,0,-1)/(8*res(srtm.etrs)[2]),ncol=3))
declive <- sqrt(Sx^2+Sy^2)
plot(declive, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores)
if (export) graphics.off()

