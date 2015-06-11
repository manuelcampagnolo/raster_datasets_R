# dar mais cor à imagem:
library(RColorBrewer) # para criar paletas
display.brewer.all() # ver possíveis paletas de cores

if (export)  png(paste(aulas,"srtm_algarve_brewer_spectral.png",sep="\\"), width=800, height=600, res=120)
plot(srtm, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores)
srtm[srtm<=0]<-NA
plot(srtm, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, horizontal=TRUE, col=brewer.pal(n=12, "Spectral"),add=TRUE)

# adicionar a localização e nome de Lagos na imagem
points(y=37.095753,x=-8.683319,col="white")
text(y=37.095753,x=-8.683319,"Lagos",col="white",pos=3)
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
plot(srtm, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores)
points(x=loc.max["long"],y=loc.max["lat"],col="red",cex=2)
if (export) graphics.off()
