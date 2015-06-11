# Exercício ndvi vs elevação na área protegida Sintra-Cascais
#limite AP Sintra-Cascais
limite.ap<-as.matrix(read.table(file="limite.AP.SintraCascais.ETRS.txt",header=FALSE))

#ler elevações SRTM mde
fich.mde<-"n38_w010_3arc_v2.tif"
mde<-raster(fich.mde)
etrs<-"+proj=tmerc +lat_0=39.6682583 +lon_0=-8.1331083 +k=1 +x_0=0 +y_0=0 +ellps=GRS80 +units=m"
mde.etrs<-projectRaster(mde,crs=etrs)
mde.etrs<-crop(mde.etrs,limite.ap)

#ler imagem ndvi
ndvi<-raster("ndviSintra.tif")
ndvi@crs
vazio<-raster(ext=mde.etrs@extent,crs=mde.etrs@crs,resolution=res(mde.etrs))
ndvi.etrs<-projectRaster(from=ndvi,to=vazio)

# construir imagens para verificar
cores<-gray(seq(0,1,length.out=100))
if (export)  png(paste(aulas,"ap_sintra_cascais_ndvi_mde_transectos.png",sep="\\"), width=800, height=500, res=120)
par(mfrow=c(1,3))
plot(mde.etrs,  xaxt="n", yaxt="n", box=FALSE, axes=FALSE, col=cores,legend=FALSE)
polygon(limite.ap,border="red")
xy<-as.vector(mde.etrs@extent) 
text(xy[c(1,2,1,1)],xy[c(3,3,3,4)],round(xy,0),pos=c(1,1,2,2),xpd=TRUE,cex=0.8)
text(x=mean(xy[1:2]),y=xy[4],"mde.etrs",col="yellow",pos=1)

plot(ndvi.etrs, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE)
polygon(limite.ap,border="black")
xy<-as.vector(ndvi.etrs@extent) 
text(xy[c(1,2,1,1)],xy[c(3,3,3,4)],round(xy,0),pos=c(1,1,2,2),xpd=TRUE,cex=0.8)
text(x=mean(xy[1:2]),y=xy[4],"ndvi.etrs",col="black",pos=1)

# extrair células num transecto dentro da AP Sintra-Cascais
xy0<-apply(limite.ap,2,mean) # centroide da AP
cp1<-eigen(cov(limite.ap))$vectors[,1] # 1a componente principal
cp2<-eigen(cov(limite.ap))$vectors[,2] # 1a componente principal
xs<-unique(coordinates(mde.etrs)[,1]) # coord x das células
amostra<-cbind(xs, xy0[2]+cp1[2]/cp1[1]*(xs-xy0[1])) # coord x e y 
amostra<-rbind(amostra, cbind(xs, xy0[2]+cp2[2]/cp2[1]*(xs-xy0[1])) )

# verificar localização do transecto:
plot(mde.etrs,  xaxt="n", yaxt="n", box=FALSE, axes=FALSE, col=cores,legend=FALSE)
polygon(limite.ap,border="red")
points(amostra,col="yellow",cex=.5)
text(x=mean(xy[1:2]),y=xy[4],"transectos",col="yellow",pos=1)

if (export) graphics.off()

# extrair valores das células sobre o transecto
y<-extract(ndvi.etrs,amostra)
x<-extract(mde.etrs,amostra)


# analisar a relação entre ndvi e elevação
if (export)  png(paste(aulas,"ap_sintra_cascais_ndvi_vs_mde_ajustamento.png",sep="\\"), width=800, height=800, res=120)
plot(y~x,xlab="elevação (m)",ylab="ndvi",main="AP Sintra-Cascais",cex.lab=1.3)
# ajustar curva
xx<-log(x[x>0]) #1/x[x>0]
yy<-log(y[x>0])
ajust<-lm(yy~xx)
curve(exp(ajust$coef[1])*x^ajust$coef[2],add=TRUE,col="red")
text(mean(range(x,na.rm=TRUE)),quantile(range(y,na.rm=TRUE),.25),paste(" y = ", round(exp(ajust$coef[1]),3), " * x^",round(ajust$coef[2],3),sep=""))
#text(0,max(y,na.rm=TRUE),paste("R^2=",round(summary(ajust)[[8]],2)),pos=4,cex=1.3)
if (export) graphics.off()
