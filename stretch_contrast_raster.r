# extrair valores das células para o vector v
v<-values(pan)

if (export)  png(paste(aulas,"val.vs.intensidade.png",sep="\\"), width=800, height=800, res=120)
out<-hist(v,main="valores vs intensidade",ylab="frequências absolutas")
fqs<-out$counts
lines(x=range(v),y=range(fqs),col="blue")
axis(4,at=seq(min(fqs),max(fqs),length.out=5),labels=seq(0,1,length.out=5))
mtext("intensidade", side=4,line=-1.5,col="blue")
if (export) graphics.off()

# restringir valores: saturar
if (export)  png(paste(aulas,"imagem_pan_stretch_legenda_horizontal.png",sep="\\"), width=800, height=800, res=120)
plot(pan,  zlim=c(0,25000), xaxt="n", yaxt="n", box=FALSE, axes=FALSE, col=cores,horizontal=TRUE) 
if (export) graphics.off()
plot(pan,  zlim=c(0,quantile(v,.999)), xaxt="n", yaxt="n", box=FALSE, axes=FALSE, col=cores,horizontal=TRUE) 

# restringir x e y
plot(pan,  xlim=c(580000,585000), xaxt="n", yaxt="n", box=FALSE, axes=FALSE, col=cores,zlim=c(0,25000)) 
plot(pan,  xlim=c(570000,575000), ylim=c(4315000,4325000),zlim=c(0,25000),xaxt="n", yaxt="n", box=FALSE, axes=FALSE, col=cores,legend=FALSE) 
image(pan,  xlim=c(570000,575000), ylim=c(4315000,4325000),zlim=c(0,25000))
head(coordinates(pan))
range(coordinates(pan)[,"y"])
