########################################################################################
#
# dados matriciais: composição de bandas, download de dados, 
# modelos digitais de elevação, filtros para imagens
#
#########################################################################################


# dados landsat ribatejo 
fichs <- list.files(pattern="banda")
s <- stack(as.list(fichs))
s # devolve sumário dos dados
b <- brick(s)
b # idem
nlayers(b) # devolve número de camadas em b
box <-extent(c(500000,  520000, 4310000, 4330000))
if (export)  png(paste(aulas,"clip_grande_ribatejo_rgb_432.png",sep="\\"), width=800, height=800, res=120)
plotRGB(b,r=4,g=3,b=2,stretch="lin",ext=box) #RGB
if (export) graphics.off()

#e<-drawExtent()
e<-extent(as.vector(c( 513316, 514509, 4317568 ,4318417)))
landsat<-crop(b,e)

# coord lat/long do centro da imagem ribatejo
wgs84<-"+proj=longlat +ellps=WGS84 +datum=WGS84"
aux<-projectRaster(landsat, crs=wgs84)
# o slot @extent devolve um objecto de classe extent 
aux@extent # coordenadas lat/long de b
zona.central<-aux@extent/2 # coordenadas lat/long da zona central de b
