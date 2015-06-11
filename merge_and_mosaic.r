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

# imagem do mosaico
if (export)  png(paste(aulas,"srtm_algarve_tons_cinzento.png",sep="\\"), width=800, height=500, res=120)
plot(srtm, xaxt="n", yaxt="n", box=FALSE, axes=FALSE, legend=FALSE, col=cores)
if (export) graphics.off()
