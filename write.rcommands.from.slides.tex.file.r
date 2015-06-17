
aulas<-"Y:\\Aulas\\CURSOS_R\\sigs_com_R"    
gofile<-function(x) paste(aulas,x,sep="\\")

x<-scan(gofile("slides_SIGs_com_R_junho_2015_raster.tex"),what=character(),sep="\n",quote=NULL)

strings<-c()
i<-1
copiar<-FALSE
while (i <= length(x))
{
  if (grepl(pattern="end.*document",x=x[[i]])) {i<-10000000}
  if (grepl(pattern="begin.*lstlisting",x=x[[i]])) {i<-i+1; copiar<-TRUE}
  if (grepl(pattern="end.*lstlisting",x=x[[i]])) {i<-i+1; copiar<-FALSE; strings<-c(strings," ") }
  if (copiar) strings<-c(strings,x[[i]])
  i<-i+1
}
      
write.table(strings,file=gofile("Script_R_sigs_raster.txt"),quote=FALSE,row.names=FALSE,col.names=FALSE)
