
install.packages

library(raster)
library(rgdal)
library(snow)
library(e1071)

setwd("C:/CARPETA QUE CONTIENE LA IMAGEN")

imgs2 <- brick("imagen_sentinel.tif")

endme <- readOGR("C:/ruta_de_la_capa/shp","rois_entrenamiento")

plorRGB(imgs2, 5, 4,3 stretch="lin")

load("Datos.RData")

modeloclasif <- svm(class~.,data=training, kernel="linear")

beginCluster()
svm_clas <- clusterR(im2, raster::predict, args = list(modeloclasif = modelclasif))
endCluster()

paleta_color <-  c("#00FF00", "0000FF", "228B22","FF1493")

plot(svm_class, main="Clasificación con SVM", col = paleta_color,cex.lab=0.6,cex.axis=0.6,cex.main=0.7)

