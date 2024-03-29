
#################################################################################################################################
#BLOQUE 0: INSTALACI�N DE LIBRERIAS
#################################################################################################################################

install.packages("sp")
install.packages("raster")
install.packages("rgdal")
install.packages("gdalUtils")
install.packages("caret")
install.packages("snow")
install.packages("e1071")

library(sp)
library(raster)
library(rgdal)
library(gdalUtils)
library(caret)
library(snow)
library(e1071)


##################################################################################################################################
#BLOQUE 1: DEFINIR DIRECTORIO DE TRABAJO
##################################################################################################################################

setwd("D:/S2_img") #ASIGNAR---Directorio de trabajo

getwd()


##################################################################################################################################
#BLOQUE 2: CONVERSI�N IMAGENES .JP2 A .TIF (GEOTIFF)
##################################################################################################################################

gdal_translate("bandaAOT.jp2", "bandaAOT_uncut.tif")   #ASIGNAR--- AOT Nombre archivo de entrada y salida

gdal_translate("banda02.jp2", "banda02_uncut.tif")   #ASIGNAR--- B02 Nombre archivo de entrada y salida

gdal_translate("banda03.jp2", "banda03_uncut.tif")   #ASIGNAR--- B03 Nombre archivo de entrada y salida

gdal_translate("banda04.jp2", "banda04_uncut.tif")   #ASIGNAR--- B04 Nombre archivo de entrada y salida

gdal_translate("banda08.jp2", "banda08_uncut.tif")   #ASIGNAR--- B08 Nombre archivo de entrada y salida

gdal_translate("bandaTCI.jp2", "bandaTCI_uncut.tif")   #ASIGNAR--- TCI Nombre archivo de entrada y salida

gdal_translate("bandaWVP.jp2", "bandaWVP_uncut.tif")   #ASIGNAR--- WVP Nombre archivo de entrada y salida


#bandas con resoluci�n 20m en otro directorio

gdal_translate("D:/S2_img/bandas_20m/banda11.jp2", "banda11_20m_uncut.tif") #ASIGNAR--- Ruta de archivo B11

gdal_translate("D:/S2_img/bandas_20m/banda12.jp2", "banda12_20m_uncut.tif") #ASIGNAR--- Ruta de archivo B12



####################################################################################################################################
#BLOQUE 3: CORRECCI�N RESOLUCI�N BANDA 11 Y 12 A 10m
####################################################################################################################################

band11_20m <- raster("banda11_20m_uncut.tif")
band12_20m <- raster("banda12_20m_uncut.tif")

band11_10m_uncut <- disaggregate(band11_20m, fac=2)
band12_10m_uncut <- disaggregate(band12_20m, fac=2)

band11_10m_uncut

writeRaster(band11_10m_uncut,"banda11_uncut.tiff", drivername="Gtiff") #exporta la BANDA 11 en .tif

band12_10m_uncut

writeRaster(band12_10m_uncut,"banda12_uncut.tiff", drivername="Gtiff") #exporta la BANDA 12 en .tif



####################################################################################################################################
#BLOQUE 4: DEFINICI�N DEL �REA DE ESTUDIO
####################################################################################################################################

zona_estudio <- readOGR("D:/s2_img/zona_estudio/zona_estudio.shp") #ASIGNAR--- Ruta de la capa de recorte


bandaAOT_uncut <- raster("bandaAOT_uncut.tif")
bandaAOT <- crop(bandaAOT_uncut, zona_estudio)
writeRaster(bandaAOT,"bandaAOT.tiff", drivername="Gtiff")

banda02_uncut <- raster("banda02_uncut.tif")
banda02 <- crop(banda02_uncut, zona_estudio)
writeRaster(banda02, "banda02.tif", drivername="Gtiff")

banda03_uncut <- raster("banda03_uncut.tif")
banda03 <- crop(banda03_uncut, zona_estudio)
writeRaster(banda03, "banda03.tif", drivername="Gtiff")

banda04_uncut <- raster("banda04_uncut.tif")
banda04 <- crop(banda04_uncut, zona_estudio)
writeRaster(banda04, "banda04.tif", drivername="Gtiff")

banda08_uncut <- raster("banda08_uncut.tif")
banda08 <- crop(banda08_uncut, zona_estudio)
writeRaster(banda08, "banda08.tif", drivername="Gtiff")

bandaTCI_uncut <- raster("bandaTCI_uncut.tif")
bandaTCI <- crop(bandaTCI_uncut, zona_estudio)
writeRaster(bandaTCI, "bandaTCI.tif", drivername="Gtiff")

bandaWVP_uncut <- raster("bandaWVP_uncut.tif")
bandaWVP <- crop(bandaWVP_uncut, zona_estudio)
writeRaster(bandaWVP, "bandaWVP.tif", drivername="Gtiff")

banda11_uncut <- raster("banda11_uncut.tif")
banda11 <- crop(banda11_uncut, zona_estudio)
writeRaster(banda11, "banda11.tif", drivername="Gtiff")

banda12_uncut <- raster("banda12_uncut.tif")
banda12 <- crop(banda12_uncut, zona_estudio)
writeRaster(banda12, "banda12.tif", drivername="Gtiff")

####################################################################################################################################
#BLOQUE 5: COMPOSICI�N MULTIESPECTRAL
####################################################################################################################################

#Orden bandas:
#Layer 1 -- banda02
#Layer 2 -- banda03
#Layer 3 -- banda04
#Layer 4 -- banda08
#Layer 5 -- banda11
#Layer 6 -- banda12
#Layer 7 -- bandaAOT
#Layer 8 -- bandaTCI
#Layer 9 -- bandaWVP

lay1 <- ("banda02.tif")
lay2 <- ("banda03.tif")
lay3 <- ("banda04.tif")
lay4 <- ("banda08.tif")
lay5 <- ("banda11.tif")
lay6 <- ("banda12.tif")

#composici�n tipo stack

comp_mult_ST <- stack(lay1, lay2, lay3, lay4, lay5, lay6)

#composici�n tipo brick

comp_mult_BR <- brick(comp_mult_ST)

writeRaster(comp_mult_BR, "comp_multiespectral.tif", drivername="Gtiff") #exporta la imagen multiespectral en .tif


#####################################################################################################################################
#BLOQUE 6: C�LCULO DE �NDICES DE VEGETACI�N, HUMEDAD Y OTROS
#####################################################################################################################################

L4 <- raster(comp_mult_BR, layer=4) #NIR=Banda 8 Sentinel-2
L3 <- raster(comp_mult_BR, layer=3) #RED=Banda 4 Sentinel-2
L2 <- raster(comp_mult_BR, layer=2) #GREEN=Banda 3 Sentinel-2
L1 <- raster(comp_mult_BR, layer=1) #BLUE=Banda 2 Sentinel-2
L5 <- raster(comp_mult_BR, layer=5) #SWIR11=Banda 11 Sentinel-2
L6 <- raster(comp_mult_BR, layer=6) #SWIR12=Banda 12 Sentinel-2

#NDVI
ndvi =  (L4-L3)/(L4+L3)
writeRaster(ndvi,"ndvi.tiff", drivername="Gtiff") #exporta la capa NDVI en .tif

#GNDVI
gndvi = (L4-L2)/(L4+L2)
writeRaster(gndvi,"gndvi.tiff", drivername="Gtiff") #exporta la capa GNDVI en .tif

#RVI
rvi = L4/L3
writeRaster(rvi,"rvi.tiff", drivername="Gtiff") #exporta la capa RVI en .tif

#GVI
gvi = L4/L2
writeRaster(gvi,"gvi.tiff", drivername="Gtiff") #exporta la capa GVI en .tif

#NGRDI
ngrdi = (L2-L3)/(L2+L3)
writeRaster(ngrdi,"ngrdi.tiff", drivername="Gtiff") #exporta la capa NGRDI en .tif

#RG
rg = L3/L2
writeRaster(rg,"rg.tiff", drivername="Gtiff") #exporta la capa RG en .tif

#NRVI
nrvi = rvi-1/rvi+1 ## ## ## DUDA EN LA ECUACI�N ## ## ##
writeRaster(nrvi,"nrvi.tiff", drivername="Gtiff") #exporta la capa NRVI en .tif

#NDWI11
ndwi11 = (L4-L5)/(L4+L5)
writeRaster(ndwi11,"ndwi11.tiff", drivername="Gtiff") #exporta la capa NDWI_11 en .tif

#NDWI12
ndwi12 = (L4-L6)/(L4+L6)
writeRaster(ndwi12,"ndwi12.tiff", drivername="Gtiff") #exporta la capa NDWI_12 en .tif

#TVI
tvi = sqrt(abs((L4-L3)/(L4+L3))) #�����REVISAR FORMULA!!!!!
writeRaster(tvi,"tvi.tiff", drivername="Gtiff") #exporta la capa TVI en .tif

#TTVI
ttvi = sqrt(abs(ndvi+0.5))
writeRaster(ttvi,"ttvi.tiff", drivername="Gtiff") #exporta la capa TTVI en .tif


##############################################################
#ESPACIO RESERVADO PARA EL C�LCULO DEL CANOPY HEIGH MODEL
##############################################################

#mdt <- ("ruta_mdt.tif")

#mds <- ("ruta_mds.tif")

#chm = mds-mdt

#writeRaster(chm,"chm.tiff", drivername="Gtiff") #exporta la capa CHM en .tif

#NOTA: El CHM debe de tener el mismo tama�o de pixel que el resto de bandas (10x10)
#En caso de no tener la misma, y que la del CHM sea menor (<10x10), es preferible reducir la resoluci�n del resto de bandas
#que aumentar la resoluci�n de CHM, ya que de esta manera perder�amos la resoluci�n del CHM



#####################################################################################################################################
#BLOQUE 7: COMPOSICI�N MULTIBANDA DE VARIABLES EXPLICATIVAS
#####################################################################################################################################

getwd()

b1 <- ("banda02.tif")
b2 <- ("banda03.tif")
b3 <- ("banda04.tif")
b4 <- ("banda08.tif")

b5 <- ("banda11.tif")
b6 <- ("banda12.tif")

b7 <- ("bandaAOT.tif")
b8 <- ("bandaTCI.tif")
b9 <- ("bandaWVP.tif")

b10 <- ("ndvi.tif")
b11 <- ("gndvi.tif")
b12 <- ("rvi.tif")
b13 <- ("gvi.tif")
b14 <- ("ngrdi.tif")
b15 <- ("rg.tif")
b16 <- ("nrvi.tif")
b17 <- ("ndwi11.tif")
b18 <- ("ndwi12.tif")
b19 <- ("tvi.tif")
b20 <- ("ttvi.tif")

#b21 <- ("chm.tif")

stack_variables <- stack(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b12, b13, b14, b15, b16, b17, b18, b19, b20) #b21

brick_variables <- brick(stack_variables)

writeRaster(brick_variables,"comp_variables.tiff", drivername="Gtiff") #Exporta un archivo multicapa con todas las variables

brick_variables


#####################################################################################################################################
#BLOQUE 7: LECTURA DE VARIABLES PARA LA CLASIFICACI�N
#####################################################################################################################################

#lectura de la imagen a clasificar

img <- brick("comp_variables.tif")

img

#abreviaci�n de los nombres
names(img) <- c(paste0("B", 1:20, coll = ""), "B20")

img

#lectura de los rois de entrenamiento

trainData <- shapefile("D:/s2_img/datos_entrenamiento/datos_entrenamiento.shp")
responseCol <- "class" #el shapefile debe contener un �nico campo con nombre "class"

trainData

#extracci�n de valores de pixel en las �reas de entrenamiento

dfAll = data.frame(matrix(vector(), nrow = 0, ncol = length(names(img)) + 1))   
for (i in 1:length(unique(trainData[[responseCol]]))){
  category <- unique(trainData[[responseCol]])[i]
  categorymap <- trainData[trainData[[responseCol]] == category,]
  dataSet <- extract(img, categorymap)
  if(is(trainData, "SpatialPointsDataFrame")){
    dataSet <- cbind(dataSet, class = as.numeric(rep(category, nrow(dataSet))))
    dfAll <- rbind(dfAll, dataSet[complete.cases(dataSet),])
  }
  if(is(trainData, "SpatialPolygonsDataFrame")){
    dataSet <- dataSet[!unlist(lapply(dataSet, is.null))]
    dataSet <- lapply(dataSet, function(x){cbind(x, class = as.numeric(rep(category, nrow(x))))})
    df <- do.call("rbind", dataSet)
    dfAll <- rbind(dfAll, df)
  }
}

#reducci�n de la muestra a 1000

nsamples <- 1000
sdfAll <- dfAll[sample(1:nrow(dfAll), nsamples), ]


##############################################################################################################################
#BLOQUE 8: ENTRENAMIENTO DEL MODELO Y PREDICCI�N MEDIANTE RANDOM FOREST (RF)
##############################################################################################################################

#entrenamiento del modelo RF

#�����ERROR!!!!! No lee la B20

modFit_rf <- train(as.factor(class) ~ B1 + B2 + B3 + B4 + B5 + B6 + B7 + B8 + B9 + B10 + B11 + B12 + B13 + B14 + B15 + B16 + B17 + B18 + B19, method = "rf", data = sdfAll) #RF por defecto 500 arboles

#predicci�n RF

beginCluster()
preds_rf <- clusterR(img, raster::predict, args = list(model = modFit_rf))
endCluster()


writeRaster(preds_rf,"preds_RF500.tiff", drivername="Gtiff") #Exporta un raster clasificado en formato .tif

preds_rf

prediccion_RF500 <- raster("preds_RF500.tif")

plot(prediccion_RF500, main = "Predicci�n RanfomForest 500")



##############################################################################################################################
#BLOQUE 9: ENTRENAMIENTO DEL MODELO Y PREDICCI�N MEDIANTE SUPPORT VECTOR MACHINE (SVM)
##############################################################################################################################

#entrenamiento del modelo SVM



