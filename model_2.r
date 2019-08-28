

##########################################################################################################################
#BLOQUE 0: INSTALACIÓN DE LIBRERIAS
##########################################################################################################################

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

###########################################################################################################################
#BLOQUE 1: DEFINIR DIRECTORIO DE TRABAJO
###########################################################################################################################

setwd("D:/S2_img") #ASIGNAR---Directorio de trabajo

getwd()

###########################################################################################################################
#BLOQUE 2: CONVERSIÓN IMAGENES .JP2 A .TIF (GEOTIFF)
###########################################################################################################################

gdal_translate("bandaAOT.jp2", "bandaAOT.tif")   #ASIGNAR--- AOT Nombre archivo de entrada y salida

gdal_translate("banda02.jp2", "banda02.tif")   #ASIGNAR--- B02 Nombre archivo de entrada y salida

gdal_translate("banda03.jp2", "banda03.tif")   #ASIGNAR--- B03 Nombre archivo de entrada y salida

gdal_translate("banda04.jp2", "banda04.tif")   #ASIGNAR--- B04 Nombre archivo de entrada y salida

gdal_translate("banda08.jp2", "banda08.tif")   #ASIGNAR--- B08 Nombre archivo de entrada y salida

gdal_translate("bandaTCI.jp2", "bandaTCI.tif")   #ASIGNAR--- TCI Nombre archivo de entrada y salida

gdal_translate("bandaWVP", "bandaWVP.tif")   #ASIGNAR--- WVP Nombre archivo de entrada y salida


#bandas con resolución 20m en otro directorio

gdal_translate("ruta_archivo_banda11.jp2", "banda11_20m.tif") #ASIGNAR--- Ruta de archivo B11

gdal_translate("ruta_archivo_banda12.jp2", "banda12_20m.tif") #ASIGNAR--- Ruta de archivo B12


#############################################################################################################################
#BLOQUE 3: CORRECCIÓN RESOLUCIÓN BANDA 11 Y 12
#############################################################################################################################

band11_20m <- raster("banda11_20m.tif")
band12_20m <- raster("banda12_20m.tif")

band11_10m <- disaggregate(band_11_20m, fac=2)
band12_10m <- disaggregate(band_12_20m, fac=2)

band11_10m

writeRaster(band11_10m,"banda11.tiff", drivername="Gtiff") #exporta la BANDA 11 en .tif

band12_10m

writeRaster(band12_10m,"banda12.tiff", drivername="Gtiff") #exporta la BANDA 12 en .tif


#############################################################################################################################
#BLOQUE 4: COMPOSICIÓN MULTIESPECTRAL
#############################################################################################################################

comp_multiespectral <- list.files('D:/S2_img', full.names = TRUE, pattern="tif") #compila todos los .tif de la carpetas

#Orden según:

#Layer 1 -- banda02
#Layer 2 -- banda03
#Layer 3 -- banda04
#Layer 4 -- banda08
#Layer 5 -- banda11
#Layer 6 -- banda12
#Layer 7 -- bandaAOT
#Layer 8 -- bandaTCI
#Layer 9 -- bandaWVP

writeRaster(comp_multiespectral,"comp_multiespectral.tiff", drivername="Gtiff") #exporta la imagen multiespectral en .tif

#composición tipo stack

comp_mult-ST <- stack(comp_multiespectral)

plot(comp_mult-ST)

#composición tipo brick

comp_mult-BR <- brick(comp_mult-ST)

plot(comp_mult-BR)

comp_mult-BR


##############################################################################################################################
#BLOQUE 5: CÁLCULO DE ÍNDICES DE VEGETACIÓN, HUMEDAD Y OTROS
##############################################################################################################################

L4 <- raster(comp_mult-BR, layer=4) #NIR=Banda 8 Sentinel-2
L3 <- raster(comp_mult-BR, layer=3) #RED=Banda 4 Sentinel-2
L2 <- raster(comp_mult-BR, layer=2) #GREEN=Banda 3 Sentinel-2
L1 <- raster(comp_mult-BR, layer=1) #BLUE=Banda 2 Sentinel-2

L5 <- raster(comp_mult-BR, layer=5) #SWIR11=Banda 11 Sentinel-2
L6 <- raster(comp_mult-BR, layer=6) #SWIR12=Banda 12 Sentinel-2


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

nrvi = rvi-1/rvi+1 ## ## ## DUDA EN LA ECUACIÓN ## ## ##

writeRaster(nrvi,"nrvi.tiff", drivername="Gtiff") #exporta la capa NRVI en .tif

#NDWI11

ndwi11 = (L4-L5)/(L4+L5)

writeRaster(ndwi11,"ndwi11.tiff", drivername="Gtiff") #exporta la capa NDWI_11 en .tif

#NDWI12

ndwi12 = (L4-L6)/(L4+L6)

writeRaster(ndwi12,"ndwi12.tiff", drivername="Gtiff") #exporta la capa NDWI_12 en .tif

#TVI

tvi = sqrt((L4-L3)/(L4+L3))

writeRaster(tvi,"tvi.tiff", drivername="Gtiff") #exporta la capa TVI en .tif

#TTVI

ttvi = sqrt(abs(ndvi+0.5))

writeRaster(ttvi,"ttvi.tiff", drivername="Gtiff") #exporta la capa TTVI en .tif


##############################################################
#ESPACIO RESERVADO PARA EL CÁLCULO DEL CANOPY HEIGH MODEL
##############################################################

#mdt <- ("ruta_mdt.tif")

#mds <- ("ruta_mds.tif")

#chm = mds-mdt

#writeRaster(chm,"chm.tiff", drivername="Gtiff") #exporta la capa CHM en .tif

#NOTA: El CHM debe de tener el mismo tamaño de pixel que el resto de bandas (10x10)
#En caso de no tener la misma, y que la del CHM sea menor (<10x10), es preferible reducir la resolución del resto de bandas
#que aumentar la resolución de CHM, ya que de esta manera perderíamos la resolución del CHM

##############################################################################################################################
#BLOQUE 6: COMPOSICIÓN MULTIBANDA DE VARIABLES EXPLICATIVAS
##############################################################################################################################

getwd()

b1 <- ("banda2.tif")
b2 <- ("banda3.tif")
b3 <- ("banda4.tif")
b4 <- ("banda8.tif")

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

writeRaster(brick_variables,"brick_variables.tiff", drivername="Gtiff") #Exporta un archivo multicapa con todas las variables



brick_variables

##############################################################################################################################
#BLOQUE 7: LECTURA DE DATOS DE ENTRENAMIENTO PARA LOS DISTINTOS MODELOS
##############################################################################################################################



