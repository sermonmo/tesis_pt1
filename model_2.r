

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

gdal_translate("ruta_banda11.jp2", "banda11.tif") #ASIGNAR--- B11 Nombre archivo de entrada y salida

gdal_translate("ruta_banda12.jp2", "banda12.tif") #ASIGNAR--- B12 Nombre archivo de entrada y salida


#############################################################################################################################
#BLOQUE 3: CORRECCIÓN RESOLUCIÓN BANDA 11 Y 12
#############################################################################################################################

band11_20m <- raster("banda11.tif")  #ASIGNAR--- Ruta archivo Banda 11
band12_20m <- raster("banda12.tif")  #ASIGNAR--- Ruta archivo Banda 12

band11_10m <- disaggregate(band_11, fac=2)
band12_10m <- disaggregate(band_12, fac=2)

band11_10m

writeRaster(band11_10m,"band11_10m.tiff", drivername="Gtiff") #exporta la BANDA 11 en .tif

band12_10m

writeRaster(band12_10m,"band11_10m.tiff", drivername="Gtiff") #exporta la BANDA 12 en .tif


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


##############################################################################################################################
#BLOQUE 6: COMPOSICIÓN MULTIBANDA DE VARIABLES EXPLICATIVAS
##############################################################################################################################



