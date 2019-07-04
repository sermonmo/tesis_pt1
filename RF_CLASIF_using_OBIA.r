
#0
#CARGAR LIBRERIAS

library(raster)
library(data.table)
library(maptools)
library(randomForest)
library(rgdal)

library(sp)
library(raster)
library(ggplot2)

library(rgeos)
library(landsat8)

#1
#CORRECCION ATMOSFERICA (LANDSAT-8)

#Corrección atmosférica
setwd("C:/Maestria en Geomatica/Percepcion remota avanzada/Taller 2/Segment_landsat")

#Bandas a utilizar

banda2 <- raster("C:/Maestria en Geomatica/Percepcion remota avanzada/Taller  2/Segment_landsat/LC08_L1TP_007057_20170814_20170825_01_T1_B2.TIF")
banda3 <- raster("C:/Maestria en Geomatica/Percepcion remota avanzada/Taller 2/Segment_landsat/LC08_L1TP_007057_20170814_20170825_01_T1_B3.TIF")
banda4 <- raster("C:/Maestria en Geomatica/Percepcion remota avanzada/Taller 2/Segment_landsat/LC08_L1TP_007057_20170814_20170825_01_T1_B4.TIF")
banda5 <- raster("C:/Maestria en Geomatica/Percepcion remota avanzada/Taller 2/Segment_landsat/LC08_L1TP_007057_20170814_20170825_01_T1_B5.TIF")
banda6 <- raster("C:/Maestria en Geomatica/Percepcion remota avanzada/Taller 2/Segment_landsat/LC08_L1TP_007057_20170814_20170825_01_T1_B6.TIF")
banda7 <- raster("C:/Maestria en Geomatica/Percepcion remota avanzada/Taller 2/Segment_landsat/LC08_L1TP_007057_20170814_20170825_01_T1_B7.TIF")

banda2.dn <- as(banda2, 'SpatialGridDataFrame')
banda2.rad <- radconv(banda2.dn, 1.2532E-02, -62.66107)
banda3.dn <- as(banda3, 'SpatialGridDataFrame')
banda3.rad <- radconv(banda3.dn, 1.1548E-02, -57.74164)
banda4.dn <- as(banda4, 'SpatialGridDataFrame')
banda4.rad <- radconv(banda4.dn, 9.7382E-03, -48.69100)
banda5.dn <- as(banda5, 'SpatialGridDataFrame')
banda5.rad <- radconv(banda5.dn, 5.9593E-03, -29.79646)
banda6.dn <- as(banda6, 'SpatialGridDataFrame')
banda6.rad <- radconv(banda6.dn, 1.4820E-03, -7.41011)
banda7.dn <- as(banda7, 'SpatialGridDataFrame')
banda7.rad <- radconv(banda7.dn, 4.9952E-04, -2.49760)

Reflectancia. reflconvS(x, Mp, Ap, sunelev)

refb2 <- reflconvS(banda2.dn, 2.0000E-05, -0.100000, 61.24856156)
refb3 <- reflconvS(banda3.dn, 2.0000E-05, -0.100000, 61.24856156)
refb4 <- reflconvS(banda4.dn, 2.0000E-05, -0.100000, 61.24856156)
refb5 <- reflconvS(banda5.dn, 2.0000E-05, -0.100000, 61.24856156)
refb6 <- reflconvS(banda6.dn, 2.0000E-05, -0.100000, 61.24856156)
refb7 <- reflconvS(banda7.dn, 2.0000E-05, -0.100000, 61.24856156)

Para poder ralizar la union de las bandas es necesario transformar el formato Spatialgriddataframe a formato raster     refb1 <- as(refb1, 'RasterLayer') plot(refb1)

refb2 <- as(refb2, 'RasterLayer')
refb3 <- as(refb3, 'RasterLayer')
refb4 <- as(refb4, 'RasterLayer')
refb5 <- as(refb5, 'RasterLayer')
refb6 <- as(refb6, 'RasterLayer')
refb7 <- as(refb7, 'RasterLayer')

#Union de bandas

bandas <- brick(refb2, refb3, refb4, refb5, refb6, refb7)

cortar por zona de estudio

zona <- shapefile("C:/Maestria en Geomatica/Percepcion remota avanzada/Taller 2/Segment_landsat/Area_est.shp")

bandas <- crop(bandas, extent(zona))
bandas <- mask(bandas, zona)

writeRaster(bandas, filename="zonaest.tif", overwrite=TRUE)

Ploteo en color verdadero
plotRGB(bandas, r=3, g=2, b=1, scale=10000, stretch="lin", main="Composicion a color verdadero zona de estudio")

#2
#CREACION DE SEGMENTOS

#Set de datos
#Nombre y ruta de la imagen 
satImage <- "C:/Maestria en Geomatica/Percepcion remota avanzada/Taller 2/Segment_landsat/zonaest.tif"

#Nombre y ruta del vector de la imagen segmentada. Si se usa una imagen raster por segmentos se utilizan dos comillas dobles o sencillas.Sin espacios entre ellas.
segVector <- 'C:/Maestria en Geomatica/Percepcion remota avanzada/Taller 2/Segment_landsat/seg2.shp'

#Etiqueta para el campo de atributo en el shapefile que tiene los segmentos IDs. Esto se ignora si los datos de los segmentos vienen con la imagen. 
idAttributeLabel <- "DN"

#Nombre y ruta de la imagen segmentada. Si se utiliza un archivo vetor de segmentos utilizar comillas dobles o sencillas sin espacio entre ellas.
segImage <- ""

#Nombre y ruta del archivo CSV de salida
outFeatures <- "C:/Maestria en Geomatica/Percepcion remota avanzada/Taller 2/Segment_landsat/SegmentFeature_Subras3.csv"

#Para los no valores de la imagen de entrada.
ndSat <- 0
""
 #No valores de la imagen segmentada.
ndSeg <- -1
###########################################################################################

#Inicio del proceso
startTime <- Sys.time()
cat("Start time", format(startTime),"\n")

#Definición de la funcion para calcular las características de cada segmento.
#Funcioón para calcular promedio despes de recortar el 20% superior e inferior de los valores.
clippedMeanFun <- function(nums,...) {
 a=quantile(nums, probs=seq(0,1,0.2), na.rm = TRUE)
 mean(nums[which(nums >= a[2] & nums <= a[5])])
}

 #Función para calcular la desviación estandar despues de recortar el 20% superior e inferior de los valores.
clippedSDFun <- function(nums,...) {
a=quantile(nums, probs=seq(0,1,0.2), na.rm = TRUE)
sd(nums[which(nums >= a[2] & nums <= a[5])])
}

 #Función para calcular el coeficiente de variacion despues de recortan el 20% superior e inferior de los valores.
clippedCVFun <- function(nums,...) {
a=quantile(nums, probs=seq(0,1,0.2), na.rm = TRUE)
as.double(cv(nums[which(nums >= a[2] & nums <= a[5])]))
}

 #Función para calcular la mediana, ya que la tabla de datos tiene problemas con la mediana de los datos enteros 
medianFun <- function(nums,...) {
median(as.double(nums), na.rm = TRUE)
}

#Función para crear etiquetas de las columnas para que coincida con el número de bandas de la imagen.
createColLabels <- function(numBands, baseName) {
labels <- "segnumber"
for (i in 1:numBands) {
  labels[i+1] <- paste(baseName, "Band", i, sep='')
 }
labels
}


#3
#CLASIFICACION RF

# Nombre y ubicación del archivo CSV resultante CSV de la Parte II de este cuaderno
segCsv <- "C:/Maestria en Geomatica/Percepcion remota avanzada/Taller 2/Segment_landsat/SegmentFeature_Subras3.csv"


# Nombre y ubicación del raster de la imagen segmentada  
segImage <- "C:/Maestria en Geomatica/Percepcion remota avanzada/Taller 2/Segment_landsat/Raster_seg2.tif"

# No valores de la raster segmentado.
nd <- 1

# Nombre y ubicacion de la imagen clasificada.
outImage <- 'C:/Maestria en Geomatica/Percepcion remota avanzada/Taller 2/Segment_landsat/Forest_NonforestMapTest_v9.tif'

# Nombre y ubicacion del shp de puntos de salida de la parte II del cuaderno. Si esta salida no se necesita se puede poner dos comillas dobles o sencillas
# Importante tener en cuenta que si este archivo existe, la escritura fallará con el mensaje "Falló la creación del archivo de salida".
outMarginFile <- 'C:/Maestria en Geomatica/Percepcion remota avanzada/Taller 2/Segment_landsat/testAccuracyPoints8.shp'

# Nombre del conjunto de datos para el archivo vectorial que contiene datos de entrenamiento.
# Esta y "layer" estan definidos por el controlador ORG.
trainingDsn <- 'C:/Maestria en Geomatica/Percepcion remota avanzada/Taller 2/Segment_landsat/Entrenamiento.shp'

# Nombre de la capa para el archivo vectorial 
trainingLayer <- 'Entrenamiento'

# Nombre de la columna que contiene el numero de clase
classNum <- "Id_Type"

# Archivo CSV de salica con la informacion de mapeo de clase. Si esta salida no se necesita se puede utilizar comillas dobles o sencillas.
outClassMapCSV <- 'C:/Maestria en Geomatica/Percepcion remota avanzada/Taller 2/Segment_landsat/classMapping_v2.csv'

# Clasificación de salida sin aplicar limites (Output classification without applying threshold (Verdadero o Falso)
classImage <- TRUE

# Probabilidad de salida de la imagen (Verdadero o Falso)
probImage <- TRUE

# Clasificacion de salida y set de pixeles con limite de 0 (Verdadero o Falso)
threshImage <- TRUE

# Enter threshold probability in percent (values must be between 0 and 100) only used if threshImage=TRUE
probThreshold <- 75

###########################################################################################
## Inicio del proceso
startTime <- Sys.time()
cat("Start time", format(startTime),"\n")

# Leer archivo vector
cat("Reading the vector file\n")

vec <- readOGR(trainingDsn, trainingLayer)

## OGR data source with driver: ESRI Shapefile 
## Source: "C:/Maestria en Geomatica/Percepcion remota avanzada/Taller 2/Segment_landsat/Entrenamiento.shp", layer: "Entrenamiento"
## with 200 features
## It has 3 fields

pts <- slot(vec, "data")

# Descargar imagen raster
segImg <-raster(segImage)

# Etraer el ID de los segmentos bajo las caracteristicas de punto.
cat("Extracting segment IDs under the vector features\n")

exSegNums <- extract(segImg, vec, cellnumbers=TRUE)
if (is.matrix(exSegNums)) {
  exSegNums <- as.list(data.frame(t(exSegNums)))
}

# Eliminar valores nulos de la listaRemove NULL values from the list
exSegNums <- exSegNums[!sapply(exSegNums, is.null)]

# Seleccionar el ID de segmetos unicos bajo las caracteristicas de caa vector y asociar al respuesta.
# variable ("classNum") del segmento ID 
trainSegs <- matrix(ncol=3, nrow=0)
for (i in 1:length(exSegNums)) {
  lineResponse <- pts[i,classNum]
  if (is.matrix(exSegNums[[i]])) { 
    segNums <- exSegNums[[i]][which(duplicated(exSegNums[[i]][,2]) == FALSE),]
  } else {
    segNums <- exSegNums[[i]]
  }
  
  if (is.matrix(segNums)) {
    trainSegs <- rbind(trainSegs, cbind(lineResponse, segNums))
  }
  else {
    trainSegs <- rbind(trainSegs, cbind(lineResponse, segNums[1], segNums[2]))
  }  
}

# Eliminar nombres de filas y adicionar nombres de columnas. 
rownames(trainSegs) <- NULL
colnames(trainSegs) <- c("response", "cellNums", "segNums")

# Eliminr valores de NA del listado del ID del segmento unico, 
trainSegs_no_na <- as.data.frame(na.omit(trainSegs))

# Leer archivo CSV con la informacion caracteristica del segmento.
segAtr <- read.csv(segCsv, header=TRUE)

# Eliminar NAs de la tabla de caracteristicas.
segAtr <- na.omit(segAtr)

# Crear un data set de los datos de entrenamiento haciendo coincidir las ID de segmento de entrenamiento únicos con la información de características del segmento.
train <- segAtr[match(trainSegs_no_na$segNums, segAtr$segnumber),]
summary(train)

# Eliminar NAs del data frame de entrenamiento
train_no_na <- as.data.frame(na.omit(train))

# Crear un data frame con los datos variables de respuesta.
response_no_na <- trainSegs_no_na[match(train_no_na$segnumber, trainSegs_no_na$segNums), c(1,3)]

# Realizar la clasificación con el algoritmo RF.
cat("Starting to calculate random forest object \n)")

## Starting to calculate random forest object 
## )

randfor <- randomForest(as.factor(response_no_na$response) ~. , data=train_no_na[,-1], proximity=TRUE)

# Escribir la salida forest/nonforest raster map.
# Redescargar el paquete raster.
bs <- blockSize(segImg)

# Calcular cuantas bandas de salia de la image pueden Calculate how many bands the output image should have
numBands <- classImage + probImage + threshImage

# Crear el raster de salida y empezar a escribir sobre el.
img.out <- brick(segImg, values=FALSE, nl=numBands)
img.out <- writeStart(img.out, outImage, overwrite=TRUE, datatype='INT1U')

# Predicció
predValues <- predict(randfor, segAtr, type='response')
predValuesDF <- data.frame(segAtr$segnumber, predValues)

if (probImage || threshImage) { 
  predProbs <- predict(randfor, segAtr, type='prob')
  maxProb <- round(apply(predProbs, 1, max) * 100)
  maxProbDF <- data.frame(segAtr$segnumber, maxProb)
}

# Recorre los bloques del ráster de salida de Orfeo y escriba el nuevo valor clasificado.
# Este método de bucle permite la entrada de rásteres más grandes sin problemas de memoria.
for (i in 1:bs$n) {
  cat("processing block", i, "of", bs$n, "\r")
  img <- getValues(segImg, row=bs$row[i], nrows=bs$nrows[i])
  outMatrix <- matrix(nrow=length(img), ncol=0)
  # Establecer el valor sin datos NA, para que no se convierta en un valor predicho
  is.na(img) <- img == nd
  if (classImage) {
    # Convierta el ID de segmento a la clase predicha (numérica) para que se pueda establecer un valor de no_data.
    outMatrix <- cbind(outMatrix, predValuesDF$predValues[match(img, predValuesDF$segAtr.segnumber)])
  }
  if (probImage) { 
    outMatrix <- cbind(outMatrix, maxProbDF$maxProb[match(img, maxProbDF$segAtr.segnumber)])
  }
  if (threshImage) {
    threshValues <- as.numeric(predValuesDF$predValues[match(img, predValuesDF$segAtr.segnumber)])
    threshValues[which(maxProbDF$maxProb[match(img, maxProbDF$segAtr.segnumber)] <= probThreshold)] <- 0
    outMatrix <- cbind(outMatrix,threshValues)
  }
  writeValues(img.out, outMatrix, bs$row[i])
}

# Terminar salvar y cerrar la covecion de la imagen.
img.out <- writeStop(img.out)

# Mapa de clase de salida.
if (outClassMapCSV != "") {
  write.csv(predValuesDF, file=outClassMapCSV, row.names=FALSE)
}

# View variable importance plot.
varImpPlot(randfor)


# Imprimir rata de error y matriz de confusion por Print error rate and confusion matrix for this classification
confMatrix <- randfor$confusion
cat("\n#################################################################################\n")

cat("OOB error rate estimate\n", 1 - (sum(diag(confMatrix)) / sum(confMatrix[,1:ncol(confMatrix)-1])), "%\n\n", sep="")

cat("Confusion matrix\n")

print(randfor$confusion)

cat("\n")

if (outMarginFile != "") {
  # Calcular el margen (proporción de votos para la clase correcta menos la proporción máxima de votos para otras clases)
  marginData <- randomForest::margin(randfor)
  trainingAccuracy <- cbind(marginData[order(marginData)], trainSegs_no_na[order(marginData),])
  
  # Adicionar nombres de columnas de atributos de la tabla
  colnames(trainingAccuracy) <- c("margin", "classNum", "cellNum", "segID")
  
  # Calcular coordenadas X y Y para los datos de entrenamiento de puntos
  xyCoords <- matrix(ncol=2, nrow=0)
  for (z in 1:nrow(trainingAccuracy)) {
    xyCoords <- rbind(xyCoords, xyFromCell(segImg, trainingAccuracy[z,3]))
  }
  
  # Crear y escribir el shp de punto con margen de información para ayudar a mejorer los datos de entrenamiento
  pointVector <- SpatialPointsDataFrame(xyCoords, trainingAccuracy[, c(1,2,4)], coords.nrs = numeric(0), proj4string = segImg@crs)
  writeOGR(pointVector, outMarginFile, "layer", driver="ESRI Shapefile", check_exists=TRUE)
}

# Calculo de tiempo de procedimiento
timeDiff <- Sys.time() - startTime
cat("Processing time", format(timeDiff), "\n")


# Ploteo
clasificacion = raster("C:/Maestria en Geomatica/Percepcion remota avanzada/Taller 2/Segment_landsat/Forest_NonforestMapTest_v9.tif")

cols <- c("aliceblue", "green", "darkgreen", "yellow2", "blue2", "navy", "gray47", "chartreuse1", "red" )

  plot(clasificacion, col = cols, main = "Clasificacion de cobertura ", legend=TRUE)

#4
#EDICION DE LA CLASIFICACION

# Nombre y ubicacion de de la imagen raster segmentada. 
segImage <- "C:/Maestria en Geomatica/Percepcion remota avanzada/Taller 2/Segment_landsat/Raster_seg2.tif"

# No valores del raster segmentado.
nd <- 1

# Nombre y ubicacion de la edicion del mapa clasificado
outImage <- 'C:/Maestria en Geomatica/Percepcion remota avanzada/Taller 2/Segment_landsat/Forest_NonforestMapTest_v8.tif'

# Ingresar archivo CSV  de ckassufucacuib y mapas
inClassMapCSV <- 'C:/Maestria en Geomatica/Percepcion remota avanzada/Taller 2/Segment_landsat/classMapping_v2.csv'

# Nombre del conjunto de datos para el archivo de puntos vectoriales que contiene las ubicaciones y asignaciones de clase para los segmentos que se modificarán. 
editPointsDsn <- 'C:/Maestria en Geomatica/Percepcion remota avanzada/Taller 2/Segment_landsat/editSegs.shp'

# Nombre del archivo vector. -este es a menudo  el nombre del archivo sin extensiónThis is often the file name without an extension. 
editPointsLayer <- 'editSegs'

# Introducir cualquier nombre o numero de columna.

newClassNum <- "newClass"
###########################################################################################
## Inicio del proceso
startTime <- Sys.time()
cat("Start time", format(startTime),"\n")

# Leer el archivo vector
cat("Reading the vector file\n")

vec <- readOGR(editPointsDsn, editPointsLayer)

## OGR data source with driver: ESRI Shapefile 
## Source: "C:/Maestria en Geomatica/Percepcion remota avanzada/Taller 2/Segment_landsat/editSegs.shp", layer: "editSegs"
## with 13 features
## It has 3 fields
## Integer64 fields read as strings:  Id newClass

newClass <- slot(vec, "data")

# Load the segment raster image
segImg <- raster(segImage)

# Abrir archivo CSV
classMap <- read.csv(inClassMapCSV)

# Extraer el ID de los segmentos, bajo los puntos editados.
cat("Extracting segment IDs under the points to modify\n")

changeSegIDs <- cbind(newClass[,newClassNum], extract(segImg, vec))

# Cambiar las clases de mapero con base en los datos punto.
for (i in 1:nrow(changeSegIDs)) {
  #classMap[changeSegIDs[i, 2], 2] <- changeSegIDs[i,1]
  classMap[which(classMap[,1]==changeSegIDs[i, 2]), 2] <- changeSegIDs[i,1]
}

# Escribir la salida forest/raster map. Forest_NonforestMapTest_v8.
# REcargar packete raster.
bs <- blockSize(segImg)

# Crea el raster de salida y comienza a escribir en el
img.out <- raster(segImg)
img.out <- writeStart(img.out, outImage, overwrite=TRUE, datatype='INT1U')

# Recorre los bloques del ráster de salida de Orfeo y escriba el nuevo valor clasificado.
# Este método de bucle permitirá la entrada de rásteres más grandes sin problemas de memoria.
for (i in 1:bs$n) {
  cat("processing block", i, "of", bs$n, "\r")
  # require(raster)
  img <- getValues(segImg, row=bs$row[i], nrows=bs$nrows[i])
  # Set the no data value to NA so it doesn't get converted to a predicted value
  is.na(img) <- img == nd
  # Convert the segment ID to the predicted (numeric) class so that a nodata value can be set.
  img.match <- as.numeric(classMap$pred[match(img, classMap$segAtr.segnumber)])
  # Set the no data value to the default value for the output image
  img.match[is.na(img.match) == TRUE] <- NAvalue(img.out)
  writeValues(img.out, img.match, bs$row[i])
}

# Termina guardando y cierra la conexion de la magen.
img.out <- writeStop(img.out)

# Calculo del tiempo de procesamiento.
timeDiff <- Sys.time() - startTime
cat("Processing time", format(timeDiff), "\n")

# Ploteo
clasificacion_correc = raster("C:/Maestria en Geomatica/Percepcion remota avanzada/Taller 2/Segment_landsat/Forest_NonforestMapTest_v8.tif")

cols <- c("aliceblue", "green", "darkgreen", "yellow2", "blue2", "navy", "gray47", "chartreuse1", "red" )

  plot(clasificacion_correc, col = cols, main = "Edicion Clasificacion de cobertura ", legend=TRUE)

