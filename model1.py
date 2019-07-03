# -*- coding: utf-8 -*-
# ---------------------------------------------------------------------------
# model1.py
# Created on: 2019-04-02 18:45:07.00000
#   (generated by ArcGIS/ModelBuilder)
# Usage: model1 <PARCELAS_EVAL_limpio_shp> <BAND_2__Sentinel_2__L2A__10m__jp2> <BAND_3__Sentinel_2__L2A__10m__jp2> <BAND_4__Sentinel_2__L2A__10m__jp2> <BAND_5__Sentinel_2__L2A__20m__jp2> <BAND_6__Sentinel_2__L2A__20m__jp2> <BAND_7__Sentinel_2__L2A__20m__jp2> <BAND_8__Sentinel_2__L2A__10m__jp2> <BAND_11__Sentinel_2__L2A__20m__jp2> <BAND_12__Sentinel_2__L2A__20m__jp2> <BAND_1__Sentinel_2__L2A__60m__jp2> <BAND_9__Sentinel_2__L2A__60m__jp2> <BAND_8A__Sentinel_2__L2A__20m__jp2> 
# Description: 
# ---------------------------------------------------------------------------

# Set the necessary product code
# import arcinfo


# Import arcpy module
import arcpy

# Script arguments
PARCELAS_EVAL_limpio_shp = arcpy.GetParameterAsText(0)
if PARCELAS_EVAL_limpio_shp == '#' or not PARCELAS_EVAL_limpio_shp:
    PARCELAS_EVAL_limpio_shp = "D:\\MODELO_1\\capas_entrada\\parcelas_evaluar\\PARCELAS_EVAL_limpio.shp" # provide a default value if unspecified

BAND_2__Sentinel_2__L2A__10m__jp2 = arcpy.GetParameterAsText(1)
if BAND_2__Sentinel_2__L2A__10m__jp2 == '#' or not BAND_2__Sentinel_2__L2A__10m__jp2:
    BAND_2__Sentinel_2__L2A__10m__jp2 = "D:\\MODELO_1\\capas_entrada\\BAND 2 (Sentinel-2, L2A, 10m).jp2" # provide a default value if unspecified

BAND_3__Sentinel_2__L2A__10m__jp2 = arcpy.GetParameterAsText(2)
if BAND_3__Sentinel_2__L2A__10m__jp2 == '#' or not BAND_3__Sentinel_2__L2A__10m__jp2:
    BAND_3__Sentinel_2__L2A__10m__jp2 = "D:\\MODELO_1\\capas_entrada\\BAND 3 (Sentinel-2, L2A, 10m).jp2" # provide a default value if unspecified

BAND_4__Sentinel_2__L2A__10m__jp2 = arcpy.GetParameterAsText(3)
if BAND_4__Sentinel_2__L2A__10m__jp2 == '#' or not BAND_4__Sentinel_2__L2A__10m__jp2:
    BAND_4__Sentinel_2__L2A__10m__jp2 = "D:\\MODELO_1\\capas_entrada\\BAND 4 (Sentinel-2, L2A, 10m).jp2" # provide a default value if unspecified

BAND_5__Sentinel_2__L2A__20m__jp2 = arcpy.GetParameterAsText(4)
if BAND_5__Sentinel_2__L2A__20m__jp2 == '#' or not BAND_5__Sentinel_2__L2A__20m__jp2:
    BAND_5__Sentinel_2__L2A__20m__jp2 = "D:\\MODELO_1\\capas_entrada\\BAND 5 (Sentinel-2, L2A, 20m).jp2" # provide a default value if unspecified

BAND_6__Sentinel_2__L2A__20m__jp2 = arcpy.GetParameterAsText(5)
if BAND_6__Sentinel_2__L2A__20m__jp2 == '#' or not BAND_6__Sentinel_2__L2A__20m__jp2:
    BAND_6__Sentinel_2__L2A__20m__jp2 = "D:\\MODELO_1\\capas_entrada\\BAND 6 (Sentinel-2, L2A, 20m).jp2" # provide a default value if unspecified

BAND_7__Sentinel_2__L2A__20m__jp2 = arcpy.GetParameterAsText(6)
if BAND_7__Sentinel_2__L2A__20m__jp2 == '#' or not BAND_7__Sentinel_2__L2A__20m__jp2:
    BAND_7__Sentinel_2__L2A__20m__jp2 = "D:\\MODELO_1\\capas_entrada\\BAND 7 (Sentinel-2, L2A, 20m).jp2" # provide a default value if unspecified

BAND_8__Sentinel_2__L2A__10m__jp2 = arcpy.GetParameterAsText(7)
if BAND_8__Sentinel_2__L2A__10m__jp2 == '#' or not BAND_8__Sentinel_2__L2A__10m__jp2:
    BAND_8__Sentinel_2__L2A__10m__jp2 = "D:\\MODELO_1\\capas_entrada\\BAND 8 (Sentinel-2, L2A, 10m).jp2" # provide a default value if unspecified

BAND_11__Sentinel_2__L2A__20m__jp2 = arcpy.GetParameterAsText(8)
if BAND_11__Sentinel_2__L2A__20m__jp2 == '#' or not BAND_11__Sentinel_2__L2A__20m__jp2:
    BAND_11__Sentinel_2__L2A__20m__jp2 = "D:\\MODELO_1\\capas_entrada\\BAND 11 (Sentinel-2, L2A, 20m).jp2" # provide a default value if unspecified

BAND_12__Sentinel_2__L2A__20m__jp2 = arcpy.GetParameterAsText(9)
if BAND_12__Sentinel_2__L2A__20m__jp2 == '#' or not BAND_12__Sentinel_2__L2A__20m__jp2:
    BAND_12__Sentinel_2__L2A__20m__jp2 = "D:\\MODELO_1\\capas_entrada\\BAND 12 (Sentinel-2, L2A, 20m).jp2" # provide a default value if unspecified

BAND_1__Sentinel_2__L2A__60m__jp2 = arcpy.GetParameterAsText(10)
if BAND_1__Sentinel_2__L2A__60m__jp2 == '#' or not BAND_1__Sentinel_2__L2A__60m__jp2:
    BAND_1__Sentinel_2__L2A__60m__jp2 = "D:\\MODELO_1\\capas_entrada\\BAND 1 (Sentinel-2, L2A, 60m).jp2" # provide a default value if unspecified

BAND_9__Sentinel_2__L2A__60m__jp2 = arcpy.GetParameterAsText(11)
if BAND_9__Sentinel_2__L2A__60m__jp2 == '#' or not BAND_9__Sentinel_2__L2A__60m__jp2:
    BAND_9__Sentinel_2__L2A__60m__jp2 = "D:\\MODELO_1\\capas_entrada\\BAND 9 (Sentinel-2, L2A, 60m).jp2" # provide a default value if unspecified

BAND_8A__Sentinel_2__L2A__20m__jp2 = arcpy.GetParameterAsText(12)
if BAND_8A__Sentinel_2__L2A__20m__jp2 == '#' or not BAND_8A__Sentinel_2__L2A__20m__jp2:
    BAND_8A__Sentinel_2__L2A__20m__jp2 = "D:\\MODELO_1\\capas_entrada\\BAND 8A (Sentinel-2, L2A, 20m).jp2" # provide a default value if unspecified

# Local variables:
Band_1_clip = "D:\\MODELO_1\\capas_salida\\Band_1_clip"
Band_2_clip = "D:\\MODELO_1\\capas_salida\\Band_2_clip"
Band_3_clip = "D:\\MODELO_1\\capas_salida\\Band_3_clip"
Band_4_clip = "D:\\MODELO_1\\capas_salida\\Band_4_clip"
Band_5_clip = "D:\\MODELO_1\\capas_salida\\Band_5_clip"
Band_6_clip = "D:\\MODELO_1\\capas_salida\\Band_6_clip"
Band_7_clip = "D:\\MODELO_1\\capas_salida\\Band_7_clip"
Band_8_clip = "D:\\MODELO_1\\capas_salida\\Band_8_clip"
Band_8A_clip = "D:\\MODELO_1\\capas_salida\\Band_8A_clip"
Band_9_clip = "D:\\MODELO_1\\capas_salida\\Band_9_clip"
Band_11_clip = "D:\\MODELO_1\\capas_salida\\Band_11_clip"
Band_12_clip = "D:\\MODELO_1\\capas_salida\\Band_12_clip"
Comp_multi = "D:\\MODELO_1\\capas_salida\\Comp_multi"
ROIs_12347_shp = "D:\\MODELO_1\\capas_entrada\\ROIs_parcelas\\ROIs_12347.shp"
cent_rois_shp = "D:\\MODELO_1\\capas_salida\\centroide_rois\\cent_rois.shp"
Firmas_cent_GSG = "D:\\MODELO_1\\capas_salida\\CSup\\Firmas_cent.GSG"
Output_confidence_raster = ""
mall_parcelas_shp = "D:\\MODELO_1\\capas_salida\\mall_parcelas.shp"
mall_parcelas_shp__2_ = mall_parcelas_shp
NDVI = "D:\\MODELO_1\\capas_salida\\NDVI"
GNDVI = "D:\\MODELO_1\\capas_salida\\GNDVI"
RVI = "D:\\MODELO_1\\capas_salida\\RVI"
GVI = "D:\\MODELO_1\\capas_salida\\GVI"
NGRDI = "D:\\MODELO_1\\capas_salida\\NGRDI"
RG = "D:\\MODELO_1\\capas_salida\\RG"
TVI = "D:\\MODELO_1\\capas_salida\\TVI"
TTVI = "D:\\MODELO_1\\capas_salida\\TTVI"
NRVI = "D:\\MODELO_1\\capas_salida\\NRVI"
NDWI11 = "D:\\MODELO_1\\capas_salida\\NDWI11"
NDVI12 = "D:\\MODELO_1\\capas_salida\\NDVI12"
mall_parcelas_shp__3_ = mall_parcelas_shp__2_
mall_parcelas_shp__4_ = mall_parcelas_shp__3_
CSup = "D:\\MODELO_1\\capas_salida\\CSup\\CSup"
cent_parcelas_shp = "D:\\MODELO_1\\capas_salida\\cent_parcelas.shp"
cent_parcelas_shp__2_ = cent_parcelas_shp
cent_parcelas_shp__3_ = cent_parcelas_shp__2_
cent_parcelas_shp__4_ = cent_parcelas_shp__3_

# Process: Clip
arcpy.Clip_management(BAND_1__Sentinel_2__L2A__60m__jp2, "746051,885998402 4304765,89261219 758112,19049833 4314494,66611232", Band_1_clip, PARCELAS_EVAL_limpio_shp, "99999", "NONE", "NO_MAINTAIN_EXTENT")

# Process: Clip (2)
arcpy.Clip_management(BAND_2__Sentinel_2__L2A__10m__jp2, "746051,885998402 4304765,89261219 758112,19049833 4314494,66611232", Band_2_clip, PARCELAS_EVAL_limpio_shp, "99999", "NONE", "NO_MAINTAIN_EXTENT")

# Process: Clip (3)
arcpy.Clip_management(BAND_3__Sentinel_2__L2A__10m__jp2, "746051,885998402 4304765,89261219 758112,19049833 4314494,66611232", Band_3_clip, PARCELAS_EVAL_limpio_shp, "99999", "NONE", "NO_MAINTAIN_EXTENT")

# Process: Clip (4)
arcpy.Clip_management(BAND_4__Sentinel_2__L2A__10m__jp2, "746051,885998402 4304765,89261219 758112,19049833 4314494,66611232", Band_4_clip, PARCELAS_EVAL_limpio_shp, "99999", "NONE", "NO_MAINTAIN_EXTENT")

# Process: Clip (5)
arcpy.Clip_management(BAND_5__Sentinel_2__L2A__20m__jp2, "746051,885998402 4304765,89261219 758112,19049833 4314494,66611232", Band_5_clip, PARCELAS_EVAL_limpio_shp, "99999", "NONE", "NO_MAINTAIN_EXTENT")

# Process: Clip (6)
arcpy.Clip_management(BAND_6__Sentinel_2__L2A__20m__jp2, "746051,885998402 4304765,89261219 758112,19049833 4314494,66611232", Band_6_clip, PARCELAS_EVAL_limpio_shp, "99999", "NONE", "NO_MAINTAIN_EXTENT")

# Process: Clip (7)
arcpy.Clip_management(BAND_7__Sentinel_2__L2A__20m__jp2, "746051,885998402 4304765,89261219 758112,19049833 4314494,66611232", Band_7_clip, PARCELAS_EVAL_limpio_shp, "99999", "NONE", "NO_MAINTAIN_EXTENT")

# Process: Clip (8)
arcpy.Clip_management(BAND_8__Sentinel_2__L2A__10m__jp2, "746051,885998402 4304765,89261219 758112,19049833 4314494,66611232", Band_8_clip, PARCELAS_EVAL_limpio_shp, "99999", "NONE", "NO_MAINTAIN_EXTENT")

# Process: Clip (12)
arcpy.Clip_management(BAND_8A__Sentinel_2__L2A__20m__jp2, "746051,885998402 4304765,89261219 758112,19049833 4314494,66611232", Band_8A_clip, PARCELAS_EVAL_limpio_shp, "99999", "NONE", "NO_MAINTAIN_EXTENT")

# Process: Clip (9)
arcpy.Clip_management(BAND_9__Sentinel_2__L2A__60m__jp2, "746051,885998402 4304765,89261219 758112,19049833 4314494,66611232", Band_9_clip, PARCELAS_EVAL_limpio_shp, "99999", "NONE", "NO_MAINTAIN_EXTENT")

# Process: Clip (10)
arcpy.Clip_management(BAND_11__Sentinel_2__L2A__20m__jp2, "746051,885998402 4304765,89261219 758112,19049833 4314494,66611232", Band_11_clip, PARCELAS_EVAL_limpio_shp, "99999", "NONE", "NO_MAINTAIN_EXTENT")

# Process: Clip (11)
arcpy.Clip_management(BAND_12__Sentinel_2__L2A__20m__jp2, "746051,885998402 4304765,89261219 758112,19049833 4314494,66611232", Band_12_clip, PARCELAS_EVAL_limpio_shp, "99999", "NONE", "NO_MAINTAIN_EXTENT")

# Process: Composite Bands
arcpy.CompositeBands_management("D:\\MODELO_1\\capas_salida\\Band_1_clip;D:\\MODELO_1\\capas_salida\\Band_2_clip;D:\\MODELO_1\\capas_salida\\Band_3_clip;D:\\MODELO_1\\capas_salida\\Band_4_clip;D:\\MODELO_1\\capas_salida\\Band_5_clip;D:\\MODELO_1\\capas_salida\\Band_6_clip;D:\\MODELO_1\\capas_salida\\Band_7_clip;D:\\MODELO_1\\capas_salida\\Band_8_clip;D:\\MODELO_1\\capas_salida\\Band_8A_clip;D:\\MODELO_1\\capas_salida\\Band_9_clip;D:\\MODELO_1\\capas_salida\\Band_11_clip;D:\\MODELO_1\\capas_salida\\Band_12_clip", Comp_multi)

# Process: Feature To Point (2)
arcpy.FeatureToPoint_management(ROIs_12347_shp, cent_rois_shp, "INSIDE")

# Process: Create Signatures
arcpy.gp.CreateSignatures_sa("D:\\MODELO_1\\capas_salida\\Comp_multi", cent_rois_shp, Firmas_cent_GSG, "COVARIANCE", "CLASS_T")

# Process: Maximum Likelihood Classification
arcpy.gp.MLClassify_sa("D:\\MODELO_1\\capas_salida\\Comp_multi", Firmas_cent_GSG, CSup, "0.0", "EQUAL", "", Output_confidence_raster)

# Process: Raster to Point
arcpy.RasterToPoint_conversion(Band_2_clip, mall_parcelas_shp, "Value")

# Process: Raster Calculator
arcpy.gp.RasterCalculator_sa("Float(\"%BAND 8 (Sentinel-2, L2A, 10m).jp2%\" - \"%BAND 4 (Sentinel-2, L2A, 10m).jp2%\") / Float(\"%BAND 8 (Sentinel-2, L2A, 10m).jp2%\" + \"%BAND 4 (Sentinel-2, L2A, 10m).jp2%\")", NDVI)

# Process: Raster Calculator (2)
arcpy.gp.RasterCalculator_sa("Float(\"%BAND 8 (Sentinel-2, L2A, 10m).jp2%\" - \"%BAND 3 (Sentinel-2, L2A, 10m).jp2%\") / Float(\"%BAND 8 (Sentinel-2, L2A, 10m).jp2%\" + \"%BAND 3 (Sentinel-2, L2A, 10m).jp2%\")", GNDVI)

# Process: Raster Calculator (3)
arcpy.gp.RasterCalculator_sa("Float(\"%BAND 8 (Sentinel-2, L2A, 10m).jp2%\") / Float(\"%BAND 4 (Sentinel-2, L2A, 10m).jp2%\")", RVI)

# Process: Raster Calculator (4)
arcpy.gp.RasterCalculator_sa("Float(\"%BAND 8 (Sentinel-2, L2A, 10m).jp2%\") / Float(\"%BAND 3 (Sentinel-2, L2A, 10m).jp2%\")", GVI)

# Process: Raster Calculator (5)
arcpy.gp.RasterCalculator_sa("Float(\"%BAND 3 (Sentinel-2, L2A, 10m).jp2%\" - \"%BAND 4 (Sentinel-2, L2A, 10m).jp2%\") / Float(\"%BAND 3 (Sentinel-2, L2A, 10m).jp2%\" + \"%BAND 4 (Sentinel-2, L2A, 10m).jp2%\")", NGRDI)

# Process: Raster Calculator (6)
arcpy.gp.RasterCalculator_sa("Float(\"%BAND 4 (Sentinel-2, L2A, 10m).jp2%\") / Float(\"%BAND 3 (Sentinel-2, L2A, 10m).jp2%\")", RG)

# Process: Raster Calculator (7)
arcpy.gp.RasterCalculator_sa("((Float(\"%BAND 8 (Sentinel-2, L2A, 10m).jp2%\" - \"%BAND 4 (Sentinel-2, L2A, 10m).jp2%\") / Float(\"%BAND 8 (Sentinel-2, L2A, 10m).jp2%\" + \"%BAND 4 (Sentinel-2, L2A, 10m).jp2%\")) + 0.5) ^ 0.5", TVI)

# Process: Raster Calculator (8)
arcpy.gp.RasterCalculator_sa("Abs(Float(\"%BAND 8 (Sentinel-2, L2A, 10m).jp2%\" - \"%BAND 4 (Sentinel-2, L2A, 10m).jp2%\") / Float(\"%BAND 8 (Sentinel-2, L2A, 10m).jp2%\" + \"%BAND 4 (Sentinel-2, L2A, 10m).jp2%\") + 0.5) ^ 0.5", TTVI)

# Process: Raster Calculator (9)
arcpy.gp.RasterCalculator_sa("1 + Float(\"%BAND 8 (Sentinel-2, L2A, 10m).jp2%\" / \"%BAND 4 (Sentinel-2, L2A, 10m).jp2%\") - (1 / Float(\"%BAND 8 (Sentinel-2, L2A, 10m).jp2%\" / \"%BAND 4 (Sentinel-2, L2A, 10m).jp2%\"))", NRVI)

# Process: Raster Calculator (10)
arcpy.gp.RasterCalculator_sa("Float(\"%BAND 8 (Sentinel-2, L2A, 10m).jp2%\" - \"%BAND 11 (Sentinel-2, L2A, 20m).jp2%\") / Float(\"%BAND 8 (Sentinel-2, L2A, 10m).jp2%\" + \"%BAND 11 (Sentinel-2, L2A, 20m).jp2%\")", NDWI11)

# Process: Raster Calculator (11)
arcpy.gp.RasterCalculator_sa("Float(\"%BAND 8 (Sentinel-2, L2A, 10m).jp2%\" - \"%BAND 12 (Sentinel-2, L2A, 20m).jp2%\") / Float(\"%BAND 8 (Sentinel-2, L2A, 10m).jp2%\" + \"%BAND 12 (Sentinel-2, L2A, 20m).jp2%\")", NDVI12)

# Process: Extract Multi Values to Points (2)
arcpy.gp.ExtractMultiValuesToPoints_sa(mall_parcelas_shp, "D:\\MODELO_1\\capas_salida\\NDVI NDVI;D:\\MODELO_1\\capas_salida\\GNDVI GNDVI;D:\\MODELO_1\\capas_salida\\RVI RVI;D:\\MODELO_1\\capas_salida\\GVI GVI;D:\\MODELO_1\\capas_salida\\NGRDI NGRDI;D:\\MODELO_1\\capas_salida\\RG RG;D:\\MODELO_1\\capas_salida\\TVI TVI;D:\\MODELO_1\\capas_salida\\TTVI TTVI;D:\\MODELO_1\\capas_salida\\NRVI NRVI;D:\\MODELO_1\\capas_salida\\NDWI11 NDWI11;D:\\MODELO_1\\capas_salida\\NDVI12 NDWI12", "NONE")

# Process: Extract Multi Values to Points (4)
arcpy.gp.ExtractMultiValuesToPoints_sa(mall_parcelas_shp__2_, "'D:\\MODELO_1\\capas_entrada\\BAND 1 (Sentinel-2, L2A, 60m).jp2' rB1_60m;'D:\\MODELO_1\\capas_entrada\\BAND 2 (Sentinel-2, L2A, 10m).jp2' rB2_10m;'D:\\MODELO_1\\capas_entrada\\BAND 3 (Sentinel-2, L2A, 10m).jp2' rB3_10m;'D:\\MODELO_1\\capas_entrada\\BAND 4 (Sentinel-2, L2A, 10m).jp2' rB4_10m;'D:\\MODELO_1\\capas_entrada\\BAND 5 (Sentinel-2, L2A, 20m).jp2' rB5_20m;'D:\\MODELO_1\\capas_entrada\\BAND 6 (Sentinel-2, L2A, 20m).jp2' rB6_20m;'D:\\MODELO_1\\capas_entrada\\BAND 7 (Sentinel-2, L2A, 20m).jp2' rB7_20m;'D:\\MODELO_1\\capas_entrada\\BAND 8 (Sentinel-2, L2A, 10m).jp2' rB8_10m;'D:\\MODELO_1\\capas_entrada\\BAND 8A (Sentinel-2, L2A, 20m).jp2' rB8A_20m;'D:\\MODELO_1\\capas_entrada\\BAND 9 (Sentinel-2, L2A, 60m).jp2' rB9_60m;'D:\\MODELO_1\\capas_entrada\\BAND 11 (Sentinel-2, L2A, 20m).jp2' rB11_20m;'D:\\MODELO_1\\capas_entrada\\BAND 12 (Sentinel-2, L2A, 20m).jp2' rB12_20m", "NONE")

# Process: Extract Multi Values to Points (5)
arcpy.gp.ExtractMultiValuesToPoints_sa(mall_parcelas_shp__3_, "D:\\MODELO_1\\capas_salida\\CSup\\CSup CSUP", "NONE")

# Process: Feature To Point
arcpy.FeatureToPoint_management(PARCELAS_EVAL_limpio_shp, cent_parcelas_shp, "CENTROID")

# Process: Extract Multi Values to Points
arcpy.gp.ExtractMultiValuesToPoints_sa(cent_parcelas_shp, "D:\\MODELO_1\\capas_salida\\NDVI NDVI;D:\\MODELO_1\\capas_salida\\GNDVI GNDVI;D:\\MODELO_1\\capas_salida\\RVI RVI;D:\\MODELO_1\\capas_salida\\GVI GVI;D:\\MODELO_1\\capas_salida\\NGRDI NGRDI;D:\\MODELO_1\\capas_salida\\RG RG;D:\\MODELO_1\\capas_salida\\TVI TVI;D:\\MODELO_1\\capas_salida\\TTVI TTVI;D:\\MODELO_1\\capas_salida\\NRVI NRVI;D:\\MODELO_1\\capas_salida\\NDWI11 NDWI11;D:\\MODELO_1\\capas_salida\\NDVI12 NDWI12", "NONE")

# Process: Extract Multi Values to Points (3)
arcpy.gp.ExtractMultiValuesToPoints_sa(cent_parcelas_shp__2_, "'D:\\MODELO_1\\capas_entrada\\BAND 1 (Sentinel-2, L2A, 60m).jp2' rB1_60m;'D:\\MODELO_1\\capas_entrada\\BAND 2 (Sentinel-2, L2A, 10m).jp2' rB2_10m;'D:\\MODELO_1\\capas_entrada\\BAND 3 (Sentinel-2, L2A, 10m).jp2' rB3_10m;'D:\\MODELO_1\\capas_entrada\\BAND 4 (Sentinel-2, L2A, 10m).jp2' rB4_10m;'D:\\MODELO_1\\capas_entrada\\BAND 5 (Sentinel-2, L2A, 20m).jp2' rB5_20m;'D:\\MODELO_1\\capas_entrada\\BAND 6 (Sentinel-2, L2A, 20m).jp2' rB6_20m;'D:\\MODELO_1\\capas_entrada\\BAND 7 (Sentinel-2, L2A, 20m).jp2' rB7_20m;'D:\\MODELO_1\\capas_entrada\\BAND 8 (Sentinel-2, L2A, 10m).jp2' rB8_10m;'D:\\MODELO_1\\capas_entrada\\BAND 8A (Sentinel-2, L2A, 20m).jp2' rB8A_20m;'D:\\MODELO_1\\capas_entrada\\BAND 9 (Sentinel-2, L2A, 60m).jp2' rB9_60m;'D:\\MODELO_1\\capas_entrada\\BAND 11 (Sentinel-2, L2A, 20m).jp2' rB11_20m;'D:\\MODELO_1\\capas_entrada\\BAND 12 (Sentinel-2, L2A, 20m).jp2' rB12_20m", "NONE")

# Process: Extract Multi Values to Points (6)
arcpy.gp.ExtractMultiValuesToPoints_sa(cent_parcelas_shp__3_, "D:\\MODELO_1\\capas_salida\\CSup\\CSup CSUP", "NONE")
