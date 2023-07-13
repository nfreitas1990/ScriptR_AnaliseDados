# Script_ Google Earth Engine
# Natalia Freitas


# Requisitos (apenas na primeira vez) -------------------------------------

# 1. Conta no Google habilitada no Google Earth Engine
# 2. Python instalado no Pc - vamos usar para acessar a API do google engine
# 3. Instalar o ambiente virtual do python e as suas dependencias

# Ambiente Virtual
# rgee::ee_install(py_env = "rgee")

# Alternativa ao ee_install()
# unix_py_path = paste0(
#   "/home/tai-rocha/venv",
#   "rgee/bin/python3"
# )
#
# #
# ee_install_set_pyenv( py_path = "C:/Users/Fernando/AppData/Local/r-miniconda/envs/rgee", py_env = "rgee")

# rgee::ee_install want to store the environment variables: EARTHENGINE_PYTHON
# and EARTHENGINE_ENV in your .Renviron file to use the Python path:
#   C:\Users\Fernando\AppData\Local\r-miniconda\envs\rgee/python.exe in future sessions.
# reticulate::py_install(packages = 'earthengine-api', envname = 'C:/Users/Fernando/AppData/Local/r-miniconda/envs/rgee')


# Numpy - pacote para manipulação de objetos do tipo array, ex.:matrizes multi-dimensioais;
# ee - pacote para interagir com a API Python do GEE



# Informações -------------------------------------------------------------
ee$Authenticate()

# Remover dependencia INSTALADAS
# rgee::ee_clean_pyenv()

# update
rgee::ee_install_upgrade()

# email de acesso GEE
# souzanf.freitas@gmail.com
# senha: nfreitas1990

reticulate::py_config()


# Comandos Obrigatórios por seção -------------------------------
# Pacotes
library(rgee)

# Checar o funcionamento do Ambiente Virtual sempre que iniciar uma R/Rstudio session.
rgee::ee_check()

# Obrigatório ao iniciar uma nova R/Rstudio session para usar o rgee
# rgee::ee_Initialize()
rgee::ee_Initialize(user = 'souzanf.freitas@gmail.com')

# Trocar nome do user
# ee$data$createAssetHome("users/PUT_YOUR_NAME_HERE")

# Sintaxe rgee x GEE -----------------------------------------------------------

# Para acesssar funções nativas do R
# rgee::ee_

# Para acessar módulos (função phyton da API py do GEE)
# rgee::ee$


# Pontos ------------------------------------------------------------------
pol <- ee$Geometry()$Point(0, 45)


# Importação Dados ------------------------------------------------------

# Tipos de dados:
#> Image - The fundamental raster data type in Earth Engine.
#> ImageCollection - a stack or time-series of images
#> Geometry - the fundamental vector data type in Earth Engine
#> Feature - carrega informações (atributos)
#> Feature Collection - stack carrega informações (atributos)


# Fonte de Dados GEE
# Image GEE (Fonte- Catálago de dados do GEE https://developers.google.com/earth-engine/datasets/catalog/AU_GA_DEM_1SEC_v10_DEM-H)


# Acessar arquivos Image
img_elev = ee$Image("AU/GA/DEM_1SEC/v10/DEM-H")
img_hydro <- ee$Image("WWF/HydroSHEDS/03CONDEM")


# Acessar arquvio Image Collection
image_col = ee$imagecollection()


# Acessar arquvio FeatureCollection
img_hydro <- ee$FeatureCollection("WWF/HydroATLAS/v1/Basins/level12")
rgee::Map$addLayers(img_hydro)

# Informações -------------------------------------------------------------
# Vendo Informações sobre o conteudo
img_elev = ee$Image("AU/GA/DEM_1SEC/v10/DEM-H") #carregar
img_elev$getInfo()   # informação
ee_print(img_elev)   # informação
class(img_elev)

# opcao
hydro <- ee$FeatureCollection("WWF/HydroATLAS/v1/Basins/level12")
hydro$getInfo()


# Plots -------------------------------------------------------------------

# Mapas: Map$addLayer( )
# plota em branco precisa de parâmetros para visualização
rgee::Map$addLayer(img_elev)
rgee::Map$addLayer(img_hydro)
Map$addLayer(img_hydro, elevationVis, 'Elevation')

# Mapas: visualizar ee_imagecollection()
rgee::Map$addLayers("ADD_IMAGECOLLECTION AQUI")

#> Parametrizando um pouco mais...para uma boa visualizaçao
#> Definindo cores de acordo com valores mins e maxs.
#> O códigos das cores pode ser obtido nesse site https://www.color-hex.com/
#> opções de uso https://r-spatial.github.io/rgee/reference/Map.html

vizParams = list(
            min = -10.0,
            max = 1300.0,
            palette = c("00FFFF", "ff0000"))

rgee::Map$addLayer(img_elev, visParams = vizParams,  opacity = 0)


# Elevação: na Image Hydro: img_hydro tem dados de elevação
# Parametrizar
elevationVis = list(
  min= -50.0,
  max= 3000.0,
  palette = c("00FFFF", "ff0000")
  )

# Plotar
rgee::Map$addLayer(img_hydro, visParams = elevationVis)


# ZOOM na imagem
# setCenter(lon = 0, lat = 0, zoom = NULL): Centers the map view at the given
# coordinates with the given zoom level. If no zoom level is provided, it uses 1 by default.
# argumento zoom: The zoom level, from 1 to 24.

rgee::Map$setCenter(133.95, -24.69, 5)

#
# setZoom(zoom = NULL): Sets the zoom level of the map.

# centerObject(eeObject, zoom = NULL, maxError = ee$ErrorMargin(1)):
# Centers the map view on a given object. If no zoom level is provided,
# it will be predicted according to the bounds of the Earth Engine object
# specified.



# Exemplo 1: --------------------------------------------------------------
library(dplyr)

#----
#Limitações Políticas
paises <- ee$FeatureCollection("USDOS/LSIB/2017")
# Parametrização
styleParams = list(
                fillColor= 'b5ffb4',
                color= '00909F',
                width= 0.05)
# Map
rgee::Map$addLayer(paises,styleParams, opacity = 90)
#----






# Elevação
img_elevacao <- ee$Image("WWF/HydroSHEDS/03CONDEM")

# Parametrização
elevationVis = list(
  min= -50.0,
  max= 3000.0,
  palette = c("00FFFF", "ff0000")
)

# Zoom em Minas Gerais
rgee::Map$setCenter(-48.266560,-18.913664, 6)

# Mapa
rgee::Map$addLayer(img_hydro, visParams = elevationVis, opacity = 90)


# Rios
dataset = ee$FeatureCollection('WWF/HydroSHEDS/v1/FreeFlowingRivers')
 visParams = list(
  min= 1,
  max= 10,
  palette= c('08519c', '3182bd', '6baed6', 'bdd7e7', 'eff3ff'))
Map$addLayer(dataset, visParams, 'Free flowing rivers', opacity = )



# Imagem LandSat ----------------------------------------------------------

# Image LandSat
img <- ee$Image('LANDSAT/LC08/C01/T1/LC08_044034_20140318')

# Informações
ee_print(img)

# Load an image.
Map$addLayer(img)

# Procurar
ee$Image$Dataset$USGS_SRTMGL1_003 %>%
  ee_utils_dataset_display()

# Carregar ImageCollection
collection <- ee$ImageCollection('LANDSAT/LC08/C01/T1')
point <- ee$Geometry$Point(-118.12500, 63.52866)   # delimitar area
start <- ee$Date('2014-06-01')                     # data da imagem (inicial)
finish <- ee$Date('2014-10-01')                    # data da imagem (final)

# Filtro
filteredCollection <- ee$ImageCollection("LANDSAT/LC08/C01/T1") %>%
  ee$ImageCollection$filterDate(start, finish) %>%
  ee$ImageCollection$filterBounds(point) %>%
  ee$ImageCollection$sort('CLOUD_cOVER')
first <- filteredCollection$first()
first <- filteredCollection[[1]]

library(rgeeExtra)





# Clip --------------------------------------------------------------------

library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

# carregar imagem
image <- ee$Image("LANDSAT/LC8_L1T_TOA/LC80440342014077LGN00")

# carregar os limites
roi <- ee$Geometry$Point(list(-122.4481, 37.7599))$buffer(20000)

# recorte
clipped <- image$clip(roi)

# Informação
print(image$getInfo())
ee_print(img)


Map$setCenter(-122.464, 37.7630, 10)

vis <- list(
  bands = c("B5", "B4", "B3"),
  min = 0,
  max = 0.5,
  gamma = c(0.95, 1.1, 1)
)

Map$addLayer(image, vis, "Full Image", FALSE) +
  Map$addLayer(clipped, vis, "Clipped Image")


# ÁGUA --------------------------------------------------------------------

library(rgee)
# ee_reattach() # reattach ee as a reserved word

ee_Initialize()

###############################
# Asset List
###############################

gsw <- ee$Image("JRC/GSW1_1/GlobalSurfaceWater")
occurrence <- gsw$select("occurrence")

###############################
# Constants
###############################

VIS_OCCURRENCE <- list(
  min = 0,
  max = 100,
  palette = c("red", "blue")
)

VIS_WATER_MASK <- list(
  palette = c("white", "black")
)

###############################
# Calculations
###############################

# Create a water mask layer, and set the image mask so that non-water areas
# are opaque.
water_mask <- occurrence$gt(90)$selfMask()

###############################
# Initialize Map Location
###############################

# Uncomment one of the following statements to center the map.
# Map$setCenter(-90.162, 29.8597, 10)   # New Orleans, USA
# Map$setCenter(-114.9774, 31.9254, 10) # Mouth of the Colorado River, Mexico
# Map$setCenter(-111.1871, 37.0963, 11) # Lake Powell, USA
# Map$setCenter(149.412, -35.0789, 11)  # Lake George, Australia
# Map$setCenter(105.26, 11.2134, 9)     # Mekong River Basin, SouthEast Asia
# Map$setCenter(90.6743, 22.7382, 10)   # Meghna River, Bangladesh
# Map$setCenter(81.2714, 16.5079, 11)   # Godavari River Basin Irrigation Project, India
# Map$setCenter(14.7035, 52.0985, 12)   # River Oder, Germany & Poland
# Map$setCenter(-59.1696, -33.8111, 9)  # Buenos Aires, Argentina
Map$setCenter(-74.4557, -8.4289, 11)  # Ucayali River, Peru

###############################
# Map Layers
###############################
Map$addLayer(occurrence$updateMask(occurrence$divide(100)), VIS_OCCURRENCE, "Water Occurrence (1984-2018)") +
  Map$addLayer(water_mask, VIS_WATER_MASK, "90% occurrence water mask", FALSE)



# Extrair precipitação ----------------------------------------------------
library(sf)
library(ggplot2)
library(dplyr)
library(tidyverse)
nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate("2001-01-01", "2002-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("pr")) %>% # Select only precipitation bands
  ee$ImageCollection$toBands() %>% # from imagecollection to image
  ee$Image$rename(sprintf("PP_%02d",1:12)) # rename the bands of an image
ee_nc_rain <- ee_extract(x = terraclimate, y = nc["NAME"], sf = FALSE)


ee_nc_rain %>%
  pivot_longer(-NAME, names_to = "month", values_to = "pr") %>%
  mutate(month, month=gsub("PP_", "", month)) %>%
  ggplot(aes(x = month, y = pr, group = NAME, color = pr)) +
  geom_line(alpha = 0.4) +
  xlab("Month") +
  ylab("Precipitation (mm)") +
  theme_minimal()


# Create an NDVI-animation ------------------------------------------------
library(magick)
library(rgee)
library(sf)

# Define the regional bounds of animation frames and a mask to clip the NDVI data by.

mask <- system.file("shp/arequipa.shp", package = "rgee") %>%
  st_read(quiet = TRUE) %>%
  sf_as_ee()
region <- mask$geometry()$bounds()

# Retrieve the MODIS Terra Vegetation Indices 16-Day Global 1km dataset as an ee.ImageCollection and select the NDVI band.

col <- ee$ImageCollection('MODIS/006/MOD13A2')$select('NDVI')
