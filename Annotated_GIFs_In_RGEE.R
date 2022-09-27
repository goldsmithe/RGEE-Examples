#27/09/2022 - How to create and annotate a GIF of satellite imagery using the rgee package!

#author: Emily Goldsmith

#Sections:
  #A - Data set-up
  #B - Set up your parameters for the GIF
  #C - Create a loop to extract the image dates from the filtered dataset
  #D - Create and annotate your GIF!!!

#Assuming you've successfully set up rgee and initalised....

#============================================================================================================
#A - Data set-up
#0 - Load in your packages
library(sf) #To load in shapefile data
library(raster) #To create a bounding box 
library(mapview) #To see if said bounding box shapefile is in correct geographical area
library(magick) #To create the GIF

#1 - Load in and prepare your vector data
Yanomami <- st_read("E:\\2.5_GIS_Coding_Workspace\\2.R\\GEE_In_R\\Data\\Yanomami_Subset.shp") #Replace with your own

Bounding_Box <- as(raster::extent(Yanomami), "SpatialPolygons") #Create a bounding box of your shapefile
proj4string(Bounding_Box) <- "+proj=longlat +datum=WGS84 +no_defs" #Set your desired projection (CRS)
mapview(Bounding_Box) #Have a peek at it in mapview to make sure its where it should be

Yanomami_sf <- st_as_sf(Bounding_Box) #Convert your spatial polygons object to sf object
mask <- sf_as_ee(Yanomami_sf) #Convert your sf object to an ee object
region <- mask$geometry()$bounds() #This is your region

#2 - Load in the image collection
col<-ee$ImageCollection('projects/planet-nicfi/assets/basemaps/americas') #We've chosen NICFI Americas for this one

#============================================================================================================
#B - Set up your parameters for the GIF
distinctDOY <- col$filterDate('2016-06-01', '2022-07-01') #Temporal range

#Define a filter which identifies which images match the DOY
filter <- ee$Filter$equals(leftField = 'doy', rightField = 'doy')

#Define a join; convert the resulting FeatureCollection to an ImageCollection.
join <- ee$Join$saveAll('doy_matches')
joinCol <- ee$ImageCollection(join$apply(distinctDOY, col, filter))

#Create RGB visualisation parameters for the NICFI imagery
VisParameters <- list(bands = c("R", "G", "B"),
                      min = 64 ,
                      max = 1000,
                      gamma = c(1,1,1))

#Create RGB visualization images for use as animation frames
rgbVis <- joinCol$map(function(img) {
  do.call(img$visualize, VisParameters) %>% 
    ee$Image$clip(mask)
})

#Define GIF visualization parameters
gifParams <- list(
  region = region,
  dimensions = 1000,
  framesPerSecond = 5
)

#============================================================================================================
#C - Create a loop to extract the image dates from the filtered dataset
dateList <- list() #Create an empty list to hold your dates

#Now create the loops
for (i in 1:length(joinCol)){ #For the length of the collection minus 1 (because index values start from 0, not 1, in GEE)
  time = ee_get_date_img(ee$Image(col$toList(col$size())$get(i))) #Get the date of the image at image index ID, i (iterate through image indices of i and store this as a variable)
  date = time$time_start #Get the date string only
  dateList[i] = as.character(date) #Create a list which includes the date per image
}

dateAnnotations <- as.character(dateList) #Convert your dates list to a character string so that it is compatible with the ee_utils_gif_annotate() function 

#============================================================================================================
#D - Create and annotate your GIF!!!
#Use the ee_utils_gif_creator function to render the GIF animation 
animation <- ee_utils_gif_creator(rgbVis, gifParams, mode = "wb")

#Now use the ee_utils_gif_annotate function to add annotations to the GIF animation
animation %>% 
  ee_utils_gif_annotate(
    text = dateAnnotations, #Annotate with your dates!!!
    size = 25, color = "white",
    location = "+10+10"
  )

#To save your GIF: right click and copy the address to your browser and then save to PC!
