# install.packages("RColorBrewer")
# install.packages("GWmodel")
# install.packages("readxl")
# install.packages("sf")

library(RColorBrewer)
library(GWmodel)
library(readxl)
library(sf)
library(maps)
library(colorspace)

# Set working directory
setwd("C:/Users/semva/OneDrive/Documenten/Study/Seminar/Reproductie")
set.seed(815147)

# Load data
data <- read_excel("main_dataset.xlsx", sheet = 1)
Wijken <- st_read("./shapefile/wijkbuurtkaart_2023/wijken_2023_v1.shp")

# For wijken
Wijkcodes <- c("WK059901", "WK059903", "WK059904", "WK059905", "WK059906", "WK059906",
               "WK059908", "WK059910", "WK059912", "WK059914", "WK059915")
Wijken <- Wijken[Wijken$WK_CODE %in% Wijkcodes,]
Wijken <- Wijken[,c("WK_CODE", "geometry")] 
#### YOU CAN CHANGE WK_CODE TO A DIFFERENT COLUMN NAME IF YOU WANT TO

coordinates(data) <- ~longitude + latitude
Wijken <- as(Wijken, "Spatial")
Wijken <- spTransform(Wijken, CRS("+init=epsg:4326"))
proj4string(data) <- CRS("+init=epsg:4326")
data <- spTransform(data, CRS("+init=epsg:4326"))

data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
Wijken_sf <- st_as_sf(Wijken)
Wijken_sf <- st_set_crs(Wijken_sf, 4326)

# Few examples:
# pch = 19: filled circle
# pch = 1 : empty circle
# pch = 2 : empty triangle up


##### CHOOSE THE VARIABLE IN DATASF THAT YOU WANT TO PLOT 
plot(Wijken_sf, reset = FALSE, main = "Distance to Stadhuis")
plot(data_sf['distance_stadhuis'], add = TRUE, pch = 19)

#### OR PLOT WITH ONLY GEOMETRY
xlim <- range(c(data_sf$geometry$x, st_coordinates(Wijken_sf$geometry)[,1]))
ylim <- range(c(data_sf$geometry$y, st_coordinates(Wijken_sf$geometry)[,2]))
plot(data_sf['price'], reset = FALSE, pch = 19, 
     xlim = xlim, ylim = ylim, main = "House Age")
plot(Wijken_sf$geometry, add = TRUE)


#### TO OBTAIN LEGEND FOR FIRST PLOT
plot(data_sf['distance_stadhuis'])

######################## LEAFLET ########################
library(leaflet)
library(sf)

world <- st_read(system.file("shape/nc.shp", package="sf"))

rotterdam <- world[world$CNTY_ID == 37015, ]

color_palette <- colorNumeric(
  palette = brewer.pal(9, "Greens"),
  domain = c(min(local_R_squared), max(local_R_squared))
)

# Add local_R_squared to data_sf
data_sf$local_R_squared <- local_R_squared

#### CHANGE PARAMETERS TO YOUR LIKING
leaflet() %>%
  addTiles() %>%
  setView(lng = 4.47917, lat = 51.9225, zoom = 12) %>%
  addPolygons(data = rotterdam) %>%
  addCircleMarkers(data = data_sf,
                   radius = 3,
                   color = ~color_palette(local_R_squared),
                   #### CHANGE ABOVE LINE TO SELECT A FUNCTION 
                   stroke = FALSE,
                   fillOpacity = 0.9) %>%
  addLegend(pal = color_palette, values = data_sf$local_R_squared, title = "Local R-squared",
            position = "bottomright")
