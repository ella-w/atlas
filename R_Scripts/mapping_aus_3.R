## --- Load librarys --- ##
library(dplyr)
require(fasterize)
require(geosphere)
library(ggplot2)
#library(magicfor)
require(raster)
library(rgl)
library(rgdal)
library(rgeos)
library(plot3D)
library(sf)
require(sp)
library(tidyverse)

## --------------------- Hey look at these, some fancy functions ----------------------- ##
#' @param new.device a logical value. If TRUE, creates a new device
#' @param bg the background color of the device
#' @param width the width of the device
rgl_init <- function(new.device = FALSE, bg = "white", width = 640) { 
  if( new.device | rgl.cur() == 0 ) {
    rgl.open()
    par3d(windowRect = 50 + c( 0, 0, width, width ) )
    rgl.bg(color = bg )
  }
  rgl.clear(type = c("shapes", "bboxdeco"))
  rgl.viewpoint(theta = 15, phi = 20, zoom = 0.7)
}

#' Get colors for the different levels of 
#' a factor variable
#' 
#' @param groups a factor variable containing the groups
#'  of observations
#' @param colors a vector containing the names of 
#   the default colors to be used
get_colors <- function(groups, group.col = palette()){
  groups <- as.factor(groups)
  ngrps <- length(levels(groups))
  if(ngrps > length(group.col)) 
    group.col <- rep(group.col, ngrps)
  color <- group.col[as.numeric(groups)]
  names(color) <- as.vector(groups)
  return(color)
}

# This is my poor attempt to make a function that makes a gradient scale
# from specified values 
# rgl_init()
# rgl.points(ausx, ausy, ausz, color = map_colour(ausz), size = 2)
# (hint i didnt work)
map_colour <- function(values, values.col = palette()){
  values <- as.factor(values)
  ngrps <- length(levels(values))
  if(ngrps > length(values.col)) 
    values.col <- rep(values.col, ngrps)
  color <- values.col[as.numeric(values)]
  names(color) <- as.vector(values)
  return(color)
}

get.colors <- function(n, start = 15, end = 375, light = 65, chroma = 100){
  
  hues <- seq(start, end, length = n+1)
  
  hcl(h = hues, l = light, c = chroma)[1:n]
  
}

## ------------------------------------------------------------------------------------ ##
## Lets plot some stuff ##

# Set the working directory 
setwd("C:/Users/wilsone6/OneDrive - Queensland University of Technology/Documents/Cancer Atlas")

# Open the required data sets
simdata <- read.csv("Simulated Data for SA2s.csv")
mesh <- read.csv("p_mesh.csv")
# "." means to read from current directory
shpAus <- readOGR( dsn = ".", layer = "SA2_2016_AUST")

# Seperate the x,y,z coordinates for the projection
ausx <- mesh[,2]
ausy <- mesh[,3]
ausz <- mesh[,4]


# This is the basic plotting of the shape, a single colour
rgl.open()
rgl.bg(color = "white")
rgl.points(ausx, ausy, ausz, color = "blue", size = 2)
# rgl.clear()

# Colour scale!
col <- get.colors(50, 10, 200, chroma = 250)
col

# Here I have cut the z factor into 50 levels, seperating them by number
z <- cut(ausz, 50)
# plotted the shape, coping and pasting the hexadecimal and then putting in commas!
# ( may need to experiment and try to get a different scaling of colors so it is easier to see)
rgl_init()
rgl.points(ausx, ausy, ausz, 
           color = get_colors(z, c("#FF0000", "#FF0000", "#FF0000", "#FF2A00", "#FF3F00", "#FF4E00", "#FF5900", "#FF6300", "#FF6C00", "#FF7400", "#FF7B00", "#FF8100",
                                   "#F98700", "#F18C00", "#E89200", "#E09600", "#D69B00", "#CC9F00", "#C2A300", "#B7A700", "#ABAB00", "#9DAF00", "#8EB200", "#7DB600",
                                   "#67B900", "#4BBC00", "#08BF00", "#00C200", "#00C500", "#00C800", "#00CA00", "#00CD00", "#00D000", "#00D200", "#00D400", "#00D700",
                                   "#00D900", "#00DB00", "#00DD00", "#00DF2F", "#00E156", "#00E371", "#00E587", "#00E79A", "#00E8AC", "#00EABD", "#00EBCE", "#00ECDE",
                                   "#00ECED", "#00EDFD")), 
           size = 2)

# oKAY now we trying to translate the long-lat coordinates to the nearest SA2 centroid
# (need to be N to 1 mapping as we have alot of long-lat coordinates)

# plotting shpAus takes so loooong
# plot(shpAus)
coords <- cbind(ausx,ausy)
coords <- as.data.frame(coords)
coords <- SpatialPointsDataFrame(coords = coords,data = coords)
# coords <- SpatialPoints(coords = coords, proj4string = CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
proj4string(coords) <- CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")

# This code below is useless
#coords <- writeOGR(coords, dsn = ".", layer ="torn", driver = "ESRI Shapefile")

# use sp::over to find consistent spatial overlay of points
SA2 <- over(coords, shpAus)
#SA2 <- na.omit(SA2)


##  now lets muck about with rgl to see if we can map stuff!
rgl_init()
rgl.points(SA2, color = "red", size = 2) # wow what a nice looking square

# well, actually it's not a nice square, not meant to look like that at all
# Okay, lets try on a smaller scale, selecting only 3 IDs from the shapefile, and then map
# them all again (everything outside should be excluded)


# [ some shit goes here ]#
#
#shpAus_sf_cancer %>%
 # filter(SA3_CODE16 == "10105")

## raster stuff
# set up a raster "template" to use in fasterize()
r <- raster(ncol=180, nrow=180)
extent(r) <- extent(shpAus)
shpAus_sf <- st_as_sf(shpAus)
simdata$SA2 <- as.character(simdata$SA2)
shpAus_sf$SA2_5DIG16 <- as.character(shpAus_sf$SA2_5DIG16)
names(shpAus_sf)[2] <- "SA2"


shpAus_sf_cancer <- inner_join(shpAus_sf, simdata, by = ("SA2"))
# where the d ARN is western australia??
#plot(shpAus_sf_cancer)

shapeAus <- fasterize(shpAus_sf_cancer, r, 'SIR')
plot(shapeAus)

WA <- shpAus_sf_cancer %>%
  filter(STE_NAME16 == "Western Australia")
# looks like its just plotting perth ...
# plot(WA)

# Why is there stuff missing?
# View(anti_join(shpAus_sf, simdata, by = c("SA2_5DIG16" = "SA2")))



## Next thing to ding up ##
# Create a for loop that takes a point from the 80000 points that is the shortest distance to one of our
# points from 2153 points. Then map that to see if we can get western australia to appear. 
# dist <- spDistsN1(pts = coordinates, pt = origin, longlat = TRUE)

simx <- simdata$Long
simy <- simdata$Lat
#shpx <- coords$ausx
#shpy <- coords$ausy

#simcoords <- cbind(simx, simy) #  2153 rows
#auscoords <- cbind(shpx, shpy) # 82074 rows

# who knows how long this is going to take (long time probably)
# for (i in simdata$Long){
#   for (j in simdata$Lat){
#     pt <- cbind(i, j)
#     mapping <- spDistsN1(coords, pt, TRUE)
#   }
# }

#plot(shpAus)

polygon <- shpAus@polygons[[5]]@Polygons[[1]]@coords
polygonx <- polygon[,1]
polygony <- polygon[,2]
polypoint <- point.in.polygon(coords$ausx, coords$ausy, polygonx, polygony)
plot(polypoint)
# now using the abve garbage, right a loop that changes the number of polygons

# magic_for()
# for (i in 1:2292){
#   polygon <- shpAus@polygons[[i]]@Polygons[[1]]@coords
#   polygonx <- polygon[,1]
#   polygony <- polygon[,2]
#   confirm <- point.in.polygon(coords$ausx, coords$ausy, polygonx, polygony)
#   put(confirm)
# }
# polyyy <- magic_result_as_dataframe()

coords2 <- coords
coords2$ID <- 1:82074


greg <- NULL
  
for (i in 1:2292){
  polygon <- shpAus@polygons[[i]]@Polygons[[1]]@coords
  polygonx <- polygon[,1]
  polygony <- polygon[,2]
  confirm <- point.in.polygon(coords2$ausx, coords2$ausy, polygonx, polygony)
  ID <- coords2$ID[which(confirm == 1)]
  if (length(ID) != 0){
    greg <- rbind(greg, data.frame(ID,i))
  }
}

plotting <- merge(greg, coords2)
plotting <- plotting[,-(5:6), drop=FALSE]

# wrote this csv so i didnt have run the gotdamned for loop again. 
write.csv(plotting, "Polygon_data_2.csv")

##    ---    Lets plots some more     ---     ##
# Going to try plotting in ggplot and rgl 
rgl_init()
rgl.points(plotting$ausx, plotting$ausy, ausz, 
           color = get_colors(plotting$i, c("#FF0000", "#FF0000", "#FF0000", "#FF2A00", "#FF3F00", "#FF4E00", "#FF5900", "#FF6300", "#FF6C00", "#FF7400", "#FF7B00", "#FF8100",
                                   "#F98700", "#F18C00", "#E89200", "#E09600", "#D69B00", "#CC9F00", "#C2A300", "#B7A700", "#ABAB00", "#9DAF00", "#8EB200", "#7DB600",
                                   "#67B900", "#4BBC00", "#08BF00", "#00C200", "#00C500", "#00C800", "#00CA00", "#00CD00", "#00D000", "#00D200", "#00D400", "#00D700",
                                   "#00D900", "#00DB00", "#00DD00", "#00DF2F", "#00E156", "#00E371", "#00E587", "#00E79A", "#00E8AC", "#00EABD", "#00EBCE", "#00ECDE",
                                   "#00ECED", "#00EDFD")), 
           size = 2)

ggplot(data = shpAus_sf_cancer, aes( x = Long, y = Lat)) + geom_polygon(aes(fill = SIR))

map.df <- fortify(shpAus)     # where map is the shapefile object

croc <- match(shpAus$SA2_5DIG16, simdata$SA2)
simdata2 = simdata[croc,]

# Append data
Append <- data.frame(
  id = unique(map.df$id),
  my.var = simdata2$SIR
  )

dat <- inner_join(map.df, Append, by = 'id')

h# Plot

ggplot(data = dat, aes(x = long, y = lat, group = group)) +
         geom_polygon(aes(fill = my.var))
       
       
  
