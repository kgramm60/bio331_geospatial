#Spatial Data for R
#git repository 
https://github.com/kgramm60/bio331_geospatial

mkdir newrepo
cd newrepo

mkdir tmp #For stuff you do NOT want to commit to Git (e.g., climate data)
mkdir data # for data you do want to keep
mkdir R # for code (script files/notes)
mkdir figures # for images and results files
echo "BIO331 Course Repository for Spatial Bioinformatics Unit" > README.md

#in console
library(raster)

#download worldclim data for world at 5 arcminute resolution
clim = getData('worldclim', var='bio', res=5, path="tmp") #path will put in tmp folder we created
clim #shows class, dimensions, resolution, extent, crs (projection), names (use website to get actual names), min values, and max values

#define extent object in eastern US
ext = extent(-74, -69, 40, 45) #l to r: xmin, xmax, ymin, ymax 0 degrees go thru UK (x=longitude, y=latitude)
#crop
c2 = crop(clim, ext)
plot(c2[[1]]) #basic plotting, legend shows us average temp (# on graph/10 degrees C= avg temp); one white pixel is mt washington


library(ggplot2)

#converts raster to dataframe (single raster into 3 dataframes)
c2_df = as.data.frame(c2, xy = TRUE)
head(c2_df)

#ggplot to make a better graph
ggplot() +
  geom_raster(data = c2_df, aes(x = x, y = y, fill = bio1)) +
  coord_quickmap() #coord_quickmap takes projection into account

#sets this all = to base so in future we can just type base to apply these settings and then add anything else we want (see below)
base = ggplot() +
  geom_raster(data = c2_df, aes(x = x, y = y, fill = bio1/10)) +
  coord_quickmap() +

#modifying base
base + theme_bw() 

#test colors
base + 
  theme_bw() +
  scale_fill_gradientn(colours=c('navy', 'white', 'darkred'),
                            na.value = "black")

#viridis
library(viridis)

base + 
  theme_bw() +
  scale_fill_gradientn(colours=viridis(10), na.value = ("black")

#distribution of data in rasters
ggplot() +
  geom_histogram(data = c2_df, aes(x = bio1))

#homework use bio12 instead of bio1 and make graph to find where stonehill's campus is and say whether campus gets more or less rain than NYC

ggplot() +
  geom_raster(data = c2_df, aes(x = x, y = y, fill = bio12)) +
  coord_quickmap() +
  theme_bw()

