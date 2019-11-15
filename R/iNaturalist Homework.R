#iNaturalist homework assignment: red fox

library(spocc)
library(mapr)
library(ggplot2)
library(raster)

spdist <- occ(query='Vulpes vulpes', from='gbif')
spdist$gbif$data
head(spdist$gbif$data)

#convert to data frame
spdist_df = occ2df(spdist)

#to get interactive map
map_leaflet(spdist)
df = as.data.frame(occ2df(spdist$gbif))

#load world clim data
wc = getData('worldclim', var='bio', res = 5, path='tmp')

ext = extent(-125, -55, 20, 60)
wc = crop(wc, ext)

wc_df = as.data.frame(wc, xy=TRUE)
sp_df = occ2df(spdist)

#graph
ggplot() +
  geom_raster(data = wc_df, aes(x = x, y = y, fill = bio1/10)) +
  geom_point(data=sp_ex, aes(x=longitude, y=latitude), col='green') +
  coord_quickmap() +
  theme_bw() + 
  scale_fill_gradientn(colours=c('navy', 'white', 'darkred'),
                       na.value = "black")

#extract
extr = extract(wc, sp_df[,c('longitude', 'latitude')])

sp_ex = cbind(df[,c('name', 
                    'longitude', 
                    'latitude',
                    'stateProvince', 
                    'year', 
                    'occurrenceID')], 
              extr)

sp_ex = na.omit(sp_ex)
head(sp_ex)

ggplot() +
  geom_raster(data = wc_df, aes(x = x, y = y, fill = bio1/10)) +
  geom_point(data=sp_ex, aes(x=longitude, y=latitude), col='green') +
  coord_quickmap() +
  theme_bw() + 
  scale_fill_gradientn(colours=c('navy', 'white', 'darkred'),
                       na.value = "black")
