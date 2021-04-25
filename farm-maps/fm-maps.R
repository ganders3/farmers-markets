library(dplyr)
library(ggmap)
library(ggplot2)
library(mapdata)
library(maps)

# Set-up ####
rm(list = ls())
# -------------------- controls------------------------------
DIRECTORY = 'C:/Users/Gregory/Desktop/farm-maps'
RATIO = 1.4
API_KEY = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxx'
# -------------------- controls------------------------------
setwd(DIRECTORY)

# Get coordinates of farm addresses ####
d = read.csv('farms.csv', header = FALSE)
colnames(d) = c('Name', 'Address')

d1 = mutate_geocode(d, location = Address, output = 'latlona')

# Create map of Washington ####
m = map_data('state', region = 'Washington')

gg = ggplot() + 
  geom_polygon(
    data = m,
    aes(x = long, y = lat, group = group),
    fill = 'white',
    color = 'black'
    ) +
  coord_fixed(RATIO)

# Add farm locations to map
gg + geom_point(data  = d1, aes(x = lon, y = lat), color = 'red', size = 5)