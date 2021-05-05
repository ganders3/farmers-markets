library(dplyr)
library(ggmap)
library(ggplot2)
library(mapdata)
library(maps)

# Set-up ####
rm(list = ls())
# -------------------- controls------------------------------
DIRECTORY = '/home/gregory/farmers-markets/farm-maps'
RATIO = 1.4
API_KEY = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxx'
# -------------------- controls------------------------------
setwd(DIRECTORY)

# Get coordinates of farm addresses ####
d = read.csv('farms.csv', header = TRUE)
# colnames(d) = c('Name', 'Address', 'Category')

# Geocode the addresses using Google Maps API
d1 = mutate_geocode(d, location = address, output = 'latlona')

# Display the entries that did not return anything from Google Maps
failed = d1[which(is.na(d1$lon)), ]

# Save the geocoded data to csv
write.csv(d1, 'farms-geocoded.csv')

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
gg + geom_point(data  = d1, aes(x = lon, y = lat), color = 'red', size = 2)