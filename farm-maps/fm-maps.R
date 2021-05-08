library(dplyr)
library(ggmap)
library(ggplot2)
library(mapdata)
library(maps)
library(sp)

# Set-up ####
rm(list = ls())
# -------------------- controls------------------------------
DIRECTORY = '/home/gregory/farmers-markets/farm-maps'
RATIO = 1.4
API_KEY = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxx'
GEOCODED_DATA_FILE = 'farms-geocoded.csv'
# -------------------- controls------------------------------
setwd(DIRECTORY)

# Get coordinates of farm addresses ####
d = read.csv('farms.csv', header = TRUE)
# colnames(d) = c('Name', 'Address', 'Category')


if (file.exists(GEOCODED_DATA_FILE)) {
  print('Geocoded file found.')
  d1 = read.csv(GEOCODED_DATA_FILE, header = TRUE)
} else {
  print('No file found. Geocoding addresses using Google Maps API...')
  # Geocode the addresses using Google Maps API
  d1 = mutate_geocode(d, location = address, output = 'latlona')
  write.csv(d1, 'farms-geocoded.csv')
}

# Display the entries that did not return anything from Google Maps
failed = d1[which(is.na(d1$lon)), ]

# Save the geocoded data to csv

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
for (yr in 2019:2021) {
  present = d1 %>% select(paste0('present', yr))
  dataToPlot = d1[which(present == 1), ]
  ## Need to wrap this line in print() in order to output ggplots within a for loop
  print(gg + 
          geom_point(data  = dataToPlot, aes(x = lon, y = lat), color = 'red', size = 1) +
          labs(title = print(yr)))
}
