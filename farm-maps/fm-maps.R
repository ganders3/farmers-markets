library(dplyr)
library(ggmap)
library(ggplot2)
library(ggimage)
library(mapdata)
library(maps)
library(sp)
library(scales)

# Set-up ####
rm(list = ls())
# -------------------- controls------------------------------
DIRECTORY = '/home/gregory/farmers-markets/farm-maps'
OUTPUT_FOLDER = 'maps'
GEOCODED_DATA_FILE = 'farms-geocoded.csv'
FUNCTIONS_FILE = 'functions.R'
LAT_LON_RATIO = 1
# Set true to save plots as png files, or false to print plots to screen
SAVE_PLOTS = FALSE
# Starting image height
IMG_HEIGHT = 551
# Determine width based on aspect ratio
IMG_RATIO = 1.51
# Scale the image to be larger (value > 1) or smaller (value < 1) 
IMG_RESIZE = 1.5
# -------------------- controls------------------------------
setwd(DIRECTORY)
source(FUNCTIONS_FILE)
imgWidth = IMG_HEIGHT*IMG_RATIO*IMG_RESIZE
imgHeight = IMG_HEIGHT*IMG_RESIZE

# Get coordinates of farm addresses ####
d = read.csv('farms.csv', header = TRUE)

if (file.exists(GEOCODED_DATA_FILE)) {
  print(paste('Geocoded file found. Opening', GEOCODED_DATA_FILE))
  d1 = read.csv(GEOCODED_DATA_FILE, header = TRUE)
} else {
  print('No geocoded file found. Geocoding addresses using Google Maps API...')
  # Geocode the addresses using Google Maps API
  d1 = mutate_geocode(d, location = address, output = 'latlona')
  # Save the geocoded data to csv
  write.csv(d1, 'farms-geocoded.csv')
}

# Display the entries that did not return anything from Google Maps
failed = d1[which(is.na(d1$lon)), ]

# pallete = hue_pal()(length(unique(d1$category)))
# pallete = rainbow(length(unique(d1$category)))
# names(pallete) = unique(d1$category)
pallete = c(
  'farm' = '#059877',
  'bakery' = '#FF7D54',
  'crafter' = '#A903E1',
  'prepared food' = '#CB211F',
  'seafood' = '#0BA7E5'
  # 'seafood' = '#163ED3',
)
# Create map of Washington ####
m = map_data('state', region = 'Washington')
# Plot the map of Washington
ggW = ggplot() +
  geom_polygon(data = m, aes(x = long, y = lat, group = group), fill = 'white', color = 'black') +
  coord_fixed(LAT_LON_RATIO)
# Plot the map of Puget Sound region
ggPS = ggplot() +
  geom_polygon(data = m, aes(x = long, y = lat, group = group), fill = 'white', color = 'black') +
  coord_fixed(LAT_LON_RATIO, xlim = c(-123.25, -121.75), ylim = c(47, 48.25))
  
# Add farm locations to map
for (yr in 2019:2021) {
  
  # Filter for data which were present in each year
  d2 = d1 %>%
    filter(get(paste0('present', yr)) == 1 )
  
  titleWash = paste0('Ballard Farmers Market Attendance, Week 16 of ', yr)
  titlePuget = paste0('Ballard Farmers Market Attendance, Puget Sound Region, Week 16 of ', yr)
  
  pngWash = paste0(OUTPUT_FOLDER, '/dot-map-', yr, '.png')
  pngPuget = paste0(OUTPUT_FOLDER, '/dot-map-puget-', yr, '.png')
  
  # Need to wrap this line in print() in order to output ggplots within a for loop
  plotFarms(ggW, d2, titleWash, SAVE_PLOTS, pngWash, imgWidth, imgHeight)
  plotFarms(ggPS, d2, titlePuget, SAVE_PLOTS, pngPuget, imgWidth, imgHeight)
}