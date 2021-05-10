library(dplyr)
library(ggmap)
library(ggplot2)
library(ggimage)
library(mapdata)
library(maps)
library(sp)

# Set-up ####
rm(list = ls())
# -------------------- controls------------------------------
DIRECTORY = '/home/gregory/farmers-markets/farm-maps'
ICONS_FOLDER = 'icons'
LAT_LON_RATIO = 1.4
API_KEY = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxx'
GEOCODED_DATA_FILE = 'farms-geocoded.csv'

# Starting image height
IMG_HEIGHT = 551
# Determine width based on aspect ratio
IMG_RATIO = 1.51
# Scale the image to be larger (value > 1) or smaller (value < 1) 
IMG_RESIZE = 1.5
# -------------------- controls------------------------------
setwd(DIRECTORY)

# Get coordinates of farm addresses ####
d = read.csv('farms.csv', header = TRUE)
# colnames(d) = c('Name', 'Address', 'Category')


if (file.exists(GEOCODED_DATA_FILE)) {
  print(paste('Geocoded file found. Opening', GEOCODED_DATA_FILE))
  d1 = read.csv(GEOCODED_DATA_FILE, header = TRUE)
} else {
  print('No geocoded file found. Geocoding addresses using Google Maps API...')
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
  coord_fixed(LAT_LON_RATIO)

# Add farm locations to map
for (yr in 2019:2021) {
  d2 = d1 %>%
    filter(get(paste0('present', yr)) == 1) %>%
    mutate(image_file = if_else(category == "",
                                paste0(DIRECTORY, "/", ICONS_FOLDER, "/fruit veggie.png"),
                                paste0(DIRECTORY, "/", ICONS_FOLDER, "/", category, ".png")))
  
  pngTitle = paste0('market-map-', yr, '.png')
  # Create a blank png file
  png(pngTitle, width = IMG_HEIGHT*IMG_RATIO*IMG_RESIZE, height = IMG_HEIGHT*IMG_RESIZE)
  # Need to wrap this line in print() in order to output ggplots within a for loop
  print(gg +
          geom_image(data = d2, mapping = aes(x = lon, y = lat, image = image_file), size = 0.05) +
          labs(title = paste('Farm attendance on week 16,', yr)))
  print(paste('Saving', pngTitle, '...'))
  # Save the png file containing the plot in the line above
  dev.off()
}
# d2 = d2 %>% mutate(image_file = if_else(category == "",
#                                         paste0(DIRECTORY, "/", ICONS_FOLDER, "/fruit veggie.png"),
#                                         paste0(DIRECTORY, "/", ICONS_FOLDER, "/", category, ".png")))

########
# dataToPlot = dataToPlot %>% mutate(img = rep("https://jeroenooms.github.io/images/frink.png", 82))


# gg + geom_image(data = d2, mapping = aes(x = lon, y = lat, image = image_file), size = 0.05)