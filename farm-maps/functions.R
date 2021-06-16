plotFarms = function(gg, d, title, savePlots, pngName, w, h) {
  
  if (savePlots) {
    png(pngName, width = w, height = h)
  }
  
  out = gg +
    # Plot data points, colored by category
    geom_point(data = d, mapping = aes(x = lon, y = lat, color = category), size = 3) +
    coord_map(projection = "mercator", xlim = c(-123.25, -121.75), ylim = c(47, 48.25))+
  
    # geom_text(data = d, aes(x = lon, y = lat, label = farm)) + 
    # Change the point color to reflect the pallete defined above. limits sets which legend categories should appear
    scale_color_manual(values = pallete, limits = names(pallete)) +
    # Add a plot title
    ggtitle(title) +
    # Adjust the title to be centered
    theme(plot.title = element_text(hjust = 0.5)) +
    # Change the x and y coordinate labels
    xlab('Longitude') + ylab('Latitude')
    
    print(out)
  
  if (savePlots) {
    print(paste('Saving', pngName, '...'))
    dev.off()
  }
  
  return(out)
}


plotFarmsAsIcons = function(gg, d, title, savePlots, pngName, w, h, img) {
  if (savePlots) {
    png(pngName, width = w, height = h)
  }
  
  out = gg +
    # Plot data points, colored by category
    geom_image(data = d, mapping = aes(x = lon, y = lat, image = image_file), size = 0.025) +
    # Change the point color to reflect the pallete defined above. limits sets which legend categories should appear
    # scale_color_manual(values = pallete, limits = names(pallete)) +
    # Add a plot title
    ggtitle(title) +
    # Adjust the title to be centered
    theme(plot.title = element_text(hjust = 0.5)) +
    # Change the x and y coordinate labels
    xlab('Longitude') + ylab('Latitude')
  print(out)
  
  if (savePlots) {
    print(paste('Saving', pngName, '...'))
    dev.off()
  }
  
  return(out)
}

# Makes a vector of unique variable names
makeUniqueNames = function(names) {
  names = names %>%
    trimws() %>%
    # Replace multiple .'s with only a single .
    str_replace('\\.+', '.') %>%
    # Remove .'s at the end of the variable name
    str_replace('\\.+[0-9]*$', '') %>%
    make.names(unique = T)
  return(names)
}