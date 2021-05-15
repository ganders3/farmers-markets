plotFarms = function(gg, d, title, savePlots, pngName, w, h) {
  
  if (savePlots) {
    png(pngName, width = w, height = h)
  }
  
  out = gg +
    # Plot data points, colored by category
    geom_point(data = d, mapping = aes(x = lon, y = lat, color = category, size = 3)) +
    geom_text(data = d, aes(x = lon, y = lat, label = farm)) + 
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