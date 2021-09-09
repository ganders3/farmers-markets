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

simplePie = function(dat, var) {
  numRow = nrow(dat)
  
  q = dat %>%
    select(x = var) %>%
    group_by(x) %>%
    summarise(y = 100*n()/numRow)
  
  pie(q$y, q$x)
  return(q)
}