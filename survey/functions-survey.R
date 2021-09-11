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

# Core wrapping function
wrap.it = function(x, len)
{ 
  sapply(x, function(y) paste(strwrap(y, len), 
                              collapse = "\n"), 
         USE.NAMES = FALSE)
}

# Call this function with a list or vector
wrap.labels = function(x, len)
{
  if (is.list(x))
  {
    lapply(x, wrap.it, len)
  } else {
    wrap.it(x, len)
  }
}

# Round number ending in 5 or more to next 10
round5 = function(x) {
  mod = x %% 10
  ifelse(mod >= 5, x + 10- mod, x - mod)
}