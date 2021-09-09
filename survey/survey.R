PACKAGES = c('dplyr', 'ggplot2', 'stringr', 'tidyr')
lapply(PACKAGES, require, character.only = TRUE)

# Set-up ####
rm(list = ls())
# -------------------- controls------------------------------
DIRECTORY = '/home/gregory/farmers-markets/survey'
DATA_FOLDER = DIRECTORY
OUTPUT_FOLDER = 'output'
DATA_FILE_INPUT = 'survey-results-combined.csv'
DATA_KEY = 'survey-key.csv'
FUNCTIONS_FILE = 'functions-survey.R'
# Set true to save plots as png files, or false to print plots to screen
SAVE_PLOTS = TRUE
# -------------------- controls------------------------------
setwd(DIRECTORY)
source(FUNCTIONS_FILE)

# Get survey data ####
# Read in data file
d = read.csv(paste0(DATA_FOLDER, '/', DATA_FILE_INPUT), header = TRUE, stringsAsFactors = F)
# Read in key file
key = read.csv(paste0(DATA_FOLDER, '/', DATA_KEY), header = TRUE, stringsAsFactors = F)

# Data queries ####
nr = nrow(d)

#### Query 1 - where do you get your food? ####
q1 = d %>% 
  gather(x, value, FS1:FS8) %>%
  group_by(x) %>%
  summarise(pctYes = 100*sum(value == 'Yes')/n(),
            pctNo = 100*sum(value == 'No')/n()) %>%
  inner_join(key, by = c("x" = "Var"))

barplot(q1$pctYes, names.arg = q1$Desc, ylim = c(0,100))
barplot(q1$pctNo, names.arg = q1$Desc, ylim = c(0,100))

#### Query 2 - how often do yu visit the FM? ####
q2 = simplePie(d, 'HO')

#### Query 3 - which FM do you visit?
q3 = simplePie(d, 'WH')

#### Query 4 - why do you visit the FM? ####
q4 = d %>% 
  gather(x, value, W1:W5) %>%
  group_by(x) %>%
  summarise(pctYes = 100*sum(value == 'Yes')/n(),
            pctNo = 100*sum(value == 'No')/n()) %>%
  inner_join(key, by = c("x" = "Var"))

barplot(q4$pctYes, names.arg = q4$Desc, ylim = c(0,100))
barplot(q4$pctNo, names.arg = q4$Desc, ylim = c(0,100))

#### Query 6 - Did these factors make you more or less likely to visit the FM? ####
q6 = d %>%
  gather(x, value, F1:F5) %>%
  group_by(x) %>%
  summarise(more = 100*sum(value == 'More likely to visit')/n(),
            same = 100*sum(value == 'Had no effect')/n(),
            less = 100*sum(value == 'Less likely to visit')/n()) %>%
  inner_join(key, by = c('x' = 'Var'))
  

#### Query 7 - Did you visit the market more or less during the pandemic? ####
q7 = simplePie(d, 'VML')

#### Query 8 - Demographics ####
q8a = simplePie(d, 'D1')
q8b = simplePie(d, 'D2')
q8c = simplePie(d, 'D3')

# Only zip codes with >= 5% of responses
q8c1 = q8c %>%
  select(x,y) %>%
  arrange(desc(y)) %>%
  filter(y >= 5)
pie(q8c1$y, q8c1$x)

# Zip codes in and out of WA state
# q8c2 = q8c %>%
#   summarise(inState =(x))
# Give the chart file a name
# png(file = "barchart_months_revenue.png")


# Save the file
# dev.off()
