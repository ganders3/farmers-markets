PACKAGES = c('dplyr', 'ggplot2', 'stringr')
lapply(PACKAGES, require, character.only = TRUE)

# Set-up ####
rm(list = ls())
# -------------------- controls------------------------------
DIRECTORY = '/home/gregory/farmers-markets/survey'
DATA_FOLDER = DIRECTORY
OUTPUT_FOLDER = 'output'
DATA_FILE_INPUT = 'survey-results-combined.csv'
# DATA_FILE_OUTPUT ='farms-geocoded.csv'
FUNCTIONS_FILE = 'functions-survey.R'
# Set true to save plots as png files, or false to print plots to screen
SAVE_PLOTS = TRUE
# -------------------- controls------------------------------
setwd(DIRECTORY)
source(FUNCTIONS_FILE)

# Get survey data ####
d = read.csv(paste0(DATA_FOLDER, '/', DATA_FILE_INPUT), header = TRUE, stringsAsFactors = F)
yesNoCols = c("Food.Source.FM", "Food.Source.CSA", "Food.Source.Coop", "Food.Source.Restaurants",
              "Food.Source.Bank", "Food.Source.Grocery", "Food.Source.Online", "Food.Source.Other",
              "Why.Local.Business", "Why.Healthy.Food", "Why.Community","Why.Cannot.Get", "Why.Other")
d[yesNoCols][d[yesNoCols] == ''] = 'No'

d1 = d %>%
  select_if(grepl('Food.Source.', names(.))) %>%
  group_by(Food.Source.FM) %>%
  summarise(Percent=n())
# Create the data for the chart
H <- c(7,12,28,3,41)
M <- c("Mar","Apr","May","Jun","Jul")

# Give the chart file a name
# png(file = "barchart_months_revenue.png")

# Plot the bar chart 
barplot(H,names.arg=M,xlab="Month",ylab="Revenue",col="blue",
        main="Revenue chart",border="red")

# Save the file
# dev.off()
