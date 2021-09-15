PACKAGES = c('dplyr', 'ggplot2', 'RColorBrewer', 'scales', 'stringr', 'tidyr', 'tm', 'wordcloud', 'wordcloud2')
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

makeWordCloud(d$LM)
makeWordCloud(d$LL)
makeWordCloud(d$LC)
