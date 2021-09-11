PACKAGES = c('dplyr', 'ggplot2', 'scales', 'stringr', 'tidyr')
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

png(file = paste0(OUTPUT_FOLDER, '/', 'q1-food-sources-yes.png'), height = 960, width = 960)
ggplot(q1, aes(x = Desc, y = pctYes)) +
  geom_col() +
  coord_flip()
dev.off()

png(file = paste0(OUTPUT_FOLDER, '/', 'q1-food-sources-no.png'), height = 960, width = 960)
ggplot(q1, aes(x = Desc, y = pctNo)) +
  geom_col() +
  coord_flip()
dev.off()
# 
# barplot(q1$pctYes, names.arg = q1$Desc, ylim = c(0,100))
# barplot(q1$pctNo, names.arg = q1$Desc, ylim = c(0,100))

#### Query 2 - how often do yu visit the FM? ####
png(file = paste0(OUTPUT_FOLDER, '/', 'q2-how-often.png'), height = 960, width = 960)
q2 = simplePie(d, 'HO')
dev.off()

#### Query 3 - which FM do you visit?
png(file = paste0(OUTPUT_FOLDER, '/', 'q3-which-fm.png'), height = 960, width = 960)
q3 = simplePie(d, 'WH')
dev.off()

#### Query 4 - why do you visit the FM? ####
q4 = d %>% 
  gather(x, value, W1:W5) %>%
  group_by(x) %>%
  summarise(pctYes = 100*sum(value == 'Yes')/n(),
            pctNo = 100*sum(value == 'No')/n()) %>%
  inner_join(key, by = c("x" = "Var"))

png(file = paste0(OUTPUT_FOLDER, '/', 'q4-why-yes.png'), height = 960, width = 960)
barplot(q4$pctYes, names.arg = q4$Desc, ylim = c(0,100))
dev.off()

png(file = paste0(OUTPUT_FOLDER, '/', 'q4-why-no.png'), height = 960, width = 960)
barplot(q4$pctNo, names.arg = q4$Desc, ylim = c(0,100))
dev.off()

#### Query 5 - Percent of purchases at FM before and during pandemic ####
q5 = d %>%
  select(P1, P2) %>%
  mutate(P1 = round5(P1),
         P2 = round5(P2)) %>%
  table() %>%
  data.frame()

png(file = paste0(OUTPUT_FOLDER, '/', 'q5-percent-before-during.png'), height = 960, width = 960)
ggplot(q5, aes(x = P1, y = P2, fill = Freq)) +
  geom_tile(color = 'white', lwd = 1.5) +
  coord_fixed() + 
  scale_fill_gradient(low = 'white', high = 'darkgreen')
dev.off()
#### Query 6 - Did these factors make you more or less likely to visit the FM? ####
q6 = d %>%
  gather(x, y, F1:F5) %>%
  inner_join(key, by = c('x' = 'Var')) %>%
  select(x = Desc, y) %>%
  filter(y != '')

q6 = table(q6$y, q6$x) %>%
  data.frame()

colnames(q6) = c('Effect', 'Factor', 'Freq')

q6 = q6 %>%
  group_by(Factor) %>%
  mutate(Pct = Freq/sum(Freq)) %>%
  arrange(Factor, desc(Effect)) %>%
  mutate(Position = cumsum(Pct) - 0.5*Pct)

png(file = paste0(OUTPUT_FOLDER, '/', 'q6-factors.png'), height = 960, width = 960)
ggplot(q6, aes(x = Factor, y = Pct, fill = Effect)) +
  geom_col() +
  scale_fill_manual(values=c('#f1f1f1', '#de425b', '#488f31')) +
  theme_classic() +
  geom_text(aes(y = Position, label = Pct), colour = 'black') +
  coord_flip()
  # + geom_text(aes(y = label_y, label = Weight), colour = "white")
dev.off()

#### Query 7 - Did you visit the market more or less during the pandemic? ####
png(file = paste0(OUTPUT_FOLDER, '/', 'q7-visit-more-less.png'), height = 960, width = 960)
q7 = simplePie(d, 'VML')
dev.off()

#### Query 8 - Demographics ####
png(file = paste0(OUTPUT_FOLDER, '/', 'q8-age.png'), height = 960, width = 960)
q8a = simplePie(d, 'D1')
dev.off()

png(file = paste0(OUTPUT_FOLDER, '/', 'q8-gender.png'), height = 960, width = 960)
q8b = simplePie(d, 'D2')
dev.off()

png(file = paste0(OUTPUT_FOLDER, '/', 'q8-zip-codes.png'), height = 960, width = 960)
q8c = simplePie(d, 'D3')
dev.off()

# Only zip codes with >= 5% of responses
q8c1 = q8c %>%
  select(x,y) %>%
  arrange(desc(y)) %>%
  filter(y >= 5)

png(file = paste0(OUTPUT_FOLDER, '/', 'q8-zip-codes-top5.png'), height = 960, width = 960)
pie(q8c1$y, q8c1$x)
dev.off()
# Zip codes in and out of WA state
# q8c2 = q8c %>%
#   summarise(inState =(x))
# Give the chart file a name
# png(file = "barchart_months_revenue.png")


# Save the file
# dev.off()
