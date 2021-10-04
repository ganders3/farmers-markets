PACKAGES = c("dplyr", "ggplot2", "profvis", "scales", "stringr", "tidyr", "waffle")
lapply(PACKAGES, require, character.only = TRUE)

# Set-up ####
#profvis({
rm(list = ls())
# -------------------- controls------------------------------
DIRECTORY = "/home/gregory/farmers-markets/survey"
DATA_FOLDER = DIRECTORY
OUTPUT_FOLDER = "output"
DATA_FILE_INPUT = "survey-results-combined.csv"
DATA_KEY = "survey-key.csv"
FUNCTIONS_FILE = "functions-survey.R"
# Set true to save plots as png files, or false to print plots to screen
SAVE_PLOTS = TRUE
# -------------------- controls------------------------------
setwd(DIRECTORY)
source(FUNCTIONS_FILE)

# Get survey data ####
# Read in data file
d = read.csv(paste0(DATA_FOLDER, "/", DATA_FILE_INPUT), header = TRUE, stringsAsFactors = F)
# Read in key file
key = read.csv(paste0(DATA_FOLDER, "/", DATA_KEY), header = TRUE, stringsAsFactors = F)

# Assign ordinal values to "How often do you visit the FM?"
d$HO = factor(d$HO, order = TRUE, levels = c("First time visiting",
                                             "A few times a year", 
                                             "Every few months",
                                             "Monthly",
                                             "Weekly"))
d$D1 = factor(d$D1, order = TRUE, levels = c("Under 20", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80 or over"))

# Data queries ####

theme_set(theme_classic())
theme_update(text = element_text(size = 16, family = "Times New Roman"))

#### Query 1 - where do you get your food? ####
# Gather on FS1:FS8 to create 2 columns: the variable FS1-FS8, and its response. This gives the table 8x as many rows
# Join with the key to include the full question text
q1 = d %>% 
  gather(x, value, FS1:FS8) %>%
  group_by(x) %>%
  summarise(pctYes = 100*sum(value == "Yes")/n(),
            pctNo = 100*sum(value == "No")/n()) %>%
  inner_join(key, by = c("x" = "Var"))

p1a = ggplot(q1, aes(x = reorder(Desc, pctYes), y = pctYes)) +
  geom_col(fill = "#74abf1") +
  coord_flip() +
  xlab("Food source") +
  ylab("Percent of respondents marking \"yes\"") +
  ylim(0, 100) +
  geom_text(aes(y = pctYes+3, label = paste0(format(pctYes, digits = 1), "%")), colour = "black")

p1b = ggplot(q1, aes(x = reorder(Desc, -pctNo), y = pctNo)) +
  geom_col(fill = "#e0657a") +
  coord_flip() +
  xlab("Food source") +
  ylab("Percent of respondents NOT marking \"yes\"") +
  ylim(0, 100) +
  geom_text(aes(y = pctNo-2, label = paste0(format(pctNo, digits = 1), "%")), colour = "black")


#### Query 2 - how often do you visit the FM? ####
q2 = summarize100pct(d, "HO")

p2 = ggplot(q2, aes(x = x, y = pct)) +
  geom_col(fill = "#74abf1") +
  coord_flip() +
  xlab("Frequency of visit") +
  ylab("Percent of respondents") +
  geom_text(aes(y = pct + 2, label = lab))
  
#### Query 3 - which FM do you visit?

# Waffle plot
theme_waffle = theme(legend.position = "bottom",
                       axis.line = element_blank(),
                       axis.ticks = element_blank(),
                       axis.text = element_blank())

# Creating a data frame of the x and y coordinates of each respondent
q3 = waffle_iron(d, aes_d(group = WH), rows = 9)

# Creating waffle plot as an alternative to a pie chart
p3 = ggplot(q3, aes(x, y, fill = group)) +
  geom_waffle() +
  coord_equal() +
  theme_waffle +
  labs(fill = "") +
  ylab("") + xlab("Each square represents 1 survey response") +
  scale_fill_brewer(palette = "Set2")


#### Query 4 - why do you visit the FM? ####
q4 = d %>% 
  gather(x, value, W1:W5) %>%
  group_by(x) %>%
  summarise(pctYes = 100*sum(value == "Yes")/n(),
            pctNo = 100*sum(value == "No")/n()) %>%
  inner_join(key, by = c("x" = "Var"))

p4a = ggplot(q4, aes(x = reorder(Desc, pctYes), y = pctYes)) +
  geom_col(fill = "#74abf1") +
  coord_flip() +
  xlab("Reason for visiting FM") +
  ylab("Percent of respondents marking \"yes\"") +
  ylim(0, 100) +
  geom_text(aes(y = pctYes+3, label = paste0(format(pctYes, digits = 2), "%")), colour = "black")

p4b = ggplot(q4, aes(x = reorder(Desc, -pctNo), y = pctNo)) +
  geom_col(fill = "#e0657a") +
  coord_flip() +
  xlab("Reason for visiting FM") +
  ylab("Percent of respondents NOT marking \"yes\"") +
  ylim(0, 100) +
  geom_text(aes(y = pctNo+3, label = paste0(format(pctNo, digits = 2), "%")), colour = "black")

#### Query 5 - Percent of purchases at FM before and during pandemic ####
q5 = d %>%
  select(P1, P2) %>%
  mutate(P1 = round5(P1),
         P2 = round5(P2)) %>%
  table() %>%
  data.frame()

p5 = ggplot(q5, aes(x = P1, y = P2, fill = Freq)) +
  geom_tile(color = "white", lwd = 1.5) +
  coord_fixed() +
  scale_fill_gradient(low = "white", high = "#74abf1") +
  xlab("Percent before pandemic") +
  ylab("Percent during pandemic")

#### Query 6 - Did these factors make you more or less likely to visit the FM? ####
# Gather on F1:F5 to create 2 columns: the variable F1-F5, and its response. This gives the table 5x as many rows
# Join with the key to include the full question text
q6 = d %>%
  gather(x, y, F1:F5) %>%
  select(x, y) %>%
  inner_join(key, by = c("x" = "Var")) %>%
  select(x = Desc, y) %>%
  filter(y != "")

# Create a table of frequencies for each combination of x and y
q6 = table(q6$y, q6$x) %>%
  data.frame()
# Updat the column names to be more clear
colnames(q6) = c("Effect", "Factor", "Freq")
q6 = q6 %>%
  group_by(Factor) %>%
  mutate(Pct = 100*Freq/sum(Freq)) %>%
  arrange(Factor, desc(Effect)) %>%
  mutate(Position = cumsum(Pct) - 0.5*Pct)

p6 = ggplot(q6, aes(x = Factor, y = Pct, fill = Effect)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Set2", direction = -1) +
  geom_text(aes(y = Position, label = paste0(format(Pct, digits = 2), "%")), color = "black") +
  ylab("Percent of respondents")

#### Query 7 - Did you visit the market more or less during the pandemic? ####
q7 = waffle_iron(d, aes_d(group = VML), rows = 9)

p7 = ggplot(q7, aes(x, y, fill = group)) +
  geom_waffle() +
  coord_equal() +
  theme_waffle +
  labs(fill = "") +
  ylab("") + xlab("Each square represents 1 survey response") +
  scale_fill_brewer(palette = "Set2", direction = -1)

#### Query 8 - Demographics ####
q8a = summarize100pct(d, "D1")

p8a = ggplot(q8a, aes(x = x, y = pct)) +
  geom_col(fill = "#74abf1") +
  xlab("Frequency of visit") +
  ylab("Percent of respondents") +
  geom_text(aes(y = pct + 0.25, label = lab))

q8b = d %>%
  filter(D2 != "") %>%
  waffle_iron(aes_d(group = D2), rows = 9)

p8b = ggplot(q8b, aes(x, y, fill = group)) +
  geom_waffle() +
  coord_equal() +
  theme_waffle +
  labs(fill = "") +
  ylab("") + xlab("Each square represents 1 survey response") +
  scale_fill_brewer(palette = "Set2", direction = -1)


q8c = simplePie(d, "D3")

# Only zip codes with >= 5% of responses
q8c1 = q8c %>%
  select(x,y) %>%
  arrange(desc(y)) %>%
  filter(y >= 5)

pie(q8c1$y, q8c1$x)

#})
plot_names = c("p1a", "p1b", "p2", "p3", "p4a", "p4b", "p5", "p6", "p7", "p8a", "p8b")
for (name in plot_names) {
  if (SAVE_PLOTS) {
    ggsave(paste0(OUTPUT_FOLDER, "/", name, ".png"), plot = get(name), width = 12, height = 8.5, units = "in")
  }
}
