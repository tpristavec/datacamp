
# TidyTuesday Week 7

# Loading packages
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(scales)

# Reading in data
data <- read_csv(url("https://github.com/fivethirtyeight/data/raw/master/star-wars-survey/StarWars.csv"))

# Preparing data

# Removing subtitle row
data <- data[-1, ]

# Renaming variables
names(data)[1] <- "id"
names(data)[2] <- "seen_movie"
names(data)[3] <- "fan"
names(data)[22] <- "vader"
names(data)[34] <- "gender"
names(data)[35] <- "age"
names(data)[36] <- "hhincome"
names(data)[37] <- "educ"
names(data)[38] <- "loc"

# Selecting columns
mydata <- subset(data, select=c("id", "seen_movie", "fan", "vader", "gender", "age",
   "hhincome", "educ", "loc"))

# Checking and removing NAs (today is not the day for multiple imputation:)
table(mydata$seen_movie, useNA = "always")
table(mydata$fan, useNA = "always")
table(mydata$vader, useNA = "always")
table(mydata$gender, useNA = "always")
table(mydata$age, useNA = "always")
table(mydata$hhincome, useNA = "always")
table(mydata$educ, useNA = "always")
table(mydata$loc, useNA = "always")

mydata$vader <- na_if(mydata$vader, "Unfamiliar (N/A)")

# How many complete cases?
mydata$countna <- as.vector(rowSums(is.na(mydata[, c("fan", "vader", "gender", "age", "hhincome", "educ", "loc")])))

colSums(is.na(mydata[, c("fan", "vader", "gender", "age", "hhincome", "educ", "loc")]))

table(mydata$countna)
table(mydata$countna, data$fan, useNA = "always")
table(mydata$countna, data$vader, useNA = "always")
table(mydata$countna, data$gender, useNA = "always")
table(mydata$countna, data$age, useNA = "always")
table(mydata$countna, data$hhincome, useNA = "always")
table(mydata$countna, data$educ, useNA = "always")
table(mydata$countna, data$loc, useNA = "always")

# Converting characters to factors
sapply(mydata, class)
convert <- c("seen_movie", "fan", "vader", "gender", "age", "hhincome", "educ", "loc")
mydata[, convert] = lapply(mydata[, convert], as.factor)
sapply(mydata, class)
sapply(mydata, levels)

# Reordering vactor levels
mydata$age <- factor(mydata$age, levels = c("18-29", "30-44", "45-60", "> 60"))
mydata$hhincome <- factor(mydata$hhincome, levels = c("$0 - $24,999", "$25,000 - $49,999", 
  "$50,000 - $99,999",  "$100,000 - $149,999"))
mydata$educ <- factor(mydata$educ, levels = c("Less than high school degree", "High school degree",
   "Some college or Associate degree", "Bachelor degree", "Graduate degree"))
mydata$loc <- factor(mydata$educ, levels = c("New England", "Middle Atlantic", 
   "East North Central", "West North Central", "South Atlantic", "East South Central", 
   "West South Central", "Mountain", "Pacific"))
mydata$vader <- factor(mydata$vader, levels = c("Very unfavorably", 
  "Somewhat unfavorably", "Neither favorably nor unfavorably (neutral)", "Somewhat favorably",
  "Very favorably"))

# Converting to ordered factors
mydata$age <- ordered(mydata$age)
mydata$hhincome <- ordered(mydata$hhincome)
mydata$educ <- ordered(mydata$educ)
mydata$vader <- ordered(mydata$vader)

# Recoding variables
mydata$vaderfan <- mydata$vader
levels(mydata$vaderfan) <- list(Dislike = c("Somewhat unfavorably", "Very unfavorably"), 
                           Neutral = "Neither favorably nor unfavorably (neutral)",
                           Like = c("Somewhat favorably", "Very favorably"))
table(mydata$vader, mydata$vaderfan)

# Sanity check
sapply(mydata, levels)
sapply(mydata, class)

# Darth Vader's fan base

# View of Darth Vader by gender
plot_gender <- ggplot(na.omit(subset(mydata, select = c(vaderfan, gender))), aes(x = vaderfan, fill = gender)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format())  + 
  labs(title = "How favorable do men and women find Darth Vader?", 
       subtitle = "Males tend to like him a bit more.", x = "View of Darth Vader", 
       y = "Percentage (%)", fill = "Gender")
plot_gender

# Fan
plot_fan <- ggplot(na.omit(subset(mydata, select = c(vaderfan, fan))), aes(x = vaderfan, fill = fan)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format())  + 
  labs(title = "How favorable do people with different opinions of Star Wars find Darth Vader?", 
       subtitle = "Those who are not Star Wars fans are somewhat less likely to have a favourable opinion of Darth Vader.", x = "View of Darth Vader", 
       y = "Percentage (%)", fill = "Star Wars fan")
plot_fan
