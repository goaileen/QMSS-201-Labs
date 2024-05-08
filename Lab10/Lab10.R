# Set working directory
setwd("/Users/aigonz/Desktop/QMSS201/Lab10")

library(dplyr)

#Open Dataset in R as and save it as lyrics_data
lyrics_data <- read.csv('Fall23 Lab 10 Assignment Data.csv')

#Use summary and glimpse (you will need dplyr) to assess variable types
summary(lyrics_data)
glimpse(lyrics_data)

# Use dplyr to clean the data: Transform some variables variables and selecting some of them.
# Clean so that Condition, Gender, Year, LyricsOnly are factors. Recode them with labels. Clean so that pieces is numeric (if needed). Save dataset with only Condition, Pieces, Sex, Year, LyricsOnly
# Phase 1: Data Transformation
cleaned_lyrics_data <- lyrics_data %>%
        mutate(Condition = as.factor(Condition),
               Condition = recode(Year, "1" = "Complete Song", "2" = "Instrumental Only", "3" = "Audio Recording of Lyrics", "4" = "Nothing"),
               Gender = as.factor(Gender),
               Gender = recode(Gender, "1" = "M", "2" = "F"),
               Year = as.factor(Year),
               Year = recode(Year, "1" = "Freshman", "2" = "Sophomore", "3" = "Junior", "4" = "Senior"),
               LyricsOnly = as.factor(LyricsOnly),
               LyricsOnly = recode(LyricsOnly, "1" = "Heard Lyrics", "2" = "Heard no Lyrics"))

# Phase 2: Variable Selection
cleaned_lyrics_data1 <- cleaned_lyrics_data %>%
        select(Condition, Pieces, Gender, Year, LyricsOnly)

# The selected_data dataframe now contains only the chosen variables.

#Create a new variable called Percent that reports the percent of pieces placed correctly out of 25 (not proportion, but %) Check that it is a numeric. [ (pieces/25)*100]
cleaned_lyrics_data1$Percent <- (cleaned_lyrics_data1$Pieces/25)*100

# Taking a look at the summary of our newly created variable
summary(cleaned_lyrics_data1$Percent)

#see how R read the cleaned data and whether data type is modified
#Look for the new variable
summary (cleaned_lyrics_data1)
glimpse(cleaned_lyrics_data1)

#GETTING BASIC UNIVARIATES FOR ALL VARS
#Run mean, median, variance, sd, and range for Pieces and Percent
mean(cleaned_lyrics_data1$Pieces, na.rm = TRUE)
median(cleaned_lyrics_data1$Pieces, na.rm = TRUE)
var(cleaned_lyrics_data1$Pieces, na.rm = TRUE)
sd(cleaned_lyrics_data1$Pieces, na.rm = TRUE)
range(cleaned_lyrics_data1$Pieces, na.rm = TRUE)
mean(cleaned_lyrics_data1$Percent, na.rm = TRUE)
median(cleaned_lyrics_data1$Percent, na.rm = TRUE)
var(cleaned_lyrics_data1$Percent, na.rm = TRUE)
sd(cleaned_lyrics_data1$Percent, na.rm = TRUE)
range(cleaned_lyrics_data1$Percent, na.rm = TRUE)

#Run freq distributions for Condition, Gender, Year, LyricsOnly
install.packages('descr')
library(descr)

freq(x = cleaned_lyrics_data1$Condition, plot = FALSE)
freq(x = cleaned_lyrics_data1$Gender, plot = FALSE)
freq(x = cleaned_lyrics_data1$Year, plot = FALSE)
freq(x = cleaned_lyrics_data1$LyricsOnly, plot = FALSE)

#Run means and Std. Dev. by for Pieces by Condition. Then run ANOVA and Interpret
cleaned_lyrics_data1 %>%
  group_by(Condition) %>%
  summarise(Pieces = mean(Pieces))

cleaned_lyrics_data1 %>%
  group_by(Condition) %>%
  summarise(Pieces = sd(Pieces))

# Compute the analysis of variance looking at Pieces for each of the 4 music conditions
Condition_Pieces_aov <- aov(Pieces ~ Condition, data = cleaned_lyrics_data1)
# Summary of the analysis
summary(Condition_Pieces_aov)
#Interpretation: The mean Pieces per Condition were 10.9 for Audio Recording, 11.5 for Complete Song, 10.8 for Instrumental Only, and 11 for Nothing. The standard deviations for these conditions are 2.67, 2.28, 2.93, and 3.03 respectively. It is indicated that there is no significant difference in the number of pieces (task performance) among the four music conditions as the f value is 0.134 with 3 and 36 degrees of freedom for condition and residuals while the p-value associated is 0.939.

#Run means and Std. Dev. of Pieces by LyricsOnly, run t-test. Interpret
tapply(cleaned_lyrics_data1$Pieces, cleaned_lyrics_data1$LyricsOnly, mean)
tapply(cleaned_lyrics_data1$Pieces, cleaned_lyrics_data1$LyricsOnly, sd)
ttest_lyricsonly_pieces <- t.test(formula = cleaned_lyrics_data1$Pieces ~
                                    cleaned_lyrics_data1$LyricsOnly)
ttest_lyricsonly_pieces
#Interpretation: The mean number of pieces by LyricsOnly for songs with lyrics was 11.55 (SD = 2.818641) while for songs without lyrics it was 10.65 (SD = 2.254236). The t-stat was 1.1152 with 36.249 degrees of freedom with a resulting p-value of 0.2721. Based on this information, there is not enough evidence to conclude there is a significant difference in the mean number of pieces between songs with or without lyrics so the determination is that absence of lyrics in songs does not appear to have a significant impact on the number of pieces (task performance).

#Run crosstab with for Sex by Condition
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(cleaned_lyrics_data1, row.vars = "Gender", col.vars = "Condition", type = "c")
#Run a crosstab for Year by Condition
crosstab(cleaned_lyrics_data1, row.vars = "Year", col.vars = "Condition", type = "c")

#If you want to save the file
write.csv(cleaned_lyrics_data1,"Cleaned_Lyrics_data1_lab9.csv", row.names = FALSE)
