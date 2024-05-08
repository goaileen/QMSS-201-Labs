###########################################
# Project: Week 11 Lab 
# Purpose: Cleaning + linear Regression in R
# Author: Aileen Gonzalez
# Edit date: Nov 20, 2023
# Data: Michigan State of the State Fall 2019 Survey 
###########################################


# CLEAN DATA -------------------------------------------------------

# set working directory so that R knows which folder your file is in

# read in dataset and save in object 'data'
data<- read.csv('Lab 11 and 12 Data(1).csv', header=TRUE)

# install 'car' package to get ready to recode variables
# USE THIS COMMAND FOR THE FIRST TIME: 
install.packages('car') # run this only once

# then load the package using library()
library(car)

# use recode() leader.bio to change '1' and '2' to Male and Female
  # first have to "create" a new variable in data (gender), just by adding it on 'data'
data$gender<- recode(data$CD1, "1='Male'; 2='Female'")
  # because 'Female' and 'Male' are characters, have to also put them in single quotes
  # if they are numbers (1, 2, 3), don't have to

# use table() to get counts
table(data$CD1)
table(data$gender)

# check the data type of CD1 and new "gender" variable using class()
class(data$CD1)
class(data$gender)

# How did recoding CD1 to gender change the data type, and why?
#Response: Recoding CD1 to gender changed the data type from an integer to a character. This is because numbers (1, 2, 3) do not need single quotes while characters do which was indicated in the code "data$gender<- recode(data$CD1, "1='Male'; 2='Female'")" surrounding Male and Female, ultimately changing our data types as desired.

#Use prop.table() to find the proportion of respondents by gender
table1<- table(data$gender) # first save the table into an object
table1 # this is the same as when we ran the initial table command above

prop1<- prop.table(table1) # then run prop.table() on the object
prop1

#Use summary() to find summary statistics for age of respondents
summary(data$age)

#Write 2-3 sentences summarizing what you see in part (a) and (b)
#Response: In part(a), the prop.table function reveals that approximately 46.63% of respondents are female while ~53.37% are male. In part(b), the summary statistics reveal that respondent ages range from 18 to 94 with the median age being 57 and the mean age being 54.22. The interquartile range spans from 40 to 69 years old. Both parts suggest that the survey/dataset includes a diverse and balanced group of respondents in both gender and age.

# REGRESSION -------------------------------------------------------------
# Run a linear regression to predict someone’s beliefs about the race-wage gap based on someone’s age and gender.
# dependent variable = inclusion6
# independent variables = age + gender
# store it in a new object called “lm”.
# Run summary() on your linear regression
lm<- lm(inclusion6 ~ age + gender, data=data)
summary(lm)

# Is the model statistically significant overall, and what does this tell us? Are the variables statistically significant? How do gender and age affect someone’s beliefs about what explains the race-wage gap? **Make sure to talk about the responses to inclusion6 in real terms (‘mostly discrimination’, ‘mostly job skills’,etc.), and not just in terms of the numbers (1-4).

#Response: The model containing the variables "age" and "gender" is statistically significant, so the model is useful for explaining someone's beliefs about what explains the race-wage gap (model p-value = 7.136e-09). An increase in age is associated with a response closer to 'mostly discrimination' regarding beliefs about what explains the race-wage gap, controlling for the respondent's gender (p = 6.51e07). Additionally, being male is also associated with a response closer to 'mostly discrimination' regarding beliefs about what explains the race-wage gap, controlling for the respondent's age (p = 6.25e-05)