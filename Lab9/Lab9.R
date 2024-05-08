##########################################################
# Project: Lab 9
# Purpose: Practice data import
# Author: Aileen Gonzalez
# Edit date: Nov 10 2023
# Data: feelings towards country music
##########################################################
#Set Working Directory Using menu option Session --> Set Working Directory
#install Tidyverse using menu option Tools --> Install packages

#Save the dataset we’ve provided (from Canvas) as a .csv file (it is currently an xlsx file)
#Open R and create a new R file for the R Script for this assignment
#Firstly, in your script create a project Prolog for this assignment

#Install the tidyverse, dplyr, and janitor packages and read them in
library(package = "tidyverse")
library(package="janitor")
library(dplyr)

### STUDENTS DOING LAB 9: SELECT JUST ONE WAY TO BRING IN YOUR DATA AND 
### EDIT THIS SCRIPT TO ONLY INCLUDE THE CODE FOR THE ONE YOU USE!!!!

setwd("/Users/aigonz/Desktop/QMSS201/Lab9") #OR use the menu above Session, set working directory

#Import the .csv file into R as a new object called music.data
music.data <- read.csv("Fall 23 Lab 9 data.csv", header=T)

#Look at the file by writing the object name
music.data

#Look at the summary info on the file for each variable, noting which data type each variable was assigned by the R program
summary(music.data)

#Take a ‘glimpse’ of the data
glimpse(music.data)

#Install the  read in so we can clean up data frame names and other characteristics
library(janitor)

# Clean the data as follows using tidyverse and its piping function and renaming the new cleaned dataset as musicdata.clean
#Rename the variables as Gender, Country, Hours, Platform. Hometown and
#Concert can remain with their names as Hometown and Concert.
#Note that two variables should be factors (Gender & Platform), two should be numerics (Country and Hours), and one should be a character (Hometown).
#Recode the datatype for any variable that was incorrectly assigned. (Recall that either ‘int’ or ‘dbl’ are numeric types so a numeric variable assigned these types not need to be reassigned.)
#Label the values in the variable Gender to Male and Female instead of 1 and 2
#Set the value ‘Not Sure’ as NA for the variable Platform
#Select only the variables Gender, Country, Hours, Platform, and Concert to be included in the clean dataframe (not Hometown)

musicdata.clean <- music.data %>%
  rename(Gender = Sex, Country = Country.Music, Hours=Hours.per.day.listening.to.Music, Platform=Favorite.Music.Streaming.Platform)%>%
  mutate(Gender = as.factor(x = Gender))%>% 
  mutate(Platform = as.factor(x = Platform))%>% 
  mutate(Gender= recode(.x = Gender, "1" = "Male", "2" = "Female"))%>%
  mutate(Platform = na_if(Platform, "Not Sure"))%>%
  select(Gender, Country, Hours, Platform, Concert)   

#Look at the summary of the cleaned data. Note the variable types and that Hometown is no longer included
summary (musicdata.clean)
glimpse(musicdata.clean)

#Using Base R, further cleaning will be applied to the new reduced datafile. Base R is the basic functionality that comes with the R programming language. tidyverse adds onto the functionality by simplifying approaches of Base R. Using base R we must call the dataframe a variable is in each time that a variable is listed whereas we can call the dataframe once in tidyverse and continue using it throughout our functions.

# Change Concert to be a factor instead of a character
musicdata.clean$Concert <- as.factor(musicdata.clean$Concert) 

# Relabel Concert so that y is now yes and n is now no
musicdata.clean$Concert <- recode(musicdata.clean$Concert,"y" = "yes", "n"="no") 

#Look at the summary of the cleaned data again. Note the variable types and check that Concert has been modified as you expected
summary (musicdata.clean)
glimpse(musicdata.clean)

#Use the following code to save your reduced dataframe as a csv. Save it as MusicLASTNAME.csv Mine would be MusicWhitaker.csv (It should go into your working directory. Obviously, you will need to change the file names)
write.csv(musicdata.clean,"MusicGonzalez.csv", row.names
            = FALSE)
  