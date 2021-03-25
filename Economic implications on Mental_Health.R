#------------------------
#Loading dataset packages
#------------------------

library(datasets)
library(ggplot2)     
library(dplyr)
library(data.table)
library(reshape2)
library(tidyr)
library(tidyverse)
library(ggpubr)

#Retrieving data
#Mental health for the year 2020
Mhealth <- read.csv("2020r.csv")
View(Mhealth)

#Data of countries from 1970-2017
Countries <-read.csv("countries of the world.csv")
View(Countries)
class(Mhealth)

summary(Mhealth)
summary(Countries)

head(Mhealth)
head(Countries)

#Data processing
sum(is.na(Countries))
#Replacing NA values by 0
Countries$Country[is.na(Countries$Country)]<-0
Countries$Literacy....[is.na(Countries$Literacy....)]<-0
Countries$GDP....per.capita.[is.na(Countries$GDP....per.capita.)]<-0
#conversion of values to numeric data type
GDP_percapita<-as.numeric(Countries$GDP....per.capita.)
GDP_percapita

#gsub function to convert to numeric type to plot a scatter plot
Literacy<-gsub(",", "", Countries$Literacy....) 
Literacy_data<-as.numeric(Literacy)
Literacy_data
plot(Literacy_data,GDP_percapita)


#----------------------------------------
#Retaining and renaming required columns
#----------------------------------------

Country_data<- Countries[-c(3,4,6,7,11,13:20)]
colnames(Country_data)[3]<-"Population_density"
colnames(Country_data)[4]<-"Infant_mortality"
colnames(Country_data)[5]<-"GDP_"
colnames(Country_data)[6]<-"Literacy"
colnames(Country_data)[7]<-"Arable"
View(Country_data)

Mental_health_data <- Mhealth[-c(4,5,6,11,13:19)]
Mental_DataF<-Mental_health_data
colnames(Mental_DataF)[1]<-"Country"
colnames(Mental_DataF)[4]<-"Log_GDP"
colnames(Mental_DataF)[7]<-"Freedom_of_life_choice"
colnames(Mental_DataF)[9]<-"Dystopia"
View(Mental_DataF)

#--------------------
#Data visualization
#--------------------

p <- ggplot(Mental_DataF, aes(x = Log_GDP, y = Ladder.score)) + 
  geom_point()
p
a<-ggplot(data = Mental_DataF, mapping = aes(x = Ladder.score, y = Social.support)) +
  geom_point(alpha = 0.6, aes(color = Regional.indicator))
a
b<-ggplot(data = Mental_DataF, mapping = aes(x = Ladder.score, y = Freedom_of_life_choice)) +
  geom_point(alpha = 0.6, aes(color = Regional.indicator))
b
c<-ggplot(data = Mental_DataF, mapping = aes(x = Social.support, y = Log_GDP)) +
  geom_point(alpha = 0.6, aes(color = Regional.indicator))
c

#Connection between two dataframe
max.len = max(length(Mental_DataF$Ladder.score), length(Literacy_data))
Ladder_score = c(Mental_DataF$Ladder.score, rep(NA, max.len - length(Mental_DataF$Ladder.score)))
#Literacy from Countries
Literacy_Countries = c(Literacy_data, rep(NA, max.len - length(Literacy_data)))
plot(Ladder_score,Literacy_Countries)

dim(Country_data)
dim(Mental_DataF)

#---------------
#Regression
#---------------

theme_set(theme_pubr())
ggplot(data=Mental_DataF,aes(x = Ladder.score, y = Log_GDP)) +
  geom_point() +
  stat_smooth()

cor(Mental_DataF$Ladder.score, Mental_DataF$Log_GDP)
model <- lm(Log_GDP ~ Ladder.score, data = Mental_DataF)
model

#Log_GDP=4.71+0.8376 * Ladder.score

ggplot(Mental_DataF, aes(Ladder.score, Log_GDP)) +
  geom_point() +
  stat_smooth(method = lm)
summary(model)

#There is approximately a 95% chance that the interval [0.7279, 0.9473] will contain the true value of b1
confint(model)

#Percentage error (prediction error)
sigma(model)*100/mean(Mental_DataF$Log_GDP)

#-------------------------------
#Predicted Log_GDP of Finland
#--------------------------------
Predicted_result<- 4.71+0.8376*7.8087
Predicted_result
actual_Log_GDP<-Mental_DataF[1,4]
actual_Log_GDP
