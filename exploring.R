#### Explore Titanic dataset ####

# Packages managemement

install.packages("dataQualityR")
install.packages("ggplot2")
install.packages("reshape2")
library(dataQualityR)
library(dplyr)
library(ggplot2)
library(reshape2)

# Set working directory
setwd("/Users/colinleverger/Downloads/titanic-ml/")

# Load data
missing.types <- c("NA", "")
train.data <- read.csv("data/train.csv",na.strings = missing.types)

# Rename some cols for beter comprehension
colnames(train.data)
colnames(train.data)[colnames(train.data)=="SibSp"]  <- "Siblings_Spouses"
colnames(train.data)[colnames(train.data)=="ParCh"]  <- "Parents_Children"
colnames(train.data)[colnames(train.data)=="Pclass"] <- "Passenger_Class"
colnames(train.data)

# DQR
checkDataQuality(train.data, out.file.num="DQR_cont", out.file.cat="DQR_cat")
dqr.cont <- read.csv("DQR_cont")
dqr.cat <- read.csv("DQR_cat")

typeof(train.data)

# What to learn from this dataset
survived <- train.data %>%
  group_by(Sex) %>%
  summarise(Survived=sum(Survived), Died=n()-sum(Survived))

# Exploring difference between men's and women's death
survived.long <- melt(survived,id.vars = "Sex")
ggplot(survived.long,aes(x=variable,y=value,fill=factor(Sex)))+
  geom_bar(stat="identity",position="dodge") +
  scale_fill_discrete(name="Gender")+
  xlab("People")+ylab("Population")
  