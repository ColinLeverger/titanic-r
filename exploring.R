#### Explore Titanic dataset #####

#### General setups ####
# Packages managemement

install.packages("dataQualityR")
install.packages("ggplot2")
install.packages("vcd")
install.packages("reshape2")
library(dataQualityR)
library(dplyr)
library(ggplot2)
library(reshape2)
library(vcd)

# Set working directory
setwd("/Users/colinleverger/Downloads/titanic-ml/")

# Load data
missing.types <- c("NA", "")
train.data <- read.csv("data/train.csv", na.strings = missing.types)

# Rename some cols for beter comprehension
colnames(train.data)
colnames(train.data)[colnames(train.data) == "SibSp"]  <-
  "SiblingsSpouses"
colnames(train.data)[colnames(train.data) == "ParCh"]  <-
  "ParentsChildren"
colnames(train.data)[colnames(train.data) == "Pclass"] <-
  "PassengerClass"
colnames(train.data)

#### DQR ####
checkDataQuality(train.data, out.file.num = "DQR_cont", out.file.cat = "DQR_cat")
dqr.cont <- read.csv("DQR_cont")
dqr.cat  <- read.csv("DQR_cat")

typeof(train.data)

#### Exploring the data ####
survived <- train.data %>%
  group_by(Sex) %>%
  summarise(Survived = sum(Survived),
            Died = n() - sum(Survived))

# Exploring difference between men's and women's death
survived.long <- melt(survived, id.vars = "Sex")
ggplot(survived.long, aes(x = variable, y = value, fill = factor(Sex))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Gender") +
  xlab("People") + ylab("Population")

# Exploring survival rates
barplot(
  table(train.data$Survived),
  names.arg = c("Perished", "Survived"),
  main = "Survived",
  col = "black"
)

# Exploring passenger classes
barplot(table(train.data$PassengerClass),
        main = "Passenger Classes",
        col = "red")

# Exploring gender repartition
barplot(table(train.data$Sex),
        main = "Sex (gender)",
        col = "blue")

# Exploring age repartition
hist(train.data$Age,
     main = "Age",
     xlab = NULL,
     col = "brown")
d <- density(train.data[!is.na(train.data$Age), ]$Age)
plot(d, main = "Age density", xlab = NULL, col = "brown")

# Exploring fare paid by passengers
hist(train.data$Fare,
     main = "Fare",
     xlab = NULL,
     col = "red")

# Exploring Siblings and spouses repartition
barplot(table(train.data$SiblingsSpouses),
        main = "Siblings & Spouses",
        col = "orange")

# Exploring parents and kid repartition
barplot(table(train.data$Parch),
        main = "Parch (parents and kid)",
        col = "white")

# Exploring boarding location
barplot(
  table(train.data$Embarked),
  names.arg = c("Cherbourg", "Queenstown", "Southampton"),
  main = "Embarked",
  col = "yellow"
)

# Exploring passenger Fate by Traveling Class
mosaicplot(
  train.data$PassengerClass ~ train.data$Survived,
  main = "Passenger Fate by Traveling Class",
  shade = FALSE,
  color = TRUE,
  xlab = "Passenger Class",
  ylab = "Survived"
)

# Exploring passenger Fate by Embarked places
mosaicplot(
  train.data$Embarked ~ train.data$Survived,
  main = "Passenger Fate by Embarked places",
  shade = FALSE,
  color = TRUE,
  xlab = "Embarqued",
  ylab = "Survived"
)

#### Treating the data ####
# Computing median and mean
mean_age <- train.data[!is.na(train.data$Age), ] %>%
  group_by(Sex) %>%
  summarise(
    Sum = sum(Age),
    "Population" = n(),
    Mean = mean(Age),
    Mediane = median(Age)
  )

# Replacing missing ages
train.data[is.na(train.data$Age) & train.data$Sex == "male", ]$Age <- 
  mean_age[mean_age$Sex == 'male', ]$Mediane

train.data[is.na(train.data$Age) & train.data$Sex == "female", ]$Age <- 
  mean_age[mean_age$Sex == 'female', ]$Mediane
  

