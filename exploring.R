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
test.data <- read.csv("data/test.csv", na.string = missing.types)
total.data <- bind_rows(train.data, test.data)

#### DQR ####
checkDataQuality(total.data,
                 out.file.num = "DQR_cont.csv",
                 out.file.cat = "DQR_cat.csv")
dqr.cont <- read.csv("DQR_cont.csv")
dqr.cat  <- read.csv("DQR_cat.csv")

#### Explore the data ####
# Explore difference between men's and women's death
survived <- train.data %>%
  group_by(Sex) %>%
  summarise(Survived = sum(Survived),
            Died = n() - sum(Survived))

survived.long <- melt(survived, id.vars = "Sex")
ggplot(survived.long, aes(x = variable, y = value, fill = factor(Sex))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Gender") +
  xlab("People") + ylab("Population")

# Explore survival rates
barplot(
  table(train.data$Survived),
  names.arg = c("Perished", "Survived"),
  main = "Survived",
  col = "black"
)

# Explore passenger classes
barplot(table(train.data$Pclass),
        main = "Passenger Classes",
        col = "red")

# Explore gender repartition
barplot(table(train.data$Sex),
        main = "Sex (gender)",
        col = "blue")

# Explore age repartition
hist(train.data$Age,
     main = "Age",
     xlab = NULL,
     col = "brown")
d <- density(train.data[!is.na(train.data$Age),]$Age)
plot(d, main = "Age density", xlab = NULL, col = "brown")

# Explore distribution of ages and sex
ggplot(train.data, aes(Age, fill = Sex)) +
  geom_histogram(alpha = 0.5, aes(y = ..count..))

# Explore old & young people survival
count(train.data[train.data$Age >= 50 & 
                   train.data$Sex == "male",])
count(train.data[train.data$Age >= 50 &
                   train.data$Sex == "female",])

ggplot(train.data[train.data$Age >= 50,], aes(Survived, fill = Sex)) +
  geom_bar(alpha = 0.5, aes(y = ..count..))

ggplot(train.data[train.data$Age < 10,], aes(Survived, fill = Sex)) +
  geom_bar(alpha = 0.5, aes(y = ..count..))

# Explore fare paid by passengers
hist(train.data$Fare,
     main = "Fare",
     xlab = NULL,
     col = "red")

# Explore Siblings and spouses repartition
barplot(table(train.data$SibSp),
        main = "Siblings & Spouses",
        col = "orange")

# Explore parents and kid repartition
barplot(table(train.data$Parch),
        main = "Parch (parents and kid)",
        col = "white")

# Explore boarding location
barplot(
  table(train.data$Embarked),
  names.arg = c("Cherbourg", "Queenstown", "Southampton"),
  main = "Embarked",
  col = "yellow"
)

# Explore passenger Fate by Traveling Class
mosaicplot(
  train.data$Pclass ~ train.data$Survived,
  main = "Passenger Fate by Traveling Class",
  shade = FALSE,
  color = TRUE,
  xlab = "Passenger Class",
  ylab = "Survived"
)

# Explore passenger Fate by Embarked places
mosaicplot(
  train.data$Embarked ~ train.data$Survived,
  main = "Passenger Fate by Embarked places",
  shade = FALSE,
  color = TRUE,
  xlab = "Embarqued",
  ylab = "Survived"
)

# Explore passenger Travelling Class by Age
boxplot(
  Age ~ Pclass,
  data = train.data,
  main = "Passenger Travelling Class by Age",
  xlab = "Passenger Class",
  ylab = "Age"
)

# Computing median and mean for age
mean.age <- train.data[!is.na(train.data$Age),] %>%
  group_by(Sex) %>%
  summarise(Mean = mean(Age),
            Mediane = median(Age))

# Extracting values "Mr", ... and create new categorical column
train.data$Title <- gsub('(.*, )|(\\..*)', '', train.data$Name)
train.data$Name <-
  gsub('(, [a-zA-Z]{,20}. )', ', ', train.data$Name)
train.data$Surname <-
  gsub('(.*,)', '', train.data$Name)
train.data$Name <-
  gsub('(,.*)', '', train.data$Name)

# Displaying repartition of the titles
barplot(table(train.data$Title),
        main = "Title",
        col = "blue")

# Linking it to the sex...
table(train.data$Sex, train.data$Title)

# Treatment of the rare titles
rare.title <-
  c(
    'Dona',
    'Lady',
    'the Countess',
    'Capt',
    'Col',
    'Don',
    'Dr',
    'Major',
    'Rev',
    'Sir',
    'Jonkheer'
  )

# TODO entire dataset
train.data$Title[train.data$Title == 'Mlle']        <- 'Miss'
train.data$Title[train.data$Title == 'Ms']          <- 'Miss'
train.data$Title[train.data$Title == 'Mme']         <- 'Mrs'
train.data$Title[train.data$Title %in% rare.title]  <- 'RareTitle'

# Explore result again
barplot(table(train.data$Title),
        main = "Titles",
        col = "blue")
table(train.data$Sex, train.data$Title)

# Explore families
train.data$FamilySize = train.data$SibSp + train.data$Parch + 1
barplot(table(train.data$FamilySize),
        main = "Family size repartition",
        col = "red")

ggplot(train.data, aes(x = FamilySize, fill = factor(Survived))) +
  geom_bar(stat = 'count', position = 'dodge') +
  scale_x_continuous(breaks = c(1:11)) +
  labs(x = 'Family Size')

# TODO entire dataset
train.data$FamilySizeD[train.data$FamilySize == 1] <- 'singleton'
train.data$FamilySizeD[train.data$FamilySize > 1 &
                         train.data$FamilySize < 5] <- 'small'
train.data$FamilySizeD[train.data$FamilySize >= 5] <- 'big'

#### Replace missing values ####

View(train.data[is.na(train.data$Embarked),])

View(
  train.data %>%
    group_by(Ticket) %>%
    summarise(NumberOfTickets = n()) %>%
    arrange(desc(NumberOfTickets)) %>%
    filter(NumberOfTickets > 1)
)

View(train.data[train.data$Ticket == 1601,])
View(train.data[train.data$Ticket == 347082,])
View(train.data[train.data$Ticket == "CA. 2343",])

# Dummy method: replace age by median for gender
train.data[is.na(train.data$Age) &
             train.data$Sex == "male",]$Age <-
  mean.age[mean.age$Sex == 'male',]$Mediane

train.data[is.na(train.data$Age) &
             train.data$Sex == "female",]$Age <-
  mean.age[mean.age$Sex == 'female',]$Mediane
