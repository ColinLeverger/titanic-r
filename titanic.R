#### Explore Titanic dataset #####

#### General setups ####
# Packages managemement
install.packages("dataQualityR")
install.packages("ggplot2")
install.packages("vcd")
install.packages("reshape2")
install.packages('randomForest')
library(dataQualityR)
library(dplyr)
library(ggplot2)
library(reshape2)
library(vcd)
library('randomForest') 

# Set working directory
setwd("/Users/colinleverger/Documents/Experiments/kaggle/titanic-ml")

# Load data
missing.types <- c("NA", "")
train.data <- read.csv("data/train.csv", na.strings = missing.types, stringsAsFactors = F)
test.data  <- read.csv("data/test.csv",  na.strings = missing.types, stringsAsFactors = F)
total.data <- bind_rows(train.data, test.data)

#### DQR ####
checkDataQuality(
  total.data,
  out.file.num = "DQR_cont.csv",
  out.file.cat = "DQR_cat.csv"
)
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
barplot(
  table(train.data$Pclass),
  main = "Passenger Classes",
  col = "red"
)

# Explore gender repartition
barplot(
  table(train.data$Sex),
  main = "Sex (gender)",
  col = "blue"
)

# Explore age repartition
hist(
  train.data$Age,
  main = "Age",
  xlab = NULL,
  col = "brown"
)
d <- density(train.data[!is.na(train.data$Age),]$Age)
plot(d, main = "Age density", xlab = NULL, col = "brown")

# Explore distribution of ages and sex
ggplot(train.data, aes(Age, fill = Sex)) +
  geom_histogram(alpha = 0.5, aes(y = ..count..))

# Explore old & young people survival
count(train.data[train.data$Age >= 50 & train.data$Sex == "male",])
count(train.data[train.data$Age >= 50 & train.data$Sex == "female",])

ggplot(train.data[train.data$Age >= 50,], aes(Survived, fill = Sex)) +
  geom_bar(alpha = 0.5, aes(y = ..count..))

ggplot(train.data[train.data$Age < 10,], aes(Survived, fill = Sex)) +
  geom_bar(alpha = 0.5, aes(y = ..count..))

# Explore fare paid by passengers
hist(
  train.data$Fare,
  main = "Fare",
  xlab = NULL,
  col = "red"
)

# Explore Siblings and spouses repartition
barplot(
  table(train.data$SibSp),
  main = "Siblings & Spouses",
  col = "orange"
)

# Explore parents and kid repartition
barplot(
  table(train.data$Parch),
  main = "Parch (parents and kid)",
  col = "white"
)

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

# Link it to the sex...
table(train.data$Sex, train.data$Title)

#### Duplicated ticket number investigations ####
duplicated.tickets <- train.data %>%
  group_by(Ticket) %>%
  summarise(NumberOfTickets = n()) %>%
  arrange(desc(NumberOfTickets)) %>%
  filter(NumberOfTickets > 1)

View(duplicated.tickets)

View(train.data[train.data$Ticket == 1601,])
View(train.data[train.data$Ticket == "PC 17477",])
View(train.data[train.data$Ticket == "CA. 2343",])

# Loop to find all duplicated ticket number not in familly
strange.dupes <- list()
strange.dupes.count = 1
for (ticket in duplicated.tickets$Ticket) {
  actual.ticket.names <-
    unique(train.data[train.data$Ticket == ticket,]$Name)
  if (length(actual.ticket.names) > 1) {
    strange.dupes[[strange.dupes.count]] <- ticket
    strange.dupes.count = strange.dupes.count + 1
  }
}

all.strange.dupes <-
  subset(train.data, train.data$Ticket %in% strange.dupes)

dupes.ticket.survival.rate <- all.strange.dupes %>%
  group_by(Ticket) %>%
  summarise(SurvivalRate = sum(Survived) / n()) %>%
  arrange(SurvivalRate)

dupes.ticket.survival.rate$SurvivalRateD[dupes.ticket.survival.rate$SurvivalRate == 0] <-
  "died"
dupes.ticket.survival.rate$SurvivalRateD[dupes.ticket.survival.rate$SurvivalRate < 0.33 &
                                           dupes.ticket.survival.rate$SurvivalRate > 0.1] <-
  "low"
dupes.ticket.survival.rate$SurvivalRateD[dupes.ticket.survival.rate$SurvivalRate >= 0.33 &
                                           dupes.ticket.survival.rate$SurvivalRate < 0.66] <-
  "medium"
dupes.ticket.survival.rate$SurvivalRateD[dupes.ticket.survival.rate$SurvivalRate >= 0.66] <-
  "high"

barplot(table(dupes.ticket.survival.rate$SurvivalRateD),
        main = "Survival rate for duplicated tickets",
        col = "violet")

#### Work on data ####
### Extract Titles and create new categorical column ###
total.data$Title   <- gsub('(.*, )|(\\..*)', '', total.data$Name)
total.data$Name    <- gsub('(, [a-zA-Z]{,20}. )', ', ', total.data$Name)
total.data$Surname <- gsub('(.*,)', '', total.data$Name)
total.data$Name    <- gsub('(,.*)', '', total.data$Name)

# Display repartition of the titles
barplot(
  table(total.data$Title),
  main = "Title",
  col = "blue"
)

# Rare titles management
rare.titles <-
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

# Replace rare titles with correct values
total.data$Title[total.data$Title == 'Mlle']        <- 'Miss'
total.data$Title[total.data$Title == 'Ms']          <- 'Miss'
total.data$Title[total.data$Title == 'Mme']         <- 'Mrs'
total.data$Title[total.data$Title %in% rare.titles]  <- 'RareTitle'

# Display result again
barplot(
  table(total.data$Title),
  main = "Titles",
  col = "blue"
)
table(total.data$Sex, total.data$Title)

### Create families description ###
# First, add family size
total.data$FamilySize = total.data$SibSp + total.data$Parch + 1

# Explore family size
barplot(table(total.data$FamilySize),
        main = "Family size repartition",
        col = "red")

# Survival vs Size:
ggplot(total.data, aes(x = FamilySize, fill = factor(Survived))) +
  geom_bar(stat = 'count', position = 'dodge') +
  scale_x_continuous(breaks = c(1:11)) +
  labs(x = 'Family Size')

# Then, add family size categorical feature
total.data$FamilySizeD[total.data$FamilySize == 1] <- 
  'singleton'
total.data$FamilySizeD[total.data$FamilySize > 1 & total.data$FamilySize < 5] <- 
  'small'
total.data$FamilySizeD[total.data$FamilySize >= 5] <- 
  'big'

### Deal with missing boarding
View(total.data[is.na(total.data$Embarked),])

# Get rid of our missing passenger IDs
embark.fare <- total.data %>%
  filter(PassengerId != 62 & PassengerId != 830)

# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embark.fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous()

total.data[(total.data$PassengerId == 62 | total.data$PassengerId == 830),]$Embarked <- "C"

### Deal with guys with several cabins (family?)
total.data$SeveralCabins <- 0
total.data[grepl(" ", total.data$Cabin),]$SeveralCabins <- 1

### Deal with missing fares
View(total.data[train.data$Fare <= 10 | is.na(total.data$Fare),] %>% arrange(Fare))
View(total.data[is.na(total.data$Fare),] %>% arrange(Fare))
ggplot(total.data[total.data$Pclass == '3' & total.data$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1)

total.data$Fare[1044] <- median(total.data[total.data$Pclass == '3' & total.data$Embarked == 'S', ]$Fare, na.rm = TRUE)

### Add some meaningfull features
# Women and children before
total.data$WomOrChildren <- 0
total.data[which(total.data$Age < 18 | total.data$Sex == 'female'),]$WomOrChildren <- 1

# First char of cabin is the deck
total.data$Deck <- substring(total.data$Cabin, 1, 1)
total.data$Deck[which(is.na(total.data$Deck))] <- "NoSe"

### Missing ages management
# Method 1: mean age method
mean.age <- total.data[!is.na(total.data$Age),] %>%
  group_by(Sex) %>%
  summarise(Mean = mean(Age),
            Mediane = median(Age))

total.data[is.na(total.data$Age) & total.data$Sex == "male",]$Age <-
  mean.age[mean.age$Sex == 'male',]$Mediane

total.data[is.na(total.data$Age) & total.data$Sex == "female",]$Age <-
  mean.age[mean.age$Sex == 'female',]$Mediane

# Method 2: predictive imputation
# TODO

#### Prediction ####
### Split the train and test data
total.data <- as.data.frame(unclass(total.data))
train <- total.data[1:891,]
test  <- total.data[892:1309,]

### Building the model
# Random seed
set.seed(42)

model1 <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + 
                        Fare + Title + 
                        WomOrChildren + Deck,
                        data = train)

plot(model1, ylim=c(0,0.5))

### Learning from the model created
# Get importance
importance    <- importance(model1)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip()

### Prediction
# Predict...
prediction <- predict(model1, test)

# Save prediction
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# Write prediction on disk
write.csv(solution, file = 'model1.csv', row.names = F)
