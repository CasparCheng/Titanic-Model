train <- read.csv("~/OneDrive/EZ4/EZ Career/demo/train.csv")
test <- read.csv("~/OneDrive/EZ4/EZ Career/demo/test.csv")

head(train)
tail(train)

head(test)
tail(test)

summary(train)

train$Pclass <- factor(train$Pclass)
train$Survived <- factor(train$Survived)


#Age have NA, embark have two missing

#Visuliazation

#1 cat - bar
#1 num - hist
#1 cat 1 num boxplot
#2 num - scatterplot

library(ggplot2)

str(train)

ggplot(train, aes(x=Sex)) + geom_bar()
ggplot(train, aes(x=Sex, fill=Survived)) + geom_bar()
#sex is good

table(train$Sex, train$Survived)

ggplot(train, aes(x=Sex, fill = factor(Survived))) + geom_bar()
ggplot(train, aes(x=Pclass, fill = factor(Survived))) + geom_bar()
#Pclass, sex, and survial

ggplot(train, aes(x=Pclass, fill = Survived)) + geom_bar() + facet_wrap(~Sex)

ggplot(train, aes(x=Age)) + geom_histogram()

ggplot(train, aes(x=Age, fill = factor(Survived))) + geom_histogram()
ggplot(train, aes(x=Age, fill = factor(Survived))) + geom_histogram() + facet_wrap(~Sex)

#sex ok, pclass, age ok

ggplot(train, aes(x=Fare, fill = factor(Survived))) + geom_histogram()
#sex ok, pclass, age ok
ggplot(train, aes(x=Fare, fill = factor(Survived))) + geom_histogram() + facet_wrap(~Sex)

ggplot(train, aes(x=Embarked, fill = factor(Survived))) + geom_bar()
#Embarked ????







#Feature eng
#Categorize
head(train)
train$agegroup <- "Adult"

train$agegroup

train$agegroup[train$Age < 15] <- "Child" 
train$agegroup[train$Age > 55] <- "Elder" 

train$agegroup

ggplot(train, aes(x=agegroup, fill = factor(Survived))) + geom_bar() + facet_wrap(~Sex)

#Split data
#Into training and testing
traintrain <- train[1:650,]
trainvalid <- train[651:891,]

#If your y is categorical
model.1 <- glm(Survived ~ Sex + Pclass + agegroup, family = "binomial", data = traintrain)
summary(model.1)
prob <- predict(model.1, trainvalid, type = "response")
probcut <- ifelse(prob > 0.5, "Yes", "No")
table(probcut, trainvalid$Survived)

(135+56)/(135+30+20+56)
#Base R, we get acc around 0.79

#Add interaction effect
model.2 <- glm(Survived ~  Pclass + agegroup * Sex, family = "binomial", data = traintrain)
summary(model.2)

prob <- predict(model.2, trainvalid, type = "response")
probcut <- ifelse(prob > 0.5, "Yes", "No")
table(probcut, trainvalid$Survived)

(135+59)/(135+27+20+59)
#Base R, we get acc around 0.804

#Add more interaction effect
model.3 <- glm(Survived ~  Pclass * agegroup * Sex, family = "binomial", data = traintrain)
summary(model.3)

prob <- predict(model.3, trainvalid, type = "response")
probcut <- ifelse(prob > 0.5, "Yes", "No")
table(probcut, trainvalid$Survived)

(137+56)/(137+18+30+56)
#Base R, we get acc around 0.8008








#More feature eng

head(train)


#strsplit
#1 what do you want to split
#Split by what

haha <- strsplit("Lance, Zhang", ",")

#list
haha[[1]][2]

#Means a list
list1 <- strsplit(c("Lance, Zhang", "Lancy, chang"), ",")

list1[[2]][2]

#List combination of vectors
#Can store many vectors
#And their lengths can be diff


#list

str(train)
train$Name <- as.character(train$Name)
train$Name

strsplit(train$Name, "[,.]")

#sapply
sapply(train$Name, FUN = function(x){strsplit(x, "[,.]")[[1]][2]})

suffix <- sapply(train$Name, FUN = function(x){strsplit(x, "[,.]")[[1]][2]})
table(suffix)

#Mlle, Ms, Lady, change to miss
#Mme change to Mrs
suffix[suffix==" Mlle"] <- " Miss"
suffix[suffix==" Ms"] <- " Miss"
suffix[suffix==" Lady"] <- " Miss"

suffix[suffix %in% c(" Mlle", " Ms", " Lady")] <- " Miss"
table(suffix)

suffix[suffix==" Mme"] <- " Mrs"
table(suffix)

suffix[suffix %in% c(" Col", " Don", " Jonkheer", 
                     " Major", " Master", " Rev", " the Countess", " Capt", " Sir")] <- "Rare"
table(suffix)

train <- cbind(train, suffix)
#train$suffix <- suffix
#train %>%
#  mutate(suffix = suffix)
summary(train)

vec <- c("a", "b", "c")
vec

vec %in% c("b", "d", "e")


#Add more interaction effect
#Into training and testing
traintrain <- train[1:650,]
trainvalid <- train[651:891,]


model.4 <- glm(Survived ~  agegroup + Pclass * Sex + suffix, family = "binomial", data = traintrain)
summary(model.4)

prob <- predict(model.4, trainvalid, type = "response")
probcut <- ifelse(prob > 0.5, "Yes", "No")
table(probcut, trainvalid$Survived)

(132+56)/(132+23+30+56)
#0.8008299

#Random Forest
library(randomForest)
traintrain <- na.omit(traintrain)

traintrain$agegroup <- "adult"
traintrain$agegroup[traintrain$Age < 15] <- "child"
traintrain$agegroup[traintrain$Age > 55] <- "elder"

model.5 <- randomForest(factor(Survived)~ Pclass + Sex + suffix + Age, data = traintrain)
model.5

#Random forest get 80% acc




