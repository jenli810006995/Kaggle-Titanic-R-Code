# Load packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

library(tidyverse)


####### read in the raw data files ######


train <- read.csv(file = "train.csv", as.is = FALSE)

test <- read.csv(file = "test.csv", as.is = FALSE)

full <- bind_rows(train,test)  # bind train and test data 



############################################## Start EDA ###################################################


# checking for the quantiles for the train data, I did this for checking the relationship of them in the train data

summary(as.matrix(full[1:891,c(2,3,5)]))

# plot histogram for the train data to see the relationship

################## Survival vs PClass ################

#### FIGURE 1.4 #####

#install.packages("ggthemes")

library(ggthemes)

ggplot(train, aes(x =train$Pclass, fill = factor(train$Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'PClass', y="counts") +
  theme_few()



################### Survival vs Gender #################

###### FIGURE 1.3 ##########


full$Gender_Recode <- ifelse(full$Sex=="male",1,2)


ggplot(train, aes(x =full[1:891,]$Gender_Recode, fill = factor(train$Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Gender(male=1)') +
  theme_few()



######################################## Cleaned the variable Name ##################################


#### grab title from passenger's names

full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

#### show title counts by sex

table(full$Sex, full$Title)


#### Titles with very low cell counts to be combined to "rare" level

rare_title <- c("Dona", "Lady", "the Countess", "Capt", "Col", "Don","Dona",
                "Dr", "Major", "Rev", "Sir", "Jonkheer")


#### Also reassign mlle, ms, and mme accordingly: grouping

full$Title[full$Title=="Mlle"] <- "Miss"

full$Title[full$Title=="Ms"] <- "Miss"

full$Title[full$Title=="Mme"] <-"Mrs"

full$Title [full$Title%in% rare_title] <-"Rare Title"

#### show title counts by sex again

table(full$Sex, full$Title)

#### Plot a histogram to take a look at the spread

ggplot(full, aes(x =full$Gender_Recode, fill = factor(full$Title))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Gender(male=1)') +
  theme_few()


#### show title counts by survied

table(full[1:891,]$Survived, full[1:891,]$Title) # not much to tell

#### Plot a histogram to take a look at the spread of title and survival

ggplot(full[1:891,], aes(x =full[1:891,]$Survived, fill =factor(full[1:891,]$Title))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Survival(survived=1)') +
  theme_few()

### pretty clear that the un-survived majority was men ###########


############################ Start with FamilySize ##############################


#### there're those two variables SibSb and Parch 
#### that indicate the number of family members the passenger is travelling with. 

#### so let's combine the two variables into a new one, FamilySize:


full$FamilySize <-full$SibSp + full$Parch +1


####  let's convert the FamilySize variable temporarily to a string 
###  and combine it with the Surname to get our new FamilyID variable

full$FamilyID <- paste(as.character(full$FamilySize),full$Surname, sep = "")

### categorize family size


full$FamilyID[full$FamilySize==2] <-"small"

full$FamilyID[full$FamilyID=="small"] <-"Small"

summary(full)

########################## Look at gender ##################################################################
## TABLE 1.1 and TABLE 1.2 ###

table(train$Sex, train$Survived)

prop.table(table(train$Sex, train$Survived), 1) # 1 is the row dimension, and 2 is the column dimension

# so we can see for each gender, how many people survived and perished.

###################################### Catogorize Fare ##############################################

full$Fare2 <- as.character("30+")
full$Fare2[full$Fare<30 & full$Fare>=20] <- "20-30"
full$Fare2[full$Fare<20 & full$Fare>=10] <- "10-20"
full$Fare2[full$Fare<10] <- "<10"

view(full)

summary(full)

full$Fare2 <- as.factor(full$Fare2)

aggregate(Survived ~ Fare2+Sex+Pclass, data = full, FUN = function(x) {sum(x)/length(x)})


##### Deal with missing values in Age ######


require(Hmisc)

str(full)

Age.impute <- impute(full$Age)

full$Age_1 <-Age.impute

summary(full) # Age_1 has no NA now, we used "Age_1" as the variable for "Age" in the final model


####### create a new variable named relAge to categorize it #########

full$Age <- as.numeric(full$Age)
full$relAge <- NA
full$relAge <- ifelse(full$Age < 19, "< 19", 
                      ifelse(20 < full$Age & full$Age < 39, "20-39", 
                             ifelse(40 < full$Age & full$Age < 59, "40-59", "60>")))

#### Check Age levels
table(full$relAge)

#### Grouped Bar Plot FIGURE 1.2 #####

counts2 <- table(full$Survived, full$relAge)
barplot(counts2, main="Survived by Age levels",
        xlab="Age groups", col=c("darkblue","red"),
        legend = rownames(counts2),args.legend = list(x="topleft"),beside=TRUE)

################################################ Correlation Plot FIGURE 1.6 ########################################################

factor_vars <- c("Pclass",  "Sex", "Embarked", "FamilyID", "Title", "PassengerId", "Survived", "Name", "Ticket", "Cabin") # fare cannot be factor, would be too many levels 

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x)) ## good to use this to change variable to factor

training_set <- full[1:891,]

test_set <- full[892:1309,]


## correlation plot ###

full_subset <- full[,c("Pclass","Sex","FamilySize","FamilyID","Title")]


nu.variable <- c("Pclass","Sex","FamilyID","Title")


full_subset[nu.variable] <- lapply(full_subset[nu.variable], function(x) as.numeric(x))   


require(stats)


cor(full_subset)### correlation coefficients for predictors used for conditonal inference trees


require(corrplot)
corrplot(cor(full_subset))


################################################# Conditional Inference Trees  ###############################################

set.seed(129)

require(party)



fit <- cforest(as.factor(Survived)~as.factor(Pclass)+Sex+FamilySize+Fare+Age_1+as.factor(Embarked)+as.factor(FamilyID)+as.factor(Title),
               data = training_set,
               controls = cforest_unbiased(ntree=2000, mtry=3))

prediction <-predict(fit, newdata=test_set, OOB=TRUE, type="response")

submit <- data.frame(PassengerID=test_set$PassengerId, Survived=prediction)
write.csv(submit, file = "CIT_0422_3.csv", row.names = FALSE) 

#### This model reached 0.8 at 2:04 a.m. on April 22 ####
