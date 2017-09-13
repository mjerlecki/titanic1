setwd("D:/analizy/titanic")

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggthemes)
library(stringr)
library(randomForest)
library(xgboost)
library(caret)
library(e1071)

test <- read.csv('test.csv')
train <- read.csv('train.csv')



full <- bind_rows(test, train)
summary(full)
str(full)

test$Pclass <- as.factor(test$Pclass)
train$Pclass <- as.factor(train$Pclass)
full$Pclass <- as.factor(full$Pclass)

train$Survived <- as.factor(train$Survived)
full$Survived <- as.factor(full$Survived)

ggplot(train, aes(x=Pclass, fill = Survived)) +
      geom_bar(stat = 'count', position = 'dodge')

ggplot(train, aes(x=Sex, fill = Survived)) +
  geom_bar(stat = 'count', position = 'dodge')


p1 <- ggplot(train, aes(x=Age, fill = Survived)) +
  geom_histogram(bins=10)

p2 <- ggplot(train, aes(x=Age, fill = Survived)) +
  geom_histogram(bins=20)

p3 <- ggplot(train, aes(x=Age, fill = Survived)) +
  geom_histogram(bins=30)

grid.arrange(p1, p2, p3)


p4 <- ggplot(train, aes(x=Age, fill = Survived)) +
  geom_histogram(bins=30) +
  facet_grid(.~Sex) +
  scale_fill_discrete(name = "Survived")

p5 <- ggplot(train, aes(x=Age, fill = Survived)) +
  geom_histogram(bins=30) +
  facet_grid(.~Sex)

grid.arrange(p4, p5)


p6 <- ggplot(train, aes(x=Sex, fill = Survived)) +
  geom_bar(stat='count') +
  facet_grid(.~Pclass) +
  ggtitle('Sex vs Survived vs Pclass') +
  ylab('# survived')

p7 <- ggplot(train, aes(x=Age, fill = Survived)) +
  geom_histogram(bins=30) +
  facet_grid(.~Pclass) +
  ggtitle('Age vs Survived vs Pclass') +
  ylab('# survived')

p8 <- ggplot(train[train$Pclass == "1",], aes(Embarked, Fare, fill = Survived)) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2)

full$Embarked[c(480, 1248)] <- 'C'

sum(is.na(full$Fare))
full[153,]
p9 <- ggplot(full[full$Pclass == "3",], aes(x=Fare))+
    geom_density(fill = 'blue', alpha = 0.3)+
    geom_vline(aes(xintercept=median(full[full$Pclass == "3",]$Fare, na.rm = T)))
p9


full$Fare[153] <- median(full[full$Pclass == "3",]$Fare, na.rm = T)

ggsave(filename ="MyPlots3.pdf", plot=grid.arrange(p6, p7))

ggplot(train, aes(x = Age, y = Sex)) + 
  geom_jitter(aes(colour = factor(Survived))) +
  facet_wrap(~Pclass) +
  theme_economist_white()


ggplot(train, aes(x = Pclass, y = Fare)) +
  geom_jitter(aes(colour = factor(Survived))) +
  labs(x = "Pclass", y = "Pclass", title = "Fare vs Pclass") +
  theme_fivethirtyeight()

title <- str_extract(full$Name, "[A-Z][a-z]*\\.")

full$Title <- title

table(full$Sex, full$Title)

full$Title[full$Title == "Capt."] <- "Other"
full$Title[full$Title == "Col."] <- "Other"
full$Title[full$Title == "Countess."] <- "Other"
full$Title[full$Title == "Don."] <- "Other"
full$Title[full$Title == "Dona."] <- "Other"
full$Title[full$Title == "Jonkheer."] <- "Other"
full$Title[full$Title == "Lady."] <- "Other"
full$Title[full$Title == "Major."] <- "Other"
full$Title[full$Title == "Mlle."] <- "Miss."
full$Title[full$Title == "Ms."] <- "Miss."
full$Title[full$Title == "Rev."] <- "Other"
full$Title[full$Title == "Sir."] <- "Other"
full$Title[full$Title == "Dr."] <- "Other"
full$Title[full$Title == "Mme."] <- "Miss."

p10 <- ggplot(full[419:1309,], aes(x = Title, fill = Survived)) +
  geom_bar()

p10

full$family_size <- full$SibSp + full$Parch + 1
full$ticket_type <- as.factor(substr(full$Ticket,1,1))


full$Title <- as.factor(full$Title)
full$Embarked <- as.factor(full$Embarked)
summary(full)

# modeling
sum(is.na(full$Age))


test1 <- full[1:418,]
train1 <- full[419:1309,]

summary(train1)
rf1 <- randomForest(data=train1, 
                  Survived ~ Pclass + Sex + SibSp +
                  Parch + Fare + Embarked + Title + family_size + ticket_type)
pred1 <- predict(rf1, test1)
pred1 <- cbind(test1$PassengerId, pred1)
pred1[,2] <- (pred1[,2] -1)

spr1 <- test1[153,]


nrow(test1)
str(pred1)

write.table(pred1, "pred4.csv", row.names = FALSE, col.names = c("PassengerID", "Survived"), sep = ",")

pred1[153,2] = 0
write.table(pred1, "pred3.csv", row.names = FALSE, col.names = c("PassengerID", "Survived"), sep = ",")

nn1 <- train(Survived ~ Pclass + Sex + SibSp +
               Parch + Fare + Embarked + Title + family_size + ticket_type,
             data=train1,
             method = "knn",
             preProcess = c("center", "scale"),
             tuneLength = 10,
             trControl = trainControl(method = "cv"))


pred2 <- predict(nn1, test1)
pred2 <- cbind(test1$PassengerId, pred2)
pred2[,2] <- (pred2[,2] -1)
write.table(pred2, "pred5.csv", row.names = FALSE, col.names = c("PassengerID", "Survived"), sep = ",")
