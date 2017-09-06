setwd("D:/analizy/titanic")

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggthemes)
library(stringr)
library(randomForest)

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

p6
p7

grid.arrange(p6, p7)

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

full$title <- title
full$family_size <- full$SibSp + full$Parch
full$ticket_type <- as.factor(substr(full$Ticket,1,1))
pred_Age <- median(train1[!is.na(train1$Age),"Age"])
pred_fare <- median(train1[!is.na(train1$Fare),"Fare"])
full$Age[is.na(full$Age)] <- pred_Age
full$Fare[is.na(full$Fare)] <- pred_fare
full$title <- as.factor(full$title)
full$Embarked <- as.factor(full$Embarked)
summary(full)

# modeling
sum(is.na(full$Age))
sum(is.na(train1$Age))

test1 <- full[1:418,]
train1 <- full[419:1309,]

summary(train1)
rf1 <- randomForest(data=train1, 
                  Survived ~ Pclass + Sex + SibSp +
                  Parch + Fare + Embarked + title + family_size + ticket_type)
pred1 <- predict(rf1, test1)
pred1 <- cbind(test1$PassengerId, pred1)
pred1[,2] <- (pred1[,2] -1)

pred1[153,2] = 1
pred1[153,2] = 0

nrow(test1)
str(pred1)


write.table(pred1, "pred2.csv", row.names = FALSE, col.names = c("PassengerID", "Survived"), sep = ",")

write.table(pred1, "pred3.csv", row.names = FALSE, col.names = c("PassengerID", "Survived"), sep = ",")
