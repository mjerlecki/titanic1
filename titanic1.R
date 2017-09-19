setwd("C:/analizy/titanic")

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
library(rpart)
library(glmnet)

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

p11 <- ggplot(full, aes(x = Title, y=Age)) +
  geom_jitter(aes(colour = factor(Survived)))
p11

age1 <- 0
for(x in full$Title){
  age1[x] <- mean(full[!is.na(full$Age) & full$Title == x,]$Age)
}

full[is.na(full$Age) & full$Title == "Mr.",]$Age <- 32.25
full[is.na(full$Age) & full$Title == "Mrs.",]$Age <- 36.99
full[is.na(full$Age) & full$Title == "Miss.",]$Age <- 21.83
full[is.na(full$Age) & full$Title == "Master.",]$Age <- 5.48
full[is.na(full$Age) & full$Title == "Other",]$Age <- 45.17

#feature engineering - adding ticket group

ticket_group <- transform(full$Ticket, id= as.numeric(factor(full$Ticket)))
full$ticket_group <- as.character(ticket_group[2,])

age1
# modeling
sum(is.na(full$Age))


test1 <- full[1:418,]
train1 <- full[419:1309,]

summary(train1)
rf1 <- randomForest(data=train1, 
                    Survived ~ Pclass + Sex + SibSp +
                      Parch + Fare + Embarked + Title + family_size + ticket_type, 
                    importance = T)
imp1 <- importance(rf1)

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


rf2 <- randomForest(data=train1, 
                    Survived ~ Pclass + Sex + SibSp + Age +
                      Parch + Fare + Embarked + Title + family_size + ticket_type, 
                     importance = T)
summary(rf2)
pred3 <- predict(rf2, test1)
pred3 <- cbind(test1$PassengerId, pred3)
pred3[,2] <- (pred3[,2] -1)
write.table(pred3, "pred6.csv", row.names = FALSE, col.names = c("PassengerID", "Survived"), sep = ",")

importance <- importance(rf2)
table_importance <- data.frame(Variables = row.names(importance),
                               Importance= round(importance[ ,'MeanDecreaseGini'],2))

table_importance[order(importance),]
imp1 <- table_importance[order(importance),]


ggplot(table_importance, aes(reorder(Variables, Importance), Importance)) +
  geom_bar(stat="identity") +
  coord_flip()

rf3 <- randomForest(data=train1, 
                    Survived ~ Pclass + Sex + SibSp + Age +
                      Parch + Fare + Embarked + Title + family_size + ticket_type, 
                    importance = T,
                    na.action = na.omit)

pred4 <- predict(rf3, test1)
pred4 <- cbind(test1$PassengerId, pred4)
pred4[,2] <- (pred4[,2] -1)
write.table(pred4, "pred7.csv", row.names = FALSE, col.names = c("PassengerID", "Survived"), sep = ",")

rf4 <- randomForest(data=train1, 
                    Survived ~ Pclass + Sex + Age +
                      Title + family_size, 
                    importance = T,
                    na.action = na.omit)

pred5 <- predict(rf4, test1)
pred5 <- cbind(test1$PassengerId, pred5)
pred5[,2] <- (pred5[,2] -1)
write.table(pred5, "pred8.csv", row.names = FALSE, col.names = c("PassengerID", "Survived"), sep = ",")


dt1 <- rpart(Survived ~ Pclass + Sex + Age +
               Title + family_size, data=train1, method = "class")

pred6 <- predict(dt1, test1, type="class")
pred6 <- cbind(test1$PassengerId, pred6)
pred6[,2] <- (pred6[,2] -1)
write.table(pred6, "pred9.csv", row.names = FALSE, col.names = c("PassengerID", "Survived"), sep = ",")

glm1 <- train(Survived ~ Pclass + Sex + SibSp + Age +
                Parch + Fare + Embarked + Title + family_size + ticket_type,
              method = "glmnet",
              data = train1,
              tuneGrid = expand.grid(alpha = 0:1,
              lambda = seq(0.0001, 1, length = 20))
)
pred7 <- predict(glm1, test1)
pred7 <- cbind(test1$PassengerId, pred7)
pred7[,2] <- (pred7[,2] -1)
write.table(pred7, "pred10.csv", row.names = FALSE, col.names = c("PassengerID", "Survived"), sep = ",")


rf5 <- randomForest(data=train1, 
                    Survived ~ Pclass + Sex + SibSp +
                      Parch + Fare + Embarked + Title + family_size + ticket_type + ticket_group, 
                    importance = T)
imp5 <- importance(rf5)

pred8 <- predict(rf5, test1)
pred8 <- cbind(test1$PassengerId, pred8)
pred8[,2] <- (pred8[,2] -1)
write.table(pred8, "pred11.csv", row.names = FALSE, col.names = c("PassengerID", "Survived"), sep = ",")