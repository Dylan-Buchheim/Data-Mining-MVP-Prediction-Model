#Libraries --------------------------------------
library(C50)

#Datasets ---------------------------------------
#PlayerData contains the cleaned and prepared dataframe created in project 1.
PlayerData <- read.csv("PlayerData.csv")

#Setup Phase ------------------------------------

#Partitioning the Data
set.seed(100)
trainingIndexes <- runif(nrow(PlayerData)) < 0.75
PlayerData_training <- PlayerData[trainingIndexes,]
PlayerData_test <- PlayerData[!trainingIndexes,]

#Validating the Partition
#Performing a t-test on three key numeric values
t.test(PlayerData_training$TRB,PlayerData_test$TRB)
t.test(PlayerData_training$AST,PlayerData_test$AST)
t.test(PlayerData_training$PTS,PlayerData_test$PTS)
#Performing a two sample z-test on the target variable "MVP"
p1 <- sum(PlayerData_training$MVP == "yes")/nrow(PlayerData_training)
p2 <- sum(PlayerData_test$MVP == "yes")/nrow(PlayerData_test)
pOverall <- sum(PlayerData$MVP == "yes")/nrow(PlayerData)
zMVP <- (p1 - p2)/sqrt(pOverall * (1-pOverall) * (1/nrow(PlayerData_training)  + 1/nrow(PlayerData_test)))
#printing the resulting p-value
print(2*pnorm(-abs(zMVP)))

#Rebalancing the Data
#Aiming to have MVP = "yes" for 20% of records.
numYes <- nrow(PlayerData_training[which(PlayerData_training$MVP == "yes"),])
resampleSize <- ((0.2 * nrow(PlayerData_training) - numYes) / 0.8)
to.resample <- which(PlayerData_training$MVP == "yes")
resample <- sample(x=to.resample, size=resampleSize, replace= TRUE)
resample.records<- PlayerData_training[resample,]
PlayerData_training_rebal <- rbind(PlayerData_training,resample.records)
rebalTable.v1<-table(PlayerData_training_rebal$MVP)
rebalTable.v2<-rbind(rebalTable.v1,round(prop.table(rebalTable.v1),4))
colnames(rebalTable.v2) <- c("y=no","y=yes")
rownames(rebalTable.v2) <- c("count","proportion")
#Verify that the rebalance worked.
rebalTable.v2

#Establishing Baseline Performance
#the accuracy generated by classifying every entry as "no"
baselineAccuracy <- nrow(PlayerData_test[which(PlayerData_test$MVP == "no"),])/nrow(PlayerData_test)

#Modeling Phase ----------------------------------

#Building various C5.0 models to predict MVP
#First Model (TRB_Binned, AST_Binned, PTS_Binned)
C5.1 <- C5.0(formula = MVP ~ TRB_Binned + AST_Binned + PTS_Binned, data = PlayerData_training_rebal, control = C5.0Control(minCases=20))
plot(C5.1)
testData = data.frame(TRB_Binned = PlayerData_test$TRB_Binned, AST_Binned = PlayerData_test$AST_Binned, PTS_Binned = PlayerData_test$PTS_Binned)
predictMVPC5.1 <- predict(C5.1, newdata = testData, type = "class")
predictMVPC5.1_df <- data.frame(MVP = predictMVPC5.1)
confMatrixC5.1 <- table(PlayerData_test$MVP,predictMVPC5.1_df$MVP)
row.names(confMatrixC5.1) <- c("Actual: False:","Actual: True")
colnames(confMatrixC5.1) <- c("Predicted: False","Predicted: True")
confMatrixC5.1 <- addmargins(A = confMatrixC5.1, FUN=list(Total = sum), quiet = TRUE)
confMatrixC5.1
accuracyC5.1 <- (confMatrixC5.1[1,1] + confMatrixC5.1[2,2]) / confMatrixC5.1[3,3]

#Second Model (TRB_Binned, AST_Binned, PTS_Binned, Age_Binned, G_Binned, Year_Binned, Pos)
C5.2 <- C5.0(formula = MVP ~ TRB_Binned + AST_Binned + PTS_Binned + G_Binned + Age_Binned + Year_Binned + Pos, data = PlayerData_training_rebal, control = C5.0Control(minCases=20))
plot(C5.2)
testData02 = data.frame(TRB_Binned = PlayerData_test$TRB_Binned,
                        AST_Binned = PlayerData_test$AST_Binned, 
                        PTS_Binned = PlayerData_test$PTS_Binned,
                        Age_Binned = PlayerData_test$Age_Binned,
                        G_Binned = PlayerData_test$G_Binned,
                        Year_Binned = PlayerData_test$Year_Binned,
                        Pos = PlayerData_test$Pos)
predictMVPC5.2 <- predict(C5.2, newdata = testData02, type = "class")
predictMVPC5.2_df <- data.frame(MVP = predictMVPC5.2)
confMatrixC5.2 <- table(PlayerData_test$MVP,predictMVPC5.2_df$MVP)
row.names(confMatrixC5.2) <- c("Actual: False:","Actual: True")
colnames(confMatrixC5.2) <- c("Predicted: False","Predicted: True")
confMatrixC5.2 <- addmargins(A = confMatrixC5.2, FUN=list(Total = sum), quiet = TRUE)
confMatrixC5.2
accuracyC5.2 <- (confMatrixC5.2[1,1] + confMatrixC5.2[2,2]) / confMatrixC5.2[3,3]

#Third Model (TRB, AST PTS, Age, G, Year_Binned, WS)
C5.3 <- C5.0(formula = MVP ~ TRB + AST + PTS + G + Age + Year_Binned + WS, data = PlayerData_training_rebal, control = C5.0Control(minCases=25))
plot(C5.3)
testData03 = data.frame(TRB = PlayerData_test$TRB,
                        AST = PlayerData_test$AST, 
                        PTS = PlayerData_test$PTS,
                        Age = PlayerData_test$Age,
                        G = PlayerData_test$G,
                        Year_Binned = PlayerData_test$Year_Binned,
                        WS = PlayerData_test$WS)
predictMVPC5.3 <- predict(C5.3, newdata = testData03, type = "class")
predictMVPC5.3_df <- data.frame(MVP = predictMVPC5.3)
confMatrixC5.3 <- table(PlayerData_test$MVP,predictMVPC5.3_df$MVP)
row.names(confMatrixC5.3) <- c("Actual: False:","Actual: True")
colnames(confMatrixC5.3) <- c("Predicted: False","Predicted: True")
confMatrixC5.3 <- addmargins(A = confMatrixC5.3, FUN=list(Total = sum), quiet = TRUE)
confMatrixC5.3
accuracyC5.3 <- (confMatrixC5.3[1,1] + confMatrixC5.3[2,2]) / confMatrixC5.3[3,3]
sensitivityC5.3 <- (confMatrixC5.3[2,2])/confMatrixC5.3[2,3]
precisionC5.3 <- (confMatrixC5.3[2,2])/confMatrixC5.3[3,2]

#The third model resulted in the best classifcation performance.
