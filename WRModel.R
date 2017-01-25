#import data
Train<-read.csv("WRTraining.csv",header = TRUE)
Test<-read.csv("WRTest.csv",header=TRUE)
attach(Train)

#create multi linear regression model
trainmodel <- lm(Adj.CarAV ~ YDPTA + TDPTA + Rush.Attempts
                 + Rush.Yards + Rush.TD + P.Yds +
                 BO.Age + Draft.Pick
                 + Weight + X40.time + X3.cone)
summary(trainmodel)

#add projections to dataframe
Train$Project <- predict(trainmodel,Train)
Test$Project <- predict(trainmodel,Test)

#set models for plots
testmodel<-lm(Test$Adj.CarAV ~ Test$Project)
summary(testmodel)
trainplot<-lm(Adj.CarAV ~ Train$Project)
summary(trainplot)

#scatterplots
plot(Train$Project, Adj.CarAV)
title(main="Model Effect on Training Data")
legend(x='topleft',legend="r^2 = 0.4509")
abline(trainplot,col="red")

plot(Test$Project, Test$Adj.CarAV)
title(main="Model Effect on Testing Data")
legend(x='topleft',legend="r^2 = 0.4421")
abline(testmodel,col="red")
