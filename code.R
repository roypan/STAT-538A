setwd("...")
sc2=read.csv("starcraft.csv",header=T)

summary(sc2)

dat=sc2[,-c(1,3,4,5)]
dat[,c(3:8,12:16)]=dat[,c(3:8,12:16)]*88.5*60
dat[,17]=dat[,17]/88.5/60

dat['LeagueIndex'][dat['LeagueIndex']>=7]=7

par(mfrow=c(1,2))
barplot(table(sc2$LeagueIndex),xlab='League Index', ylab='count', main='Original Index')
barplot(table(dat$LeagueIndex),xlab='League Index', ylab='count', main='After Merging League 7 and 8')

library(ggplot2) 
library(reshape2)
ggplot(melt(cor(dat)), aes(Var1, Var2))+geom_tile(aes(fill = value))+theme(axis.text.x = element_text(angle = 90, hjust = 1))

png(filename='boxplot.png',  width = 800, height = 800, units = "px")
par(mfrow=c(4,4))
boxplot(APM~LeagueIndex,xlab='LeagueIndex',ylab='APM',data=dat)
boxplot(SelectByHotkeys~LeagueIndex,xlab='LeagueIndex',ylab='SelectByHotkeys',data=dat)
boxplot(AssignToHotkeys~LeagueIndex,xlab='LeagueIndex',ylab='AssignToHotkeys',data=dat)
boxplot(UniqueHotkeys~LeagueIndex,xlab='LeagueIndex',ylab='UniqueHotkeys',data=dat)
boxplot(MinimapAttacks~LeagueIndex,xlab='LeagueIndex',ylab='MinimapAttacks',data=dat)
boxplot(MinimapRightClicks~LeagueIndex,xlab='LeagueIndex',ylab='MinimapRightClicks',data=dat)
boxplot(NumberOfPACs~LeagueIndex,xlab='LeagueIndex',ylab='NumberOfPACs',data=dat)
boxplot(GapBetweenPACs~LeagueIndex,xlab='LeagueIndex',ylab='GapBetweenPACs',data=dat)
boxplot(ActionLatency~LeagueIndex,xlab='LeagueIndex',ylab='ActionLatency',data=dat)
boxplot(ActionsInPAC~LeagueIndex,xlab='LeagueIndex',ylab='ActionsInPAC',data=dat)
boxplot(TotalMapExplored~LeagueIndex,xlab='LeagueIndex',ylab='TotalMapExplored',data=dat)
boxplot(WorkersMade~LeagueIndex,xlab='LeagueIndex',ylab='WorkersMade',data=dat)
boxplot(UniqueUnitsMade~LeagueIndex,xlab='LeagueIndex',ylab='UniqueUnitsMade',data=dat)
boxplot(ComplexUnitsMade~LeagueIndex,xlab='LeagueIndex',ylab='ComplexUnitsMade',data=dat)
boxplot(ComplexAbilityUsed~LeagueIndex,xlab='LeagueIndex',ylab='ComplexAbilityUsed',data=dat)
boxplot(MaxTimeStamp~LeagueIndex,xlab='LeagueIndex',ylab='MaxTimeStamp',data=dat)
dev.off()

library(randomForest)
rffit=randomForest(as.factor(LeagueIndex)~., data=dat)
importance(rffit)
varImpPlot(rffit,type=2, main="Mean Gini decrease for each explanatory variable")

set.seed(2016)
ind=sample(1:nrow(dat),1000)
train=dat[-ind,]
test=dat[ind,]

library(MASS)
coe=(glm(I(LeagueIndex>=2)~.-MaxTimeStamp-ComplexAbilityUsed-ComplexUnitsMade-UniqueUnitsMade-SelectByHotkeys-ActionsInPAC, data=train)$coef)
coe=cbind(coe,glm(I(LeagueIndex>=3)~.-MaxTimeStamp-ComplexAbilityUsed-ComplexUnitsMade-UniqueUnitsMade-SelectByHotkeys-ActionsInPAC, data=train)$coef)
coe=cbind(coe,glm(I(LeagueIndex>=4)~.-MaxTimeStamp-ComplexAbilityUsed-ComplexUnitsMade-UniqueUnitsMade-SelectByHotkeys-ActionsInPAC, data=train)$coef)
coe=cbind(coe,glm(I(LeagueIndex>=5)~.-MaxTimeStamp-ComplexAbilityUsed-ComplexUnitsMade-UniqueUnitsMade-SelectByHotkeys-ActionsInPAC, data=train)$coef)
coe=cbind(coe,glm(I(LeagueIndex>=6)~.-MaxTimeStamp-ComplexAbilityUsed-ComplexUnitsMade-UniqueUnitsMade-SelectByHotkeys-ActionsInPAC, data=train)$coef)
coe=cbind(coe,glm(I(LeagueIndex>=7)~.-MaxTimeStamp-ComplexAbilityUsed-ComplexUnitsMade-UniqueUnitsMade-SelectByHotkeys-ActionsInPAC, data=train)$coef)

mod1=polr(as.factor(LeagueIndex)~ APM + SelectByHotkeys + AssignToHotkeys + 
    UniqueHotkeys + MinimapAttacks + MinimapRightClicks + NumberOfPACs + 
    GapBetweenPACs + ActionLatency + TotalMapExplored + WorkersMade, data=train, Hess=TRUE)
summary(mod1)
stepAIC(mod1)

mod2=polr(as.factor(LeagueIndex)~ APM + SelectByHotkeys + AssignToHotkeys + 
    UniqueHotkeys + MinimapAttacks + NumberOfPACs + 
    GapBetweenPACs + ActionLatency + TotalMapExplored + WorkersMade, data=train, Hess=TRUE)
summary(mod2)
table(predict(mod2,test),test$LeagueIndex)

mod3=polr(as.factor(LeagueIndex)~ APM + SelectByHotkeys + AssignToHotkeys + 
    UniqueHotkeys + MinimapAttacks + MinimapRightClicks + NumberOfPACs + 
    GapBetweenPACs + ActionLatency + TotalMapExplored + WorkersMade +
    I(APM^2) + I(SelectByHotkeys^2) + I(AssignToHotkeys^2) + 
    I(UniqueHotkeys^2) + I(MinimapAttacks^2) + I(MinimapRightClicks^2) + I(NumberOfPACs^2) + 
    I(GapBetweenPACs^2) + I(ActionLatency^2) + I(TotalMapExplored^2) + I(WorkersMade^2), data=train, Hess=TRUE)
summary(mod3)
stepAIC(mod3)

mod4=polr(as.factor(LeagueIndex)~ APM + SelectByHotkeys + AssignToHotkeys + 
    UniqueHotkeys + MinimapAttacks + NumberOfPACs + 
    GapBetweenPACs + ActionLatency + TotalMapExplored + WorkersMade +
    I(APM^2) + I(MinimapAttacks^2) + I(NumberOfPACs^2) + 
    I(ActionLatency^2) + I(TotalMapExplored^2) + I(WorkersMade^2), data=train, Hess=TRUE)
summary(mod4)
table(predict(mod4,test),test$LeagueIndex)

mean(predict(mod2,test)==test$LeagueIndex)
mean(predict(mod4,test)==test$LeagueIndex)
library(irr)
kappa2(cbind(predict(mod2,test),test$LeagueIndex),weight=0:6)
kappa2(cbind(predict(mod4,test),test$LeagueIndex),weight=0:6)

library(caret)
fitControl=trainControl(method = "cv", number = 10)
xgbfit=train(LeagueIndex~., data = train, method="xgbTree", trControl = fitControl, verbose = FALSE)
xgbpred=predict(xgbfit, newdata=test)
xgbpred[xgbpred>7]=7
xgbpred[xgbpred<1]=1
xgbpred=round(xgbpred)
mean(xgbpred==test$LeagueIndex)
kappa2(cbind(xgbpred,test$LeagueIndex),weight=0:6)
#0.408, 0.502 

gbmfit=train(LeagueIndex~., data = train, method="gbm", trControl = fitControl, verbose = FALSE)
gbmpred=predict(gbmfit, newdata=test)
gbmpred[gbmpred>7]=7
gbmpred[gbmpred<1]=1
gbmpred=round(gbmpred)
mean(gbmpred==test$LeagueIndex)
kappa2(cbind(gbmpred,test$LeagueIndex),weight=0:6)
#0.402, 0.508

rrffit=train(LeagueIndex~., data = train, method="RRF", trControl = fitControl, verbose = FALSE)
rrfpred=predict(rrffit, newdata=test)
rrfpred[rrfpred>7]=7
rrfpred[rrfpred<1]=1
rrfpred=round(rrfpred)
mean(rrfpred==test$LeagueIndex)
kappa2(cbind(rrfpred,test$LeagueIndex),weight=0:6)
#0.405, 0.492

svmfit=train(LeagueIndex~., data = train, method="svmPoly", trControl = fitControl, verbose = FALSE)
svmpred=predict(svmfit, newdata=test)
svmpred[svmpred>7]=7
svmpred[svmpred<1]=1
svmpred=round(svmpred)
mean(svmpred==test$LeagueIndex)
kappa2(cbind(svmpred,test$LeagueIndex),weight=0:6)
#0.411, 0.507 