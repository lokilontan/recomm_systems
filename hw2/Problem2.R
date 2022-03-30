library(regtools)
library(rectools)
library(qeML)

# collect everything Hwk2.RData
#ml100kpluscovs <- getML100K(FALSE)
#InstEval <- getInstEval()
#hv <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/voting-records/house-votes-84.data"))
#save(ml100kpluscovs, InstEval, hv, file = "Hwk2.RData")
load("Hwk2.RData") #ml100kpluscovs, InstEval, hv

# MovieLens
ml <- ml100kpluscovs
ml$user <- as.factor(ml100kpluscovs$user)
ml$item <- as.factor(ml100kpluscovs$item)
#ml <- head(ml, n=10000)
#ml$rating <- as.factor(ml100kpluscovs$rating)

#LINEAR
#X=(user, item); Y=rating
ml_init <- ml[,1:3]
print("qeLin: X=(user, item); Y=rating")
print(qeLin(ml_init,'rating')$testAcc) #0.7633097
print("qePoly: X=(user, item); Y=rating")
print(qePolyLin(ml_init[1:10000, ],'rating')$testAcc)

#X=(user, item, userMean, itemMean); Y=rating
ml_small <- ml[,c(1:3,9,30)]
print("qeLin: X=(user, item, userMean, itemMean); Y=rating")
print(qeLin(ml_small,'rating')$testAcc)
print("qePoly: X=(user, item, userMean, itemMean); Y=rating")
print(qePolyLin(ml_small,'rating')$testAcc)

# X=(userMean, itemMean); Y=rating
ml_smaller <- ml[,c(3,9,30)]
print("qeLin: X=(userMean, itemMean); Y=rating")
print(qeLin(ml_smaller,'rating')$testAcc)
print("qePoly: X=(userMean, itemMean); Y=rating")
print(qePolyLin(ml_smaller,'rating')$testAcc)

#LOGISTIC
ml_log <- ml[,c('item','itemMean','G2')]
ml_log$G2 <- as.factor(ml_log$G2)
print(qeLogit(ml_log,'G2')$testAcc)  

#InstEval
#preprocess InstEval ds
ie <- InstEval
ie[,1] <- as.factor(ie[,1]) 
ie[,2] <- as.factor(ie[,2]) 

#LINEAR
#X=(user(s), item(d)); Y=rating(y)
print("qeLin: X=(user(s), item(d)); Y=rating(y)")
print(qeLin(ie[c(1:40000),c(1:3)],'y')$testAcc) #1.011209
print("qePoly: X=(user(s), item(d)); Y=rating(y)")
print(qePolyLin(ie[c(1:10000),c(1:3)],'y')$testAcc)

#X=(user(s), item(d), dept); Y=rating(y)
print("qePoly: X=(user(s), item(d), dept); Y=rating(y)")
print(qeLin(ie[1:10000, c(1:3, 7)], "y")$testAcc)

#LOGISTIC
ie$y <- as.factor(ie$y)
#X=(user(s), item(d), dept); Y=rating(y)
print("qeLogit: X=(user(s), item(d), dept); Y=rating(y)")
print(qeLogit(ie[1:10000, c(1:3, 7)], "y")$testAcc)

#HouseVoting
#preprocess HouseVoting ds
isRepublican <- hv[, 1] == "republican"
hv[, 1] <- isRepublican
for(i in 1:length(hv)) {
  tmp <- hv[, i]
  hv[, i] <- as.factor(tmp)
}

#X=all columns; Y=y.4
print("HouseVoting logistic model on all factors")
print(replicMeans(10, "qeLogit(hv, 'y.4')$testAcc")) #0.4883721
print(qeLogit(hv,'republican')$glmOuts)  #useful for exploring coefficients

#X=(n,y,n.1,y.2,n.4,y.4); Y=republican
small_hv <- hv[, c('republican','n','y','n.1','y.2','n.4','y.4')] 
print("HouseVoting logistic model on best features, predict republicans")
print(replicMeans(10, "qeLogit(small_hv,'republican')$testAcc")) #0.144186


