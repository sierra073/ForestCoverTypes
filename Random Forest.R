#This runs the random forest algorithm with 300 trees for both equal and proportional
#training set sampling methods. NOTE: training and test set sizes were reduced by half (11,340-->5,670)
#for computing time purposes in case you run it. However, this changes the results a bit...

#number of variables to sample each time a tree is built in the forest. One random forest 
#will be built for each value in this vector:
mtries=c(8,10,13,15,16,19,24,34,54)

#number of simulations to run (we ran 20 but it takes hours)
myN=1

#vectors/matrices of the classification errors that will be outputted
errse=errsp=c()
errs20e=errs20p=matrix(0,9,myN)

#collect data
for (k in 1:myN) {
  # Repeated sub-sampling cross-validation prep of the data
  data_rand <- data[sample(nrow(data)),] #Permutate the rows
  Y_rand <- data_rand[,55]
  X_rand <- data_rand[,1:54]
  training_X <- NULL # Training set data
  training_Y <- NULL # Training set class
  testing_X <- NULL # Testing set data
  testing_Y <- NULL # Testing set class
  # Counters used for determing when training set quotas are met
  cntr1 <- 0
  cntr2 <- 0
  cntr3 <- 0
  cntr4 <- 0
  cntr5 <- 0
  cntr6 <- 0
  cntr7 <- 0
  cntrTest <- 0
  # Index tracker to facilitate the assignment of all non-training set data to be used in the test set
    index_trackr <- NULL
  for(i in 1:length(X_rand[,1])) {
    #---------------EQUAL SAMPLING----------------#
    if (cntr1 == 810 && cntr2 == 810 && cntr3 == 810 && cntr4 == 810 && cntr5 == 810 && cntr6 == 810 && cntr7 == 810) {
      testing_X <- X_rand[-index_trackr,]
      testing_Y <- Y_rand[-index_trackr]
      break
    } else if (Y_rand[i] == 1 && cntr1 < 810) {
      training_X <- rbind(training_X, X_rand[i,])
      training_Y <- rbind(training_Y, Y_rand[i])
      cntr1 <- cntr1 + 1
      index_trackr <- c(index_trackr, i)
    } else if (Y_rand[i] == 2 && cntr2 < 810) {
      training_X <- rbind(training_X, X_rand[i,])
      training_Y <- rbind(training_Y, Y_rand[i])
      cntr2 <- cntr2 + 1
      index_trackr <- c(index_trackr, i)
    } else if (Y_rand[i] == 3 && cntr3 < 810) {
      training_X <- rbind(training_X, X_rand[i,])
      training_Y <- rbind(training_Y, Y_rand[i])
      cntr3 <- cntr3 + 1
      index_trackr <- c(index_trackr, i)
    } else if (Y_rand[i] == 4 && cntr4 < 810) {
      training_X <- rbind(training_X, X_rand[i,])
      training_Y <- rbind(training_Y, Y_rand[i])
      cntr4 <- cntr4 + 1
      index_trackr <- c(index_trackr, i)
    } else if (Y_rand[i] == 5 && cntr5 < 810) {
      training_X <- rbind(training_X, X_rand[i,])
      training_Y <- rbind(training_Y, Y_rand[i])
      cntr5 <- cntr5 + 1
      index_trackr <- c(index_trackr, i)
    } else if (Y_rand[i] == 6 && cntr6 < 810) {
      training_X <- rbind(training_X, X_rand[i,])
      training_Y <- rbind(training_Y, Y_rand[i])
      cntr6 <- cntr6 + 1
      index_trackr <- c(index_trackr, i)
    } else if (Y_rand[i] == 7 && cntr7 < 810) {
      training_X <- rbind(training_X, X_rand[i,])
      training_Y <- rbind(training_Y, Y_rand[i])
      cntr7 <- cntr7 + 1
      index_trackr <- c(index_trackr, i)
    }
  }
  #Comment the following 2 lines if you want to test on the whole data set
  testing_X <- testing_X[1:5670,]
  testing_Y <- testing_Y[1:5670]
  
  train=cbind(training_Y,training_X)
  test=cbind(testing_Y,testing_X)
  names(train)[c(1)]='Class'
  names(test)[c(1)]='Class'
  train[,"Class"]=as.factor(train[,"Class"])
  #----------------PROPORTIONAL SAMPLING------------------#
  training_Xprop <- NULL
  training_Yprop <- NULL
  testing_Xprop <- NULL
  testing_Yprop <- NULL
  cntr1 <- 0
  cntr2 <- 0
  cntr3 <- 0
  cntr4 <- 0
  cntr5 <- 0
  cntr6 <- 0
  cntr7 <- 0
  index_trackr <- NULL
  for(i in 1:length(X_rand[,1])) {
    if (cntr1 == 2067 && cntr2 == 2765 && cntr3 == 349 && cntr4 == 27 && cntr5 == 92 && cntr6 == 170 && cntr7 == 200) {
      testing_Xprop <- X_rand[-index_trackr,]
      testing_Yprop <- Y_rand[-index_trackr]
      break
    } else if (Y_rand[i] == 1 && cntr1 < 2067) {
      training_Xprop <- rbind(training_Xprop, X_rand[i,])
      training_Yprop <- rbind(training_Yprop, Y_rand[i])
      cntr1 <- cntr1 + 1
      index_trackr <- c(index_trackr, i)
    } else if (Y_rand[i] == 2 && cntr2 < 2765) {
      training_Xprop <- rbind(training_Xprop, X_rand[i,])
      training_Yprop <- rbind(training_Yprop, Y_rand[i])
      cntr2 <- cntr2 + 1
      index_trackr <- c(index_trackr, i)
    } else if (Y_rand[i] == 3 && cntr3 < 349) {
      training_Xprop <- rbind(training_Xprop, X_rand[i,])
      training_Yprop <- rbind(training_Yprop, Y_rand[i])
      cntr3 <- cntr3 + 1
      index_trackr <- c(index_trackr, i)
    } else if (Y_rand[i] == 4 && cntr4 < 27) {
      training_Xprop <- rbind(training_Xprop, X_rand[i,])
      training_Yprop <- rbind(training_Yprop, Y_rand[i])
      cntr4 <- cntr4 + 1
      index_trackr <- c(index_trackr, i)
    } else if (Y_rand[i] == 5 && cntr5 < 92) {
      training_Xprop <- rbind(training_Xprop, X_rand[i,])
      training_Yprop <- rbind(training_Yprop, Y_rand[i])
      cntr5 <- cntr5 + 1
      index_trackr <- c(index_trackr, i)
    } else if (Y_rand[i] == 6 && cntr6 < 170) {
      training_Xprop <- rbind(training_Xprop, X_rand[i,])
      training_Yprop <- rbind(training_Yprop, Y_rand[i])
      cntr6 <- cntr6 + 1
      index_trackr <- c(index_trackr, i)
    } else if (Y_rand[i] == 7 && cntr7 < 200) {
      training_Xprop <- rbind(training_Xprop, X_rand[i,])
      training_Yprop <- rbind(training_Yprop, Y_rand[i])
      cntr7 <- cntr7 + 1
      index_trackr <- c(index_trackr, i)
    }
  }
  #--------SAMPLING END--------#
  
  #Comment the following 2 lines if you want to test on the whole data set
  testing_Yprop=testing_Yprop[1:5670]
  testing_Xprop=testing_Xprop[1:5670,]
  
  trainprop=cbind(training_Yprop,training_Xprop)
  testprop=cbind(testing_Yprop,testing_Xprop)
  names(trainprop)[c(1)]='Class'
  names(testprop)[c(1)]='Class'
  trainprop[,"Class"]=as.factor(trainprop[,"Class"])
  
  #build models and test 
  for (i in 1:9){
    #equal
    rf=randomForest(Class~.,data=train, importance=T,ntree=300,mtry=mtries[i])
    ypre=predict(rf,test)
    errse[i]=100*sum(ypre!= testing_Y)/length(ypre)
    #proportional
    rfprop=randomForest(Class~.,data=trainprop, importance=T,ntree=300,mtry=mtries[i])
    ypreprop=predict(rfprop,testprop)
    errsp[i]=100*sum(ypreprop!= testing_Yprop)/length(ypreprop)
  }
  errs20e[,k]=errse
  errs20p[,k]=errsp
}

#compute the average misclassification error among each of the 9 forests accross the myN number of simulations
avgerrse=c()
avgerrsp=c()
for (i in 1:9){
  avgerrse[i]=mean(errs20e[i,])
  avgerrsp[i]=mean(errs20p[i,])
}

#display the misclassification rates for each sampling method
#(minimum average error among 9 feature sampling tries)
print(sprintf("misclassification rate %f percent",
              min(avgerrse)))
print(sprintf("misclassification rate %f percent",
              min(avgerrsp)))

#confusion matrices
#equal
table(testing_Y,ypre)
#proportional
table(testing_Yprop,ypreprop)

#Plot out-of-bag (OOB) misclassification error by number of trees in a forest 
#(misclass. error from the algorithm's holding 1/3 of the bootstrapped (bagged) data from 
#the training set out each time for testing). Use this to determine the optimal number of trees in the forest. (done before simulations were run)
plot(rf, main="OOB Error by Number of Trees")

#Misclass. error on one model per sampling method by "mtry" (#features sampled)
mnfl=data.frame(features=mtries,error=avgerrse)
mnfp=data.frame(features=mtries,error=avgerrsp)
plot(mnfl,type="l",col="red",main="Average Misclassification Error by # Features Sampled",cex.main=.85)
par(new=T)
plot(mnfp,type="l",col="blue",axes = FALSE, xlab = "", ylab = "")
legend("topright", c("Linear","Proportional"),lty=c(1,1),  
       lwd=c(2.5,2.5),col=c("red","blue"),cex=.8)

#variable importance plot for equal sampling
varImpPlot(rf,n.var=22,main="Variable Importance")




