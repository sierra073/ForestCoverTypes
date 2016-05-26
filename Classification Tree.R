
#Builds a classification tree for both equal and prop. training set sampling methods, 
#using information as the splitting criteria.
#A note on pruning: The algorithm performs a cross validation on the training set and
#selects an optimal complexity parameter (cp) associated with the minimum cross-validated
#error. "Any split that does not decrease the overall lack of fit by a factor of cp is not 
#attempted."

#1 simuulation both an equal and proportional sampling approach (we ran 50 but it takes hours).
#Also reduced the size of the testing set from 569,672 to 5,670 and made the size of the
#training set 5,670 to reduce computing time. Reducing the size of the testing set so much does
#not give many informative results, however, as you will see.
myN=1 #number of trials
errs=errsprop=c() #vector of the misclassification errors across trials

#collect data
for (k in 1:myN){
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
  # Index tracker to facilitate the assignment of all non-training set data to be used in the test set
    index_trackr <- NULL
  
  for(i in 1:length(X_rand[,1])) {
    #--------EQUAL SAMPLING---------#
    if(cntr1 == 810 && cntr2 == 810 && cntr3 == 810 && cntr4 == 810 && cntr5 == 810 && cntr6 == 810 && cntr7 == 810) {
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
  # Comment the following 2 lines if you want to test on the whole data set
  testing_X=testing_X[1:5670,] # Test set is limited to 5,670 observations
  testing_Y=testing_Y[1:5670]
  
  train=cbind(training_Y,training_X)
  test=cbind(testing_Y,testing_X)
  #-------PROPORTIONAL SAMPLING-----------#
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
  #Comment the following 2 lines if you want to test on the whole data set
  testing_Yprop=testing_Yprop[1:5670]
  testing_Xprop=testing_Xprop[1:5670,]
  
  trainprop=cbind(training_Yprop,training_Xprop)
  testprop=cbind(testing_Yprop,testing_Xprop)
  #--------SAMPLING END--------#
  
  #EQUAL model building
  fulltree <- rpart(as.factor(training_Y)~.,data=train,method=c("class"),parms = list(split = "information"))
  ypre<-predict(fulltree, test,type=c("class"))
  errs[k]=100*sum(ypre != testing_Y)/length(ypre)
  
  #PROPORTIONAL model building
  fulltreeprop <- rpart(as.factor(training_Yprop)~.,data=trainprop,method=c("class"),parms = list(split = "information"))
  ypreprop<-predict(fulltreeprop, testprop,type=c("class"))
  errsprop[k]=100*sum(ypreprop!= testing_Yprop)/length(ypreprop)
  
}


#Plot equal tree model
plot(fulltree,uniform=F,main="Classification Tree for Cover Type",margin=0.009,branch=.8)
text(fulltree, all=TRUE, cex=.69, col="blue")

#Display the misclassification error
print(sprintf("misclassification rate %f percent",
              mean(errs)))
sd(errs)
print(sprintf("misclassification rate %f percent",
              mean(errsprop)))
sd(errsprop)

#confusion matrices
#equal
table(testing_Y,ypre)
#proportional
table(testing_Yprop,ypreprop)


