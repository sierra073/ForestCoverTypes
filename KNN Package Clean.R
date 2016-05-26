# Runs the K-nearest neighbors algorithm with k=1 on data set
# Output: matrixKNN - List of misclassification rates from the myN number of run trials
# User-controlled variables: myN (number of trials)

# Define global variables
myN=1 # Number of trials
matrixKNN <- matrix(rep(0,myN),ncol=1) # Matrix of misclassification rates

# Collect data
for (a in 1:myN) {
  
  # Repeated sub-sampling cross-validation prep of the data
  data_rand <- data[sample(nrow(data)),] #Permutate the rows
  Y_rand <- data_rand[,55]
  X_rand <- data_rand[,1:54]
  
  # Define local variables
  training_X <- NULL # Training set data
  training_Y <- NULL # Training set class
  testing_X <- NULL # Testing set data
  testing_Y <- NULL # Testing set class
  # Counters used for determining when training set quotas are met
  cntr1 <- 0
  cntr2 <- 0
  cntr3 <- 0
  cntr4 <- 0
  cntr5 <- 0
  cntr6 <- 0
  cntr7 <- 0
  # Index tracker to facilitate the assignment of all non-training set data to be used in the test set
  index_trackr <- NULL
  #--------EQUAL SAMPLING--------#
  for(i in 1:length(X_rand$elevation)) {
    if (cntr1 == 1620 && cntr2 == 1620 && cntr3 == 1620 && cntr4 == 1620 && cntr5 == 1620 && cntr6 == 1620 && cntr7 == 1620) {
      testing_X <- X_rand[-index_trackr,]
      testing_Y <- Y_rand[-index_trackr]
      break
    } else if (Y_rand[i] == 1 && cntr1 < 1620) {
      training_X <- rbind(training_X, X_rand[i,])
      training_Y <- rbind(training_Y, Y_rand[i])
      cntr1 <- cntr1 + 1
      index_trackr <- c(index_trackr, i)
    } else if (Y_rand[i] == 2 && cntr2 < 1620) {
      training_X <- rbind(training_X, X_rand[i,])
      training_Y <- rbind(training_Y, Y_rand[i])
      cntr2 <- cntr2 + 1
      index_trackr <- c(index_trackr, i)
    } else if (Y_rand[i] == 3 && cntr3 < 1620) {
      training_X <- rbind(training_X, X_rand[i,])
      training_Y <- rbind(training_Y, Y_rand[i])
      cntr3 <- cntr3 + 1
      index_trackr <- c(index_trackr, i)
    } else if (Y_rand[i] == 4 && cntr4 < 1620) {
      training_X <- rbind(training_X, X_rand[i,])
      training_Y <- rbind(training_Y, Y_rand[i])
      cntr4 <- cntr4 + 1
      index_trackr <- c(index_trackr, i)
    } else if (Y_rand[i] == 5 && cntr5 < 1620) {
      training_X <- rbind(training_X, X_rand[i,])
      training_Y <- rbind(training_Y, Y_rand[i])
      cntr5 <- cntr5 + 1
      index_trackr <- c(index_trackr, i)
    } else if (Y_rand[i] == 6 && cntr6 < 1620) {
      training_X <- rbind(training_X, X_rand[i,])
      training_Y <- rbind(training_Y, Y_rand[i])
      cntr6 <- cntr6 + 1
      index_trackr <- c(index_trackr, i)
    } else if (Y_rand[i] == 7 && cntr7 < 1620) {
      training_X <- rbind(training_X, X_rand[i,])
      training_Y <- rbind(training_Y, Y_rand[i])
      cntr7 <- cntr7 + 1
      index_trackr <- c(index_trackr, i)
    }
  }
  
  #--------PROPORTIONAL SAMPLING--------#
  #for(i in 1:length(X_rand$elevation)) {
  #  if (cntr1 == 4135 && cntr2 == 5530 && cntr3 == 698 && cntr4 == 53 && cntr5 == 185 && cntr6 == 339 && cntr7 == 400) {
  #    testing_X <- X_rand[-index_trackr,]
  #    testing_Y <- Y_rand[-index_trackr]
  #    break
  #  } else if (Y_rand[i] == 1 && cntr1 < 4135) {
  #    training_X <- rbind(training_X, X_rand[i,])
  #    training_Y <- rbind(training_Y, Y_rand[i])
  #    cntr1 <- cntr1 + 1
  #    index_trackr <- c(index_trackr, i)
  #  } else if (Y_rand[i] == 2 && cntr2 < 5530) {
  #    training_X <- rbind(training_X, X_rand[i,])
  #    training_Y <- rbind(training_Y, Y_rand[i])
  #    cntr2 <- cntr2 + 1
  #    index_trackr <- c(index_trackr, i)
  #  } else if (Y_rand[i] == 3 && cntr3 < 698) {
  #    training_X <- rbind(training_X, X_rand[i,])
  #    training_Y <- rbind(training_Y, Y_rand[i])
  #    cntr3 <- cntr3 + 1
  #    index_trackr <- c(index_trackr, i)
  #  } else if (Y_rand[i] == 4 && cntr4 < 53) {
  #    training_X <- rbind(training_X, X_rand[i,])
  #    training_Y <- rbind(training_Y, Y_rand[i])
  #    cntr4 <- cntr4 + 1
  #    index_trackr <- c(index_trackr, i)
  #  } else if (Y_rand[i] == 5 && cntr5 < 185) {
  #    training_X <- rbind(training_X, X_rand[i,])
  #    training_Y <- rbind(training_Y, Y_rand[i])
  #    cntr5 <- cntr5 + 1
  #    index_trackr <- c(index_trackr, i)
  #  } else if (Y_rand[i] == 6 && cntr6 < 339) {
  #    training_X <- rbind(training_X, X_rand[i,])
  #    training_Y <- rbind(training_Y, Y_rand[i])
  #    cntr6 <- cntr6 + 1
  #    index_trackr <- c(index_trackr, i)
  #  } else if (Y_rand[i] == 7 && cntr7 < 400) {
  #    training_X <- rbind(training_X, X_rand[i,])
  #    training_Y <- rbind(training_Y, Y_rand[i])
  #    cntr7 <- cntr7 + 1
  #    index_trackr <- c(index_trackr, i)
  #  }
  #}
  #--------SAMPLING END--------#
  
  # Comment the following 2 lines if you want to test on the whole data set
  testing_X <- testing_X[1:11340,] # Test set is limited to 11,340 observations
  testing_Y <- testing_Y[1:11340]
  
  # Model Building
  K = 1 #Max number of nearest neighbors
  pred = knn(train=training_X,test=testing_X,cl=training_Y, k=K)
  
  # Misclassification Rate
  matrixKNN[a] <- 100*sum(pred != testing_Y)/length(pred)
}

# Display the misclassification error
print(sprintf("misclassification rate %f percent", mean(matrixKNN)))

# Confusion matrix
table(testing_Y, pred)