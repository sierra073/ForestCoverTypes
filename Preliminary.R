#Code to load the dataset and install/open packages we used

#load data
data <- read.csv("covtype_labeled.csv", header= T, sep= ",", quote= "", dec= ".", fill= FALSE, comment.char= "")

#packages

library(class)
library(MASS)

install.packages(c("e1071","rpart","randomForest","lattice"))
library(e1071)
library(rpart)
library(randomForest)
library(lattice)