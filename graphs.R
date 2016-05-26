#Code for charts on slides 6,8 and 22 of the presentation

#p.6 (data only, used excel to make it look nicer)

counts=c(rep(0,7))
for (i in 1:7) {
  counts[i]=sum(data$Class==i)
}
counts

#p.8

corr=cor(data)
levelplot(corr)

#p.22

#data frames for linear and proportional approach average errors, along with the neural 
#networks approach in the 1999 paper for comparison
Linear=data.frame(Methods=c("LDA","1-NN","Log. Reg.","Naive Bayes","Tree","Forest","N.N.('99)"),
                  Errors=c(42.6680782,34.703704,60.910053,54.604057,57.087,28.357,29.42))
Proportional=data.frame(Methods=c("LDA","1-NN","Log. Reg.","Naive Bayes","Tree","Forest","N.N.('99)"),
                  Errors=c(32.508818,19.799825,39.483246,57.24074,32.922,17.658,29.42))

#set colors to match presentation theme
colors=c("chartreuse3","chartreuse4","darkgoldenrod1","darkorange","darkred","burlywood4","black")

#plot 2 bar charts
barplot(Linear$Errors,names.arg=c("LDA","1-NN","Log. Reg.","Naive Bayes","Tree","Forest","N.N.('99)"),
        cex.names=.7,ylab="Misclassification Error (%)",
        col=colors,main="Linear")
barplot(Proportional$Errors,names.arg=c("LDA","1-NN","Log. Reg.","Naive Bayes","Tree","Forest","N.N.('99)"),
        cex.names=.7,ylab="Misclassification Error (%)",
        col=colors,main="Proportional")
