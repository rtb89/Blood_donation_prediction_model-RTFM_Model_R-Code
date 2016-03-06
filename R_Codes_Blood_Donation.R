donation = read.table("Blood_donation.txt", header = TRUE)     #Loading the data set
dim(donation)
names(donation)
plot(don)
library('readr')
library('corrplot')
corrplot.mixed(cor(donation), upper= "ellipse")                    #Plotting the correlation plot
par(bty = 'n')
donT <-donation[, c("Recency", "Frequency", "Time"), drop=FALSE]

boxplot(donT, boxwex = 0.28, at = 1:3 - 0.2, col = "light blue",  main = "Blood Donation", xlab = "", ylab = "", cex.axis=0.85,bty="n", xaxt="n" )                                                                                                 #Box Plot for the three attributes
axis(1,side = 1, at=c(1,2,3),labels=c('Recency','Frequency','Time'),tick=FALSE)

# Creating a table of Class with each variable

Ty = table (Recency, Class)
Ty = cbind (Ty, Ty[,2]/(Ty[,1] + Ty [,2]))     #Calculate the probability of occurrence of each class for every value of the Recency
plot(as.numeric(row.names(Ty)), Ty[, 3], xlab = "Recency", ylab = "Probability of a blood donation", pch = 19, cex = 0.5)

Ty1 = table(Frequency, Class)
Ty1 = cbind(Ty1, Ty1[,2]/(Ty1[,1] + Ty1[,2]))
plot(as.numeric(row.names(Ty1)), Ty1[, 3], xlab = "Frequency", ylab = "Probability of a blood donation", pch = 19, cex = 0.5)

Ty2 = table (Monetary, Class)
Ty2 = cbind (Ty2, Ty2[,2]/(Ty2[,1] + Ty2[,2]))
plot(as.numeric( row.names(Ty2)), Ty2[, 3], xlab = "Monetary", ylab = "Probability of a blood donation", pch = 19, cex = 0.5)

Ty3 = table(Time, Class)
Ty3 = cbind(Ty3, Ty3[,2]/(Ty3[,1] + Ty3[,2]))
plot(as.numeric(row.names(Ty3)), Ty3[, 3], xlab = "Time", ylab = "Probability of a blood donation", pch = 19, cex = 0.5)


#  Transformation of variables
plot(log(as.numeric(row.names(Ty1))), Ty1[, 3], xlab = "Log(Frequency)", ylab = "Probability of a blood donation", pch = 19, cex = 0.5)
summary(glm(Class ~  Recency + log(Frequency)+ Time, data=donation, family = binomial(link = "logit")))
plot(1/log(as.numeric(row.names(Ty1))), Ty1[, 3], xlab = "1/Log(Frequency)", ylab = "Probability of a blood donation", pch = 19, cex = 0.5)
summary(glm(Class ~  Recency + 1/log(Frequency)+ Time, data=donation, family = binomial(link = "logit")))

plot(exp(as.numeric(row.names(Ty1))), Ty1[, 3], xlab = "exp(Frequency)", ylab = "Probability of a blood donation", pch = 19, cex = 0.5)
summary(glm(Class ~  Recency + exp(Frequency)+ Time, data=donation, family = binomial(link = "logit")))

plot(log(as.numeric(row.names(Ty))), Ty[, 3], xlab = "log(Recency)", ylab = "Probability of a blood donation", pch = 19, cex = 0.5)
summary(glm(Class ~  log(Recency) + Frequency+ Time, data=donation, family = binomial(link = "logit")))


# Adding a new variable RBD to the donation data frame
donation$RBD <- 0
donation$RBD[ (Recency <= 6 )& (Frequency >= 4) & (Time> 24)]<- 1


#Comparing Logit and Probit link function in logistic regression model
libray(pscl)
modl<- glm(Class ~  Recency + log(Frequency)+ Time, data=donation, family = binomial(link = "logit")))
summary(modl)
anova(modl, test= "Chisq")
pR2(modl)
modn<- glm (Class ~  Recency + log(Frequency)+ Time, data=donation, family = binomial(link = "Probit")))
summary(modn)
anova(modl, test= "Chisq")
pR2(modn)

# Logistic Regression Model
modl<- glm(Class ~  Recency+log(Frequency)+Time+RBD, data=donation, family = binomial(link = "logit"))
summary(modl)
anova (modl,test="Chisq")
pred <- predict(modl,newdata=donation)
predTest2<-ifelse(pred >= 0.35,1,0)
confusionMatrix(data=predTest2, Class)

  # Create partition in the data
  t_data <- createDataPartition(donation$Class, p=0.8, list=FALSE)
  train <- donation[t_data,]
  test <- donation[-t_data,]

  # Build the logistic regression model on Train Data
  modl= glm(Class ~  Recency + log(Frequency)+ Time+RBD , data=train, family = binomial(link = "logit"))
  # Predict the probability of the class of the test data
  lrtest<-predict(modl,newdata = test, type="response")
  # Convert the probability class value to the class value using the cutoff 0.35
  result1<-ifelse(lrtest<0.35,0,1)
  pr1 <-prediction(lrtest, test$Class)
  #Plot the Precission Recall chart
  prf1 <- performance(pr1, measure = "prec", x.measure = "rec")
  plot(prf1)
  # Calculate the area under the curve
  auc1 <- performance(pr1, measure = "auc")
  auc1 <- auc1@y.values[[1]]
  auc1
  # Calculate the confusion matrix for LR
  confusionMatrix(result1,test$Class)
  # Plot the Q-Q plot, Residual Plot
  plot(modl, which=1)
  plot(modl, which=2)
  plot(modl, which=3)
  plot(modl, which=4)



  # Fit the Random Forest Model on Training Data
  fit <- randomForest(as.factor(Class) ~ Recency + Frequency + Time, data=train, importance=TRUE, ntree=2000)
  rftest<-predict(fit,newdata = test, type="response")
  pr2 <-prediction(rftest, test$Class)
  auc2 <- performance(pr2, measure = "auc")
  auc2 <- auc2@y.values[[1]]
  auc2
  prf2 <- performance(pr2, measure = "prec", x.measure = "rec")
  plot(prf2)
  confusionMatrix(rftest,test$Class)

  getTree(fit, k=1, labelVar=TRUE)



  # Fit the PART model on the training data
  fitrp <- rpart(Class ~ Recency + Frequency + Time, data=train)
  rptest<-predict(fitrp,newdata = test)
  pr3 <-prediction(rptest, test$Class)
  prf3 <- performance(pr3, measure = "prec", x.measure = "rec")
  plot(prf3)
  result2<-ifelse(lrtest<0.35,0,1)
  confusionMatrix(result2, test$Class)
  auc2 <- performance(pr3, measure = "auc")
  auc2 <- auc2@y.values[[1]]
  auc2
  # Plot relative error with varying CP
  plotcp(fitrp)
  # Summary of the PART Model built
  summary(fitrp)
  # Plot the tree built
  plot(fitrp, uniform=TRUE, main="Classification Tree for Donation")
  text(fitrp, use.n=TRUE, all=TRUE, cex=.8)
  # Pruning the tree
  pfit<- prune(fitrp, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
  # Plot the pruned tree
  plot(pfit, uniform=TRUE,main="Pruned Classification Tree for Donation")
  text(pfit, use.n=TRUE, all=TRUE, cex=.8)
  # Predict the result using the pruned model
  rptest<-predict(pfit,newdata = test)
  result3<-ifelse(lrtest<0.35,0,1)
  confusionMatrix(result3, test$Class)

  # Plotting the Precision-Recall Chart
  plot(prf1,col="red")
  plot(prf3, add=TRUE, col="blue")
  legend("topright",c("PART","Logistic Regression"),lty=c(1,1),lwd=c(2.5,2.5), col=c("blue","red"))

  #Plotting the Recall_precision Chart
  prf4 <- performance(pr1, measure = "tpr", x.measure = "fpr")
  prf5 <- performance(pr3, measure = "tpr", x.measure = "fpr")
  plot(prf4,col="red")
  plot(prf5, add=TRUE, col="blue")
  legend("bottomright",c("PART","Logistic Regression"),lty=c(1,1),lwd=c(3.5,3.5), col=c("blue","red"))

  # Adding interaction Terms and comparing models

  # Model1
  modl1= glm(Class ~  Recency + log(Frequency)+ Time+RBD+ RBD* Recency , data=train, family = binomial(link = "logit"))
  # Predict the probability of the class of the test data
  lrtest<-predict(modl1,newdata = test, type="response")
  # Convert the probability class value to the class value using the cutoff 0.35
  result1<-ifelse(lrtest<0.35,0,1)
  pr1 <-prediction(lrtest, test$Class)
  #Plot the Precission Recall chart
  prf1 <- performance(pr1, measure = "prec", x.measure = "rec")
  # Calculate the area under the curve
  auc1 <- performance(pr1, measure = "auc")
  auc1 <- auc1@y.values[[1]]
  auc1
  # Calculate the confusion matrix for LR
  confusionMatrix(result1,test$Class)
  pR2(modl1)
  summary(modl1)

  # Model 2
  modl2= glm(Class ~  Recency +log(Frequency)+ Time+RBD+ RBD* log(Frequency)+ RBD*Time , data=train, family = binomial(link = "logit"))
  # Predict the probability of the class of the test data
  lrtest<-predict(modl2,newdata = test, type="response")
  # Convert the probability class value to the class value using the cutoff 0.35
  result1<-ifelse(lrtest<0.35,0,1)
  pr1 <-prediction(lrtest, test$Class)
  #Plot the Precission Recall chart
  prf1 <- performance(pr1, measure = "prec", x.measure = "rec")
  # Calculate the area under the curve
  auc1 <- performance(pr1, measure = "auc")
  auc1 <- auc1@y.values[[1]]
  auc1
  # Calculate the confusion matrix for LR
  confusionMatrix(result1,test$Class)
  pR2(modl2)
  summary(modl2)

  # Model3
  modl3= glm(Class ~  Recency + (Frequency)+ Time+RBD+RBD*(Time) +RBD*(Frequency), data=train, family = binomial(link = "logit"))
  # Predict the probability of the class of the test data
  lrtest3<-predict(modl3,newdata = test, type="response")
  # Convert the probability class value to the class value using the cutoff 0.35
  result4<-ifelse(lrtest3<0.35,0,1)
  pr1 <-prediction(lrtest3, test$Class)
  #Plot the Precission Recall chart
  prf1 <- performance(pr1, measure = "prec", x.measure = "rec")
  # Calculate the area under the curve
  auc1 <- performance(pr1, measure = "auc")
  auc1 <- auc1@y.values[[1]]
  auc1
  # Calculate the confusion matrix for LR
  confusionMatrix(result4,test$Class)
  pR2(modl3)
  summary(modl3)


  # Model4
  modl4= glm(Class ~  Recency +log (Frequency)+ Time+RBD+RBD*(Time) +RBD*(Frequency), data=train, family = binomial(link = "logit"))
  # Predict the probability of the class of the test data
  lrtest3<-predict(modl3,newdata = test, type="response")
  # Convert the probability class value to the class value using the cutoff 0.35
  result4<-ifelse(lrtest3<0.35,0,1)
  pr1 <-prediction(lrtest3, test$Class)
  #Plot the Precission Recall chart
  prf1 <- performance(pr1, measure = "prec", x.measure = "rec")
  # Calculate the area under the curve
  auc1 <- performance(pr1, measure = "auc")
  auc1 <- auc1@y.values[[1]]
  auc1
  # Calculate the confusion matrix for LR
  confusionMatrix(result4,test$Class)
  pR2(modl4)
  summary(modl4)

  # Model0
  modl0= glm(Class ~  Recency +log (Frequency)+ Time+RBD, data=train, family = binomial(link = "logit"))
# Predict the probability of the class of the test data
lrtest3<-predict(modl0,newdata = test, type="response")
# Convert the probability class value to the class value using the cutoff 0.35
result4<-ifelse(lrtest3<0.35,0,1)
pr1 <-prediction(lrtest3, test$Class)
#Plot the Precission Recall chart
prf1 <- performance(pr1, measure = "prec", x.measure = "rec")
# Calculate the area under the curve
auc1 <- performance(pr1, measure = "auc")
auc1 <- auc1@y.values[[1]]
auc1
# Calculate the confusion matrix for LR
confusionMatrix(result4,test$Class)
pR2(modl0)
summary(modl0)


