#To get the location of working directory
getwd()

#To set the location of working directory
setwd("C:\\Users\\Sai Krishna\\Desktop\\acaprojects")

#Loading Training data 
letter<-read.csv("Letter.csv",stringsAsFactors = FALSE,header = FALSE)

#To display structure of the data
str(letter)

#Function to normalize all continuous data attributes
normalize<-function(x)
{
  return ((x-min(x))/(max(x)-min(x)))
}

#Apply the above normalize function to all attributes excluding the letter attribute as it is a categorical variable
lenorm<-(as.data.frame(lapply(letter[,-1],normalize)))

#Column Binding the letter attribute and all other continous attributes
letternorm<-cbind(letter[,1],lenorm)
letternorm

#Setting column names to each attribute
colnames(letternorm)<-c("lettr","x-box","y-box","width","high","onpix","x-bar","y-bar","x2bar","y2bar","xybar","x2ybr","xy2br","x-ege","xegvy","y-ege","yegvx")

#Set seed to a random number
set.seed(46)

#Calculating the number of rows required for training purpose (60% of total data)
trainingdatasize<-ceiling(0.6*nrow(letternorm))

#Calculating the number of rows required for validation purpose (20% of total data)
valdatasize<-ceiling(0.2*nrow(letternorm))

#Calculating the number of rows required for testing purpose (20% of total data)
testdatasize<-ceiling((0.2*nrow(letternorm)))

#Sampling(Randomly selecting) the training data indexes 
letter_train_index<-sample(1:nrow(letternorm),trainingdatasize)

#Sampling(Randomly selecting) the validation data indexes
letter_val_index<-sample(1:nrow(letternorm),valdatasize)

#Sampling(Randomly selecting) the testing data indexes
letter_test_index<-sample(1:nrow(letternorm),testdatasize)

#Training Data 
letter_train_data<-letternorm[letter_train_index,]

#Validation Data
letter_val_data<-letternorm[letter_val_index,]

#Testing Data
letter_test_data<-letternorm[letter_test_index,]

install.packages("C50")
library(C50)

#Attaching the letternorm 
attach(letternorm)

#Installing required packages
install.packages("e1071")
library(e1071)

#################################### Training Data ##############################################


#Model building using C5.0 algorithm with minimal leaf node size 250 
letter_model250<-C5.0(letter_train_data[,-1],letter_train_data$lettr,control=C5.0Control(minCases=250))

#Predicting the training data with the model created
letter_pred_train250<-predict(letter_model250,letter_train_data)

#Confusion matrix between observed and expected results
confusionMatrix(letter_pred_train250,letter_train_data$lettr)

#Finding accuracy for the model with min leaf node 250
letter_train_acc250<-mean(ifelse(letter_pred_train250==letter_train_data$lettr,1,0))*100
letter_train_acc250

# Repeating the above steps with training data for different min leaf node size 

#Min leaf node size = 225
letter_model225<-C5.0(letter_train_data[,-1],letter_train_data$lettr,control=C5.0Control(minCases=225))
letter_pred_train225<-predict(letter_model225,letter_train_data)
confusionMatrix(letter_pred_train225,letter_train_data$lettr)
letter_train_acc225<-mean(ifelse(letter_pred_train225==letter_train_data$lettr,1,0))*100
letter_train_acc225

#Min leaf node size = 200
letter_model200<-C5.0(letter_train_data[,-1],letter_train_data$lettr,control=C5.0Control(minCases=200))
letter_pred_train200<-predict(letter_model200,letter_train_data)
confusionMatrix(letter_pred_train200,letter_train_data$lettr)
letter_train_acc200<-mean(ifelse(letter_pred_train200==letter_train_data$lettr,1,0))*100
letter_train_acc200

#Min leaf node size = 175
letter_model175<-C5.0(letter_train_data[,-1],letter_train_data$lettr,control=C5.0Control(minCases=175))
letter_pred_train175<-predict(letter_model175,letter_train_data)
confusionMatrix(letter_pred_train175,letter_train_data$lettr)
letter_train_acc175<-mean(ifelse(letter_pred_train175==letter_train_data$lettr,1,0))*100
letter_train_acc175

#Min leaf node size = 150
letter_model150<-C5.0(letter_train_data[,-1],letter_train_data$lettr,control=C5.0Control(minCases=150))
letter_pred_train150<-predict(letter_model150,letter_train_data)
confusionMatrix(letter_pred_train150,letter_train_data$lettr)
letter_train_acc150<-mean(ifelse(letter_pred_train150==letter_train_data$lettr,1,0))*100
letter_train_acc150

#Min leaf node size = 125
letter_model125<-C5.0(letter_train_data[,-1],letter_train_data$lettr,control=C5.0Control(minCases=125))
letter_pred_train125<-predict(letter_model125,letter_train_data)
confusionMatrix(letter_pred_train125,letter_train_data$lettr)
letter_train_acc125<-mean(ifelse(letter_pred_train125==letter_train_data$lettr,1,0))*100
letter_train_acc125

#Min leaf node size = 100
letter_model100<-C5.0(letter_train_data[,-1],letter_train_data$lettr,control=C5.0Control(minCases=100))
letter_pred_train100<-predict(letter_model100,letter_train_data)
confusionMatrix(letter_pred_train100,letter_train_data$lettr)
letter_train_acc100<-mean(ifelse(letter_pred_train100==letter_train_data$lettr,1,0))*100
letter_train_acc100

#Min leaf node size = 75
letter_model75<-C5.0(letter_train_data[,-1],letter_train_data$lettr,control=C5.0Control(minCases=75))
letter_pred_train75<-predict(letter_model75,letter_train_data)
confusionMatrix(letter_pred_train75,letter_train_data$lettr)
letter_train_acc75<-mean(ifelse(letter_pred_train75==letter_train_data$lettr,1,0))*100
letter_train_acc75

#Min leaf node size = 50
letter_model50<-C5.0(letter_train_data[,-1],letter_train_data$lettr,control=C5.0Control(minCases=50))
letter_pred_train50<-predict(letter_model50,letter_train_data)
confusionMatrix(letter_pred_train50,letter_train_data$lettr)
letter_train_acc50<-mean(ifelse(letter_pred_train50==letter_train_data$lettr,1,0))*100
letter_train_acc50

#Min leaf node size = 25
letter_model25<-C5.0(letter_train_data[,-1],letter_train_data$lettr,control=C5.0Control(minCases=25))
letter_pred_train25<-predict(letter_model25,letter_train_data)
confusionMatrix(letter_pred_train25,letter_train_data$lettr)
letter_train_acc25<-mean(ifelse(letter_pred_train25==letter_train_data$lettr,1,0))*100
letter_train_acc25

#Min leaf node size = 10
letter_model10<-C5.0(letter_train_data[,-1],letter_train_data$lettr,control=C5.0Control(minCases=10))
letter_pred_train10<-predict(letter_model10,letter_train_data)
confusionMatrix(letter_pred_train10,letter_train_data$lettr)
letter_train_acc10<-mean(ifelse(letter_pred_train10==letter_train_data$lettr,1,0))*100
letter_train_acc10

#Min leaf node size = 5
letter_model5<-C5.0(letter_train_data[,-1],letter_train_data$lettr,control=C5.0Control(minCases=5))
letter_pred_train5<-predict(letter_model5,letter_train_data)
confusionMatrix(letter_pred_train5,letter_train_data$lettr)
letter_train_acc5<-mean(ifelse(letter_pred_train5==letter_train_data$lettr,1,0))*100
letter_train_acc5

install.packages("caret")
library(caret)


#################################### Validation Data ##############################################

#Predicting the model with validation data
letter_pred250<-predict(letter_model250,letter_val_data)

#Confusion matrix between observed and expected results
confusionMatrix(letter_val_data$lettr,letter_pred250)

#Finding accuracy for the model with min leaf node 250
accuracy250<-mean(ifelse(letter_val_data$lettr==letter_pred250,1,0))*100

# Repeating the above steps with validation data for different min leaf node size 

#Min leaf node size = 225
letter_pred225<-predict(letter_model225,letter_val_data)
confusionMatrix(letter_val_data$lettr,letter_pred225)
accuracy225<-mean(ifelse(letter_val_data$lettr==letter_pred225,1,0))*100

#Min leaf node size = 200
letter_pred200<-predict(letter_model200,letter_val_data)
confusionMatrix(letter_val_data$lettr,letter_pred200)
accuracy200<-mean(ifelse(letter_val_data$lettr==letter_pred200,1,0))*100

#Min leaf node size = 175
letter_pred175<-predict(letter_model175,letter_val_data)
confusionMatrix(letter_val_data$lettr,letter_pred175)
accuracy175<-mean(ifelse(letter_val_data$lettr==letter_pred175,1,0))*100

#Min leaf node size = 150
letter_pred150<-predict(letter_model150,letter_val_data)
confusionMatrix(letter_val_data$lettr,letter_pred150)
accuracy150<-mean(ifelse(letter_val_data$lettr==letter_pred150,1,0))*100

#Min leaf node size = 125
letter_pred125<-predict(letter_model125,letter_val_data)
confusionMatrix(letter_val_data$lettr,letter_pred125)
accuracy125<-mean(ifelse(letter_val_data$lettr==letter_pred125,1,0))*100

#Min leaf node size = 100
letter_pred100<-predict(letter_model100,letter_val_data)
confusionMatrix(letter_val_data$lettr,letter_pred100)
accuracy100<-mean(ifelse(letter_val_data$lettr==letter_pred100,1,0))*100

#Min leaf node size = 75
letter_pred75<-predict(letter_model75,letter_val_data)
confusionMatrix(letter_val_data$lettr,letter_pred75)
accuracy75<-mean(ifelse(letter_val_data$lettr==letter_pred75,1,0))*100

#Min leaf node size = 50
letter_pred50<-predict(letter_model50,letter_val_data)
confusionMatrix(letter_val_data$lettr,letter_pred50)
accuracy50<-mean(ifelse(letter_val_data$lettr==letter_pred50,1,0))*100

#Min leaf node size = 25
letter_pred25<-predict(letter_model25,letter_val_data)
confusionMatrix(letter_val_data$lettr,letter_pred25)
accuracy25<-mean(ifelse(letter_val_data$lettr==letter_pred25,1,0))*100

#Min leaf node size = 10
letter_pred10<-predict(letter_model10,letter_val_data)
confusionMatrix(letter_val_data$lettr,letter_pred10)
accuracy10<-mean(ifelse(letter_val_data$lettr==letter_pred10,1,0))*100

#Min leaf node size = 5
letter_pred5<-predict(letter_model5,letter_val_data)
confusionMatrix(letter_val_data$lettr,letter_pred5)
accuracy5<-mean(ifelse(letter_val_data$lettr==letter_pred5,1,0))*100

#################################### Test Data ##############################################

letter_test5<-predict(letter_model5,letter_test_data)
confusionMatrix(letter_test_data$lettr,letter_test5)
accuracytest5<-mean(ifelse(letter_test_data$lettr==letter_test5,1,0))*100

install.packages("ggplot2")
library(ggplot2)

#Number of Nodes
nodes  <- c(250,225,200,175,150,125,100,75,50,25,10,5)
#Accuracy for training data
trainingacc<-c(letter_train_acc250,letter_train_acc225,letter_train_acc200,letter_train_acc175,letter_train_acc150,letter_train_acc125,letter_train_acc100,letter_train_acc75,letter_train_acc50,letter_train_acc25,letter_train_acc10,letter_train_acc5)

#Accuracy for validation data
validationacc<-c(accuracy250,accuracy225,accuracy200,accuracy175,accuracy150,accuracy125,accuracy100,accuracy75,accuracy50,accuracy25,accuracy10,accuracy5)

# first plot
plot(nodes, trainingacc,type="l",col="red",xlab="Nodes",ylab="Accuracy(%) ",main="Number of Nodes Vs Accuracy")

# second plot
par(new = TRUE)

#Legend for the below plot
legend("topright",c('Training Data','Validation Data'), lty=c(1,1),lwd=c(2.5,2.5),col=c('red','darkgreen')) 

#Plotting accuracies for different data with different min leaf node sizes
plot(nodes, validationacc,type="l",col="darkgreen", axes = FALSE,xlab="",ylab = "")