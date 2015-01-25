#install.packages("caret")
#install.packages("randomForest")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("e1071")

#Download Data
download.file(url="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",
              destfile="pml-training.csv",method="wget")
download.file(url="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",
              destfile="pml-testing.csv",method="wget")


#Packages needed
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(e1071)

#Load data
training.data<-read.csv("pml-training.csv")
testing.data<-read.csv("pml-training.csv")

# Delete columns with all missing values
training.data<-training.data[,colSums(is.na(training.data)) == 0]
testing.data <-testing.data[,colSums(is.na(testing.data)) == 0]

#Irrelevante variables (data descriptors)
training.data   <-training.data[,-c(1:7)]
testing.data <-testing.data[,-c(1:7)]

#Removeing factors and variables with too few data (too much NA)
training.data<-data.frame(classe=training.data$classe,training.data[,!sapply(training.data,is.factor)])
testing.data<-data.frame(classe=testing.data$classe,testing.data[,!sapply(testing.data,is.factor)])

#generateing training(60%) and testing(40%) data
subsamples <- createDataPartition(y=training.data$classe, p=0.60, list=FALSE)
subTraining <- training.data[subsamples, ] 
subTesting <- training.data[-subsamples, ]

#Plot of the Training data levels
classe.plot <- ggplot(subTraining, aes(classe))
classe.plot + geom_bar() + xlab("Class levels") + ylab("Frequency") +
    ggtitle("Bar Plot of levels of the variable classe within the subTraining data set")

#Variables for the model
colnames(subTraining)

#Decision tree model
decisiontree.model <- rpart(classe ~ ., data=subTraining, method="class")

#Plot of Decision tree model
rpart.plot(decisiontree.model, main="Classification Tree", extra=102, under=TRUE, faclen=0)

#Predicting
decisiontree.prediction <- predict(decisiontree.model, subTesting, type = "class")
confusionMatrix(decisiontree.prediction, subTesting$classe)

#Random Forest Model
randomForest.model <- randomForest(classe ~. , data=subTraining, method="class")

#Plot of Random Forest Model
varImpPlot(randomForest.model)


#Predicting
randomForest.prediction <- predict(randomForest.model, subTesting, type = "class")
confusionMatrix(randomForest.prediction, subTesting$classe)
