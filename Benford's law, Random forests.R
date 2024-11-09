##Benford's law 
#The probability of 1 occuring as the first digit is log10(1+1/n). n = 1,....

library(benford.analysis)
data("corporate.payment")
data("lakes.perimeter")

cp <- corporate.payment
head(cp,2)
dim(cp)

#this is function that finds the probability after analysis of the first two digits in each number of the dataset. 

bfd.cp = benford(cp$Amount,number.of.digits = 2, sign = 'positive')
further_anl <- getSuspects(bfd.cp,cp) #further analysis of suspects
dim(further_anl)
#plot function 
plot(bfd.cp)

#Chi-square tests on samples first two digits using defaults
library(BenfordTests)

chisq.benftest(cp$Amount)

chisq.benftest(cp$Amount,digits=2)

sample_data <- c(9,15,25,35,45,55,65)
benford_result1 <- benford(sample_data,number.of.digits =1)


#summary
summary(benford_result1)



#CC fraud detection
#Load the Kaggle credit card fault

library(caTools)
library(randomForest)
library(dplyr)
library(caret)
library(ROSE)

data <- read.csv("C:\\Users\\ramasastrydatta\\OneDrive\\Desktop\\MBA 2nd Year\\AFA\\creditcard.csv")
head(data)
str(data)    #displays structure
class(data)
sum(is.na(data)) # should return 0
dim(data)
class(data[31])
typeof(data[31])

#Split the data into training and testing dataset (70% training, 305 testing)
set.seed(123) #for reproducibility(repeating the n values)
split <- sample.split(data$Class, SplitRatio = 0.7)
train_data <- subset(data,split==TRUE)
test_data <- subset(data,split==FALSE)
sum(train_data$Class) #class here is used for classification; fraud / not fraud
count(test_data)
count(train_data)


# Apply ROSE to balance the training data (Random Ove-Sampling Examples)
train_data_rose <- ROSE(Class ~.,data = train_data, seed = 123)$data # . means all the other variables in the data frame
?ROSE
#Check the class of the distribution after ROSE
table(train_data_rose$Class)

# RW vs Random Forest talks about a decision tree; it gives the result for which decision tree is useful.


#Now train the Random Forest Model on the ROSE balanced data
set.seed(123)
?set.seed
rf_model <- randomForest(Class ~.,data = train_data_rose, ntree =50,importance =TRUE)
#make predictions
rf_predictions <- predict(rf_model, newdata = test_data)
head(rf_predictions)