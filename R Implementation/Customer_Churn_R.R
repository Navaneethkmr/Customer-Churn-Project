# clean the environment 
rm(list=ls())

#set the working directory
setwd("C:/Users/Navaneeth/Desktop/Edwisor/Project/Employee Churn")

#load the required libraries
x = c("ggplot2","gridExtra","DMwR","corrgram","C50","caret","randomForest","e1071","class","Matrix","xgboost")
lapply(x, require, character.only = TRUE)
rm(x)



#*************************** Exploratory Data Analysis ******************************#

#read the Train_data csv file by replacing space,comma & NA values with NA.
train=read.csv("Train_data.csv", header=T, na.strings = c(""," ","NA"))
test= read.csv("Test_data.csv", header = T, na.strings = c(""," ","NA"))
whole_data= rbind(train,test)

#To have a look at the structure of the dataset
str(whole_data)

#Convert the area.code variable to factor. 
areacode_unique= unique(whole_data$area.code)
cat("The unique values in area.code variable are : ", areacode_unique)
whole_data$area.code= as.factor(whole_data$area.code)

# Bin the number.customer.service.calls variable by driving a new variable called "new"
whole_data$new[whole_data$number.customer.service.calls >= 0 & whole_data$number.customer.service.calls <= 3]="Low"
whole_data$new[whole_data$number.customer.service.calls > 3 & whole_data$number.customer.service.calls <= 6]= "Moderate"
whole_data$new[whole_data$number.customer.service.calls>6]= "High"

#drop the number.customer.service.calls variable & rename the "new" variable as number.customer.service.calls
whole_data$number.customer.service.calls=NULL
names(whole_data)[21]="number.customer.service.calls"
whole_data$number.customer.service.calls=as.factor(whole_data$number.customer.service.calls)

# Move the target variable to the end
whole_data=whole_data[,c(1:19,21,20)]


# Plotting a density plot for train predicotr variables
p1= ggplot(train, aes(x= account.length)) + geom_density()
p2= ggplot(train, aes(x= area.code)) + geom_density()
p3= ggplot(train, aes(x= number.vmail.messages)) + geom_density()
p4= ggplot(train, aes(x= total.day.minutes)) + geom_density()
p5= ggplot(train, aes(x= total.day.calls)) + geom_density()
p6= ggplot(train, aes(x= total.day.charge)) + geom_density()
p7= ggplot(train, aes(x= total.eve.minutes)) + geom_density()
p8= ggplot(train, aes(x= total.eve.calls)) + geom_density()
p9= ggplot(train, aes(x= total.eve.charge)) + geom_density()
p10= ggplot(train, aes(x= total.night.minutes)) + geom_density()
p11= ggplot(train, aes(x= total.night.calls)) + geom_density()
p12= ggplot(train, aes(x= total.night.charge)) + geom_density()
p13= ggplot(train, aes(x= total.intl.minutes)) + geom_density()
p14= ggplot(train, aes(x= total.intl.calls)) + geom_density()
p15= ggplot(train, aes(x= total.intl.charge)) + geom_density()
p16= ggplot(train, aes(x= number.customer.service.calls)) + geom_density()
gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol = 2)
gridExtra::grid.arrange(p9, p10, p11, p12, p13, p14, p15, p16, ncol = 2)

#****************** Below are some of the visualizations performed *************************#
#How many customers have churned out
g1 = ggplot(train, aes(x = Churn, fill = Churn)) + geom_bar(stat = 'count') +geom_label(stat='count',aes(label=..count..), size=5)+ ggtitle("Churning Out Count")
gridExtra::grid.arrange(g1, ncol = 1)

#Checking churn wrt state
g2 = ggplot(train, aes(x = state, fill = Churn, width= 5)) + geom_bar(stat = 'count') + geom_label(stat='count',aes(label=..count..), size=5) + ggtitle("Customer churn based on state")
gridExtra::grid.arrange(g2, ncol = 1)

#Churn wrt customer service call
g3 = ggplot(train, aes(x = number.customer.service.calls, fill = Churn, width = 5))+ geom_bar(stat = 'count', position = 'dodge') + geom_label(stat = 'count', aes(label= ..count..))+ggtitle('Customer churn based on number of service calls made')
gridExtra::grid.arrange(g3, ncol = 1)


#Churn wrt to area code
g4 = ggplot(train, aes(x = area.code, fill = Churn, width = 1)) + geom_bar(stat = 'count', position = 'dodge')+geom_label(stat = 'count', aes(label =..count..)) +ggtitle('Customer Churn based on their area code')
gridExtra::grid.arrange(g4, ncol = 1)

#churn wrt voice call plan
g5 = ggplot(train, aes(x = voice.mail.plan, fill = Churn)) + geom_bar(stat = 'count', position = 'dodge') + geom_label(stat = 'count', aes(label = ..count..)) +ggtitle("Number of Churned customers wrt to voice plan")
gridExtra::grid.arrange(g5, ncol = 1)

#************************** Missing Value Analysis *************************************#

#calcualte the sum of NAs in each colum and store it as a dataframe in missing_val
missing_val = data.frame(apply(whole_data,2,function(x) {sum(is.na(x))}))
#Storing the row names
missing_val$Columns = row.names(missing_val)
# rename the 1st column name
names(missing_val)[1] =  "Missing_percentage"
# calculate the percentage of NAs
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(whole_data)) * 100
# get percentage of NAs in decreasing order
missing_val = missing_val[order(-missing_val$Missing_percentage),] 
#drop the row names
row.names(missing_val) = NULL
#re-arrange the columns
missing_val = missing_val[,c(2,1)]



#************************ Outlier Analysis ********************************************#

# get indexes of numerical variables
numeric_index = sapply(whole_data,is.numeric) #selecting only numeric
# store the numeric data
numeric_data = whole_data[,numeric_index]
# store the column names of numerical variables
cnames = colnames(numeric_data)

# loop for plotting the box plot for all the numerical variables
 for (i in 1:length(cnames))
 {
   assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Churn"), data = subset(whole_data))+ 
            stat_boxplot(geom = "errorbar", width = 0.5) +
            geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                         outlier.size=1, notch=FALSE) +
            theme(legend.position="bottom")+
            labs(y=cnames[i],x="Churn")+
            ggtitle(paste("Box plot of Churn for",cnames[i])))
 }
 
# Plotting the boxplots together
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,gn6,ncol=3)
gridExtra::grid.arrange(gn7,gn8,gn9,gn10,gn11,gn12,ncol=3)
gridExtra::grid.arrange(gn13,gn14,ncol=2)


#********************* Outlier Treatment *************************


# optional funcion to check the standard deviation of all the numeric variables before KNN imputaion,
# the value of K that gives lowest standard deviation is selected.
#std= function(x)
#{
#  sd_value=sd(x)
#  sd_value
#}
#sd_data=data.frame(apply(whole_data[,cnames],2,std))
#names(sd_data)="Original SD"

# Replace Outlier values with NAs
for (i in cnames)
{
  outlier_values=whole_data[,i][whole_data[,i] %in% boxplot.stats(whole_data[,i])$out]
  whole_data[,i][whole_data[,i] %in% outlier_values]= NA
}

# Impute NAs with KNN Imputation
whole_data= knnImputation(whole_data,k=9)


#*********************** Feature Selection ***************************

# Correlation Plot 
corrgram(whole_data[,cnames], order = F,upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#OR 

# Correlation Plot without upper pie chart
corrgram(whole_data[,cnames], order = F,text.panel=panel.txt, main = "Correlation Plot")



## Chi-squared Test of Independence
factor_index = sapply(whole_data,is.factor)
factor_data = whole_data[,factor_index]
cat_names= colnames(factor_data)

# Loop to print the p values for all the categorical variables
for (i in 1:(ncol(factor_data)-1))
{
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$Churn,factor_data[,i])),simulate.p.value=TRUE)
}


## Dimension Reduction
whole_data = subset(whole_data, select = -c(total.day.charge, total.eve.charge,total.night.charge,total.intl.charge,area.code,phone.number))


#**************************** Feature Scaling ******************************#

#Updating cnames
cnames= colnames(whole_data[,sapply(whole_data, is.numeric)])

# Normalization
for ( i in cnames)
{
  whole_data[,i]= ((whole_data[,i]- min(whole_data[,i]))/( max(whole_data[,i])- min(whole_data[,i])))
}

#to check the max & min values to verify the normalization 
range(whole_data[,cnames])


#********************** Data Modelling *****************************#

# Assign levels to catagorical variables
for ( i in 1:ncol(whole_data))
{
  if (class(whole_data[,i])== 'factor')
  {
    print(colnames(whole_data[i]))
    whole_data[,i]= factor(whole_data[,i],labels= (1:length(levels(factor(whole_data[,i])))))
  }
}



#******************** Train - Test Split ****************************#

#We have been given 3333 Train observations and 1667 Test observations which we have combined 
# for pre-processing, and now we are going spilt as it was earlier, i.e from the row 3334 till 5000
# the observations come under test.


# Splitting train & test data
train_data= whole_data[1:3333,]  # OR train_data = whole_data[1:nrow(train_data),]
test_data = whole_data[3334:5000,] # OR test_data= whole_data[3334:nrow(whole_data),]
table(train_data$Churn)

#check the train & test percentage which have been given
train_percentage= ((nrow(train_data)*100)/nrow(whole_data))
test_percentage= ((nrow(test_data)*100) / nrow(whole_data))
cat("The train data percentage is = ",train_percentage)
cat ("The test data percentage is = ",test_percentage)

#SMOTE Sampling 
set.seed(123)
smote_train = SMOTE(Churn ~.,train_data,perc.over = 200,perc.under =150)
#to check the class of sampled train data
table(smote_train$Churn)

#******************************** Model Development ***************************

#************* Decison Tree Classifier Model ****************

set.seed(321)
DT_model = C5.0(Churn ~., smote_train, trials =90, rules = TRUE)
summary(DT_model)
#write(capture.output(summary(DT_model)), "DTModel_Rules.txt")

#Lets predict for test cases
DT_Predictions = predict(DT_model,test_data[,-15], type = "class")

##Evaluate the performance of classification model
ConfMatrix_C50 = table(actual= test_data$Churn, predicted= DT_Predictions)
print(ConfMatrix_C50)
confusionMatrix(ConfMatrix_C50)

# Function to calculate the classification matrix metrics
metrics= function(ConfMatrix)
{
  TN = ConfMatrix[1,1]
  FP = ConfMatrix[1,2]
  FN = ConfMatrix[2,1]
  TP = ConfMatrix[2,2]
  
  Accuracy = ((TN + TP) * 100) / (TN + TP + FN + FP)
  
  FNR = (FN / (FN + TP)) * 100
  
  Recall= (TP/(TP+FN))*100
  
  Precision = (TP/(TP+FP))*100
  
  return(c(Accuracy, FNR, Recall, Precision))
  
}

metrics_list= metrics(ConfMatrix_C50)
print(paste0("Decision Tree Accuracy is : ", metrics_list[1]))
print(paste0("False Negative rate of Decision Tree is : ", metrics_list[2]))
print(paste0("Recall of Decision Tree is : ", metrics_list[3]))
print(paste0("Precision of Decision Tree is : ", metrics_list[4]))

#save the Decision Tree Model for Future Use
saveRDS(DT_model,"DecisionTree_Rmodel.rds") 


#rm('ml','DT_model2','outlier_values','C2')

#******************** Random Forest Classifier Model ***************

#Random Forest model 
set.seed(45)
RF_model = randomForest(Churn ~ .,smote_train, importance = TRUE, ntree = 700)
# get the summary of the model
summary(RF_model)

#predict the test cases
RF_Predictions = predict(RF_model, test_data[,-15])
ConfMatrix_RF = table(actual=test_data$Churn, predicted= RF_Predictions)
print(ConfMatrix_RF)
confusionMatrix(ConfMatrix_RF)

#call the function to get the metrics
RF_metrics= metrics(ConfMatrix_RF)
print(paste0("Random Forest Accuracy % is : ", RF_metrics[1]))
print(paste0("False Negative rate % of Random Forest is : ", RF_metrics[2]))
print(paste0("Recall % of Random Forest is : ", RF_metrics[3]))
print(paste0("Precision % of Random forest is : ", RF_metrics[4]))

#Save the RF Model 
saveRDS(RF_model,"RandomForest_Rmodel.rds") 

#rm('RF_model','RF_Predictions')

#************************* KNN Algorithm ***********************

#KNN Model
set.seed(12)
KNN_Predictions = knn(smote_train[, 1:14], test_data[, 1:14], smote_train$Churn, k = 3)
ConfMatrix_KNN = table(actual=test_data$Churn, predicted= KNN_Predictions)
print(ConfMatrix_KNN)

#call the function to get the metrics
KNN_metrics= metrics(ConfMatrix_KNN)
print(paste0("KNN Accuracy % is : ", KNN_metrics[1]))
print(paste0("False Negative rate % of KNN is : ", KNN_metrics[2]))
print(paste0("Recall % of KNN is : ", KNN_metrics[3]))
print(paste0("Precision % of KNN is : ", KNN_metrics[4]))


#rm('KNN_Predictions')

#************************* Naive Bayes Model ***************************

#Naive Bayes Model
NB_model = naiveBayes(Churn ~ ., data = smote_train)
summary(NB_model)
NB_Predictions = predict(NB_model, test_data[,1:14], type = 'class')
ConfMatrix_NB = table(actual=test_data$Churn, predicted= NB_Predictions)
print(ConfMatrix_NB)

#call the function to get the metrics
NB_metrics= metrics(ConfMatrix_NB)
print(paste0("Naive Bayes Accuracy % is : ", NB_metrics[1]))
print(paste0("False Negative rate % of Naive Bayes is : ", NB_metrics[2]))
print(paste0("Recall % of Naive Bayes is : ", NB_metrics[3]))
print(paste0("Precision % of Naive Bayes is : ", NB_metrics[4]))

#Save the Naive Bayes Model 
saveRDS(NB_model,"NaiveBayes_Rmodel.rds") 

#*********************** Logistic Regression Model ***********************

#Logistic Regression model
logit_model= glm(Churn ~., data= smote_train, family = "binomial")
summary(logit_model)
logit_predictions= predict(logit_model,newdata= test_data, type="response")

#defiine the p value threshold
logit_predictions= ifelse(logit_predictions > 0.5,1,0)
ConfMatrix_logit = table(actual=test_data$Churn, predicted= logit_predictions)
print(ConfMatrix_logit)

#call the function to get the metrics
logit_metrics= metrics(ConfMatrix_logit)
print(paste0("Logistic Regression Accuracy % is : ", logit_metrics[1]))
print(paste0("False Negative rate % of Logistic Regression is : ", logit_metrics[2]))
print(paste0("Recall % of Logistic Regression is : ", logit_metrics[3]))
print(paste0("Precision % of Logistic Regression is : ", logit_metrics[4]))

#Save the Logistic Regression Model 
saveRDS(logit_model,"LogisticRegression_Rmodel.rds") 


#************************ Optionally Tried ****************************

#******************* XgBoost*******************************#

#Build Sparse Matrix for train & test data which is required for XgBoost model input
trainm= sparse.model.matrix(Churn ~., data = smote_train)[,-1]
train_label = as.integer(as.character(smote_train[,"Churn"]))-1
train_matrix = xgb.DMatrix(data=as.matrix(trainm), label= train_label)

testm = sparse.model.matrix(Churn ~. , data = test_data)[,-1]
test_label= as.integer(as.character(test_data[,"Churn"]))-1
test_matrix= xgb.DMatrix(data= as.matrix(testm), label = test_label)

#XgBoost Model
xg_model= xgboost(data = train_matrix,label=train_label,max.depth=10,eta=0.2,nrounds =10,objective="binary:logistic",eval_metric="auc",verbose = 1)

#Predictions
pred= predict(xg_model,test_matrix)  

#If probability value > 0.5 we are assiging 1 else 0
test_pred <- as.numeric(pred > 0.5) #or ifelse(pred<0.5, 0, 1)

#checking error rate of the predictions
err <- mean(as.numeric(pred > 0.5) != test_label)
print(paste("test-error=", err))

#Getting Important parameters as per XGBoost model evaluation
importance_matrix <- xgb.importance(model = xg_model)
print(importance_matrix)
#plot the Important parameters
xgb.plot.importance(importance_matrix = importance_matrix)

#Building the confusion matrix
cm_xgb = table(actual=test_label, predicted= test_pred)
print(cm_xgb)
confusionMatrix(cm_xgb)

#accuracy =89.14%
#FNR= 22.32%
#Recall = 77.67%
#Precision = 57.04%

#Saving the XgBoost model
saveRDS(xg_model,"XgBoost_Rmodel.rds")  

 
#***************************  END Of The File  ************************************
