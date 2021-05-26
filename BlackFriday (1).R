#libraries used in this project
library(biglasso)
library(bigmemory)
library(bigmemory.sri)
library(ggplot2)
library(dplyr)
library(corrplot)
library(Amelia)
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(caretEnsemble)
library(VIM)
library(gridExtra)
library(randomForest)

install.packages("caret")

# Loading the Black Friday Data Set CSV file

bf<-read.csv("/Users/tuhenasen/Desktop/r files/BlackFriday.csv")

object.size(black_friday)


#--------- FUNCTION TO PRINT DISTINCT VALUES of customers, sum and mean of purchase
myanalysis <- function(bf,cols){
  temp_data = bf[cols]
  myvec %>%
    group_by(name) %>%
    summarise(n_distinct(order_no))
  
}

  
#Age Analysis
temp <- data.table(bf[c('User_ID','Age','Purchase','Product_ID')])
distinct_cust_by_age = temp[, .(number_of_distinct_customers = length(unique(User_ID))), by = Age]
distinct_cust_by_age
total_purchase_by_age  = temp[, .(total_purcahse_in_Millions = sum(Purchase)/1000000), by = Age]

total_purchase_by_age

total_products_by_age = temp[, .(number_of_distinct_products = length(unique(Product_ID))), by = Age]
total_products_by_age
t1 = merge(distinct_cust_by_age,total_purchase_by_age)
t1
final = merge(t1,total_products_by_age)
final

#Box Plot

par(mfrow=c(3,2))
citysales<- plot(black_friday$City_Category,black_friday$Purchase,xlab="City")

#Bar Chart of Top 5 Product_ID by Quantity
attach(black_friday)
Most_sold <- black_friday %>% group_by(Product_ID) %>% summarise(count=n()) %>% arrange(desc(count))
top_5<-Most_sold[1:5,]
top_5
names(top_5)<-c('Product_ID','Quantity')
top_5
ggplot(top_5,aes(factor(
  Product_ID,
  levels = c('P00265242','P00110742','P00025442','P00112142','P00057642')),Quantity)) + geom_bar(stat = 'identity',fill=c('#c4d8ba', '#d8ceba', '#bac4d8', '#e1daae', '#fa5845')
  )+ xlab('Product_ID')+theme_bw()

#histogram by purchase

ggplot(bf, aes(y = Purchase)) +
  geom_histogram(bins = 100) +
  labs(title= "Purchases Histogram")

#BAr Chart of purchase by Gender



by_gend <- black_friday %>% group_by(Gender) %>% summarise(Purchase = mean(Purchase))
ggplot(by_gend, aes(Gender, Purchase)) + geom_bar(stat='identity', fill = c('pink', 'light blue'))
attach(black_friday)



#histogram of purchase by age

ggplot(bf, aes(x = Purchase, fill = Age)) +
  geom_histogram(bins = 75) +
  facet_wrap(~ Age) +
  labs(title= "Purchases Histogram by Age") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#histogram of purchase by occupation

ggplot(bf, aes(Occupation)) + geom_bar(fill= 'dark blue') + theme_classic()

#histogram of purchase by stay in current city years

ggplot(black_friday, aes(x = Purchase, fill = Stay_In_Current_City_Years)) +
  geom_histogram() +
  facet_wrap(~ Stay_In_Current_City_Years) +
  labs(title= "Purchases Histogram by Stay In Current City Years") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#BAr chart of purchase by city category

ggplot(black_friday,aes(City_Category)) + geom_bar(fill = c('light blue','dark blue','blue')) 

library(dplyr)

#Bar Chart of Top 5 Product_IDs by Revenue


Revenue <- bf %>% group_by(Product_ID) %>% summarise(revenue = sum(Purchase)) %>% arrange(desc(revenue))
top_5_revenue <- Revenue[1:5, ]
names(top_5_revenue) <- c('Product_ID', 'Revenue')
ggplot(top_5_revenue,aes(factor(
  Product_ID,
  level = c('P00025442','P00110742','P00255842','P00184942','P00059442')), Revenue)) + geom_bar(stat = 'identity',fill = c('#fa5845','#e1daae','#bac4d8','#d8ceba','#c4d8ba')
  ) + xlab('Product_ID') + theme_bw()

library(ggplot2)

ggplot(bf, aes(Purchase)) + 
  geom_histogram(aes(y=..density..), fill="light blue") +
  geom_density(alpha=1, color = '#49a4aa', size = 1.2) + theme_bw() + xlab('Amount Spent by each buyer') + ylab('Number of buyers') + ggtitle('Purchase Distribution') +  theme(plot.title = element_text(hjust = 0.5))


# Correlation matrix

data <- bf[, c(5,7,8,12)]
data
res <- cor(data, use = "complete.obs")
res <- round(res, 2)
corrplot(res, tl.col = 'black', tl.cex = .7, tl.srt = 45)
library(corrplot)
?cor()
#------------------------------------------------------------------


na_count <-sapply(bf, function(y) sum(length(which(is.na(y)))))
na_count



##Changing numeric variables to categorical variables

bf$Occupation <- factor(bf$Occupation)
bf$Product_ID <- factor(bf$Product_ID)
bf$User_ID <- factor(bf$User_ID)
bf$Gender <- factor(bf$Gender)
bf$Product_Category_2 <- factor(bf$Product_Category_2)
bf$Product_Category_3 <- factor(bf$Product_Category_3)
bf$Stay_In_Current_City_Years <- factor(bf$Stay_In_Current_City_Years)
bf$City_Category <- factor(bf$City_Category)
bf$Stay_In_Current_City_Years <- gsub("4\\+", "4", bf$Stay_In_Current_City_Years)
unique(bf$Stay_In_Current_City_Years)
bf$Stay_In_Current_City_Years<-as.factor(bf$Stay_In_Current_City_Years)
bf$Age<-bf$Age

# Imputing NULL values for NA in the Product_categories for 2 and 3

bf$Product_Category_2 <- as.numeric(bf$Product_Category_2)

bf[is.na(bf$Product_Category_2), "Product_Category_2"] <- 0

bf$Product_Category_3 <- as.numeric(bf$Product_Category_3)

bf[is.na(bf$Product_Category_3), "Product_Category_3"] <- 0

bf$Product_ID <- NULL
bf$User_ID <- NULL
bf$Product_Category_2 <- NULL
bf$Product_Category_3 <- NULL

head(bf)

# Cross Validation of the data set

set.seed(101)
sample_bf <- sample.split(bf$Purchase, SplitRatio = .7)
train <- subset(bf, sample_bf == T)
test <- subset(bf, sample_bf == F)

#Analysis : The evaluation metric used here is root mean squared error (rmse). Compared to the Mean Absolute Error, RMSE punishes large errors.

#LINEAR REGRESSION MODEL 

lm_model<- lm(Purchase~.,train)
summary(lm_model)

#Applying LM_MODEL Prediction on the test data

lm_predict <- predict(lm_model, test[,-8]) 
result<-cbind(lm_predict,test$Purchase)
colnames(result) <- c('Predicted','Actual')
head(result)

#RMSE of LM model

result <- as.data.frame(result)
sqrt(mean((result$Actual - result$Predicted)^2))

plotROC(test$Purchase, lm_predict)

confusionMatrix(test$Purchase, lm_predict, threshold = 0.4)
sensitivity(test$Purchase, lm_predict, threshold = 0.4) #Ture Positive, 0.740
specificity(test$Purchase, lm_predict, threshold = 0.4) #

# Decision Tree Model on training Data

d_tree <- rpart(Purchase ~ Age+Occupation+City_Category+Stay_In_Current_City_Years+Marital_Status+Gender+Product_Category_1, method = 'anova',data=train)
prp(d_tree)
dev.off()
str(d_tree)


dt_predict <- predict(d_tree, test[,-8])
result2<-cbind(dt_predict,test$Purchase)
colnames(result2)<-c('Predicted','Actual')
head(result2)

#RMSE of decision tree model

result2 <- as.data.frame(result2)
sqrt(mean((result2$Actual - result2$Predicted)^2))

plotROC(test$Purchase, dt_predict) 

confusionMatrix(test$Purchase, dt_predict, threshold = 0.5)
sensitivity(test$Purchase, dt_predict, threshold = 0.5) #Ture Positive, 0.740
specificity(test$Purchase, dt_predict, threshold = 0.5)

set.seed(101)

black_friday_sample <- createDataPartition(y = bf$Purchase, 
                                           p = 0.1, list=FALSE)
black_friday_sample <- bf[black_friday_sample, ]
inTrain <- createDataPartition(y = black_friday_sample$Purchase, 
                               p = 0.7, list=FALSE)
train <- black_friday_sample[inTrain, ]
test <- black_friday_sample[-inTrain, ]

# Since the data set is huge more than 36MB containing nearly 500k rows unable to perform the random forest model on the data set.

rf_model<-randomForest(Purchase~.,train)


## Logistic Regression on the Data Set

#Loading data in r
bf = read.csv("/Users/tuhenasen/Downloads/BlackFriday.csv")
dim(bf)
str(bf)
#overview of data
summary(bf)
#check missing values
t(colSums(is.na(mydata)))

# Logistic Regression
getwd()
install.packages("InformationValue")
library(InformationValue)

bf <- read.csv("/Users/tuhenasen/Downloads/BlackFriday.csv", header=TRUE)
#trans
str(bf)
bf$User_ID <- as.factor(bf$User_ID)
bf$Occupation <- as.factor(bf$Occupation)
bf$Product_Category_1 <- as.factor(bf$Product_Category_1)
bf$Product_Category_2 <- as.factor(bf$Product_Category_2)
bf$Product_Category_3 <- as.factor(bf$Product_Category_3)
bf$Purchase <- as.numeric(bf$Purchase)
bf$Marital_Status <- ifelse(bf$Marital_Status==1,"married", "single")
bf$Marital_Status <- as.factor(bf$Marital_Status)

#remove variables
bf <- bf[,-1]
bf <- bf[,-1]
bf <- bf[,-8]
bf <- bf[,-8]
str(bf)
mean<-mean(bf$Purchase)
mean
bf$Purchase <- ifelse(bf$Purchase >= 9333.86, 1, 0)
bf$Purchase

#logistic regression
set.seed(123)
row.number <- sample(x=1:nrow(bf), size=0.75*nrow(bf))
train = bf[row.number,]
test = bf[-row.number,]


logistic_model <- glm(Purchase ~ Age +  Product_Category_1 + City_Category + Marital_Status + Stay_In_Current_City_Years + Gender, family = binomial, data=train)
summary(logistic_model)

logistic_model <- predict(logistic_model, test[,-8]) 
result<-cbind(lm_predict,test$Purchase)
colnames(result) <- c('Predicted','Actual')
head(result)



#predict
pred <- predict(logistic_model, newdata = test, type = "response")
#error
misClassError(test$Purchase, pred, threshold = 0.5) 

#check accuracy
plotROC(test$Purchase, pred) 

#confusion matrix
confusionMatrix(test$Purchase, pred, threshold = 0.5)
sensitivity(test$Purchase, pred, threshold = 0.5) #Ture Positive, 0.740
specificity(test$Purchase, pred, threshold = 0.5) #False Negative 0.889




