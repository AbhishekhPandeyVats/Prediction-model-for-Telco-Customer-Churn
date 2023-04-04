install.packages(c('boot', 'randomForest', 'reshape2', 'car', 'caret', 'data.table', 'caTools', 'earth', 'gplots', 'RColorBrewer'))

library(boot)
library(randomForest)
library(ggplot2)
library(reshape2)
library(car)
library(caret)
library(data.table)
library(caTools)
library(earth)
library(gplots)
library(RColorBrewer)

set.seed(1)

setwd('C:/Users/abhis/OneDrive/Desktop/Academics/Year 2 Sem 2/BC2407 Analytic II Advanced Predictive Techniques/Group Project')

original.dt<- read.csv('telco_churn_data.csv')

View(original.dt)

summary(original.dt)

#Creating a clean dataset
clean.dt <- copy(original.dt)

#Dropping CustomerID column
clean.dt$Customer.ID <- NULL

#convert features to factor
factor <- c('Referred.a.Friend', 'Offer', 'Phone.Service', 'Multiple.Lines', 'Internet.Service', 'Internet.Type', 
            'Online.Security', 'Online.Backup', 'Device.Protection.Plan', 'Premium.Tech.Support', 'Streaming.TV',
            'Streaming.Movies', 'Streaming.Music', 'Unlimited.Data', 'Contract', 'Paperless.Billing', 'Payment.Method',
            'Gender', 'Under.30', 'Senior.Citizen', 'Married', 'Dependents', 'City', 'Churn.Value', 'Churn.Category', 
            'Customer.Satisfaction')

clean.dt[factor] <-lapply(clean.dt[factor], factor)

# Churn Value: 1 = the customer left the company this quarter. 0 = the customer remained with the company

drop <- c('Zip.Code', 'Latitude', 'Longitude', 'Population', 'City')

clean.dt<- clean.dt[, !(names(clean.dt) %in% drop)]


#Create new column with the number of subscriptions a customer has
clean.dt$Number.of.Subscriptions <- rowSums(clean.dt[,c('Phone.Service', 'Multiple.Lines', 'Internet.Service',
                                                        'Online.Security', 'Online.Backup', 'Device.Protection.Plan', 
                                                        'Premium.Tech.Support', 'Streaming.TV', 'Streaming.Movies', 
                                                        'Streaming.Music', 'Unlimited.Data')]=="Yes")

View(clean.dt)

summary(clean.dt)

# ==============================================================================================
# ------------------------------------- Chi Square Matrix -------------------------------------
# ==============================================================================================
# Chi-square test for categorical variables
chisqmatrix <- function(x) {
  names = colnames(x);  num = length(names)
  m = matrix(nrow=num, ncol=num, dimnames=list(names,names))
  for (i in 1:num) {
    for (j in 1:num) {
      complete_cases = complete.cases(x[[i]], x[[j]])
      m[i,j] = chisq.test(x[[i]][complete_cases], x[[j]][complete_cases])$p.value
    }
  }
  return (m)
}

subset2 <- clean.dt[, c('Referred.a.Friend', 'Phone.Service', 'Multiple.Lines', 'Internet.Service', 'Internet.Type', 
                        'Online.Security', 'Online.Backup', 'Device.Protection.Plan', 'Premium.Tech.Support', 'Streaming.TV',
                        'Streaming.Movies', 'Streaming.Music', 'Unlimited.Data', 'Contract', 'Paperless.Billing', 'Payment.Method',
                        'Gender', 'Under.30', 'Senior.Citizen', 'Married', 'Dependents', 'Churn.Value')]
subset2
mat <- chisqmatrix(subset2)
mat

# create the heatmap
heatmap.2(mat, trace="none", dendrogram="none", scale="none", key=TRUE, keysize=1.5, density.info="none", 
          cexRow=1.0, cexCol=1.0, margins=c(10,10), cellnote=round(mat,2), notecex=0.6,
          col = brewer.pal(n=15, name="Reds"), notecol = "black")

# ==============================================================================================
# --------------------------------- Pearson Correlation Matrix ---------------------------------
# ==============================================================================================
#Pearson Correlation Matrix for Continuous Variables
continuous.dt<- copy(clean.dt)

drop_continuous <- c('Referred.a.Friend', 'Offer', 'Phone.Service', 'Multiple.Lines', 'Internet.Service', 'Internet.Type', 
                     'Online.Security', 'Online.Backup', 'Device.Protection.Plan', 'Premium.Tech.Support', 'Streaming.TV',
                     'Streaming.Movies', 'Streaming.Music', 'Unlimited.Data', 'Contract', 'Paperless.Billing', 'Payment.Method',
                     'Gender', 'Under.30', 'Senior.Citizen', 'Married', 'Dependents', 'Churn.Value', 'Churn.Category', 
                     'Churn.Reason', 'Customer.Satisfaction')

continuous.dt<- continuous.dt[, !(names(clean.dt) %in% drop_continuous)]

#creating a correlation matrix using Pearson Correlation
correlation.dt <- copy(continuous.dt)
corr_matrix <- cor(correlation.dt, method = "pearson")
print(corr_matrix)

# Filter out values greater than 0.7
corr_matrix[abs(corr_matrix) <= 0.7] <- NA

# Plot the filtered correlation matrix
melted_corr <- melt(corr_matrix)
ggplot(melted_corr, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "#0000FF", high = "#FF0000") +
  theme_minimal() +
  labs(title = "Pearson Correlation Matrix") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  geom_text(aes(label = round(value, 2)), size = 2, color = "black")

# ==============================================================================================
# -------------------------------------- Data Exploration --------------------------------------
# ==============================================================================================

# Stacked Bar Plot - Churn.Value vs Referred.a.Friend
ggplot(clean.dt) + aes(Referred.a.Friend, fill = Churn.Value) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Referred.a.Friend", y = "Churn.Value",
        title ="Relationship Between Churn.Value & Referred.a.Friend")

# Stacked Bar Plot - Churn.Value vs Offer
ggplot(clean.dt) + aes(Offer, fill = Churn.Value) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Offer", y = "Churn.Value",
        title ="Relationship Between Churn.Value & Offer")

# Stacked Bar Plot - Churn.Value vs Phone.Service
ggplot(clean.dt) + aes(Phone.Service, fill = Churn.Value) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Phone.Service", y = "Churn.Value",
        title ="Relationship Between Churn.Value & Phone.Service")

# Stacked Bar Plot - Churn.Value vs Multiple.Lines
ggplot(clean.dt) + aes(Multiple.Lines, fill = Churn.Value) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Multiple.Lines", y = "Churn.Value",
        title ="Relationship Between Churn.Value & Multiple.Lines")

# Stacked Bar Plot - Churn.Value vs Internet.Service
ggplot(clean.dt) + aes(Internet.Service, fill = Churn.Value) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Internet.Service", y = "Churn.Value",
        title ="Relationship Between Churn.Value & Internet.Service")

# Stacked Bar Plot - Churn.Value vs Internet.Type
ggplot(clean.dt) + aes(Internet.Type, fill = Churn.Value) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Internet.Type", y = "Churn.Value",
        title ="Relationship Between Churn.Value & Internet.Type")

# Stacked Bar Plot - Churn.Value vs Online.Security
ggplot(clean.dt) + aes(Online.Security, fill = Churn.Value) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Online.Security", y = "Churn.Value",
        title ="Relationship Between Churn.Value & Online.Security")

# Stacked Bar Plot - Churn.Value vs Online.Backup
ggplot(clean.dt) + aes(Online.Backup, fill = Churn.Value) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Online.Backup", y = "Churn.Value",
        title ="Relationship Between Churn.Value & Online.Backup")

# Stacked Bar Plot - Churn.Value vs Device.Protection.Plan
ggplot(clean.dt) + aes(Device.Protection.Plan, fill = Churn.Value) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Device.Protection.Plan", y = "Churn.Value",
        title ="Relationship Between Churn.Value & Device.Protection.Plan")

# Stacked Bar Plot - Churn.Value vs Premium.Tech.Support
ggplot(clean.dt) + aes(Premium.Tech.Support, fill = Churn.Value) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Premium.Tech.Support", y = "Churn.Value",
        title ="Relationship Between Churn.Value & Premium.Tech.Support")

# Stacked Bar Plot - Churn.Value vs Streaming.TV
ggplot(clean.dt) + aes(Streaming.TV, fill = Churn.Value) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Streaming.TV", y = "Churn.Value",
        title ="Relationship Between Churn.Value & Streaming.TV")

# Stacked Bar Plot - Churn.Value vs Streaming.Movies
ggplot(clean.dt) + aes(Streaming.Movies, fill = Churn.Value) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Streaming.Movies", y = "Churn.Value",
        title ="Relationship Between Churn.Value & Streaming.Movies")

# Stacked Bar Plot - Churn.Value vs Streaming.Music
ggplot(clean.dt) + aes(Streaming.Music, fill = Churn.Value) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Streaming.Music", y = "Churn.Value",
        title ="Relationship Between Churn.Value & Streaming.Music")

# Stacked Bar Plot - Churn.Value vs Unlimited.Data
ggplot(clean.dt) + aes(Unlimited.Data, fill = Churn.Value) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Unlimited.Data", y = "Churn.Value",
        title ="Relationship Between Churn.Value & Unlimited.Data")

# Stacked Bar Plot - Churn.Value vs Contract
ggplot(clean.dt) + aes(Contract, fill = Churn.Value) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Contract", y = "Churn.Value",
        title ="Relationship Between Churn.Value & Contract")

# Stacked Bar Plot - Churn.Value vs Paperless.Billing
ggplot(clean.dt) + aes(Paperless.Billing, fill = Churn.Value) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Paperless.Billing", y = "Churn.Value",
        title ="Relationship Between Churn.Value & Paperless.Billing")

# Stacked Bar Plot - Churn.Value vs Payment.Method
ggplot(clean.dt) + aes(Payment.Method, fill = Churn.Value) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Payment.Method", y = "Churn.Value",
        title ="Relationship Between Churn.Value & Payment.Method")

# Stacked Bar Plot - Churn.Value vs Gender
ggplot(clean.dt) + aes(Gender, fill = Churn.Value) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Gender", y = "Churn.Value",
        title ="Relationship Between Churn.Value & Gender")

# Stacked Bar Plot - Churn.Value vs Under.30
ggplot(clean.dt) + aes(Under.30, fill = Churn.Value) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Under.30", y = "Churn.Value",
        title ="Relationship Between Churn.Value & Under.30")

# Stacked Bar Plot - Churn.Value vs Senior.Citizen
ggplot(clean.dt) + aes(Senior.Citizen, fill = Churn.Value) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Senior.Citizen", y = "Churn.Value",
        title ="Relationship Between Churn.Value & Senior.Citizen")

# Stacked Bar Plot - Churn.Value vs Married
ggplot(clean.dt) + aes(Married, fill = Churn.Value) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Married", y = "Churn.Value",
        title ="Relationship Between Churn.Value & Married")

# Stacked Bar Plot - Churn.Value vs Dependents
ggplot(clean.dt) + aes(Dependents, fill = Churn.Value) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Dependents", y = "Churn.Value",
        title ="Relationship Between Churn.Value & Dependents")

# Stacked Bar Plot - Churn.Value vs Customer.Satisfaction
ggplot(clean.dt) + aes(Customer.Satisfaction, fill = Churn.Value) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Customer.Satisfaction", y = "Churn.Value",
        title ="Relationship Between Churn.Value & Customer.Satisfaction")

#Bar Plot - Churn.Value vs Customer.Satisfaction
Customer.Satisfaction_data <- subset(clean.dt, Customer.Satisfaction %in% c('1', '2', '3', '4', '5'))

ggplot(Customer.Satisfaction_data, aes(x = Customer.Satisfaction , fill = Churn.Value)) + geom_bar() 

# Stacked Bar Plot - Churn.Value vs Number.of.Subscriptions
ggplot(clean.dt) + aes(Number.of.Subscriptions, fill = Churn.Value) + 
  geom_bar(position = 'fill') + 
  scale_y_continuous(labels = scales::percent) + 
  labs( x = "Number.of.Subscriptions", y = "Churn.Value",
        title ="Relationship Between Churn.Value & Number.of.Subscriptions")

#Bar Plot - Churn.Value vs Churn.Category
Churn.Category_data <- subset(clean.dt, Churn.Category %in% c("Attitude", "Competitor", "Dissatisfaction", "Other", "Price"))

ggplot(Churn.Category_data, aes(x = Churn.Category , fill = Churn.Category)) + geom_bar() 

#Box Plot - Number.of.Referrals
ggplot(data=clean.dt, aes(x=Number.of.Referrals, y=Churn.Value, fill=Churn.Value)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "orange")) +
  labs(title = "Distribution of Churn.Value vs. Number.of.Referrals")
  
#Box Plot - Tenure.in.Months
ggplot(data=clean.dt, aes(x=Tenure.in.Months, y=Churn.Value, fill=Churn.Value)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "orange")) +
  labs(title = "Distribution of Churn.Value vs. Tenure.in.Months")

#Box Plot - Avg.Monthly.Long.Distance.Charges
ggplot(data=clean.dt, aes(x=Avg.Monthly.Long.Distance.Charges, y=Churn.Value, fill=Churn.Value)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "orange")) +
  labs(title = "Distribution of Churn.Value vs. Avg.Monthly.Long.Distance.Charges")

#Box Plot - Avg.Monthly.GB.Download
ggplot(data=clean.dt, aes(x=Avg.Monthly.GB.Download, y=Churn.Value, fill=Churn.Value)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "orange")) +
  labs(title = "Distribution of Churn.Value vs. Avg.Monthly.GB.Download")

#Box Plot - Monthly.Charge
ggplot(data=clean.dt, aes(x=Monthly.Charge, y=Churn.Value, fill=Churn.Value)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "orange")) +
  labs(title = "Distribution of Churn.Value vs. Monthly.Charge")

#Box Plot - Total.Regular.Charges
ggplot(data=clean.dt, aes(x=Total.Regular.Charges, y=Churn.Value, fill=Churn.Value)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "orange")) +
  labs(title = "Distribution of Churn.Value vs. Total.Regular.Charges")

#Box Plot - Total.Refunds
ggplot(data=clean.dt, aes(x=Total.Refunds, y=Churn.Value, fill=Churn.Value)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "orange")) +
  labs(title = "Distribution of Churn.Value vs. Total.Refunds")

#Box Plot - Total.Extra.Data.Charges
ggplot(data=clean.dt, aes(x=Total.Extra.Data.Charges, y=Churn.Value, fill=Churn.Value)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "orange")) +
  labs(title = "Distribution of Churn.Value vs. Total.Extra.Data.Charges")

#Box Plot - Total.Long.Distance.Charges
ggplot(data=clean.dt, aes(x=Total.Long.Distance.Charges, y=Churn.Value, fill=Churn.Value)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "orange")) +
  labs(title = "Distribution of Churn.Value vs. Total.Long.Distance.Charges")

#Box Plot - Number.of.Dependents
ggplot(data=clean.dt, aes(x=Number.of.Dependents, y=Churn.Value, fill=Churn.Value)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "orange")) +
  labs(title = "Distribution of Churn.Value vs. Number.of.Dependents")

#Box Plot - CLTV
ggplot(data=clean.dt, aes(x=CLTV, y=Churn.Value, fill=Churn.Value)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "orange")) +
  labs(title = "Distribution of Churn.Value vs. CLTV")

#Box Plot - Total.Customer.Svc.Requests
ggplot(data=clean.dt, aes(x=Total.Customer.Svc.Requests, y=Churn.Value, fill=Churn.Value)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "orange")) +
  labs(title = "Distribution of Churn.Value vs. Total.Customer.Svc.Requests")

#Box Plot - Product.Service.Issues.Reported
ggplot(data=clean.dt, aes(x=Product.Service.Issues.Reported, y=Churn.Value, fill=Churn.Value)) + 
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "orange")) +
  labs(title = "Distribution of Churn.Value vs. Product.Service.Issues.Reported")

#Scatter Plot - Number.of.Subscriptions vs Monthly.Charge
ggplot(data=clean.dt, aes(x = Number.of.Subscriptions, y = Monthly.Charge)) +
  geom_point(color= "steelblue") + geom_smooth(formula = y ~ x,method = "lm") +
  ggtitle("Scatterplot of Number.of.Subscriptions against Monthly.Charge")

#Scatter Plot - Number.of.Subscriptions vs Total.Regular.Charges
ggplot(data=clean.dt, aes(x = Number.of.Subscriptions, y = Total.Regular.Charges)) +
  geom_point(color= "steelblue") + geom_smooth(formula = y ~ x,method = "lm") +
  ggtitle("Scatterplot of Number.of.Subscriptions against Total.Regular.Charges")
  
#Scatter Plot - Tenure.in.Months vs Total.Regular.Charges
ggplot(data=clean.dt, aes(x = Tenure.in.Months, y = Total.Regular.Charges)) +
  geom_point(color= "steelblue") + geom_smooth(formula = y ~ x,method = "lm") +
  ggtitle("Scatterplot of Tenure.in.Months against Total.Regular.Charges")

# ==============================================================================================
# --------------------------------------- Final Dataset ---------------------------------------
# ==============================================================================================

#Create a datatable with feature set
final.dt <-  copy(clean.dt)

#Features to be dropped
drop <- c('Total.Regular.Charges', 'Number.of.Subscriptions', 'Churn.Category', 'Churn.Reason', 'Customer.Satisfaction', 
          'Referred.a.Friend', 'Internet.Service', 'Under.30', 'Senior.Citizen', 'Number.of.Dependents')

final.dt<- final.dt[, !(names(clean.dt) %in% drop)]

View(final.dt)

summary(final.dt)

train <- sample.split(Y = final.dt$Churn.Value, SplitRatio = 0.7)
trainset <- subset(final.dt, train == T)
testset <- subset(final.dt, train == F)

summary(testset)

trainset<- setDT(trainset)
majority <- trainset[Churn.Value == '0']
minority <- trainset[Churn.Value == '1']
chosen <- sample(seq(1:nrow(majority)), size = nrow(minority))
majority.chosen <- majority[chosen]
trainset <- rbind(majority.chosen, minority)
summary(trainset) #    No   Yes  1308

predictors <-c('Number.of.Referrals', 'Tenure.in.Months', 'Offer', 'Phone.Service', 
               'Avg.Monthly.Long.Distance.Charges', 'Multiple.Lines', 'Internet.Type', 'Avg.Monthly.GB.Download', 
               'Online.Security', 'Online.Backup', 'Device.Protection.Plan', 'Premium.Tech.Support', 'Streaming.TV', 
               'Streaming.Movies', 'Streaming.Music', 'Unlimited.Data', 'Contract', 'Paperless.Billing', 
               'Payment.Method', 'Monthly.Charge', 'Total.Refunds', 'Total.Extra.Data.Charges','Total.Long.Distance.Charges', 
               'Gender', 'Age', 'Married', 'Dependents', 'CLTV', 'Total.Customer.Svc.Requests', 
               'Product.Service.Issues.Reported')

# ==============================================================================================
# --------------------------------- Mode1: Logistic Regression ---------------------------------
# ==============================================================================================

#Building model first time
lr <- glm(Churn.Value ~ ., family = binomial, data = final.dt)

#Building model second time, remove insignificant variables
lr = step(lr)
summary(lr)

#odd ratio
OR <- exp(coef(lr))
OR

#Find the confidence interval
OR.CI <- exp(confint(lr))
OR.CI

m.logisticregression <- glm(formula = Churn.Value ~ Number.of.Referrals + Tenure.in.Months + 
                            Offer + Phone.Service + Multiple.Lines + Internet.Type + 
                            Avg.Monthly.GB.Download + Online.Security + Online.Backup + 
                            Device.Protection.Plan + Premium.Tech.Support + Streaming.TV + 
                            Streaming.Movies + Unlimited.Data + Contract + Paperless.Billing + 
                            Payment.Method + Monthly.Charge + Age + Married + Dependents + 
                            Total.Customer.Svc.Requests + Product.Service.Issues.Reported, 
                            family = binomial, data = trainset)

# Checking on Trainset
train.logisticregression <- predict(m.logisticregression, newdata = trainset, type = 'response')
train.logisticregression  <- ifelse(train.logisticregression >0.5, 1, 0)
train.logisticregression <- factor(train.logisticregression)

confusionMatrix(train.logisticregression, reference = trainset$Churn.Value, positive='1', mode="everything")

# Predicting on Testset
predictions.logisticregression <- predict(m.logisticregression, newdata = testset, type = 'response')
predictions.logisticregression  <- ifelse(predictions.logisticregression >0.5, 1, 0)
predictions.logisticregression <- factor(predictions.logisticregression)

##-----Evaluation of the model-----##
# Quick overview of model
confusionMatrix(predictions.logisticregression, reference = testset$Churn.Value, positive='1', mode="everything")

# ==============================================================================================
# ------------------------------------Mode2: Random Forest ------------------------------------
# ==============================================================================================

rf <-randomForest(Churn.Value ~ ., data=final.dt, ntree=500)
print(rf)

#Find optimal mtry value
#Step Factor: To test for different mtry values by scaling by this value
#Improve: Required improvement to continue testing for other mtry
#Trace: To print the progress of search
#Plot: To plot the OOB error as function of mtry

mtry <- tuneRF(final.dt[predictors],final.dt$Churn.Value, ntreeTry=500,
               stepFactor=1,improve=0.01, trace=TRUE, plot=TRUE)

#Find mtry with lowest OOBError
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m) #mtry 5 is the best mtry

#Build model again with significant features
m.randomforest <- randomForest(Churn.Value ~ Number.of.Referrals + Tenure.in.Months + Offer + Phone.Service + 
                               Multiple.Lines + Internet.Type + Avg.Monthly.GB.Download + Online.Security + 
                               Online.Backup + Device.Protection.Plan + Premium.Tech.Support + Streaming.TV + 
                               Streaming.Movies + Unlimited.Data + Contract + Paperless.Billing + 
                               Payment.Method + Monthly.Charge + Age + Married + Dependents + 
                               Total.Customer.Svc.Requests + Product.Service.Issues.Reported,
                               data=trainset, mtry=best.m, importance=TRUE, ntree=500)

print(m.randomforest)

#Checking on trainset
train.m.randomforest = predict(m.randomforest, newdata=trainset, type="class")

confusionMatrix(train.m.randomforest, reference = trainset$Churn.Value, positive="1", mode="everything")

#Predict on testset
predictions.m.randomforest = predict(m.randomforest, newdata=testset, type="class")

#All in one confusion matrix
confusionMatrix(predictions.m.randomforest, reference = testset$Churn.Value, positive="1", mode="everything")

#Evaluate variable importance
importance(m.randomforest)
varImpPlot(m.randomforest, cex = 0.75)
plot(m.randomforest)

# ==============================================================================================
# ---------------------------------------- Mode3: MARS -----------------------------------------
# ==============================================================================================

m.mars <- earth(Churn.Value ~ Number.of.Referrals + Tenure.in.Months + Offer + Phone.Service + Multiple.Lines + 
                Internet.Type + Avg.Monthly.GB.Download + Online.Security + Online.Backup + Device.Protection.Plan + 
                Premium.Tech.Support + Streaming.TV + Streaming.Movies + Unlimited.Data + Contract + Paperless.Billing + 
                Payment.Method + Monthly.Charge + Age + Married + Dependents + Total.Customer.Svc.Requests + 
                Product.Service.Issues.Reported, degree=1, data=trainset, glm = list(family = binomial))

summary(m.mars)
evimp(m.mars)

#checking on trainset
train.m.mars <- predict(m.mars, newdata=trainset, type="response")
train.m.mars  <- ifelse(train.m.mars >0.5, 1, 0)
train.m.mars <- factor(train.m.mars)

confusionMatrix(table(train.m.mars, trainset$Churn.Value), reference = trainset$Churn.Value, positive="1", mode="everything")

#Predicting on testset
predictions.mars <- predict(m.mars, newdata=testset, type="response")
predictions.mars  <- ifelse(predictions.mars >0.5, 1, 0)
predictions.mars <- factor(predictions.mars)

confusionMatrix(table(predictions.mars, testset$Churn.Value), reference = testset$Churn.Value, positive="1", mode="everything")



