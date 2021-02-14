#--------Import the libraries--------------
# for impute function to replace NA
library(Hmisc)
# for scatter plot and correlation
library(GGally)
#for vif( variable inflation factor)
library(car)
library(dplyr)


setwd('C://Users//HP//Desktop//analytics//NASDAQ and S&P 500')
# ------- read the csv data -----------------
nasdaq<-read.csv("nasdaq.csv", stringsAsFactors = FALSE)
sp<-read.csv("snp500.csv", stringsAsFactors = FALSE)


# filter nasdaq column with same date from sp
nasdaqFilter = filter(nasdaq, nasdaq$Date %in%  sp$Date)

# filter sandp column with same date from nasdaq
spFilter = filter(sp, sp$Date %in% nasdaqFilter$Date)

#combine column

newDF = data.frame(nasdaqFilter$Adj.Close, spFilter$Adj.Close)

View(newDF)

names(newDF) = c('nasdaq', 'sandp')

#First 6 rows of the data
head(mashable_data)

#structure of the data to see sample data and datatype
str(newDF)

#summary of data (check for NA Value for Missing value treatment) 
summary(newDF)

#----------- NO Missing Value-------------


#------------Outliers treatment (Do for all variable)-----------
#look for outliers
#box plot sets the outlier statistics coef is 1.5 time IQR from Q3
# value above ((Q3 - Q1) * 1.5) + Q3 is outlier 
outlier_values = boxplot.stats(newDF$nasdaq)$out  

# Hmisc puts a * on missing value treatment, to solve that use as.numeric
boxplot(as.numeric(newDF$nasdaq), main="No of token")


head(outlier_values)

#------------------nasdaq colum----------------
# take a new variable to manipulate
x <- newDF$nasdaq

# find Q1 = qnt[1] and Q3 = qnt[2]
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)

# find 5 percentile and 95 percentile caps[1] and caps[2]
caps <- quantile(x, probs=c(.05, .95), na.rm = T)

#1.5 times of range
H <- 1.5 * IQR(x, na.rm = T)

#value below 1.5 times range from Q1 replaced by 5 percentile value
x[x < (qnt[1] - H)] <- caps[1]

#value aboe 1.5 times range from Q3 replaced by 95 percentile value
x[x > (qnt[2] + H)] <- caps[2]

# replacing the existing column
newDF$nasdaq<-x

# -----------------sandp colum outlier treatment
# Hmisc puts a * on missing value treatment, to solve that use as.numeric
boxplot(as.numeric(newDF$sandp), main="No of token")


head(outlier_values)

# take a new variable to manipulate
x <- newDF$sandp

# find Q1 = qnt[1] and Q3 = qnt[2]
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)

# find 5 percentile and 95 percentile caps[1] and caps[2]
caps <- quantile(x, probs=c(.05, .95), na.rm = T)

#1.5 times of range
H <- 1.5 * IQR(x, na.rm = T)

#value below 1.5 times range from Q1 replaced by 5 percentile value
x[x < (qnt[1] - H)] <- caps[1]

#value aboe 1.5 times range from Q3 replaced by 95 percentile value
x[x > (qnt[2] + H)] <- caps[2]

# replacing the existing column
newDF$sandp<-x



#---------------- check correlation----------------

ggpairs(data=newDF)

# creating a matrix of correlation
correlation_matrix = cor(newDF)
correlation_matrix


#---------------- multicollinearity -----------------


# first we will need to run the linear model
car_model<-lm(sandp~ ., data=newDF)

# NOt required only 2 variables present
vif(car_model)


#-------------- test and learn data ---------------

smp_size = 0.75 * nrow(newDF)
set.seed(123)
train_ind <- sample(seq_len(nrow(newDF)), size = smp_size)
train <- newDF[train_ind, ]
test <- newDF[-train_ind, ]


#--------------model creation----------
linearMod <- lm(sandp ~ ., data=train)  # build linear regression model on full data
# Check the model output
summary(linearMod)


# ------------predict target variable/ scoring ---------------------
# predict shares
sandp_pred <- predict(linearMod, test)  
head(sandp_pred)


# ---------------- Evaluation/ Validation ----------------------------
actuals_preds <- data.frame(cbind(actuals=train$sandp, predicteds=sandp_pred))  # make actuals_predicteds dataframe.

tail(actuals_preds)

#Evaluation predictions
mape <- mean((abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals),na.rm = TRUE)
mape
