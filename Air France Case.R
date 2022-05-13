# Taylor's Coding

Mean_min_max_sd <- function(x){
  mean_AF <- mean(x, na.rm = TRUE)
  min_AF <- min(x, na.rm = TRUE)
  Max_AF <- max(x, na.rm = TRUE)
  sd_AF <- sd(x, na.rm = TRUE)
  return(c("mean", mean_AF, 
           "min", min_AF, 
           "max", Max_AF, 
           "st.dev.", sd_AF))
}# closing mean_min_sd function

# calculating mean, min, max, sd on ROA
Mean_min_max_sd(Air_France_Case_Spreadsheet_Supplement_1_calculated$Amount)



# creating binary for business success and failure
Air_France_Case_Spreadsheet_Supplement_1_calculated$total_booking_binary <-
  ifelse(Air_France_Case_Spreadsheet_Supplement_1_calculated$`Total Volume of Bookings`>=1, 1, 0) 
Air_France_Case_Spreadsheet_Supplement_1_calculated$amount_binary <-
  ifelse(Air_France_Case_Spreadsheet_Supplement_1_calculated$Amount>=1033.68, 1, 0)


# success factor: amount>=$1033 (avg) fails if <$1033 (avg)

install.packages(ggplot2)
library(ggplot2)
ggplot(data = Air_France_Case_Spreadsheet_Supplement_1_calculated, 
       aes(x=Air_France_Case_Spreadsheet_Supplement_1_calculated$`Total Volume of Bookings`,
           y=Amount))+geom_point(aes(size = ))+geom_smooth()
?ggplot
# publisher name to numeric
Air_France_Case_Spreadsheet_Supplement_1_calculated$Publisher_name_variable <- gsub("Google - Global", "1", Air_France_Case_Spreadsheet_Supplement_1_calculated$`Publisher Name`)
Air_France_Case_Spreadsheet_Supplement_1_calculated$Publisher_name_variable <- gsub("Google - US", "2", Air_France_Case_Spreadsheet_Supplement_1_calculated$Publisher_name_variable)
Air_France_Case_Spreadsheet_Supplement_1_calculated$Publisher_name_variable <- gsub("MSN - Global", "3", Air_France_Case_Spreadsheet_Supplement_1_calculated$Publisher_name_variable)
Air_France_Case_Spreadsheet_Supplement_1_calculated$Publisher_name_variable <- gsub("MSN - US", "4", Air_France_Case_Spreadsheet_Supplement_1_calculated$Publisher_name_variable)
Air_France_Case_Spreadsheet_Supplement_1_calculated$Publisher_name_variable <- gsub("Overture - Global", "5", Air_France_Case_Spreadsheet_Supplement_1_calculated$Publisher_name_variable)
Air_France_Case_Spreadsheet_Supplement_1_calculated$Publisher_name_variable <- gsub("Overture - US", "6", Air_France_Case_Spreadsheet_Supplement_1_calculated$Publisher_name_variable)
Air_France_Case_Spreadsheet_Supplement_1_calculated$Publisher_name_variable <- gsub("Yahoo - US", "7", Air_France_Case_Spreadsheet_Supplement_1_calculated$Publisher_name_variable)
Air_France_Case_Spreadsheet_Supplement_1_calculated$Publisher_name_variable <- as.numeric(Air_France_Case_Spreadsheet_Supplement_1_calculated$Publisher_name_variable)


# regression model with amount_binary
air_france_logit <- glm(data = Air_France_Case_Spreadsheet_Supplement_1_calculated, 
    amount_binary ~ Clicks+`Click Charges`+`Engine Click Thru %`+
      `Total Cost/ Trans.`+`Total Volume of Bookings`, 
    family = "binomial")
summary(air_france_logit)


# building the normal function
normalize <- function(x){
  my_minmax <- (x-min(x, na.rm = TRUE))/
    (max(x, na.rm = TRUE)-min(x, na.rm = TRUE))
  return(my_minmax)
}#closing normalize function
# normalizing everything
Normal_air_france$search_engine_bid <- normalize(Air_France_Case_Spreadsheet_Supplement_1_calculated$`Search Engine Bid`)
Normal_air_france$clicks <- normalize(Air_France_Case_Spreadsheet_Supplement_1_calculated$Clicks)
Normal_air_france$click_charges <- normalize(Air_France_Case_Spreadsheet_Supplement_1_calculated$`Click Charges`)
Normal_air_france$avg_cost_per_click <- normalize(Air_France_Case_Spreadsheet_Supplement_1_calculated$`Avg. Cost per Click`)
Normal_air_france$impressions <- normalize(Air_France_Case_Spreadsheet_Supplement_1_calculated$Impressions)
Normal_air_france$engine_click_through_rate <- normalize(Air_France_Case_Spreadsheet_Supplement_1_calculated$`Engine Click Thru %`)
Normal_air_france$avg_position <- normalize(Air_France_Case_Spreadsheet_Supplement_1_calculated$`Avg. Pos.`)
Normal_air_france$trans_conv_rate <- normalize(Air_France_Case_Spreadsheet_Supplement_1_calculated$`Trans. Conv. %`)
Normal_air_france$total_cost_per_trans <- normalize(Air_France_Case_Spreadsheet_Supplement_1_calculated$`Total Cost/ Trans.`)
Normal_air_france$amount <- normalize(Air_France_Case_Spreadsheet_Supplement_1_calculated$Amount)
Normal_air_france$total_cost <- normalize(Air_France_Case_Spreadsheet_Supplement_1_calculated$`Total Cost`)
Normal_air_france$total_booking <- normalize(Air_France_Case_Spreadsheet_Supplement_1_calculated$`Total Volume of Bookings`)
Normal_air_france <- as.data.frame(Normal_air_france)


#create normalized regression
normal_reg <- glm(data = Normal_air_france, amount ~ search_engine_bid+
      clicks+ click_charges +impressions+
      trans_conv_rate+ total_cost_per_trans+
      total_booking, family = gaussian)
summary(normal_reg)
exp(-1.745e-03) #search engine bid
exp(-1.425e-02) #clicks
exp(2.534e-02) # click charges
exp(1.267e-02) #impressions
exp(-5.270e-03) # total conv rate
exp(-9.581e-03) # total cost per trans
exp(9.480e-01) #total booking

# findings (all values in regression are significant)
 # for every additional unit of the search engine bid
    # the amount will go up .998
# for every additional click the amount will increase by .986
# for every additional click charge the amount will increase by 1.026
# for every additional impression the amount will increase by 1.013
# for every additional conv rate the amount will increase by .995
# for each additional cost per trans the amount will increase by .99
# for each additional booking the amount will increase by 2.58
    # ^*BIGGEST INFLUENCE*^

ggplot(data = Normal_air_france, aes(x = total_booking, y = amount))+ 
  geom_point(aes(size=))+geom_smooth()

Mean_min_max_sd(Normal_air_france$amount)

# creating testing and training data
nrow_norm <- nrow(Normal_air_france)
train_index <- sample(1:nrow_norm, size=.8*nrow_norm)
air_france_norm_training <- Normal_air_france[train_index,]
air_france_norm_testing <- Normal_air_france[-train_index,]

# Juan's coding

#Import data set
library(readxl)
Air_France_Case_Spreadsheet_Supplement_1 <- read_excel("Air France Case.xlsx")
View(Air_France_Case_Spreadsheet_Supplement_1)
my_df_af <- Air_France_Case_Spreadsheet_Supplement_1
summary(my_df_af)
table(my_df_af$`Engine Click Thru %`)
table(my_df_af$`Publisher Name`)

num_visitors <- nrow(my_df_af)
num_var <- ncol(my_df_af)

hist(my_df_af$`Click Charges`)
hist(my_df_af$engine_binary)
hist((my_df_af$`Engine Click Thru %`))
hist(my_df_af$Amount)
hist(my_df_af$`Trans. Conv. %`)
hist(my_df_af$`Total Cost`)
hist(my_df_af$`Total Volume of Bookings`)


my_df_af$publisher_name_num <- gsub("Yahoo - US", "1", my_df_af$`Publisher Name`)
my_df_af$publisher_name_num <- gsub("Google - Global", "2", my_df_af$publisher_name_num)
my_df_af$publisher_name_num <- gsub("Google - US", "3", my_df_af$publisher_name_num)
my_df_af$publisher_name_num <- gsub("MSN - Global", "4", my_df_af$publisher_name_num)
my_df_af$publisher_name_num <- gsub("MSN - US", "5", my_df_af$publisher_name_num)
my_df_af$publisher_name_num <- gsub("Overture - Global", "6", my_df_af$publisher_name_num)
my_df_af$publisher_name_num <- as.numeric (gsub("Overture - US", "7", my_df_af$publisher_name_num))


my_amount_vector <- as.numeric(c(my_df_af$Amount>1304))
my_df_af$amount_binary <- replace(my_amount_vector, isTRUE(my_amount_vector), "1")                              
my_df_af$amount_binary <- gsub("TRUE", "1", my_df_af$amount_binary)
my_df_af$amount_binary <- as.numeric(gsub("FALSE", "0", my_df_af$amount_binary))

my_df_af$engine_binary <- as.numeric(c(my_df_af$`Engine Click Thru %`>11.141))

mean_min <- function(x){
  my_mean <- mean(x, na.rm=TRUE)
  my_min <- min(x, na.rm=TRUE)
  my_max <- max(x, na.rm=TRUE)
  my_std <- sd(x, na.rm=TRUE)
  return(c("min",my_min, 
           "max", my_max, 
           "mean", my_mean, 
           "sd", my_std))
} #closing the mean_min UDF

#Normalization using min-max re-scaling
norm_sd <- function(x){
  my_minmax <- (x-min(x))/(max(x)-min(x))
  return(my_minmax)
}#closing norm_sd UDF

my_df_af$click_charges_norm <- norm_sd(my_df_af$`Click Charges`)
my_df_af$impressions_norm <- norm_sd(my_df_af$Impressions)
my_df_af$engine_norm <- norm_sd(my_df_af$`Engine Click Thru %`)
my_df_af$trans_conv_norm <- norm_sd(my_df_af$`Trans. Conv. %`)
my_df_af$avg_pos_norm <- norm_sd(my_df_af$`Avg. Pos.`)
my_df_af$bookings_norm <- norm_sd(my_df_af$`Total Volume of Bookings`)
my_df_af$total_cost_norm <- norm_sd(my_df_af$`Total Cost`)
my_df_af$clicks_norm <- norm_sd(my_df_af$Clicks)                                   
my_df_af$amount_norm <- norm_sd(my_df_af$Amount)

#Performing random sampling to split into training and testing 
training_indx <- sample(1:num_visitors, size = .80*num_visitors)
mydf_train <- my_df_af[training_indx, ]

#Create indexes for testing for the 20% remain data
mydf_test <- my_df_af[-training_indx,]

#Designing my logistic regression for amount_binary 
logit <- glm(data=mydf_train, amount_binary~`Click Charges`+Impressions+`Engine Click Thru %`+`Trans. Conv. %`+`Avg. Pos.`+
               Clicks, family="binomial")
summary(logit)

#Cleaning my logistic regression 
logit_clean <- glm(data=mydf_train, amount_binary~`Click Charges`+Impressions+`Trans. Conv. %`+`Avg. Pos.`+
                     Clicks, family="binomial")
summary(logit_clean)

#Cleaning my logistic regression 
logit_cleaner <- glm(data=mydf_train, amount_binary~`Click Charges`+Impressions+`Trans. Conv. %`+
                       Clicks, family="binomial")
summary(logit_cleaner)

exp(-8.411e-04)-1 #The odds of having an amount bigger than 1034, decrease by -0.0840% by each click charge
(exp(-2.667e-06)-1)*100 #The odds of having an amount bigger than 1034, decrease by -0.00026% by each impression 
exp(7.754e-02)-1 #The odds of having an amount bigger than 1034, increase by 8.06% by each trans. cov%
(exp(7.288e-03)-1)*100 #The odds of having an amount bigger than 1034, increase by 0.73% by each click

#Designing my logistic regression for engine_binary 
logit_engine <- glm(data=mydf_train, engine_binary~`Click Charges`+Impressions+`Trans. Conv. %`+`Avg. Pos.`+
                      Clicks+Amount, family="binomial")
summary(logit_engine)

#Cleaning my logistic regression 
logit_engine_clean <- glm(data=mydf_train, engine_binary~`Trans. Conv. %`+`Avg. Pos.`+
                            Clicks+Amount, family="binomial")
summary(logit_engine_clean)

#Cleaning my logistic regression 
logit_engine_cleaner <- glm(data=mydf_train, engine_binary~`Avg. Pos.`+
                              Clicks+Amount, family="binomial")
summary(logit_engine_cleaner)

#Building a normalized (unitless) logistic amount_binary
logit_norm <- glm(data=mydf_train, amount_binary~click_charges_norm+impressions_norm+trans_conv_norm+
                    clicks_norm, family="binomial")
summary(logit_norm)

#Building a normalized (unitless) logistic engine_binary
logit_engine_norm <- glm(data=mydf_train, engine_binary~avg_pos_norm+clicks_norm+amount_norm
                         , family="binomial")
summary(logit_engine_norm)

#Performance testing confusion matrix
#install.packages("caret")
library(caret)

#Confusion Matrix for amount_binary
my_prediction <- predict(logit_cleaner, mydf_train, type="response")
confusion_matrix_amount <- confusionMatrix(data=as.factor(as.numeric(my_prediction>0.5)),
                                           reference= as.factor(as.numeric(mydf_train$amount_binary)))
print(confusion_matrix_amount)

#Confusion matrix for testing data (new customers)
my_prediction_test <- predict(logit_cleaner, mydf_test, type="response")

cm_test_amount <- confusionMatrix(data=as.factor(as.numeric(my_prediction_test>0.5)), 
                                  reference = as.factor(as.numeric(mydf_test$amount_binary)))
print(cm_test_amount)


#Confusion Matrix for engine_binary
my_prediction_engine <- predict(logit_engine_cleaner, mydf_train, type="response")
confusion_matrix_engine <- confusionMatrix(data=as.factor(as.numeric(my_prediction_engine>0.5)),
                                           reference= as.factor(as.numeric(mydf_train$engine_binary)))
print(confusion_matrix_engine)

#Confusion matrix for testing data (new customers)
my_prediction_test_engine <- predict(logit_engine_cleaner, mydf_test, type="response")

cm_test_engine <- confusionMatrix(data=as.factor(as.numeric(my_prediction_test_engine>0.5)), 
                                  reference = as.factor(as.numeric(mydf_test$engine_binary)))
print(cm_test_engine)
#Plotting normalized logistic regression model - training dataset for amount as business success.
library(ggplot2)
ggplot(data= mydf_train, aes(x=click_charges_norm+impressions_norm+trans_conv_norm+
                               clicks_norm,y=amount_binary))+
  geom_smooth()
#Plotting normalized logistic regression model - testing dataset for amount as business success.
ggplot(data= mydf_test, aes(x=click_charges_norm+impressions_norm+trans_conv_norm+
                              clicks_norm,y=amount_binary))+
  geom_smooth()

# Adriana's Coding
#Import data set
library(readxl)
Air_France_Case_Spreadsheet_Supplement_1 <- read_excel("~/HULT/DATA R/Air France Case Spreadsheet Supplement-1.xls")
View(Air_France_Case_Spreadsheet_Supplement_1)
my_df_af <- Air_France_Case_Spreadsheet_Supplement_1
summary(my_df_af)
table(my_df_af$`Engine Click Thru %`)
table(my_df_af$`Publisher Name`)

num_visitors <- nrow(my_df_af)
num_var <- ncol(my_df_af)

hist(my_df_af$`Click Charges`)
hist((my_df_af$`Engine Click Thru %`))
hist(my_df_af$Amount)
hist(my_df_af$`Trans. Conv. %`)
hist(my_df_af$`Total Cost`)
hist(my_df_af$`Total Volume of Bookings`)


my_df_af$publisher_name_num <- gsub("Yahoo - US", "1", my_df_af$`Publisher Name`)
my_df_af$publisher_name_num <- gsub("Google - Global", "2", my_df_af$publisher_name_num)
my_df_af$publisher_name_num <- gsub("Google - US", "3", my_df_af$publisher_name_num)
my_df_af$publisher_name_num <- gsub("MSN - Global", "4", my_df_af$publisher_name_num)
my_df_af$publisher_name_num <- gsub("MSN - US", "5", my_df_af$publisher_name_num)
my_df_af$publisher_name_num <- gsub("Overture - Global", "6", my_df_af$publisher_name_num)
my_df_af$publisher_name_num <- as.numeric (gsub("Overture - US", "7", my_df_af$publisher_name_num))


my_amount_vector <- as.numeric(c(my_df_af$Amount>1304))
my_df_af$amount_binary <- replace(my_amount_vector, isTRUE(my_amount_vector), "1")                              
my_df_af$amount_binary <- gsub("TRUE", "1", my_df_af$amount_binary)
my_df_af$amount_binary <- as.numeric(gsub("FALSE", "0", my_df_af$amount_binary))

mean_min <- function(x){
  my_mean <- mean(x, na.rm=TRUE)
  my_min <- min(x, na.rm=TRUE)
  my_max <- max(x, na.rm=TRUE)
  my_std <- sd(x, na.rm=TRUE)
  return(c("min",my_min, 
           "max", my_max, 
           "mean", my_mean, 
           "sd", my_std))
} #closing the mean_min UDF

#Normalization using min-max re-scaling
norm_sd <- function(x){
  my_minmax <- (x-min(x))/(max(x)-min(x))
  return(my_minmax)
}#closing norm_sd UDF

my_df_af$click_charges_norm <- norm_sd(my_df_af$`Click Charges`)
my_df_af$impressions_norm <- norm_sd(my_df_af$Impressions)
my_df_af$engine_norm <- norm_sd(my_df_af$`Engine Click Thru %`)
my_df_af$trans_conv_norm <- norm_sd(my_df_af$`Trans. Conv. %`)
my_df_af$avg_pos_norm <- norm_sd(my_df_af$`Avg. Pos.`)
my_df_af$bookings_norm <- norm_sd(my_df_af$`Total Volume of Bookings`)
my_df_af$total_cost_norm <- norm_sd(my_df_af$`Total Cost`)
my_df_af$clicks_norm <- norm_sd(my_df_af$Clicks)                                   
my_df_af$amount_norm <- norm_sd(my_df_af$Amount)

#Performing random sampling to split into training and testing 
training_indx <- sample(1:num_visitors, size = .80*num_visitors)
mydf_train <- my_df_af[training_indx, ]

#Create indexes for testing for the 20% remain data
mydf_test <- my_df_af[-training_indx,]

#Designing my logistic regression for amount_binary 
logit <- glm(data=mydf_train, amount_binary~`Click Charges`+Impressions+`Engine Click Thru %`+`Trans. Conv. %`+`Avg. Pos.`+
               Clicks, family="binomial")
summary(logit)

#Cleaning my logistic regression 
logit_clean <- glm(data=mydf_train, amount_binary~`Click Charges`+Impressions+`Trans. Conv. %`+`Avg. Pos.`+
                     Clicks, family="binomial")
summary(logit_clean)

#Cleaning my logistic regression 
logit_cleaner <- glm(data=mydf_train, amount_binary~`Click Charges`+Impressions+`Trans. Conv. %`+
                       Clicks, family="binomial")
summary(logit_cleaner)

exp(-8.411e-04)-1 #The odds of having an amount bigger than 1034, decrease by -0.0840% by each click charge
(exp(-2.667e-06)-1)*100 #The odds of having an amount bigger than 1034, decrease by -0.00026% by each impression 
exp(7.754e-02)-1 #The odds of having an amount bigger than 1034, increase by 8.06% by each trans. cov%
(exp(7.288e-03)-1)*100 #The odds of having an amount bigger than 1034, increase by 0.73% by each click


#Building a normalized (unitless) logistic amount_binary
logit_norm <- glm(data=mydf_train, amount_binary~click_charges_norm+impressions_norm+trans_conv_norm+
                    clicks_norm, family="binomial")
summary(logit_norm)

#Performance testing confusion matrix
install.packages("caret")
library(caret)

#Confusion Matrix for amount_binary
my_prediction <- predict(logit_cleaner, mydf_train, type="response")
confusion_matrix_amount <- confusionMatrix(data=as.factor(as.numeric(my_prediction>0.5)),
                                           reference= as.factor(as.numeric(mydf_train$amount_binary)))
print(confusion_matrix_amount)

#Confusion matrix for testing data (new customers)
my_prediction_test <- predict(logit_cleaner, mydf_test, type="response")

cm_test_amount <- confusionMatrix(data=as.factor(as.numeric(my_prediction_test>0.5)), 
                                  reference = as.factor(as.numeric(mydf_test$amount_binary)))
print(cm_test_amount)


#Confusion Matrix for engine_binary
my_prediction_engine <- predict(logit_engine_cleaner, mydf_train, type="response")
confusion_matrix_engine <- confusionMatrix(data=as.factor(as.numeric(my_prediction_engine>0.5)),
                                           reference= as.factor(as.numeric(mydf_train$engine_binary)))
print(confusion_matrix_engine)

#Confusion matrix for testing data (new customers)
my_prediction_test_engine <- predict(logit_engine_cleaner, mydf_test, type="response")

cm_test_engine <- confusionMatrix(data=as.factor(as.numeric(my_prediction_test_engine>0.5)), 
                                  reference = as.factor(as.numeric(mydf_test$engine_binary)))
print(cm_test_engine)

# Aziz's coding

install.packages("ROCR")
Mean_min_max_sd <- function(x){
  mean_AF <- mean(x, na.rm = TRUE)
  min_AF <- min(x, na.rm = TRUE)
  Max_AF <- max(x, na.rm = TRUE)
  sd_AF <- sd(x, na.rm = TRUE)
  return(c("mean", mean_AF,
           "min", min_AF,
           "max", Max_AF,
           "st.dev.", sd_AF))
}# closing mean_min_sd function

# calculating mean, min, max, sd on Amount
Mean_min_max_sd(Air_France_Case_Spreadsheet_Supplement_1$Amount)
Air_France_Case_Spreadsheet_Supplement_1$amount_binary <-
  ifelse(Air_France_Case_Spreadsheet_Supplement_1$Amount>=1033.68, 1, 0)


# gini tree
library(rpart)
library(rpart.plot)

my_tree <- rpart(data=dat_train, binary~Amount+Impressions+ROA+Net_revenu+
                   Total_Volume_of_Bookings+Search_Engine_Bid+Engine_Click_Thru, method = "class", cp=0.02)
rpart.plot(my_tree, extra = 1, type = 5)


