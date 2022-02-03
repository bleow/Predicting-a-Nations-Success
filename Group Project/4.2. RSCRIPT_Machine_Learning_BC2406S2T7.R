#################################  B C 2 4 0 6   S E M I N A R   2   T E A M   7  #################################
########################################  M A C H I N E   L E A R N I N G  ########################################
######################################### Instructor: Prof Vivek Choudhary ########################################

setwd("C:/Users/.../BC2406_S2_Team7_Project/3. Data, Data Dictionary")

library(rpart)
library(rpart.plot)
library(data.table)
library(nnet)
library(caret)
library(caTools)
library(car)
library(dplyr)
library(ggplot2)
library(tidyr)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> #
# >>>>>>>>>>>>>>>>>>> continuation of part 4 from the data cleaning Rscript >>>>>>>>>>>>>>>>>>> #
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>       >1>       >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> #

############## exploratory analysis of the PISA predictions ##############
pisa_predict = fread("PISA_PREDICTORS.csv")

#### Univariate plots ####
# Plots reveal presence of outliers.
# If these outliers are caused by data entry error, they MUST be corrected
# It is observed that none of the outliers are nonsensical in context 
# (e.g. <0 years or >100%). This makes sense since the data is coming
# from the World Bank, a reputable source. 
# Else, they only need to be removed for linear regression if they are
# influential. We take a look at this later (using Cook's Distance Plot)
defaultW <- getOption("warn")
options(warn = -1)

# PISA Read
ggplot(pisa_predict) + aes(x=PISA_read) + geom_area(stat="bin", fill="lightblue", bins = 30) + labs(x = "PISA Score on the Reading Scale", y = "Count", title = "Distribution of PISA Reading Scores") + scale_fill_brewer(palette="BrBG")##; Sys.sleep(0.5)
ggplot(pisa_predict) + aes(x=PISA_read) + geom_boxplot(width=.2, fill="pink") + labs(x = "PISA Score on the Reading Scale", title = "Distribution of PISA Reading Scores")#; Sys.sleep(0.5)
# More skewed than math/sci. Has a single outlier below average
# Mean is similar to math/sci at around 470-480

# PISA Math
# ggplot(pisa_predict) + aes(x=PISA_math) + geom_area(stat="bin", fill="lightblue", bins = 30) + labs(x = "PISA Score on the Mathematics Scale", y = "Count", title = "Distribution of PISA Mathematics Scores") + scale_fill_brewer(palette="BrBG")#; Sys.sleep(2)
# ggplot(pisa_predict) + aes(x=PISA_math) + geom_boxplot(width=.2, fill ="pink") + labs(x = "PISA Score on the Mathematics Scale", title = "Distribution of PISA Math Scores")#; Sys.sleep(2)
# Forms a slightly left-skewed normal distribution (with two peaks) as seen from the boxplot's central line
# No outliers 

# PISA Sci
# ggplot(pisa_predict) + aes(x=PISA_sci) + geom_area(stat="bin", fill="lightblue", bins = 30) + labs(x = "PISA Score on the Science Scale", y = "Count", title = "Distribution of PISA Science Scores") + scale_fill_brewer(palette="BrBG")#; Sys.sleep(0.5)
# ggplot(pisa_predict) + aes(x=PISA_sci) + geom_boxplot(width=.2, fill="pink") + labs(x = "PISA Score on the Science Scale", title = "Distribution of PISA Science Scores")#; Sys.sleep(0.5)
# Similar to math - a slightly left-skewed normal distribution with two peaks
# No outliers
# Mean is similar to math at around 480. 

# Sec_sch_expectancy
ggplot(pisa_predict) + aes(x=Sec_sch_expectancy) + geom_area(stat="bin", fill="lightblue", bins = 30) + labs(x = "Secondary School Life Expectancy (Years)", y = "Count", title = "Distribution of Secondary School Life Expectancy")#; Sys.sleep(0.5)
ggplot(pisa_predict) + aes(x=Sec_sch_expectancy) + geom_boxplot(width=.2, fill="pink") + labs(x = "Secondary School Life Expectancy (Years)", title = "Distribution of Secondary School Life Expectancy")#; Sys.sleep(0.5)
# An even but not normally distributed spread, with no outliers.
# Mean is around 4-5 years, consistent with our intuition.

# Pre_pri_sch_expectancy
ggplot(pisa_predict) + aes(x=Pre_pri_sch_expectancy) + geom_area(stat="bin", fill="lightblue", bins = 30) + labs(x = "Pre-Primary School Life Expectancy (Years)", y = "Count", title = "Distribution of Pre-Primary School Life Expectancy")#; Sys.sleep(0.5)
ggplot(pisa_predict) + aes(x=Pre_pri_sch_expectancy) + geom_boxplot(width=.2, fill="pink") + labs(x = "Pre-Primary School Life Expectancy (Years)", title = "Distribution of Pre-Primary School Life Expectancy")#; Sys.sleep(0.5)
# Histogram shows decreasing population density as years increase. 
# Has a handful of outliers above average.

# Pri_sch_expectancy
ggplot(pisa_predict) + aes(x=Pri_sch_expectancy) + geom_area(stat="bin", fill="lightblue", bins = 30) + labs(x = "Primary School Life Expectancy (Years)", y = "Count", title = "Distribution of Primary School Life Expectancy")#; Sys.sleep(0.5)
ggplot(pisa_predict) + aes(x=Pri_sch_expectancy) + geom_boxplot(width=.2, fill="pink") + labs(x = "Primary School Life Expectancy (Years)", title = "Distribution of Primary School Life Expectancy")#; Sys.sleep(0.5)
# Forms a slightly left-skewed normal distribution, again with two peaks. 
# Density is most concentrated at (i.e. the mean is) 6 years and falls sharply afterward, 
# consistent with our intuition that primary school lasts 6 years from P1-P6 in Singapore.
# Has outliers on both extreme ends.

# Pri_sec_sch_expectancy
# ggplot(pisa_predict) + aes(x=Pri_sec_sch_expectancy) + geom_area(stat="bin", fill="lightblue", bins = 30) + labs(x = "Primary and Secondary School Life Expectancy (Years)", y = "Count", title = "Distribution of Primary & Secondary School Life Expectancy")#; Sys.sleep(0.5)
# ggplot(pisa_predict) + aes(x=Pri_sec_sch_expectancy) + geom_boxplot(width=.2, fill="pink") + labs(x = "Primary and Secondary School Life Expectancy (Years)", title = "Distribution of Primary & Secondary School Life Expectancy")#; Sys.sleep(0.5)
# Forms a slightly left-skewed normal distribution. 
# Outliers lie on the left side, but a single outlier lies on the right side.
# Mean is around 11 years.

# Lower_sec_completion
# ggplot(pisa_predict) + aes(x=Lower_sec_completion) + geom_area(stat="bin", fill="lightblue", bins = 30) + labs(x = "Lower Secondary Completion Rate (%)", y = "Count", title = "Distribution of Completion Rate - Lower Secondary")#; Sys.sleep(0.5)
# ggplot(pisa_predict) + aes(x=Lower_sec_completion) + geom_boxplot(width=.2, fill="pink") + labs(x = "Lower Secondary Completion Rate (%)", title = "Distribution of Completion Rate - Lower Secondary")#; Sys.sleep(0.5)
# Roughly even but not normal distribution. No outliers.
# Mean is around 70%. As mentioned earlier, the data makes sense as they fall between 1%-100%,
# reassuring us of data quality.

# Upper_sec_completion
# ggplot(pisa_predict) + aes(x=Upper_sec_completion) + geom_area(stat="bin", fill="lightblue", bins = 30) + labs(x = "Upper Secondary Completion Rate (%)", y = "Count", title = "Distribution of Completion Rate - Upper Secondary")#; Sys.sleep(0.5)
# ggplot(pisa_predict) + aes(x=Upper_sec_completion) + geom_boxplot(width=.2, fill="pink") + labs(x = "Upper Secondary Completion Rate (%)", title = "Distribution of Completion Rate - Upper Secondary")#; Sys.sleep(0.5)
# Same observations as lower secondary (roughly even but not normal, no outliers)
# Mean is lower than lower secondary at 50%, as expected since upper sec is a progression from
# lower sec and hence completion rate will decrease. No country has 100% completion rate too,
# which makes sense given that people can choose to work after taking their 'O' Levels,
# at least in the context of Singapore.

# Pri_completion
# ggplot(pisa_predict) + aes(x=Pri_completion) + geom_area(stat="bin", fill="lightblue", bins = 30) + labs(x = "Primary Education Completion Rate (%)", y = "Count", title = "Distribution of Completion Rate - Primary")#; Sys.sleep(0.5)
# ggplot(pisa_predict) + aes(x=Pri_completion) + geom_boxplot(width=.2, fill="pink") + labs(x = "Primary Education Completion Rate (%)", title = "Distribution of Completion Rate - Primary")#; Sys.sleep(0.5)
# Increasing density over percentage. Some outliers below the average.

#### Pair plots #### 
# Plots reveal how the variables are correlated with one another.
# Variables that are highly correlated with the response variable
# are more "promising". HOWEVER, we should not remove lowly-correlated
# variables at this point. We take a look at this later using backward
# elimination via AIC.

# PISA scores
pisaplot <- pisa_predict[,c("PISA_math", "PISA_sci", "PISA_read")]
pairs(pisaplot)
cor(pisaplot, use="complete.obs")
# Very strong linear association between all the PISA scores. 
# This is to be expected, since each data row measures the same student 
# population and country at the same time.
# Hence, we will just use PISA_math as the y variable when building our 
# model. Only one (math/science/reading) score is the response variable 
# at at any point in time, and they should not be predictor variables.
pisa_math = pisa_sci = pisa_read = pisa_predict
pisa_math = subset(pisa_math, select = -c(`PISA_sci` , `PISA_read`))
pisa_sci  = subset(pisa_sci , select = -c(`PISA_math`, `PISA_read`))
pisa_read = subset(pisa_read, select = -c(`PISA_math`, `PISA_sci`))

# School Life Expectancy
p <- pisa_predict[,c("PISA_math", "Pre_pri_sch_expectancy", "Pri_sch_expectancy", "Pri_sec_sch_expectancy")]
pairs(p)
cor(p, use="complete.obs")

# # School Completion Rates
# p <- pisa_predict[,c("PISA_math", "Lower_sec_completion", "Upper_sec_completion", "Pri_completion")]
# pairs(p)
# cor(p, use="complete.obs")
# 
# # Enrolment ratio
# p <- pisa_predict[,c("PISA_math", "Pre_pri_enrolment_ratio", "Pri_enrolment_ratio", "Sec_enrolment_ratio")]
# pairs(p)
# cor(p, use="complete.obs")

p <- pisa_predict[,c("PISA_math", "Illiterate_pop", "Gov_ex_capita", "Gov_ex_3")]
pairs(p)
cor(p, use="complete.obs")
# Let's take a closer look at Gov_ex_3
ggplot(pisa_predict) + aes(x=Gov_ex_3, y= PISA_math) + geom_jitter() + geom_smooth(method=lm) + labs(x = "Government Expenditure on Education per Capita (lagged 3 years), US$ (millions)", y = "PISA score on the Mathematics Scale", title = "Govt Edu Expenditure per Capita (lagged) vs PISA Math Scores")
# Plotting Gov_ex_3 against PISA scores seems to give a logarithmic curve, 
# so we will take log(Gov_ex_3) instead
pisa_predict$Log_gov_ex_3 <- log(pisa_predict$Gov_ex_3)
ggplot(pisa_predict) + aes(x=Log_gov_ex_3, y= PISA_math) + geom_jitter() + geom_smooth(method=lm) + labs(x = "Government Expenditure on Education per Capita (lagged 3 years), US$ (millions)", y = "PISA score on the Mathematics Scale", title = "Govt Edu Expenditure per Capita (lagged) vs PISA Math Scores")
# It is definitely looking more linear, so we will add it to the model
pisa_math$Log_gov_ex_3 = log(pisa_math$Gov_ex_3)
pisa_sci$Log_gov_ex_3  = log(pisa_sci$Gov_ex_3)
pisa_read$Log_gov_ex_3 = log(pisa_read$Gov_ex_3)

options(warn = defaultW)

# We remove the time and country column from the dataset, 
# we do not use them in the prediction since this is not a 
# time-series or region-based prediction
pisa_math = pisa_math[, -c(1:4)]
pisa_sci  = pisa_sci[, -c(1:4)]
pisa_read = pisa_read[, -c(1:4)]

linregdata = pisa_math  # Removing PISA_sci and PISA_read
cartdata   = pisa_math  # Saving this data for a later part (part 6)

################### END PART 4: PREPARING PISA PREDICTOR VARIABLES ####################

#                                        #~-~#                                        #

#######################################################################################
#################### PART 5: MODELLING PISA with EDUCATION (LINREG) ###################
##################### (supplementing Section 3.3.3 of the report) #####################
#######################################################################################

############## attempting to use all variables to predict PISA_math ##############
full_model <- lm(PISA_math ~ ., data = linregdata)


summary(full_model)

# ALL 9 residuals are 0: no residual degrees of freedom!
# PISA score can be perfectly explained using these variables
# Concerning issue is that there are only 9 data points used in the model - why?

# Having many predictor variables with missing values will limit the data we work with 
# resulting in the problem above, since there are not enough complete rows.
# Sanity check - how many rows if all columns are non-null?
nrow(na.omit(linregdata))
# Only 9 left - explaining the over-fitting on the model above

############## dropping columns with very high NA% ##############
# When dealing with NAs, we should replace them with the mean of the relevant subgroup,
# or use a model. However, the mean of the relevant subgroup is difficult to determine
# (year or country?). More importantly, this will not fill accurate data if the 
# variable has a very high percentage of NA, e.g. more than 50%, where essentially 
# more than half the data is not "real". Hence we will explore which variables 
# have high percentage of NA values and drop them accordingly
colMeans(is.na(linregdata))

# Since our data set has around 9500 rows now, we will only drop columns with extremely high NA percentage,
# at >80% (arbitrary value)
linregdata <-dplyr::select(linregdata, -c('Lower_sec_completion', 
                                          'Upper_sec_completion',
                                          'Pri_completion',
                                          'Literacy_rate',
                                          'STEM_ter_grad_percent',
                                          'Illiterate_pop',
                                          'Pre_pri_pupil_teacher_ratio'))

# Checking how many data points we have after dropping
linregdata <- na.omit(linregdata)
nrow(linregdata)
#520 rows left

############## using what's left to generate a linear regression model ##############
# Train Test Split
set.seed(2406)
train <- createDataPartition(linregdata$PISA_math,
                             p = 0.80, # % of data going to training
                             times = 1,
                             list = F)
linreg.train <- linregdata[ train,]
linreg.test  <- linregdata[-train,]

# Verifying that distribution of Y is similar in trainset vs testset.
summary(linreg.train$PISA_math)
summary(linreg.test$PISA_math)

m1 <- lm(PISA_math ~ ., data = linreg.train)
summary(m1) #adj R2 of 0.7868
RMSE <- sqrt(c(crossprod(m1$residuals)) / length(m1$residuals))
RMSE
# Gives an RMSE of 22.61908

pred_values <- predict(m1,linreg.test)
sqrt(mean((pred_values-linreg.test$PISA_math)**2))
# In the test set, it is 25.93762

############## performing backward elimination using AIC ##############
# Using the step function to determine which variables that we can remove
stepped_model <- step(m1)
# According to step function, we can remove: 
# Sec_enrolment_num, Pre_pri_enrolment_ratio, Pri_sec_sch_expectancy, 
# Sec_sch_expectancy, Pri_sch_expectancy, Sec_enrolment_ratio
# Removing them reduces AIC, so let's remove them

summary(stepped_model) #adj R2 of 0.7887
RMSE <- sqrt(c(crossprod(stepped_model$residuals)) / length(stepped_model$residuals))
RMSE
# Gives an RMSE of 22.69114

pred_values <- predict(stepped_model,linreg.test)
sqrt(mean((pred_values-linreg.test$PISA_math)**2))
# In the test set, it is 25.97954

linreg.train <-dplyr::select(linreg.train, -c('Sec_enrolment_num', 
                                              'Pre_pri_enrolment_ratio', 
                                              'Pri_sec_sch_expectancy',
                                              'Sec_sch_expectancy',
                                              'Pri_sch_expectancy',
                                              'Sec_enrolment_ratio'))
linreg.test  <-dplyr::select(linreg.test,   -c('Sec_enrolment_num',
                                               'Pre_pri_enrolment_ratio', 
                                               'Pri_sec_sch_expectancy',
                                               'Sec_sch_expectancy',
                                               'Pri_sch_expectancy',
                                               'Sec_enrolment_ratio'))

############## dealing with multicollinearity ##############
vif(stepped_model)
# Even after step(), we still have multicollinear variables indicated by their high VIF
# These are problematic and should be removed since they are 1) not control variables,
# 2) not caused by powers or products and 3) not dummy variables

# Three highest mulicollinearity:
# Gov_ex_3:         66
# Gov_ex_capita:    63
# Population:       62

p <- pisa_predict[,c("Gov_ex_capita", "Gov_ex_3")]
pairs(p)
cor(p, use="complete.obs")

# Which one to remove? Remove both in two different scenarios and check

# Gov_ex_3
m2 <- lm(PISA_math ~ .-Gov_ex_3, data = linreg.train)
RMSE <- sqrt(c(crossprod(m2$residuals)) / length(m2$residuals))
summary(m2) #0.7792
RMSE #23.22115
vif(m2)

# Gov_ex_capita
m2 <- lm(PISA_math ~ .-Gov_ex_capita, data = linreg.train)
RMSE <- sqrt(c(crossprod(m2$residuals)) / length(m2$residuals))
summary(m2) #0.7873
RMSE #22.79488
vif(m2)

# In both cases, multicollinearity of the remaining variable decreases to 6.
# Removing Gov_ex_3 will increase error margin and decrease explainability more than Gov_ex_capita. 
# Hence we remove Gov_ex_capita instead of Gov_ex_3 as what the ratings alone would suggest.

vif(m2)
# Many VIFs are still high. Population is the highest.

# Population
m3 <- lm(PISA_math ~ .-Population -Gov_ex_capita, data = linreg.train)
RMSE <- sqrt(c(crossprod(m3$residuals)) / length(m3$residuals))
summary(m3) #0.7651
RMSE #23.98203

vif(m3)
# All VIFs are now below 10. There is no universally accepted cut-off for VIF (can be >5 or >10).
# Since education is a single, mostly homogeneous industry in most countries, 
# it is expected that their metrics will be somewhat collinear.

# Call step() again after removing collinearity to continue removing noisy variables
step(m3)
m4 <- lm(PISA_math ~ .-Population -Gov_ex_capita -Ter_enrolment_num, data = linreg.train)
RMSE <- sqrt(c(crossprod(m4$residuals)) / length(m4$residuals))
summary(m4) #0.7645
RMSE #24.04172

step(m4)
m5 <- lm(PISA_math ~ .-Population -Gov_ex_capita -Ter_enrolment_num -Gov_ex, data = linreg.train)
RMSE <- sqrt(c(crossprod(m5$residuals)) / length(m5$residuals))
summary(m5) #0.7648

step(m5) # no more variables to cut

RMSE #24.05426
pred_values <- predict(m5,linreg.test); sqrt(mean((pred_values-linreg.test$PISA_math)**2))
# In the test set, it is 28

linreg.train <-dplyr::select(linreg.train, -c('Population', 'Gov_ex_capita', 'Ter_enrolment_num', 'Gov_ex'))
linreg.test  <-dplyr::select(linreg.test,  -c('Population', 'Gov_ex_capita', 'Ter_enrolment_num', 'Gov_ex'))

############## model diagnostics ##############
# Diagnostic Plots
m5 <- lm(PISA_math ~ ., data = linreg.train)
par(mfrow = c(2,2)) 
plot(m5) 
par(mfrow = c(1,1))  # Reset plot options to 1 chart in one plot.

## Residuals vs Fitted (Top left): residuals are roughly equally spread around the 
## horizontal line, with some deviation as PISA scores increase
## QQ Plot (Top right): Residuals deviate slightly from normal distribution at 
## extremities, but nothing too serious
## Scale-location (Bottom left): Residuals are quite randomly spread, line is almost 
## horizontal 
## Influential outliers (bottom right): There are some data points slightly exceeding 
## Cook's distance, but they are not influential because 1) they lie on both sides of 
## the graph and mostly cancel each other out, and 2) their effects are balanced out 
## by the large cluster of data points at the same leverage but nearer to the centre.

################# END PART 5: MODELLING PISA with EDUCATION (LINREG) ##################

#                                        #~-~#                                        #

#######################################################################################
################## PART 6: MODELLING PISA with EDUCATION (Cont. CART) #################
##################### (supplementing Section 3.3.4 of the report) #####################
#######################################################################################

set.seed(2406)
# Taking only data that has non null PISA_math values, 
# since PISA_math is our response variable
cartdata = cartdata[!is.na(cartdata$PISA_math),]

# Train Test Split
train <- createDataPartition(cartdata$PISA_math,
                             p = 0.80, # % of data going to training
                             times = 1,
                             list = F)
cartdata.train <- cartdata[ train,]
cartdata.test  <- cartdata[-train,]

# Verifying that distribution of Y is similar in trainset vs testset.
summary(cartdata.train$PISA_math)
summary(cartdata.test$PISA_math)

############## growing model ##############
# set cp to 0 to grow the tree to maximum and minsplit to 4 since this is a moderately sized dataset
model <- rpart(PISA_math ~ ., method = 'anova', control = rpart.control(minsplit = 4, cp = 0), data=cartdata.train)
printcp(model)
plotcp(model)

############## pruning model ##############
# find the optimal cp to prune the tree at
CVerror.cap <- model$cptable[which.min(model$cptable[,"xerror"]), "xerror"] + model$cptable[which.min(model$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (model$cptable[i,j] > CVerror.cap) {
    i <- i + 1
} 
cp.opt = ifelse(i > 1, sqrt(model$cptable[i,1] * model$cptable[i-1,1]), 1)
cp.opt
model <- prune(model, cp=cp.opt)

############## finding RMSE ##############
pred_value <- predict(model, newdata = cartdata.train)
sqrt(mean((pred_value-cartdata.train$PISA_math)^2))
# RMSE of 14.92613 which beats the linear regression model
# Hence we will use this model to predict PISA_scores

pred_value <- predict(model, newdata = cartdata.test)
sqrt(mean((pred_value-cartdata.test$PISA_math)^2))
# RMSE of 21.58275 on test set

############## using continuous CART model to predict PISA values ##############

# PISA Math Predictions
set.seed(17)
cart_math <- pisa_math[!is.na(pisa_math$PISA_math),]
model <- rpart(PISA_math ~ ., method = 'anova', control = rpart.control(minsplit = 2, cp = 0), data=cart_math)
CVerror.cap <- model$cptable[which.min(model$cptable[,"xerror"]), "xerror"] + model$cptable[which.min(model$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (model$cptable[i,j] > CVerror.cap) {i <- i + 1} 
cp.opt = ifelse(i > 1, sqrt(model$cptable[i,1] * model$cptable[i-1,1]), 1)
cp.opt
model <- prune(model, cp=cp.opt)
pisa_math$PISA_pred_math <- predict(model, pisa_math)

# PISA Sci Predictions
set.seed(17)
cart_sci <- pisa_sci[!is.na(pisa_sci$PISA_sci),]
model <- rpart(PISA_sci ~ ., method = 'anova', control = rpart.control(minsplit = 2, cp = 0), data=cart_sci)
CVerror.cap <- model$cptable[which.min(model$cptable[,"xerror"]), "xerror"] + model$cptable[which.min(model$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (model$cptable[i,j] > CVerror.cap) {i <- i + 1} 
cp.opt = ifelse(i > 1, sqrt(model$cptable[i,1] * model$cptable[i-1,1]), 1)
cp.opt
model <- prune(model, cp=cp.opt)
pisa_sci$PISA_pred_sci <- predict(model, pisa_sci)

# PISA Read Predictions
set.seed(17)
cart_read <- pisa_read[!is.na(pisa_read$PISA_read),]
model <- rpart(PISA_read ~ ., method = 'anova', control = rpart.control(minsplit = 2, cp = 0), data=cart_read)
CVerror.cap <- model$cptable[which.min(model$cptable[,"xerror"]), "xerror"] + model$cptable[which.min(model$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (model$cptable[i,j] > CVerror.cap) {i <- i + 1} 
cp.opt = ifelse(i > 1, sqrt(model$cptable[i,1] * model$cptable[i-1,1]), 1)
cp.opt
model <- prune(model, cp=cp.opt)
pisa_read$PISA_pred_read <- predict(model, pisa_read)

############## transferring predicted columns back to original dataframe ##############
pisa_predict$PISA_pred_math <- pisa_math$PISA_pred_math
pisa_predict$PISA_pred_sci  <- pisa_sci$PISA_pred_sci
pisa_predict$PISA_pred_read <- pisa_read$PISA_pred_read

# If-else logic to merge the predicted PISA scores + actual PISA scores
# If actual PISA scores exist, then PISA_xxx_new = actual score
# else PISA_xxx_new = predicted score
pisa_predict$PISA_math_new = NA
for (i in 1:(nrow(pisa_predict))){
    if (is.na(pisa_predict$PISA_math[i])){
        pisa_predict$PISA_math_new[i] = pisa_predict$PISA_pred_math[i]
    }
    else{
        pisa_predict$PISA_math_new[i] = pisa_predict$PISA_math[i]
    }
}
pisa_predict$PISA_sci_new = NA
for (i in 1:(nrow(pisa_predict))){
    if (is.na(pisa_predict$PISA_sci[i])){
        pisa_predict$PISA_sci_new[i] = pisa_predict$PISA_pred_sci[i]
    }
    else{
        pisa_predict$PISA_sci_new[i] = pisa_predict$PISA_sci[i]
    }
}
pisa_predict$PISA_read_new = NA
for (i in 1:(nrow(pisa_predict))){
    if (is.na(pisa_predict$PISA_read[i])){
        pisa_predict$PISA_read_new[i] = pisa_predict$PISA_pred_read[i]
    }
    else{
        pisa_predict$PISA_read_new[i] = pisa_predict$PISA_read[i]
    }
}

# Sanity check to see how many PISA values
table(is.na(pisa_predict$PISA_math_new))
# All rows filled

############## export the dataframe with just the final filled pisa values columns ##############
pisa_predict <-dplyr::select(pisa_predict, -c('PISA_math', 'PISA_sci','PISA_read',
                          'PISA_pred_math','PISA_pred_sci','PISA_pred_read'))

write.csv(pisa_predict,"PISA_results_cart.csv", row.names = FALSE)

rm(list = ls())

############################# END PART 6: MODELLING PISA with EDUCATION (Cont. CART) ##############################




# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #
# <<<<<<<<<<<<<< with the PISA values modelled, we can now move on to obtain GDP <<<<<<<<<<<<<< #
# <<<<<<<<<<<<<<<<<<<<<<<<<< continued in the data cleaning Rscript <<<<<<<<<<<<<<<<<<<<<<<<<<< #
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<       <2<       <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #




# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>       >3>       >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> #
# >>>>>>>>>>>>>>>>>>>>> the data cleaning is done, we have clean GDP data >>>>>>>>>>>>>>>>>>>>> #
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> #




##################################################################################################################
############ PART 7: MODELLING GDP GROWTH with ABS EDUCATION (Cont. CART) #############
###################### (supplementing Section 3.4 of the report) ######################
#######################################################################################
dt <- fread("GDP_INPUT_EDU.csv")

############## removing GDP columns that aren't the response nor predictor variable ##############
dt <-dplyr::select(dt, -c('GDP_1','GDP_capita_1','GDP_capita_growth_1'))

dt <- dt[!is.na(dt$GDP_growth_1),]
set.seed(66)
train <- createDataPartition(dt$GDP_growth_1,
                             p = 0.75, # % of data going to training
                             times = 1,
                             list = F)
trainset <- dt[ train,]
testset  <- dt[-train,]

############## CART Model  -  Using all absolute education metrics to predict GDP growth ##############
model <- rpart(GDP_growth_1 ~ ., data = trainset, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))
printcp(model)
plotcp(model)
CVerror.cap <- model$cptable[which.min(model$cptable[,"xerror"]), "xerror"] + model$cptable[which.min(model$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (model$cptable[i,j] > CVerror.cap) {
    i <- i + 1
} 
cp.opt = ifelse(i > 1, sqrt(model$cptable[i,1] * model$cptable[i-1,1]), 1)
cp.opt
model <- prune(model, cp=cp.opt)

pred_values <- predict(model,trainset)
sqrt(mean((pred_values-trainset$GDP_growth_1)**2))
# RMSE of 6.496138

pred_values <- predict(model,testset)
sqrt(mean((pred_values-testset$GDP_growth_1)**2))
# RMSE of 5.683342

# A deviation of 6 is way too huge in the context of GDP growth since it is usually not a large number.
# It is interesting to note that testset error is lower than trainset error. This means that we are 
# underfitting the model. This inference is supported by the graph on the right, which shows that
# the tree was pruned to its bare minimum, i.e. all the data is useless in the prediction.
# Qualitatively speaking, this aligns with our intuition since it does not make sense to only 
# use education metrics to predict GDP growth since GDP growth is affected by many other factors.
# Hence, we will attempt to include other non-education related variables that have been discussed
# in previous literature, to determine if it will increase the accuracy of our model

########### END PART 7: MODELLING GDP GROWTH with ABS EDUCATION (Cont. CART) ###########


# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #
# <<<<<<<<<<< with a change in methodology, we now need to obtain macroeconmic data <<<<<<<<<<< #
# <<<<<<<<<<<<<<<<<<<<<<<<<< continued in the data cleaning Rscript <<<<<<<<<<<<<<<<<<<<<<<<<<< #
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<       <4<       <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #



# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>       >5>       >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> #
# >>>>>>>>>>>>>>>>>>>>>>>>>> we now have cleaned macroeconomic data. >>>>>>>>>>>>>>>>>>>>>>>>>> #
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> #


#######################################################################################
##### PART 8: MODELLING GDP GROWTH with ABS EDUCATION + ECONOMIC VARS (Cont. CART) ####
###################### (supplementing Section 3.5 of the report) ######################
#######################################################################################
dt <- fread("GDP_INPUT_ALL.csv")

############## removing GDP columns that aren't the response nor predictor variable ##############
dt <-dplyr::select(dt, -c('GDP_1','GDP_capita_1','GDP_capita_growth_1'))
dt <- dt[!is.na(dt$GDP_growth_1),]
set.seed(77)
train <- createDataPartition(dt$GDP_growth_1,
                             p = 0.85, # % of data going to training
                             times = 1,
                             list = F)
trainset <- dt[ train,]
testset  <- dt[-train,]

############## CART Model  -  Using education + economic metrics to predict GDP growth ##############
model <- rpart(GDP_growth_1 ~ ., data = trainset, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))
printcp(model)
plotcp(model)
CVerror.cap <- model$cptable[which.min(model$cptable[,"xerror"]), "xerror"] + model$cptable[which.min(model$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (model$cptable[i,j] > CVerror.cap) {
    i <- i + 1
} 
cp.opt = ifelse(i > 1, sqrt(model$cptable[i,1] * model$cptable[i-1,1]), 1)
cp.opt
model <- prune(model, cp=cp.opt)

pred_values <- predict(model,trainset)
sqrt(mean((pred_values-trainset$GDP_growth_1)**2))
# 6.407968

pred_values <- predict(model,testset)
sqrt(mean((pred_values-testset$GDP_growth_1)**2))
# 5.670133

# Even though the model has become marginally more accurate, the improvement is not statistically
# significant enough.
# We posit that using absolute values of the metric to predict GDP growth, a % change,
# is not a proper way to predict since absolute values do not contain information about changes, 
# especially in non-time series models like ours.
# We hence propose predicting absolute GDP instead of GDP growth.

rm(list = ls())

############## END PART 8: MODELLING GDP with PISA & OTHERS (Cont. CART) ##############

#                                        #~-~#                                        #

#######################################################################################
######## PART 9: MODELLING GDP with ABS EDUCATION + ECONOMIC VARS (CART/LINREG) #######
###################### (supplementing Section 3.6 of the report) ######################
#######################################################################################
dt <- fread("GDP_INPUT_ALL.csv")

############## removing GDP columns that aren't the response nor predictor variable ##############
dt <-dplyr::select(dt, -c('GDP_growth_1','GDP_capita_1','GDP_capita_growth_1'))

dt <- dt[!is.na(dt$GDP),]
train <- createDataPartition(dt$GDP_1,
                             p = 0.75, # % of data going to training
                             times = 1,
                             list = F)
trainset <- dt[ train,]
testset  <- dt[-train,]

############## CART Model  -  Using absolute education + economic metrics to predict GDP itself ##############
set.seed(2406)
model <- rpart(GDP_1 ~ ., data = dt, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))
printcp(model)
plotcp(model)
CVerror.cap <- model$cptable[which.min(model$cptable[,"xerror"]), "xerror"] + model$cptable[which.min(model$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (model$cptable[i,j] > CVerror.cap) {
    i <- i + 1
} 
cp.opt = ifelse(i > 1, sqrt(model$cptable[i,1] * model$cptable[i-1,1]), 1)
cp.opt
model <- prune(model, cp=cp.opt)

# Predictions
pred_value <- predict(model,dt)
actual_value <- dt$GDP
sqrt(mean((pred_value-actual_value)^2))
## RMSE in the tens of billions even when using the full dt (not trainset) which is unacceptable
## However, the tree is no longer being pruned to its minimum - finally some good news

############## Linear Regression Model  -  Using absolute education + economic metrics to predict GDP itself ##############
# Remove the problematic >80% NA columns as previously identified.
dt <-dplyr::select(dt, -c('Lower_sec_completion', 'Upper_sec_completion','Pri_completion','Literacy_rate',
                          'STEM_ter_grad_percent','Illiterate_pop','Pre_pri_pupil_teacher_ratio'))
m1 <- lm(GDP_1 ~ ., data = dt)
summary(m1)
RMSE <- sqrt(c(crossprod(m1$residuals)) / length(m1$residuals))
RMSE
# RMSE of more than 100 billion, which is worse than CART
# Can step() salvage this?
step(m1)
m2 <- lm(GDP_1 ~ -Pri_sec_sch_expectancy, data = dt)
#-Pre_pri_enrolment_num -Gross_capital_formation -Govt_expenditure_total -Pri_enrolment_num
#-Pre_pri_sch_expectancy -Pri_sec_sch_expectancy -Pri_sch_expectancy -Sec_sch_expectancy
#-PISA_read_new -Population 
summary(m2)
RMSE <- sqrt(c(crossprod(m2$residuals)) / length(m2$residuals))
RMSE
# RMSE becomes worse (in the trillions) when attempting to do backward elimination.

# Even though the predictions are stil inaccurate, we are now able to get a tree of size >1.
# This tells us we are on the right line of thinking - to compare absolute with absolute, and
# % change to % change.
# Following this same idea, we now attempt to calculate the year on year % change for 
# each variable to match GDP growth (instead of the other way around as just shown)

rm(list = ls())

#### END PART 9: MODELLING GDP with ABS EDUCATION + ECONOMIC VARS (CART/LINREG)  ######


# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #
# <<<<<<<<<<<<<<<<<<<<<<<<<< calculate percentage change in the vars <<<<<<<<<<<<<<<<<<<<<<<<<< #
# <<<<<<<<<<<<<<<<<<<<<<<<<< continued in the data cleaning Rscript <<<<<<<<<<<<<<<<<<<<<<<<<<< #
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<       <6<       <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< #

# <<< (part 10) <<<

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>       >7>       >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> #
# >>>>>>>>>>>>>>>>>>>>>>>>>>>> we now have the percentage changes. >>>>>>>>>>>>>>>>>>>>>>>>>>>> #
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> #


#######################################################################################
############# PART 11: MODELLING GDP GROWTH with ECONOMIC % (Cont. CART) ##############
##################### (supplementing Section 3.8.1 of the report) #####################
#######################################################################################
dt <- fread("GDP_INPUT_ALL_P.csv")
# Dropping the other y variables
dt <- dplyr::select(dt, -c('GDP_1','GDP_capita_1','GDP_capita_growth_1'))

############## using economic variables only to predict next year's GDP growth ##############
econ <-dplyr::select(dt, c('GDP_growth_1','Trade_change','Gross_capital_formation_change','Govt_expenditure_total_change','Life_expectancy_change'))

# Train Test Split
set.seed(5)
econ <- na.omit(econ)
train <- createDataPartition(econ$GDP_growth_1,
                             p = 0.75, # % of data going to training
                             times = 1,
                             list = F)
train.orig <- econ[ train,]
test       <- econ[-train,]


############## CART Model  -  Using % change in economic metrics only to predict GDP growth ##############
set.seed(5)
model <- rpart(GDP_growth_1 ~ ., data = train.orig, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))
printcp(model)
plotcp(model)
CVerror.cap <- model$cptable[which.min(model$cptable[,"xerror"]), "xerror"] + model$cptable[which.min(model$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (model$cptable[i,j] > CVerror.cap) {
    i <- i + 1
} 
cp.opt = ifelse(i > 1, sqrt(model$cptable[i,1] * model$cptable[i-1,1]), 1)
cp.opt
model <- prune(model, cp=cp.opt)

pred_values <- predict(model,test)
sqrt(mean((pred_values-test$GDP_growth_1)**2))
# RMSE of 5.225843


########### END PART 11: MODELLING GDP GROWTH with ECONOMIC % (Cont. CART) ############

#                                        #~-~#                                        #

#######################################################################################
######## PART 12: MODELLING GDP GROWTH with EDUCATION+ECONOMIC % (CART/LINREG) ########
#################### (supplementing Section 3.8.2/3 of the report) ####################
#######################################################################################
all <- na.omit(dt)

# Train Test Split
set.seed(5)
train <- createDataPartition(all$GDP_growth_1,
                             p = 0.75, # % of data going to training
                             times = 1,
                             list = F)
train.orig <- all[ train,]
test       <- all[-train,]

############## CART Model  -  Using % change in economic and educational metrics to predict GDP growth ##############
set.seed(5)
model <- rpart(GDP_growth_1 ~ ., data = train.orig, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))
printcp(model)
plotcp(model)
CVerror.cap <- model$cptable[which.min(model$cptable[,"xerror"]), "xerror"] + model$cptable[which.min(model$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (model$cptable[i,j] > CVerror.cap) {
    i <- i + 1
} 
cp.opt = ifelse(i > 1, sqrt(model$cptable[i,1] * model$cptable[i-1,1]), 1)
cp.opt
model <- prune(model, cp=cp.opt)

pred_values <- predict(model,test)
sqrt(mean((pred_values-test$GDP_growth_1)**2))
# RMSE of 4.102533

# This model performs better than the previous model with economic vars only, but the RMSE is still too high
# Will not be useful in the context of EIU

unique(pred_values)
# We can see that for the continuous CART models, the decision tree only predicts a single value
# The cross validation error continues to increase as the size of tree increases as well
# Hence continuous CART model might not be suited for this problem


############## Linear Regression Model  -  Using % change in economic and educational metrics to predict if GDP growth will be positive or negative ##############
m1 <- lm(GDP_growth_1 ~ ., data = all)
summary(m1)
RMSE <- sqrt(c(crossprod(m1$residuals)) / length(m1$residuals))
RMSE #3.519335

# The linear regression model performs better than the previous 2 continuous CART models
# However, the RMSE is still too high.
# Perhaps these models are unable to catch the complexity of the relationship between
# GDP and the predictor variables, and thus are unable to give an accurate prediction
# for GDP growth year on year.

# The models might be able to perform better if we tone down the complexity of prediction
# E.g. Predicting if GDP growth is positive or negative year on year, instead of predicting
# the actual value


###### END PART 12: MODELLING GDP GROWTH with EDUCATION+ECONOMIC % (CART/LINREG) ######


#######################################################################################
########## PART 13: MODELLING GDP Growth with EDUCATION+ECONOMIC % (Cat. CART) ########
###################### (supplementing Section 3.9 of the report) ######################
#######################################################################################
############## encoding a categorical variable ##############
dt <- fread("GDP_INPUT_ALL_P.csv")
# Setting a new categorical variable based on GDP growth - positive/negative
dt$GDP_growth_positive <- ifelse(dt$GDP_growth_1 > 0,1,0)
count(dt, "GDP_growth_positive")
# Removing the other y variables
dt <-dplyr::select(dt, -c('GDP_1','GDP_growth_1','GDP_capita_1','GDP_capita_growth_1'))


############## CART Model  -  Using % change in economic and educational metrics to predict if GDP growth will be positive or negative ##############
all_var <- dt[!is.na(dt$GDP_growth_positive),]
set.seed(2406)
model <- rpart(GDP_growth_positive ~ ., data = all_var, method = 'class', control = rpart.control(minsplit = 2, cp = 0))
printcp(model)
plotcp(model)
CVerror.cap <- model$cptable[which.min(model$cptable[,"xerror"]), "xerror"] + model$cptable[which.min(model$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (model$cptable[i,j] > CVerror.cap) {
    i <- i + 1
} 
cp.opt = ifelse(i > 1, sqrt(model$cptable[i,1] * model$cptable[i-1,1]), 1)
cp.opt
model <- prune(model, cp=cp.opt)

# Confusion Matrix
pred_values <- predict(model,all_var,type = "class")
table(pred_values, all_var$GDP_growth_positive)

# Sadly, using %change variables seemingly does not help to increase accuracy of predictions
# The model simply predicts all data points to be positive, which means that there might be an imbalance of majority and minority classes

count(dt, "GDP_growth_positive")
# There are around 4 times as many positive data points as there are negative, which might explain
# why the model as an inclination of predicting positive
# Thus, if we introduce synthetic data points to remove this imbalance, it might increase the accuracy of the model


##### END PART 13: MODELLING GDP Growth with EDUCATION+ECONOMIC % (Cat. CART)  ########

#                                        #~-~#                                        #

#######################################################################################
######### PART 14: MODELLING GDP Growth with ECONOMIC% & SMOTE (Cat. CART) ############
###################### (supplementing Section 3.10 of the report) #####################
#######################################################################################
# Train Test Split
set.seed(5)
econ <-dplyr::select(dt, c('GDP_growth_positive','Trade_change','Gross_capital_formation_change','Govt_expenditure_total_change','Life_expectancy_change'))
econ <- na.omit(econ)
train <- createDataPartition(econ$GDP_growth_positive,
                             p = 0.75, # % of data going to training
                             times = 1,
                             list = F)
train.orig <- econ[ train,]
test       <- econ[-train,]

############## introducing artificial data points in testset to remove imbalance in positive/negative GDP change ##############
library(smotefamily)
train.dt <- train.orig[, -c("GDP_growth_positive")]
colnames(train.dt)
train.smote <- SMOTE(train.dt, train.orig$GDP_growth_positive, K=5, dup_size = 4)
train.smote <- train.smote$data # extract only the balanced dataset
train.smote$class <- as.factor(train.smote$class)
prop.table(table(train.smote$class))

############## CART Model w/SMOTE  -  Using % change in economic metrics to predict if GDP growth will be positive or negative ##############
set.seed(5)
model <- rpart(class ~ ., data = train.smote, method = 'class', control = rpart.control(minsplit = 2, cp = 0))
printcp(model)
plotcp(model)
CVerror.cap <- model$cptable[which.min(model$cptable[,"xerror"]), "xerror"] + model$cptable[which.min(model$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (model$cptable[i,j] > CVerror.cap) {
    i <- i + 1
} 
cp.opt = ifelse(i > 1, sqrt(model$cptable[i,1] * model$cptable[i-1,1]), 1)
cp.opt
model <- prune(model, cp=cp.opt)

dt_smote_pred <- predict(model,test,type = "class")
table(dt_smote_pred, test$GDP_growth_positive)
# TN rate of about 33%, TP rate of about 70%
# This model yields a much better result as compared to previous models
# It is able to predict for some years whether the GDP growth will be positive or negative
# Let us try using economic + education variables in the next section to see if our model can be improved

###### END PART 14: MODELLING GDP Growth with ECONOMIC% & SMOTE (Cat. CART) ###########

#                                        #~-~#                                        #

#######################################################################################
##### PART 15: MODELLING GDP Growth with ECONOMIC+EDUCATION% & SMOTE (Cat. CART) ######
###################### (supplementing Section ??? of the report) ######################
#######################################################################################
# Train Test Split using all indicators
all <- na.omit(dt)
set.seed(5)
train <- createDataPartition(all$GDP_growth_positive,
                             p = 0.75, # % of data going to training
                             times = 1,
                             list = F)
train.orig <- all[ train,]
test       <- all[-train,]
count(test,"GDP_growth_positive")

############## introducing artificial data points in testset to remove imbalance in positive/negative GDP change ##############
library(smotefamily)
train.dt <- train.orig[, -c("GDP_growth_positive")]
colnames(train.dt)
train.smote <- SMOTE(train.dt, train.orig$GDP_growth_positive, K=5, dup_size = 16)
train.smote <- train.smote$data # extract only the balanced dataset
train.smote$class <- as.factor(train.smote$class)
prop.table(table(train.smote$class))

############## CART Model w/SMOTE  -  Using % change in economic and educational metrics to predict if GDP growth will be positive or negative ##############
set.seed(5)
model <- rpart(class ~ ., data = train.smote, method = 'class', control = rpart.control(minsplit = 2, cp = 0))
printcp(model)
plotcp(model)
CVerror.cap <- model$cptable[which.min(model$cptable[,"xerror"]), "xerror"] + model$cptable[which.min(model$cptable[,"xerror"]), "xstd"]
i <- 1; j<- 4
while (model$cptable[i,j] > CVerror.cap) {
    i <- i + 1
} 
cp.opt = ifelse(i > 1, sqrt(model$cptable[i,1] * model$cptable[i-1,1]), 1)
cp.opt
model <- prune(model, cp=cp.opt)

dt_smote_pred <- predict(model,test,type = "class")
table(dt_smote_pred, test$GDP_growth_positive)
# Can see that the predictions are improved tremendously as compared to 
# previous models whereby we did not re-balance the data

# TN rate is 48%, whereas TP rate is at 73%
# 45% increase in TN accuracy as compared to (12) which only uses economic predictors

# We are more concerned about predicting years in which the economy will have recession
# Since those are the years that policymakers and corporations will have to take special precautions
# Hence TN rate is more valuable to us in this context

round(100*model$variable.importance/sum(model$variable.importance))
# Can see that educational metrics are ranked highly in terms of variable importance
# Reaffirms the statistical significance that these factors have on the growth of GDP

#### END PART 15: MODELLING GDP Growth with ECONOMIC+EDUCATION% & SMOTE (Cat. CART) ###

#                                        #~-~#                                        #

#######################################################################################
##### PART 16: MODELLING GDP Growth with ECONOMIC+EDUCATION% & SMOTE (Log Reg) ########
###################### (supplementing Section ??? of the report) ######################
#######################################################################################

###///  GDP Categorical Model/GDP_Logreg.r  ///###

# Using the Train set data from (14)
set.seed(2406)          
m1 <- glm(class ~ ., family = "binomial", data = train.smote)
summary(m1)

# Confusion Matrix
prob <- predict(m1, test,type = 'response')
threshold <- 0.5
y.hat <- ifelse(prob > threshold, 1, 0)
table(test$GDP_growth_positive, y.hat, deparse.level = 2)
# Logistic regression model performs much worse than the CART model
# TN rate of 13% and TP rate of 95%
# As we explained in (14), we are more concerned about the TN rate
# Hence even though the Logistic Regression has a higher TP rate, we will still 
# choose the Categorical CART model


### END PART 16: MODELLING GDP Growth with ECONOMIC+EDUCATION% & SMOTE (Log Reg) ######

rm(list = ls())
# END ML