
# Setting up a working directory 
setwd("C:/Users/vuyyu/Desktop/JSOM/BA with R")
# Calling out to the R libraries
library(DBI)
library(lmtest)
library(data.table)
library(sandwich)
library(RSQLite)
library(ggplot2)
library(tidyverse)
library(tseries)
# Connecting to the wooldridge2.db file
connect <- dbConnect(RSQLite::SQLite(),'wooldridge2.db')
# Displays the list of columns in the tables of the database
dbListTables(connect)
dt <- dbReadTable(connect,'bwght')
dt <- data.table(dt)
dbDisconnect(connect) # Should run this at the end of the data analysis


# Pulls the datasets from the database tables
wpull <- function(tablename){
  connect <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge2.db')
  dt <- DBI::dbReadTable(connect,tablename)
  dt <- data.table(dt)
  print(DBI::dbReadTable(connect,paste0(tablename,'_labels')))
  DBI::dbDisconnect(connect)
  rm(connect)
  return(dt)
}
# Prints the datasets in the tables of database
wtables <- function(){
  connect <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge2.db')
  print(DBI::dbListTables(connect))
  DBI::dbDisconnect(connect)
  rm(connect)
}

# Problem 1
# a.)State the null hypothesis that, controlling for other factors, catchers and outfielders earn, on average, the same amount. Test this hypothesis using the data in mlb1 and comment on the size of the estimated salary differential.
# Pulling out mlb1 data from the database and assigning it to a variable
mlb1_dt <- wpull('mlb1')
mlb1_dt # Displays the m1b1 data

# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(log(salary) ~ years+gamesyr+bavg+hrunsyr+rbisyr+runsyr+fldperc+allstar+frstbase+scndbase+thrdbase+shrtstop+catcher, data = mlb1_dt)
mlb1_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.6535 and 339 dF
# We can state that the intercept in the equation is 11.129
# We can also state that coefficient of years is 0.058, gamesyr is 0.009, bavg is 0.0004,hrunsyr is 0.019,rbisyr is 0.0017,runsyr is 0.011, fldperc is 0.0002, allstar is 0.006, frstbase is -0.132, scndbase is  -0.161, thrdbase is 0.014, shrtstop is  -0.06 and catcher is 0.253
# Final equation is log(salary)=11.129+years*0.0584178+gamesyr*0.0097670+bavg*0.0004814+hrunsyr*0.0191459+rbisyr*0.0017875+runsyr*0.0118707+fldperc*0.0002833+allstar*0.0063351-frstbase*0.1328008-scndbase*0.1611010+thrdbase*0.0145271-shrtstop*0.0605672+catcher*0.2535592

# Null hypothesis (H0): the coefficients are equal to zero (i.e., no relationship between variables)
# If the coefficients are zero in the linear model, any change in catchers shall remain constant with respect to any change in outfielders
# Standard error for catcher is 0.1313 and t value is 1.931
# A small p-value (typically ??? 0.05) and t > 1.654 indicates strong evidence against the null hypothesis, so you reject it at 10% significance level.

# The estimated salary differential in catcher :
100*(exp(0.253)-1)
28.78833
# The difference  of 28.78% with respect to catcher is considerable

# b.) State and test the null hypothesis that there is no difference in average salary across positions, once other factors have been controlled for.
# Removing all the base variables from the regression equation of above model
model1 <- lm(log(salary) ~ years+gamesyr+bavg+hrunsyr+rbisyr+runsyr+fldperc+allstar, data = mlb1_dt)
mlb1_dt
summary(model1)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.6445 and 344 dF
# We can state that the intercept in the equation is 1.033e+01
# We can also state that coefficient of years is 6.293e-02, gamesyr is 9.186e-03, bavg is 3.868e-04,hrunsyr is 1.950e-02,rbisyr is 2.674e-03,runsyr is 9.625e-03, fldperc is 1.199e-03 and allstar is 6.902e-03
# Final equation is log(salary)=1.033e+01+years* 6.293e-02+gamesyr*9.186e-03+bavg*3.868e-04+hrunsyr*1.950e-02+rbisyr*2.674e-03+runsyr*9.625e-03+fldperc* 1.199e-03+allstar*6.902e-03

#Difference in average salaries of both models
anova(model1,model)

# Res Difference = 344, F statistic = 1.774 < 2.76, p value is 0.11 > 0.05.
# We can state that null hypothesis cannot be rejected and there is no difference in avergae salaries across positions

# c.) Are the results from parts 1 and 2 consistent? If not, explain what is happening.
# The models 1 and 2 are in consistent because we are testing less significant parameters in model1 as when compared to model.

# ____________________________________________________________________________________________________________________________________________________________________________________________________________________________________

# Problem 2
# a.) Consider the equation colgpa = 0 + 1hsize + 2hsize2 + 3hsperc + 4sat + 5female + 6athlete + u where colgpa is cumulative college grade point average, hsize is size of high school graduating class,in hundreds, hsperc is academic percentile in graduating class, sat is combined SAT score, female is
# a binary gender variable, and athlete is a binary variable, which is one for student-athletes. What are your expectations for the coefficients in this equation? Which ones are you unsure about?

# Pulling out gpa2 data from the database and assigning it to a variable
gpa2_dt <- wpull('gpa2')
gpa2_dt # Displays the gpa2 data

# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(colgpa ~ hsize+I(hsize^2)+hsperc+sat+female+athlete, data = gpa2_dt)
gpa2_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.2925 and 4130 dF
# We can state that the intercept in the equation is 1.241e+00
# We can also state that coefficient of hsize is -5.685e-02, I(hsize^2) is 4.675e-03, hsperc is -1.321e-02,sat is 1.646e-03,female is 1.549e-01 and athlete is 1.693e-01 
# Final equation is colgpa=1.241e+00-hsize*5.685e-02+I(hsize^2)*4.675e-03-hsperc*1.321e-02+sat*1.646e-03 +female*1.549e-01+athlete*1.693e-01

# By the above equation, we can assume that as students gpa increases, students in class decreases. Student gpa increases if sat score increase (it's coefficient being the highest)
# We cannot be sure about the change in I(hsize^2) and female.But, hsperc may be positive as the gpa increses with high percentile.
# Also, athelete should be negative as the people who play sports may have less gpa.

#b.) Estimate the equation in part 1 and report the results in the usual form. What is the estimated GPA differential between athletes and nonathletes? Is it statistically significant?
# Estimated GPA differential for athletes and non-athletes is 1.693e-01 (coefficient of athlete) and the t value is 3.998 which is significant.

# c.) Drop sat from the model and reestimate the equation. Now, what is the estimated effect of being an athlete? Discuss why the estimate is different than that obtained in part 2.
# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(colgpa ~ hsize+I(hsize^2)+hsperc+female+athlete, data = gpa2_dt)
gpa2_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.1885 and 4131 dF
# We can state that the intercept in the equation is 3.0476980
# We can also state that coefficient of hsize is -0.0534038, I(hsize^2) is 0.0053228, hsperc is -0.0171365,female is 0.0581231 and athlete is 0.0054487 
# Final equation is colgpa=3.0476980-hsize*-0.0534038+I(hsize^2)*-0.0534038-hsperc*0.0171365-02+female*0.0581231+athlete*0.0054487

# Estimated effect on the colgpa by being an athlete is 0.0054487 with t value 0.122
# Removing sat from the regression model, made athlete insignificant

# d.)In the model from part 1, allow the effect of being an athlete to differ by gender and test the null hypothesis that there is no ceteris paribus difference between women athletes and women nonathletes.
# Let us consider femaleathlete, maleathlete and the remaining as femalenonathlete and malenonathlete
femaleathlete <- ifelse((gpa2_dt$female==1)&(gpa2_dt$athlete==1),1,0)
femaleathlete
maleathlete <- ifelse((gpa2_dt$female==0)&(gpa2_dt$athlete==1),1,0)
maleathlete
malenonathlete <- 1 - maleathlete
malenonathlete
femalenonathlete <- 1 - femaleathlete
malenonathlete
# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(colgpa ~ hsize+I(hsize^2)+hsperc+sat+femaleathlete+maleathlete+malenonathlete, data = gpa2_dt)
gpa2_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.2803 and 4130 dF
# We can state that the intercept in the equation is 1.461e+00
# We can also state that coefficient of hsize is -6.039e-02 , I(hsize^2) is 5.283e-03, hsperc is -1.408e-02,sat is 1.522e-03, femaleathlete is 2.511e-01, maleathlete is 9.575e-02
# Final equation is colgpa=1.461e+00-hsize*6.039e-02 +I(hsize^2)*5.283e-03-hsperc*1.408e-02-02+sat*1.522e-03+femaleathlete*2.511e-01+maleathlete*9.575e-02

# Malenonathelete has no significance on the regression model of gpa
# Null hypothesis (H0): the coefficients are equal to zero (i.e., no relationship between variables)
# If the coefficients are zero in the linear model, any change in salary shall remain constant with respect to any change in rank.
# Coefficient of femaleathlete is 2.511e-01 and t value is 2.981
# A small p-value (typically ??? 0.05) and t > 1.96 indicates strong evidence against the null hypothesis, so you reject it.
# Being a female athlete and a non-athlete has some effect on gpa, ceteris paribus.

# e.) Does the effect of sat on colgpa differ by gender? Justify your answer.
# To predict the effect of sat on colgpa, we shall multiply sat with female as below
# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(colgpa ~ hsize+I(hsize^2)+hsperc+(sat*female)+athlete, data = gpa2_dt)
gpa2_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.2925 and 4129 dF
# We can state that the intercept in the equation is 1.264e+00
# We can also state that coefficient of hsize is -5.691e-02 , I(hsize^2) is 4.686e-03, hsperc is -1.323e-02,sat is 1.625e-03, female is 1.023e-01, athlete is  1.678e-01 
# Final equation is colgpa=1.264e+00-hsize*-5.691e-02 +I(hsize^2)*4.686e-03-hsperc*1.323e-02+sat*1.625e-03+female*1.023e-01+athlete* 1.678e-01

# The sat:female depicts the effect of sat on gpa depending on gender and coefficient is 5.121e-05 with t value 0.397
# There is not much effect of sat on colgpa by gender

#__________________________________________________________________________________________________________________________________________________________________________________________

#Problem 3
# a.) If there is discrimination against minorities, and the appropriate factors have been controlled for, what is the sign of 1?
# Pulling out loanapp data from the database and assigning it to a variable
loanapp_dt <- wpull('loanapp')
loanapp_dt # Displays the loanapp data

# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(approve ~ white, data = loanapp_dt)
loanapp_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.3201 and 1987 dF
# We can state that the intercept in the equation is 0.70779
# We can also state that coefficient of white is 0.20060 
# Final equation is approve=0.70779+00+white*0.20060

# The coeffcient of white is positive and approval will be positive though there are discriminations in minorities

# b.) Regress approve on white and report the results in the usual form. Interpret the coefficient on white. Is it statistically significant? Is it practically large?
# The coefficient of white is 0.20060 and t value is 10.11
# At 10% significance level, the whites have 20.06% higher rate of getting approved.

# c.) As controls, add the variables hrat, obrat, loanprc, unem, male, married, dep, sch, cosign, chist, pubrec, mortlat1, mortlat2, and vr. What happens to the coefficient on white? Is there still evidence of discrimination against nonwhites?
# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(approve ~ white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr, data = loanapp_dt)
loanapp_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.1656 and 1955 dF
# We can state that the intercept in the equation is 0.936731
# We can also state that coefficient of white is 0.128820,hrat is 0.001833,obrat is -0.005432,loanprc is -0.147300, unem is -0.007299, male is -0.004144, married is 0.045824, dep is -0.006827, sch is 0.001753, cosign is 0.009772, chist is 0.133027, pubrec is -0.241927, mortlat1 is -0.057251, mortlat2 is -0.113723 and vr is -0.031441    
# Final equation is approve=0.936731+00+white*0.128820+hrat*0.001833-obrat*0.005432-loanprc*0.147300-unem*0.007299-male*0.004144+married*0.045824-dep*0.006827+sch*0.001753+cosign*0.009772+chist*0.133027-pubrec*0.241927-mortlat1*0.057251-mortlat2*0.113723-vr*0.031441

# The coefficient of white is 0.128820 and t value is 6.529 > 1.96 and is thus significant statistically
# At 10% significance level, the whites still have 12.88% effect on rate of approval

# d.) Now, allow the effect of race to interact with the variable measuring other obligations as a percentage of income (obrat). Is the interaction term significant?
# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(approve ~ white+hrat+obrat+(white*obrat)+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr, data = loanapp_dt)
loanapp_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.1709 and 1954 dF
# We can state that the intercept in the equation is -0.145975
# We can also state that coefficient of white is 0.128820,hrat is 0.001790,obrat is -0.012226,loanprc is -0.147300, unem is -0.007299, male is -0.004144, married is 0.045824, dep is -0.006827, sch is 0.001753, cosign is 0.009772, chist is 0.133027, pubrec is -0.241927, mortlat1 is -0.057251, mortlat2 is -0.113723 and vr is -0.031441    
# Final equation is approve=1.180648-white*0.145975+hrat*0.001790-obrat*0.005432-loanprc*0.147300-unem*0.007299-male*0.004144+married*0.045824-dep*0.006827+sch*0.001753+cosign*0.009772+chist*0.133027-pubrec*0.241927-mortlat1*0.057251-mortlat2*0.113723-vr*0.031441

# e.) Using the model from part 4, what is the effect of being white on the probability of approval when obrat = 32, which is roughly the mean value in the sample? Obtain a 95% confidence interval for this effect.
obratmean <- loanapp_dt$obrat == 32
obratmean
model <- lm(approve ~ white+hrat+obrat+(white*obratmean)+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr, data = loanapp_dt)
summary(model)
confint(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.1709 and 1954 dF
# We can state that the intercept in the equation is 0.927600
# We can also state that coefficient of white is 0.135474,hrat is 0.001844,obrat is -0.005400,loanprc is -0.147805, unem is -0.003663, male is -0.003663, married is 0.045228, dep is -0.007150, sch is 0.002594, cosign is 0.011634, chist is 0.133671, pubrec is -0.240211, mortlat1 is -0.056040, mortlat2 is -0.112674 and vr is -0.032138    
# Final equation is approve=0.927600+white*0.135474+hrat*0.001844-obrat*0.005400-loanprc*0.147805-unem*0.007128-male*0.003663+married*0.045228-dep*0.007150+sch*0.002594+cosign*0.011634+chist*0.133671-pubrec*0.240211-mortlat1*0.056040-mortlat2*0.112674-vr*0.032138

# The coefficient of white is 0.135474 (has positive effect on approve unlike above model which has negative effect on approve) and t value is 6.714 > 1.96. 
# So white is significant and 95% confidence interval = (0.0732,0.1524)

#_________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________

# Problem 4
# a.)Use the data in hprice1 to obtain the heteroskedasticity-robust standard errors for equation: price = 0 + 1lotsize + 2sqrft + 3bdrms + u. Discuss any important differences with the usual standard errors. 
# Pulling out hprice1 data from the database and assigning it to a variable
hprice1_dt <- wpull('hprice1')
hprice1_dt # Displays the hprice1 data

# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(price ~ lotsize+sqrft+bdrms, data = hprice1_dt)
hprice1_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.6223 and 84 dF
# We can state that the intercept in the equation is -2.177e+01
# We can also state that coefficient of lotsize is 2.068e-03, sqrft is  1.228e-01 and bdrms is 1.385e+01
# Final equation is price=-2.177e+01+lotsize*2.068e-03+ 1.228e-01*sqrft+1.385e+01*bdrms

# Heteroskedasticity-robust standard errors which determines inefficiency of regression models
coeftest(model, vcov. = vcovHAC)

# We observe that robust standard error of lotsize is almost twice that of standard error
# t value of lotsize decreases from 3.220 to 1.6289 and thus becomes insignificant
# t value of sqrft decreases from 9.275 to 6.8405 and thus becomes insignificant though coefficients are almost the same
# bdrms has almosst same standard error and coefficient

# b.) Repeat part 1 for equation ln(price) = 0 + 1 ln(lotsize) + 2 ln(sqrft) + 3bdrms + u
# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(log(price) ~ log(lotsize)+log(sqrft)+bdrms, data = hprice1_dt)
hprice1_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.643 and 84 dF
# We can state that the intercept in the equation is -1.29704 
# We can also state that coefficient of lotsize is 0.16797, sqrft is  0.70023 and bdrms is 0.03696
# Final equation is price=-1.29704 +log(lotsize)*0.16797+0.70023*log(sqrft)+0.03696*bdrms

# Heteroskedasticity-robust standard errors which determines inefficiency of regression models
coeftest(model, vcov. = vcovHAC)

# We observe that robust standard errors has increased for all variables
# t value of log(lotsize) and log(sqrft) are very high and thus significant 
# t values of bdrms are 1.342 and 1.1712 which are both < 1.96 and thus insignificant

# c.) What does this example suggest about heteroskedasticity and the log transformation
# Heteroskedasticity-robust standard errors determines inefficiency of regression models and it reduced in case of log model than the before model

#___________________________________________________________________________________________________________________________________________________________________________

# Problem 5
# a.) Use OLS to estimate a model relating colGPA to hsGPA, ACT, skipped, and PC. Obtain the OLS residuals and fitted values.
# Pulling out gpa1 data from the database and assigning it to a variable
gpa1_dt <- wpull('gpa1')
gpa1_dt # Displays the gpa1 data

# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(colGPA ~ hsGPA+ACT+skipped+PC, data = gpa1_dt)
gpa1_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.2593 and 136 dF
# We can state that the intercept in the equation is 1.35651        
# We can also state that coefficient of hsGPA is 0.41295, ACT is 0.01334,skipped is -0.07103 and PC is 0.12444 
# Final equation is colGPA=1.35651+hsGPA*0.41295+0.01334*ACT-0.07103*skipped+PC*0.12444

# Represents the residual values in the regression model
resmodel <- residuals(model)
resmodel
# Represents the squares of residual values in regression model
resmodel1 <- (resmodel)^2
resmodel1

# b.) In the regression of ^u2i on co\lGPA, co\lGPA 2, obtain the fitted values, say^hi.
# Represents the conditional if loop in creating the regression model
collegeGPA <- gpa1_dt$colGPA
for(i in 1:141){collegeGPA[i] <- 1.35651+0.41295*(gpa1_dt$hsGPA[i])+(gpa1_dt$ACT[i])*0.01334-0.07103*(gpa1_dt$skipped[i])+(gpa1_dt$PC[i])*0.12444}
collegeGPA
# Represents the squares of collegeGPA
collegeGPA1 <- collegeGPA^2
collegeGPA1
# R squared value is 0.04934 and 138 dF
model1 <- lm(resmodel1 ~ collegeGPA+collegeGPA1)
summary(model1)
# Predicting the fitted values for gpa
fittedgpa <- fitted(model1)
fittedgpa
min(fittedgpa) # The minimum value of fittedgpa is 0.02738078
max(fittedgpa) # The maximum value of fittedgpa is 0.1648041

# c.) Verify that the fitted values from part 2 are all strictly positive. Then, obtain the weighted least squares estimates using weights 1/^hi. Compare the weighted least squares estimates for the effect of skipping lectures and the effect of PC ownership with the corresponding OLS estimates. What about their statistical significance?

predictmodel <- predict(model1)
a <- predictmodel

fittedgpa <- (1/fittedgpa)
fittedgpa

model2 <- lm(gpa1_dt$colGPA ~ gpa1_dt$hsGPA + gpa1_dt$ACT + gpa1_dt$skipped + gpa1_dt$PC, weights = 1/fittedgpa)
model2
summary(model2)

# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.3062 and 136 dF
# We can state that the intercept in the equation is 1.401557       
# We can also state that coefficient of hsGPA is 0.402506, ACT is 0.013162,skipped is -0.076365 and PC is 0.126003
# Final equation is colGPA=1.401557+gpa_dt$hsGPA*0.402506+0.013162*gpa_dt$ACT-0.076365*gpa_dt$skipped+gpa_dt$PC*0.126003
# t values of hsGPA=4.828, ACT=0.009827, skipped=-0.076365 and PC=0.126003 are statistically significant

# d.) In the WLS estimation from part 3, obtain heteroskedasticity-robust standard errors. In other words, allow for the fact that the variance function estimated in part 2 might be misspecified. Do the standard errors change much from part 3?
coeftest(model2, vcov. = vcovHAC)
# We can say that new standard errors are almost equal to the earlier standard errors
# PC standard error has increased significantly compared to normal standard error

# ________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________

# Problem 6
# a.) Go online and search for daily bitcoin prices. Find and download a historical series of bitcoin prices going back to at least 2014.
# b.) Go to St Louis FRED. Find and download the S&P500 (SP500), the London bullion market price for gold in US dollars (GOLDAMGBD228NLBM), the US/Euro exchange rate (DEXUSEU), and the West Texas Intermediate spot price of oil (DCOILWTICO). These should all be available daily as well.
# c.) Merge all the data sets together (you can use either R or Excel or whatever).
# d.) Plot the series in R.

# Merging the data sets downloaded into a single csv file merged and plotting in R 
mergeddata <- read.csv('merged.csv')
mergeddata$ï..DATE <- as.Date(mergeddata$ï..DATE) # Formatted the date from factor to date
class(mergeddata$ï..DATE)
plot(mergeddata)
plot_mergeddata <- plot.ts(cbind(mergeddata$Bitcoin,mergeddata$SP500,mergeddata$GOLDAMGBD228NLBM,mergeddata$DEXUSEU,mergeddata$DCOILWTICO), plot.type = "single", col=c("red","green","blue","orange","purple"))

# Building a regression model and determing coefficient of regression
mergedmodel <- lm(Bitcoin~SP500+GOLDAMGBD228NLBM+DEXUSEU+DCOILWTICO, data = mergeddata)
mergedmodel
summary(mergedmodel)
coef(mergedmodel)

# f.) Use the KPSS test to find how many differences each series takes to become stationary

# KPSS test and differences for SP500
kpss.test(mergeddata$SP500, null="Level")
# KPSS Level = 15.601, p-value = 0.01
kpss.test(mergeddata$SP500, null="Trend")
# KPSS Trend = 0.61667, p-value = 0.01 and it is non-stationary
kpss.test(diff(mergeddata$SP500), null="Level")
# KPSS Level = 0.0056266, p-value = 0.1
kpss.test(diff(mergeddata$SP500), null="Trend")
# KPSS Trend = 0.0043599, p-value = 0.1 and it is stationary 

# KPSS test and differences for GOLDAMGBD228NLBM
kpss.test(mergeddata$GOLDAMGBD228NLBM, null="Level")
# KPSS Level = 8.0996, p-value = 0.01 and it is non-stationary
kpss.test(mergeddata$GOLDAMGBD228NLBM, null="Trend")
# KPSS Trend = 0.44595, p-value = 0.01
kpss.test(diff(mergeddata$GOLDAMGBD228NLBM), null="Level")
# KPSS Level = 0.0156, p-value = 0.1 and it is stationary
kpss.test(diff(mergeddata$GOLDAMGBD228NLBM), null="Trend")
# KPSS Trend = 0.01033, p-value = 0.1

# KPSS test and differences for DCOILWTICO
kpss.test(mergeddata$DCOILWTICO, null="Level")
# KPSS Level = 6.7064, p-value = 0.01 and it is non-stationary
kpss.test(mergeddata$DCOILWTICO, null="Trend")
# KPSS Trend = 1.0621, p-value = 0.01
kpss.test(diff(mergeddata$DCOILWTICO), null="Level")
# KPSS Level = 0.034792, p-value = 0.1 and it is stationary
kpss.test(diff(mergeddata$DCOILWTICO), null="Trend")
# KPSS Trend = 0.024815, p-value = 0.1

# KPSS test and differences for DEXUSEU
kpss.test(mergeddata$DEXUSEU, null="Level")
# KPSS Level = 2.2478, p-value = 0.01 and it is non-stationary
kpss.test(mergeddata$DEXUSEU, null="Trend")
# KPSS Trend = 1.3257, p-value = 0.01
kpss.test(diff(mergeddata$DEXUSEU), null="Level")
# KPSS Level = 0.026589, p-value = 0.1 and it is stationary
kpss.test(diff(mergeddata$DEXUSEU), null="Trend")
# KPSS Trend = 0.025769, p-value = 0.1

# From above KPSS results, every series became stationary after the level difference

# e.) Use a naïve regression to find spurious correlations to the bitcoin price in the data set (e.g., regress the bitcoin price on the other series without any differencing to see if you find any interesting but total bullshit relationships).
# The final regression equation is Bitcoin = 5.91e+8.82e(SP500)-1.11e(GOLDAMGBD228NLBM)-1.86(DEXUSEU)-4.62e(DCOILWTICO)

# g.) After taking differences, regress the bitcoin price on the other series. What relationships do you find now?
# Building regression model after determining differences to state a relationship afetr becoming stationary
mergedmodel1 <- lm(diff(Bitcoin)~diff(SP500)+diff(GOLDAMGBD228NLBM)+diff(DEXUSEU)+diff(DCOILWTICO), data = mergeddata)
mergedmodel1
summary(mergedmodel1)
coef(mergedmodel1)

# h.) Remove all the data before 2017 where the bitcoin price starts to spike. Plot the new data. This is the data you are to use for the rest of the question.

#Removed all data before 2017, created another subset and plotted in R
newmergeddata <- subset(mergeddata, mergeddata$ï..DATE> "2016-12-31")
view(newmergeddata)
plot(newmergeddata)
plot_newmergeddata <- plot.ts(cbind(newmergeddata$Bitcoin,newmergeddata$SP500,newmergeddata$GOLDAMGBD228NLBM,newmergeddata$DEXUSEU,newmergeddata$DCOILWTICO), plot.type = "single", col=c("red","green","blue","orange","purple"))

# i.) Plot the ACF and PACF of the bitcoin price.
# ACF of bitcoin price
acf(newmergeddata$Bitcoin)
acf(diff(newmergeddata))
pacf(newmergeddata$Bitcoin)
pacf(newmergeddata)

kpss.test(diff(newmergeddata$Bitcoin),null='Level')
# KPSS Level = 0.042309, p-value = 0.1 and it is stationary

# j.) Fit various arima models to the bitcoin price. Which model fits best using the AIC?
model <- arima(newmergeddata$Bitcoin)
steps <- 30
future <- forecast(model,h=steps) 
View(future)
plot(future)
plot(future,xlim=c(400-steps,400+steps),ylim=c(2000,15000))

# k.) Forecast the next 30 days of the bitcoin price and plot the forecast.
#Plotting the periodogram of Bitcoin price
periodogram(newmergeddata$Bitcoin)
#Plotting the periodogram of diff. in Bitcoin price
periodogram(newmergeddata_diff)

# l.) Plot the periodogram of the data. Do you see any seasonality in the data?
Yes we do see a seasonality in the periodogram after plotting.
