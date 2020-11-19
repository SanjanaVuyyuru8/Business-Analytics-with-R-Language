
# Setting up a working directory 
setwd("C:/Users/vuyyu/Desktop/JSOM/BA with R")
# Calling out to the R libraries
library(dplyr)
library(broom)
library(forecast)
library(tidyverse)
library(lmtest)
library(Greg)
library(margins)
library(plm)
library(DBI)
library(data.table)
library(sandwich)
library(RSQLite)
library(ggplot2)
library(tseries)
library(tidyr)
library(tidyverse)

tidy.g <- function(model,vc=vcov(model),conf.int=FALSE,conf.level=0.95){
  dt <- tidy(model,conf.int=conf.int,conf.level=conf.level)
  dt$std.error <- sqrt(diag(vc))
  dt$statistic <- dt$estimate/dt$std.error
  dt$p.value <- 2*pnorm(-abs(dt$statistic))
  if(conf.int){
    dt$conf.low <- dt$estimate+qnorm((1-conf.level)/2)*dt$std.error
    dt$conf.high <- dt$estimate-qnorm((1-conf.level)/2)*dt$std.error
  }
  return(dt)
}
tidy.w <- function(model,...)tidy.g(model,vc=sandwich::vcovHC(model),...)

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
# a.)Using the hprice1 table, find the best model for the housing price that you can using the AIC and BIC.
hprice1_dt <- wpull('hprice1')
hprice1_dt # Displays the hprice1 data

# Stating a simple regression equation to obtain coefficients of linear model
model_1=lm(price~bdrms+lotsize+sqrft+colonial+I(bdrms^2)+I(lotsize^2)+I(sqrft^2),hprice1_dt)
model_2 <- step(model_1)  ## Step-wise to obtain AIC value
#AIC best model displays the AIC value as 696.68

summary(model_2)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.7705 and 82 dF
# We can state that the intercept in the equation is 8.146e+01 
#Thus the regression equation is price ~ lotsize + I(sqrft^2) + colonial + I(bdrms^2) + I(lotsize^2)

model_3 <- step(model_1,k=log(nrow(hprice1_dt)),direction="forward")  ## BIC forward step-wise 
summary(model_3)

#In the BIC Step, the AIC value is 720.5
#price ~ lotsize + colonial + I(lotsize^2) + I(sqrft^2)

#---------------------------------------------------------------------------------------------------------------------

# Problem 2
# a.) Using the gpa2 table, find the best model for the college GPA that you can using the AIC and BIC.
gpa2_dt <- wpull('gpa2')
gpa2_dt # Displays the hprice1 data

# Stating a simple regression equation to obtain coefficients of linear model
model_1=lm(colgpa~sat+tothrs+athlete+verbmath+hsize+hsrank+hsperc+female+black+white,gpa2_dt)
model_2 <- step(model_1)  ## Step-wise to obtain AIC value
#AIC best model displays the AIC value as 5005.39,

summary(model_2)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.315 and 4129 dF
# We can state that the intercept in the equation is 1.2072730
#colgpa ~ sat + tothrs + athlete + hsrank + hsperc + female + black

model_3 <- step(model_1,k=log(nrow(gpa2_dt))) #BIC step wise

#BIC = 4954.77,
#colgpa ~ sat + tothrs + athlete + hsrank + hsperc + female +  black

#---------------------------------------------------------------------------------------------------------------------------

# Problem 3
# a.) Using the mlb1 table, find the best model that you can for salary using the AIC and BIC.
mlb1_dt <- wpull('mlb1')
mlb1_dt # Displays the mlb1 data

# Stating a simple regression equation to obtain coefficients of linear model
model_1=lm(log(salary)~.,mlb1_dt)
model_2 <- step(model_1)  ## Step-wise to obtain AIC value
#AIC best model displays the AIC value as 251.14

summary(model_2)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.6779 and 318 dF
# We can state that the intercept in the equation is 10.9689774
#salary ~ index + teamsal + years + runs + so + yrsallst + hrunsyr + allstar + rbisyr + hispph

model_3 <- step(model_1,k=log(nrow(mlb1_dt))) #BIC step wise

#BIC 211.27
#log(salary) ~ years + games + runs + scndbase + yrsallst + gamesyr + allstar + sbasesyr + blckpb

#-------------------------------------------------------------------------------------------------------------------------------------

# Problem 4
# a.)Estimate the equation by pooled OLS and report the results in standard form. What do you make of the estimate on the 1990 dummy variable? What do you get for ^ 3?
rental_dt=wpull('rental')
rental_dt # Displays the rental data

pdrental<-pdata.frame(rental_dt,c('city','year'))
pdrental$pctstu <- pdrental$enroll/pdrental$pop*100

# Stating a regression equation to obtain coefficients of linear model 
model_1=plm(log(rent)~as.factor(year)+log(pop)+log(avginc)+pctstu,data=pdrental,model ="pool")
summary(model_1)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.86128 and 123 dF
# We can state that the intercept in the equation is -0.5688064 
# The final equation is log(rent)=-0.5688 +0.2622*y90+0.0406*log(pop)+0.5714*log(avginc)+0.0050*pctstu
#y90 and pctstu are significant at 0.01% level.
#The rent in 1990 is 26% higher than the rent in 1980.
#Rents are 0.5% higher for every percent increase in the student population.

# b.) Are the standard errors you report in part 1 valid? Explain.
# No, the they are not valid.
coeftest(model_1)

# c.) Now, difference the equation and estimate by OLS. Compare your estimate of 3 with that from part 1. Does the relative size of the student population appear to affect rental prices?
model_1=plm(diff(log(rent))~diff(log(pop))+diff(log(avginc))+diff(pctstu),model ="pooling",data=pdrental)
summary(model_1)

#standard errors are valid after first differencing.
# Yes. The relative size of the student population appear to affect rental prices.

# d.) Estimate the model by fixed effects to verify that you get identical estimates and standard errors to those in part 3.
model_1=plm(log(rent)~as.factor(year)+log(pop)+log(avginc)+pctstu,data=pdrental,model ="within")
summary(model_1)

#The pctstu has increased twice in fixed effect method though the coeffients of pctstu in both fixed effects and first diff are the same.
#The student population increased by 1% and 1.1% increase in rental rates.

#----------------------------------------------------------------------------------------------------------------------------------------------------

# Problem 5
# a.)Consider the unobserved effects model mrdrteit = i + t + 1execit + 2unemit + eit where t simply denotes different year intercepts and i is the unobserved state effect. If past executions of convicted murderers have a deterrent effect, what should be the sign of 1? What sign do you think 2 should have? Explain.
murder_dt=wpull("murder")
murder_dt # Displays murder table data

murder =pdata.frame(murder_dt,index = c("id","year"))
head(murder)

#If deterrent effect is present, murder rate would go down. b1 should be negative.
#coefficient of unemployment rate b2 may be positive but not sure.

# b.) Using just the years 1990 and 1993, estimate the equation from part 1 by pooled OLS. Ignore the serial correlation problem in the composite errors. Do you find any evidence for a deterrent effect?
murder_1=murder_dt%>%filter(year != 87)
model_1= plm(mrdrte ~ as.factor(year)+exec+unem, data=murder_1,model="pool")
summary(model_1)

#The coefficient of exec is positive but there is no deterrent effect.

# c.) Now, using 1990 and 1993, estimate the equation by fixed effects. You may use first differencing since you are only using two years of data. Is there evidence of a deterrent effect? How strong?
model1 <- plm(mrdrte~exec+unem,data=murder %>% subset(year==90 | year==93),model='fd')
summary(model1)

#The coefficient of exec is negative in both first differencing and fixed effects methods, so there is a deterrent effect.

# d.) Compute the heteroskedasticity-robust standard error for the estimation in part 2.
tidy.w(model_1)

# The heteroskedasticity-robust standard error for the estimation in model_1 states that coefficient of exec is positive and there is no deterrent effect.

# e.) Find the state that has the largest number for the execution variable in 1993. (The variable exec is total executions in 1991, 1992, and 1993.) How much bigger is this value than the next highest value? 
murder_93 = murder%>%filter(as.factor(year)==93)
head(murder_93[order(-murder_93$exec),])

#  Texas had 34. Virginia only had 11. Texas displays a larger effect as per the results.

# f.) Estimate the equation using first differencing, dropping Texas from the analysis. Compute the usual and heteroskedasticity-robust standard errors. Now, what do you find? What is going on?
murder_tx <- plm(mrdrte~exec+unem,data=murder %>%  subset(state!="TX"),model='within',effect='twoway')
summary(murder_tx)

#There is a decrease in defereent effect from 0.12 (b) to -0.0674 (f). 
#When TX is not considered, the varaition in exec decreases.

# g.) Use all three years of data and estimate the model by fixed effects. Include Texas in the analysis. Discuss the size and statistical significance of the deterrent effect compared with only using 1990 and 1993.
model_1 <- plm(mrdrte~exec+unem,data=murder,model='within',effect='twoway')
summary(model_1)

# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.010883 and 98 dF
#The final equation is mrdrte = 1.556(d90)+1.73(d93)- 0.138(exec)+ 0.22132(unem). 
#By adding the year '87' the standard error of 'exec' has increased and is not significant at 5% level.

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Problem 6
# a.) Estimate the above equation by pooled OLS with the year dummies included. If bmktshr = 0.10, what is the estimated percentage increase in fare?
airfare_dt=wpull("AIRFARE")
airfare_dt # Displays the airfare table
airfare = pdata.frame(airfare_dt,index=c('id','year'))
airfare$concen=airfare$bmktshr

# Stating a regression equation to obtain coefficients of linear model 
model_1=plm(log(fare) ~ as.factor(year)+concen+log(dist)+I(log(dist)^2), data = airfare, model = "pooling")
summary(model_1)

#The coefficient of concen is 0.3601. If change(concen) = 0.1, then change[log(fare)] = 0.03601 
#The air fare is estimated to increase by 3.601% with other factors fixed 

# b.) What is the usual OLS 95% confidence interval for 1? Why is it probably not reliable? If you have access to a statistical package that computes fully robust standard errors, find the fully robust 95% CI for 1. Compare it to the usual CI and comment.
confint(model_1)

#The concen is [0.301186070  0.41905465]

tidy.w(model_1)
confint_robust(model_1)

#The robust CI has the concen [0.24526258  0.47497813]

# c.)Describe what is happening with the quadratic in ln(dist). In particular, for what value of dist does the relationship between ln(fare) and dist become positive? [Hint: First figure out the turning point value for ln(dist), and then exponentiate.] Is the turning point outside the range of the data?
model1 <- plm(log(fare)~concen+log(dist)+I(log(dist)^2),model='within',effect='twoways',data=airfare)
summary(model1)

# At first differential log(dist) = 4.379 = 4.38 which is the turning point for the quadratic equation
dist = exp(4.38)
dist  # 79.83 miles
min(airfare$dist)

# The dist of 95 miles is out of the range of sample

# d.) Now estimate the equation using fixed effects. What is the FE estimate of 1?
model_1=plm(log(fare) ~ as.factor(year)+concen+log(dist)+I(log(dist)^2), data = airfare, model = "within")
summary(model_1)

#The Fixed Effect estimate of b1 is 0.1688, which is less than the estimate of pooled OLS and 'concen' is significant.

# e.) Name two characteristics of a route (other than distance between stops) that are captured by i. Might these be correlated with bmktshrit?
# Population in the cities and education levels

# f.) Are you convinced that higher concentration on a route increases airfares? What is your best estimate?
# Yes. The pooled model is probably the best.

#-------------------------------------------------------------------------------------------------------------------------------------------------------------

# Problem 7
# a.)Estimate a logit model of approve on white. Find the estimated probability of loan approval for both whites and nonwhites. How do these compare with the linear probability estimates?
loanapp_dt=wpull("loanapp")
loanapp_dt # Displays the loanapp table

# Stating a GL regression equation to obtain coefficients of linear model 
glm(approve~white,family=binomial(),data=loanapp_dt) %>% summary
glm(approve~white,family=binomial(),data=loanapp_dt) %>% predict(data.table(white=c(1,0)),type='response')
lm(approve~white,data=loanapp_dt) %>% predict(data.table(white=c(1,0)))

# Probability of loan approval being non-white = 70.78% and probability of loan approval being white is (70.78 + 20.06) = 90.78%
# The GL regression and linear regression look the same.

# b.) Now, add the variables hrat, obrat, loanprc, unem, male, married, dep, sch, cosign, chist, pubrec, mortlat1, mortlat2, and vr to the logit model. Is there statistically significant evidence of discrimination against nonwhites?
# Stating a GL regression equation to obtain coefficients of linear model 
model_1=glm(approve ~ white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr, 
             family = binomial(link = "logit"),data=loanapp_dt)
summary(model_1)

#There is a statistically significant evidence of discrimination against nonwhites.

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Problem 8
# a.)What fraction of the sample is employed at the time of the interview? What fraction of the sample has abused alcohol?
alcohol_dt=wpull("alcohol")
alcohol_dt # Displays the alcohol table

nrow(alcohol_dt%>%filter(employ==1))/nrow(alcohol) 
# 0.8981 of sample is employed at the time of the interview
nrow(alcohol_dt%>%filter(abuse==1))/nrow(alcohol) 
#0.09916 of the sample has abused alcohol

# b.) Run the simple regression of employ on abuse and report the results in the usual form, obtaining the heteroskedasticity-robust standard errors. Interpret the estimated equation. Is the relationship as you expected? Is it statistically significant?
# Stating a regression equation to obtain coefficients of linear model 
model_1=lm(employ ~ abuse, data = alcohol_dt) 
summary(model_1)

# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.3023and 9820 dF
# We can state that the intercept in the equation is 0.9009
#The final equation is employ=0.9009-0.0283*abuse

margins(model_1)

#It is estimated that employment rate is 0.028 times
# Yes. The relationship is as expected.

tidy.w(model_1)

#For the robust standard errors the estimates remains almost same.
#The sign of abuse remains negative in OLS and robust standard error estimations and is statistically significant at 10%

# c.) Run a glm-logit of employ on abuse. Do you get the same sign and statistical significance as in part 2? How does the average marginal effect for the logit compare with that for the linear probability model?
# Stating a GL regression equation to obtain coefficients of linear model
model_1=glm(employ ~ abuse, data = alcohol_dt,family = binomial(link = "logit"))
summary(model_1)
margins(model_1)

#The sign of abuse is same as the linear model. But the effect is 10 times more compared to the linear regression.

# d.) Obtain the fitted values for the LPM estimated in part 2 and report what they are when abuse = 0 and when abuse = 1. How do these compare to the logit fitted values, and why?
lm(employ ~ abuse, data = alcohol_dt) %>% predict(data.table(abuse=c(0,1)))

glm(employ~abuse,data=alcohol_dt,family=binomial(link="logit")) %>% predict(data.table(abuse=c(0,1)),type="response")

#Predictions are exactly same for both models

# e.) To the LPM in part 2 add the variables age, age2, educ, educ2, married, famsize, white, northeast, midwest, south, centcity, outercity, qrt1, qrt2, and qrt3. What happens to the coefficient on abuse and its statistical significance?
# Stating a regression equation to obtain coefficients of linear model
model_1=lm(employ ~ abuse+age+I(age^2)+educ+I(educ^2)+married+famsize+white+northeast+midwest+south+centcity+outercity+qrt1+qrt2+qrt3, data = alcohol_dt,family = binomial(link = "logit"))
summary(model_1)

# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.06948 and 9805 dF
# We can state that the intercept in the equation is 1.793e-01

tidy.w(model_1)
margins(model_1)

#The marginal effect of abuse is -0.02025 and abuse is now statistically significant at 5% level

# f.) Estimate a glm-logit model using the variables in part 5. Find the marginal effect of abuse and its t-statistic. Is the estimated effect now identical to that for the linear model? Is it "close"?
# Stating a regression equation to obtain coefficients of linear model
model_1=glm(employ ~ abuse+age+I(age^2)+educ+I(educ^2)+married+famsize+white+northeast+midwest+south+centcity+outercity+qrt1+qrt2+qrt3, data = alcohol_dt,family = binomial(link = "logit"))
summary(model_1)

# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# We can state that the intercept in the equation is -3.1140166
# Null deviance: 6463.8  on 9821  degrees of freedom, Residual deviance: 5865.9  on 9805  degrees of freedom and AIC: 5899.9

tidy.w(model_1)
margins(model_1)

#The marginal effect of abuse = -0.01938
#Yes. The marginal effects of both LM and GLM models are the same and abuse is now statistically significant.

# g.) Variables indicating the overall health of each man are also included in the data set. Is it obvious that such variables should be included as controls? Explain.
glm(employ ~ abuse + exhealth+vghealth+goodhealth+fairhealth, data =alcohol_dt,family = binomial())
lm(employ ~ abuse + exhealth+vghealth+goodhealth+fairhealth, data =alcohol_dt)%>%summary()

#Using lm model, the overall health variable looks significant, unlike using glm model.
# It cannot be that a heavy drinker bears overall good health variable.
# Though the overall health variable is correlated with the other independent variables.

# h.) Why might abuse be properly thought of as endogenous in the employ equation? Do you think the variables mothalc and fathalc, indicating whether a man's mother or father were alcoholics, are sensible instrumental variables for abuse?
# Stating a glm and lm regression equations to obtain coefficients of linear model
glm(employ ~ abuse +mothalc+ fathalc, data = alcohol_dt, family = binomial())
lm(employ ~ abuse +mothalc+ fathalc, data = alcohol_dt)%>%summary()

cor(alcohol_dt$abuse,alcohol_dt$mothalc)
cor(alcohol_dt$abuse,alcohol_dt$fathalc)

# mothalc, and fathalc may not be sensible instrumental variables of abuse.

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Problem 9
# a.) Estimate a Poisson regression model for kids, using educ, age, age2, black, east, northcen, west, farm, othrural, town, smcity, y74 , y76 , y78 , y80 , y82 , and y84 . Interpret the coefficient on y82 .
fert_dt=wpull("fertil1")
fert_dt # Displays the fertil1 table
fert=fert%>%pdata.frame(index=c('index','year'))

# Stating a regression equation to obtain coefficients of linear model
model_1=glm(kids ~ educ+age+I(age^2)+black+east+northcen+west+farm+othrural+
               town+smcity+as.factor(year),data = fert_dt,family = poisson)
summary(model_1)

# Women in Botswanahad on average 19% fewer living children in 1982 than a woman in 1972

# b.) What is the estimated percentage difference in fertility between a black woman and a nonblack woman, holding other factors fixed?
# The black women had on average 36% more living children than a non black woman

# c.) Compute the fitted values from the Poisson regression and obtain the R-squared as the squared correlation between kidsi and dkidsi.. Compare this with the R-squared for the linear regression model.
cor(fert_dt$kids,predict(model_1,fert_dt))^2
#The coefficient of correlation is 0.1289

# Stating a regression equation to obtain coefficients of linear model
model_1=lm(kids ~ educ+age+I(age^2)+black+east+northcen+west+farm+othrural+town+smcity+as.factor(year),data = fert_dt)
summary(model_1)

# R-squared: 0.1295
# This is fairly standard and indicates that the poisson is a good fit.

#The LM R-squared value is slightly more than the GLM R-squared value. 
#Hence, LM model is slightly better than GLM model.
