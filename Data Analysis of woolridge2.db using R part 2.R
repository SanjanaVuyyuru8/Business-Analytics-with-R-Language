
# Setting up a working directory 
setwd("C:/Users/vuyyu/Desktop/JSOM/BA with R")
# Calling out to the R libraries
library(DBI)
library(data.table)
# library(RSQLite)
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
# a.) What is the interpretation of B1.
# Pulling out vote1 data from the database and assigning it to a variable
vote1_dt <- wpull('vote1')
vote1_dt # Displays the vote1 data

# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(voteA ~ log(expendA)+log(expendB)+prtystrA, data = vote1_dt)
vote1_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.7925 and 169 dF
# We can state that the intercept in the equation is 45.08
# We can also state that coefficient of log(expendA) is 6.08, coefficient of log(expendB) is -6.61 and coefficient of prtystrA is 0.15
# Final equation is price=45.08+6.08*log(expendA)-6.61*log(expendB)+0.15*prtystrA

# Interpretation of B1 is 6.08

# b.) In terms of the parameters, state the null hypothesis that a 1% increase in A's expenditures is offset by
# a 1% increase in B's expenditures.

# Null hypothesis (H0): the coefficients are equal to zero (i.e., no relationship between variables)
# If the coefficients are zero in the linear model, any change in A's expenditure shall remain constant with respect to any change in B's expenditure.
# p-value: < 2.2e-16 and t > 1.96 (Assuming 95% CI)
# A small p-value (typically ??? 0.05) and t > 1.96 indicates strong evidence against the null hypothesis, so you reject it.
# Thus, the 1% increase in A's expenditures is offset by a 1% increase in B's expenditures.

# c.) Estimate the given model using the data in the vote1 table and report the results in usual form. Do
# A's expenditures affect the outcome? What about B's expenditures? Can you use these results to test
# the hypothesis in part 2?
# Pulling out vote1 data from the database and assigning it to a variable
vote1_dt <- wpull('vote1')
vote1_dt # Displays the vote1 data

# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(voteA ~ log(expendA)+log(expendB)+prtystrA, data = vote1_dt)
vote1_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.7925 and 169 dF
# We can state that the intercept in the equation is 45.08
# We can also state that coefficient of log(expendA) is 6.08, coefficient of log(expendB) is -6.61 and coefficient of prtystrA is 0.15
# Final equation is price=45.08+6.08*log(expendA)-6.61*log(expendB)+0.15*prtystrA

# A's expenditure is considered in it's log value which is very small. Though the coefficient of A's expenditure is positive(6.08) 
# Thus as the A's expenditure increases, the income also increases.
# B's expenditure is considered in it's log value which is very small. Though the coefficient of B's expenditure is negative(-6.61) 
# Thus as the B's expenditure increases, the income decreases and vice versa. 

# Yes, we can use the above results to test the hypothesis in b.) as the summary of the model depicts the p-value: < 2.2e-16  and only then can we assume the conditions for null hypothesis.

# d.) Estimate a model that directly gives the t-statistic for testing the hypothesis in part 2?
x <- summary(vote1_dt$expendA)
y <- summary(vote1_dt$expendB)
t.test(x,y)

# We obtained the t = -0.012039, df = 9.9611, p-value = 0.9906 by the Welch Two Sample t-test.
# As the t < 1.96 (assume 95% CI) and p > 0.05 the null hypothesis cannot be rejected

# e.) What do you conclude? (Use a two-sided alternative.)
# A two-sided hypothesis claims that a parameter is simply not equal to the value given by the null hypothesis (the direction does not matter) 
# Considering the alternate hypothesis for the model, true difference in means is not equal to 0.
# sample estimates:
# mean of x  mean of y 
#  427.2335   431.0847 

# ---------------------------------------------------------------------------------------------------------------------------------------------------------------

# Problem 2
# a.) Using the model: ln[salary] = 0 + 1LSAT + 2GPA + beta3 ln[libvol] + 4 ln[cost] + 5rank + u,
# state and test the null hypothesis that the rank of law schools has no ceteris paribus effect on median starting salary.
# Pulling out lawsch85 data from the database and assigning it to a variable
lawsch85_dt <- wpull('lawsch85')
lawsch85_dt # Displays the lawsch85 data

# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(log(salary) ~ LSAT+GPA+log(libvol)+log(cost)+rank, data = lawsch85_dt)
lawsch85_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.8417, and 130 dF
# We can state that the intercept in the equation is 8.3432262
# We can also state that coefficient of LSAT is 0.0046965, coefficient of GPA is 0.2475239, coefficient of log(libvol) is 0.0949932, coefficient of log(cost) is 0.0375539 and coefficient of rank is -0.0033246  
# Final equation is salary=8.3432262+0.0046965*LSAT+0.2475239*GPA+0.0949932*log(libvol)+0.0375539*log(cost)-0.0033246*rank  

# Null hypothesis (H0): the coefficients are equal to zero (i.e., no relationship between variables)
# If the coefficients are zero in the linear model, any change in salary shall remain constant with respect to any change in rank.
# p-value: < 2.2e-16 and t > 1.96 (Assume 95% CI)
# A small p-value (typically ??? 0.05) and t > 1.96 indicates strong evidence against the null hypothesis, so you reject it.
# An increase in 1 unit of rank decreases your salary by $112.38, ceteris paribus.

#If the correlation between two explanatory variable is comparatively very low, result of ceteris  paribus  assumption  and  Simple  Linear Regression  are  approximately  same.
cor.test(lawsch85_dt$salary,lawsch85_dt$index)
# As the correlation between salary and rank is -0.06 (very low), it has no ceteris paribus effect.

# b.) Are features of the incoming class of students-namely, LSAT and GPA-individually or jointly significant for explaining salary?
# An increase in 1 unit of LSAT increases your salary by $178.06
# An increase in 1 unit of GPA increases your salary by $15599.34 

# c.) Test whether the size of the entering class (clsize) and the size of the faculty (faculty) need to be added to this equation jointly.
# Pulling out lawsch85 data from the database and assigning it to a variable
lawsch85_dt <- wpull('lawsch85')
lawsch85_dt # Displays the lawsch85 data

# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(salary ~ LSAT+GPA+log(libvol)+log(cost)+rank+clsize+faculty, data = lawsch85_dt)
lawsch85_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.7962 and 123 dF
# We can state that the intercept in the equation is -87676.147
# We can also state that coefficient of LSAT is 211.356 , coefficient of GPA is 16437.107 , coefficient of log(libvol) is 3862.013, coefficient of log(cost) is 2635.234, coefficient of rank is -117.261, 
# coefficient of clsize is 5.646 and coefficient of faculty is 3.255

# Final equation is salary=-87676.147 +211.356*LSAT+16437.107*GPA+3862.013*log(libvol)+2635.234*log(cost)-117.261*rank+5.646*clsize+3.255*faculty

# clsize (5.466) and faculty (3.255) are not making major differences in salary (individually or jointly) and thus aren't that important parameters to affect salary value.

# d.) What factors might influence the rank of the law school that are not included in the salary regression?

# faculty, age,clsize, north, south, east, west are the factors that influence the salary apart from regression model.
cor.test(lawsch85_dt$rank,lawsch85_dt$faculty)
-0.3079391 
cor.test(lawsch85_dt$rank,lawsch85_dt$age)
-0.507981 
cor.test(lawsch85_dt$rank,lawsch85_dt$clsize)
-0.2466291 
cor.test(lawsch85_dt$rank,lawsch85_dt$east)
-0.1411058 
cor.test(lawsch85_dt$rank,lawsch85_dt$west)
-0.1261346 
cor.test(lawsch85_dt$rank,lawsch85_dt$north)
0.01590573
cor.test(lawsch85_dt$rank,lawsch85_dt$south)
0.2628926

# Amongst all factors, north and south locations of law school have a slight influence on the salary as their correlation values are positive.

# ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
#Problem 3
# a.) You are interested in estimating and obtaining a confidence interval for the percentage change in price when a 150-square-foot bedroom is added to a house. In decimal form, this is 1 = 1501 + 2. Use the data in the hprice1 table to estimate 1.
# Pulling out hprice1 data from the database and assigning it to a variable
hprice1_dt <- wpull('hprice1')
hprice1_dt # Displays the hprice1 data

# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(log(price) ~ sqrft+bdrms, data = hprice1_dt)
hprice1_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.5883 and 85 dF
# We can state that the intercept in the equation is 4.766e+00
# We can also state that coefficient of sqrft is 3.794e-04 and coefficient of bdrms is 2.888e-02
# Final equation is price=4.766e+00+3.794e-04*sqrft+2.888e-02*bdrms

t.test(hprice1_dt$price)
# The confidence interval displayed is 95% with t = 26.81, df = 87, p-value < 2.2e-16 and mean of price is 293.546 

# Hence,price=-19.31+0.12*sqrft+15.19*bdrms where sqrft is 150 and bdrms is 1
predict(model,newdata= data.frame(sqrft=150,bdrms=1))
# 15.14863 is the estimated price


# Percentage change in estimated price is 8.5%

# b.) Write 2 in terms of 1 and 1 and plug this into the ln[price] equation.
# T1 = 150*B1 + B2 : where T1 is price and B1 is sqrft and B2 is bdrms
# Then, B2 = T1 - 150*B1 

model <- lm(log(price) ~ I(sqrft-150*bdrms), data = hprice1_dt)
hprice1_dt
summary(model1)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.5385 and 86 dF
# We can state that the intercept in the equation is 5.001e+00 and p-value: 4.187e-16

#c.) Use part 2 to obtain a standard error for ^1 and use this standard error to construct a 95% confidence interval.
confint(model)
# We obtained a standard error for T1 and constructed a 95% confidence interval
#                              2.5 %       97.5 %
#(Intercept)            4.8683595171 5.1341200881
#I(sqrft - 150 * bdrms) 0.0003426121 0.0005122473

# --------------------------------------------------------------------------------------------------------------------------------------------------

# Problem 4
# a.) Consider the standard wage equation ln[wage] = 0 + 1educ + 2exper + 3tenure + u. State the null hypothesis that another year of general workforce experience has the same effect on ln[wage] as another year of tenure with the current employer.
# Pulling out wage2 data from the database and assigning it to a variable
wage2_dt <- wpull('wage2')
wage2_dt # Displays the wage2 data

# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(log(wage) ~ educ+exper+tenure, data = wage2_dt)
wage2_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.1551 and 931 dF
# We can state that the intercept in the equation is 5.496696
# We can also state that coefficient of educ is 0.074864, coefficient of exper is 0.015328 and coefficient of tenure is 0.013375

# Final equation is wage=5.496696+0.074864*educ+0.015328*exper+0.013375*tenure

# Null hypothesis (H0): the coefficients are equal to zero (i.e., no relationship between variables)
# If the coefficients are zero in the linear model, any change in salary shall remain constant with respect to any change in rank.
# p-value: < 2.2e-16 
# A small p-value (typically ??? 0.05) indicates strong evidence against the null hypothesis, so you reject it.

# t value of exper is 4.549 and tenure is 5.170 which are more than 1.96
# Thus, another year of general workforce experience has the same effect on ln[wage] as another year of tenure with the current employer

#-----------------------------------------------------------------------------------------------------------------------------------------------------

# Problem 5
# a.) How many single-person households are there in the data set

# Pulling out 401Ksubs data from the database and assigning it to a variable
K401subs_dt <- wpull('401Ksubs')
K401subs_dt # Displays the 401Ksubs data

fsize_count <- sum(K401subs_dt$fsize==1)
fsize_count

# 2017 are the number of single-person households

# b.) Use OLS to estimate the model nettfa = 0 + 1inc + 2age + u, and report the results using the usual format. be sure to use only the single-person households in the sample. Interpret the slope coefficients. Are there any surprises in the slope estimates?
# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(nettfa ~ inc+age, data = K401subs_dt)
K401subs_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.1619 and 9272 dF
# We can state that the intercept in the equation is -60.69654
# We can also state that coefficient of inc is 0.95336, coefficient of age is 1.03078

# Final equation is nettfa=-60.69654+0.95336*inc+1.03078*age

# c.) Does the intercept from the regression in part 2 have an interesting meaning? Explain.
# In a regression model where the intercept is negative (-60.69654) implies that the model is overestimating on an average the y values thereby a negative correction in the predicted values is needed.
# The p value (<2e-16) is < 0.05 and t value (-23.38) is < 1.96, denotes that null hypothesis can be rejected

# d.) Find the p-value for the test H0 : 2 = 1 against H1 : 2 < 1. Do you reject H0 at the 1% significance level?
# p-value is < 2.2e-16 and less than 0.05 at a 1% significance level and thus null hypothesis is rejected

# e.) If you do a simple regression of nettfa on inc, is the estimated coefficient on inc much different from the estimate in part 2? Why or why not?
# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(nettfa ~ inc, data = K401subs_dt)
K401subs_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.1418 and 9273 dF
# We can state that the intercept in the equation is -20.17948
# We can also state that coefficient of inc is 0.99991

# Final equation is nettfa=-20.17948+0.99991*inc

# There is not much difference in the coefficients of income in both cases 
# But the impact of income on the nettfa remains the same and plays a role of key factor

#----------------------------------------------------------------------------------------------------------------------------------------------

# Problem 6
# a.) To study the effects of the incinerator location on housing price, consider the simple regression model ln[price] = 0 + 1 ln[dist] + u, where price is housing price in dollars and dist is distance from the house to the incinerator measured in feet. Interpreting this equation causally, what sign do you expect for 1 if the presence of the incinerator depresses housing prices? Estimate this equation and interpret the results.
# Pulling out kielmc data from the database and assigning it to a variable
kielmc_dt <- wpull('kielmc')
kielmc_dt # Displays the kielmc data

# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(log(price) ~ log(dist), data = kielmc_dt)
kielmc_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.1199 and 319 dF
# We can state that the intercept in the equation is 8.25750
# We can also state that coefficient of log(dist) is 0.31722

# Final equation is wage=8.25750+0.31722*log(dist)

# b.) To the simple regression model in part 1, add the variables ln[intst], ln[area], ln[land], rooms, bath, and age, where intst is distance from the home to the interstate, area is square footage of the house,land is the lot size in square feet, rooms is total number of rooms, baths is number of bathrooms, and age is age of the house in years. Now, what do you conclude about the effects of the incinerator? Explain why 1 and 2 give conflicting results
# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(log(price) ~ log(dist)+log(intst)+log(area)+log(land)+rooms+baths+age, data = kielmc_dt)
kielmc_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.5925 and 313 dF
# We can state that the intercept in the equation is 6.2996586
# We can also state that coefficient of log(dist) is  0.0281887, log(intst) is -0.0437804, log(area) is 0.5124071, log(land) is 0.0782098, rooms is 0.0503129, baths is 0.1070528 and age is -0.0035630
# Final equation is price=6.2996586+0.0281887*log(dist)-0.0437804*log(intst)+0.5124071*log(area)+0.0782098*log(land)+ 0.0503129*rooms+0.1070528*baths-0.0035630*age

# As the parameters dist, area, land, rooms and baths increases, the price also increases
# As the parameters intst and age increases, the price decreases
# The parameters area and age are the key factors for price followed by  baths, land and area

# c.) Add ln[intst]2 to the model from part 2. Now what happens? What do you conclude about the importance of functional form?
# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(log(price) ~ log(dist)+log(intst^2)+log(area)+log(land)+rooms+baths+age, data = kielmc_dt)
kielmc_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.5925 and 313 dF
# We can state that the intercept in the equation is 6.2996586
# We can also state that coefficient of log(dist) is 0.0281887, log(intst^2) is -0.0218902, log(area) is 0.5124071, log(land) is 0.0782098, rooms is 0.0503129, baths is 0.1070528 and age is -0.0035630
# Final equation is price=6.2996586+0.0281887*log(dist)-0.0218902*log(intst^2)+0.5124071*log(area)+0.0782098*log(land)+0.0503129*rooms+0.1070528*baths-0.0035630*age

# Only the coefficient of log(inst) changed from -0.0437804 to -0.0218902
# As the log(inst) increases, price decreases by a lesser value

# d.) Is the square of ln[dist] significant when you add it to the model from part 3?
# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(log(price) ~ log(dist^2)+log(intst^2)+log(area)+log(land)+rooms+baths+age, data = kielmc_dt)
kielmc_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.5925 and 313 dF
# We can state that the intercept in the equation is 6.2996586
# We can also state that coefficient of log(dist^2) is 0.0140944, log(intst^2) is -0.0218902, log(area) is 0.5124071, log(land) is 0.0782098, rooms is 0.0503129, baths is 0.1070528 and age is -0.0035630 
# Final equation is price=6.2996586+0.0140944*log(dist^2)-0.0218902*log(intst^2)+0.5124071*log(area)+0.0782098*log(land)+0.0503129*rooms+0.1070528*baths-0.0035630 *age

# Only the coefficient of log(dist^2) changed from 0.0281887 to 0.0140944 (i.e almost half)
# As the log(dist) increases, price decreases by a half it's value

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Problem 7
# a.) Use OLS to estimate the equation ln[wage] = 0 + 1educ + 2exper + 3exper2 + u and report the results using the usual format.
# Pulling out wage1 data from the database and assigning it to a variable
wage1_dt <- wpull('wage1')
wage1_dt # Displays the wage1 data

# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(log(wage) ~ educ+exper+I(exper^2), data = wage1_dt)
wage1_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.3003 and 522 dF
# We can state that the intercept in the equation is 0.1263226 
# We can also state that coefficient of educ is 0.0906207, coefficient of exper is 0.0409731 and coefficeint of exper^2 is -0.0007121

# Final equation is price=0.1263226+0.0906207*educ+0.0409731*exper-0.0007121*exper^2

# b.) Is exper2 statistically significant at the 1% level?
# p value of exper^2 is 3.70e-08 which is < 0.05 and t value is -6.141 which is less than 1.28 so the null hypothesis is rejected 
# The p value is almost equal to zero and thus exper2 is statistically significant at the 1% level

# c.) Using the approximation @ ln[wage] / @exper  ^ 2 + 2 ^ 3exper, find the approximate return to the fifth year of experience. What is the approximate return to the twentieth year of experience?
model1 <- coef(model)
return5 <- (model1[3]+2*model1[4]*4) # As 5th year means, 4 years of experience
return5
# The approximate return for 5th year is 5.9%
return5 <- (model1[3]+2*model1[4]*19) # As 20th year means, 19 years of experience
return5
# The approximate return for 20th year is 1.39%

# d.) At what value of exper does additional experience actually lower predicted ln[wage]? How many people have more experience in this sample?
wage1 <- model1[3]/2/model1[4]
wage1
# When the exper reaches 29.12 years, the additional experience actually lowers in prediction

exper_count <- sum(wage1_dt$exper>29.12)
exper_count
# 111 people have more experience than 29.12 years in this sample

#---------------------------------------------------------------------------------------------------------------------------------------------------------------

# Problem 8
# a.) Show that the return to another year of education (in decimal form), holding experience fixed, is 1 + 3exper
# Pulling out wage1 data from the database and assigning it to a variable
wage2_dt <- wpull('wage2')
wage2_dt # Displays the wage2 data

# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(log(wage) ~ educ+exper+I(educ*exper), data = wage2_dt)
wage2_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.1349 and 931 dF
# We can state that the intercept in the equation is 5.949455   
# We can also state that coefficient of educ is 0.044050, coefficient of exper is -0.021496 and coefficeint of exper*educ is 0.003203 

# Final equation is price=5.949455+0.044050*educ-0.021496*exper+0.003203*(exper*educ)

# d[log(wage)]/d(educ) = d[b0 + b1educ + b2exper + b3educ*exper]/d(educ)
# d[log(wage)]/d(educ) = 0 + b1 + 0 + b3exper
# d[log(wage)]/d(educ) = b1 + b3exper

# b.) State the null hypothesis that the return to education does not depend on the level of experience. What do you think is the appropriate alternative?
# Null hypothesis (H0): the coefficients are equal to zero (i.e., no relationship between variables)
# If the coefficients are zero in the linear model, any change in A's expenditure shall remain constant with respect to any change in B's expenditure.
# p-value: < 2.2e-16 and t > 1.96 (Assuming 95% CI)
# A small p-value (typically ??? 0.05) and t > 1.96 indicates strong evidence against the null hypothesis, so you reject it.
cor.test(wage2_dt$exper,wage2_dt$educ)
# -0.4555731 is the value of correlation
# Thus, the increase in education will not effect the experience as they are correlated negatively.

# c.) Use the data in the wage2 table to test the null hypothesis in 2 against your stated alternative.
# From the given model, we get the t-value = 2.095 > 1.96(standard value)
# p = 0.03 < 0.05. So, from the obtained t value and significance level,we reject the null hypothesis and consider the alternative hypothesis.

# d.) Let 1 denote the return to education (in decimal form), when exper = 10: 1 = 1 + 103. Estimate 1 and a 95% confidence interval for 1. (Hint: Write 1 = 1 ??? 103and plug this into the equation; then rearrange. This gives the regression for obtaining the confidence interval for 1.)
confint(model)
# Estimating for 95% interval 
#                     2.5 %      97.5 %
# (Intercept)      5.4768291195 6.422080300
# educ             0.0099194642 0.078180123
# exper           -0.0607036116 0.017711742
# I(educ * exper)  0.0002019664 0.006203983

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Problem 9
# a.) Estimate the model sat = 0 + 1hsize + 2hsize2 + u. where hsize is the size of the graduating class (in hundreds), and write the results in the usual form. Is the quadratic term statistically significant?
# Pulling out gpa2 data from the database and assigning it to a variable
gpa2_dt <- wpull('gpa2')
gpa2_dt # Displays the gpa2 data

# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(sat ~ hsize+I(hsize^2), data = gpa2_dt)
gpa2_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.00765 and 4134 dF
# We can state that the intercept in the equation is 997.981        
# We can also state that coefficient of hsize is 19.814, coefficient of hsize^2 is -2.131

# Final equation is price=997.981+19.814*hsize-2.131*(hsize^2)

# b.) Using the estimated equation from part 1, what is the "optimal" high school size? Justify your answer.
# We need to derivate the model with respect to hsize and equal it to 0.
# d(sat)/d(hsize) = d[997.981 + 19.814*hsize - 2.131*hsize^2]/d(hsize)
# => 0 = 0 + 19.814 - (2 * 2.131) hsize
# => hsize = 19.814/4.262 => hsize = 465
# The optimal high school size is 465

# c.) Is this analysis representative of the academic performance of all high school seniors? Explain.
# No, the analysis does not represent the all high school seniors because, the data only represents the students with SAT scores as there may be students who did not give SAT to go to college.

# d.) Find the estimated optimal high school size, using ln(sat) as the dependent variable. Is it much different from what you obtained in part 2?
gpa21 <- coef(model)
hsize1 <- -gpa21[2]/2/gpa21[3]
hsize1
# The difference in optimal hsize is 4.64

gpa22 <- lm(log(sat) ~ hsize + I(hsize^2), data = gpa2_dt)
summary(gpa2_dt)
gpa22 <- coef(model)
hsize2 <- -gpa22[2]/2/gpa22[3]
hsize2
# The optimal hsize of second model = 469.6 (i.e., 465+4.64)

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Problem 10
# a.) Estimate the model ln[price] = 0 + 1 ln[lotsize] + 2 ln[sqrft] + 3bdrms + u and report the results in the usual OLS format.
# Pulling out hprice1 data from the database and assigning it to a variable
hprice1_dt <- wpull('hprice1')
hprice1_dt # Displays the hprice1 data

# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(log(price) ~ log(lotsize)+log(sqrft)+bdrms, data = hprice1_dt)
hprice1_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.643 and 84 dF
# We can state that the intercept in the equation is -1.29704        
# We can also state that coefficient of lotsize is 0.16797, coefficient of sqrft is 0.70023 and coefficient of bdrms is 0.03696

# Final equation is log(price)=-1.29704+log(lotsize)*0.16797+log(Sqrft)*0.70023+bdrms*0.03696

# b.) Find the predicted value of price, when lotsize = 20 000, sqrft = 2 500, and bdrms = 4.
predict(model,newdata= data.frame(lotsize=20000,sqrft=2500,bdrms=4))
# Predicted log(price) is 5.992899 

# c.) For explaining variation in price, decide whether you prefer the model from part 1 or the model price = 0 + 1lotsize + 2sqrft + 3bdrms + u
# Pulling out hprice1 data from the database and assigning it to a variable
hprice1_dt <- wpull('hprice1')
hprice1_dt # Displays the hprice1 data

# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(price ~ lotsize+sqrft+bdrms, data = hprice1_dt)
hprice1_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.6724 and 84 dF
# We can state that the intercept in the equation is -2.177e+01      
# We can also state that coefficient of lotsize is 2.068e-03, coefficient of sqrft is 1.228e-01 and coefficient of bdrms is 1.385e+01

# Final equation is log(price)=-2.177e+01+lotsize*2.068e-03+Sqrft*1.228e-01+bdrms*1.385e+01

# Part 1 has R-squared:  0.643 and Part 2 has R-squared:  0.6724
# As the R-squared value of part 2 is higher, the variation is more significantly explained in part 2 model

