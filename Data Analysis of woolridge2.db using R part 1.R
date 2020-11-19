
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

# Problem 1 : 

# a.)Find the average education level in the sample. What are the lowest and highest years of education?
# Pulling out wage1 data from the database and assigning it to a variable
wage1_dt <- wpull('wage1')

wage1_dt # Displays the wage1 data

# Calculates the minimum, maximum, median, mean, 1st & 3rd quartile values of wage1 data for education level
summary(wage1_dt$educ)
# We can conclude that the : 
# Mean = 12.56, Lowest year of education = 0, Highest year of education = 18

# b.)Find the average hourly wage in the sample. Does it seem high or low?
# Calculates the minimum, maximum, median, mean, 1st & 3rd quartile values of wage1 data for wage
summary(wage1_dt$wage)
# We can conclude that the : 
# Mean = 5.909, Lowest year of education = 0.53, Highest year of education = 25
# The average hourly wage seems reasonable

# c.)The wage data are reported in 1976 dollars. Using the Economic Report of the President (2011 or
# later), obtain and report the Consumer Price Index (CPI) for the years 1976 and 2010

# As per the https://www.govinfo.gov/app/collection/ERP/ 
cpi_1976 <- 56.9
cpi_2010 <- 218.1

# d.)Use the CPI values from above to find the average hourly wage in 2010 dollars. Now does the average
# hourly wage seem reasonable?
avg_wage_1976 <- mean(wage1_dt$wage)
avg_wage_2010 <- (cpi_2010/cpi_1976)*avg_wage_1976
avg_wage_2010
# We can conclude that the average wage of 2010 is 22.64 is has reasonable improvement.

# e.)How many women are in the sample? How many men?
female_count <- sum(wage1_dt$female==1)
female_count
# We can conclude that there 252 women in the sample
male_count <- sum(wage1_dt$female==0)
male_count
# We can conclude that there 274 women in the sample

__________________________________________________________________________________________________________________

# Problem 2

# a.)Find the largest and smallest values of math4. Does the range make sense? Explain.
# Pulling out meap01 data from the database and assigning it to a variable
meap01_dt <- wpull('meap01')
meap01_dt # Displays the meap01 data

# Calculates the minimum, maximum, median, mean, 1st & 3rd quartile values of meap01 data for math4
summary(meap01_dt$math4)
# We can conclude that the : 
# Mean = 71.91, Lowest year of education = 0, Highest year of education = 100
# The range does not make sense as the lowest year of education shows 0, it means no education.

# b.) How many schools have a perfect pass rate on the math test? What percentage is this of the total
# sample?
perfect_passrate <- sum(meap01_dt$math4==100)
perfect_passrate
# We can conclude that 38 out of 100 schools have a perfect pass rate on the math test i.e. 38% of schools.

# c.)How many schools have math pass rates of exactly 50%?
math_passrate <- sum(meap01_dt$math4==50)
math_passrate
# We can conclude that 17 schools have a 50% pass rate on the math test.

# d.)Compare the average pass rates for the math and reading scores. Which test is harder to pass?
# Calculates the minimum, maximum, median, mean, 1st & 3rd quartile values of meap01 data for education level
summary(meap01_dt$math4)
# We can conclude that the mean of math4 = 71.91

# Calculates the minimum, maximum, median, mean, 1st & 3rd quartile values of meap01 data for read4
summary(meap01_dt$read4)
# We can conclude that the mean of read4 = 60.06
# Read4 is easier to pass than math4 as the average mean of read4 is lesser than math4

# e.) Find the correlation between math4 and read4. What do you conclude?
cor(meap01_dt$math4,meap01_dt$read4)
# We can conclude that the correlation value of math4 and read4 is 0.84. As it is positive, if math4 increases, read4 also increases and vice-versa.

# f.) The variable exppp is expenditure per pupil. Find the average of exppp along with its standard
# deviation. Would you say there is wide variation in per pupil spending?

# Calculates the minimum, maximum, median, mean, 1st & 3rd quartile values of exppp data for exppp
summary(meap01_dt$exppp)
# We can conclude that the mean = 5195

# Calculates the standard deviation of the expenditure per pupil
sd(meap01_dt$exppp)
# We can conclude that the standard deviation = 1091.89
# If there is a wide variation in per pupil spending, the expenditures of both the schools shall vary.

# g.)Suppose School A spends $6000 per student and School B spends $5500 per student. By what percentage
# does School A’s spending exceed School B’s? Compare this to 100  [ln(6000) − ln(5500)], which is the
# approximation percentage difference based on the difference in the natural logs.
a=6000 #Expenditure per student of school A
b=5500 #Expenditure per student of school B
print((a-b)/b*100)
# We obtained the percentage of difference in spending between school A and B is 9.09%

# Calculating the condition stated in question for comparison
a=log10(6000)
b=log10(5500)
c=100*(a-b)
c
# We obtained the percentage of difference in spending between school A and B is 3.77%

______________________________________________________________________________________


#Question 3
# a.)Find the average participation rate and the average match rate in the sample of plans.
# Pulling out 401k data from the database and assigning it to a variable
dt_401k <- wpull('401k')
dt_401k # Displays the data_401k data

# Calculates the minimum, maximum, median, mean, 1st & 3rd quartile values of 401k data for participation rate
summary(dt_401k$prate) # where prate is the participation rate in sample of plans
# We can conclude that the mean = 87.36

# Calculates the minimum, maximum, median, mean, 1st & 3rd quartile values of 401k data for participation rate
summary(dt_401k$mrate) # where mrate is the match rate in sample of plans
# We can conclude that the mean = 0.7315

# b.) Now, estimate the simple regression equation: prˆate = ˆ0 + ˆ1mrate,and report the results along with the sample size and R-squared.
# Stating a simple regression equation to obtain coefficients of linear model
model <- lm(prate ~ mrate, data = dt_401k)
dt_401k
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# We can state that the range of linear model is : Min (-82.30) to Max(16.80)
# We can also state that Multiple R-squared:  0.0747,	Adjusted R-squared:  0.0741 

# d.) Interpret the intercept in your equation. Interpret the coefficient on mrate.
# The intercept in equation is 83.0755
# Coefficient of mrate is 5.8611 

# e.) Find the predicted prate when mrate = 3.5. Is this a reasonable prediction? Explain what is happening
# here.
mrate <- 3.5
prate <- 83.0755+5.861*mrate
prate
# We obtained the prate as 103.589 when the mrate is 3.5

# e.) How much of the variation in prate is explained by mrate?

_______________________________________________________________________________________________________________

# Question 4
# a.) Find the average salary and the average tenure in the sample.
# Pulling out ceosal2 data from the database and assigning it to a variable
ceosal2_dt <- wpull('ceosal2')
ceosal2_dt # Displays the ceosal2_dt data

# Calculates the minimum, maximum, median, mean, 1st & 3rd quartile values of salary
summary(ceosal2_dt$salary)
# We can conclude that the average salary = 865.9 (8,66,000 $)

# Calculates the minimum, maximum, median, mean, 1st & 3rd quartile values of tenure in company
summary(ceosal2_dt$comten) # where comten is tenure in a company
# We can conclude that the average tenure = 22.5

# b.)How many CEOs are in their first year as CEO (that is, ceoten = 0)? What is the longest tenure as a
# CEO?
CEO_count <- sum(ceosal2_dt$ceoten==0) # where ceoten is tenure of CEO in a company
CEO_count
# We can conclude that there 5 CEOs in the first year

# Calculates the minimum, maximum, median, mean, 1st & 3rd quartile values of tenure in company
summary(ceosal2_dt$comten) # where comten is tenure in a company
# We can conclude that the longest tenure by a CEO is 58 years

# c.) Estimate the simple regression model ln[salary] = 0 + 1ceoten + u and report your results in the usual form. What is the (approximate) predicted percentage increase in salary given one more year as a CEO?
# Stating a simple regression equation to obtain coefficients of linear model
model <- lm(log10(salary) ~ ceoten, data = ceosal2_dt)
ceosal2_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# We can state that the intercept in the equation is 2.825
# We can also state that coefficient of ceoten is 0.004

_________________________________________________________________________________________________________________________________________

# Problem 5
# a.) Find the average salary and average IQ in the sample. What is the sample standard deviation of IQ? (IQ scores are standardized so that the average in the population is 100 with a standard deviation equal to 15.)
# Pulling out wage1 data from the database and assigning it to a variable
wage2_dt <- wpull('wage2')
wage2_dt # Displays the wage2 data

# Calculates the minimum, maximum, median, mean, 1st & 3rd quartile values of wage2 data for wage
summary(wage2_dt$wage)
# We can conclude that the mean of wage = 957.9

# Calculates the minimum, maximum, median, mean, 1st & 3rd quartile values of wage2 data for IQ
summary(wage2_dt$IQ)
# We can conclude that the mean of IQ = 101.3

# Calculates the standard deviation of IQ
sd(wage2_dt$IQ)
# We obtained the standard deviation of IQ as 15.05

# b.) Estimate a simple regression model where a one-point increase in IQ changes wage by a constant dollar amount. Use this model to find the predicted increase in wage for an increase in IQ of 15 points. Does IQ explain most of the variation in wage?
# Stating a simple regression equation to obtain coefficients of linear model
model <- lm(wage ~ IQ, data = wage2_dt)
wage2_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# We can state that the intercept in the equation is 116.99
# We can also state that coefficient of IQ is 8.30

IQ <- 15 # As per the condition provided in the question
wage <- (15*8.30)+116.99
wage
# We obtained the wage as 241.49$ if the IQ is 15 according to the simple regression model

# c.) Now, estimate a model where each one-point increase in IQ has the same percentage effect on wage. If IQ increases by 15 points, what is the approximate percentage increase in predicted wage?
# Stating a simple regression equation to obtain coefficients of linear model
model <- lm(IQ ~ wage, data = wage2_dt)
wage2_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# We can state that the intercept in the equation is 90.26
# We can also state that coefficient of wage is 0.01

IQ <- 15 # As per the condition provided in the question
wage <- (90.26-15)/0.01
wage
# We obtained the wage as 7526$ if the IQ is 15 according to the simple regression model

# The percent difference between both the wage values obtained above 
a=7526
b=241.49
c= (a-b)/a*100
c
# We can state that there is 96.79% difference in predicted wage.

____________________________________________________________________________________________________________________________________________

# Problem 6
# a.) Do you think each additional dollar spent has the same effect on the pass rate, or does a diminishing effect seem more appropriate? Explain.
# Pulling out meap93 data from the database and assigning it to a variable
meap93_dt <- wpull('meap93')
meap93_dt # Displays the meap93 data

# Calculating relationship between math10(math pass rate) and expend(expenditure per student of meap93 data
cor(meap93_dt$math10,meap93_dt$expend)
# We can conclude that the correlation between math10(math pass rate) and expend(expenditure per student) is 0.18
# Thus the diminishing effect seems more appropriate

# b.) In the population model, math10 = 0 + 1 ln[expend] + u argue that 1/10 is the percentage point change in math10 given a 10% increase in expend.
# c.) Estimate this model. Report the estimated equation in the usual way, including the sample size and R-squared.
# Stating a simple regression equation to obtain coefficients of linear model
model <- lm(math10 ~ expend, data = meap93_dt)
meap93_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.03296 and 406 dF
# We can state that the intercept in the equation is 1.336e+01 
# We can also state that coefficient of expend is 2.456e-03 (same as 0.0002456)

expend <- 0.11 # As per the condition provided in the question
math10 <- (0.002456/11)+13.36
math10
# We obtained the math10 as 13.36 if the expend is 10% according to the simple regression model
# As per the condition in the question 
2.456e-03/10
0.0002456
# Hence we can conclude that there is a percentage point change in math10 given 10% increase in expend

# d.) How big is the estimated spending effect? Namely, if spending increases by 10%, what is the estimated percentage point increase in math10?
expend <- 0.11 # As per the condition provided in the question
math10 <- (0.002456/11)+13.36
math10
# We obtained the math10 as 13.36 if the expend is 10% according to the simple regression model

# e.) One might worry that regression analysis can produce fitted values for math10 that are greater than 100. Why is this not much of a worry in this data set?
# Calculating max value of expend to predict the max value of math10
summary(meap93_dt$expend)
# We obtained the max value of expend as 7419
# Now substituting the same into linear regression model equation 
expend <- 7419
math10 <- (0.002456*7419)+13.36
math10

# We observed the the maximum value of expend is fitting only 31.58 pass rate in math. 
# Thus there is no worry that fitted values for math10 will be greater than 66.70 (which is math10 maximum value) in this data set.

____________________________________________________________________________________________________________________________________________________

# Problem 7
# a.) Write out the results in equation form.
# Pulling out hprice1 data from the database and assigning it to a variable
hprice1_dt <- wpull('hprice1')
hprice1_dt # Displays the hprice1 data

# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(price ~ sqrft+bdrms, data = hprice1_dt)
hprice1_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.6319 and 85 dF
# We can state that the intercept in the equation is -19.31
# We can also state that coefficient of sqrft is 0.12 and coefficient of bdrms is 15.19
# Final equation is price=-19.31+0.12*sqrft+15.19*bdrms

# b.) What is the estimated increase in price for a house with one more bedroom, holding square footage constant?
# From the data set of hprice1_dt let us take the below consideration
#index   price  assess   bdrms  lotsize  sqrft  colonial
# 0      300.000  349.1     4    6126    2438      1
predict(model,newdata= data.frame(sqrft=2438,bdrms=4))
354.6052
# If bdrm increase by 1 and sqrft remaining constant
predict(model,newdata= data.frame(bdrms=5,sqrft=2438))
369.8034
# The estimated increase in price is
369.80-354.60
15.2

# c.) What is the estimated increase in price for a house with an additional bedroom that is 140 square feet in size? Compare this to your answer from above. 
# If bdrm increases by 1 (default 5) and sqrft is 140, then the price is
predict(model,newdata= data.frame(bdrms=6,sqrft=140))
89.85522 

# The estimated increase in price of house when compared to previous findings
89.85-15.2
74.65

# d.) What percentage of the variation in price is explained by square footage and number of bedrooms?
predict(model,newdata=data.frame(bdrms,sqrft))
3.5 %
# There is 3.5% variation in price with respect to sqrft and bdrms

# e.) The first house in the sample has sqrft = 2438 and bdrms = 4. Find the predicted selling price for this house from the OLS regression line.
predict(model, newdata= data.frame(bdrms=4,sqrft=2438))
 # The selling price is 354.6052

# f.) The actual selling price of the first house in the sample was $300000 (so price = 300). Find the residual for this house. Does it suggest that the buyer underpaid or overpaid for the house? 
# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(price ~ sqrft+bdrms, data = hprice1_dt)
hprice1_dt
summary(model)
# We obtained the residuals standard error as 63.04. As the value is positive, the buyer overpaid for it.

_____________________________________________________________________________________________________________________________________________________________________________________________________

# Problem 8
# a.) Estimate a model relating annual salary to firm sales and market value. Make the model of the constant elasticity variety for both independent variables. Write the results out in equation form.
# Pulling out ceosal2 data from the database and assigning it to a variable
ceosal2_dt <- wpull('ceosal2')
ceosal2_dt # Displays the ceosal data

# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(salary ~ sales+mktval, data = ceosal2_dt)
ceosal2_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.17 and 174 dF
# We can state that the intercept in the equation is 7.166e+02
# We can also state that coefficient of sales is 1.650e-02 and coefficient of mktval is 2.529e-02
# Final equation is atndrte=7.166e+02+1.650e-02*sales+2.529e-02*mktval

#b.)  Add profits to the model. Why can this variable not be included in logarithmic form? Would you say that these firm performance variables explain most of the variation in CEO salaries? 
# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(salary ~ sales+mktval+profits, data = ceosal2_dt)
ceosal2_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.17 and 173 dF
# We can state that the intercept in the equation is 717.06
# We can also state that coefficient of sales is 0.01598, coefficient of mktval is 0.02383 and coefficient of profits is 0.03170.     
# Final equation is atndrte=717.06+0.01598*sales+0.02383*mktval+0.03170*profits

# As suggested in question, if log of profits is considered
log(0.03170)
-3.451439
# As the logarithmic value of profits is negative, thus it cannot be included.
# Yes, the variables explain most of the variation in CEO salaries along with ceoten.

# c.) Now also add the variable ceoten to the model. What is the estimated percentage return for another year of CEO tenure, holding other factors fixed?
# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(salary ~ sales+mktval+profits+ceoten, data = ceosal2_dt)
ceosal2_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.2014 and 172 dF
# We can state that the intercept in the equation is 613.95878
# We can also state that coefficient of sales is 0.01823, coefficient of mktval is 0.02116, coefficient of profits is 0.04864 and coefficient of ceoten is 12.73086     
# Final equation is atndrte=613.95878+.01823*sales+0.02116*mktval+0.04864*profits+12.73086*ceoten

# # From the data set of ceosal2_dt let us take the below consideration
# index salary age college  grad comten ceoten sales profits  mktval
#  0   1161    49       1    1      9      2    6200  966     23200
predict(model,newdata= data.frame(sales=1161,mktval=23200,profits=966,ceoten=2))
1198.414
# As per the condition, if ceoten is increased by 1 with other factors fixed
predict(model,newdata= data.frame(sales=1161,mktval=23200,profits=966,ceoten=3))
1211.145
# The estimated percentage return is 
(1211.145-1198.414)/100
0.12 %

# d.) Find the sample correlation coefficient between the variables log(mktval) and profits. Are these variables highly correlated? What does this say about the OLS estimators? 
cor(log(ceosal2_dt$mktval),ceosal2_dt$profits)
0.7768
# The coefficient of correlation between log(mktval) and profits is 0.7768. 
# The varaibles are highly correlated as the value is alomst equal to 1.

_______________________________________________________________________________________________________________________________________________________________________________________________

# Problem 9
# a.) Obtain the minimum, maximum, and average values for the variables atndrte, priGPA, and ACT.
# Pulling out hprice1 data from the database and assigning it to a variable
attend_dt <- wpull('attend')
attend_dt # Displays the attend data
summary(attend_dt)
# Values for atndrte : Min=6.25, Max=100, Mean=81.71
# Values of priGPA : Min=0.85, Max=3.93, Mean=2.58
# Vales of ACT : Min=13, Max=32, Mean=22.51

# b.) Estimate the model atndrte = 0 + 1GPA + 2ACT + u and write the results in equation form. Interpret the intercept. Does it have a useful meaning?
# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(atndrte ~ priGPA+ACT, data = attend_dt)
attend_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.2906 and 677 dF
# We can state that the intercept in the equation is 75.70
# We can also state that coefficient of priGPA is 17.26 and coefficient of ACT is -1.71
# Final equation is atndrte=75.70+17.26*priGPA-1.71*ACT

# c.) Discuss the estimated slope coefficients. Are there any surprises?
# The slope coefficients are : 17.26 for priGPA and -1.71 for ACT
# For the attendance rate to be more, the GPA should be more but the ACT should be less.

# d.) What is the predicted atndrte if priGPA = 3.65 and ACT = 20? What do you make of this result? Are there any students in the sample with these values of the explanatory variables?
predict(model,newdata= data.frame(priGPA=3.65,ACT=20))
104.3705 

# Counting number of students with explanatory variables as above
priGPA_count <- sum(attend_dt$priGPA==3.65,attend_dt$ACT==20)
priGPA_count
# There are 72 students with explanatory variables (priGPA=3.65,ACT=20)

# e.) If Student A has priGPA = 3.1 and ACT = 21 and Student B has priGPA = 2.1 and ACT = 26, what is the predicted difference in their attendance rates?
# Student A
predict(model, newdata= data.frame(priGPA=3.1,ACT=21))
93.16063 
# Student B
predict(model, newdata= data.frame(priGPA=2.1,ACT=26))
67.31727 
# Predicted difference in attendance rates between students A and B
93.16063 -67.31727 
25.84336

_______________________________________________________________________________________________________________________________________________________________________

# Problem 10
# a.) What is the range of the educ variable in the sample? What percentage of men completed 12th grade but no higher grade? Do the men or their parents have, on average, higher levels of education?
# Pulling out htv data from the database and assigning it to a variable
htv_dt <- wpull('htv')
htv_dt # Displays the htv data
summary(htv_dt)
# Range of educ is Min (6.00) to Max(20.00)

# Calculating number of students with 12 grade
men12_grade <- sum(htv_dt$educ==12)
men12_grade

# There are 512 out of 1230 men who finished 12 grade.
512/1230*100
41.62 
# 41.62% of men finished 12th grade but no higher grade

# Calculating the averages of men and their parents
summary(htv_dt$motheduc)
# Mean of motheduc is 12.18
summary(htv_dt$fatheduc)
# Mean of fatheduc is 12.00
summary(htv_dt$educ)
# Mean of men educ is 12.00
# We can state that men education levels are higher than father's education but lesser than mother's education

# b.) Estimate the regression model educ = 0 + 1motheduc + 2fatheduc + u by OLS and report the results in the usual form. How much sample variation in educ is explained by parents’ education? Interpret the coefficient on motheduc.
# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(educ ~ motheduc+fatheduc, data = htv_dt)
htv_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.2493 and 1227 dF
# We can state that the intercept in the equation is 6.96
# We can also state that coefficient of motheduc is 0.30 and coefficient of fatheduc is 0.19
# Final equation is educ=6.96+0.30motheduc+0.19fatheduc

# We can state that the dependency of mother's education is more than the father's education as per the coefficient.  

# c.) Add the variable abil (a measure of cognitive ability) to the regression above, and report the results in equation form. Does ability help to explain variations in education, even after controlling for parents’ education? Explain.   
# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(educ ~ motheduc+fatheduc+abil, data = htv_dt)
htv_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.4275 and 1226 dF
# We can state that the intercept in the equation is 8.44
# We can also state that coefficient of motheduc is 0.18, coefficient of fatheduc is 0.11 adn coeff of abil is 0.50
# Final equation is educ=8.44+0.18motheduc+0.11fatheduc+0.50abil

# We can state that ability coeff 0.50, which explains more variation than fatheduc coeff 0.11

# d.) Now estimate an equation where abil appears in quadratic form: educ = 0 + 1motheduc + 2fatheduc + 3abil + 4abil2 + u. With the estimated coefficients on ability, use calculus to find the value of abil where educ is minimized. (The other coefficients and values of parents’ education variables have no effect; we are holding parents’ education fixed.) Notice that abil is measured so that negative values are permissible. You might also verify that the second derivative is positive so that you do indeed have a minimum.
# Stating a simple regression equation to obtain coefficients of linear model 
model <- lm(educ ~ motheduc+fatheduc+abil+I(abil^2), data = htv_dt)
htv_dt
summary(model)
# We obtained the residuals, coefficients, significant codes, residual standard error, multiple R-squared and F-statistic.
# R-squared:  0.444 and 1225 dF
# We can state that the intercept in the equation is 8.44
# We can also state that coefficient of motheduc is 0.19, coefficient of fatheduc is 0.10, coefficient of abil is 0.40 and coefficient of abil^2 is 0.05
# Final equation is educ=8.44+0.19motheduc+0.10fatheduc+0.40abil+0.05abil^2

# Differentiating educ with respect to abil (motheduc and fatheduc as constants)
# d(educ)/d(abil) = 0.40+0.05*2*abil (First derivative is positive)
# d(educ)/d^2(abil) = 0+0.05(2) = 0.10 (Second derivative is also positive)

# Calculating abil at minimized educ means educ = 0
# d(educ)/d(abil) = 0.40+0.05*2*abil
# 0 = 0.4+0.1*abil
# -0.4/0.1 = abil : abil = -3.97 (Negative values are permissible)

# e.) Argue that only a small fraction of men in the sample have ability less than the value calculated above. Why is this important?
# As obtained above 
summary(htv_dt$abil)
abil_count <- sum(htv_dt$abil< -3.974) # where -3.97 is value of ability found
abil_count
15

# Fraction of people 
15/1230
0.01
# We can conclude 1.2% of people (a small fraction) have ability less than value of ability -3.97. 

# f.) Use the estimates above to plot the relationship beween the predicted education and abil. Let motheduc and fatheduc have their average values in the sample, 12.18 and 12.45, respectively.
cor.test(htv_dt$educ,htv_dt$abil)
#The correlation between predicted eduction and abil is between 0.55 to 0.62

# As suggested, if motheduc is 12.18 and fatheduc is 12.45
predict(model,newdata = data.frame(abil=0.59,motheduc=12.18,fatheduc=12.45))
# The predicted educ is 12.16         
