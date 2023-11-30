## Psych 1900 Final Project
## Paulina Piwowarczyk and Madison Saylor
## December 11th, 2019
## Brian Leahy

## ------------------------------- Load data -------------------------------

## load data

# packages (only as needed)
require(pwr)
if (!require(rstudioapi)) {install.packages("rstudioapi"); require(rstudioapi)}
if (!require(pastecs)) {install.packages("pastecs"); require("pastecs")}
if (!require(car)) {install.packages("car"); require(car)} ## For data preparation
if (!require(vcd)) {install.packages("vcd"); require(vcd)} ## For visualizations
if (!require(pastecs)) {install.packages("pastecs"); require(pastecs)}  ## Utility functions descriptives     
if (!require(pgirmess)) {install.packages("pgirmess"); require(pgirmess)}   ## Post-hoc tools for Kruskal-Wallis 
if (!require(effects)) {install.packages("effects"); require(effects)}
if (!require(pwr)) {install.packages("pwr"); require(pwr)}                       ## package for power calculation
if (!require(lm.beta)) {install.packages("lm.beta"); require(lm.beta)}           ## standardized regression coefficients
if (!require(vcd)) {install.packages("vcd"); require(vcd)}          ## for visualizing categorical data
if (!require(grDevices)) {install.packages("grDevices"); require(grDevices)}
require(ez)
require(foreign)
require(psych)

## install helpful plotting package
if (!require("ggstatsplot")) {install.packages("ggstatsplot");
  require("ggstatsplot")}   

## get path for current R script
current_path <- getActiveDocumentContext()$path

## set working directory to current path of R script
setwd(dirname(current_path))     

## load class survey data into a data frame
class_survey <- na.omit(read.csv("PSY1900_ClassSurvey.csv", header = TRUE, stringsAsFactors = FALSE))
head(class_survey)
dim(class_survey)
attach(class_survey)

## ------------------------------- Analysis 1: Correlation -------------------------------

## --- 1) Intro / background / motivation for research question:
## We believe that personal experience has an effect on the way we perceive the world. Personal experience with studying, 
## thus, should have an effect on how we think other people study. We tend to normalize our behavior and expect people to 
## behave similarly to us. For this reason, we believe that we will find a correlation between the number of hours per week 
## someone studies and the number of hours per week they think the average Harvard student studies.

## --- 2) Research question:
## Is there a correlation between the number of hours per week someone studies and 
## the number of hours per week they think the average Harvard student studies?

## --- 3) Hypotheses:
## H0: There is no correlation between the number of hours per week someone studies 
## and the number of hours per week they think the average Harvard student studies.
## H1: There is a correlation between the number of hours per week someone studies 
## and the number of hours per week they think the average Harvard student studies.

## --- 4) Variables involved, including scale level:
## Independent variable: The number of hours per week someone studies (metric)
## Dependent variable: The number of hours per week they think the average Harvard student studies (metric)

## --- 5) Data preparation and descriptives:

summary(class_survey$studyhours)
sd(class_survey$studyhours)
summary (class_survey$avgworkhours)
sd(class_survey$avgworkhours) 

sd_cutoff <- mean(class_survey$studyhours) + (2 * sd(class_survey$studyhours))
class_survey$studyhours[class_survey$studyhours > sd_cutoff]

dim(class_survey)
class_survey <- subset(class_survey, studyhours <= sd_cutoff)
dim(class_survey)

sd_cutoff <- mean(class_survey$avgworkhours) + (2 * sd(class_survey$avgworkhours))
class_survey$avgworkhours[class_survey$avgworkhours > sd_cutoff]

dim(class_survey)
class_survey <- subset(class_survey, avgworkhours <= sd_cutoff)
dim(class_survey) 

## --- 6) Plots:

## create a histogram displaying data of students' hours per week spent studying
## draw a red line to represent the mean and a blue line to represent the median
hist(class_survey$studyhours, main = "Hours per Week Spent Studying", xlab = "Hours per week"); abline(v = mean(class_survey$studyhours), col = "red");
abline(v = median(class_survey$studyhours), col = "blue")

## create a histogram displaying data of students' assumptions of other students' hours per week spent studying
## draw a red line to represent mean and blue line to represent median
hist(class_survey$avgworkhours, main = "Perceived Average Hours per Week Others Spend Studying", xlab = "Hours per week"); 
abline(v = mean(class_survey$avgworkhours), col = "red"); abline(v = median(class_survey$avgworkhours), col = "blue")

## create a box plot of the data for each variable
boxplot(class_survey$studyhours)

boxplot(class_survey$avgworkhours)

plot(class_survey$studyhours, class_survey$avgworkhours, xlab = "Hours per week spent studying", ylab = "Perceived hours per week others spend studying", las = 1,
     col = "red")
abline(lm(class_survey$avgworkhours ~ class_survey$studyhours), col = "blue", lw = 3)
plot(jitter(class_survey$studyhours) ~ jitter(class_survey$avgworkhours))
abline(lm(class_survey$avgworkhours ~ class_survey$studyhours), col = "blue", lw = 3)

## --- 7) Check assumptions (if appropriate):

str(class_survey$studyhours)
str(class_survey$avgworkhours)

stat.desc(class_survey$studyhours)
stat.desc(class_survey$avgworkhours)

qqnorm(class_survey$studyhours)
qqline(class_survey$studyhours)

qqnorm(class_survey$avgworkhours)
qqline(class_survey$avgworkhours)

ks.test(class_survey$studyhours, "pnorm", mean = mean(class_survey$studyhours), sd = sd(class_survey$studyhours)) ## Kolmogoroff-Smirnov test
## Null hypothesis: data is normally distributed
## Conclude that the data is normal because the p-value is greater than 0.05 (p-value = 0.1959)

ks.test(class_survey$avgworkhours, "pnorm", mean = mean(class_survey$avgworkhours), sd = sd(class_survey$avgworkhours)) ## Kolmogoroff-Smirnov test
## Null hypothesis: data is normally distributed
## Conclude that the data is not normal because the p-value is less than 0.05 (p-value = 0.03914)
## We will use the Spearman test because the data is not normally distributed

## --- 8) Fit model / run analysis:

cor.spearman <- cor.test(class_survey$studyhours, class_survey$avgworkhours, use = "complete.obs", method = "spearman")
cor.spearman
## Correlation of 0.7111236 and p-value < 2.2e-16 (therefore, significant and can reject the null hypothesis)

## ---- optimal sample size
pwr.r.test(r = 0.3, power = 0.8)          ## The optimal sample size is 85 people.

## --- 9) Check assumptions (if appropriate):

## --- 10) Decision and interpretation:
## We would make the decision to reject the null hypothesis, as our data shows that there is the possibility of a correlation between average hours studied and average predicted hours that other students study.
## The Spearman test shows a correlation of 0.7111236 The p-value found for the Spearman test is 2.2e-16, which is less than 0.05. Thus, we can reject our null hypothesis.
## The two variables were strongly correlated, r(110) = 0.71, p < 2.2e-16.

## ------------------------------- Analysis 2: Simple regression -------------------------------

## --- 1) Intro / background / motivation for research question:
## People who like to travel likely travel extensively both domestically and internationally. Thus, it would
## make sense for there to be an effect such that the number of US states someone has visited affects the number
## of countries someone has visited.

## --- 2) Research question:
## Does the number of US states a person has visited have an effect on 
## the number of countries they have visited?

## --- 3) Hypotheses:
## H0: The number of US states a person has visited does not have an effect on
## the number of countries they have visited.
## H1: The number of US states a person has visited has an effect on
## the number of countries they have visited.

## --- 4) Variables involved, including scale level:
## Independent variable: The number of US states a person has visited (metric)
## Dependent variable: The number of countries a person has visited (metric)

## --- 5) Data preparation and descriptives:
class_survey <- na.omit(read.csv("PSY1900_ClassSurvey.csv", header = TRUE, stringsAsFactors = FALSE))

summary(class_survey$travelUS)
sd(class_survey$travelUS)
summary (class_survey$travelcountry)
sd(class_survey$travelcountry) 

sd_cutoff <- mean(class_survey$travelUS) + (2 * sd(class_survey$travelUS))
class_survey$travelUS[class_survey$travelUS > sd_cutoff]

dim(class_survey)
class_survey <- subset(class_survey, travelUS <= sd_cutoff)
dim(class_survey)

sd_cutoff <- mean(class_survey$travelcountry) + (2 * sd(class_survey$travelcountry))
class_survey$travelcountry[class_survey$travelcountry > sd_cutoff]

dim(class_survey)
class_survey <- subset(class_survey, travelcountry <= sd_cutoff)
dim(class_survey) 

## --- 6) Plots:

## create a histogram displaying data of students' hours per week spent studying
## draw a red line to represent the mean and a blue line to represent the median
hist(class_survey$travelUS, main = "Number of US States Visited", xlab = "Number of states"); abline(v = mean(class_survey$travelUS), col = "red");
abline(v = median(class_survey$travelUS), col = "blue")

## create a histogram displaying data of students' assumptions of other students' hours per week spent studying
## draw a red line to represent mean and blue line to represent median
hist(class_survey$travelcountry, main = "Number of Countries Visited", xlab = "Number of countries"); 
abline(v = mean(class_survey$travelcountry), col = "red"); abline(v = median(class_survey$travelcountry), col = "blue")

## create a box plot of the data for each variable
boxplot(class_survey$travelUS)

boxplot(class_survey$travelcountry)

plot(class_survey$travelUS, class_survey$travelcountry, xlab = "Number of US States Visited", ylab = "Number of Countries Visited", las = 1,
     col = "red")
abline(lm(class_survey$travelcountry ~ class_survey$travelUS), col = "blue", lw = 3)
plot(jitter(class_survey$travelUS) ~ jitter(class_survey$travelcountry))
abline(lm(class_survey$travelcountry ~ class_survey$travelUS), col = "blue", lw = 3)

## --- 7) Check assumptions (if appropriate):

str(class_survey$travelUS)
str(class_survey$travelcountry)

stat.desc(class_survey$travelUS)
stat.desc(class_survey$travelcountry)

qqnorm(class_survey$travelUS)
qqline(class_survey$travelUS)

qqnorm(class_survey$travelcountry)
qqline(class_survey$travelcountry)

ks.test(class_survey$travelUS, "pnorm", mean = mean(class_survey$travelUS), sd = sd(class_survey$travelUS)) ## Kolmogoroff-Smirnov test
## Null hypothesis: data is normally distributed
## Conclude that the data is normal because p is greater than 0.05 (p-value = 0.2832)

ks.test(class_survey$travelcountry, "pnorm", mean = mean(class_survey$travelcountry), sd = sd(class_survey$travelcountry)) ## Kolmogoroff-Smirnov test
## Null hypothesis: data is normally distributed
## Conclude that the data is not normal because p < 0.05 (p-value = 0.002545)

## --- 8) Fit model / run analysis:

## regression analysis
surveyRegression <- lm(travelcountry ~ travelUS, data = class_survey)
surveyRegression
summary(surveyRegression) ## p-value is 0.1345, which is not significant

confint(surveyRegression)

cor(class_survey$travelUS, class_survey$travelcountry, use = "complete.obs") ## r = 0.1410119
cor(class_survey$travelUS, class_survey$travelcountry, use = "complete.obs")^2 ## R^2 = 0.01988434
summary(surveyRegression)$r.squared

round(summary(surveyRegression)$r.squared, 5) == round(cor(class_survey$travelUS, class_survey$travelcountry, use = "complete.obs")^2, 5) ## TRUE

## Cohen's f2
R2 <- summary(surveyRegression)$r.squared
R2
Cohenf2 <- R2/(1-R2)
Cohenf2  ## small effect size (0.02028775)               

## ---- power based on Cohen's F2
## degrees of freedom from the regression output (1 and 110)
## 'F-statistic: 116 on 1 and 110 DF'

pwr.f2.test(u = 1, v = 110, f2 = Cohenf2)   ## low power (power = 0.3209819)

## --- 9) Check assumptions (if appropriate):

residuals(surveyRegression)

resstd <- rstandard(surveyRegression)   ## Standardized residuals (z-scores: mean = 0, var = 1 -- approximately)   

hist(resstd)
mean(resstd) # The residuals are properly standardized (mean is close to 0)
var(resstd) # The residuals are properly standardized (variance is close to 1)

fitvals <- fitted(surveyRegression)          ## Fitted values
fitvals
plot(fitvals, resstd, pch = 19, xlab = "Fitted Values", ylab = "Standardized Residuals")
abline(h = 0, lty = 2, col = "darkgray")

hist(resstd, main = "Histogram Residuals", xlab = "Standardized Residuals", breaks = 15)
qqnorm(resstd, main = "Q-Q Plot Residuals") ## kind of linear
qqline(resstd)

## use the Kolmogoroff-Smirnov and Shapiro-Wilks tests to test residuals
## mean and sd arguments are (by default) 0 and 1
ks.test(resstd, "pnorm")      
shapiro.test(resstd) 
## p-values are less than 0.05, so the resstd are not normally distributed

## --- 10) Decision and interpretation:
## We cannot reject the null hypothesis that there is no effect of the number of US states visited on the number of countries visited.
## The p-value is 0.1345, which is not significant. Thus, we cannot interpret the R-squared value, the intercept (beta-0), or the slope parameter (beta-1).

## ------------------------------- Analysis 3: Multiple regression -------------------------------

## --- 1) Intro / background / motivation for research question:
## People who spend a lot of time watching TV and working out may spend less time studying because there are only 24 hours in a day.
## For this reason, we believe that we will find that the number of hours per week someone works out and/or the number of hours per week someone watches TV
## will have an effect on the number of hours per week that individual spends studying.

## --- 2) Research question:
## Is there an effect of the number of hours per week someone works out or an effect of the number of hours
## per week they watch TV on the number of hours per week they spend studying?

## --- 3) Hypotheses:
## H0(1): The number of hours per week someone works out does not have an effect on
## the number of hours per week they study.
## H1(1): The number of hours per week someone works out has an effect on
## the number of hours per week they study.

## H0(2): The number of hours per week someone watches TV does not have an effect on
## the number of hours per week they study.
## H1(2): The number of hours per week someone watches TV has an effect on
## the number of hours per week they study.

## --- 4) Variables involved, including scale level:
## Independent variables: The number of hours per week someone works out (metric); the number of hours per week someone watches TV (metric)
## Dependent variable: The number of hours per week someone studies (metric)

## --- 5) Data preparation and descriptives:
class_survey <- na.omit(read.csv("PSY1900_ClassSurvey.csv", header = TRUE, stringsAsFactors = FALSE))

summary(class_survey$workouthours)
sd(class_survey$workouthours)
summary(class_survey$tvhours)
sd(class_survey$tvhours)
summary(class_survey$studyhours)
sd(class_survey$studyhours)

sd_cutoff <- mean(class_survey$workouthours) + (2 * sd(class_survey$workouthours))
class_survey$workouthours[class_survey$workouthours > sd_cutoff]

dim(class_survey)
class_survey <- subset(class_survey, workouthours <= sd_cutoff)
dim(class_survey)

sd_cutoff <- mean(class_survey$tvhours) + (2 * sd(class_survey$tvhours))
class_survey$tvhours[class_survey$tvhours > sd_cutoff]

dim(class_survey)
class_survey <- subset(class_survey, tvhours <= sd_cutoff)
dim(class_survey)

sd_cutoff <- mean(class_survey$studyhours) + (2 * sd(class_survey$studyhours))
class_survey$studyhours[class_survey$studyhours > sd_cutoff]

dim(class_survey)
class_survey <- subset(class_survey, studyhours <= sd_cutoff)
dim(class_survey)

## --- 6) Plots:

hist(class_survey$workouthours)
hist(class_survey$tvhours)
hist(class_survey$studyhours)
scatterplotMatrix(~ workouthours + tvhours + studyhours, col = "black")

## --- 7) Check assumptions (if appropriate):

str(class_survey$workouthours) 
str(class_survey$tvhours)
str(class_survey$studyhours)

stat.desc(class_survey$workouthours)
stat.desc(class_survey$tvhours)
stat.desc(class_survey$studyhours)

qqnorm(class_survey$workouthours)
qqline(class_survey$workouthours)

qqnorm(class_survey$tvhours)
qqline(class_survey$tvhours)

qqnorm(class_survey$studyhours)
qqline(class_survey$studyhours)

ks.test(class_survey$workouthours, "pnorm", mean = mean(class_survey$workouthours), sd = sd(class_survey$workouthours)) ## Kolmogoroff-Smirnov test
## Null hypothesis: data is normally distributed
## Conclude that the data is normal because p > 0.05

ks.test(class_survey$tvhours, "pnorm", mean = mean(class_survey$tvhours), sd = sd(class_survey$tvhours)) ## Kolmogoroff-Smirnov test
## Null hypothesis: data is normally distributed
## Conclude that the data is not normal because p < 0.05

ks.test(class_survey$studyhours, "pnorm", mean = mean(class_survey$studyhours), sd = sd(class_survey$studyhours)) ## Kolmogoroff-Smirnov test
## Null hypothesis: data is normally distributed
## Conclude that the data is normal because p > 0.05
## We can assume that the data is normally distributed.

## --- 8) Fit model / run analysis:

regmodel <- lm(studyhours ~ workouthours + tvhours, data = class_survey)

summary(regmodel)  ## the p-value for workouthours is 0.824 and the p-value for tvhours is 0.452, so we cannot reject either null hypothesis.
plot(allEffects(regmodel))
confint(regmodel)

## --- 9) Check assumptions (if appropriate):

regmodel$residuals
mean(regmodel$residuals)
var(regmodel$residuals)

resstd <- rstandard(regmodel)
resstd
mean(resstd) # the residuals are properly standardized
var(resstd) # the residuals are properly standardized

fitvals <- fitted(regmodel)          ## Fitted (or predicted) values (on the regression plane)
fitvals
plot(fitvals, resstd, pch = 19, xlab = "Fitted Values", ylab = "Standardized Residuals")  ## Does not appear randomly distributed
abline(h = mean(resstd), lty = 2, col = "blue")

plot(fitvals, regmodel$residuals, pch = 19, xlab = "Hours Spent Studying", ylab = "Unstandardized Error")  ## Does not appear randomly distributed
abline(h = mean(regmodel$residuals), lty = 2, col = "darkgray")

hist(resstd, main = "Histogram Residuals", xlab = "Standardized Residuals", breaks = 15) ## appears somewhat normally distributed
qqnorm(resstd, main = "Q-Q Plot Residuals")
qqline(resstd)  ## appears somewhat normally distributed

ks.test(resstd, "pnorm")  ## p-value is greater than 0.05 (p-value = 0.4769), so the residuals are normally distributed
shapiro.test(resstd) ## p-value is greater than 0.05 (p-value = 0.1427), so the residuals are normally distributed

# Check for correlations between the error term and each predictor?
plot(class_survey$workouthours, regmodel$residuals, pch = 19, xlab = "Hours Spent Working Out", ylab = "Unstandardized Error")
abline(h = mean(regmodel$residuals), lty = 2, col = "blue")  ## There does not appear to be any correlation between the error term and workouthours
plot(class_survey$tvhours, regmodel$residuals, pch = 19, xlab = "Hours Spent Watching TV", ylab = "Unstandardized Error")
abline(h = mean(regmodel$residuals), lty = 2, col = "blue")  ## There does not appear to be any correlation between the error term and tvhours

## --- 10) Decision and interpretation:
## We cannot reject either null hypothesis (that there is no effect of time spent working out and that there is no effect of time spent watching TV).
## The p-value for the effect of workouthours on studyhours is 0.824, and the p-value for the effect of tvhours on studyhours is 0.452 Both p-values are above 0.05 and thus not significant.
## We cannot interpret the slope parameters or the R-squared. Also, the p-value for the F-statistic test is 0.744, so the slope parameters do not differ from 0.

## ------------------------------- Analysis 4: Chi-squared Test -------------------------------

## --- 1) Intro / background / motivation for research question:
## There is a stereotype that females do not like/are not interested in sports. We would like to see if 
## this is shown in our data when we look at how (and if) gender affects whether a student likes
## the Red Sox, the Yankees, or doesn't care.

## --- 2) Research question:
## Does a student's gender affect which baseball team they support?

## --- 3) Hypotheses:
## H0: The two variables (gender and baseball team supported) are independent from each other.
## H1: The two variables (gender and baseball team supported) are not independent from each other.

## --- 4) Variables involved, including scale level:
## A student's gender (categorical); the baseball team (if any) they support (categorical)

## --- 5) Data preparation and descriptives:
class_survey <- na.omit(read.csv("PSY1900_ClassSurvey.csv", header = TRUE, stringsAsFactors = FALSE))

summary(class_survey$gender)
str(class_survey$gender)  ## character
class_survey$gender <- factor(class_survey$gender)
class(class_survey$gender)

summary (class_survey$yankredsox)
str(class_survey$yankredsox)  ## character
class_survey$yankredsox <- factor(class_survey$yankredsox)
class(class_survey$yankredsox)

table(class_survey$gender)
table(class_survey$yankredsox)
table(class_survey$gender, class_survey$yankredsox)

cont.table <- table(class_survey$gender, class_survey$yankredsox)
cont.table ## each cell's absolute frequencies

##  total frequency
margin.table(cont.table)

## relative frequencies
prop.table(cont.table)
cont.table.p <- cont.table/sum(cont.table)
cont.table.p 

## --- 6) Plots:

barplot(cont.table, main = "Grouped Bar Chart", cex.names = 0.8, xlab = "Baseball Team", 
        legend.text = rownames(cont.table),
        beside = TRUE,
        args.legend = list(x = "right", title = "Gender"))

mosaicplot(~ class_survey$gender + class_survey$yankredsox, main = "Mosaic plot",
           xlab = "Gender", ylab = "Baseball Team", col = "white")

## --- 7) Check assumptions (if appropriate):

cont.table >= 5 ## It is not true that each cell in our location matrix is >= 5. (not true for other/prefer not to identify)
cont.table

## --- 8) Fit model / run analysis:

resChi <- chisq.test(cont.table)
resChi  ## p-value is 0.4662 (not significant)

resChi$expected
resChi$observed

resChi$residuals

## Mosaic plot with residual shading
mosaicplot(~ class_survey$gender + class_survey$yankredsox, main = "Mosaic plot",
           xlab = "Gender", ylab = "Baseball Team", shade = TRUE)
## p-value is not significant, so no shading

## effect size Cohen's w
ES.w2(cont.table.p)   ## small overall effect (0.1691666)

## --- optimal sample size
pwr.chisq.test(w = 0.30, df = 2, power = 0.8) 
## We need 107 subjects

## --- 9) Check assumptions (if appropriate):

## --- 10) Decision and interpretation:
## We cannot reject the null hypothesis that there is no effect of a student's gender on the baseball team a student supports.
## The p-value is 0.4662,which is above 0.05 and thus not significant. However, it is important to note that 
## the expected cell frequencies were less than 5 in the "other/prefer not to answer" category, which likely affected the p-value because the assumption was violated.



## ------------------------------- Analysis 5:  t-test vs. U test -------------------------------

## --- 1) Intro / background / motivation for research question:
#   Many people have expressed that they dislike texting and communicating through text
#   messages as opposed to iMessages. To see if this has an effect on whether friendships 
#   are maintained, we would like to see if iPhone users indicate that they have more close
#   friends than Android users.

## --- 2) Research question:
##	Is there a difference in terms of an individual’s amount of close friends depending on
##       whether the person has an iPhone or an Android?

## --- 3) Hypotheses:
## H0: The mean number of close friends does not differ across phone types.
## H1: The mean number of close friends does differ across across phone types.

## --- 4) Variables involved, including scale level and whether IV or DV:
## -) DV: Number of close friends: metric (0-infinity) 
## -) IV: Type of Phone: categorical ----- vs. categorical (2 categories: iPhone/Android)


## --- 5) Data preparation and descriptives:

summary(class_survey$phone)
str(class_survey$phone)  
phone <- factor(class_survey$phone)
class(class_survey$phone)

summary (class_survey$friends)
str(class_survey$friends)  

## convert metric data to numeric data type
friends <- as.numeric(class_survey$friends)    
class(class_survey$friends)

table(class_survey$phone)
table(class_survey$friends)
table(class_survey$phone, class_survey$friends)

cont.table.phone.friends <- table(class_survey$phone, class_survey$friends)
cont.table.phone.friends ## each cell's absolute frequencies

## create function to compute the geometric mean
gmeans.friends.phone <- tapply(friends, phone, mean)        


## --- 6) Plots:

## create a box plot of the data

## boxplot
boxplot(friends ~ phone, main = expression("Number of Friends"), ylab =
          "Friends", col = "bisque", data = class_survey)
points(gmeans.friends.phone, pch = 19, col = "red", type = "b", cex = 0.9)


## histograms and Q-Q-plots
op <- par(mfrow = c(2,2))
with(class_survey, hist(friends[phone == "Android"], main = "Android User Friends", xlab = "Number
of Friends"))
with(class_survey, qqnorm(friends[phone== "Android"], main = "Q-Q Plot Android"))
with(class_survey, qqline(friends[phone == "Android"]))
with(class_survey, hist(friends[phone == "iPhone"], main = "iPhone User Friends", xlab =
                          "Number of Friends"))
with(class_survey, qqnorm(friends[phone == "iPhone"], main = "Q-Q Plot iPhone"))
with(class_survey, qqline(friends[phone == "iPhone"]))
par(op)


## --- 7) Check assumptions (if appropriate):

## ## KS-test and SW-tests
## Android
## KS Test
with(class_survey, ks.test(friends[phone == "Android"], "pnorm", mean = mean(friends[phone == "Android"]), sd = sd(friends[phone == "Android"])))

## Shapiro-Wilks test
with(class_survey, shapiro.test(friends[phone == "Android"]))
## p-value = 0.3254	

## iPhone
## KS Test
with(class_survey, ks.test(friends[phone == "iPhone"], "pnorm", mean = mean(friends[phone == "iPhone"]), sd = sd(friends[phone == "iPhone"])))


## Shapiro-Wilks test
with(class_survey, shapiro.test(friends[phone == "iPhone"]))
## p-value = 3.539e-13

## ---------- effect size 
cohen.d(friends ~ phone)   ## Cohen's d shows a medium effect (sign doesn't matter)
## Cohen's d estimate: -0.7391405, medium effect size


## --- 8) Fit model / run analysis:

rest <- t.test(friends ~ phone, data = class_survey)
rest

## sig difference in the group means
## t = -6.8171, df = 44.603, p-value = 1.986e-08

## --- 9) Check assumptions (if appropriate):

## U-test to check assumptions
resw <- wilcox.test(friends ~ phone, data = class_survey)
resw

## p-value = 0.01354
## roughly: sig difference in the group medians


## --- 10) Decision and interpretation:
## We would reject the null hypothesis H0 that there is no effect of a student's phone on the
## number of close friends that they have.
## The p-value is .986e-08,which is less than 0.05 and thus is significant. 



## ----------Analysis 6:  one-way F-test (ANOVA) vs. Kruskal-Wallis ANOVA + sample size
##  estimate (for one-way) -------------------------------

## --- 1) Intro / background / motivation for research question:
#   Within society, it can often be stereotyped that men more so spend time working out, building 
#   muscle, and caring about their physical fitness. Because of this, we want to see if the amount
#   of time that an individual spends working out can be correlated with their gender identity.

## --- 2) Research question:
## Is there a relationship between an individual’s gender and the amount of time that they
## spend working out?

## --- 3) Hypotheses:
## H0: The means do not differ across groups (μ1=μ2=...=μr).
## H1: The means differ across groups (μ1,μ2,...,μr).

## --- 4) Variables involved, including scale level:
## -) DV: Hours spent Working Out : metric (0-168)
## -) IV: Gender identity : categorical ----- vs. categorical (3 categories:
##     male/female/other)


## --- 5) Data preparation and descriptives:

summary(class_survey$gender)
str(class_survey$phone)  
gender <- factor(class_survey$gender)
class(class_survey$gender)

summary (class_survey$workouthours)
str(class_survey$workouthours)  

## convert metric data to numeric data type
workouthours <- as.numeric(class_survey$friends)    
class(class_survey$workouthours)

table(gender)
table(workouthours)
table(gender, workouthours)

cont.table.gender.workouthours <- table(gender, workouthours)
cont.table.gender.workouthours ## each cell's absolute frequencies



## --- 6) Plots:

## Box plot
plot(gender, workouthours, ylab = "Hours Worked Out per Week", xlab = "Gender")

## define gmeans for gender and workout hours
gmeans.gender.workouthours <- tapply(workouthours, gender, mean)

## add group means to the plot
points(gmeans.gender.workouthours, pch = 19, col = "red", type = "b", cex = 0.9)   



## --- 7) Check assumptions (if appropriate):

##--- 1) Metric DV
## Is our response really metric? --> yes, it is a numerical measure of number of hours from 0 to 168 (which is
## the total number of hours in a week

##--- 2) Independence of observations within and across factors→ yes

##--- 3) Check for Normality

by(workouthours, gender, mean, norm = TRUE)


op <- par(mfrow = c(3,1))
hist(workouthours[gender == "Male"], main = "Histogram Male Group", xlab = "Hours Worked Out")
hist(workouthours[gender == "Female"], main = "Histogram Female Group", xlab = "Hours Worked Out")
hist(workouthours[gender == "Other/Prefer not to identify"],
     main = "Histogram Other/Prefer not to identify Group", xlab = "Hours Worked Out")

par(op)
op <- par(mfrow = c(1,3))
qqnorm(workouthours[gender == "Male"], main = "Q-Q Plot Male")
qqline(workouthours[gender == "Male"])
qqnorm(workouthours[gender == "Female"], main = "Q-Q Plot Female")
qqline(workouthours[gender == "Female"])
qqnorm(workouthours[gender == "Other/Prefer not to identify"], main = "Q-Q Plot Other/Prefer not to identify")
qqline(workouthours[gender == "Other/Prefer not to identify"])
par(op)


## KS and SW-test
ks.test(workouthours[gender == "Male"], "pnorm", mean = mean(workouthours[gender == "Male"]),
        sd = sd(workouthours[gender == "Male"]))

shapiro.test(workouthours[gender == "Male"])
## p-value = 8.813e-06


ks.test(workouthours[gender == "Female"], "pnorm", 
        mean = mean(workouthours[gender == "Female"]), sd =
          sd(workouthours[gender == "Female"]))
## p-value = 8.895e-12

shapiro.test(workouthours[gender == "Female"])

ks.test(workouthours[gender == "Other/Prefer not to identify"], 
        "pnorm", mean = mean(workouthours[gender == "Other/Prefer not to identify"]), sd =
          sd(workouthours[gender == "Other/Prefer not to identify"]))

## The KS test gives us a p-value of 1, but the Shapiro-Wilks test cannot be used for this data.
##shapiro.test(workouthours[gender == "Other/Prefer not to identify"])


#--- 4) variance homogeneity (homoscedasticity)
leveneTest(workouthours ~ gender)    ## H0: equal variances --> can't reject --> variances

##  Df = 2, F value = 0.2693,  Pr(>F) = 0.7644


## p-value is not less than 0.05, thus is it not significant


## --- 8) Fit model / run analysis:

fitaov <- aov(workouthours ~ gender)
summary(fitaov)

## The p-value is 0.708, which is not significant, as it is not less than 0.05.

## --- 9) Check assumptions (if appropriate):

## We could have used Welch’s F-test if the p-value of the Levene test had been significant, but
## the p-value was greater than 0.05→ thus it was not significant.

## --- 10) Decision and interpretation:
## We cannot reject H0. We cannot prove that gender and hours worked out per week have a
## relationships as there was not significant data to conclude that the two variables differ across means. 

## --- optimal sample size
## We want to detect medium effects (f2 = 0.15) and have a power of 0.8.
pwr.f2.test(u = 2, f2 = 0.15, power = 0.8)  ## we need 67 subjects in total




## ----------Analysis 7:  Factorial ANOVA  -------------------------------

## --- 1) Intro / background / motivation for research question:
#  We would also like to see if gender and the phone that an individual uses has an influence on #   the amount of close friends that a person has.

## --- 2) Research question:
## Do phone and gender influence the amount of close friends that an individual has?

## --- 3) Hypotheses:
## H0: Main effect A: Phone : the means of Y differ across the levels of phone type (H0: no mean differences across A).
## H1: Main effect B: Gender: the means of Y differ across the levels of gender (H0: no mean differences across B).
## H2: Interaction effect: the means of Y differ across combinations of gender and phone (H0: no mean differences across A×B combinations, or, simply, no interaction.).

## --- 4) Variables involved, including scale level:
## -) DV: The Amount of Close Friends that a Person Has : metric (0+)
## -) IV:  Phone : categorical ----- vs. categorical (2 categories:
##     iPhone/Android/ I don’t care)
## -) IV: Gender identity : categorical ----- vs. categorical (3 categories:
##     male/female/other)


## --- 5) Data preparation and descriptives:

summary(gender)
class(gender)

summary(phone)
class(phone)

summary(friends)
class(friends)

table(gender)
table(phone)
table(friends)
table(gender, phone, friends)

cont.table.gender.phone.friends <- table(gender, phone, friends)

## each cell's absolute frequencies
cont.table.gender.phone.friends 


## --- 6) Plots:

## box plots across all factor level combinations
par(mar=c(1,1,1,1))
boxplot(friends ~ phone + gender, names = c("Android/male", "iPhone/male", "Android/female", "iPhone/female", "Android/Other/Prefer not to identify", "iPhone/Other/Prefer not to identify"), main = "Number of Close Friends", cex.axis = 0.8, data = class_survey, col = "bisque") 



## interaction plot
dev.new()
op <- par(mfrow = c(1,2))

interaction.plot(gender, phone, friends, ylab = "Number of Close Friends", type = "b", pch = 19, lty = 1, col = 1:3, main = "Interaction Plot Phones", legend = FALSE, xlab = "Gender")  

legend("right", legend = c("Android", "iPhone"), col = 1:2, lty = 1, cex = 0.8)

interaction.plot(phone, gender, friends, ylab = "Number of Close Friends", type = "b", pch = 19, lty = 1, col = 1:2, main = "Interaction Plot Gender", legend = FALSE, xlab = "Phone") 

legend("topright", legend = c("Male", "Female", "Other/Prefer not to identify"), col = 1:2, lty = 1, cex = 0.8)

par(op)

## --- 7) Check assumptions (if appropriate):

## 1) normality within each group 

dev.new()

op <- par(mfrow = c(4,2))

## male + Android
maleAndroid <- subset(class_survey, subset = ((gender == "Male") &
                                                (phone == "Android")))

hist(maleAndroid$friends, main = "Histogram M/Android", xlab = "Friends")
qqnorm(maleAndroid$friends)

qqline(maleAndroid$friends)

## female + Android
femaleAndroid <- subset(class_survey, subset = ((gender == "Female") &
                                                  (phone == "Android")))

hist(femaleAndroid$friends, main = "Histogram F/Android", xlab = "Friends")
qqnorm(femaleAndroid$friends)

qqline(femaleAndroid$friends)

## Other/Prefer not to identify + Android
OtherAndroid <- subset(class_survey, subset = ((gender == "Other/Prefer not to identify") &
                                                 (phone == "Android")))

## The histogram cannot exist as there is no data in which an individual that identified with an “Other/Prefer
## not to identify” gender also had an “Android” phone; thus, no data exists in our dataset.
## hist(OtherAndroid$friends, main = "Histogram O/Android", xlab = "Friends")
## qqnorm(OtherAndroid$friends)

## qqline(OtherAndroid$friends)


## male + iPhone
maleiPhone <- subset(class_survey, subset = ((gender == "Male") &
                                               (phone == "iPhone")))

hist(maleiPhone$friends, main = "Histogram M/iPhone", xlab = "Friends")
qqnorm(maleiPhone$friends)

qqline(maleiPhone$friends)


## female + iPhone
femaleiPhone <- subset(class_survey, subset = ((gender == "Female") &
                                                 (phone == "iPhone")))

hist(femaleiPhone$friends, main = "Histogram F/iPhone", xlab = "Friends")
qqnorm(femaleiPhone$friends)

qqline(femaleiPhone$friends)

## Other/prefer not to identify + iPhone
OtheriPhone <- subset(class_survey, subset = ((gender == "Other/Prefer not to identify") &
                                                (phone == "iPhone")))

hist(OtheriPhone$friends, main = "Histogram O/iPhone", xlab = "Friends")
qqnorm(OtheriPhone$friends)

qqline(OtheriPhone$friends)



## 2) variance homogeneity: Levene Test

leveneTest(friends, interaction(gender, phone))

# p-value = 0.5755, thus it is insignificant as p < 0.05


## --- 8) Fit model / run analysis:

fitaov2 <- aov(friends ~ phone*gender, data = class_survey) 
summary(fitaov2)

## not a significant interaction --> all p-values > 0.05


## --- 9) Check assumptions (if appropriate):

## all pairwise post hoc tests
TukeyHSD(fitaov2)   ## all pairwise post hoc tests

## --- 10) Decision and interpretation:
## We cannot reject the null hypotheses as the p-value is greater than 0.05 and thus does not disprove a relationship. We cannot conclude that phone and gender do not have an effect on each other in the number of close friends that an individual has.
