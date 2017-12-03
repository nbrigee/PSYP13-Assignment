# PSYP13-Assignment

library(psych) # for describe
library(lm.beta) # for lm.beta
library(dplyr)
library(gsheet)
library(car) # for scatter3d, residualPlots, vif, pairs.panels, ncvTest
library(ggplot2) # for ggplot
library(rgl)

error_plotter <- function(mod, col = "black"){
  mod_vars = as.character(mod$call[2])
  data = eval(parse(text = as.character(mod$call[3])))
  y = substr(mod_vars, 1, as.numeric(gregexpr(pattern ='~',mod_vars))-2)
  x = substr(mod_vars, as.numeric(gregexpr(pattern ='~',mod_vars))+2, nchar(mod_vars))
  
  data$pred = predict(mod)
  
  if(x == "1"){x = "response_ID"
  data$response_ID = 1:nrow(data)}
  
  plot(data[,y] ~ data[,x], ylab = y, xlab = x)
  abline(mod)
  
  for(i in 1:nrow(data)){
    clip(min(data[,x]), max(data[,x]), min(data[i,c(y,"pred")]), max(data[i,c(y,"pred")]))
    abline(v = data[i,x], lty = 2, col = col)
  }
  
}


paindata =
  read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/ho
me_sample_1.csv")

#exploring data
summary(paindata)
describe(paindata)
str(paindata)

# Removing the datapoints wich mess around my data, first we take a look at the levels of the varibales
#And also the scores
paindata$sex
levels(paindata$sex)
paindata$mindfulness < 1
paindata <- paindata[-c(15),]
paindata$sex <- factor(paindata$sex)


#We will replace the scores which are lower than 1 with the mean
mean_mindfulness <- mean(paindata$mindfulness)

paindata$mindfulness[paindata$mindfulness<1]=mean_mindfulness


paindata$mindfulness<1


#checking histograms for all the variables
windows()
hist(paindata$pain, breaks = 30)
hist(paindata$age, breaks = 30)
hist(paindata$STAI_trait, breaks = 30)
hist(paindata$pain_cat, breaks = 30)
hist(paindata$mindfulness, breaks = 30)
hist(paindata$cortisol_serum, breaks = 30)
hist(paindata$cortisol_saliva, breaks = 30)

# scatterplots
plot(pain ~ age, data = paindata)
plot(pain ~ sex, data = paindata)
plot(pain ~ STAI_trait, data = paindata)
plot(pain ~ pain_cat, data = paindata)
plot(pain ~ mindfulness, data = paindata)
plot(pain ~ cortisol_serum, data = paindata)
plot(pain ~ cortisol_saliva, data = paindata)

#Checking normality
qqnorm(paindata$age)
qqnorm(paindata$STAI_trait)
qqnorm(paindata$pain_cat)
qqnorm(paindata$mindfulness)
qqnorm(paindata$cortisol_serum)
qqnorm(paindata$cortisol_saliva)

 shapiro.test(paindata$mindfulness)
 shapiro.test(paindata$STAI_trait)
 shapiro.test(paindata$age)
 shapiro.test(paindata$pain_cat)
 shapiro.test(paindata$cortisol_serum)
 shapiro.test(paindata$cortisol_saliva)
 
 
 boxplot(paindata$mindfulness)
 boxplot(paindata$age)
 boxplot(paindata$STAI_trait)
 boxplot(paindata$pain_cat)
 boxplot(paindata$cortisol_serum)
 boxplot(paindata$cortisol_saliva)
 
 
 
#fit the first regression model which contain age and sex as predictors
mod1_age_sex = lm(pain ~ age + sex, data = paindata)
lm(pain ~ age + sex, data=paindata)
summary(mod1_age_sex)

#Checking for influential outliars on the first model
lev_pain =hat(model.matrix(mod1_age_sex))
plot(lev_pain)
lev_pain

paindata[lev_pain > .07,]

N= nrow(paindata)
mahad=(N-1)*(lev_pain-1 / N)

tail(sort(mahad),2)

order(mahad,decreasing=T)[c(5,4,3,2,1)]

#Excluding one outlier
paindata1=paindata[-c(69),]
model2=lm(formula=pain ~ age + sex, data=paindata1)
lev2_pain=hat(model.matrix(model2))
plot(lev2_pain)

paindata1[lev2_pain > .07,]

# plotting the scatterplots for each predictor separately
# with the simple regression regression lines
plot(pain ~ age, data = paindata1)
abline(lm(pain ~ age, data = paindata1))
plot(pain ~ sex, data = paindata1)
abline(lm(pain ~ sex, data = paindata1))



#Getting the test statistic values of the model
summary(model2)
lm.beta(model2)
AIC(model2)
model2=lm(pain ~ age + sex, data=paindata1)
plot(model2)

require(psych)
require(lsr)


#Getting the confidence interval levels
confint(model2)

#Comparing the model with the mean as a predictor for pain, it is not included in the report, I just checked the difference
reg_mean <- lm(pain ~ 1, data = paindata1)
error_plotter(reg_mean, col = "red")

AIC(reg_mean)
AIC(mod1_age_sex)


mod2_extended <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = paindata1)
mod2_extended

#Getting the standardized coefficients and p values
beta=lm.beta(model2)
beta2=lm.beta(mod2_extended)

#Getting the unstandardized values
summary(model2)
summary(mod2_extended)
#Gettin the standardised values
summary(beta)
summary(beta2)

#Getting the confience intervalls's level
confint(beta)
confint(beta2)
confint(mod2_extended)
confint(model2)

#-Cheking inluential outliers on my second model
lev_pain_mod2 =hat(model.matrix(mod2_extended))
plot(lev_pain_mod2)

paindata1[lev_pain_mod2 > .13,]

N= nrow(paindata1)
mahad=(N-1)*(lev_pain_mod2-1 / N)
tail(sort(mahad),7)

order(mahad,decreasing=T)[c(5,4,3,2,1)]


#Comparing the two models which is better
anova(model2, mod2_extended)
#Second model lower so it is better, so I will use that.
AIC(model2)
AIC(mod2_extended)

# checking for influential outliers
windows()
plot(pain ~ age,  data = paindata1)

windows()
plot(mod2_extended, which = 4) #how much different the data point from the other, and how much it is deviate from the trend
#higher then 1 can be an outlier, so in this case there is no need to remove anything
windows()
plot(mod2_extended, which = 5)

## checking assumptions

# normality assumption
# QQ plot
windows()
plot(mod2_extended, which = 2)
# skew and kurtosis
describe(residuals(mod2_extended))
#skew greater than 1 means deviate from normal distribution, I do not have such score
# histogram
windows()
hist(residuals(mod2_extended), breaks = 20)



# linearity assumption
# predicted values against actual values

windows()
plot( x = pred, y = mod2_extended ["pain"], xlab = "Fitted Values", ylab = "Observed Values")
# predicted values against residuals
plot(mod2_extended, which = 1)
# residual plot
residualPlots(mod2_extended)


# homoscedasticty assumption (homogeneity of variance)
plot(mod2_extended, which = 3)
ncvTest(mod2_extended) #there is no heteroscedasticity accordint to the output

# multicollinearity (VIF above 5)

vif(mod2_extended) #above 5, you should look at the correlation and exclude one of them
pairs.panels(paindata1, col = "red", lm = T)


#BEcasue the vif for the saliva and serum are above 5 which means they are highly correlated,
#we exclude the saliva from the model called model 3

model3 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data= paindata1)
model3
summary(model2)

#Model comparison
summary(model3)
anova( mod2_extended, model3)
AIC(mod2_extended)
AIC(model3)


#Backward regression

library(psych) # for describe
library(lm.beta) # for lm.beta
library(dplyr)
library(gsheet)
library(car) # for scatter3d, residualPlots, vif, pairs.panels, ncvTest
library(ggplot2) # for ggplot
library(rgl)
library(rlm)
names(paindata)

# model with potential predictors, with all can make an outcome
model4_with_weight_without_saliva  <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight, data = paindata)




# backward regression to identify the variables with the highest unique predictive value
model5_back = step(model4_with_weight_without_saliva, direction = "backward") #step function refers to the method it can backward or also both regression
summary(model4_with_weight_without_saliva)
summary(model5_back)

#Checking for influential outliers
lev_model5_back=hat(model.matrix(model5_back))
plot(lev_model5_back)
paindata[lev_model5_back > .10,]


N2= nrow(paindata)
mahad=(N2-1)*(lev_model5_back-1 / N2)
tail(sort(mahad),5)
order(mahad,decreasing=T)[c(5,4,3,2,1)]

#Excluding outliers
paindata2=paindata[-c(65, 24),]
model_4_after_correction=lm(formula=pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight, data=paindata2)
lev_model_4_after_correction=hat(model.matrix(model_4_after_correction))
plot(lev_model_4_after_correction)

#Final backward model and comparing it with theinitial one
model_backward = step(model_4_after_correction, direction = "backward")
summary(model_backward)
beta_backward=lm.beta(model_backward)
summary(beta_backward)

AIC(model_backward)
AIC(model_4_after_correction)
anova(model_backward, model_4_after_correction)

#Confidence interval of the backward model
confint(model_backward)
confint(beta_backward)


#Comparing models
model_theory_based=mod2_extended

model_theory_based <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = paindata2)
summary(model_theory_based)
summary(model_backward)

anova(model_theory_based, model_backward)
AIC(model_theory_based)
AIC(model_backward)

require(psych)
require(lsr)

#Now we use a new dataset to check our models if they fit on the new well, and if so which is better.

newdata= read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_2.csv")
#Exploring data

summary(newdata)
describe(newdata)

#Predict scores by the model
predict(model_theory_based, data=newdata)
predict(model_backward, data=newdata)


#test the performance of backward regression model and theory based model on the new dataset
pred_test <- predict(model_theory_based, newdata)
pred_test2 <- predict(model_backward, newdata)

#Comparing performnce based on the RSS and AIC
RSS_test_model_theory_based = sum((newdata[,"pain"] - pred_test)^2)
RSS_test_model_theory_based
RSS_test_model_backward = sum((newdata[,"pain"] - pred_test2)^2)
RSS_test_model_backward
AIC(model_theory_based)
AIC(model_backward)




# sum of squared differences between actual value and prediction
RSS = sum((newdata$pain - predict(model_theory_based))^2)
RSS
RSS_back = sum((newdata$pain - predict(model_backward))^2)
RSS_back


#Linear mixed effects

library(lme4) # for lmer
library(lmerTest)
library(psych) # for describe
library(car) # for residualPlots, vif, pairs.panels, ncvTest
library(ggplot2) # for ggplot
library(cAIC4) # for cAIC
library(r2glmm) # for r2beta
library(influence.ME) # for influence
library(lattice) # for qqmath
library(reshape2) # for melt function


#Repeated measures with mixed effects


#The new data file
pain_repeated=read.csv = ("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_3.csv")

# asign ID as factor
pain_repeated$ID = factor(pain_repeated$ID)
str(pain_repeated)

# varriables
names(pain_repeated)

# designate which are the repeated varibales
repeated_variables = c("pain1",	"pain2", "pain3",	"pain4")


#Exploring data
# descriptives
describe(pain_repeated)
summary(pain_repeated)


# histograms
hist(pain_repeated$pain1)
hist(pain_repeated$pain2)
hist(pain_repeated$pain3)
hist(pain_repeated$pain4)

hist(pain_repeated$age)
hist(pain_repeated$STAI_trait)
hist(pain_repeated$pain_cat)
hist(pain_repeated$cortisol_serum)
hist(pain_repeated$cortisol_saliva)
hist(pain_repeated$mindfulness)
hist(pain_repeated$weight)


#Normality test for the all the variables
shapiro.test(pain_repeated$pain1)
shapiro.test(pain_repeated$pain2)
shapiro.test(pain_repeated$pain3)
shapiro.test(pain_repeated$pain4)

shapiro.test(pain_repeated$age)
shapiro.test(pain_repeated$STAI_trait)
shapiro.test(pain_repeated$pain_cat)
shapiro.test(pain_repeated$cortisol_serum)
shapiro.test(pain_repeated$cortisol_saliva)
shapiro.test(pain_repeated$mindfulness)
shapiro.test(pain_repeated$weight)


# correlation of repeated variables
cor(pain_repeated[,repeated_variables])


#Tranformation to long format
# id.vars should be all non-repeated variables
pain_repeated_long = melt(pain_repeated, measure.vars=repeated_variables, variable.name = "time", value.name = "pain_rating") #melt function trasform the data to long istead of wide
# order data frame by participant ID in order to be easier interpretable
View(pain_repeated_long)

pain_repeated_long = pain_repeated_long[order(pain_repeated_long[,"ID"]),]
# I changed the time variable to a numerical vector
pain_repeated_long$time = as.numeric(pain_repeated_long$time)

# now the data looks like this
pain_repeated_long

#Building mixed effects models

mod_rep_int = lmer(pain_rating ~ time + age + sex + weight + STAI_trait + pain_cat + cortisol_serum + mindfulness + (1|ID), data = pain_repeated_long) # 1 refer to random int model (ID) is the cluster variable now
mod_rep_slope = lmer(pain_rating ~ time + age+ sex + weight + STAI_trait + pain_cat + cortisol_serum + mindfulness + (time|ID), data = pain_repeated_long)
summary(mod_rep_int)
summary(mod_rep_slope)
confint(mod_rep_int)


### model comparison to see whether to use random slope or random intercept models

pain_repeated_long$pred_int = predict(mod_rep_int)
pain_repeated_long$pred_slope = predict(mod_rep_slope)

# random intercept model
windows()
ggplot(pain_repeated_long, aes(y = pain_rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_int, x=time))+
  facet_wrap( ~ ID, ncol = 6)

# random slope and intercept model
windows()
ggplot(pain_repeated_long, aes(y = pain_rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_slope, x=time))+
  facet_wrap( ~ ID, ncol = 5)

# compare models with cAIC

cAIC(mod_rep_int)$caic
cAIC(mod_rep_slope)$caic

# compare models with anova

anova(mod_rep_int, mod_rep_slope)

#The random slope model looks much better.

# adding a quadratic term of time to the random slope model
# to account for curved relationship between time and rating pain

mod_rep_slope_quad = lmer(pain_rating ~ time +I(time^2) + age + sex + weight + STAI_trait + pain_cat + cortisol_serum + mindfulness + (time|ID), data = pain_repeated_long)
summary(mod_rep_slope_quad)
confint(mod_rep_slope_quad)
## plot the results
# save prediction of the model to new variable
pain_repeated_long$pred_slope_quad = predict(mod_rep_slope_quad)

# random slope model with quadratic term
windows()
ggplot(pain_repeated_long, aes(y = pain_rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_slope_quad, x=time))+
  facet_wrap( ~ ID, ncol = 5)
# this looks like a better fit than the others

# compare models with cAIC
cAIC(mod_rep_slope)$caic
cAIC(mod_rep_slope_quad)$caic

# compare models with anova
anova(mod_rep_slope, mod_rep_slope_quad)

# based on thee results it seems that the random slope model
# including the quadratic term of time would be the best model to choose


#Data and model diagnostics

# checking for influential outliers
influence(mod_rep_slope_quad, group = "ID")$alt.fixed
influence(mod_rep_slope_quad, obs = T)$alt.fixed 
#There is no need for exclusion

# checking assumptions
# normality assumption
# QQ plot
windows()
qqmath(mod_rep_slope_quad, id=0.05)

# linearity assumption
# linearity of prediction and standardized residuals
windows()
plot(mod_rep_slope_quad)
#linearity of each predictor and the standardized residual
# visualize the linearity of connection of each predictor
predictors=c("pain_rating")

for(i in 1:length(predictors)){
  predictor_to_test = pain_repeated_long[,predictors[i]]
  print(
    ggplot(data.frame(x = predictor_to_test,pearson=residuals(mod_rep_slope_quad,type="pearson")),
           aes(x=x,y=pearson)) +
      geom_point() +
      geom_smooth(method = 'loess') +
      theme_bw()
  )
}


# homoscedasticty assumption (homogeneity of variance)

windows()
plot(mod_rep_slope_quad)

summary(lm(residuals(mod_rep_slope_quad)^2 ~ pain_repeated_long[,"ID"]))

# multicollinearity

windows()
pairs.panels(pain_repeated, col = "red", lm = T)

#Standardized scores
r2beta(mod_rep_slope)
r2beta(mod_rep_slope_quad)





