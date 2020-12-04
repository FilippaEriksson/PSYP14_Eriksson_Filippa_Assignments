data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1") # Get the data we need
View(data_sample_1) # viewing the data 


library(tidyverse) # for tidy format, ggplot2, dplyr
library(psych) # for describe
library(gridExtra) # for grid.arrange
library(car) # for cooks distance
library(lm.beta)
library(lmtest)
library(sandwich)
library(boot)
library(dplyr)

# custom function by Zoltan
coef_table = function(model) {
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,
                                                             4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values !=
                     "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" &
                                                      mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values !=
                                                                                                            "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model),
                                                  confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])),
                                            2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta",
                           "p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  return(mod_sum_table)
}



model_1 <- lm(pain ~ sex + age, data = data_sample_1)
model_2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_saliva, data = data_sample_1)

summary(model_1)$adj.r.squared
summary(model_2)$adj.r.squared

AIC(model_1)
AIC(model_2)

anova(model_1)
anova(model_2)

# Cooks distance
plot(cooks.distance(lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_saliva, data = data_sample_1)))

#QQ
model_2 %>% 
  plot(which = 2)

#histogram
model_sum = enframe(residuals(model_2))
model_sum %>% 
  ggplot() +
  aes(x = value) +
  geom_histogram()
  
#skew and kurtosis
describe(residuals(model_2))

# take out the outliners
model_sum_nooutliners = data_sample_1 %>% 
  slice(-c(100,114,26))
model_2_slice = lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_saliva, data = model_sum_nooutliners)

# recheck the assumption of normality of residuals
describe(residuals(model_3))

#histogram  without outliners
model_sum_2 = enframe(residuals(model_2_slice))
model_sum_2 %>% 
  ggplot() +
  aes(x = value) +
  geom_histogram()

#comparing the two models with and without the outliners
summary(model_1)
summary(model_2_slice)

#regular confidence intervals for the model coefficients
confint(model_2_slice)

#bootstrapped confidence intervals for the model
# coefficient 
confint.boot(model_2_slice)


# regular adjusted R squared
summary(model_2_slice)$adj.r.squared

# bootstrapping with 1000 replications
results.boot <- boot(data = data_sample_1, statistic = adjR2_to_boot,
                     R = 100, model = model_3)


# get 95% confidence intervals for the adjusted R^2
boot.ci(results.boot, type = "bca", index = 1)

# explore linearity # all the tests are non significant.  roughly flat lines if the assumption of linearity holds true
model_2_slice %>% 
  residualPlots()

# explore homoscedasticty # the result show a routhgly equal variation at all values of the predicted values. We can se a cigar shaped line and this is considered accapteble
model_2_slice %>% 
  plot(which = 3)

# NCV Test #  indicate a violation of the assumption ofhomoscedasticity, so it would mean that there is significant heteroscedasticity
model_2_slice %>% 
  ncvTest()

# Breush-Pagan test 
# indicate a violation of the assumption of homoscedasticity, so it would mean that there is significant heteroscedasticity. This means that the model is less effective at predicting data but we can still use it. Interpreting the individual coefficients and our confidence in their addedpredictive value is no longer possible. 
model_2_slice %>% 
  bptest()

# Tranformation Note: we don´t use the 2:nd option because our samples is relatively small.
model_sum_nooutliners = model_sum_nooutliners %>% 
  mutate(pain_transformed = log(pain))


model_2_transformed = lm(pain_transformed ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_saliva, data = model_sum_nooutliners)

model_2_transformed

model_2_transformed %>% 
  plot(which = 3)

# NCV Test # the result is significant
model_2_transformed %>% 
  ncvTest()

# Breush-Pagan test # the result is significant
model_2_transformed %>% 
  bptest()

# explore multicollinearity # not over 3. There is no multicollinearity
model_2_slice %>% 
  vif()



