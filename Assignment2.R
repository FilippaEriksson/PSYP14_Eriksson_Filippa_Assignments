data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1") # Get the data we need
data_sample_2 = read_csv("https://tinyurl.com/ha-dataset2") # Get the new data we need

View(data_sample_1) # viewing the data 
View(data_sample_2) #viewing the data


library(tidyverse) # for tidy format, ggplot2, dplyr
library(psych) # for describe
library(gridExtra) # for grid.arrange
library(car) # for cooks distance
library(lm.beta)
library(lmtest)
library(sandwich)
library(boot)
library(dplyr)

# Custom function by Zoltan
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

#Assignment 2

model_2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_saliva, data = data_sample_1)

model_sum_nooutliners = data_sample_1 %>% 
  slice(-c(100,114,26))

model_2_slice = lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_saliva, data = model_sum_nooutliners)

model_sum_nooutliners = model_sum_nooutliners %>% 
  mutate(pain_transformed = log(pain))


# theory-based model
model_2_transformed = lm(pain_transformed ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_saliva, data = model_sum_nooutliners)


# backward model
model_new = lm(pain_transformed ~ sex + age + STAI_trait + pain_cat + mindfulness + weight + IQ + household_income, data = model_sum_nooutliners)


# backward regression
model_new_back = step(model_new, direction = "backward")

# comparing the prediction performance of the final model returned by backward  regression (comparing my final model (model_transformed) with her new model)
anova(model_new_back, model_2_transformed)

summary(model_2_transformed)$adj.r.squared
summary(model_new_back)$adj.r.squared

AIC(model_2_transformed) # the AIC is lower in model_transformed, and there for better, than in model_new_back (the model called backward model)
AIC(model_new_back)

# theory-based model
model_2_transformed = lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_saliva, data = data_sample_2)

# backward model
model_new = lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + weight + IQ + household_income, data = data_sample_2)



# compairing theory-based model and backward model which the new data
AIC(model_2_transformed)
AIC(model_new)
anova(model_2_transformed, model_new)

anova(model_new_back)



