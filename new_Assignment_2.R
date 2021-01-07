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

data_sample_no_outliners = data_sample_1 %>% 
slice(-c(43,99,127,93))

# theory based model
model_no_ouliners = lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_saliva, data = data_sample_no_outliners)
summary(model_no_ouliners)

# backward model
model_back <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_saliva, data = data_sample_no_outliners)
summary(model_back)
model_back %>% plot(which = 3)
model_back %>% plot(which = 4)
model_back %>% bptest()
model_back %>% vif()

model_backregression = step(model_back, direction = "backward")
summary(model_backregression)

#compairing the two models
AIC(model_no_ouliners)
AIC(model_backregression)

# comparing the two models
anova(model_no_ouliners, model_backregression)

# comparing the confidense interval
confint(model_no_ouliners)
confint(model_backregression)

table1 = coef_table(model_no_ouliners)
table2 = coef_table(model_backregression)
view(table1)
view(table2)

data_sample_prediction = read.csv("https://tinyurl.com/ha-dataset2")

test_set <-data_sample_prediction[1:100, ]
predicting_pain <- predict(model_no_ouliners, test_set)
predicting_pain_backwardsregression <- predict(model_backregression, test_set)

RSS_test = sum((test_set[, "pain"] - predicting_pain)^2)
RSS_test_back = sum((test_set[, "pain"] - predicting_pain_backwardsregression)^2)
RSS_test
RSS_test_back



