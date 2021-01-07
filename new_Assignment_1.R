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


summary(data_sample_1)
View(data_sample_1)

# take away sample 93 from the dataset
data_sample_removed <- data_sample_1 %>% 
  slice(-c(93))
summary(data_sample_removed)

# boxplot
boxplot = data_sample_removed %>% 
  ggplot() +
  aes(x = sex, y = pain) +
  geom_boxplot()

# violin
violin = data_sample_removed %>% 
  ggplot() +
  aes(x = sex, y = pain) +
  geom_violin()

boxplot
violin

model_1 <- lm(pain ~ sex + age, data = data_sample_removed)
summary(model_1)
model_1 %>% plot(which = 4)
model_1 %>% plot(which = 3)
model_1 %>% plot(which = 2)
model_1 %>% bptest()
model_1 %>% vif()


model_final <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_saliva, data = data_sample_removed)
summary(model_final)
model_final %>% plot(which = 3)
model_final %>% plot(which = 4)
model_final %>% bptest()
model_final %>% vif()

# comparing the two models
AIC(model_1)
AIC(model_final)

# comparing the two models
anova(model_1)
anova(model_final)

# comparing the confidense interval
confint(model_1)
confint(model_final)

table1 = coef_table(model_1)
table2 = coef_table(model_final)
view(table1)
view(table2)

anova(model_1)
anova(model_final)

