data_sample_3 = read.csv("https://tinyurl.com/ha-dataset3") # Get the third dataset we need
data_sample_4 = read.csv("https://tinyurl.com/ha-dataset4") # Get the fourth dataset we need

View(data_sample_3) # viewing the data 
View(data_sample_4) # viewing the data 

library(psych)
library(tidyverse)
library(cAIC4)
library(r2glmm)
library(lme4)
library(lmerTest)
library(MuMIn)
library(optimx)

# custom function 
stdCoef.merMod <-function(object) {
  sdy <-sd(getME(object,"y"))
  sdx <-apply(getME(object,"X"), 2, sd)
  sc <-fixef(object)*sdx/sdy
  se.fixef <-coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}

# asign ID and location as factors
data_sample_3 = data_sample_3 %>% 
  mutate(ID = factor(ID))
         
data_sample_4 = data_sample_4 %>% 
  mutate(class = factor(ID))


# descriptives
describe(data_sample_3)
table(data_sample_3$location)


data_sample_3 %>% 
  ggplot() +
  aes(x = pain, 
      sex) +
  geom_point(aes(colour = class), size = 4) +
  geom_smooth(method = "lm", se = F, fullrange = TRUE)
         
         