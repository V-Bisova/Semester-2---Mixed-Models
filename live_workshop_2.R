
### Challenge 1 -----


#Loading packages ----

library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(performance)
library(pbkrtest)
library(visdat)
library(fitdistrplus)
library(arm)
library(dplyr)


#Importing data ----

factorial_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/16_mixed_models_pt2/master/data/factorial_data.csv")

str(factorial_data)


# Data Wrangling ----

tidied_factorial_data <- factorial_data %>% 
  mutate(Subject = factor(Subject), Item = factor(Item),
         Prime = factor(Prime), Target = factor(Target), Time = Time)

str(tidied_factorial_data)




# Summary Statistics ----

vis_miss(tidied_factorial_data)

tidied_factorial_data %>%
  filter(!is.na(Time)) %>% 
  group_by(Prime, Target) %>% 
  summarise(mean_RT = mean(Time), sd_RT = sd(Time))




# Data Visualisations ----

    #sorry, I focused on sorting which model fits best and did not get to the visualisation

# Setting contrasts ----

contrasts(tidied_factorial_data$Prime) <- matrix(c(.5, -.5))
contrasts(tidied_factorial_data$Target) <- matrix(c(.5, -.5))

# Building the mdoel ----

factorial_model <- lmer(Time ~ Prime * Target +
                          (1 + Prime * Target| Subject) +
                          (1 + Prime * Target | Item), 
                        data = tidied_factorial_data)

factorial_model_1 <- lmer(Time ~ Prime * Target +
                            (1 | Subject) +
                            (1 | Item), 
                          data = tidied_factorial_data)

check_model(factorial_model_1)





# Gamma model ----

missing_data_removed <- tidied_factorial_data %>%
  filter(!is.na(Time))

descdist(missing_data_removed$Time)


gamma_model <- glmer(Time ~ Prime * Target + 
                       (1 | Subject) +
                       (1 | Item), 
                     family = Gamma,
                     nAGQ = 0,
                     data = tidied_factorial_data)

gamma_model_2 <- glmer(Time ~ Prime * Target + 
                         (1 | Subject), 
                       family = Gamma,
                       data = tidied_factorial_data)

check_model(gamma_model)


# Compare models ----

summary(factorial_model_1)

summary(gamma_model)



# Post hoc ----

emmeans(gamma_model, pairwise ~ Prime*Target, adjust = "none")

emmeans(factorial_model_1, pairwise ~ Prime*Target, adjust = "none")



### CHALLENGE 2 ------

# Import data -----

accuracy_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/16_mixed_models_pt2/master/data/accuracy_data.csv")

accuracy_data


# Data wrangling ----

tidied_accuracy_data <- accuracy_data %>% 
  mutate(Subject = factor(Subject), FaceExpression = factor(FaceExpression),
         Face = factor(Face), Acc = Acc)


str(tidied_accuracy_data)





# Summary Statistics ----

tidied_accuracy_data %>% 
  group_by(FaceExpression) %>% 
  summarise(mean_Acc = mean(Acc), sd_Acc = sd(Acc))



# Building the model ----

binomial_model <- glmer(Acc ~ FaceExpression + # model failed to converge, need to simplify
                          (1 + FaceExpression | Subject) +
                          (1 + FaceExpression | Face),
                        data = tidied_accuracy_data, family = binomial)


binomial_model <- glmer(Acc ~ FaceExpression +
                          (1 + FaceExpression | Subject),
                        data = tidied_accuracy_data, family = binomial)

summary(binomial_model)

# Comparing Fixed effect vs no fixed effect ----

null_binomial_model <- glmer(Acc ~ (1 + FaceExpression | Subject), # random effect must be the same as the previous model
                             data = tidied_accuracy_data,
                             family = binomial)



anova(binomial_model, null_binomial_model)

# Post Hoc -----
emmeans(binomial_model, pairwise ~ FaceExpression) # higher accuracy for Sad than Happy faces


