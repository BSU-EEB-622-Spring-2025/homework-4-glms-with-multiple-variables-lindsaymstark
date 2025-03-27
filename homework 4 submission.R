## Homework 4 Submission ## 

library(pROC)
library(MLmetrics)
library(marginaleffects)
library(modelr)
library(tidyverse)
library(ggplot2)
library(MASS)
library(performance)
## Question 1:

mistletoe_dat <- read.csv("mistletoes.csv")
treemortality_dat <- read.csv("treemortality.csv")


## 1a)
mod.pois <- glm(Seedlings~Treatment, data=mistletoe_dat, family = poisson(link = "log"))
summary(mod.pois)
check_overdispersion(mod.pois)
#overdispersed so try nbinom
mod.nbin <-glm.nb(Seedlings~Treatment, data=mistletoe_dat)
summary(mod.nbin)
# Compute MAE for the best-fitting model (Poisson or Negative Binomial)
preds <- predictions(mod.nbin, data=mistletoe_dat$Treatment)
mae <- mean(abs(mistletoe_dat$Seedlings - preds$estimate))
mae

#I used a log link function so the predictions couldn't be negative.
#I could choose a poisson or nbinom because these deal with discrete data.
#I chose a negative binomial over a poisson because the data was overdispersed.
#The mae is 145.84. This error is high, but the data is highly variable, so to an extend this is to be expected.

## ASW: Totally agree! The fit is not the best, but also the number of seedlings ranges from 0 to >2000, so I think this is a great assessment!


##1b)
marginaleffects <- predictions(mod.nbin, newdata = datagrid(Treatment = unique(mistletoe_dat$Treatment)))

ggplot(marginaleffects, aes(x = Treatment, y = estimate)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  labs(title = "Predicted Number of Seedlings Beneath Trees",
       x = "Tree Treatment",
       y = "Predicted Number of Seedlings") +
  theme_minimal()                              


marginaleffects_preds <- avg_predictions(mod.nbin, by = "Treatment") #pulling average predicts for my model
effect_size <- marginaleffects_preds$estimate[1] / marginaleffects_preds$estimate[2]  #comparing average predictions for parasitized vs unparasitized

#There is a 95% decrease in the predicted number of seedlings under the tree if the tree is unparasitized.
summary(mod.nbin)
#From this model, it appears that mistletoe infection has a signficant effect on seedling density. Using the NBinom predictions, I see that unparasitized trees have 95% less seedlings.
#The marginal effects plot shows this too, with large differences in mean number of seedlings between parasitized and unparasitized trees.

## ASW: Excellent work! 


## 1c) 

mod.nbin2 <-glm.nb(Seedlings~Treatment*Year, data=mistletoe_dat)
summary(mod.nbin2)
mfx <- predictions(mod.nbin2, newdata = datagrid(Treatment = unique(mistletoe_dat$Treatment),Year = unique(mistletoe_dat$Year)))

      
# Plot the marginal effects
ggplot(mfx, aes(x = Treatment, y = estimate, color = factor(Year))) +
  geom_point(position = position_dodge(0.2), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(0.2)) +
  labs(title = "Predicted Seedling Density Beneath Trees",
       x = "Tree Treatment (Parasitized vs. Unparasitized)",
       y = "Predicted Seedling Density",
       color = "Survey Year") +
  theme_minimal()

marginaleffects_preds2 <- avg_predictions(mod.nbin2, by = c("Treatment", "Year"))

# Calculate effect size comparing parasitized vs. unparasitized for each Year
effect_size_2011 <- marginaleffects_preds2$estimate[marginaleffects_preds2$Treatment == "parasitized" & marginaleffects_preds2$Year == 2011] / 
  marginaleffects_preds2$estimate[marginaleffects_preds2$Treatment == "unparasitized" & marginaleffects_preds2$Year == 2011]

effect_size_2012 <- marginaleffects_preds2$estimate[marginaleffects_preds2$Treatment == "parasitized" & marginaleffects_preds2$Year == 2012] / 
  marginaleffects_preds2$estimate[marginaleffects_preds2$Treatment == "unparasitized" & marginaleffects_preds2$Year == 2012]

#In 2011, parasitized trees had a far greater effect on the number of seedlings underneath than in 2012.
#The high rain in 2012 could have contributed to increased seedling density, rather than parasitization.

## ASW: This is an excellent interpretation of the interaction! 30/30

## Question 2:

## 2a)
mod <- glm(mortality~thinning , 
           data=treemortality_dat, family="binomial"(link="logit"))
summary(mod)
plogis(0.9933-1.8559) - plogis(0.9933) #thinned minus not thinned probability of mortality

plot_predictions(mod, condition="thinning") + 
  ylab("chance of mortality") +
  xlab("0 = not thinned , 1 = thinned") + 
  theme_bw()


test_prob <- predict(mod, type = "response")
test_roc <- roc(treemortality_dat$mortality # Actual survival data
                ~ test_prob, plot = TRUE,  # Test probabilities
                print.auc = TRUE)
#Thinned trees are much less likely to die with a high amount of certainty (pvalue <0.05)
#Mortality decreases by 43% for thinned trees.
#The area under the receiver operater characteristic curve is 0.71. This means that 71% of the time, the model correctly predicts tree mortality.

## 2b)
#Although tree sizes are randomly sampled, the size of tree may not necessarily be randomly distributed.
#I think it would provide the most certainty if tree sizes are included in the glm.


## ASW:  if they completely randomized thinning treatments in relationship to tree size, it will not bias the estimate of thinning's effect. The models should estimate a consistent coefficient for thinning of, regardless of whether tree size is included. That said, the authors may want to include tree size for other reasons!

## 2c) 

## ASW: The interactions here aren't necessary to condition on roaddistance and slope (this model now communicates how the effect of thinnign varies by slope and road distance, which is why the "baseline" coefficient's pvalues got so high (it now represents the effect of thinning when slope = 0 and road dist = 0))... Additive terms (as I've adjusted here) should suffice for the research question and the concerns about confounding.

mod <- glm(mortality~thinning + slope + roaddist , 
           data=treemortality_dat, family="binomial"(link="logit"))
plot_predictions(mod, condition="slope") + 
  ylab("chance of mortality") +
  xlab("slope (deg)") + 
  theme_bw()
plot_predictions(mod, condition="roaddist") + 
  ylab("chance of mortality") +
  xlab("distance from road (km)") + 
  theme_bw()
plot_predictions(mod, condition="thinning") + 
  ylab("chance of mortality") +
  xlab("0 = not thinned , 1 = thinned") + 
  theme_bw()
summary(mod)
#The p-value for the thinning coefficient is now very high (0.641) which indicates a low degree of certainty about the effect of thinning on mortality.
#The interaction terms for thinning and slope and thinning and road distance are small with high p-values, which indicated that the interaction between slope or road distance and thinning is negligible and uncertain.
#Road distance and slope have large coefficients with significant p-values. It appears that greater distance from roads and slope are the greatest drivers of tree mortality.
#Slope increasing from 20-30 degrees is modeled to increase mortality by ~95%
#Distance from road increasing from 2.5-7.5 km increases mortality by ~50%
#The effect of thinning changes from negative to positive although it's magnitude is similar. The certainty of the effect also decreases significantly.
#It seems as though slope and distance from road were driving the "thinning" treatment. Therefore, the "thinning" treatment wasn't necessarily the predictor of survival. Instead, slope and distance from road were.


## ASW: See comment above, but for the model you fit, this is a great interpretation!  The key thing here is that slope and distance from roads are biasing the effect of thinning in the first model, making it appear more effective than it is because of the fact that thinning treatments are more likely to occur in locations where fire severity is already lower (closer to roads, on shallower slopes). The predicted effect of thinning in the first model is a decrease in mortality from 73% to 29%, but in the revised version of the second model, this effect decreases (Mortality decreases from 54% to 29%)

## 17/20

## ASW: nice work! 47/50