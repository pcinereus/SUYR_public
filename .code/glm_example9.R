## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE)


## ----libraries, results='markdown', eval=TRUE, message=FALSE, warning=FALSE----
library(car)       #for regression diagnostics
library(broom)     #for tidy output
library(ggfortify) #for model diagnostics
library(sjPlot)    #for outputs
library(knitr)     #for kable
library(effects)   #for partial effects plots
library(DHARMa)    #for residual diagnostics
library(emmeans)   #for estimating marginal means
library(ggeffects) #for diagnostic plots in ggplotjk pR
library(MASS)      #for glm.nb
library(MuMIn)     #for AICc
library(pscl)      #for zero inflated models
library(glmmTMB)
library(nlme)
library(tidyverse) #for data wrangling
library(modelr)    #for auxillary modelling functions


## ----readData, results='markdown', eval=TRUE----------------------------------
fish = read_csv('../data/fish1.csv', trim_ws=TRUE)
glimpse(fish)


## ----eda, results='markdown', eval=TRUE, hidden=TRUE--------------------------
fish <- fish %>%
  mutate(CAMPER = factor(CAMPER))

ggplot(fish, aes(y=COUNT, x= CAMPER)) +
  geom_boxplot() +
  geom_point()
ggplot(fish, aes(y=COUNT, x= CAMPER)) +
  geom_boxplot() +
  geom_point()+
  scale_y_continuous(trans=scales::pseudo_log_trans())

ggplot(fish, aes(y=COUNT, x= CHILDREN, color=CAMPER)) +
  geom_point(position='jitter') +
  scale_y_continuous(trans=scales::pseudo_log_trans())

 

fish <- fish %>% mutate(CHILDREN=factor(CHILDREN))

ggplot(fish, aes(y=COUNT, x= CHILDREN)) + geom_boxplot() +
  scale_y_continuous(trans=scales::pseudo_log_trans())

ggplot(fish, aes(y=COUNT, x= CHILDREN, color=CAMPER)) + geom_boxplot() +
  scale_y_continuous(trans=scales::pseudo_log_trans())

ggplot(fish,  aes(x=COUNT)) + geom_bar() +
  scale_x_continuous(trans=scales::pseudo_log_trans()) +
  facet_wrap(~PERSONS,  scales='free')


## ----fitModel, hidden=TRUE----------------------------------------------------
fish.glm <- glm(COUNT ~ CHILDREN * CAMPER,  data=fish,
                family=poisson(link='log'))


## ----ValidateModel, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
fish.resids <- simulateResiduals(fish.glm, integerResponse = TRUE)
plot(fish.resids)
testZeroInflation(fish.resids)


## ----ValidateModel2, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
library(pscl)
fish.glm2 <- zeroinfl(COUNT ~ CHILDREN * CAMPER | PERSONS,
                      data=fish,  dist='poisson')
#fish.resids <- simulateResiduals(fish.glm2, integerResponse = TRUE)
fish.glm3 <- zeroinfl(COUNT ~ CHILDREN * CAMPER | PERSONS,
                      data=fish,  dist='negbin')

fish.glm4 <- zeroinfl(COUNT ~ CHILDREN + CAMPER | PERSONS,
                      data=fish,  dist='negbin')
AICc(fish.glm2, fish.glm3, fish.glm4)


## ----partialplots, results='markdown', eval=TRUE, hidden=TRUE-----------------
ggemmeans(fish.glm4,  ~CHILDREN|CAMPER) %>% plot
plot_model(fish.glm4,  transform='I')

summary(fish.glm4)
                                        #tidy(fish.glm3,  exponentiate=TRUE)
exp(1.21)
exp(1.21+0.96)
exp(1.21-3.43)

exp(1.47)
exp(-1.52)
## when there is only one person in the group,
## - zeros are 4.3 time more likely to be false than true.
## - zeros are 'false' because the person did not go fishing (or lied)
## - hence it is 4.3 times more likely that a 0 is because they did not go fishing than they did go fishing.
## The more people in the group,  the less likely that a 0 is due to not going fishing
## - the odds decline by 78% (factor of 0.22) for each additional person in the group.


## ----mainEffects, results='markdown', eval=TRUE, hidden=TRUE------------------
emmeans(fish.glm4,  pairwise~CHILDREN,  type='response')

newdata = emmeans(fish.glm4,  ~CHILDREN|CAMPER,  type='response') %>%
  as.data.frame
newdata %>% head

ggplot(newdata,  aes(y=emmean,  x=CHILDREN,  color=CAMPER)) +
  geom_pointrange(aes(ymin=asymp.LCL,  ymax=asymp.UCL)) +
  theme_classic()

fish.grid <- with(fish, list(PERSONS = 1:4))
emmeans(fish.glm4,  ~PERSONS,  at = fish.grid,  mode='zero',  lin.pred=FALSE,  type='response')
emmeans(fish.glm4,  ~PERSONS,  at = fish.grid,  mode='prob0')
emmeans(fish.glm4,  ~PERSONS,  mode='zero',  lin.pred=TRUE)  #link scale
emmeans(fish.glm4,  ~PERSONS,  mode='prob0')
#https://cran.r-project.org/web/packages/emmeans/vignettes/models.html

