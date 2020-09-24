## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE)


## ----libraries, results='markdown', eval=TRUE, message=FALSE, warning=FALSE----
library(car)       #for regression diagnostics
library(broom)     #for tidy output
library(broom.mixed)
library(ggfortify) #for model diagnostics
library(sjPlot)    #for outputs
library(knitr)     #for kable
library(effects)   #for partial effects plots
library(emmeans)   #for estimating marginal means
library(MASS)      #for glm.nb
library(MuMIn)     #for AICc
library(tidyverse) #for data wrangling
library(nlme)
library(lme4)      #for lmer
library(lmerTest)  #for satterthwaite p-values with lmer
#library(pbkrtest)  #for kenward-roger p-values with lmer
library(glmmTMB)   #for glmmTMB
library(DHARMa)   #for residuals and diagnostics


## ----readData, results='markdown', eval=TRUE----------------------------------
tobacco = read_csv('../data/tobacco.csv', trim_ws=TRUE)
glimpse(tobacco)


## ----tobaccoEDA, results='markdown', eval=TRUE, hidden=TRUE-------------------
tobacco = tobacco %>%
  mutate(LEAF=factor(LEAF),
         TREATMENT=factor(TREATMENT))

ggplot(tobacco,  aes(y=NUMBER,  x=TREATMENT)) +
  geom_boxplot()

ggplot(tobacco,  aes(y=NUMBER,  x=as.numeric(LEAF))) +
  geom_line(aes(linetype=TREATMENT))

## If we want to retain the original LEAF labels
ggplot(tobacco,  aes(y=NUMBER,  x=as.numeric(LEAF))) +
  geom_blank(aes(x=LEAF)) +
  geom_line(aes(linetype=TREATMENT))



## ----fitModel, results='markdown', eval=TRUE, hidden=TRUE---------------------
tobacco.lme <- lme(NUMBER ~ TREATMENT,  random=~1|LEAF,  data=tobacco,  method='REML')
tobacco.lme1 <- lme(NUMBER ~ TREATMENT,  random=~TREATMENT|LEAF,  data=tobacco,  method='REML')
AIC(tobacco.lme,  tobacco.lme1)
AICc(tobacco.lme,  tobacco.lme1)
anova(tobacco.lme,  tobacco.lme1)


## ----modelValidation, results='markdown', eval=TRUE, hidden=TRUE--------------
#autoplot(tobacco.lme)
plot(tobacco.lme)
plot_grid(plot_model(tobacco.lme,  type='diag'))
## unfortunately,  nlme is not supported...
#tobacco.resid = simulateResiduals(tobacco.lme,  plot=TRUE)
plot(allEffects(tobacco.lme))
#plot_model(tobacco.lme,  type='eff')


## ----investigateModel, results='markdown', eval=TRUE, hidden=TRUE-------------
summary(tobacco.lme)
intervals(tobacco.lme)
tidy(tobacco.lme,  effects='fixed', conf.int=TRUE)
tidy(tobacco.lme, conf.int=TRUE)
r.squaredGLMM(tobacco.lme)
anova(tobacco.lme)


## ----lmeFig, results='markdown', eval=TRUE, hidden=TRUE-----------------------
emmeans(tobacco.lme,  ~TREATMENT) %>%
  as.data.frame %>%
  ggplot() +
  geom_pointrange(aes(y=emmean,  x=TREATMENT,  ymin=lower.CL,  ymax=upper.CL))


## ----fitModellmer, results='markdown', eval=FALSE, hidden=TRUE----------------
## tobacco.lmer <- lmer(NUMBER ~ TREATMENT + (1|LEAF),  data=tobacco,  REML=TRUE)
## tobacco.lmer1 <- lmer(NUMBER ~ TREATMENT + (TREATMENT|LEAF),  data=tobacco,  REML=TRUE)
##                                         #AIC(tobacco.lmer,  tobacco.lmer1)
## plot_grid(plot_model(tobacco.lmer,  type='diag')[-2])
## tobacco.resid = simulateResiduals(tobacco.lmer,  plot=TRUE)
## 
## summary(tobacco.lmer)
## tidy(tobacco.lmer)
## glance(tobacco.lmer)
## r.squaredGLMM(tobacco.lmer)
## emmeans(tobacco.lmer,  ~TREATMENT) %>%
##   as.data.frame() %>%
##   ggplot() +
##   geom_pointrange(aes(y=emmean,  x=TREATMENT,  ymin=lower.CL,  ymax=upper.CL))


## ----fitModelglmm, results='markdown', eval=FALSE, hidden=TRUE----------------
## tobacco.glmmTMB <- glmmTMB(NUMBER ~ TREATMENT + (1|LEAF),  data=tobacco,  REML=TRUE)
## tobacco.glmmTMB1 <- glmmTMB(NUMBER ~ TREATMENT + (TREATMENT|LEAF),  data=tobacco,  REML=TRUE)
##                                         #AIC(tobacco.glmmTMB,  tobacco.glmmTMB1)
## plot_grid(plot_model(tobacco.glmmTMB,  type='diag')[-2])
## tobacco.resid = simulateResiduals(tobacco.glmmTMB,  plot=TRUE)
## 
## summary(tobacco.glmmTMB)
## 
## confint(tobacco.glmmTMB)
## tidy(tobacco.glmmTMB)
## glance(tobacco.glmmTMB)
## r.squaredGLMM(tobacco.glmmTMB)
## emmeans(tobacco.glmmTMB,  ~TREATMENT) %>%
##   as.data.frame() %>%
##   ggplot() +
##   geom_pointrange(aes(y=emmean,  x=TREATMENT,  ymin=lower.CL,  ymax=upper.CL))

