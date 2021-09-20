## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE)


## ----libraries, results='markdown', eval=TRUE, message=FALSE, warning=FALSE----
library(car)       #for regression diagnostics
library(broom)     #for tidy output
library(broom.mixed)
library(ggfortify) #for model diagnostics
library(sjPlot)    #for outputs
library(knitr)     #for kable
library(effects)   #for partial effects plots
library(emmeans)   #for estimating marginal means
library(ggeffects)  #for partial effects plots
library(MASS)      #for glm.nb
library(MuMIn)     #for AICc
library(nlme)
library(lme4)      #for lmer
library(lmerTest)  #for satterthwaite p-values with lmer
library(performance) #for residuals diagnostics
library(see)         #for plotting residuals
#library(pbkrtest)  #for kenward-roger p-values with lmer
library(glmmTMB)   #for glmmTMB
library(DHARMa)   #for residuals and diagnostics
library(tidyverse) #for data wrangling


## ----readData, results='markdown', eval=TRUE----------------------------------
tobacco = read_csv('../data/tobacco.csv', trim_ws=TRUE)
glimpse(tobacco)


## ----tobaccoEDA1, results='markdown', eval=TRUE, hidden=TRUE------------------
tobacco <- tobacco %>%
  mutate(LEAF=factor(LEAF),
         TREATMENT=factor(TREATMENT))


## ----tobaccoEDA2, results='markdown', eval=TRUE, hidden=TRUE------------------
ggplot(tobacco,  aes(y=NUMBER,  x=TREATMENT)) +
  geom_boxplot()


## ----tobaccoEDA3, results='markdown', eval=TRUE, hidden=TRUE------------------
ggplot(tobacco,  aes(y=NUMBER,  x=as.numeric(LEAF))) +
  geom_line(aes(linetype=TREATMENT))

## If we want to retain the original LEAF labels
ggplot(tobacco,  aes(y=NUMBER,  x=as.numeric(LEAF))) +
  geom_blank(aes(x=LEAF)) +
  geom_line(aes(linetype=TREATMENT))


## ----tobaccoEDA4, results='markdown', eval=TRUE, hidden=TRUE------------------
ggplot(tobacco,  aes(y=NUMBER,  x=TREATMENT,  group=LEAF)) +
  geom_point() +
  geom_line(aes(x=as.numeric(TREATMENT))) 


## ----fitModel1a, results='markdown', eval=TRUE, hidden=TRUE-------------------
## Fit the random intercepts model
tobacco.lme <- lme(NUMBER ~ TREATMENT,  random=~1|LEAF,  data=tobacco,  method='REML')
## Fit the random intercepts/slope model
tobacco.lme1 <- lme(NUMBER ~ TREATMENT,  random=~TREATMENT|LEAF,  data=tobacco,  method='REML')


## ----fitModel1b, results='markdown', eval=TRUE, hidden=TRUE-------------------
AICc(tobacco.lme,  tobacco.lme1)


## ----fitModel1c, results='markdown', eval=TRUE, hidden=TRUE-------------------
anova(tobacco.lme,  tobacco.lme1) %>% print


## ----fitModel2a, results='markdown', eval=TRUE, hidden=TRUE-------------------
## Fit the random intercepts model
tobacco.lmer <- lmer(NUMBER ~ TREATMENT + (1|LEAF),  data=tobacco, REML=TRUE)
## Fit the random intercepts/slope model
## Note the following could not be run in lmer as there was not enough observations
## to estimate all of the effects and random effects.
#tobacco.lmer1 <- lmer(NUMBER ~ TREATMENT + (TREATMENT|LEAF),  data=tobacco, REML=TRUE)


## ----fitModel2b, results='markdown', eval=TRUE, hidden=TRUE-------------------
#AICc(tobacco.lme,  tobacco.lme1)


## ----fitModel2c, results='markdown', eval=TRUE, hidden=TRUE-------------------
#anova(tobacco.lme,  tobacco.lme1) %>% print


## ----fitModel3a, results='markdown', eval=TRUE, hidden=TRUE-------------------
## Fit the random intercepts model
tobacco.glmmTMB <- glmmTMB(NUMBER ~ TREATMENT + (1|LEAF),
                           data=tobacco, REML=TRUE)
## Fit the random intercepts/slope model
## Note the following model did not converge - probably due to insufficient data.
tobacco.glmmTMB1 <- glmmTMB(NUMBER ~ TREATMENT + (TREATMENT|LEAF),
                            data=tobacco, REML=TRUE)
## Try a different optimizer (BFGS)
tobacco.glmmTMB1 <- glmmTMB(NUMBER ~ TREATMENT + (TREATMENT|LEAF),
                            data=tobacco, REML=TRUE,
                            control=glmmTMBControl(optimizer='optim',
                                                   optArgs='Nelder-Mead'))
                                                   ## optArgs='BFGS'))


## ----fitModel3b, results='markdown', eval=TRUE, hidden=TRUE-------------------
AICc(tobacco.glmmTMB,  tobacco.glmmTMB1)


## ----fitModel3c, results='markdown', eval=TRUE, hidden=TRUE-------------------
anova(tobacco.glmmTMB,  tobacco.glmmTMB1) %>% print


## ----modelValidation1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
#autoplot(tobacco.lme)
plot_grid(plot_model(tobacco.lme,  type='diag'))


## ----modelValidation1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
performance::check_model(tobacco.lme)


## ----modelValidation1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
## unfortunately,  nlme is not supported...
#tobacco.resid = simulateResiduals(tobacco.lme,  plot=TRUE)


## ----modelValidation2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
plot_grid(plot_model(tobacco.lmer,  type='diag')[-2])


## ----modelValidation2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
performance::check_model(tobacco.lmer)


## ----modelValidation2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
tobacco.resid = simulateResiduals(tobacco.lmer,  plot=TRUE)


## ----modelValidation3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
plot_grid(plot_model(tobacco.glmmTMB,  type='diag')[-2])


## ----modelValidation3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
performance::check_model(tobacco.glmmTMB)


## ----modelValidation3c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
tobacco.resid = simulateResiduals(tobacco.glmmTMB,  plot=TRUE)


## ----partialPlots1a, results='markdown', eval=TRUE, hidden=TRUE---------------
#plot_model(tobacco.lme,  type='eff')


## ----partialPlots1b, results='markdown', eval=TRUE, hidden=TRUE---------------
plot(allEffects(tobacco.lme))


## ----partialPlots1c, results='markdown', eval=TRUE, hidden=TRUE---------------
#ggpredict(tobacco.lme, terms='TREATMENT')


## ----partialPlots1d, results='markdown', eval=TRUE, hidden=TRUE---------------
#ggemmeans(tobacco.lme, ~TREATMENT)


## ----partialPlots2a, results='markdown', eval=TRUE, hidden=TRUE---------------
plot_model(tobacco.lmer,  type='eff')


## ----partialPlots2b, results='markdown', eval=TRUE, hidden=TRUE---------------
plot(allEffects(tobacco.lmer))


## ----partialPlots2c, results='markdown', eval=TRUE, hidden=TRUE---------------
ggpredict(tobacco.lmer, terms='TREATMENT') %>% plot


## ----partialPlots2d, results='markdown', eval=TRUE, hidden=TRUE---------------
ggemmeans(tobacco.lmer, ~TREATMENT) %>% plot


## ----partialPlots3a, results='markdown', eval=TRUE, hidden=TRUE---------------
plot_model(tobacco.glmmTMB,  type='eff')


## ----partialPlots3b, results='markdown', eval=TRUE, hidden=TRUE---------------
plot(allEffects(tobacco.glmmTMB))


## ----partialPlots3c, results='markdown', eval=TRUE, hidden=TRUE---------------
ggpredict(tobacco.glmmTMB, terms='TREATMENT') %>% plot


## ----partialPlots3d, results='markdown', eval=TRUE, hidden=TRUE---------------
ggemmeans(tobacco.glmmTMB, ~TREATMENT) %>% plot


## ----investigateModel1a, results='markdown', eval=TRUE, hidden=TRUE-----------
summary(tobacco.lme)
## to get confidence intervals
intervals(tobacco.lme)


## ----investigateModel1b, results='markdown', eval=TRUE, hidden=TRUE-----------
tidy(tobacco.lme,  effects='fixed', conf.int=TRUE)
## including the random effects
tidy(tobacco.lme, conf.int=TRUE)


## ----investigateModel1c, results='markdown', eval=TRUE, hidden=TRUE-----------
# warning this is only appropriate for html output
sjPlot::tab_model(tobacco.lme,show.se=TRUE,show.aic=TRUE)


## ----investigateModel1d, results='markdown', eval=TRUE, hidden=TRUE-----------
r.squaredGLMM(tobacco.lme)
## Nakagawa's R2
performance::r2_nakagawa(tobacco.lme)


## ----investigateModel2a, results='markdown', eval=TRUE, hidden=TRUE-----------
summary(tobacco.lmer)
## to get confidence intervals
confint(tobacco.lmer)


## ----investigateModel2b, results='markdown', eval=TRUE, hidden=TRUE-----------
tidy(tobacco.lmer,  effects='fixed', conf.int=TRUE)
## including the random effects
tidy(tobacco.lmer, conf.int=TRUE)


## ----investigateModel2c, results='markdown', eval=TRUE, hidden=TRUE-----------
# warning this is only appropriate for html output
sjPlot::tab_model(tobacco.lmer,show.se=TRUE,show.aic=TRUE)


## ----investigateModel2d, results='markdown', eval=TRUE, hidden=TRUE-----------
r.squaredGLMM(tobacco.lmer)
## Nakagawa's R2
performance::r2_nakagawa(tobacco.glmmTMB)


## ----investigateModel3a, results='markdown', eval=TRUE, hidden=TRUE-----------
summary(tobacco.glmmTMB)
vcov(tobacco.glmmTMB)
cov2cor(vcov(tobacco.glmmTMB)$cond)
## to get confidence intervals
confint(tobacco.glmmTMB)
ranef(tobacco.glmmTMB)


## ----investigateModel3b, results='markdown', eval=TRUE, hidden=TRUE-----------
tidy(tobacco.glmmTMB,  effects='fixed', conf.int=TRUE)
## including the random effects
tidy(tobacco.glmmTMB, conf.int=TRUE)


## ----investigateModel3c, results='markdown', eval=TRUE, hidden=TRUE-----------
# warning this is only appropriate for html output
sjPlot::tab_model(tobacco.glmmTMB,show.se=TRUE,show.aic=TRUE)


## ----investigateModel3d, results='markdown', eval=TRUE, hidden=TRUE-----------
r.squaredGLMM(tobacco.glmmTMB)
## Nakagawa's R2
performance::r2_nakagawa(tobacco.glmmTMB)


## ----prediction1a, results='markdown', eval=TRUE------------------------------
multcomp::glht(tobacco.lme, linfct=c('TREATMENTWeak==-2')) %>% summary


## ----prediction1b, cache=TRUE, results='markdown', eval=TRUE------------------
## lme not supported
#brms::hypothesis(tobacco.lme, 'TREATMENTWeak < -2')


## ----prediction2a, results='markdown', eval=TRUE------------------------------
multcomp::glht(tobacco.lmer, linfct=c('TREATMENTWeak==-2')) %>% summary


## ----prediction2b, cache=TRUE, results='markdown', eval=TRUE------------------
brms::hypothesis(tobacco.lmer, 'TREATMENTWeak < -2')


## ----prediction3a, results='markdown', eval=TRUE------------------------------
multcomp::glht(tobacco.glmmTMB, linfct=c('TREATMENTWeak==-2')) %>% summary


## ----prediction3b, cache=TRUE, results='markdown', eval=TRUE------------------
## Does not appear to work for glmmTMB?
#brms::hypothesis(tobacco.glmmTMB, 'TREATMENTWeak < -2')


## ----summaryFigure1a, results='markdown', eval=TRUE, hidden=TRUE--------------
emmeans(tobacco.lme,  ~TREATMENT) %>%
  as.data.frame %>%
  ggplot() +
  geom_pointrange(aes(y=emmean,  x=TREATMENT,  ymin=lower.CL,  ymax=upper.CL))


## ----summaryFigure2a, results='markdown', eval=TRUE, hidden=TRUE--------------
emmeans(tobacco.lmer,  ~TREATMENT) %>%
  as.data.frame() %>%
  ggplot() +
  geom_pointrange(aes(y=emmean,  x=TREATMENT,  ymin=lower.CL,  ymax=upper.CL))


## ----summaryFigure3a, results='markdown', eval=TRUE, hidden=TRUE--------------
emmeans(tobacco.glmmTMB,  ~TREATMENT) %>%
  as.data.frame() %>%
  ggplot() +
  geom_pointrange(aes(y=emmean,  x=TREATMENT,  ymin=lower.CL,  ymax=upper.CL))

