## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE,cache.lazy = FALSE, tidy='styler')


## ----libraries, results='markdown', eval=TRUE, message=FALSE, warning=FALSE----
library(car)       #for regression diagnostics
library(broom)     #for tidy output
library(broom.mixed) ## for tidying mixed effects models
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
tobacco <- read_csv('../data/tobacco.csv', trim_ws = TRUE)
glimpse(tobacco)


## ----tobaccoEDA1, results='markdown', eval=TRUE, hidden=TRUE------------------
tobacco <- tobacco %>%
  mutate(LEAF = factor(LEAF),
         TREATMENT = factor(TREATMENT))


## ----tobaccoEDA2, results='markdown', eval=TRUE, hidden=TRUE------------------
ggplot(tobacco,  aes(y=NUMBER,  x=TREATMENT)) +
  geom_boxplot()


## ----tobaccoEDA3, results='markdown', eval=TRUE, hidden=TRUE------------------
ggplot(tobacco,  aes(y = NUMBER,  x = as.numeric(LEAF))) +
  geom_line(aes(linetype = TREATMENT))

## If we want to retain the original LEAF labels
ggplot(tobacco,  aes(y = NUMBER,  x = as.numeric(LEAF))) +
  geom_blank(aes(x = LEAF)) +
  geom_line(aes(linetype = TREATMENT))


## ----tobaccoEDA4, results='markdown', eval=TRUE, hidden=TRUE------------------
ggplot(tobacco,  aes(y = NUMBER, x = TREATMENT,  group = LEAF)) +
  geom_point() +
  geom_line(aes(x = as.numeric(TREATMENT))) 


## ----fitModel1a, results='markdown', eval=TRUE, hidden=TRUE-------------------
## Fit the random intercepts model
tobacco.lme <- lme(NUMBER ~ TREATMENT,  random = ~1|LEAF,  data = tobacco,  method = 'REML')
## Fit the random intercepts/slope model
tobacco.lme1 <- lme(NUMBER ~ TREATMENT,  random = ~TREATMENT|LEAF,  data = tobacco,  method = 'REML')


## ----fitModel1b, results='markdown', eval=TRUE, hidden=TRUE-------------------
AICc(tobacco.lme,  tobacco.lme1)


## ----fitModel1c, results='markdown', eval=TRUE, hidden=TRUE-------------------
anova(tobacco.lme,  tobacco.lme1) %>% print


## ----fitModel2a, results='markdown', eval=TRUE, hidden=TRUE-------------------
## Fit the random intercepts model
tobacco.lmer <- lmer(NUMBER ~ TREATMENT + (1|LEAF),  data = tobacco, REML = TRUE)
## Fit the random intercepts/slope model
## Note the following could not be run in lmer as there was not enough observations
## to estimate all of the effects and random effects.
## tobacco.lmer1 <- lmer(NUMBER ~ TREATMENT + (TREATMENT|LEAF),  data=tobacco, REML=TRUE)
## tobacco.lmer1 <- lmer(NUMBER ~ TREATMENT + (TREATMENT|LEAF),  data=tobacco, REML=TRUE,
##                       control = lmerControl(
##                           check.nobs.vs.nRE = "ignore",
##                           optimizer = 'optim',
##                           optCtrl = list(method = 'BFGS'))
##                       )


## ----fitModel2b, results='markdown', eval=TRUE, hidden=TRUE-------------------
#AICc(tobacco.lme,  tobacco.lme1)


## ----fitModel2c, results='markdown', eval=TRUE, hidden=TRUE-------------------
#anova(tobacco.lme,  tobacco.lme1) %>% print


## ----fitModel3a, results='markdown', eval=TRUE, hidden=TRUE-------------------
## Fit the random intercepts model
tobacco.glmmTMB <- glmmTMB(NUMBER ~ TREATMENT + (1|LEAF),
                           data = tobacco, REML = TRUE)
## Fit the random intercepts/slope model
## Note the following model did not converge - probably due to insufficient data.
tobacco.glmmTMB1 <- glmmTMB(NUMBER ~ TREATMENT + (TREATMENT|LEAF),
                            data = tobacco, REML = TRUE)
## Try a different optimizer (BFGS)
tobacco.glmmTMB1 <- glmmTMB(NUMBER ~ TREATMENT + (TREATMENT|LEAF),
                            data = tobacco, REML = TRUE,
                            control = glmmTMBControl(optimizer = 'optim',
                                                   optArgs='BFGS'))
                                                   ## optArgs = 'Nelder-Mead'))


## ----fitModel3b, results='markdown', eval=TRUE, hidden=TRUE-------------------
AICc(tobacco.glmmTMB,  tobacco.glmmTMB1)


## ----fitModel3c, results='markdown', eval=TRUE, hidden=TRUE-------------------
anova(tobacco.glmmTMB,  tobacco.glmmTMB1) %>% print


## ----modelValidation1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
#autoplot(tobacco.lme)
tobacco.lme %>% plot_model(type='diag') %>%  plot_grid()


## ----modelValidation1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
tobacco.lme %>% performance::check_model()


## ----modelValidation1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
## unfortunately,  nlme is not supported...
#tobacco.resid = simulateResiduals(tobacco.lme,  plot=TRUE)


## ----modelValidation2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
plot_model(tobacco.lmer, type='diag')[-2] %>% plot_grid()


## ----modelValidation2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
tobacco.lmer %>% performance::check_model()


## ----modelValidation2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
tobacco.resid <- tobacco.lmer %>% simulateResiduals(plot=TRUE)


## ----modelValidation3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
plot_model(tobacco.glmmTMB,  type='diag')[-2] %>% plot_grid()


## ----modelValidation3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
tobacco.glmmTMB %>% performance::check_model()


## ----modelValidation3c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
tobacco.resid <- tobacco.glmmTMB %>% simulateResiduals(plot=TRUE)


## ----partialPlots1a, results='markdown', eval=TRUE, hidden=TRUE---------------
#plot_model(tobacco.lme,  type='eff')


## ----partialPlots1b, results='markdown', eval=TRUE, hidden=TRUE---------------
tobacco.lme %>% allEffects() %>% plot()


## ----partialPlots1c, results='markdown', eval=TRUE, hidden=TRUE---------------
tobacco.lme1 <- update(tobacco.lme, data=as.data.frame(tobacco))
tobacco.lme1 %>% ggpredict(terms='TREATMENT') %>% plot()


## ----partialPlots1d, results='markdown', eval=TRUE, hidden=TRUE---------------
tobacco.lme1 <- update(tobacco.lme, data=as.data.frame(tobacco))
tobacco.lme1 %>% ggemmeans(~TREATMENT) %>% plot()


## ----partialPlots2a, results='markdown', eval=TRUE, hidden=TRUE---------------
tobacco.lmer %>% plot_model(type='eff')


## ----partialPlots2b, results='markdown', eval=TRUE, hidden=TRUE---------------
tobacco.lmer %>% allEffects() %>% plot()


## ----partialPlots2c, results='markdown', eval=TRUE, hidden=TRUE---------------
tobacco.lmer %>% ggpredict(terms='TREATMENT') %>% plot


## ----partialPlots2d, results='markdown', eval=TRUE, hidden=TRUE---------------
tobacco.lmer %>% ggemmeans(~TREATMENT) %>% plot


## ----partialPlots3a, results='markdown', eval=TRUE, hidden=TRUE---------------
tobacco.glmmTMB %>% plot_model(type='eff')


## ----partialPlots3b, results='markdown', eval=TRUE, hidden=TRUE---------------
tobacco.glmmTMB %>% allEffects() %>% plot()


## ----partialPlots3c, results='markdown', eval=TRUE, hidden=TRUE---------------
tobacco.glmmTMB %>% ggpredict(terms='TREATMENT') %>% plot


## ----partialPlots3d, results='markdown', eval=TRUE, hidden=TRUE---------------
tobacco.glmmTMB %>% ggemmeans(~TREATMENT) %>% plot


## ----investigateModel1a, results='markdown', eval=TRUE, hidden=TRUE-----------
tobacco.lme %>% summary()
## to get confidence intervals
tobacco.lme %>% intervals()


## ----investigateModel1b, results='markdown', eval=TRUE, hidden=TRUE-----------
tobacco.lme %>% tidy(effects='fixed', conf.int=TRUE)
## including the random effects
tobacco.lme %>% tidy(conf.int=TRUE)


## ----investigateModel1c, results='markdown', eval=TRUE, hidden=TRUE-----------
# warning this is only appropriate for html output
tobacco.lme %>% sjPlot::tab_model(show.se=TRUE,show.aic=TRUE)


## ----investigateModel1d, results='markdown', eval=TRUE, hidden=TRUE-----------
tobacco.lme %>% r.squaredGLMM()
## Nakagawa's R2
tobacco.lme %>% performance::r2_nakagawa()


## ----investigateModel2a, results='markdown', eval=TRUE, hidden=TRUE-----------
tobacco.lmer %>% summary()
## to get confidence intervals
tobacco.lmer %>% confint()


## ----investigateModel2b, results='markdown', eval=TRUE, hidden=TRUE-----------
tobacco.lmer %>% tidy(effects='fixed', conf.int=TRUE)
## including the random effects
tobacco.lmer %>% tidy(conf.int=TRUE)


## ----investigateModel2c, results='markdown', eval=TRUE, hidden=TRUE-----------
# warning this is only appropriate for html output
tobacco.lmer %>% sjPlot::tab_model(show.se=TRUE, show.aic=TRUE)


## ----investigateModel2d, results='markdown', eval=TRUE, hidden=TRUE-----------
tobacco.lmer %>% r.squaredGLMM()
## Nakagawa's R2
tobacco.glmmTMB %>% performance::r2_nakagawa()


## ----investigateModel3a, results='markdown', eval=TRUE, hidden=TRUE-----------
tobacco.glmmTMB %>% summary()
tobacco.glmmTMB %>% vcov()
## the following is not compatible with piping
cov2cor(vcov(tobacco.glmmTMB)$cond)
## to get confidence intervals
tobacco.glmmTMB %>% confint()
tobacco.glmmTMB %>% ranef()


## ----investigateModel3b, results='markdown', eval=TRUE, hidden=TRUE-----------
tobacco.glmmTMB %>% tidy(effects='fixed', conf.int=TRUE)
## including the random effects
tobacco.glmmTMB %>% tidy(conf.int=TRUE)


## ----investigateModel3c, results='markdown', eval=TRUE, hidden=TRUE-----------
# warning this is only appropriate for html output
tobacco.glmmTMB %>% sjPlot::tab_model(show.se=TRUE,show.aic=TRUE)


## ----investigateModel3d, results='markdown', eval=TRUE, hidden=TRUE-----------
tobacco.glmmTMB %>% r.squaredGLMM()
## Nakagawa's R2
tobacco.glmmTMB %>% performance::r2_nakagawa()


## ----prediction1a, results='markdown', eval=TRUE------------------------------
tobacco.lme %>% multcomp::glht(linfct=c('TREATMENTWeak=-2')) %>% summary


## ----prediction1b, cache=TRUE, results='markdown', eval=TRUE------------------
## lme not supported
#brms::hypothesis(tobacco.lme, 'TREATMENTWeak < -2')


## ----prediction2a, results='markdown', eval=TRUE------------------------------
tobacco.lmer %>% multcomp::glht(linfct=c('TREATMENTWeak=-2')) %>% summary


## ----prediction2b, cache=TRUE, results='markdown', eval=TRUE------------------
tobacco.lmer %>% brms::hypothesis('TREATMENTWeak < -2')


## ----prediction3a, results='markdown', eval=TRUE------------------------------
tobacco.glmmTMB %>% multcomp::glht(linfct=c('TREATMENTWeak=-2')) %>% summary


## ----prediction3b, cache=TRUE, results='markdown', eval=TRUE------------------
## Does not appear to work for glmmTMB?
                                        #brms::hypothesis(tobacco.glmmTMB, 'TREATMENTWeak < -2')


## ----summaryFigure1a, results='markdown', eval=TRUE, hidden=TRUE--------------
tobacco.lme %>% emmeans(~TREATMENT) %>%
  as.data.frame %>%
  ggplot() +
  geom_pointrange(aes(y=emmean,  x=TREATMENT,  ymin=lower.CL,  ymax=upper.CL))


## ----summaryFigure2a, results='markdown', eval=TRUE, hidden=TRUE--------------
tobacco.lmer %>% emmeans(~TREATMENT) %>%
  as.data.frame() %>%
  ggplot() +
  geom_pointrange(aes(y=emmean,  x=TREATMENT,  ymin=lower.CL,  ymax=upper.CL))


## ----summaryFigure3a, results='markdown', eval=TRUE, hidden=TRUE--------------
tobacco.glmmTMB %>% emmeans(~TREATMENT) %>%
  as.data.frame() %>%
  ggplot() +
  geom_pointrange(aes(y=emmean,  x=TREATMENT,  ymin=lower.CL,  ymax=upper.CL))

