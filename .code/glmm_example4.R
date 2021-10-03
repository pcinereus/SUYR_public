## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE,cache.lazy = FALSE, tidy='styler')


## ----libraries, results='markdown', eval=TRUE, message=FALSE, warning=FALSE----
library(car)       #for regression diagnostics
library(broom)     #for tidy output
library(broom.mixed)
library(ggfortify) #for model diagnostics
library(sjPlot)    #for outputs
library(knitr)     #for kable
library(effects)   #for partial effects plots
library(ggeffects) #for effects plots in ggplot
library(emmeans)   #for estimating marginal means
library(MASS)      #for glm.nb
library(MuMIn)     #for AICc
library(tidyverse) #for data wrangling
library(DHARMa)   #for residuals and diagnostics
library(nlme)     #for lme
library(lme4)      #for glmer
library(glmmTMB)    #for glmmTMB
library(performance) #for diagnostic plots
library(see)         #for diagnostic plots


## ----readData, results='markdown', eval=TRUE----------------------------------
mckeon = read_csv('../data/mckeon.csv', trim_ws=TRUE)
glimpse(mckeon)


## ----processData, results='markdown', eval=TRUE, hidden=TRUE------------------
mckeon = mckeon %>%
  mutate(BLOCK=factor(BLOCK),
         SYMBIONT=factor(SYMBIONT, levels=c('none','crabs','shrimp','both')))


## ----eda1a, results='markdown', eval=TRUE, hidden=TRUE------------------------
ggplot(mckeon, aes(y=PREDATION, x=SYMBIONT)) +
    geom_point(position=position_jitter(width=0.2, height=0))+
    facet_wrap(~BLOCK)


## ----fitModel1a, cache=TRUE,results='markdown', eval=TRUE, hidden=TRUE--------
mckeon.glmer1 = glmer(PREDATION ~ SYMBIONT+(1|BLOCK),
                      data=mckeon,
                    family=binomial(link='logit'))


## ----fitModel1b, cache=TRUE,results='markdown', eval=TRUE, hidden=TRUE--------
mckeon.glmer2 <- update(mckeon.glmer1,  ~ . - (1|BLOCK) + (SYMBIONT|BLOCK))


## ----fitModel1c1, cache=TRUE,results='markdown', eval=FALSE, echo=FALSE, hidden=TRUE----
## ## Try the Nelder_Mead optimizer
## mckeon.glmer1 %>% jmckeon.glmer2 <- update(,  ~ . - (1|BLOCK) + (SYMBIONT|BLOCK),
##                        control=glmerControl(optimizer="Nelder_Mead"))
## ## Try the L-BFGS-Bj optimizer
## mckeon.glmer1 %>% jmckeon.glmer2 <- update(,  ~ . - (1|BLOCK) + (SYMBIONT|BLOCK),
##                        control=glmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
## ## Try the nlminb ojptimizer
## mckeon.glmer1 %>% jmckeon.glmer2 <- update(,  ~ . - (1|BLOCK) + (SYMBIONT|BLOCK),
##                        control=glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))
## mckeon.glmer1 %>% jjAICc(,  mckeon.glmer2)


## ----fitModel1c, cache=TRUE,results='markdown', eval=TRUE, hidden=TRUE--------
mckeon.allFit <- mckeon.glmer2 %>% allFit()
## Check which of the models are considered valid (OK)
is.OK <- sapply(mckeon.allFit, is, "merMod")
is.OK


## ----fitModel1d, cache=TRUE,results='markdown', eval=TRUE, hidden=TRUE--------
diff_optims.OK <- mckeon.allFit[is.OK]
lapply(diff_optims.OK,function(x) x@optinfo$conv$lme4$messages)


## ----fitModel1e, cache=TRUE,results='markdown', echo=FALSE, eval=FALSE, hidden=TRUE----
## ss <- mckeon.allFit %>% summary()
## ## determine which models are valid
## ss$which.OK
## zapsmall(diff(ss$llik))
## ss$fixef


## ----fitModel1f, cache=TRUE,results='markdown', eval=TRUE, hidden=TRUE--------
mckeon.glmer2 <- mckeon.glmer1 %>% update(~ . - (1|BLOCK) + (SYMBIONT|BLOCK),
                       control=glmerControl(optimizer='bobyqa',
                        optCtrl=list(maxfun=2e5)))


## ----fitModel1g, cache=TRUE,results='markdown', eval=TRUE, hidden=TRUE--------
pars <- getME(mckeon.glmer2, c("theta","fixef"))
devfun <- update(mckeon.glmer2, devFunOnly=TRUE)
library(numDeriv)
cat("hess:\n"); print(hess <- hessian(devfun, unlist(pars)))
cat("grad:\n"); print(grad <- grad(devfun, unlist(pars)))
cat("scaled gradient:\n")
#print(scgrad <- solve(chol(hess), grad))
## compare with internal calculations:
mckeon.glmer2@optinfo$derivs


## ----fitModel1h, cache=TRUE,results='markdown', eval=TRUE, hidden=TRUE--------
#strict_tol <- glmerControl(optCtrl=list(xtol_abs=1e-8, ftol_abs=1e-8))
pars <- getME(mckeon.glmer2, c("theta","fixef"))
mckeon.glmer3 <- update(mckeon.glmer2, start=pars, control=glmerControl(optimizer='bobyqa',
                        optCtrl=list(maxfun=2e5)))


## ----fitModel2a, cache=TRUE,results='markdown', eval=TRUE, hidden=TRUE--------
mckeon.glmmTMB1 <- glmmTMB(PREDATION ~ SYMBIONT+(1|BLOCK),
                           data = mckeon,
                           family = binomial(link = 'logit'),
                           REML=TRUE)


## ----fitModel2b, cache=TRUE,results='markdown', eval=TRUE, hidden=TRUE, error=TRUE----
mckeon.glmmTMB2 <- mckeon.glmmTMB1 %>% update(~ . - (1|BLOCK) + (SYMBIONT|BLOCK))
mckeon.glmmTMB2 <- mckeon.glmmTMB1 %>% update(~ . - (1|BLOCK) + (SYMBIONT|BLOCK),
                          control=glmmTMBControl(optimizer=optim,
                                                 optArgs = list(method='SANN')))
AICc(mckeon.glmmTMB1,  mckeon.glmmTMB2)


## ----validation1a, results='markdown', eval=TRUE, hidden=TRUE,warning=FALSE,message=FALSE----
mckeon.glmer1 %>% plot_model(type='diag') %>% plot_grid()


## ----validation1b, results='markdown', eval=TRUE, hidden=TRUE,warning=FALSE,message=FALSE,fig.width=7,fig.height=7----
mckeon.glmer1 %>% performance::check_model()


## ----validation1c, results='markdown', eval=TRUE, hidden=TRUE,warning=FALSE,message=FALSE,fig.width=7,fig.height=4----
mckeon.resid <-  mckeon.glmer1 %>% simulateResiduals(plot=TRUE)
mckeon.resid %>% testZeroInflation()


## ----validation2a, results='markdown', eval=TRUE, hidden=TRUE,warning=FALSE,message=FALSE----
mckeon.glmmTMB1 %>% plot_model(type='diag') %>% plot_grid()


## ----validation2b, results='markdown', eval=TRUE, hidden=TRUE,warning=FALSE,message=FALSE,fig.width=7,fig.height=7----
mckeon.glmmTMB1 %>% performance::check_model()


## ----validation2c, results='markdown', eval=TRUE, hidden=TRUE,warning=FALSE,message=FALSE,fig.width=7,fig.height=4----
mckeon.resid <- mckeon.glmmTMB1 %>% simulateResiduals(plot=TRUE)
mckeon.resid %>% testZeroInflation()
mckeon.resid %>% testDispersion()


## ----partialPlots1a, results='markdown', eval=TRUE, hidden=TRUE---------------
mckeon.glmer1 %>% plot_model(type='eff', show.data=TRUE, jitter=c(0.05,0))


## ----partialPlots1b, results='markdown', eval=TRUE, hidden=TRUE---------------
mckeon.glmer1 %>% allEffects() %>% plot()


## ----partialPlots1c, results='markdown', eval=TRUE, hidden=TRUE---------------
mckeon.glmer1 %>% ggpredict() %>% plot(add.data=TRUE, jitter=c(0.1,0))


## ----partialPlots1d, results='markdown', eval=TRUE, hidden=TRUE---------------
mckeon.glmer1 %>% ggemmeans(~SYMBIONT) %>% plot(add.data=TRUE, jitter=c(0.1,0))


## ----partialPlots2a, results='markdown', eval=TRUE, hidden=TRUE---------------
mckeon.glmmTMB1 %>% plot_model(type='eff', show.data=TRUE, jitter=c(0.05,0))


## ----partialPlots2b, results='markdown', eval=TRUE, hidden=TRUE---------------
mckeon.glmmTMB1 %>% allEffects() %>% plot()


## ----partialPlots2c, results='markdown', eval=TRUE, hidden=TRUE---------------
mckeon.glmmTMB1 %>% ggpredict() %>% plot(add.data=TRUE, jitter=c(0.05,0))


## ----partialPlots2d, results='markdown', eval=TRUE, hidden=TRUE---------------
mckeon.glmmTMB1 %>% ggemmeans(~SYMBIONT) %>% plot(add.data=TRUE, jitter=c(0.05,0))


## ----summary1a, results='markdown', eval=TRUE, hidden=TRUE--------------------
mckeon.glmer1 %>% summary()


## ----summary1a1, results='markdown', eval=TRUE, echo=FALSE,hidden=TRUE--------
mckeon.tidy <-  mckeon.glmer1 %>% tidy()


## ----summary1b, results='markdown', eval=TRUE, hidden=TRUE--------------------
mckeon.glmer1 %>% tidy(effect='fixed', conf.int=TRUE, infer=c(TRUE, TRUE),  exponentiate=TRUE)
mckeon.glmer1 %>% tidy(effect='fixed', conf.int=TRUE, infer=c(TRUE, TRUE),  exponentiate=TRUE) %>% kable


## ----summary1c, results='markdown', eval=TRUE, hidden=TRUE--------------------
# warning this is only appropriate for html output
mckeon.glmer1 %>% sjPlot::tab_model(show.se=TRUE, show.aic=TRUE)


## ----summary2a, results='markdown', eval=TRUE, hidden=TRUE--------------------
mckeon.glmmTMB1 %>% summary()


## ----summary2a1, results='markdown', eval=TRUE, echo=FALSE,hidden=TRUE--------
mckeon.tidy <- mckeon.glmmTMB1 %>% tidy()


## ----summary2b, results='markdown', eval=TRUE, hidden=TRUE--------------------
mckeon.glmmTMB1 %>% tidy(effect='fixed', conf.int=TRUE, infer=c(TRUE, TRUE))
mckeon.glmmTMB1 %>% tidy(effect='fixed', conf.int=TRUE, infer=c(TRUE, TRUE),  exponentiate=TRUE)


## ----summary2c, results='markdown', eval=TRUE, hidden=TRUE--------------------
# warning this is only appropriate for html output
mckeon.glmmTMB1 %>% sjPlot::tab_model(show.se=TRUE, show.aic=TRUE)


## ----posteriors1a, results='markdown', eval=TRUE, hidden=TRUE-----------------
cmat=cbind(
    crab_vs_shrimp=c(0,1,-1,0),
    one_vs_both=c(0,1/2,1/2,-1),
    symbiont=c(1, -1/3, -1/3,-1/3)
)
round(crossprod(cmat),1)
## all contrasts orthogonal
mckeon.glmer1 %>%
    emmeans(~SYMBIONT, type='response') %>%
    contrast(method=list(SYMBIONT=cmat)) %>%
    summary(infer=TRUE)
## Or on an absolute difference scale
mckeon.glmer1 %>%
    emmeans(~SYMBIONT, link='link') %>%
    regrid() %>% 
    contrast(method=list(SYMBIONT=cmat)) %>%
    summary(infer=TRUE)


## ----posteriors1b, results='markdown', eval=TRUE, hidden=TRUE-----------------
mckeon.glmer1 %>% r.squaredGLMM()
     ## The delta mehod can be used with for all distributions and link
     ## functions, jwhile lognormal approximation and trigamma function are
     ## limited to distributions with logarithmic link. Trigamma-estimate
     ## is recommended whenever available. Additionally, for binomial
     ## distributions, theoretical variances exist specific for each link
     ## function distribution.
mckeon.glmer1 %>% performance::r2_nakagawa()


## ----posteriors2a, results='markdown', eval=TRUE, hidden=TRUE-----------------
cmat=cbind(
    crab_vs_shrimp=c(0,1,-1,0),
    one_vs_both=c(0,1/2,1/2,-1),
    symbiont=c(1, -1/3, -1/3,-1/3)
)
round(crossprod(cmat),1)
## all contrasts orthogonal
## On the link scale
mckeon.glmmTMB1 %>%
    emmeans(~SYMBIONT, type='link') %>%
    contrast(method=list(SYMBIONT=cmat)) %>%
    summary(infer=TRUE)
## On the ratio (fold) scale
mckeon.glmmTMB1 %>%
    emmeans(~SYMBIONT, type='response') %>%
    contrast(method=list(SYMBIONT=cmat)) %>%
    summary(infer=TRUE)
## On an absolute difference scale
mckeon.glmmTMB1 %>%
    emmeans(~SYMBIONT, type='link') %>%
    regrid() %>%
    contrast(method=list(SYMBIONT=cmat)) %>%
    summary(infer=TRUE)


## ----posteriors2b, results='markdown', eval=TRUE, hidden=TRUE-----------------
mckeon.glmmTMB1 %>% r.squaredGLMM()
     ## The delta method can be used with for all distributions and link
     ## functions, while lognormal approximation and trigamma function are
     ## limited to distributions with logarithmic link. Trigamma-estimate
     ## is recommended whenever available. Additionally, for binomial
     ## distributions, theoretical variances exist specific for each link
     ## function distribution.
mckeon.glmmTMB1 %>% performance::r2_nakagawa()


## ----summaryFig1a, results='markdown', eval=TRUE, hidden=TRUE-----------------
mckeon.glmer1 %>% emmeans(~SYMBIONT, type='response') %>%
  as.data.frame %>%
  ggplot(aes(y=prob,  x=SYMBIONT)) +
  geom_pointrange(aes(ymin=asymp.LCL,  ymax=asymp.UCL))


## ----summaryFig2a, results='markdown', eval=TRUE, hidden=TRUE-----------------
mckeon.glmmTMB1 %>% emmeans(~SYMBIONT, type='response') %>% 
  as.data.frame %>%
  ggplot(aes(y=prob,  x=SYMBIONT)) +
  geom_pointrange(aes(ymin=lower.CL,  ymax=upper.CL))

