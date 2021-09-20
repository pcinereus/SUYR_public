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
mckeon.glmer1 = glmer(PREDATION ~ SYMBIONT+(1|BLOCK), data=mckeon,
                    family=binomial(link='logit'))


## ----fitModel1b, cache=TRUE,results='markdown', eval=TRUE, hidden=TRUE--------
mckeon.glmer2 <- update(mckeon.glmer1,  ~ . - (1|BLOCK) + (SYMBIONT|BLOCK))


## ----fitModel1c1, cache=TRUE,results='markdown', eval=FALSE, echo=FALSE, hidden=TRUE----
## ## Try the Nelder_Mead optimizer
## mckeon.glmer2 <- update(mckeon.glmer1,  ~ . - (1|BLOCK) + (SYMBIONT|BLOCK),
##                         control=glmerControl(optimizer="Nelder_Mead"))
## ## Try the L-BFGS-B optimizer
## mckeon.glmer2 <- update(mckeon.glmer1,  ~ . - (1|BLOCK) + (SYMBIONT|BLOCK),
##                         control=glmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
## ## Try the nlminb optimizer
## mckeon.glmer2 <- update(mckeon.glmer1,  ~ . - (1|BLOCK) + (SYMBIONT|BLOCK),
##                         control=glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))
## AICc(mckeon.glmer1,  mckeon.glmer2)


## ----fitModel1c, cache=TRUE,results='markdown', eval=TRUE, hidden=TRUE--------
mckeon.allFit <- allFit(mckeon.glmer2)
## Check which of the models are considered valid (OK)
is.OK <- sapply(mckeon.allFit, is, "merMod")
is.OK


## ----fitModel1d, cache=TRUE,results='markdown', eval=TRUE, hidden=TRUE--------
diff_optims.OK <- mckeon.allFit[is.OK]
lapply(diff_optims.OK,function(x) x@optinfo$conv$lme4$messages)


## ----fitModel1e, cache=TRUE,results='markdown', echo=FALSE, eval=FALSE, hidden=TRUE----
## ss <- summary(mckeon.allFit)
## ## determine which models are valid
## ss$which.OK
## zapsmall(diff(ss$llik))
## ss$fixef


## ----fitModel1f, cache=TRUE,results='markdown', eval=TRUE, hidden=TRUE--------
mckeon.glmer2 <- update(mckeon.glmer1,  ~ . - (1|BLOCK) + (SYMBIONT|BLOCK),
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
mckeon.glmmTMB1 = glmmTMB(PREDATION ~ SYMBIONT+(1|BLOCK), data=mckeon,
                         family=binomial(link='logit'),  REML=TRUE)


## ----fitModel2b, cache=TRUE,results='markdown', eval=TRUE, hidden=TRUE, error=TRUE----
mckeon.glmmTMB2 <- update(mckeon.glmmTMB1, ~ . - (1|BLOCK) + (SYMBIONT|BLOCK))
mckeon.glmmTMB2 <- update(mckeon.glmmTMB1, ~ . - (1|BLOCK) + (SYMBIONT|BLOCK),
                          control=glmmTMBControl(optimizer=optim,
                                                 optArgs = list(method='SANN')))
AICc(mckeon.glmmTMB1,  mckeon.glmmTMB2)


## ----validation1a, results='markdown', eval=TRUE, hidden=TRUE,warning=FALSE,message=FALSE----
plot_grid(plot_model(mckeon.glmer1,  type='diag'))


## ----validation1b, results='markdown', eval=TRUE, hidden=TRUE,warning=FALSE,message=FALSE,fig.width=7,fig.height=7----
performance::check_model(mckeon.glmer1)


## ----validation1c, results='markdown', eval=TRUE, hidden=TRUE,warning=FALSE,message=FALSE,fig.width=7,fig.height=4----
mckeon.resid = simulateResiduals(mckeon.glmer1,  plot=TRUE)
testZeroInflation(mckeon.resid)


## ----validation2a, results='markdown', eval=TRUE, hidden=TRUE,warning=FALSE,message=FALSE----
plot_grid(plot_model(mckeon.glmmTMB1,  type='diag'))


## ----validation2b, results='markdown', eval=TRUE, hidden=TRUE,warning=FALSE,message=FALSE,fig.width=7,fig.height=7----
performance::check_model(mckeon.glmmTMB1)


## ----validation2c, results='markdown', eval=TRUE, hidden=TRUE,warning=FALSE,message=FALSE,fig.width=7,fig.height=4----
mckeon.resid = simulateResiduals(mckeon.glmmTMB1,  plot=TRUE)
testZeroInflation(mckeon.resid)
testDispersion(mckeon.resid)
testZeroInflation(mckeon.resid)
#testTemporalAutocorrelation(mckeon.resid)


## ----partialPlots1a, results='markdown', eval=TRUE, hidden=TRUE---------------
plot_model(mckeon.glmer1,  type='eff')


## ----partialPlots1b, results='markdown', eval=TRUE, hidden=TRUE---------------
plot(allEffects(mckeon.glmer1))


## ----partialPlots1c, results='markdown', eval=TRUE, hidden=TRUE---------------
ggpredict(mckeon.glmer1) %>% plot


## ----partialPlots1d, results='markdown', eval=TRUE, hidden=TRUE---------------
ggemmeans(mckeon.glmer1,  ~SYMBIONT) %>% plot


## ----partialPlots2a, results='markdown', eval=TRUE, hidden=TRUE---------------
plot_model(mckeon.glmmTMB1,  type='eff')


## ----partialPlots2b, results='markdown', eval=TRUE, hidden=TRUE---------------
plot(allEffects(mckeon.glmmTMB1))


## ----partialPlots2c, results='markdown', eval=TRUE, hidden=TRUE---------------
ggpredict(mckeon.glmmTMB1) %>% plot


## ----partialPlots2d, results='markdown', eval=TRUE, hidden=TRUE---------------
ggemmeans(mckeon.glmmTMB1,  ~SYMBIONT) %>% plot


## ----summary1a, results='markdown', eval=TRUE, hidden=TRUE--------------------
summary(mckeon.glmer1)

## ----summary1a1, results='markdown', eval=TRUE, echo=FALSE,hidden=TRUE--------
mckeon.tidy <- tidy(mckeon.glmer1)


## ----summary1b, results='markdown', eval=TRUE, hidden=TRUE--------------------
tidy(mckeon.glmer1, effect='fixed', conf.int=TRUE, infer=c(TRUE, TRUE))
tidy(mckeon.glmer1, effect='fixed', conf.int=TRUE, infer=c(TRUE, TRUE),  exponentiate=TRUE)
tidy(mckeon.glmer1, effect='fixed', conf.int=TRUE, infer=c(TRUE, TRUE),  exponentiate=TRUE) %>% kable


## ----summary1c, results='markdown', eval=TRUE, hidden=TRUE--------------------
# warning this is only appropriate for html output
sjPlot::tab_model(mckeon.glmer1, show.se=TRUE, show.aic=TRUE)


## ----summary2a, results='markdown', eval=TRUE, hidden=TRUE--------------------
summary(mckeon.glmmTMB1)


## ----summary2a1, results='markdown', eval=TRUE, echo=FALSE,hidden=TRUE--------
mckeon.tidy <- tidy(mckeon.glmmTMB1)


## ----summary2b, results='markdown', eval=TRUE, hidden=TRUE--------------------
tidy(mckeon.glmmTMB1, effect='fixed', conf.int=TRUE, infer=c(TRUE, TRUE))
tidy(mckeon.glmmTMB1, effect='fixed', conf.int=TRUE, infer=c(TRUE, TRUE),  exponentiate=TRUE)


## ----summary2c, results='markdown', eval=TRUE, hidden=TRUE--------------------
# warning this is only appropriate for html output
sjPlot::tab_model(mckeon.glmmTMB1, show.se=TRUE, show.aic=TRUE)


## ----posteriors1a, results='markdown', eval=TRUE, hidden=TRUE-----------------
cmat=cbind(
    crab_vs_shrimp=c(0,1,-1,0),
    one_vs_both=c(0,1/2,1/2,-1),
    symbiont=c(1, -1/3, -1/3,-1/3)
)
round(crossprod(cmat),1)
## all contrasts orthogonal
emmeans(mckeon.glmer1, ~SYMBIONT, contr=list(SYMBIONT=cmat), type='link')
emmeans(mckeon.glmer1, ~SYMBIONT, contr=list(SYMBIONT=cmat), type='response')


## ----posteriors1b, results='markdown', eval=TRUE, hidden=TRUE-----------------
r.squaredGLMM(mckeon.glmer1)
     ## The delta method can be used with for all distributions and link
     ## functions, while lognormal approximation and trigamma function are
     ## limited to distributions with logarithmic link. Trigamma-estimate
     ## is recommended whenever available. Additionally, for binomial
     ## distributions, theoretical variances exist specific for each link
     ## function distribution.
performance::r2_nakagawa(mckeon.glmer1)


## ----posteriors2a, results='markdown', eval=TRUE, hidden=TRUE-----------------
cmat=cbind(
    crab_vs_shrimp=c(0,1,-1,0),
    one_vs_both=c(0,1/2,1/2,-1),
    symbiont=c(1, -1/3, -1/3,-1/3)
)
round(crossprod(cmat),1)
## all contrasts orthogonal
emmeans(mckeon.glmmTMB1, ~SYMBIONT, contr=list(SYMBIONT=cmat), type='link')
emmeans(mckeon.glmmTMB1, ~SYMBIONT, contr=list(SYMBIONT=cmat), type='response')
emmeans(mckeon.glmmTMB1, ~SYMBIONT) %>% regrid() %>% contrast(list(SYMBIONT=cmat))


## ----posteriors2b, results='markdown', eval=TRUE, hidden=TRUE-----------------
r.squaredGLMM(mckeon.glmmTMB1)
     ## The delta method can be used with for all distributions and link
     ## functions, while lognormal approximation and trigamma function are
     ## limited to distributions with logarithmic link. Trigamma-estimate
     ## is recommended whenever available. Additionally, for binomial
     ## distributions, theoretical variances exist specific for each link
     ## function distribution.
performance::r2_nakagawa(mckeon.glmmTMB1)


## ----summaryFig1a, results='markdown', eval=TRUE, hidden=TRUE-----------------
emmeans(mckeon.glmer1, ~SYMBIONT, type='response') %>%
  as.data.frame %>%
  ggplot(aes(y=prob,  x=SYMBIONT)) +
  geom_pointrange(aes(ymin=asymp.LCL,  ymax=asymp.UCL))


## ----summaryFig2a, results='markdown', eval=TRUE, hidden=TRUE-----------------
emmeans(mckeon.glmmTMB1, ~SYMBIONT, type='response') %>% 
  as.data.frame %>%
  ggplot(aes(y=prob,  x=SYMBIONT)) +
  geom_pointrange(aes(ymin=lower.CL,  ymax=upper.CL))

