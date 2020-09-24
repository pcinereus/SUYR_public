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
library(lme4)      #for glmer
library(glmmTMB)    #for glmmTMB
library(performance) #for diagnostic plots
library(see)         #for diagnostic plots


## ----readData, results='markdown', eval=TRUE----------------------------------
mckeon = read_csv('../data/mckeon.csv', trim_ws=TRUE)
glimpse(mckeon)


## ----eda, results='markdown', eval=TRUE, hidden=TRUE--------------------------
mckeon = mckeon %>%
  mutate(BLOCK=factor(BLOCK),
         SYMBIONT=factor(SYMBIONT, levels=c('none','crabs','shrimp','both')))

ggplot(mckeon, aes(y=PREDATION, x=SYMBIONT)) +
    geom_point(position=position_jitter(width=0.2, height=0))+
    facet_wrap(~BLOCK)


## ----fitModel, results='markdown', eval=FALSE, hidden=TRUE--------------------
## ##GLMER
## mckeon.glmer = glmer(PREDATION ~ SYMBIONT+(1|BLOCK), data=mckeon,
##                     family=binomial(link='logit'))
## mckeon.glmer1 <- update(mckeon.glmer,  ~ . - (1|BLOCK) + (SYMBIONT|BLOCK))
## mckeon.glmer1 <- update(mckeon.glmer,  ~ . - (1|BLOCK) + (SYMBIONT|BLOCK),
##                         control=glmerControl(optimizer="bobyqa"))
## AICc(mckeon.glmer,  mckeon.glmer1)
## 
## ## GLMMTMB
## mckeon.glmmTMB = glmmTMB(PREDATION ~ SYMBIONT+(1|BLOCK), data=mckeon,
##                          family=binomial(link='logit'),  REML=TRUE)
## mckeon.glmmTMB1 <- update(mckeon.glmmTMB, ~ . - (1|BLOCK) + (SYMBIONT|BLOCK))
## #AICc(mckeon.glmmTMB,  mckeon.glmmTMB1)


## ----validation, results='markdown', eval=FALSE, hidden=TRUE------------------
## ##GLMER
## plot_grid(plot_model(mckeon.glmer,  type='diag'))
## performance::check_model(mckeon.glmer)
## mckeon.resid = simulateResiduals(mckeon.glmer,  plot=TRUE)
## testZeroInflation(mckeon.resid)
## 
## ##glmmTMB
## plot_grid(plot_model(mckeon.glmmTMB,  type='diag'))
## performance::check_model(mckeon.glmmTMB)
## mckeon.resid = simulateResiduals(mckeon.glmmTMB,  plot=TRUE)
## plotResiduals(mckeon.resid)
## #testDispersion(mckeon.glmer)
## #testZeroInflation(mckeon.glmer)
## #testTemporalAutocorrelation(mckeon.glmer)


## ----summary, results='markdown', eval=FALSE, hidden=TRUE---------------------
## plot(allEffects(mckeon.glmer))
## #plot_model(mckeon.glmer,  type='eff')
## ggemmeans(mckeon.glmer,  ~SYMBIONT) %>% plot
## summary(mckeon.glmer)
## tidy(mckeon.glmer, effect='fixed', conf.int=TRUE)
## #confint(mckeon.glmer) #profile (based on likelihood ratio tests) confidence intervals
## #confint(mckeon.glmer, method='Wald') # fixed effects only
##                                         #confint(mckeon.glmer, method='boot') # bootstrapped
## 
## ##glmmTMB
## plot(allEffects(mckeon.glmmTMB))
## #plot_model(mckeon.glmmTMB,  type='eff')
## ggemmeans(mckeon.glmmTMB,  ~SYMBIONT) %>% plot
## summary(mckeon.glmmTMB)
## tidy(mckeon.glmmTMB, effect='fixed', conf.int=TRUE,  exponentiate=TRUE)
## #confint(mckeon.glmmTMB) #profile (based on likelihood ratio tests) confidence intervals
## #confint(mckeon.glmmTMB, method='Wald') # fixed effects only
## #confint(mckeon.glmmTMB, method='boot') # bootstrapped


## ----posteriors, results='markdown', eval=FALSE, hidden=TRUE------------------
## ##GLMER
## cmat=cbind(
##     crab_vs_shrimp=c(0,1,-1,0),
##     one_vs_both=c(0,-1/2,-1/2,1),
##     symbiont=c(1, -1/3, -1/3,-1/3)
## )
## round(crossprod(cmat),1)
## emmeans(mckeon.glmer, ~SYMBIONT, contr=list(SYMBIONT=cmat), type='link')
## emmeans(mckeon.glmer, ~SYMBIONT, contr=list(SYMBIONT=cmat), type='response')
## 
## r.squaredGLMM(mckeon.glmer)
##      ## The delta method can be used with for all distributions and link
##      ## functions, while lognormal approximation and trigamma function are
##      ## limited to distributions with logarithmic link. Trigamma-estimate
##      ## is recommended whenever available. Additionally, for binomial
##      ## distributions, theoretical variances exist specific for each link
##      ## function distribution.
## performance::r2_nakagawa(mckeon.glmer)
## 
## ##glmmTMB
## cmat=cbind(
##     crab_vs_shrimp=c(0,1,-1,0),
##     one_vs_both=c(0,-1/2,-1/2,1),
##     symbiont=c(1, -1/3, -1/3,-1/3)
## )
## round(crossprod(cmat),1)
## emmeans(mckeon.glmmTMB, ~SYMBIONT, contr=list(SYMBIONT=cmat), type='link')
## emmeans(mckeon.glmmTMB, ~SYMBIONT, contr=list(SYMBIONT=cmat), type='response')
## 
## r.squaredGLMM(mckeon.glmmTMB)
##      ## The delta method can be used with for all distributions and link
##      ## functions, while lognormal approximation and trigamma function are
##      ## limited to distributions with logarithmic link. Trigamma-estimate
##      ## is recommended whenever available. Additionally, for binomial
##      ## distributions, theoretical variances exist specific for each link
##      ## function distribution.
## performance::r2_nakagawa(mckeon.glmmTMB)


## ----summaryFig, results='markdown', eval=FALSE, hidden=TRUE------------------
## ##GLMER
## emmeans(mckeon.glmer, ~SYMBIONT, type='response') %>%
##   as.data.frame %>%
##   ggplot(aes(y=prob,  x=SYMBIONT)) +
##   geom_pointrange(aes(ymin=asymp.LCL,  ymax=asymp.UCL))
## 
## ##glmmTMB
## emmeans(mckeon.glmmTMB, ~SYMBIONT, type='response') %>%
##   as.data.frame %>%
##   ggplot(aes(y=prob,  x=SYMBIONT)) +
##   geom_pointrange(aes(ymin=lower.CL,  ymax=upper.CL))

