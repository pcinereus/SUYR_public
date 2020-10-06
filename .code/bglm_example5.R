## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE)


## ----libraries, results='markdown', eval=TRUE---------------------------------
library(rstanarm)   #for fitting models in STAN
library(brms)       #for fitting models in STAN
library(coda)       #for diagnostics
library(bayesplot)  #for diagnostics
library(rstan)      #for interfacing with STAN
library(DHARMa)     #for residual diagnostics
library(emmeans)    #for marginal means etc
library(broom)      #for tidying outputs
library(tidybayes)  #for more tidying outputs
library(ggeffects)  #for partial plots
library(tidyverse)  #for data wrangling etc


## ----readData, results='markdown', eval=TRUE----------------------------------
day = read_csv('../data/day.csv', trim_ws=TRUE)
glimpse(day)


## ----prepare, results='markdown', eval=TRUE, hidden=TRUE----------------------
day = day %>% mutate(TREAT=factor(TREAT))


## ----fitModel, results='markdown', eval=FALSE, hidden=TRUE--------------------
## day.rstanarm <- stan_glm(BARNACLE ~ TREAT, data=day,
##                       family=poisson(link='log'),
##                       chains = 3,iter = 5000, warmup=2000, thin=5,
##                       refresh=0)
## prior_summary(day.rstanarm)
## 
## day.rstan <- stan_glm(BARNACLE ~ TREAT, data=day, family='poisson',
##                       prior = normal(c(0,0,0), c(2.5,2.5,2.5)),
##                       prior_intercept = normal(0,10),
##                       chains = 3,iter = 2000, thin=2, refresh=0)
## prior_summary(day.rstan)
## 
## plot(day.rstanarm,  'mcmc_trace')
## plot(day.rstanarm,  'mcmc_acf_bar')
## plot(day.rstanarm,  'mcmc_rhat_hist')
## plot(day.rstanarm,  'mcmc_neff_hist')
## 
## 
## preds <- posterior_predict(day.rstanarm,  nsamples=250,  summary=FALSE)
## day.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = day$BARNACLE,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(day.resids)
## 
## 
## #pp_check(day.rstanarm, x=as.numeric(day$TREAT),'intervals')
## 
## 
## 
## 
## ## Compare the proportion of zeros in the observed and expected data
## #yrep = posterior_predict(day.rstan)
## prop_zero <- function(y) mean(y == 0)
## (prop_zero_test1 <- pp_check(day.rstanarm, plotfun = "stat", stat = "prop_zero"))
##                                         # no zeros - so not zero inflated
## 
## 
## day.rstanarmNB <- stan_glm(BARNACLE ~ TREAT, data=day,
##                       family='neg_binomial_2',
##                       chains = 3,iter = 5000, thin=5, warmup=2000, refresh=0)
## preds <- posterior_predict(day.rstanarmNB,  nsamples=250,  summary=FALSE)
## day.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = day$BARNACLE,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(day.resids)
## 
## 
## day.rstanNB <- update(day.rstan, family = neg_binomial_2)
## (loo.P=loo(day.rstan))
## (loo.NB=loo(day.rstanNB))
## compare_models(loo.P, loo.NB)
## 
## ggpredict(day.rstanarm, term='TREAT') %>% plot
## ggpredict(day.rstanarm, ~TREAT) %>% plot
## ggemmeans(day.rstanarm, ~TREAT) %>% plot
## 
## 
## summary(day.rstanarm)
## library(tidybayes)
## tidyMCMC(day.rstanarm$stanfit, conf.int=TRUE,
##          conf.method='HPDinterval', rhat=TRUE,ess=TRUE)
## 
## 
## # Pairwise comparisons
## library(emmeans)
## ## factor statements
## emmeans(day.rstanarm, pairwise~TREAT, type='response')
## ## what about probabilities
## day.em = emmeans(day.rstanarm, pairwise~TREAT, type='link')$contrasts %>%
##     gather_emmeans_draws() %>%
##     mutate(Fit=exp(.value))
## day.em %>% head
## day.em %>% group_by(contrast) %>%
##     ggplot(aes(x=Fit)) +
##     geom_histogram() +
##     geom_vline(xintercept=1, color='red') +
##     facet_wrap(~contrast, scales='free')
## day.em %>% group_by(contrast) %>% median_hdi()
## # Probability of effect
## day.em %>% group_by(contrast) %>% summarize(P=sum(Fit>1)/n())
## ##Probability of effect greater than 10%
## day.em %>% group_by(contrast) %>% summarize(P=sum(Fit>1.1)/n())
## 
## 
## ##Planned contrasts
## cmat<-cbind('Alg2_Alg1'=c(-1,1,0,0),
##               'NB_S'=c(0,0,1,-1),
##              'Alg_Bare'=c(0.5,0.5,-0.5,-0.5),
##              'Alg_NB'=c(0.5,0.5,-1,0))
## #crossprod(cmat)
## emmeans(day.rstanarm, ~TREAT, contr=list(TREAT=cmat), type='link')
## emmeans(day.rstanarm, ~TREAT, contr=list(TREAT=cmat), type='response')
## day.em = emmeans(day.rstanarm, ~TREAT, contr=list(TREAT=cmat), type='link')$contrasts %>%
##       gather_emmeans_draws() %>% mutate(Fit=exp(.value))
## day.em %>% group_by(contrast) %>% mean_hdi()
## # Probability of effect
## day.em %>% group_by(contrast) %>% summarize(P=sum(Fit>1)/n())
## ##Probability of effect greater than 10%
## day.em %>% group_by(contrast) %>% summarize(P=sum(Fit>1.5)/n())
## 
## hist(bayes_R2(day.rstanarmNB))
## 
## bayes_R2(day.rstanarm) %>% median_hdi
## bayes_R2(day.rstanarmNB) %>% hist
## 
## 
## ## Summary plot
## day.grid = with(day, list(TREAT=levels(TREAT)))
## newdata = emmeans(day.rstanarm, ~TREAT, type='response') %>% as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=rate, x=TREAT)) +
##     geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD))


## ----fitModel.brms, results='markdown', eval=FALSE, hidden=TRUE---------------
## day.form <- bf(BARNACLE ~ TREAT,  family=poisson(link='log'))
## get_prior(day.form,  data=day)
## day.brmsP <- brm(day.form, data=day,
##                  chains=3,  iter=5000,  warmup=2000, thin=5,
##                  refresh=0)
## 
## plot(day.brmsP)
## mcmc_plot(day.brmsP,  type='acf_bar')
## mcmc_plot(day.brmsP,  type='rhat_hist')
## mcmc_plot(day.brmsP,  type='neff_hist')
## 
## preds <- posterior_predict(day.brmsP,  nsamples=250,  summary=FALSE)
## day.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = day$BARNACLE,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(day.resids)
## 
##                                         #pp_check(day.brmsP, x=as.numeric(day$TREAT),'intervals')
## 
## 
## ggpredict(day.brmsP, term='TREAT') %>% plot
## ggpredict(day.brmsP, ~TREAT) %>% plot
## ggemmeans(day.brmsP, ~TREAT) %>% plot
## 
## summary(day.brmsP)
## 
## tidyMCMC(day.brmsP$fit, conf.int=TRUE,
##          conf.method='HPDinterval', rhat=TRUE,ess=TRUE)
## 
## # Pairwise comparisons
## library(emmeans)
## ## factor statements
## emmeans(day.brmsP, pairwise~TREAT, type='response')
## ## what about probabilities
## day.em = emmeans(day.brmsP, pairwise~TREAT, type='link')$contrasts %>%
##     gather_emmeans_draws() %>%
##     mutate(Fit=exp(.value))
## day.em %>% head
## day.em %>% group_by(contrast) %>%
##     ggplot(aes(x=Fit)) +
##     geom_histogram() +
##     geom_vline(xintercept=1, color='red') +
##     facet_wrap(~contrast, scales='free')
## day.em %>% group_by(contrast) %>% median_hdi()
## # Probability of effect
## day.em %>% group_by(contrast) %>% summarize(P=sum(.value>0)/n())
## day.em %>% group_by(contrast) %>% summarize(P=sum(Fit>1)/n())
## ##Probability of effect greater than 10%
## day.em %>% group_by(contrast) %>% summarize(P=sum(Fit>1.1)/n())
## 
## ##Planned contrasts
## cmat<-cbind('Alg2_Alg1'=c(-1,1,0,0),
##               'NB_S'=c(0,0,1,-1),
##              'Alg_Bare'=c(0.5,0.5,-0.5,-0.5),
##              'Alg_NB'=c(0.5,0.5,-1,0))
## #crossprod(cmat)
## emmeans(day.brmsP, ~TREAT, contr=list(TREAT=cmat), type='link')
## emmeans(day.brmsP, ~TREAT, contr=list(TREAT=cmat), type='response')
## day.em = emmeans(day.brmsP, ~TREAT, contr=list(TREAT=cmat), type='link')$contrasts %>%
##       gather_emmeans_draws() %>% mutate(Fit=exp(.value))
## day.em %>% group_by(contrast) %>% mean_hdi()
## # Probability of effect
## day.em %>% group_by(contrast) %>% summarize(P=sum(Fit>1)/n())
## ##Probability of effect greater than 10%
## day.em %>% group_by(contrast) %>% summarize(P=sum(Fit>1.5)/n())
## 
## hist(bayes_R2(day.brmsP, summary=FALSE))
## 
## bayes_R2(day.brmsP, summary=FALSE) %>% median_hdi
## 
## 
## ## Summary plot
## day.grid = with(day, list(TREAT=levels(TREAT)))
## newdata = emmeans(day.brmsP, ~TREAT, type='response') %>% as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=rate, x=TREAT)) +
##     geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD))

