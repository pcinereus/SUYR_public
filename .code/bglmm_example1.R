## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE)


## ----libraries, results='markdown', eval=TRUE, message=FALSE, warning=FALSE----
library(rstanarm)   #for fitting models in STAN
library(brms)       #for fitting models in STAN
library(coda)       #for diagnostics
library(bayesplot)  #for diagnostics
library(rstan)      #for interfacing with STAN
library(emmeans)    #for marginal means etc
library(broom)      #for tidying outputs
library(DHARMa)     #for residual diagnostics
library(tidybayes)  #for more tidying outputs
library(ggeffects)  #for partial plots
library(tidyverse)  #for data wrangling etc


## ----readData, results='markdown', eval=TRUE----------------------------------
tobacco = read_csv('../data/tobacco.csv', trim_ws=TRUE)
glimpse(tobacco)


## ----fitModel, results='markdown', eval=FALSE, hidden=TRUE--------------------
## tobacco = tobacco %>% mutate(LEAF=factor(LEAF),
##                              TREATMENT=factor(TREATMENT))
## head(tobacco)
## 
## ggplot(tobacco, aes(y=NUMBER,x=TREATMENT)) + geom_boxplot()
## ggplot(tobacco, aes(y=NUMBER,x=as.numeric(LEAF), linetype=TREATMENT)) + geom_line()
## ggplot(tobacco, aes(y=NUMBER,x=as.numeric(LEAF), linetype=TREATMENT)) + geom_blank(aes(x=LEAF)) + geom_line()
## 
## tobacco.rstan <- stan_glmer(NUMBER~(1|LEAF)+TREATMENT, data=tobacco,
##                            family=gaussian,refresh=0,
##                            chains=3, iter=5000, warmup=2000, thin=5)
## prior_summary(tobacco.rstan)
## ## decov is prior on covariance
## 
## 
## plot(tobacco.rstan,  'mcmc_trace')
## plot(tobacco.rstan,  'mcmc_acf_bar')
## plot(tobacco.rstan,  'mcmc_rhat_hist')
## plot(tobacco.rstan,  'mcmc_neff_hist')
## 
## posterior_vs_prior(tobacco.rstan, color_by='vs', group_by=TRUE,
##                    facet_args=list(scales='free_y'), pars=nms[wch])
## pp_check(tobacco.rstan, group='TREATMENT', plotfun='violin_grouped')
## 
## tobacco.rstan1 <- stan_glmer(NUMBER~(TREATMENT|LEAF)+TREATMENT, data=tobacco,
##                            family=gaussian,refresh=0,
##                            chains=3, iter=5000, warmup=2000, thin=5)
## 
## tobacco.rstan1 <- stan_glmer(NUMBER~(TREATMENT|LEAF)+TREATMENT, data=tobacco,
##                            family=gaussian,refresh=0,
##                            chains=3, iter=5000, warmup=2000, thin=5,
##                            adapt_delta=0.99)
## 
## plot(tobacco.rstan1,  'mcmc_trace')
## plot(tobacco.rstan1,  'mcmc_acf_bar')
## plot(tobacco.rstan1,  'mcmc_rhat_hist')
## plot(tobacco.rstan1,  'mcmc_neff_hist')
## 
## 
## preds <- posterior_predict(tobacco.rstan1,  nsamples=250,  summary=FALSE)
## tobacco.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = tobacco$NUMBER,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(tobacco.resids)
## 
## (l.1 <- loo(tobacco.rstan))
## (l.2 <- loo(tobacco.rstan1))
## loo_compare(l.1, l.2)
## 
## preds <- posterior_predict(tobacco.rstan,  nsamples=250,  summary=FALSE)
## tobacco.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = tobacco$NUMBER,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(tobacco.resids)
## 
## 
## ggpredict(tobacco.rstan) %>% plot
## ##ggemmeans(tobacco.rstan, ~TREATMENT) %>% plot
## 
## summary(tobacco.rstan)
## tidyMCMC(tobacco.rstan$stanfit,conf.int=TRUE, conf.method='HPDinterval',
##          rhat=TRUE, ess=TRUE, estimate.method='median')
## 
## bayes_R2(tobacco.rstan, re.form=~(1|LEAF)) %>% median_hdi
## bayes_R2(tobacco.rstan, re.form=NA) %>% median_hdi
## 
## newdata = emmeans(tobacco.rstan, ~TREATMENT) %>%
##     gather_emmeans_draws() %>%
##     spread(key=TREATMENT, value=.value)
## newdata
## newdata = newdata %>% mutate(Eff = Strong-Weak,
##                    PEff = 100*(Strong-Weak)/Weak)
## newdata %>% dplyr::select(Eff,PEff) %>% median_hdi
## newdata %>% summarize(Prob = sum(PEff>0)/n())
## newdata %>% summarize(Prob = sum(PEff>20)/n())
## hypothesis(newdata,  'PEff>0')
## hypothesis(newdata,  'PEff>20')
## 
## 
## newdata = emmeans(tobacco.rstan, ~TREATMENT) %>% as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=emmean, x=TREATMENT)) +
##     geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD))


## ----fitModel.brms, results='markdown', eval=FALSE, hidden=TRUE---------------
## tobacco = tobacco %>% mutate(LEAF=factor(LEAF),
##                              TREATMENT=factor(TREATMENT))
## head(tobacco)
## 
## ggplot(tobacco, aes(y=NUMBER,x=TREATMENT)) + geom_boxplot()
## ggplot(tobacco, aes(y=NUMBER,x=as.numeric(LEAF), linetype=TREATMENT)) + geom_line()
## ggplot(tobacco, aes(y=NUMBER,x=as.numeric(LEAF), linetype=TREATMENT)) + geom_blank(aes(x=LEAF)) + geom_line()
## 
## tobacco.form <- bf(NUMBER~(1|LEAF)+TREATMENT,  family=gaussian)
## 
## tobacco.brms <- brm(tobacco.form, data=tobacco,
##                            refresh=0,
##                            chains=3, iter=5000, warmup=2000, thin=5)
## tobacco.brms <- brm(tobacco.form, data=tobacco,
##                            refresh=0,
##                     chains=3, iter=5000, warmup=2000, thin=5,
##                     control=list(adapt_delta=0.99))
## 
## 
## mcmc_plot(tobacco.brms,  type='trace')
## mcmc_plot(tobacco.brms,  type='acf_bar')
## mcmc_plot(tobacco.brms,  type='rhat_hist')
## mcmc_plot(tobacco.brms,  type='neff_hist')
## 
## tobacco.form <- bf(NUMBER~(TREATMENT|LEAF)+TREATMENT,  family=gaussian)
## tobacco.brms1 <- brm(tobacco.form, data=tobacco,
##                      refresh=0,
##                      chains=3, iter=10000, warmup=5000, thin=10,
##                      control=list(adapt_delta=0.99))
## 
## mcmc_plot(tobacco.brms1,  type='trace')
## mcmc_plot(tobacco.brms1,  type='acf_bar')
## mcmc_plot(tobacco.brms1,  type='rhat_hist')
## mcmc_plot(tobacco.brms1,  type='neff_hist')
## 
## (l.1 <- loo(tobacco.brms))
## (l.2 <- loo(tobacco.brms1))
## loo_compare(l.1, l.2)
## 
## preds <- posterior_predict(tobacco.brms,  nsamples=250,  summary=FALSE)
## tobacco.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = tobacco$NUMBER,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(tobacco.resids)
## 
## 
## ggpredict(tobacco.brms) %>% plot
## ##ggemmeans(tobacco.rstan, ~TREATMENT) %>% plot
## 
## summary(tobacco.brms)
## tidyMCMC(tobacco.brms$fit,conf.int=TRUE, conf.method='HPDinterval',
##          rhat=TRUE, ess=TRUE, estimate.method='median')
## 
## bayes_R2(tobacco.brms, re.form=~(1|LEAF), summary=FALSE) %>% median_hdi
## bayes_R2(tobacco.brms, re.form=NA,  summary=FALSE) %>% median_hdi
## 
## newdata = emmeans(tobacco.brms, ~TREATMENT) %>%
##     gather_emmeans_draws() %>%
##     spread(key=TREATMENT, value=.value)
## newdata
## newdata = newdata %>% mutate(Eff = Strong-Weak,
##                    PEff = 100*(Strong-Weak)/Weak)
## newdata %>% dplyr::select(Eff,PEff) %>% median_hdi
## newdata %>% summarize(Prob = sum(PEff>0)/n())
## newdata %>% summarize(Prob = sum(PEff>20)/n())
## hypothesis(newdata,  'PEff>0')
## newdata %>% summarize(Prob = sum(PEff>50)/n())
## hypothesis(newdata,  'PEff>20')
## 
## 
## newdata = emmeans(tobacco.brms, ~TREATMENT) %>% as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=emmean, x=TREATMENT)) +
##     geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD))

