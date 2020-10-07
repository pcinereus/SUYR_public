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
norin = read_csv('../data/norin.csv', trim_ws=TRUE)
glimpse(norin)


## ----fitModels, results='markdown', eval=FALSE, hidden=TRUE-------------------
## norin = norin %>% mutate(FISHID=factor(FISHID),
##                          TRIAL=factor(TRIAL))
## 
## ggplot(norin, aes(y=CHANGE, x=TRIAL)) + geom_boxplot()
## ggplot(norin, aes(y=CHANGE, x=SMR_contr, shape=TRIAL, color=TRIAL)) +
##     geom_smooth(method='lm') + geom_point()
## ggplot(norin, aes(y=CHANGE, x=MASS, shape=TRIAL, color=TRIAL)) +
##     geom_smooth(method='lm') + geom_point()
## ggplot(norin, aes(y=CHANGE, x=as.numeric(FISHID), color=TRIAL)) +
##     geom_point() + geom_line()
## 
## ggplot(norin, aes(y=MASS, x=TRIAL)) + geom_boxplot()
## ggplot(norin, aes(y=CHANGE, x=MASS, color=TRIAL)) + geom_point() + geom_smooth(method='lm')
## 
## norin.rstan = stan_glmer(CHANGE ~ (1|FISHID)+TRIAL*SMR_contr+MASS, data=norin,
##                          iter=5000, warmup=2000, chains=3, thin=5, refresh=0)
## 
## norin.rstan %>% get_variables()
## plot(norin.rstan,  'mcmc_trace', regex_pars='^.Intercept|TRIAL|SMR|MASS|[sS]igma')
## plot(norin.rstan,  'mcmc_acf_bar', regex_pars='^.Intercept|TRIAL|SMR|MASS|[sS]igma')
## plot(norin.rstan,  'mcmc_rhat_hist', regex_pars='^.Intercept|TRIAL|SMR|MASS|[sS]igma')
## plot(norin.rstan,  'mcmc_neff_hist', regex_pars='^.Intercept|TRIAL|SMR|MASS|[sS]igma')
## 
## #norin.rstan1 = stan_glmer(CHANGE ~ (TRIAL|FISHID)+TRIAL*SMR_contr+MASS, data=norin,
## #                          iter=5000, warmup=2000, chains=3, thin=5, refresh=0, cores=3)
## norin.rstan1 = stan_glmer(CHANGE ~ (SMR_contr|FISHID) + TRIAL*SMR_contr+MASS, data=norin,
##                           iter=5000, warmup=200, chains=3, thin=5, refresh=0, cores=3)
## 
## norin.rstan1 %>% get_variables()
## plot(norin.rstan1,  'mcmc_trace', regex_pars='^.Intercept|TRIAL|^SMR|MASS|[sS]igma')
## plot(norin.rstan1,  'mcmc_acf_bar', regex_pars='^.Intercept|TRIAL|^SMR|MASS|[sS]igma')
## plot(norin.rstan1,  'mcmc_rhat_hist', regex_pars='^.Intercept|TRIAL|^SMR|MASS|[sS]igma')
## plot(norin.rstan1,  'mcmc_neff_hist', regex_pars='^.Intercept|TRIAL|^SMR|MASS|[sS]igma')
## 
## 
## (l.1 <- loo(norin.rstan))
## (l.2 <- loo(norin.rstan1))
## loo_compare(l.1,  l.2)
## 
## 
## preds <- posterior_predict(norin.rstan,  nsamples=250,  summary=FALSE)
## norin.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = norin$CHANGE,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(norin.resids)
## 
## 
## g=ggpredict(norin.rstan) %>% plot
## do.call('grid.arrange', g)
## 
## ggemmeans(norin.rstan, ~TRIAL)
## 
## 
## nms <- norin.rstan %>% get_variables()
## wch <- grep('^.Intercept|TRIAL|^SMR|[sS]igma', nms)
## tidyMCMC(norin.rstan$stanfit,conf.int=TRUE, conf.method='HPDinterval',
##          rhat=TRUE, ess=TRUE, pars=nms[wch], estimate.method='median')
## 
## 
## norin.grid = with(norin, list(SMR_contr=seq(min(SMR_contr),max(SMR_contr), len=100)))
## newdata = emmeans(norin.rstan, ~TRIAL|SMR_contr, at=norin.grid) %>% as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=emmean, x=SMR_contr, color=TRIAL)) +
##     geom_ribbon(aes(ymin=lower.HPD, ymax=upper.HPD, fill=TRIAL), alpha=0.3,color=NA) +
##     geom_line()
## 
## norin.grid = with(norin, list(SMR_contr=c(min(SMR_contr),mean(SMR_contr),max(SMR_contr))))
## 
## emmeans(norin.rstan, pairwise~TRIAL|SMR_contr, at=norin.grid)
## 
## norin.em = emmeans(norin.rstan, pairwise~TRIAL|SMR_contr, at=norin.grid)$contrast %>%
##               gather_emmeans_draws() %>%
##               mutate(Fit=.value)
## norin.em %>%
##     group_by(contrast, SMR_contr) %>%
##     summarize(P=sum(Fit>0)/n())
## 
## 
## bayes_R2(norin.rstan, re.form=~(1|FISHID)) %>% median_hdi
## #bayes_R2(norin.rstan1, re.form=~(SMR_contr|FISHID)) %>% median_hdi
## bayes_R2(norin.rstan, re.form=NA) %>% median_hdi
## 


## ----fitModels.brms, results='markdown', eval=FALSE, hidden=TRUE--------------
## norin = norin %>% mutate(FISHID=factor(FISHID),
##                          TRIAL=factor(TRIAL))
## 
## ggplot(norin, aes(y=CHANGE, x=TRIAL)) + geom_boxplot()
## ggplot(norin, aes(y=CHANGE, x=SMR_contr, shape=TRIAL, color=TRIAL)) +
##     geom_smooth(method='lm') + geom_point()
## ggplot(norin, aes(y=CHANGE, x=MASS, shape=TRIAL, color=TRIAL)) +
##     geom_smooth(method='lm') + geom_point()
## ggplot(norin, aes(y=CHANGE, x=as.numeric(FISHID), color=TRIAL)) +
##     geom_point() + geom_line()
## 
## ggplot(norin, aes(y=MASS, x=TRIAL)) + geom_boxplot()
## ggplot(norin, aes(y=CHANGE, x=MASS, color=TRIAL)) + geom_point() + geom_smooth(method='lm')
## 
## 
## norin.form <- bf(CHANGE ~ (1|FISHID)+TRIAL*SMR_contr+MASS, family=gaussian)
## 
## norin.brms = brm(norin.form, data=norin,
##                  iter=5000, warmup=2000, chains=3, thin=5, refresh=0)
## 
## norin.brms %>% get_variables()
## mcmc_plot(norin.brms,  type='trace',
##           regex_pars='^b.Intercept|TRIAL|SMR|MASS|[sS]igma|^sd')
## mcmc_plot(norin.brms,  type='acf_bar',
##           regex_pars='^b.Intercept|TRIAL|SMR|MASS|[sS]igma|^sd')
## mcmc_plot(norin.brms,  type='rhat_hist',
##           regex_pars='^b.Intercept|TRIAL|SMR|MASS|[sS]igma|^sd')
## mcmc_plot(norin.brms,  type='neff_hist',
##           regex_pars='^b.Intercept|TRIAL|SMR|MASS|[sS]igma|^sd')
## 
## #norin.rstan1 = stan_glmer(CHANGE ~ (TRIAL|FISHID)+TRIAL*SMR_contr+MASS, data=norin,
## #                          iter=5000, warmup=2000, chains=3, thin=5, refresh=0, cores=3)
## norin.form <- bf(CHANGE ~ (SMR_contr|FISHID) + TRIAL*SMR_contr+MASS, family=gaussian)
## norin.brms1 = brm(norin.form, data=norin,
##                   iter=5000, warmup=2000, chains=3, thin=5, refresh=0, cores=3,
##                   control=list(adapt_delta=0.99))
## 
## norin.brms1 %>% get_variables()
## mcmc_plot(norin.brms1,  type='trace',
##           regex_pars='^b.Intercept|TRIAL|SMR|MASS|[sS]igma|^sd')
## mcmc_plot(norin.brms1,  type='acf_bar',
##           regex_pars='^b.Intercept|TRIAL|SMR|MASS|[sS]igma|^sd')
## mcmc_plot(norin.brms1,  type='rhat_hist',
##           regex_pars='^b.Intercept|TRIAL|SMR|MASS|[sS]igma|^sd')
## mcmc_plot(norin.brms1,  type='neff_hist',
##           regex_pars='^b.Intercept|TRIAL|SMR|MASS|[sS]igma|^sd')
## 
## 
## 
## (l.1 <- loo(norin.brms))
## (l.2 <- loo(norin.brms1))
## loo_compare(l.1,  l.2)
## 
## 
## preds <- posterior_predict(norin.brms1,  nsamples=250,  summary=FALSE)
## norin.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = norin$CHANGE,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(norin.resids)
## 
## 
## g=ggpredict(norin.brms1) %>% plot
## do.call('grid.arrange', g)
## 
## ggemmeans(norin.brms1, ~TRIAL)
## 
## 
## nms <- norin.brms1 %>% get_variables()
## wch <- grep('^b.Intercept|TRIAL|^b.*SMR|[sS]igma|^sd', nms)
## tidyMCMC(norin.brms1$fit,conf.int=TRUE, conf.method='HPDinterval',
##          rhat=TRUE, ess=TRUE, pars=nms[wch], estimate.method='median')
## 
## 
## norin.em = emmeans(norin.brms1, pairwise~TRIAL)$contrast %>%
##               gather_emmeans_draws() %>%
##               mutate(Fit=.value)
## 
## norin.em %>%
##   group_by(contrast) %>%
##   median_hdi()
## 
## norin.em %>%
##     group_by(contrast) %>%
##   summarize(P=sum(Fit>0)/n())
## 
## norin.grid = with(norin, list(SMR_contr=seq(min(SMR_contr),max(SMR_contr), len=100)))
## newdata = emmeans(norin.brms1, ~SMR_contr|TRIAL, at=norin.grid) %>% as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=emmean, x=SMR_contr, color=TRIAL)) +
##     geom_ribbon(aes(ymin=lower.HPD, ymax=upper.HPD, fill=TRIAL), alpha=0.3,color=NA) +
##     geom_line()
## 
## norin.grid = with(norin, list(SMR_contr=c(min(SMR_contr),mean(SMR_contr),max(SMR_contr))))
## 
## emmeans(norin.brms1, pairwise~TRIAL|SMR_contr, at=norin.grid)
## 
## norin.em = emmeans(norin.brms1, pairwise~TRIAL|SMR_contr, at=norin.grid)$contrast %>%
##                                                                        gather_emmeans_draws()
## norin.em %>% head
## norin.em %>%
##     group_by(contrast, SMR_contr) %>%
##     median_hdi()
## 
## norin.em %>%
##     group_by(contrast, SMR_contr) %>%
##     summarize(P=sum(.value>0)/n())
## 
## bayes_R2(norin.brms1, re.form=NA,  summary=FALSE) %>% median_hdi
## bayes_R2(norin.brms1, re.form=~(1|FISHID), summary=FALSE) %>% median_hdi
## bayes_R2(norin.brms1, re.form=~(SMR_contr|FISHID), summary=FALSE) %>% median_hdi
## 

