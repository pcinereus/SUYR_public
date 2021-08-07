## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE)


## ----libraries, results='markdown', eval=TRUE, message=FALSE, warning=FALSE----
library(rstanarm)   #for fitting models in STAN
library(brms)       #for fitting models in STAN
library(coda)       #for diagnostics
library(bayesplot)  #for diagnostics
library(ggmcmc)     #for MCMC diagnostics
library(rstan)      #for interfacing with STAN
library(emmeans)    #for marginal means etc
library(broom)      #for tidying outputs
library(DHARMa)     #for residual diagnostics
library(tidybayes)  #for more tidying outputs
library(ggeffects)  #for partial plots
library(broom.mixed)
library(tidyverse)  #for data wrangling etc


## ----readData, results='markdown', eval=TRUE----------------------------------
tobacco = read_csv('../data/tobacco.csv', trim_ws=TRUE)
glimpse(tobacco)


## ----processData, results='markdown', eval=TRUE-------------------------------
tobacco = tobacco %>% mutate(LEAF=factor(LEAF),
                             TREATMENT=factor(TREATMENT))
head(tobacco)


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


## ----fitModel1a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
tobacco.rstanarm = stan_glmer(NUMBER ~ (1|LEAF) + TREATMENT, data=tobacco,
                        family=gaussian(), 
                         iter = 5000, warmup = 2000,
                         chains = 3, thin = 5, refresh = 0)


## ----fitModel1b, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
prior_summary(tobacco.rstanarm)


## ----fitModel1c, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
2.5*sd(tobacco$NUMBER)


## ----fitModel1d, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
2.5*sd(tobacco$NUMBER)/sd(model.matrix(~TREATMENT, tobacco)[, 2])


## ----fitModel1e, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
1/sd(tobacco$NUMBER)


## ----fitModel1f, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
tobacco.rstanarm1 <- update(tobacco.rstanarm,  prior_PD=TRUE)


## ----fitModel1g, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
ggpredict(tobacco.rstanarm1) %>% plot(add.data=TRUE)


## ----fitModel1h, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
tobacco.rstanarm2= stan_glmer(NUMBER ~ (1|LEAF) + TREATMENT, data=tobacco,
                              family=gaussian(), 
                              prior_intercept = normal(35, 10, autoscale=FALSE),
                              prior = normal(0, 10, autoscale=FALSE),
                              prior_aux=rstanarm::exponential(0.1, autoscale=FALSE),
                              prior_covariance=decov(1, 1, 1, 1), 
                              prior_PD=TRUE, 
                              iter = 5000, warmup = 1000,
                              chains = 3, thin = 5, refresh = 0
                              )


## ----fitModel1i, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
ggpredict(tobacco.rstanarm2) %>%
  plot(add.data=TRUE)


## ----fitModel2a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
tobacco.brm = brm(bf(NUMBER ~ (1|LEAF) + TREATMENT), data=tobacco,
              family=gaussian(), 
              iter = 5000, warmup = 1000,
              chains = 3, thin = 5, refresh = 0)


## ----fitModel2b, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE, paged.print=FALSE,tidy.opts = list(width.cutoff = 80), echo=2----
options(width=100)
prior_summary(tobacco.brm)
options(width=80)


## ----fitModel, results='markdown', eval=FALSE, hidden=TRUE--------------------
## 
## ggplot(tobacco, aes(y=NUMBER,x=TREATMENT)) + geom_boxplot()
## ggplot(tobacco, aes(y=NUMBER,x=as.numeric(LEAF), linetype=TREATMENT)) + geom_line()
## ggplot(tobacco, aes(y=NUMBER,x=TREATMENT, group=LEAF)) + geom_line()
## 
## tobacco.rstanarm <- stan_glmer(NUMBER~(1|LEAF)+TREATMENT, data=tobacco,
##                            family=gaussian,refresh=0,
##                             prior_PD=TRUE,
##                            chains=3, iter=5000, warmup=2000, thin=5)
## prior_summary(tobacco.rstanarm)
## ## decov is prior on covariance
## 
## tobacco.rstanarm <- stan_glmer(NUMBER~(1|LEAF)+TREATMENT, data=tobacco,
##                             family=gaussian,refresh=0,
##                             prior_intercept=normal(35, 10, autoscale=FALSE),
##                             prior=normal(0, 10, autoscale=FALSE),
##                             prior_aux=rstanarm::exponential(0.1, autoscale=FALSE),
##                             prior_covariance=decov(1, 1, 1, 1),
##                             prior_PD=TRUE,
##                            chains=3, iter=5000, warmup=2000, thin=5)
## 
## prior_summary(tobacco.rstanarm)
## 
## ggpredict(tobacco.rstanarm) %>% plot(add.data=TRUE)
## 
## tobacco.rstanarm <- update(tobacco.rstanarm, prior_PD=FALSE)
## 
## ggpredict(tobacco.rstanarm) %>% plot(add.data=TRUE)
## 
## posterior_vs_prior(tobacco.rstanarm, color_by='vs', group_by=TRUE,
##                    facet_args=list(scales='free_y'))
## 
## 
## 
## tobacco.rstanarm1 <- stan_glmer(NUMBER~(TREATMENT|LEAF)+TREATMENT, data=tobacco,
##                             family=gaussian,refresh=0,
##                             prior_PD=TRUE,
##                            chains=3, iter=5000, warmup=2000, thin=5)
## prior_summary(tobacco.rstanarm1)
## 
## ggpredict(tobacco.rstanarm1) %>% plot(add.data=TRUE)
## 
## tobacco.rstanarm1 <- stan_glmer(NUMBER~(TREATMENT|LEAF)+TREATMENT, data=tobacco,
##                             family=gaussian,refresh=0,
##                             prior_intercept=normal(35, 10, autoscale=FALSE),
##                             prior=normal(0, 10, autoscale=FALSE),
##                             prior_aux=rstanarm::exponential(0.1, autoscale=FALSE),
##                             prior_covariance=decov(1, 1, 1, 1),
##                             prior_PD=TRUE,
##                            chains=3, iter=5000, warmup=2000, thin=5)
## 
## tobacco.rstanarm1 <- stan_glmer(NUMBER~(TREATMENT|LEAF)+TREATMENT, data=tobacco,
##                             family=gaussian,refresh=0,
##                             prior_intercept=normal(35, 10, autoscale=FALSE),
##                             prior=normal(0, 10, autoscale=FALSE),
##                             prior_aux=rstanarm::exponential(0.1, autoscale=FALSE),
##                             prior_covariance=decov(1, 1, 1, 1),
##                             prior_PD=TRUE,
##                             chains=3, iter=5000, warmup=2000, thin=5,
##                             adapt_delta=0.99)
## 
## ggpredict(tobacco.rstanarm1) %>% plot(add.data=TRUE)
## tobacco.rstanarm1 <- update(tobacco.rstanarm1, prior_PD=FALSE)
## tobacco.rstanarm1 <- update(tobacco.rstanarm1, prior_PD=FALSE, adapt_delta=0.99)
## 
## ggpredict(tobacco.rstanarm1) %>% plot(add.data=TRUE)
## 
## posterior_vs_prior(tobacco.rstanarm1, color_by='vs', group_by=TRUE,
##                    facet_args=list(scales='free_y'))
## 
## 
## plot(tobacco.rstanarm1,  'mcmc_trace')
## plot(tobacco.rstanarm1,  'mcmc_acf_bar')
## plot(tobacco.rstanarm1,  'mcmc_rhat_hist')
## plot(tobacco.rstanarm1,  'mcmc_neff_hist')
## 
## pp_check(tobacco.rstan)
## pp_check(tobacco.rstan, group='TREATMENT', plotfun='violin_grouped')
## 
## ## tobacco.rstan1 <- stan_glmer(NUMBER~(TREATMENT|LEAF)+TREATMENT, data=tobacco,
## ##                            family=gaussian,refresh=0,
## ##                            chains=3, iter=5000, warmup=2000, thin=5)
## 
## ## tobacco.rstan1 <- stan_glmer(NUMBER~(TREATMENT|LEAF)+TREATMENT, data=tobacco,
## ##                            family=gaussian,refresh=0,
## ##                            chains=3, iter=5000, warmup=2000, thin=5,
## ##                            adapt_delta=0.99)
## 
## ## plot(tobacco.rstan1,  'mcmc_trace')
## ## plot(tobacco.rstan1,  'mcmc_acf_bar')
## ## plot(tobacco.rstan1,  'mcmc_rhat_hist')
## ## plot(tobacco.rstan1,  'mcmc_neff_hist')
## 
## 
## preds <- posterior_predict(tobacco.rstanarm1,  nsamples=250,  summary=FALSE)
## tobacco.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = tobacco$NUMBER,
##                             fittedPredictedResponse = apply(preds, 2, median),
##                             integerResponse=FALSE)
## plot(tobacco.resids)
## 
## (l.1 <- loo(tobacco.rstanarm))
## (l.2 <- loo(tobacco.rstanarm1))
## loo_compare(l.1, l.2)
## 
## preds <- posterior_predict(tobacco.rstanarm,  nsamples=250,  summary=FALSE)
## tobacco.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = tobacco$NUMBER,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(tobacco.resids)
## 
## 
## ggpredict(tobacco.rstanarm) %>% plot
## ##ggemmeans(tobacco.rstan, ~TREATMENT) %>% plot
## 
## summary(tobacco.rstanarm)
## tidyMCMC(tobacco.rstanarm$stanfit,conf.int=TRUE, conf.method='HPDinterval',
##          rhat=TRUE, ess=TRUE, estimate.method='median') %>%
##   data.frame
## 
## bayes_R2(tobacco.rstanarm, re.form=NA) %>% median_hdci
## bayes_R2(tobacco.rstanarm, re.form=~(1|LEAF)) %>% median_hdci
## 
## newdata = emmeans(tobacco.rstanarm, ~TREATMENT) %>%
##     gather_emmeans_draws() %>%
##     spread(key=TREATMENT, value=.value)
## newdata
## newdata = newdata %>% mutate(Eff = Strong-Weak,
##                    PEff = 100*(Strong-Weak)/Weak)
## newdata %>% dplyr::select(Eff,PEff) %>% median_hdci
## newdata %>% summarize(Prob = sum(PEff>0)/n())
## newdata %>% summarize(Prob = sum(PEff>10)/n())
## hypothesis(newdata,  'PEff>0')
## hypothesis(newdata,  'PEff>10')
## 
## 
## newdata = emmeans(tobacco.rstanarm, ~TREATMENT) %>% as.data.frame
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
##                      chains=3, iter=5000, warmup=2000, thin=5,
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
## 
## preds <- posterior_predict(tobacco.brms,  nsamples=250,  summary=FALSE)
## tobacco.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = tobacco$NUMBER,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(tobacco.resids)
## 
## preds <- posterior_predict(tobacco.brms1,  nsamples=250,  summary=FALSE)
## tobacco.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = tobacco$NUMBER,
##                             fittedPredictedResponse = apply(preds, 2, median),
##                             integerResponse = TRUE)
## plot(tobacco.resids)
## 
## 
## ggpredict(tobacco.brms1) %>% plot
## ##ggemmeans(tobacco.rstan, ~TREATMENT) %>% plot
## 
## summary(tobacco.brms)
## tidyMCMC(tobacco.brms1$fit,
##          conf.int=TRUE, conf.method='HPDinterval',
##          rhat=TRUE, ess=TRUE,
##          estimate.method='median')
## 
## bayes_R2(tobacco.brms1, re.form=~(1|LEAF), summary=FALSE) %>% median_hdi
## bayes_R2(tobacco.brms, re.form=~(1|LEAF), summary=FALSE) %>% median_hdi
## bayes_R2(tobacco.brms1, re.form=~(TREATMENT|LEAF), summary=FALSE) %>% median_hdi
## bayes_R2(tobacco.brms1, re.form=NA,  summary=FALSE) %>% median_hdi
## bayes_R2(tobacco.brms1, re.form=NA,  summary=FALSE) %>% median_hdi
## 
## newdata = emmeans(tobacco.brms1, ~TREATMENT) %>%
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
## newdata = emmeans(tobacco.brms1, ~TREATMENT) %>% as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=emmean, x=TREATMENT)) +
##     geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD))

