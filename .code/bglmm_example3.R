## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE)


## ----libraries, results='markdown', eval=TRUE, message=FALSE, warning=FALSE----
library(car)       #for regression diagnostics
library(broom)     #for tidy output
library(ggfortify) #for model diagnostics
library(sjPlot)    #for outputs
library(knitr)     #for kable
library(effects)   #for partial effects plots
library(emmeans)   #for estimating marginal means
library(MASS)      #for glm.nb
library(MuMIn)     #for AICc
library(tidyverse) #for data wrangling


## ----readData, results='markdown', eval=TRUE----------------------------------
starling = read_csv('../data/starling_full.csv', trim_ws=TRUE)
glimpse(starling)


## ----fitModel, results='markdown', eval=FALSE,hidden=TRUE---------------------
## ggplot(starling, aes(y=MASS, x=MONTH, group=BIRD)) +
##     geom_point() +
##     geom_line() +
##     facet_grid(~SITUATION)
## 
## starling.rstan = stan_glmer(MASS ~ MONTH*SITUATION+(1|BIRD),data=starling,
##                             iter=5000, warmup=2000, thin=5, chains=3, refresh=0)
## prior_summary(starling.rstan)
## 
## starling.rstan %>% get_variables()
## plot(starling.rstan,  'mcmc_trace', regex_pars = '^.Intercept|^SITUATION|^MONTH|[sS]igma')
## plot(starling.rstan,  'mcmc_acf_bar', regex_pars = '^.Intercept|^SITUATION|^MONTH|[sS]igma')
## plot(starling.rstan,  'mcmc_rhat_hist', regex_pars = '^.Intercept|^SITUATION|^MONTH|[sS]igma')
## plot(starling.rstan,  'mcmc_neff_hist', regex_pars = '^.Intercept|^SITUATION|^MONTH|[sS]igma')
## 
## 
## preds <- posterior_predict(starling.rstan,  nsamples=250,  summary=FALSE)
## starling.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = starling$MASS,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(starling.resids)
## 
## 
## starling.rstan1 = stan_glmer(MASS ~ MONTH*SITUATION+(MONTH|BIRD),data=starling,
##                             iter=5000, warmup=2000, thin=5, chains=3, refresh=0)
## starling.rstan1 = stan_glmer(MASS ~ MONTH*SITUATION+(MONTH|BIRD),data=starling,
##                              iter=5000, warmup=2000, thin=5, chains=3, refresh=0,
##                              adapt_delta = 0.99)
## #pairs(starling.rstan1,  pars=c('(Intercept)', 'MONTHNov'))
## starling.rstan1 %>% get_variables()
## pairs(starling.rstan1,  regex_pars=c('SITUATION', 'sigma'))
## prior_summary(starling.rstan1)
## 
## plot(starling.rstan1,  'mcmc_trace', regex_pars = '^.Intercept|^SITUATION|^MONTH|[sS]igma')
## plot(starling.rstan1,  'mcmc_acf_bar', regex_pars = '^.Intercept|^SITUATION|^MONTH|[sS]igma')
## plot(starling.rstan1,  'mcmc_rhat_hist', regex_pars = '^.Intercept|^SITUATION|^MONTH|[sS]igma')
## plot(starling.rstan1,  'mcmc_neff_hist', regex_pars = '^.Intercept|^SITUATION|^MONTH|[sS]igma')
## 
## starling.rstan1 = stan_glmer(MASS ~ MONTH*SITUATION+(MONTH|BIRD),data=starling,
##                              iter=10000, warmup=5000, thin=15, chains=3, refresh=0,
##                              adapt_delta = 0.99)
## 
## plot(starling.rstan1,  'mcmc_trace', regex_pars = '^.Intercept|^SITUATION|^MONTH|[sS]igma')
## plot(starling.rstan1,  'mcmc_acf_bar', regex_pars = '^.Intercept|^SITUATION|^MONTH|[sS]igma')
## plot(starling.rstan1,  'mcmc_rhat_hist', regex_pars = '^.Intercept|^SITUATION|^MONTH|[sS]igma')
## plot(starling.rstan1,  'mcmc_neff_hist', regex_pars = '^.Intercept|^SITUATION|^MONTH|[sS]igma')
## preds <- posterior_predict(starling.rstan1,  nsamples=250,  summary=FALSE)
## starling.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = starling$MASS,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(starling.resids)
## 
## (l.1 <- loo(starling.rstan))
## (l.2 <- loo(starling.rstan1))
## loo_compare(l.1, l.2)
## 
## as.matrix(starling.rstan) %>% colnames
## posterior_vs_prior(starling.rstan1, color_by='vs', group_by=TRUE, regex_pars=c('^MONTH','^SITUATION','^[sS]igma'),
##                    facet_args=list(scales='free_y'))
## 
## 
## g=ggpredict(starling.rstan1) %>% plot
## do.call('grid.arrange',  g)
## ggemmeans(starling.rstan1, ~SITUATION|MONTH) %>% plot
## 
## summary(starling.rstan1)
## 
## nms <- starling.rstan1 %>% get_variables()
## nms
## wch <- grep('^.Intercept|^MONTH|^SITUATION|[sS]igma', nms)
## tidyMCMC(starling.rstan1$stanfit,conf.int=TRUE, conf.method='HPDinterval',
##          rhat=TRUE, ess=TRUE, pars=nms[wch])
## 
## emmeans(starling.rstan1, pairwise~MONTH|SITUATION)
## starling.em = emmeans(starling.rstan1, ~MONTH|SITUATION) %>%
##     gather_emmeans_draws() %>% spread(key=MONTH, value=.value) %>%
##     mutate(Eff=Jan-Nov,
##            PEff=100*(Jan-Nov)/Nov)
## starling.em %>% head
## 
## starling.em %>% ungroup %>%
##     dplyr::select(SITUATION,Eff,PEff) %>% group_by(SITUATION) %>% median_hdi
## 
## starling.em %>% ungroup %>%
##     dplyr::select(SITUATION,Eff,PEff) %>% group_by(SITUATION) %>%
##     summarize(Prob=sum(PEff>10)/n())
## 
## bayes_R2(starling.rstan1, re.form=NA) %>% median_hdi
## bayes_R2(starling.rstan1, re.form=~(1|BIRD)) %>% median_hdi
## bayes_R2(starling.rstan1, re.form=~(MONTH|BIRD)) %>% median_hdi
## 
## newdata = emmeans(starling.rstan1, ~MONTH|SITUATION) %>% as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=emmean, x=SITUATION)) +
##     geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD, fill=MONTH),
##                     position=position_dodge(width=0.3), shape=21)


## ----fitModel.brms, results='markdown', eval=FALSE,hidden=TRUE----------------
## 
## ggplot(starling,  aes(y=MASS,  x=SITUATION,  color=MONTH)) +
##   geom_boxplot()
## 
## 
## ggplot(starling, aes(y=MASS, x=MONTH, group=BIRD)) +
##     geom_point() +
##     geom_line() +
##     facet_grid(~SITUATION)
## 
## ggplot(starling, aes(y=MASS, x=SITUATION,  color=MONTH)) +
##     geom_point() +
##     geom_line() +
##     facet_grid(~BIRD)
## 
## 
## 
## 
## starling.form <- bf(MASS ~ MONTH*SITUATION+(1|BIRD),
##                     family=gaussian())
## starling.brms <- brm(starling.form, data=starling,
##                      iter=5000, warmup=2000, thin=5, chains=3, refresh=0,
##                      cores=3)
## prior_summary(starling.brms)
## 
## nms <- starling.brms %>% get_variables()
## 
## mcmc_plot(starling.brms,  type='trace', regex_pars = '^b.Intercept|^b.SITUATION|^b.MONTH|[sS]igma|sd')
## mcmc_plot(starling.brms,  type='acf_bar', regex_pars = '^.Intercept|^SITUATION|^MONTH|[sS]igma')
## mcmc_plot(starling.brms,  type='rhat_hist', regex_pars = '^.Intercept|^SITUATION|^MONTH|[sS]igma')
## mcmc_plot(starling.brms,  type='neff_hist', regex_pars = '^.Intercept|^SITUATION|^MONTH|[sS]igma')
## 
## 
## preds <- posterior_predict(starling.brms,  nsamples=250,  summary=FALSE)
## starling.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = starling$MASS,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(starling.resids)
## 
## starling.form <- bf(MASS ~ MONTH*SITUATION+(MONTH|BIRD),
##                     family=gaussian())
## starling.brms1 <- brm(starling.form, data=starling,
##                      iter=5000, warmup=2000, thin=10, chains=3, refresh=0,
##                      cores=3,  control=list(adapt_delta=0.99))
## prior_summary(starling.brms1)
## 
## starling.brms1 %>% get_variables()
## mcmc_plot(starling.brms1,  type='trace', regex_pars = '^b.Intercept|^b.SITUATION|^b.MONTH|[sS]igma')
## mcmc_plot(starling.brms1,  type='acf_bar', regex_pars = '^b.Intercept|^b.SITUATION|^b.MONTH|[sS]igma')
## mcmc_plot(starling.brms1,  type='rhat_hist')
## mcmc_plot(starling.brms1,  type='neff_hist')
## 
## 
## preds <- posterior_predict(starling.brms1,  nsamples=250,  summary=FALSE)
## starling.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = starling$MASS,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(starling.resids)
## #pairs(starling.rstan1,  pars=c('(Intercept)', 'MONTHNov'))
## starling.rstan1 %>% get_variables()
## pairs(starling.brms1,  regex_pars=c('SITUATION', 'sigma'))
## 
## (l.1 <- loo(starling.brms))
## (l.2 <- loo(starling.brms1))
## loo_compare(l.1, l.2)
## 
## as.matrix(starling.brms) %>% colnames
## posterior_vs_prior(starling.rstan1, color_by='vs', group_by=TRUE, regex_pars=c('^MONTH','^SITUATION','^[sS]igma'),
##                    facet_args=list(scales='free_y'))
## 
## 
## 
## g=ggpredict(starling.brms) %>% plot
## do.call('grid.arrange',  g)
## 
## ggemmeans(starling.brms, ~SITUATION|MONTH) %>% plot
## mcmc_plot(starling.brms1,  type='interval', regex_pars = '^.Intercept|^SITUATION|^MONTH')
## summary(starling.brms1)
## 
## nms <- starling.brms %>% get_variables()
## nms
## wch <- grep('^b.Intercept|^b.MONTH|^b.SITUATION|sigma|sd', nms)
## tidyMCMC(starling.brms$fit,conf.int=TRUE, conf.method='HPDinterval',
##          rhat=TRUE, ess=TRUE, pars=nms[wch])
## 
## emmeans(starling.brms1, pairwise~MONTH|SITUATION)
## starling.em = emmeans(starling.brms, ~MONTH|SITUATION) %>%
##   gather_emmeans_draws() %>%
##   spread(key=MONTH, value=.value) %>%
##     mutate(Eff=Jan-Nov,
##            PEff=100*(Jan-Nov)/Nov)
## starling.em %>% head
## 
## starling.em %>% ungroup %>%
##   dplyr::select(SITUATION,Eff,PEff) %>%
##   group_by(SITUATION) %>%
##   median_hdi
## 
## starling.em %>% ungroup %>%
##   dplyr::select(SITUATION,Eff,PEff) %>%
##   group_by(SITUATION) %>%
##     summarize(Prob=sum(PEff>10)/n())
## 
## 
## cmat <- cbind(Comp1=c(0.5, 0.5, -1, 0),
##               Comp2=c(1, -0.5, -0.5, 0))
## emmeans(starling.brms1,  ~SITUATION|MONTH,  contr=list(SITUATION=cmat))
## 
## bayes_R2(starling.brms1, re.form=NA) %>% median_hdi
## bayes_R2(starling.brms1, re.form=~(1|BIRD)) %>% median_hdi
## bayes_R2(starling.brms1, re.form=~(MONTH|BIRD)) %>% median_hdi
## 
## newdata = emmeans(starling.brms1, ~MONTH|SITUATION) %>% as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=emmean, x=SITUATION)) +
##     geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD, fill=MONTH),
##                     position=position_dodge(width=0.3), shape=21)

