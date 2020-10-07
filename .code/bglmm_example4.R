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
mckeon = read_csv('../data/mckeon.csv', trim_ws=TRUE)
glimpse(mckeon)


## ----fitModel, results='markdown', eval=FALSE, hidden=TRUE--------------------
## mckeon = mckeon %>% mutate(BLOCK=factor(BLOCK),
##    SYMBIONT=factor(SYMBIONT, levels=c('none','crabs','shrimp','both')))
## 
## ggplot(mckeon, aes(y=PREDATION, x=SYMBIONT)) +
##     geom_point(position=position_jitter(width=0.2, height=0))+
##     facet_wrap(~BLOCK)
## 
## mckeon.rstan = stan_glmer(PREDATION ~ SYMBIONT + (1|BLOCK),
##                           data=mckeon, family=binomial(link='logit'),
##                           iter=5000, warmup=2000, chains=3, thin=5, refresh=0,
##                           cores=3)
## mckeon.rstan %>% get_variables()
## plot(mckeon.rstan,  'mcmc_trace', regex_pars='^.Intercept|SYMBIONT|[sS]igma')
## plot(mckeon.rstan,  'mcmc_acf_bar', regex_pars='^.Intercept|SYMBIONT|[sS]igma')
## plot(mckeon.rstan,  'mcmc_rhat_hist', regex_pars='^.Intercept|SYMBIONT|[sS]igma')
## plot(mckeon.rstan,  'mcmc_neff_hist', regex_pars='^.Intercept|SYMBIONT|[sS]igma')
## 
## 
## preds <- posterior_predict(mckeon.rstan,  nsamples=250,  summary=FALSE)
## mckeon.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = mckeon$PREDATION,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(mckeon.resids)
## 
## mckeon.rstan1 = stan_glmer(PREDATION ~ SYMBIONT + (SYMBIONT|BLOCK),
##                            data=mckeon, family=binomial(link='logit'),
##                            iter=5000, warmup=2000, chains=3, thin=5, refresh=0,
##                           cores=3)
## 
## mckeon.rstan1 %>% get_variables()
## plot(mckeon.rstan1,  'mcmc_trace', regex_pars='^.Intercept|^SYMBIONT|[sS]igma')
## plot(mckeon.rstan1,  'mcmc_acf_bar', regex_pars='^.Intercept|^SYMBIONT|[sS]igma')
## plot(mckeon.rstan1,  'mcmc_rhat_hist', regex_pars='^.Intercept|^SYMBIONT|[sS]igma')
## plot(mckeon.rstan1,  'mcmc_neff_hist', regex_pars='^.Intercept|^SYMBIONT|[sS]igma')
## 
## 
## preds <- posterior_predict(mckeon.rstan1,  nsamples=250,  summary=FALSE)
## mckeon.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = mckeon$PREDATION,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(mckeon.resids)
## 
## #prior_summary(mckeon.rstan1)
## #posterior_vs_prior(mckeon.rstan1, color_by='vs', group_by=TRUE,
## #                   facet_args=list(scales='free_y'))
## 
## loo(mckeon.rstan)
## loo(mckeon.rstan1)
## 
## #as.matrix(mckeon.rstan1) %>% colnames
## #posterior_vs_prior(mckeon.rstan1, color_by='vs', group_by=TRUE,
## #                   regex_pars=c('^(Intercept)','^SYMBIONT','^[sS]igma'),
## #                   facet_args=list(scales='free_y'))
## 
## ## mckeon.rstan1 = stan_glmer(PREDATION ~ SYMBIONT + (SYMBIONT|BLOCK),
## ##                           data=mckeon, family=binomial(link='logit'),
## ##                           iter=4000, warmup=1000, chains=3, thin=5, refresh=0,
## ##                           cores=3,
## ##                           prior_intercept = normal(0,10),
## ##                           prior=normal(0,2.5),
## ##                           prior_covariance = decov(1,1,1,4))
## ## posterior_vs_prior(mckeon.rstan1, color_by='vs', group_by=TRUE, regex_pars=c('^(Intercept)','^SYMBIONT','^[sS]igma'),
## ##                    facet_args=list(scales='free_y'))
## 
## mckeon.rstan1 %>% get_variables()
## nms=colnames(as.matrix(mckeon.rstan1))
## wch = grep('^.Intercept|^SYMBIONT',nms)
## #posterior_vs_prior(mckeon.rstan1, color_by='vs', group_by=TRUE,
## #                   facet_args=list(scales='free_y'), pars=nms[wch])
## 
## 
## ggpredict(mckeon.rstan1) %>% plot
## 
## summary(mckeon.rstan1)
## tidyMCMC(mckeon.rstan1$stanfit,conf.int=TRUE, conf.method='HPDinterval',
##          rhat=TRUE, ess=TRUE,  pars=nms[wch])
## 
## emmeans(mckeon.rstan1, pairwise~SYMBIONT, type='response')
## mckeon.em = emmeans(mckeon.rstan1, pairwise~SYMBIONT, type='link')$contrasts %>%
##       gather_emmeans_draws() %>%
##       mutate(PEff=exp(.value))
## mckeon.em %>% head
## mckeon.em %>%
##   group_by(contrast) %>%
##   dplyr::select(contrast, PEff) %>%
##   median_hdi
## mckeon.em %>%
##   group_by(contrast) %>%
##   summarize(Prob=sum(PEff>1)/n())
## 
## cmat=cbind(
##     crab_vs_shrimp=c(0,1,-1,0),
##     one_vs_both=c(0,-1/2,-1/2,1),
##     symbiont=c(1, -1/3, -1/3,-1/3)
## )
## mckeon.em = emmeans(mckeon.rstan1, ~SYMBIONT, contr=list(cmat), type='link')$contrast %>%
##                                                                  gather_emmeans_draws() %>%
##                                                                  mutate(Fit=exp(.value))
## mckeon.em %>%
##   group_by(contrast) %>%
##   median_hdi(Fit)
## 
## mckeon.em %>%
##   group_by(contrast) %>%
##   summarize(sum(Fit>1)/n())
## 
## newdata = emmeans(mckeon.rstan1, ~SYMBIONT, type='response') %>% as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=prob, x=SYMBIONT)) +
##     geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD))
## 


## ----fitModel.brms, results='markdown', eval=FALSE, hidden=TRUE---------------
## mckeon = mckeon %>% mutate(BLOCK=factor(BLOCK),
##    SYMBIONT=factor(SYMBIONT, levels=c('none','crabs','shrimp','both')))
## 
## ggplot(mckeon, aes(y=PREDATION, x=SYMBIONT)) +
##     geom_point(position=position_jitter(width=0.2, height=0))+
##     facet_wrap(~BLOCK)
## 
## ggplot(mckeon, aes(y=PREDATION, x=SYMBIONT)) +
##     geom_point()+
##     facet_wrap(~BLOCK)
## mckeon.form <- bf(PREDATION | trials(1) ~ SYMBIONT + (1|BLOCK),
##                   family=binomial(link='logit'))
## mckeon.brms <- brm(mckeon.form,
##                    data=mckeon,
##                    iter=5000, warmup=2000, chains=3, thin=5, refresh=0,
##                    cores=3)
## mckeon.brms %>% get_variables()
## mcmc_plot(mckeon.brms,  type='trace')
## , regex_pars='^.Intercept|SYMBIONT|sd')
## mcmc_plot(mckeon.brms,  type='acf_bar')
## , regex_pars='^.Intercept|SYMBIONT|sd')
## mcmc_plot(mckeon.brms,  type='rhat_hist')
## , regex_pars='^.Intercept|SYMBIONT|sd')
## mcmc_plot(mckeon.brms,  type='neff_hist')
## , regex_pars='^.Intercept|SYMBIONT|sd')
## 
## 
## preds <- posterior_predict(mckeon.brms,  nsamples=250,  summary=FALSE)
## mckeon.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = mckeon$PREDATION,
##                             fittedPredictedResponse = apply(preds, 2, median),
##                             integerResponse=gaussian)
## plot(mckeon.resids)
## 
## mckeon.form <- bf(PREDATION | trials(1) ~ SYMBIONT + (SYMBIONT|BLOCK),  family=binomial(link='logit'))
## mckeon.brms1 <- brm(mckeon.form,
##                    data=mckeon,
##                    iter=5000, warmup=2000, chains=3, thin=5, refresh=0,
##                    cores=3)
## 
## mcmc_plot(mckeon.brms1,  type='trace')
## mcmc_plot(mckeon.brms1,  type='trace', regex_pars='^.Intercept|SYMBIONT|sd')
## mcmc_plot(mckeon.brms1,  type='acf_bar')
## , regex_pars='^.Intercept|SYMBIONT|sd')
## mcmc_plot(mckeon.brms1,  type='rhat_hist')
## , regex_pars='^.Intercept|SYMBIONT|sd')
## mcmc_plot(mckeon.brms1,  type='neff_hist')
## , regex_pars='^.Intercept|SYMBIONT|sd')
## 
## preds <- posterior_predict(mckeon.brms1,  nsamples=250,  summary=FALSE)
## mckeon.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = mckeon$PREDATION,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(mckeon.resids)
## 
## #prior_summary(mckeon.brms1)
## #posterior_vs_prior(mckeon.brms1, color_by='vs', group_by=TRUE,
## #                   facet_args=list(scales='free_y'))
## 
## loo(mckeon.brms)
## loo(mckeon.brms1)
## 
## #as.matrix(mckeon.brms1) %>% colnames
## #posterior_vs_prior(mckeon.brms1, color_by='vs', group_by=TRUE,
## #                   regex_pars=c('^(Intercept)','^SYMBIONT','^[sS]igma'),
## #                   facet_args=list(scales='free_y'))
## 
## ## mckeon.brms1 = stan_glmer(PREDATION ~ SYMBIONT + (SYMBIONT|BLOCK),
## ##                           data=mckeon, family=binomial(link='logit'),
## ##                           iter=4000, warmup=1000, chains=3, thin=5, refresh=0,
## ##                           cores=3,
## ##                           prior_intercept = normal(0,10),
## ##                           prior=normal(0,2.5),
## ##                           prior_covariance = decov(1,1,1,4))
## ## posterior_vs_prior(mckeon.brms1, color_by='vs', group_by=TRUE, regex_pars=c('^(Intercept)','^SYMBIONT','^[sS]igma'),
## ##                    facet_args=list(scales='free_y'))
## ggpredict(mckeon.brms1) %>% plot
## 
## mckeon.brms1 %>% get_variables()
## nms=colnames(as.matrix(mckeon.brms1))
## wch = grep('^b.Intercept|^b.SYMBIONT|^sd',nms)
## #posterior_vs_prior(mckeon.brms1, color_by='vs', group_by=TRUE,
## #                   facet_args=list(scales='free_y'), pars=nms[wch])
## 
## 
## 
## summary(mckeon.brms1)
## tidyMCMC(mckeon.brms1$fit,conf.int=TRUE, conf.method='HPDinterval',
##          rhat=TRUE, ess=TRUE,  pars=nms[wch])
## tidyMCMC(mckeon.brms1$fit,conf.int=TRUE, conf.method='HPDinterval',
##          rhat=TRUE, ess=TRUE)
## 
## emmeans(mckeon.brms1, pairwise~SYMBIONT, type='response')
## mckeon.em = emmeans(mckeon.brms1, pairwise~SYMBIONT, type='link')$contrasts %>%
##       gather_emmeans_draws() %>%
##       mutate(PEff=exp(.value)))#,
##              #Prob = plogis(.value))
## mckeon.em %>% head
## mckeon.em %>%
##   group_by(contrast) %>%
##   dplyr::select(contrast, PEff) %>%
##   median_hdi
## mckeon.em %>%
##   group_by(contrast) %>%
##   summarize(Prob=sum(PEff>1)/n())
## 
## mckeon.em = emmeans(mckeon.brms1, ~SYMBIONT, type='link') %>%
##       gather_emmeans_draws()
## mckeon.em %>% mutate(P=plogis(.value)) %>% median_hdci(P)
## 
## mutate(PEff=exp(.value)))#,
##              #Prob = plogis(.value))
## 
## cmat=cbind(
##     crab_vs_shrimp=c(0,1,-1,0),
##     one_vs_both=c(0,-1/2,-1/2,1),
##     symbiont=c(1, -1/3, -1/3,-1/3)
## )
## mckeon.em = emmeans(mckeon.brms1, ~SYMBIONT, contr=list(cmat), type='link')$contrast %>%
##                                                                  gather_emmeans_draws() %>%
##                                                                  mutate(Fit=exp(.value))
## mckeon.em %>%
##   group_by(contrast) %>%
##   median_hdi(Fit)
## 
## mckeon.em %>%
##   group_by(contrast) %>%
##   summarize(sum(Fit>1)/n())
## 
## newdata = emmeans(mckeon.brms1, ~SYMBIONT, type='response') %>% as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=prob, x=SYMBIONT)) +
##     geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD))
## 

