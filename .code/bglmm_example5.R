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
owls = read_csv('../data/owls.csv', trim_ws=TRUE)
glimpse(owls)


## ----fitModel, results='markdown', eval=FALSE, hidden=TRUE--------------------
## ## Amount of Sibling negotiation (vocalizations when parents are absent)
## ## Foot treatment (deprived or satiated
## ## Sex of parent
## ## Arrival time of parent
## ## Nest as random
## ## Brood size offset
## owls = owls %>% mutate(Nest =factor(Nest),
##                        FoodTreatment = factor(FoodTreatment),
##                        SexParent = factor(SexParent),
##                        NCalls = SiblingNegotiation)
## ggplot(data = owls) +
##     geom_boxplot(aes(y = NCalls, x = Nest)) +
##     facet_grid(SexParent ~ FoodTreatment)
## 
## ggplot(data=owls) +
##   geom_boxplot(aes(y=NCalls,  x=SexParent,  color=FoodTreatment)) +
##   facet_wrap(~Nest)
## 
## ggplot(data = owls,aes(y = NCalls, x = ArrivalTime, color=SexParent)) +
##     geom_point() +
##     geom_smooth(method='lm') +
##   facet_grid(~FoodTreatment)
## 
## ggplot(data = owls,aes(y = NCalls, x = ArrivalTime, color=FoodTreatment)) +
##     geom_point() +
##     geom_smooth(method='lm')
## #  facet_grid(~FoodTreatment)
## 
## ggplot(data = owls,aes(y = NegPerChick, x = ArrivalTime, color=FoodTreatment)) +
##     geom_point() +
##   geom_smooth(method='lm') +
##   facet_wrap(~Nest,  scale='free_y')
## 
## owls.rstanP <- stan_glmer(NCalls ~ FoodTreatment+scale(ArrivalTime) +
##                            offset(log(BroodSize)) + (1|Nest),
##                          dat=owls,  family=poisson(link='log'), refresh=0,
##                          iter=5000,  warmup=2000,  thin=10,  chains=3, cores=3)
## 
## owls.rstanP %>% get_variables()
## plot(owls.rstanP,  'mcmc_trace', regex_pars='^.Intercept|Food|Arrival|[sS]igma')
## plot(owls.rstanP,  'mcmc_acf_bar', regex_pars='^.Intercept|Food|Arrival|[sS]igma')
## plot(owls.rstanP,  'mcmc_rhat_hist', regex_pars='^.Intercept|Food|Arrival|[sS]igma')
## plot(owls.rstanP,  'mcmc_neff_hist', regex_pars='^.Intercept|Food|Arrival|[sS]igma')
## 
## 
## preds <- posterior_predict(owls.rstanP,  nsamples=250,  summary=FALSE)
## owls.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = owls$NCalls,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(owls.resids)
## 
## owls.rstanNB <- stan_glmer(NCalls ~ FoodTreatment+scale(ArrivalTime) +
##                            offset(log(BroodSize)) + (1|Nest),
##                          dat=owls,  family=neg_binomial_2(link='log'), refresh=0,
##                          iter=5000,  warmup=2000,  thin=10,  chains=3, cores=3)
## 
## owls.rstanNB %>% get_variables()
## plot(owls.rstanNB,  'mcmc_trace', regex_pars='^.Intercept|Food|Arrival|[sS]igma')
## plot(owls.rstanNB,  'mcmc_acf_bar', regex_pars='^.Intercept|Food|Arrival|[sS]igma')
## plot(owls.rstanNB,  'mcmc_rhat_hist', regex_pars='^.Intercept|Food|Arrival|[sS]igma')
## plot(owls.rstanNB,  'mcmc_neff_hist', regex_pars='^.Intercept|Food|Arrival|[sS]igma')
## 
## 
## preds <- posterior_predict(owls.rstanNB,  nsamples=250,  summary=FALSE)
## owls.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = owls$NCalls,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(owls.resids)
## testZeroInflation(owls.resids)
## 
## testTemporalAutocorrelation(owls.resids,  time=owls$ArrivalTime)
## owls.resids1 <- recalculateResiduals(owls.resids,  group=interaction(owls$ArrivalTime,  owls$Nest),  aggregateBy = mean)
## testTemporalAutocorrelation(owls.resids1,  time=unique(owls$ArrivalTime))
## 
## ## Cant use zero inflation with glmer
## 
## owls.rstan1 <- stan_glmer(NCalls ~ FoodTreatment+scale(ArrivalTime) +
##                            offset(log(BroodSize)) + (FoodTreatment*scale(ArrivalTime)|Nest),
##                          dat=owls,  family=neg_binomial_2(link='log'), refresh=0,
##                          iter=5000,  warmup=2000,  thin=10,  chains=3, cores=3)
## 
## owls.rstan1 %>% get_variables()
## plot(owls.rstan1,  'mcmc_trace', regex_pars='^.Intercept|^Food|^scale.Arrival|[sS]igma')
## plot(owls.rstan1,  'mcmc_acf_bar', regex_pars='^.Intercept|^Food|^scale.Arrival|[sS]igma')
## plot(owls.rstan1,  'mcmc_rhat_hist', regex_pars='^.Intercept|^Food|^scale.Arrival|[sS]igma')
## plot(owls.rstan1,  'mcmc_neff_hist', regex_pars='^.Intercept|^Food|^scale.Arrival|[sS]igma')
## 
## preds <- posterior_predict(owls.rstan1,  nsamples=250,  summary=FALSE)
## owls.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = owls$NCalls,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(owls.resids)
## 


## ----fitModel.brms, results='markdown', eval=FALSE, hidden=TRUE---------------
## ## Amount of Sibling negotiation (vocalizations when parents are absent)
## ## Foot treatment (deprived or satiated
## ## Sex of parent
## ## Arrival time of parent
## ## Nest as random
## ## Brood size offset
## owls = owls %>% mutate(Nest =factor(Nest),
##                        FoodTreatment = factor(FoodTreatment),
##                        SexParent = factor(SexParent),
##                        NCalls = SiblingNegotiation)
## ggplot(data = owls) +
##     geom_boxplot(aes(y = NCalls, x = Nest)) +
##     facet_grid(SexParent ~ FoodTreatment)
## 
## ggplot(data=owls) +
##   geom_boxplot(aes(y=NCalls,  x=SexParent,  color=FoodTreatment)) +
##   facet_wrap(~Nest)
## 
## ggplot(data = owls,aes(y = NCalls, x = ArrivalTime, color=SexParent)) +
##     geom_point() +
##     geom_smooth(method='lm') +
##   facet_grid(~FoodTreatment)
## 
## owls.form <- bf(NCalls ~ FoodTreatment+scale(ArrivalTime) +
##                   offset(log(BroodSize)) + (1|Nest),
##                 family=poisson(link='log'))
## 
## owls.brmsP <- brm(owls.form,  data=owls, refresh=0,
##                   iter=5000,  warmup=2000,  thin=10,  chains=3, cores=3)
## 
## owls.brmsP %>% get_variables()
## mcmc_plot(owls.brmsP,  type='trace')
## mcmc_plot(owls.brmsP,  type='acf_bar')
## mcmc_plot(owls.brmsP,  type='rhat_hist')
## mcmc_plot(owls.brmsP,  type='neff_hist')
## mcmc_plot(owls.brmsP,  type='trace', regex_pars='^b.Intercept|Food|Arrival|sd')
## mcmc_plot(owls.brmsP,  type='acf_bar', regex_pars='^b.Intercept|Food|Arrival|sd')
## mcmc_plot(owls.brmsP,  type='rhat_hist', regex_pars='^b.Intercept|Food|Arrival|sd')
## mcmc_plot(owls.brmsP,  type='neff_hist', regex_pars='^b.Intercept|Food|Arrival|sd')
## 
## 
## preds <- posterior_predict(owls.brmsP,  nsamples=250,  summary=FALSE)
## owls.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = owls$NCalls,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(owls.resids)
## testZeroInflation(owls.resids)
## 
## 
## owls.form <- bf(NCalls ~ FoodTreatment+scale(ArrivalTime) +
##                   offset(log(BroodSize)) + (1|Nest),
##                 zi ~ 1,
##                 family=zero_inflated_poisson(link='log'))
## 
## owls.brmsZIP <- brm(owls.form,  data=owls, refresh=0,
##                   iter=5000,  warmup=2000,  thin=10,  chains=3, cores=3)
## 
## owls.brmsZIP %>% get_variables()
## 
## mcmc_plot(owls.brmsZIP,  type='trace')
## , regex_pars='^b.Intercept|Food|Arrival|sd')
## mcmc_plot(owls.brmsZIP,  type='acf_bar')
## , regex_pars='^b.Intercept|Food|Arrival|sd')
## mcmc_plot(owls.brmsZIP,  type='rhat_hist')
## , regex_pars='^b.Intercept|Food|Arrival|sd')
## mcmc_plot(owls.brmsZIP,  type='neff_hist')
## , regex_pars='^b.Intercept|Food|Arrival|sd')
## 
## 
## preds <- posterior_predict(owls.brmsZIP,  nsamples=250,  summary=FALSE)
## owls.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = owls$NCalls,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(owls.resids)
## 
## 
## owls.form <- bf(NCalls ~ FoodTreatment+scale(ArrivalTime) +
##                   offset(log(BroodSize)) + (1|Nest),
##                 zi ~ scale(ArrivalTime),
##                 family=zero_inflated_poisson(link='log'))
## 
## owls.brmsZIP1 <- brm(owls.form,  data=owls, refresh=0,
##                   iter=5000,  warmup=2000,  thin=10,  chains=3, cores=3)
## 
## owls.brmsZIP1 %>% get_variables()
## mcmc_plot(owls.brmsZIP1,  type='trace', regex_pars='^b.Intercept|Food|Arrival|sd')
## mcmc_plot(owls.brmsZIP1,  type='acf_bar', regex_pars='^b.Intercept|Food|Arrival|sd')
## mcmc_plot(owls.brmsZIP1,  type='rhat_hist', regex_pars='^b.Intercept|Food|Arrival|sd')
## mcmc_plot(owls.brmsZIP1,  type='neff_hist', regex_pars='^b.Intercept|Food|Arrival|sd')
## 
## 
## preds <- posterior_predict(owls.brmsZIP1,  nsamples=250,  summary=FALSE)
## owls.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = owls$NCalls,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(owls.resids)
## 
## 
## 
## owls.form <- bf(NCalls ~ FoodTreatment+scale(ArrivalTime) +
##                   offset(log(BroodSize)) + (1|Nest),
##                 zi ~ scale(ArrivalTime),
##                 family=zero_inflated_negbinomial(link='log'))
## 
## owls.brmsZINB <- brm(owls.form,  data=owls, refresh=0,
##                   iter=5000,  warmup=2000,  thin=10,  chains=3, cores=3)
## 
## owls.brmsZINB %>% get_variables()
## mcmc_plot(owls.brmsZINB,  type='trace', regex_pars='^b.Intercept|Food|Arrival|sd')
## mcmc_plot(owls.brmsZINB,  type='acf_bar', regex_pars='^b.Intercept|Food|Arrival|sd')
## mcmc_plot(owls.brmsZINB,  type='rhat_hist', regex_pars='^b.Intercept|Food|Arrival|sd')
## mcmc_plot(owls.brmsZINB,  type='neff_hist', regex_pars='^b.Intercept|Food|Arrival|sd')
## 
## 
## preds <- posterior_predict(owls.brmsZINB,  nsamples=250,  summary=FALSE)
## owls.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = owls$NCalls,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(owls.resids)
## 
## 
## owls.form <- bf(NCalls ~ FoodTreatment*scale(ArrivalTime) +
##                   offset(log(BroodSize)) + (scale(ArrivalTime)|Nest),
##                 zi ~ scale(ArrivalTime),
##                 family=zero_inflated_negbinomial(link='log'))
## 
## owls.brmsZINB2 <- brm(owls.form,  data=owls, refresh=0,
##                   iter=5000,  warmup=2000,  thin=10,  chains=3, cores=3)
## 
## owls.brmsZINB1 %>% get_variables()
## mcmc_plot(owls.brmsZINB1,  type='trace', regex_pars='^b.Intercept|Food|Arrival|sd')
## mcmc_plot(owls.brmsZINB1,  type='acf_bar', regex_pars='^b.Intercept|Food|Arrival|sd')
## mcmc_plot(owls.brmsZINB1,  type='rhat_hist', regex_pars='^b.Intercept|Food|Arrival|sd')
## mcmc_plot(owls.brmsZINB1,  type='neff_hist', regex_pars='^b.Intercept|Food|Arrival|sd')
## 
## 
## preds <- posterior_predict(owls.brmsZINB2,  nsamples=250,  summary=FALSE)
## owls.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = owls$NCalls,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(owls.resids)
## 
## testZeroInflation(owls.resids)
## 
## testTemporalAutocorrelation(owls.resids,  time=owls$ArrivalTime)
## owls.resids1 <- recalculateResiduals(owls.resids,  group=interaction(owls$ArrivalTime,  owls$Nest),  aggregateBy = mean)
## testTemporalAutocorrelation(owls.resids1,  time=unique(owls$ArrivalTime))
## 
## summary(owls.brmsZINB1)
## tidyMCMC(owls.brmsZINB2$fit,  conf.int=TRUE,  conf.method='HPDinterval',
##          rhat=TRUE,  ess=TRUE)
## 
## 
## owls.grid = with(owls, list(FoodTreatment=levels(FoodTreatment),
##                             ArrivalTime=modelr::seq_range(ArrivalTime,  n=100)
##                             ))
## owls.grid = with(owls,  list(FoodTreatment=levels(FoodTreatment),
##                              ArrivalTime=seq(min(ArrivalTime),  max(ArrivalTime),  len=100)))
## 
## newdata = emmeans(owls.brmsZINB2, ~ArrivalTime|FoodTreatment, at=owls.grid,
##                   type='response') %>%
##   as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=prob, x=ArrivalTime)) +
##     geom_line(aes(color=FoodTreatment)) +
##     geom_ribbon(aes(ymin=lower.HPD, ymax=upper.HPD, fill=FoodTreatment), alpha=0.2)
## 

