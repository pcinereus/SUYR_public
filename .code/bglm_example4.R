## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE)


## ----libraries, results='markdown', eval=TRUE---------------------------------
library(rstanarm)   #for fitting models in STAN
library(brms)       #for fitting models in STAN
library(coda)       #for diagnostics
library(bayesplot)  #for diagnostics
library(rstan)      #for interfacing with STAN
library(emmeans)    #for marginal means etc
library(DHARMa)     #for residual diagnostics
library(broom)      #for tidying outputs
library(tidybayes)  #for more tidying outputs
library(ggeffects)  #for partial plots
library(tidyverse)  #for data wrangling etc


## ----readData, results='markdown', eval=TRUE----------------------------------
loyn = read_csv('../data/loyn.csv', trim_ws=TRUE)
glimpse(loyn)


## ----fitModel, results='markdown', eval=FALSE, hidden=TRUE--------------------
## loyn = loyn %>% mutate(fGRAZE=factor(GRAZE))
## 
## library(car)
## scatterplotMatrix(~ABUND+DIST+LDIST+AREA+fGRAZE+ALT+YR.ISOL, data=loyn,
##                   diagonal = list(method='boxplot'))
## 				
## scatterplotMatrix(~ABUND+log(DIST)+log(LDIST)+log(AREA)+fGRAZE+ALT+YR.ISOL, data=loyn,
##                   diagonal = list(method='boxplot'))
## 
## loyn.lm<-lm(ABUND~scale(log(DIST))+scale(log(LDIST))+scale(log(AREA))+
##                 fGRAZE + scale(ALT) + scale(YR.ISOL), data=loyn)
## vif(loyn.lm)
## 
## summary(loyn.lm)
## 
## loyn.rstanarm <- stan_glm(ABUND ~ scale(log(DIST))+
##                               scale(log(LDIST))+
##                               scale(log(AREA))+
##                               fGRAZE+
##                               scale(ALT)+
##                               scale(YR.ISOL), data=loyn,
##                           family='gaussian', refresh=0,
##                           iter = 5000, thin=5,chains = 3, warmup=2000)
## system.time(loyn.rstanarm <- stan_glm(ABUND ~ scale(log(DIST))+
##                               scale(log(LDIST))+
##                               scale(log(AREA))+
##                               fGRAZE+
##                               scale(ALT)+
##                               scale(YR.ISOL), data=loyn,
##                           family='gaussian', refresh=0,
##                           iter = 10000, thin=2,chains = 3, warmup=500, cores=3))
## 
## prior_summary(loyn.rstanarm)
## stan_trace(loyn.rstanarm)
## stan_dens(loyn.rstanarm,separate_chains=TRUE)
## stan_hist(loyn.rstanarm)
## stan_ac(loyn.rstanarm)
## stan_rhat(loyn.rstanarm)
## stan_ess(loyn.rstanarm)
## 
## pp_check(loyn.rstanarm, group='DIST', plotfun='scatter_avg')
## posterior_vs_prior(loyn.rstanarm, color_by='vs',
##        group_by=TRUE, facet_args=list(scales='free_y'))
## 
## ## HERE
## loyn.rstanarmG <- stan_glm(ABUND ~ scale(log(DIST))+
##                               scale(log(LDIST))+
##                               scale(log(AREA))+
##                               fGRAZE+
##                               scale(ALT)+
##                               scale(YR.ISOL), data=loyn,
##                           family=Gamma(link='log'),
##                           iter = 5000, thin=5,chains = 3, warmup=2000)
##                           control=list(adapt_delta=0.99))
## 
## 
## preds <- posterior_predict(loyn.rstanarmG,  nsamples=250,  summary=FALSE)
## loyn.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = loyn$ABUND,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(loyn.resids)
## 
## preds <- posterior_predict(loyn.rstanarm,  nsamples=250,  summary=FALSE)
## loyn.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = loyn$ABUND,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(loyn.resids)
## ggplot() +
##   geom_point(data=NULL,  aes(x=peake$INDIV,  y=peake.resids$scaledResiduals))
## 
## fit = fitted(loyn.rstanarm)
## res = residuals(loyn.rstanarm)
## ggplot(data=NULL) +
##     geom_point(aes(y=res, x=fit))
## 
## ggplot(data=NULL) +
##     geom_point(aes(y=resid(loyn.rstanarmG), x=fitted(loyn.rstanarmG)))
## plot(residuals(loyn.rstanarm) ~ fitted(loyn.rstanarm))
## ndata=data.frame(Fitted=fitted(loyn.rstanarm),
##     Residuals=residuals(loyn.rstanarm))
## ggplot(data=ndata, aes(y=Residuals, x=Fitted)) +
##     geom_point() +
##         geom_hline(yintercept=0)
## 
## loo(loyn.rstanarm)
## loo(loyn.rstanarmG)
## waic(loyn.rstanarm)
## 
## #ggpredict(loyn.rstanarm)
## 
## tidyMCMC(loyn.rstanarm$stanfit, conf.int=TRUE,
##          conf.method='HPDinterval',rhat=TRUE, ess=TRUE)
## 
## loyn.rstanarm1 <- stan_glm(ABUND ~ scale(log(AREA))+
##                               fGRAZE+
##                               scale(YR.ISOL), data=loyn,
##                           family='gaussian', refresh=0,
##                           iter = 2000, thin=2,chains = 3, warmup=500)
## waic(loyn.rstanarm)
## waic(loyn.rstanarm1)
## loo(loyn.rstanarm)
## loo(loyn.rstanarm1)
## 
## compare_models(waic(loyn.rstanarm), waic(loyn.rstanarm1))
## 
## #as.matrix(loyn.rstanarmG)
## tidyMCMC(loyn.rstanarm1$stanfit, conf.int=TRUE,
##          conf.method='HPDinterval',rhat=TRUE, ess=TRUE)
## 
## 
## loyn.list = with(loyn, list(AREA = seq(min(AREA), max(AREA), len=100)))
## newdata = emmeans(loyn.rstanarm1, ~AREA|fGRAZE, at = loyn.list, type='response') %>%
##     as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=emmean, x=AREA)) +
## geom_ribbon(aes(ymin=lower.HPD, ymax=upper.HPD, fill=fGRAZE), alpha=0.3) +
## geom_line(aes(color=fGRAZE)) +
## theme_bw() +
## scale_x_log10() + scale_y_log10()
## 
## ggplot(newdata, aes(y=emmean, x=AREA)) +
## geom_ribbon(aes(ymin=lower.HPD, ymax=upper.HPD, fill=fGRAZE), alpha=0.3) +
## geom_line(aes(color=fGRAZE)) +
## theme_bw() +
## scale_x_log10()
## 
## loyn.rstanarmG <- stan_glm(ABUND ~ scale(log(AREA))+fGRAZE+scale(YR.ISOL), data=loyn,
##                           family=Gamma(link='log'), refresh=0,
##                           iter = 2000, thin=2,chains = 3, warmup=500)
## loyn.list = with(loyn, list(AREA = seq(min(AREA), max(AREA), len=100)))
## newdata = emmeans(loyn.rstanarmG, ~AREA|fGRAZE, at = loyn.list, type='response') %>%
##     as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=response, x=AREA)) +
## geom_ribbon(aes(ymin=lower.HPD, ymax=upper.HPD, fill=fGRAZE), alpha=0.3) +
## geom_line(aes(color=fGRAZE)) +
## theme_bw() +
## scale_x_log10() + scale_y_log10()
## 
## 
## library(tidybayes)
## spaghetti = emmeans(loyn.rstanarmG, ~AREA|fGRAZE, at = loyn.list, type='response') %>%
##     gather_emmeans_draws() %>% mutate(Fit=exp(.value))
## wch = sample(1:max(spaghetti$.draw), 100,replace=FALSE)
## spaghetti = spaghetti %>% filter(.draw %in% wch)
## ggplot(newdata) +
##     geom_line(data=spaghetti, aes(y=Fit, x=AREA, color=fGRAZE,
##                                   group=interaction(fGRAZE,.draw)), alpha=0.05) +
##     geom_line(aes(y=response, x=AREA, color=fGRAZE)) +
##     theme_bw() +
##     scale_x_log10() + scale_y_log10()
## 
## summary(loyn.rstanarmG)
## 
## bayes_R2(loyn.rstanarmG) %>% median_hdi


## ----fitModeli.brms, results='markdown', eval=FALSE, hidden=TRUE--------------
## loyn = loyn %>% mutate(fGRAZE=factor(GRAZE))
## 
## library(car)
## scatterplotMatrix(~ABUND+DIST+LDIST+AREA+fGRAZE+ALT+YR.ISOL, data=loyn,
##                   diagonal = list(method='boxplot'))
## 				
## scatterplotMatrix(~ABUND+log(DIST)+log(LDIST)+log(AREA)+fGRAZE+ALT+YR.ISOL, data=loyn,
##                   diagonal = list(method='boxplot'))
## 
## loyn.lm<-glm(ABUND~scale(log(DIST))+scale(log(LDIST))+scale(log(AREA))+
##                fGRAZE + scale(ALT) + scale(YR.ISOL), data=loyn,  family=Gamma(link='log'))
## car::vif(loyn.lm)
## 
## summary(loyn.lm)
## loyn.form <- bf(ABUND ~ scale(log(DIST))+
##                     scale(log(LDIST))+
##                     scale(log(AREA))+
##                     fGRAZE+
##                     scale(ALT)+
##                     scale(YR.ISOL),
##                 family=Gamma(link='log'))
## get_prior(loyn.form,  data=loyn)
## loyn.brms <- brm(loyn.form, data=loyn, refresh=0,
##                  iter = 5000, thin=5,chains = 3, warmup=2000)
## 
## 
## preds <- posterior_predict(loyn.brms,  nsamples=250,  summary=FALSE)
## loyn.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = loyn$ABUND,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(loyn.resids)
## ggplot() +
##   geom_point(data=NULL,  aes(x=loyn$ABUND,  y=loyn.resids$scaledResiduals))
## 
## plot(loyn.brms)
## mcmc_plot(loyn.brms, type='acf_bar')
## mcmc_plot(loyn.brms, type='trace')
## 
## mcmc_plot(loyn.brms, type='rhat_hist')
## mcmc_plot(loyn.brms, type='neff_hist')
## conditional_effects(loyn.brms) %>%  plot(points=TRUE)
## 
## summary(loyn.brms)
## loyn.brms %>%
##   tidyMCMC(conf.int=TRUE,  conf.method = 'HPDinterval')
## emmeans(loyn.brms,  pairwise ~ fGRAZE,  type='link')$contrasts
## 
## loyn.brms %>%
##   posterior_samples() %>%
##   dplyr::select(-`lp__`, -shape) %>%
##   gather_variables() %>%
##   mutate(.value=exp(.value)) %>%
##   group_by(.variable) %>%
##   median_hdi
## 
## 
## 
## emmeans(loyn.brms,  pairwise ~ fGRAZE,  type='link')$contrasts %>%
##     gather_emmeans_draws() %>%
##     median_qi
## 
## emmeans(loyn.brms,  pairwise ~ fGRAZE,  type='link')$contrasts %>%
##     gather_emmeans_draws() %>%
##       mutate(.value=exp(.value)) %>%
##   median_qi
## 
## emmeans(loyn.brms,  pairwise ~ fGRAZE,  type='link')$contrasts %>%
##                                                      gather_emmeans_draws() %>%
##                                                      dplyr::select(-.chain, -.iteration, -.draw) %>%
##                                                      filter(contrast=='1 - 5') %>%
##                                                      pull(.value) %>%
##                                                      exp() %>%
##                                                      as.mcmc() %>%
##                                                      tidyMCMC(conf.int=TRUE,  conf.method='HPDinterval')
## 
## emmeans(loyn.brms,  pairwise ~ fGRAZE,  type='response')$contrasts
## 
## 
## bayes_R2(loyn.brms, summary=FALSE) %>% median_hdi
## 
## loyn.brms <- brm(ABUND ~ scale(log(AREA))+
##                     fGRAZE+
##                     scale(YR.ISOL), data=loyn,
##                   family=Gamma(link='log'), refresh=0,
##                  iter = 5000, thin=5,chains = 3, warmup=2000)
## loyn.list = with(loyn, list(AREA = seq(min(AREA), max(AREA), len=100)))
## newdata = emmeans(loyn.brms, ~AREA|fGRAZE, at = loyn.list, type='response') %>%
##     as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=response, x=AREA)) +
## geom_ribbon(aes(ymin=lower.HPD, ymax=upper.HPD, fill=fGRAZE), alpha=0.3) +
## geom_line(aes(color=fGRAZE)) +
## theme_bw() +
## scale_x_log10() + scale_y_log10()
## 
## 
## library(tidybayes)
## spaghetti = emmeans(loyn.brms, ~AREA|fGRAZE, at = loyn.list, type='response') %>%
##   gather_emmeans_draws() %>% mutate(Fit=exp(.value))
## wch = sample(1:max(spaghetti$.draw), 100,replace=FALSE)
## spaghetti = spaghetti %>% filter(.draw %in% wch)
## ggplot(newdata) +
##   geom_line(data=spaghetti, aes(y=Fit, x=AREA, color=fGRAZE,
##      group=interaction(fGRAZE,.draw)), alpha=0.05) +
##   geom_line(aes(y=response, x=AREA, color=fGRAZE)) +
##   theme_bw() +
##   scale_x_log10() + scale_y_log10()

