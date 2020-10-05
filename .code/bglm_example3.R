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
peake = read_csv('../data/peakquinn.csv', trim_ws=TRUE)
glimpse(peake)


## ----stan, results='markdown', eval=FALSE, hidden=TRUE, cache=TRUE------------
## peake.glmP = stan_glm(INDIV ~ log(AREA), data=peake,
##                       family=poisson(link='log'),
##                       iter = 5000,chains = 3, thin = 5,
##                       warmup=2000,refresh = 0)
## prior_summary(peake.glmP)
## 
## peake.glmP = stan_glm(INDIV ~ log(AREA), data=peake,
##                           family=poisson,
##                           iter = 5000,
##                          chains = 3, thin = 3, refresh = 0,
##                          prior_intercept = normal(0, 10),
##                          prior = normal(0, 2))
## posterior_vs_prior(peake.glmP, color_by='vs', group_by=TRUE,
##                    facet_args=list(scales='free_y'))


## ----stan.brms, results='markdown', eval=FALSE, hidden=TRUE, cache=TRUE-------
## peake.form <- bf(INDIV ~ log(AREA),  family=poisson(link='log'))
## get_prior(peake.form,  data=peake)
## median(log(peake$INDIV))
## mad(log(peake$INDIV))
## ## if we do not specify priors - then we cannot sample inproper priors
## peake.brmsP = brm(peake.form, data=peake,
##                   iter = 5000,chains = 3, thin = 5,
##                   warmup=2000,refresh = 0,
##                   sample_prior=TRUE)
## peake.brmsP %>%
##   posterior_samples() %>%
##   gather_variables() %>%
##   median_hdci
## peake.brmsP %>% get_variables()
## mcmc_plot(peake.brmsP,  pars='prior.*|b.*|sigma')


## ----modelValidation, results='markdown', eval=FALSE, hidden=TRUE, fig.width=6, fig.height=4----
## stan_trace(peake.glmP)
## stan_ac(peake.glmP)
## stan_rhat(peake.glmP)
## stan_ess(peake.glmP)
## 
## pp_check(peake.glmP, x=peake$AREA,plotfun='error_scatter_avg_vs_x')
## 
## 
## preds <- posterior_predict(peake.glmP,  nsamples=250,  summary=FALSE)
## peake.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = peake$INDIV,
##                             fittedPredictedResponse = apply(preds, 2, median),
##                             integerResponse = 'gaussian')
## plot(peake.resids)
## 
## ggplot() +
##   geom_point(data=NULL,  aes(x=peake$INDIV,  y=peake.resids$scaledResiduals))
## 
## 
## y=peake.glmP$y
## mu=fitted(peake.glmP)
## #wt=peake.glmP$weights
## r=poisson()$dev.resids(y,mu,wt=1)
## #r=poisson()$dev.resids(y,mu,wt=wt)
## res <- sqrt(pmax(r, 0))
## resid=ifelse(y > mu, res, -res)
## ggplot(data=NULL) +
##     geom_point(aes(y=resid, x=mu))
## RSS = sum(resid^2)
## RSS/(length(y)-2)


## ----modelValidation.brms, results='markdown', eval=FALSE, hidden=TRUE, fig.width=6, fig.height=4----
## plot(peake.brmsP)
## mcmc_plot(peake.brmsP, type='acf_bar')
## mcmc_plot(peake.brmsP, type='rhat_hist')
## mcmc_plot(peake.brmsP, type='neff_hist')
## 
## preds <- posterior_predict(peake.brmsP,  nsamples=250,  summary=FALSE)
## peake.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = peake$INDIV,
##                             fittedPredictedResponse = apply(preds, 2, median),
##                             integerResponse = 'gaussian')
## plot(peake.resids)
## 
## ggplot() +
##   geom_point(data=NULL,  aes(x=peake$INDIV,  y=peake.resids$scaledResiduals))
## 


## ----stan1, results='markdown', eval=FALSE, hidden=TRUE, cache=TRUE-----------
## peake.glmNB = stan_glm(INDIV ~ log(AREA), data=peake,
##                        family='neg_binomial_2',
##                        iter = 5000, warmup=2000,
##                        chains = 3, thin = 5, refresh = 0)
## prior_summary(peake.glmNB)
## peake.glmNB = stan_glm(INDIV ~ log(AREA), data=peake,
##                        family='neg_binomial_2',
##                        iter = 5000,
##                        chains = 3, thin = 2, refresh = 0,
##                        prior_intercept = normal(0, 10),
##                        prior = normal(0, 5),
##                        prior_aux = cauchy(0, 5))
## prior_summary(peake.glmNB)
## posterior_vs_prior(peake.glmNB, color_by='vs', group_by=TRUE,
##                    facet_args=list(scales='free_y'), na.rm=TRUE)
## 
## stan_trace(peake.glmNB)
## stan_ac(peake.glmNB)
## stan_rhat(peake.glmNB)
## stan_ess(peake.glmNB)
## 
## 
## 
## preds <- posterior_predict(peake.glmNB, nsamples=250,  summary=FALSE)
## peake.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = peake$INDIV,
##                             fittedPredictedResponse = apply(preds, 2, median),
##                             integerResponse = 'gaussian')
## plot(peake.resids)
## 
## ggplot() +
##   geom_point(data=NULL,  aes(x=peake$INDIV,  y=peake.resids$scaledResiduals))
## 
## pp_check(peake.glmNB, x=peake$AREA,plotfun='error_scatter_avg_vs_x')
## ggplot(data=NULL) +
##     geom_point(aes(y=resid(peake.glmNB), x=fitted(peake.glmNB)))
## 
## ##Deviance residuals (for plotting residuals)
## y=peake.glmNB$y
## mu=fitted(peake.glmNB)
## (theta = mean(as.matrix(peake.glmNB)[,'reciprocal_dispersion']))
## r=MASS:::neg.bin(theta)$dev.resids(y,mu,wt=1)
## #r=2 * wt * (y * log(pmax(1, y)/mu) - (y + theta) * log((y + theta)/(mu + theta)))
## res <- sqrt(pmax(r, 0))
## resid=ifelse(y > mu, res, -res)
## RSS = sum(resid^2)
## RSS/(nrow(peake)-3)
## 
## ggplot() + geom_point(data=NULL, aes(y=resid, x=mu)) + scale_x_log10()
## ggplot() + geom_point(data=NULL, aes(y=resid, x=peake$AREA)) + scale_x_log10()
## 
## #compare fit to observed
## y_pred = posterior_predict(peake.glmNB)
## newdata = peake %>% cbind(t(y_pred)) %>%
##     gather(key='Rep',value='Value',-AREA)
## ggplot(newdata, aes(Value,x=AREA)) +
##     geom_violin(color='blue', fill='blue',alpha=0.5) +
##     geom_violin(data=peake, aes(y=INDIV, x=AREA),
##                 fill='red',color='red', alpha=0.5)
## 
## ##expected log predictive density
## ##loo = leave one out
## ## similar to AIC except that AIC does not consider priors
## ## and assumes that the posterior likelihood is multivariate normal
## ## loo AIC does not and integrates over all uncertainty
## ## The difference in ELPD will be negative if the
## ## expected out-of-sample predictive accuracy of the first model is
## ## higher. If the difference is be positive then the second model is
## ## preferred.
## (l.glmP = loo(peake.glmP))
## (l.glmNB = loo(peake.glmNB))
## 
## loo_compare(l.glmP, l.glmNB)
## 
## ggpredict(peake.glmNB, term='AREA [exp]') %>% plot
## ggpredict(peake.glmNB, ~AREA) %>% plot
## 


## ----stan1.brms, results='markdown', eval=FALSE, hidden=TRUE, cache=TRUE------
## peake.form <- bf(INDIV ~ log(AREA),  family='negbinomial')
## peake.brmsNB = brm(peake.form, data=peake,
##                     iter = 5000, warmup=2000,
##                     chains = 3, thin = 5, refresh = 0)
## plot(peake.brmsNB)
## 
## mcmc_plot(peake.brmsNB, type='acf_bar')
## mcmc_plot(peake.brmsNB, type='rhat_hist')
## mcmc_plot(peake.brmsNB, type='neff_hist')
## 
## preds <- posterior_predict(peake.brmsNB,  nsamples=250,  summary=FALSE)
## peake.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = peake$INDIV,
##                             fittedPredictedResponse = apply(preds, 2, median),
##                             integerResponse = 'gaussian')
## plot(peake.resids)
## 
## ggplot() +
##   geom_point(data=NULL,  aes(x=peake$INDIV,  y=peake.resids$scaledResiduals))
## 
## (l.brmsP = loo(peake.brmsP))
## (l.brmsNB = loo(peake.brmsNB))
## 
## loo_compare(l.brmsP, l.brmsNB)


## ----Samples, results='markdown', eval=FALSE, hidden=TRUE---------------------
## summary(peake.glmNB)
## tidyMCMC(peake.glmNB$stanfit,conf.int=TRUE,
##          conf.method='HPDinterval', rhat=TRUE, ess=TRUE)
## 
## peake.glmNB %>%
##   gather_draws(x=`.*Intercept.*|.*AREA.*`,  regex=TRUE) %>%
##   median_hdci %>%
##   mutate(across(where(is.numeric), exp))
## 
## coefs = as.matrix(peake.glmNB)
## peake.mcmc = as.matrix(peake.glmNB)
## head(peake.mcmc)
## 
## 
## #prior_summary(peake.glmNB)
## #mcmcpvalue(as.matrix(peake.glmNB)[, c("log(AREA)")])
## #posterior_interval(peake.glmNB, prob=0.95)
## 
## #bayes_R2(peake.glmNB) %>% hist
## #bayes_R2(peake.glmNB) %>% median_hdi
## #bayes_R2(peake.glmNB) %>% as.mcmc %>% tidyMCMC(conf.int=TRUE, conf.method='HPDinterval', estimate.method = 'median')
## 


## ----Samples.brms, results='markdown', eval=FALSE, hidden=TRUE----------------
## summary(peake.brmsNB)
## tidyMCMC(peake.brmsNB$fit,conf.int=TRUE,
##          conf.method='HPDinterval', rhat=TRUE, ess=TRUE)
## 
## peake.brmsNB %>%
##   gather_draws(x=`.*Intercept.*|.*AREA.*`,  regex=TRUE) %>%
##   median_hdci %>%
##   mutate(across(where(is.numeric), exp))
## 
## peake.mcmc = peake.brmsNB %>%
##   tidy_draws()
## head(peake.mcmc)
## 
## 
## #prior_summary(peake.glmNB)
## #mcmcpvalue(as.matrix(peake.glmNB)[, c("log(AREA)")])
## #posterior_interval(peake.glmNB, prob=0.95)
## 
## bayes_R2(peake.brmsNB)
## bayes_R2(peake.brmsNB, summary=FALSE) %>% hist
## bayes_R2(peake.brmsNB, summary=FALSE) %>% median_hdci
## bayes_R2(peake.brmsNB, summary=FALSE) %>% as.mcmc %>% tidyMCMC(conf.int=TRUE, conf.method='HPDinterval', estimate.method = 'median')
## 


## ----posterior, results='markdown', eval=FALSE, hidden=TRUE-------------------
## ## Calculate on link scale so we have the flexibility
## peake.list = with(peake, list(AREA=c(5000,10000)))
## newdata = emmeans(peake.glmNB, ~AREA, at=peake.list, type='link') %>%
##   gather_emmeans_draws() %>%
##   spread(key=AREA, value=.value)
## newdata %>% head
## newdata = newdata %>%
##   mutate(Diff1 = exp(`10000` - `5000`),
##          Diff2 = exp(`10000`) - exp(`5000`),
##          PercDiff = 100*(exp(`10000`) - exp(`5000`))/exp(`5000`))
## newdata %>% head
## newdata %>% median_hdci
## newdata %>%
##   dplyr::select(Diff1, Diff2, PercDiff) %>%
##   gather() %>%
##   group_by(key) %>%
##   median_hdci
## ## Probability that the effect is greater than 50%
## newdata %>%
##   dplyr::select(Diff1, Diff2, PercDiff) %>%
##   summarize(P=sum(PercDiff > 50)/n())
## 
## 
## fit = emmeans(peake.glmNB, ~AREA, at=peake.list, type='link') %>%
##     gather_emmeans_draws() %>%
##     spread(key=AREA, value=.value) %>%
##     dplyr::select(-.chain, -.iteration, -.draw)
## ## Fractional change
## f1 = fit[,1]
## f2 = fit[,2]
## tidyMCMC(as.mcmc(exp(f2 - f1)),
##          conf.int = TRUE, conf.method = "HPDinterval")
## 
## ## Absolute change
## f1 = exp(fit[,1])
## f2 = exp(fit[,2])
## tidyMCMC(as.mcmc(f2 - f1),
##                  conf.int = TRUE, conf.method = "HPDinterval")
## ## Percentage change
## f1 = exp(fit[,1])
## f2 = exp(fit[,2])
## ESp = 100*(f2 - f1)/f1
## hist(ESp[,1])
## tidyMCMC(as.mcmc(ESp), conf.int = TRUE, conf.method = "HPDinterval")
## 
## ## Probability that the effect is greater than 50%
## ESp = 100*(f2 - f1)/f1
## sum(ESp > 50)/nrow(ESp)


## ----posteriori.brms, results='markdown', eval=FALSE, hidden=TRUE-------------
## ## Calculate on link scale so we have the flexibility
## peake.list = with(peake, list(AREA=c(5000,10000)))
## newdata = emmeans(peake.brmsNB, ~AREA, at=peake.list, type='link') %>%
##     gather_emmeans_draws() %>%
##   spread(key=AREA, value=.value)
## newdata %>% head
## newdata = newdata %>% mutate(Diff1 = exp(`10000` - `5000`),
##                              Diff2 = exp(`10000`) - exp(`5000`),
##                              PercDiff = 100*(exp(`10000`) - exp(`5000`))/exp(`5000`))
## newdata %>% head
## newdata %>%
##   dplyr::select(Diff1, Diff2, PercDiff) %>%
##   gather() %>%
##   group_by(key) %>%
##   median_hdci
## ## Probability that the effect is greater than 50%
## newdata %>%
##   dplyr::select(Diff1, Diff2, PercDiff) %>%
##   summarize(P=sum(PercDiff > 50)/n())
## 


## ----summaryFigure, results='markdown', eval=FALSE, hidden=TRUE---------------
## ## Graphics
## library(emmeans)
## peake.list = with(peake, list(AREA=seq(min(AREA), max(AREA), len=100)))
## newdata = emmeans(peake.glmNB, ~AREA, at=peake.list, type='response') %>%
##     as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=prob, x=AREA)) +
##     geom_ribbon(aes(ymin=lower.HPD, ymax=upper.HPD), fill='blue', alpha=0.3) +
##     geom_line() +
##     geom_point(data=peake, aes(y=INDIV, x=AREA)) +
##     scale_x_log10() +
##     scale_y_log10() +
##     theme_bw()


## ----summaryFigure.brms, results='markdown', eval=FALSE, hidden=TRUE----------
## ## Graphics
## library(emmeans)
## peake.list = with(peake, list(AREA=seq(min(AREA), max(AREA), len=100)))
## newdata = emmeans(peake.brmsNB, ~AREA, at=peake.list, type='response') %>%
##     as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=prob, x=AREA)) +
##     geom_ribbon(aes(ymin=lower.HPD, ymax=upper.HPD), fill='blue', alpha=0.3) +
##     geom_line() +
##     geom_point(data=peake, aes(y=INDIV, x=AREA)) +
##     scale_x_log10() +
##     scale_y_log10() +
##     theme_bw()

