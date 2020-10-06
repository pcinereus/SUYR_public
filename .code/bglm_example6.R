## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE)


## ----libraries, results='markdown', eval=TRUE, message=FALSE, warning=FALSE----
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
quinn = read_csv('../data/quinn.csv', trim_ws=TRUE)
glimpse(quinn)
summary(quinn)


## ----dataprep, results='markdown', eval=TRUE----------------------------------
quinn = quinn %>%
  mutate(SEASON = factor(SEASON,
                         levels=c('Summer', 'Autumn', 'Winter', 'Spring')),
                         DENSITY = factor(DENSITY))


## ----fitModel, results='markdown', eval=FALSE, hidden=TRUE--------------------
## head(quinn)
## ggplot(quinn, aes(y=RECRUITS, x=SEASON, fill=DENSITY)) + geom_boxplot()
## library(rstanarm)
## ##Poisson
## quinn.rstanarmP <- stan_glm(RECRUITS~SEASON*DENSITY, data=quinn,
##                          family=poisson(link='log'),refresh=0,
##                          chains = 3, iter = 5000, thin=5, warmup=2000)
## prior_summary(quinn.rstanarmP)
## 
## plot(quinn.rstanarmP,  'mcmc_trace')
## plot(quinn.rstanarmP,  'mcmc_acf_bar')
## plot(quinn.rstanarmP,  'mcmc_rhat_hist')
## plot(quinn.rstanarmP,  'mcmc_neff_hist')
## 
## 
## preds <- posterior_predict(quinn.rstanarmP,  nsamples=250,  summary=FALSE)
## quinn.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = quinn$RECRUITS,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(quinn.resids)
## testZeroInflation(quinn.resids)
## 
## 
## #pp_check(day.rstanarm, x=as.numeric(day$TREAT),'intervals')
## quinn.rstanarmNB <- stan_glm(RECRUITS~SEASON*DENSITY, data=quinn,
##                         family='neg_binomial_2', refresh=0,
##                         chains = 3,iter = 5000, thin=5,warmup=2000)
## 
## plot(quinn.rstanarmNB,  'mcmc_trace')
## plot(quinn.rstanarmNB,  'mcmc_acf_bar')
## plot(quinn.rstanarmNB,  'mcmc_rhat_hist')
## plot(quinn.rstanarmNB,  'mcmc_neff_hist')
## 
## 
## preds <- posterior_predict(quinn.rstanarmNB,  nsamples=250,  summary=FALSE)
## quinn.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = quinn$RECRUITS,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(quinn.resids)
## 
## prior_summary(quinn.rstanarmNB)
## (loo.P = loo(quinn.rstanarmP))
## (loo.NB = loo(quinn.rstanarmNB))
## loo_compare(loo.P, loo.NB)
## 
## posterior_vs_prior(quinn.rstanarmNB, color_by='vs', group_by=TRUE,
##                    facet_args=list(scales='free_y'))
## 
## 
## ggplot() + geom_point(data=NULL, aes(y=quinn.resids$scaledResiduals, x=quinn$SEASON))
## ggplot() + geom_point(data=NULL, aes(y=quinn.resids$scaledResiduals, x=quinn$DENSITY))
## 
## ## stan_trace(quinn.rstanarm)
## ## stan_dens(quinn.rstanarm,separate_chains=TRUE)
## ## stan_ac(quinn.rstanarm)
## ## stan_rhat(quinn.rstanarm)
## ## stan_ess(quinn.rstanarm)
## 
## ## pp_check(quinn.rstanarm)
## ## available_ppc()
## ## pp_check(quinn.rstanarm, x=quinn$SEASON,'boxplot')
## ## pp_check(quinn.rstanarm, group=interaction(quinn$DENSITY,quinn$SEASON),
## ##          'stat_grouped')
## ## pp_check(quinn.rstanarm, x=as.numeric(quinn$SEASON),group=quinn$DENSITY,
## ##          plotfun='intervals_grouped')
## ## #pp_check(quinn.rstanarm, x=as.numeric(quinn$SEASON), plotfun='error_scatter_avg_vs_x')
## 
## 
## g=ggpredict(quinn.rstanarmNB) %>% plot
## library(patchwork)
## g[[1]] + g[[2]]
## do.call('+', g)
## 
## summary(quinn.rstanarmNB)
## tidyMCMC(quinn.rstanarmNB$stanfit,
##          estimate.method='median',
##          conf.int=TRUE, conf.method='HPDinterval',
##          rhat=TRUE, ess=TRUE)
## ## express this on response scale
## quinn.rstanarmNB %>% get_variables()
## quinn.rstanarmNB %>%
##   gather_draws(`.Intercept.*|SEASON.*|DENSITY.*`, regex=TRUE) %>%
##   group_by(.variable) %>%
##   mutate(.value=exp(.value)) %>%
##   median_hdci
## 
## newdata = emmeans(quinn.rstanarmNB, ~SEASON|DENSITY, type='response') %>%
##     as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=prob, x=SEASON, fill=DENSITY)) +
##     geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD), shape=21,
##                     position=position_dodge(width=0.5)) +
##     #geom_line(aes(x=as.numeric(SEASON))) +
##     theme_bw()
## 
## 
## bayes_R2(quinn.rstanarmNB) %>% median_hdi
## 
## ## Compare effect of density separate within each season
## emmeans(quinn.rstanarmNB, pairwise~DENSITY|SEASON, type='response')$contrast
## quinn.em = emmeans(quinn.rstanarmNB, pairwise~DENSITY|SEASON, type='link')$contrast %>%
##   gather_emmeans_draws() %>%
##   mutate(Fit=exp(.value))
## head(quinn.em)
## 
## g2 = quinn.em %>%
##   group_by(contrast, SEASON) %>%
##   median_hdci %>%
##     ggplot() + geom_pointrange(aes(y=Fit, x=SEASON, ymin=Fit.lower, ymax=Fit.upper)) +
##     geom_hline(yintercept=1, linetype='dashed') + coord_flip() +
##     scale_y_continuous('Effect size (High/Low)')
## 
## ggplot(quinn.em, aes(x=Fit)) +
##     geom_histogram() +
##     facet_wrap(SEASON~contrast, scales='free')
## quinn.em %>% group_by(contrast, SEASON) %>% mean_hdci(Fit)
## # Probability of effect
## quinn.em %>% group_by(contrast,SEASON) %>% summarize(P=sum(Fit>1)/n())
## ##Probability of effect greater than 10%
## quinn.em %>% group_by(contrast,SEASON) %>% summarize(P=sum(Fit>1.1)/n())
## 
## 
## 
## bayes_R2(quinn.rstanarmNB) %>% median_hdci
## 
## newdata = emmeans(quinn.rstanarmNB, ~SEASON|DENSITY, type='response') %>% as.data.frame
## head(newdata)
## g1 = ggplot(newdata, aes(y=prob, x=SEASON, color=DENSITY)) +
##     geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD),
##                     position=position_dodge(width=0.2))
## library(patchwork)
## g1 + g2
## grid.arrange(g1, g2, nrow=1)
## 
## 
## 
## 
## loo(quinn.rstanarmP)
## 
## quinn.rstanarm1 <- stan_glm(RECRUITS~SEASON+DENSITY, data=quinn,
##                         family='neg_binomial_2',
##                        prior = normal(0, 1),
##                        prior_intercept = normal(0,10),
##                        prior_aux=cauchy(0,2),
##                        chains = 3,iter = 2000, thin=2,warmup=1000)
## 
## quinn.rstanarm <- stan_glm(RECRUITS~SEASON*DENSITY, data=quinn,
##                         family='neg_binomial_2',
##                        prior = normal(0, 10),
##                        prior_intercept = normal(0,10),
##                        prior_aux=cauchy(0,2),
##                        chains = 3,iter = 2000, thin=2,warmup=1000)
## quinn.rstanarm1 <- stan_glm(RECRUITS~SEASON+DENSITY, data=quinn,
##                         family='neg_binomial_2',
##                        prior = normal(0, 10),
##                        prior_intercept = normal(0,10),
##                        prior_aux=cauchy(0,2),
##                        chains = 3,iter = 2000, thin=2,warmup=1000)
## 
## l1 = loo(quinn.rstanarm)
## l2 = loo(quinn.rstanarm1)
## compare_models(l1,l2)
## 
## newdata <- with(quinn,expand.grid(SEASON=levels(SEASON),
##                                   DENSITY=levels(DENSITY)))
## Xmat<- model.matrix(~SEASON*DENSITY, data=newdata)
## as.matrix(quinn.rstanarm) %>% head
## coefs = as.matrix(quin.rstanarm)
## coefs = as.matrix(as.data.frame(quinn.rstanarm) %>%
##                   dplyr:::select(-reciprocal_dispersion)) %>%
##     as.matrix
## coefs = as.data.frame(quinn.rstanarm) %>%
##     dplyr:::select(-reciprocal_dispersion) %>%
##     as.matrix
## fit = exp(coefs %*% t(Xmat))
## newdata = newdata %>%
##     cbind(tidyMCMC(fit, conf.int=TRUE, conf.method='HPDinterval'))
## head(newdata)
## 
## ggplot(newdata, aes(y=estimate, x=SEASON, fill=DENSITY)) +
## geom_blank() +
## geom_line(aes(x=as.numeric(SEASON), ymin=conf.low, ymax=conf.high, linetype=DENSITY))+
## geom_pointrange(aes(ymin=conf.low, ymax=conf.high), shape=21)
## 
## #Compare high and low in each season
## #via contrasts
## newdata <- with(quinn,expand.grid(SEASON=levels(SEASON),DENSITY=levels(DENSITY)))
## ## factor differences
## Xmat<- model.matrix(~SEASON*DENSITY, data=newdata)
## Xmat.high <- Xmat[newdata$DENSITY=="High",]
## Xmat.low <- Xmat[newdata$DENSITY=="Low",]
## Xmat.density <- Xmat.high-Xmat.low
## rownames(Xmat.density) <- levels(quinn$SEASON)
## coefs = as.matrix(as.data.frame(quinn.rstanarm) %>% dplyr:::select(-reciprocal_dispersion))
## fit = exp(coefs %*% t(Xmat.density))
## tidyMCMC(fit, conf.int=TRUE, conf.method='HPDinterval')
## ## or absolute
## fit.high = coefs %*% t(Xmat.high)
## fit.low = coefs %*% t(Xmat.low)
## fit = exp(fit.high) - exp(fit.low)
## #fit = exp(fit.high - fit.low)
## tidyMCMC(fit, conf.int=TRUE, conf.method='HPDinterval')


## ----fitModel.brms, results='markdown', eval=FALSE, hidden=TRUE---------------
## head(quinn)
## ggplot(quinn, aes(y=RECRUITS, x=SEASON, fill=DENSITY)) + geom_boxplot()
## library(rstanarm)
## ##Poisson
## quinn.form <- bf(RECRUITS ~ SEASON*DENSITY,  family=poisson(link='log'))
## get_prior(quinn.form,  data=quinn)
## 
## quinn.brmsP <- brm(quinn.form, data=quinn,
##                          refresh=0,
##                          chains = 3, iter = 5000, thin=5, warmup=2000)
## 
## mcmc_plot(quinn.brmsP,  type='trace')
## mcmc_plot(quinn.brmsP,  type='acf_bar')
## mcmc_plot(quinn.brmsP,  type='rhat_hist')
## mcmc_plot(quinn.brmsP,  type='neff_hist')
## 
## 
## preds <- posterior_predict(quinn.brmsP,  nsamples=250,  summary=FALSE)
## quinn.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = quinn$RECRUITS,
##                             fittedPredictedResponse = apply(preds, 2, median),
##                             integerResponse = TRUE)
## plot(quinn.resids)
## 
## 
## quinn.form <- bf(RECRUITS ~ SEASON*DENSITY,  family=negbinomial(link='log'))
## get_prior(quinn.form,  data=quinn)
## 
## quinn.brmsNB <- brm(quinn.form, data=quinn,
##                          refresh=0,
##                          chains = 3, iter = 5000, thin=5, warmup=2000)
## 
## mcmc_plot(quinn.brmsNB,  type='trace')
## mcmc_plot(quinn.brmsNB,  type='acf_bar')
## mcmc_plot(quinn.brmsNB,  type='rhat_hist')
## mcmc_plot(quinn.brmsNB,  type='neff_hist')
## 
## 
## preds <- posterior_predict(quinn.brmsNB,  nsamples=250,  summary=FALSE)
## quinn.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = quinn$RECRUITS,
##                             fittedPredictedResponse = apply(preds, 2, median),
##                             integerResponse=TRUE)
## plot(quinn.resids)
## #pp_check(day.rstanarm, x=as.numeric(day$TREAT),'intervals')
## 
## (loo.P = loo(quinn.brmsP))
## (loo.NB = loo(quinn.brmsNB))
## loo_compare(loo.P, loo.NB)
## 
## ggplot() +
##   geom_point(data=NULL, aes(y=quinn.resids$scaledResiduals, x=quinn$SEASON))
## ggplot() +
##   geom_point(data=NULL, aes(y=quinn.resids$scaledResiduals, x=quinn$DENSITY))
## 
## 
## #g=ggpredict(quinn.brmsNB) %>% plot
## ggemmeans(quinn.brmsNB, terms=~SEASON*DENSITY) %>% plot
## #g[[1]] + g[[2]]
## #do.call('grid.arrange', g)
## 
## summary(quinn.brmsNB)
## tidyMCMC(quinn.brmsNB$fit,
##          estimate.method='median',
##          conf.int=TRUE, conf.method='HPDinterval',
##          rhat=TRUE, ess=TRUE)
## emmeans(quinn.brmsNB, pairwise~SEASON, type='link')
## newdata =  emmeans(quinn.brmsNB, ~SEASON|DENSITY, type='response') %>%
##     as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=prob, x=SEASON, fill=DENSITY)) +
##     geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD), shape=21,
##                     position=position_dodge(width=0.5)) +
##     #geom_line(aes(x=as.numeric(SEASON))) +
##     theme_bw()
## 
## 
## bayes_R2(quinn.brmsNB, summary=FALSE) %>% median_hdi
## 
## ## Compare effect of density separate within each season
## emmeans(quinn.brmsNB, pairwise~DENSITY|SEASON, type='response')
## quinn.em = emmeans(quinn.brmsNB, pairwise~DENSITY|SEASON, type='link')$contrast %>%
##   gather_emmeans_draws() %>%
##   mutate(Fit=exp(.value))
## head(quinn.em)
## 
## g2 = quinn.em %>%
##   group_by(contrast, SEASON) %>%
##   median_hdi %>%
##   ggplot() +
##   geom_pointrange(aes(y=Fit, x=SEASON, ymin=Fit.lower, ymax=Fit.upper)) +
##   geom_hline(yintercept=1, linetype='dashed') + coord_flip() +
##   scale_y_continuous('Effect size (High/Low)')
## g2
## 
## ggplot(quinn.em, aes(x=Fit)) +
##     geom_histogram() +
##   facet_wrap(SEASON~contrast, scales='free')
## quinn.em %>% group_by(contrast, SEASON) %>% mean_hdci()
## # Probability of effect
## quinn.em %>% group_by(contrast,SEASON) %>% summarize(P=sum(Fit>1)/n())
## ##Probability of effect greater than 10%
## quinn.em %>% group_by(contrast,SEASON) %>% summarize(P=sum(Fit>1.1)/n())
## 
## 
## 
## bayes_R2(quinn.brmsNB, summary=FALSE) %>% median_hdi
## 
## newdata = emmeans(quinn.brmsNB, ~SEASON|DENSITY, type='response') %>% as.data.frame
## head(newdata)
## g1 = ggplot(newdata, aes(y=prob, x=SEASON, color=DENSITY)) +
##     geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD),
##                     position=position_dodge(width=0.2))
## library(patchwork)
## g1 + g2
## #grid.arrange(g1, g2, nrow=1)
## 
## 
## 
## quinn = quinn %>%
##   group_by(SEASON, DENSITY) %>%
##   mutate(Obs=factor(1:n()))
## 
## quinn.form <- bf(RECRUITS ~ SEASON*DENSITY + (1|Obs),  family=poisson(link='log'))
## get_prior(quinn.form,  data=quinn)
## 
## quinn.brmsU <- brm(quinn.form, data=quinn,
##                          refresh=0,
##                          chains = 3, iter = 5000, thin=5, warmup=2000)
## 
## preds <- posterior_predict(quinn.brmsU,  nsamples=250,  summary=FALSE)
## quinn.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = quinn$RECRUITS,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(quinn.resids)
## newdata = emmeans(quinn.brmsU, ~SEASON|DENSITY, type='response') %>% as.data.frame
## newdata
## ggplot(newdata, aes(y=rate, x=SEASON, color=DENSITY)) +
##     geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD),
##                     position=position_dodge(width=0.2))
## 

