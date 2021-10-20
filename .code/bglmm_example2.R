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
## #ggplot(norin, aes(y=CHANGE, x=MASS, shape=TRIAL, color=TRIAL)) +
## #geom_smooth(method='lm') + geom_point()
## ggplot(norin, aes(y=CHANGE, x=as.numeric(FISHID), color=TRIAL)) +
##     geom_point() + geom_line()
## 
## #ggplot(norin, aes(y=MASS, x=TRIAL)) + geom_boxplot()
## ggplot(norin, aes(y=CHANGE, x=MASS, color=TRIAL)) + geom_point() + geom_smooth(method='lm')
## 
## norin.rstanarm = stan_glmer(CHANGE ~ (1|FISHID)+TRIAL*SMR_contr+MASS, data=norin,
##                             prior_PD=TRUE,
##                          iter=5000, warmup=2000, chains=3, thin=5, refresh=0)
## prior_summary(norin.rstanarm)
## 
## posterior_vs_prior(norin.rstanarm, color_by='vs', group_by=TRUE,
##                    facet_args=list(scales='free_y'), pars=c('(Intercept)'))
## ggpredict(norin.rstanarm, ~TRIAL*SMR_contr) %>% plot(add.data=TRUE)
## 
## norin.rstanarm %>% get_variables()
## plot(norin.rstanarm,  'mcmc_trace', regex_pars='^.Intercept|TRIAL|SMR|MASS|[sS]igma')
## plot(norin.rstanarm,  'mcmc_acf_bar', regex_pars='^.Intercept|TRIAL|SMR|MASS|[sS]igma')
## plot(norin.rstanarm,  'mcmc_rhat_hist', regex_pars='^.Intercept|TRIAL|SMR|MASS|[sS]igma')
## plot(norin.rstanarm,  'mcmc_neff_hist', regex_pars='^.Intercept|TRIAL|SMR|MASS|[sS]igma')
## 
## #norin.rstan1 = stan_glmer(CHANGE ~ (TRIAL|FISHID)+TRIAL*SMR_contr+MASS, data=norin,
## #                          iter=5000, warmup=2000, chains=3, thin=5, refresh=0, cores=3)
## norin.rstanarm1 = stan_glmer(CHANGE ~ (SMR_contr|FISHID) + TRIAL*SMR_contr+MASS, data=norin,
##                              prior_PD=FALSE,
##                           iter=5000, warmup=2000, chains=3, thin=5, refresh=0, cores=3)
## norin.rstanarm1 = update(norin.rstanarm1,  prior_PD=FALSE)
## 
## 
## 
## norin.rstanarm2 = stan_glmer(CHANGE ~ (TRIAL*SMR_contr|FISHID) + TRIAL*SMR_contr+MASS, data=norin,
##                              prior_PD=FALSE,
##                           iter=5000, warmup=2000, chains=3, thin=5, refresh=0, cores=3)
## 
## posterior_vs_prior(norin.rstanarm1, color_by='vs', group_by=TRUE,
##                    facet_args=list(scales='free_y'), pars=c('(Intercept)'))
## 
## ggpredict(norin.rstanarm1, ~TRIAL*SMR_contr) %>% plot(add.data=TRUE)
## 
## norin.rstanarm1 %>% get_variables()
## plot(norin.rstanarm1,  'mcmc_trace', regex_pars='^.Intercept|TRIAL|^SMR|MASS|[sS]igma')
## plot(norin.rstanarm1,  'mcmc_acf_bar', regex_pars='^.Intercept|TRIAL|^SMR|MASS|[sS]igma')
## plot(norin.rstanarm1,  'mcmc_rhat_hist', regex_pars='^.Intercept|TRIAL|^SMR|MASS|[sS]igma')
## plot(norin.rstanarm1,  'mcmc_neff_hist', regex_pars='^.Intercept|TRIAL|^SMR|MASS|[sS]igma')
## 
## 
## (l.1 <- loo(norin.rstanarm))
## (l.2 <- loo(norin.rstanarm1))
## loo_compare(l.1,  l.2)
## 
## 
## preds <- posterior_predict(norin.rstanarm,  nsamples=250,  summary=FALSE)
## norin.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = norin$CHANGE,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(norin.resids)
## 
## 
## g=ggpredict(norin.rstanarm) %>% plot
## do.call('grid.arrange', g)
## 
## #ggemmeans(norin.rstan, ~TRIAL)
## 
## summary(norin.rstanarm)
## nms <- norin.rstanarm1 %>% get_variables()
## wch <- grep('^.Intercept|TRIAL|^SMR|[sS]igma', nms)
## tidyMCMC(norin.rstanarm$stanfit,conf.int=TRUE, conf.method='HPDinterval',
##          rhat=TRUE, ess=TRUE, pars=nms[wch], estimate.method='median')
## 
## tidyMCMC(norin.rstanarm1$stanfit,conf.int=TRUE, conf.method='HPDinterval',
##          rhat=TRUE, ess=TRUE, pars=nms[wch], estimate.method='median')
## 
## 
## norin.grid = with(norin, list(SMR_contr=seq(min(SMR_contr),max(SMR_contr), len=100)))
## newdata = emmeans(norin.rstanarm, ~TRIAL|SMR_contr, at=norin.grid) %>% as.data.frame
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
## norin.em
## norin.em %>% group_by(contrast) %>% median_hdci(Fit)
## norin.em %>% group_by(contrast, SMR_contr) %>% median_hdci(Fit)
## ## norin.em %>%
## ##     group_by(contrast) %>%
## ##     summarize(P=sum(Fit>0)/n())
## norin.em %>%
##     group_by(contrast, SMR_contr) %>%
##     summarize(P=sum(Fit>0)/n())
## 
## 
## bayes_R2(norin.rstanarm, re.form=NA) %>% median_hdi
## bayes_R2(norin.rstanarm, re.form=~(1|FISHID)) %>% median_hdi
## #bayes_R2(norin.rstan1, re.form=~(SMR_contr|FISHID)) %>% median_hdi
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
## ##ggplot(norin, aes(y=MASS, x=TRIAL)) + geom_boxplot()
## ##ggplot(norin, aes(y=CHANGE, x=MASS, color=TRIAL)) + geom_point() + geom_smooth(method='lm')
## 
## norin %>% group_by(TRIAL) %>%
##     summarise(median(CHANGE),
##               mad(CHANGE))
## priors <- prior(normal(50, 20), class='Intercept') +
##     prior(normal(0, 10), class='b') +
##     prior(gamma(2,1), class='sigma') +
##     prior(gamma(2,1), class='sd')
## 
## norin.form <- bf(CHANGE ~ (1|FISHID)+TRIAL*SMR_contr+MASS,
##                  family=gaussian)
## 
## norin.brm1 = brm(norin.form,
##                  data=norin,
##                  prior = priors,
##                  sample_prior = 'yes',
##                  iter=5000, warmup=2000,
##                  chains=3, thin=5, refresh=0)
## 
## norin.brm1 %>% get_variables()
## pars <- norin.brm1 %>% get_variables()
## wch <- grepl('^b.Intercept|TRIAL|SMR|MASS|[sS]igma|^sd', pars, perl=TRUE)
## 
## stan_trace(norin.brm1$fit, pars=pars[wch])
## stan_ac(norin.brm1$fit, pars=pars[wch])
## stan_rhat(norin.brm1$fit, pars=pars[wch])
## stan_ess(norin.brm1$fit, pars=pars[wch])
## 
## ##mcmc_plot(norin.brms,  type='trace',
## ##          regex_pars='^b.*|sigma|^sd')
## ##mcmc_plot(norin.brms,  type='trace',
## ##          regex_pars='^b.Intercept|TRIAL|SMR|MASS|[sS]igma|^sd')
## ##mcmc_plot(norin.brms,  type='acf_bar',
## ##          regex_pars='^b.Intercept|TRIAL|SMR|MASS|[sS]igma|^sd')
## ##mcmc_plot(norin.brms,  type='rhat_hist',
## ##          regex_pars='^b.Intercept|TRIAL|SMR|MASS|[sS]igma|^sd')
## ##mcmc_plot(norin.brms,  type='neff_hist',
## ##          regex_pars='^b.Intercept|TRIAL|SMR|MASS|[sS]igma|^sd')
## 
## preds <- posterior_predict(norin.brm1,  nsamples=250,  summary=FALSE)
## norin.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = norin$CHANGE,
##                             fittedPredictedResponse = apply(preds, 2, median),
##                             integerResponse =FALSE)
## plot(norin.resids)
## #norin.rstan1 = stan_glmer(CHANGE ~ (TRIAL|FISHID)+TRIAL*SMR_contr+MASS, data=norin,
## #                          iter=5000, warmup=2000, chains=3, thin=5, refresh=0, cores=3)
## norin.form <- bf(CHANGE ~ (TRIAL|FISHID) + TRIAL*SMR_contr+MASS,
##                  family=gaussian)
## norin.brm2 = brm(norin.form, data=norin,
##                  prior = priors,
##                  sample_prior = 'yes',
##                  iter=5000, warmup=2000,
##                  chains=3, thin=5, refresh=0, cores=3,
##                  control=list(adapt_delta=0.99))
## 
## norin.brm2 %>% get_variables()
## 
## pars <- norin.brm2 %>% get_variables()
## ## wch <- grepl('^b.Intercept|TRIAL|SMR|MASS|[sS]igma|^sd', pars, perl=TRUE)
## wch <- grepl('^b_.*|[sS]igma|^sd_.*', pars, perl=TRUE)
## 
## stan_trace(norin.brm2$fit, pars=pars[wch])
## stan_ac(norin.brm2$fit, pars=pars[wch])
## stan_rhat(norin.brm2$fit)#, pars=pars[wch])
## stan_ess(norin.brm2$fit)#, pars=pars[wch])
## ##mcmc_plot(norin.brm2,  type='trace',
## ##          regex_pars='^b.Intercept|TRIAL|SMR|MASS|[sS]igma|^sd')
## ##mcmc_plot(norin.brm2,  type='trace',
## ##          regex_pars='^b.Intercept|TRIAL|SMR|MASS|[sS]igma|^sd')
## ##mcmc_plot(norin.brm2,  type='acf_bar',
## ##          regex_pars='^b.Intercept|TRIAL|SMR|MASS|[sS]igma|^sd')
## ##mcmc_plot(norin.brm2,  type='rhat_hist',
## ##          regex_pars='^b.Intercept|TRIAL|SMR|MASS|[sS]igma|^sd')
## ##mcmc_plot(norin.brm2,  type='neff_hist',
## ##          regex_pars='^b.Intercept|TRIAL|SMR|MASS|[sS]igma|^sd')
## 
## (l.1 <- loo(norin.brm1))
## (l.2 <- loo(norin.brm2))
## loo_compare(l.1,  l.2)
## 
## 
## preds <- posterior_predict(norin.brm2,  nsamples=250,  summary=FALSE)
## norin.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = norin$CHANGE,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(norin.resids)
## 
## g <- norin.brm2 %>%
##     conditional_effects() %>%
##     plot(points=TRUE, ask=FALSE)
## library(patchwork)
## g[[1]] + g[[2]] + g[[3]] + g[[4]]
## 
## 
## ##g=ggpredict(norin.brms1) %>% plot
## ##library(patchwork)
## ##g[[1]] + g[[2]] + g[[3]]
## 
## ##do.call('grid.arrange', g)
## 
## ggemmeans(norin.brm2, ~TRIAL) %>% plot
## 
## summary(norin.brm2)
## 
## tidyMCMC(norin.brm2$fit,conf.int=TRUE, conf.method='HPDinterval',
##          rhat=TRUE, ess=TRUE, estimate.method='median') %>%
##   slice(1:11)
## 
## pars <- norin.brm2 %>% get_variables()
## wch <- grep('^b.Intercept|TRIAL|^b.*SMR|[sS]igma|^sd', pars)
## tidyMCMC(norin.brms1$fit,conf.int=TRUE, conf.method='HPDinterval',
##          rhat=TRUE, ess=TRUE, pars=pars[wch], estimate.method='median')
## 
## bayes_R2(norin.brm2, re.form=NA,  summary=FALSE) %>%
##     median_hdci
## bayes_R2(norin.brm2, re.form=~(1|FISHID), summary=FALSE) %>%
##     median_hdci
## bayes_R2(norin.brm2, re.form=~(TRIAL|FISHID), summary=FALSE) %>%
##     median_hdci
## 
## emmeans(norin.brm2, pairwise~TRIAL)
## 
## 
## norin.em <- norin.brm2 %>%
##     emmeans(~TRIAL) %>%
##     pairs() %>%
##     gather_emmeans_draws() %>%
##     mutate(Fit=.value)
## 
## norin.em %>%
##   group_by(contrast) %>%
##   median_hdi()
## 
## norin.em %>%
##     ggplot() +
##     geom_vline(xintercept=0, linetype='dashed') +
##     stat_slab(aes(x=.value, y=contrast,
##                   fill = stat(ggdist::cut_cdf_qi(cdf,
##                             .width = c(0.5, 0.8, 0.95),
##                             labels = scales::percent_format())
##                             )), color='black') +
##     scale_fill_brewer('Interval', direction = -1, na.translate = FALSE) +
##     theme_bw()
## 
## norin.em %>%
##     group_by(contrast) %>%
##   summarize(P=sum(Fit>0)/n())
## 
## 
## norin.grid <- with(norin, list(SMR_contr=c(min(SMR_contr),
##                                            mean(SMR_contr),
##                                            max(SMR_contr))))
## 
## norin.em <- norin.brm2 %>%
##     emmeans(~TRIAL|SMR_contr, at=norin.grid) %>%
##     pairs() %>%
##     gather_emmeans_draws()
## 
## norin.em %>% head
## norin.em %>%
##     group_by(contrast, SMR_contr) %>%
##     median_hdi()
## 
## norin.em %>%
##     group_by(contrast, SMR_contr) %>%
##     summarize(P=sum(.value>0)/n())
## 
## norin.grid <- with(norin, list(SMR_contr=modelr::seq_range(SMR_contr, n=100)))
## newdata <- norin.brm2 %>%
##     emmeans(~SMR_contr|TRIAL, at=norin.grid) %>%
##     as.data.frame
## head(newdata)
## partial.obs <- norin %>%
##     mutate(Pred = predict(norin.brm2, re.form = NA, summary=TRUE)[,'Estimate'],
##            Resid = resid(norin.brm2)[,'Estimate'],
##            Obs = Pred + Resid)
## ggplot(newdata, aes(y=emmean, x=SMR_contr, color=TRIAL)) +
##     geom_point(data=partial.obs, aes(y=Obs)) +
##     ##geom_point(data=partial.obs, aes(y=CHANGE), shape=2) +
##     geom_ribbon(aes(ymin=lower.HPD, ymax=upper.HPD, fill=TRIAL), alpha=0.3,color=NA) +
##     geom_line()

