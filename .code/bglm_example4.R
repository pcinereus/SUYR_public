## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE)


## ----libraries, results='markdown', eval=TRUE---------------------------------
library(car)
library(rstanarm)   #for fitting models in STAN
library(brms)       #for fitting models in STAN
library(coda)       #for diagnostics
library(bayesplot)  #for diagnostics
library(ggmcmc)     #for MCMC diagnostics
library(rstan)      #for interfacing with STAN
library(emmeans)    #for marginal means etc
library(DHARMa)     #for residual diagnostics
library(broom)      #for tidying outputs
library(tidybayes)  #for more tidying outputs
library(ggeffects)  #for partial plots
library(broom.mixed)#for summarising models
library(ggeffects)  #for partial effects plots
library(tidyverse)  #for data wrangling etc


## ----readData, results='markdown', eval=TRUE----------------------------------
loyn = read_csv('../data/loyn.csv', trim_ws=TRUE)
glimpse(loyn)


## ----processData, results='markdown', eval=TRUE-------------------------------
loyn = loyn %>% mutate(fGRAZE=factor(GRAZE))


## ----EDA1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
scatterplotMatrix(~ABUND+DIST+LDIST+AREA+GRAZE+ALT+YR.ISOL, data=loyn,
                  diagonal = list(method='boxplot'))


## ----EDA1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
scatterplotMatrix(~ABUND+log(DIST)+log(LDIST)+log(AREA)+GRAZE+ALT+YR.ISOL, data=loyn,
                  diagonal = list(method='boxplot'))


## ----lm, results='markdown', eval=TRUE, hidden=TRUE---------------------------
loyn.lm<-lm(ABUND~scale(log(DIST))+scale(log(LDIST))+scale(log(AREA))+
              fGRAZE + scale(ALT) + scale(YR.ISOL), data=loyn)
summary(loyn.lm)


## ----fitModel1a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
loyn.rstanarm = stan_glm(ABUND ~ scale(log(DIST), scale=FALSE)+
                              scale(log(LDIST), scale=FALSE)+
                              scale(log(AREA), scale=FALSE)+
                              fGRAZE+
                              scale(ALT, scale=FALSE)+
                              scale(YR.ISOL, scale=FALSE),
                         data=loyn,
                         family=gaussian(link='log'), 
                         iter = 5000, warmup = 1000,
                         chains = 3, thin = 5, refresh = 0)


## ----fitModel1b, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
prior_summary(loyn.rstanarm)


## ----fitModel1f, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
loyn.rstanarm1 <- update(loyn.rstanarm,  prior_PD=TRUE)

## ----fitModel1g, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
ggemmeans(loyn.rstanarm1,  ~AREA) %>% plot(add.data=TRUE) + scale_y_log10()


## ----fitModel1h, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
loyn.rstanarm2 <- stan_glm(ABUND ~ scale(log(DIST))+
                             scale(log(LDIST))+
                              scale(log(AREA))+
                              fGRAZE+
                              scale(ALT)+
                              scale(YR.ISOL), data=loyn,
                          family=gaussian(link='log'),
                          prior_intercept = normal(3, 10,  autoscale=FALSE),
                          prior = normal(0, 2.5, autoscale=FALSE),
                          prior_aux = cauchy(0, 5),
                          prior_PD=TRUE, 
                          iter = 5000, thin=5,chains = 3, warmup=2000, 
                          refresh=0) 


## ----fitModel1i, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
ggemmeans(loyn.rstanarm2,  ~AREA) %>%
  plot(add.data=TRUE) + scale_y_log10()


## ----fitModel1j, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE, dependson='fitModel1h'----
loyn.rstanarm3= update(loyn.rstanarm2,  prior_PD=FALSE)


## ----modelFit1k, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
posterior_vs_prior(loyn.rstanarm3, color_by='vs', group_by=TRUE,
                   facet_args=list(scales='free_y'))


## ----modelFit1l, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
#ggemmeans(loyn.rstanarm3,  ~AREA) %>% plot(add.data=TRUE) + scale_y_log10()


## ----fitModel2a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
loyn.brm = brm(bf(ABUND ~ scale(log(DIST))+
                     scale(log(LDIST))+
                     scale(log(AREA))+
                     fGRAZE+
                     scale(ALT)+
                     scale(YR.ISOL),
                   family=lognormal()),
                data=loyn,
                iter = 5000, warmup = 1000,
                chains = 3, thin = 5, refresh = 0)


## ----fitModel2b, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE, paged.print=FALSE,tidy.opts = list(width.cutoff = 80), echo=2----
options(width=100)
prior_summary(loyn.brm)
options(width=80)


## ----fitModel2d, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
loyn.brm1 = brm(bf(ABUND ~ scale(log(DIST))+
                     scale(log(LDIST))+
                     scale(log(AREA))+
                     fGRAZE+
                     scale(ALT)+
                     scale(YR.ISOL),
                   family=gaussian(link='log')),
                data=loyn, 
                prior=c(
                  prior(normal(0, 2.5), class='b')), 
                sample_prior = 'only', 
                iter = 5000, warmup = 1000,
                chains = 3, thin = 5, refresh = 0)


## ----fitModel2e, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
ggemmeans(loyn.brm1,  ~AREA) %>% plot(add.data=TRUE) + scale_y_log10()
conditional_effects(loyn.brm1) %>%  plot(points=TRUE)


## ----fitModel2h, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
loyn.brm2 = brm(bf(ABUND ~ scale(log(DIST))+
                     scale(log(LDIST))+
                     scale(log(AREA))+
                     fGRAZE+
                     scale(ALT)+
                     scale(YR.ISOL),
                   family=gaussian(link='log')),
                data=loyn, 
                prior=c(
                   prior(normal(0, 10),  class='Intercept'),
                  prior(normal(0, 2.5), class='b'), 
                  prior(cauchy(0, 5), class='sigma')
                ), 
                sample_prior = 'only', 
                iter = 5000, warmup = 1000,
                chains = 3, thin = 5, refresh = 0)


## ----fitModel2i, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
ggemmeans(loyn.brm2,  ~AREA) %>%
  plot(add.data=TRUE) + scale_y_log10()


## ----fitModel2j, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
loyn.brm3 <- update(loyn.brm2,  sample_prior=TRUE, refresh=0)


## ----fitModel2k, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
loyn.brm3 %>% get_variables()
loyn.brm3 %>%
  posterior_samples %>%
  select(-`lp__`) %>%
  gather %>%
  mutate(Type=ifelse(str_detect(key, 'prior'), 'Prior', 'b'),
         Class=ifelse(str_detect(key, 'Intercept'),  'Intercept',
               ifelse(str_detect(key, 'b'),  'b', 'sigma'))) %>%
  ggplot(aes(x=Type,  y=value)) +
  stat_pointinterval()+
  facet_wrap(~Class,  scales='free')


## ----fitModel2l, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
standata(loyn.brm3)
stancode(loyn.brm3)


## ----modelValidation1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
available_mcmc()


## ----modelValidation1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
plot(loyn.rstanarm3, plotfun='mcmc_trace')


## ----modelValidation1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
plot(loyn.rstanarm3, 'acf_bar')


## ----modelValidation1d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(loyn.rstanarm3, 'rhat_hist')


## ----modelValidation1e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(loyn.rstanarm3, 'neff_hist')


## ----Validation1f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
plot(loyn.rstanarm3, 'combo')
plot(loyn.rstanarm3, 'violin')


## ----modelValidation1g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_trace(loyn.rstanarm3)


## ----modelValidation1h, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_ac(loyn.rstanarm3) 


## ----modelValidation1i, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_rhat(loyn.rstanarm3) 


## ----modelValidation1j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_ess(loyn.rstanarm3)


## ----modelValidation1k, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_dens(loyn.rstanarm3, separate_chains = TRUE)


## ----modelValidation1l, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
loyn.ggs <- ggs(loyn.rstanarm3)
ggs_traceplot(loyn.ggs)


## ----modelValidation1m, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_autocorrelation(loyn.ggs)


## ----modelValidation1n, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_Rhat(loyn.ggs)


## ----modelValidation1o, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_effective(loyn.ggs)


## ----modelValidation1p, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_crosscorrelation(loyn.ggs)


## ----modelValidation1q, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_grb(loyn.ggs)


## ----modelValidation2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
available_mcmc()


## ----modelValidation2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
mcmc_plot(loyn.brm3, type='trace')


## ----modelValidation2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
mcmc_plot(loyn.brm3, type='acf_bar')


## ----modelValidation2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
mcmc_plot(loyn.brm3, type='rhat_hist')


## ----modelValidation2e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
mcmc_plot(loyn.brm3, type='neff_hist')


## ----Validation2f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
mcmc_plot(loyn.brm3, type='combo')
mcmc_plot(loyn.brm3, type='violin')


## ----modelValidation2g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_trace(loyn.brm3$fit)


## ----modelValidation2h, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_ac(loyn.brm3$fit) 


## ----modelValidation2i, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_rhat(loyn.brm3$fit) 


## ----modelValidation2j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_ess(loyn.brm3$fit)


## ----modelValidation2k, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_dens(loyn.brm3$fit, separate_chains = TRUE)


## ----modelValidation2l, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
loyn.ggs <- ggs(loyn.brm3)
ggs_traceplot(loyn.ggs)


## ----modelValidation2m, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_autocorrelation(loyn.ggs)


## ----modelValidation2n, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_Rhat(loyn.ggs)


## ----modelValidation2o, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_effective(loyn.ggs)


## ----modelValidation2p, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_crosscorrelation(loyn.ggs)


## ----modelValidation2q, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_grb(loyn.ggs)


## ----modelValidation3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
available_ppc()


## ----modelValidation3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(loyn.rstanarm3,  plotfun='dens_overlay')


## ----modelValidation3c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(loyn.rstanarm3, plotfun='error_scatter_avg')


## ----modelValidation3d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(loyn.rstanarm3, x=loyn$AREA, plotfun='error_scatter_avg_vs_x')


## ----modelValidation3e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(loyn.rstanarm3, x=loyn$AREA, plotfun='intervals')


## ----modelValidation3f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(loyn.rstanarm3, x=loyn$AREA, plotfun='ribbon')


## ----modelValidation3g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
#library(shinystan)
#launch_shinystan(loyn.rstanarm3)


## ----modelValidation4a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
preds <- posterior_predict(loyn.rstanarm3,  nsamples=250,  summary=FALSE)
loyn.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = loyn$ABUND,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = FALSE)
plot(loyn.resids)


## ----modelValidation5a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
available_ppc()


## ----modelValidation5b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(loyn.brm3,  type='dens_overlay')


## ----modelValidation5c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(loyn.brm3, type='error_scatter_avg')


## ----modelValidation5d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(loyn.brm3, x='AREA', type='error_scatter_avg_vs_x')


## ----modelValidation5e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(loyn.brm3, x='AREA', type='intervals')


## ----modelValidation5f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(loyn.brm3, x='AREA', type='ribbon')


## ----modelValidation5g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
#library(shinystan)
#launch_shinystan(loyn.brm3)


## ----modelValidation6a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
preds <- posterior_predict(loyn.brm3,  nsamples=250,  summary=FALSE)
loyn.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = loyn$ABUND,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = FALSE)
plot(loyn.resids)


## ----partialPlot1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
loyn.rstanarm3 %>% ggpredict() %>% plot(add.data=TRUE, facet=TRUE) 


## ----partialPlot1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
#loyn.rstanarm3 %>% ggemmeans(~AREA,  type='fixed') %>% plot(add.data=TRUE) + scale_y_log10()


## ----partialPlot1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
loyn.rstanarm3 %>% fitted_draws(newdata=loyn) %>%
  median_hdci() %>%
  ggplot(aes(x=AREA, y=.value)) +
  geom_ribbon(aes(ymin=.lower, ymax=.upper), fill='blue', alpha=0.3) + 
  geom_line() +
  geom_point(data=loyn,  aes(y=ABUND,  x=AREA)) +
  scale_y_log10()


## ----partialPlot2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
loyn.brm3 %>%
  conditional_effects() %>%
  plot(ask=FALSE, points=TRUE) %>%
  sjPlot::plot_grid()
#loyn.brm3 %>% conditional_effects(spaghetti=TRUE,nsamples=200) 


## ----partialPlot2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
loyn.brm3 %>% ggpredict() %>% plot(add.data=TRUE) %>%
  sjPlot::plot_grid()


## ----partialPlot2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
loyn.brm3 %>% ggemmeans(~AREA) %>% plot(add.data=TRUE)


## ----partialPlot2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
loyn.brm3 %>% fitted_draws(newdata=loyn) %>%
  median_hdci() %>%
  ggplot(aes(x=AREA, y=.value)) +
  geom_ribbon(aes(ymin=.lower, ymax=.upper), fill='blue', alpha=0.3) + 
  geom_line() +
  geom_point(data=loyn,  aes(y=ABUND,  x=AREA)) +
  scale_y_log10() 


## ----summariseModel1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
summary(loyn.rstanarm3)


## ----summariseModel1a1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, echo=FALSE----
loyn.sum <- summary(loyn.rstanarm3)


## ----summariseModel1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
tidyMCMC(loyn.rstanarm3$stanfit, estimate.method='median',  conf.int=TRUE,
         conf.method='HPDinterval',  rhat=TRUE, ess=TRUE)

## ----summariseModel1b1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
loyn.tidy <- tidyMCMC(loyn.rstanarm3$stanfit, estimate.method='median',  conf.int=TRUE,  conf.method='HPDinterval',  rhat=TRUE, ess=TRUE)


## ----summariseModel1c1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
loyn.rstanarm3 %>% get_variables()


## ----summariseModel1c2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
loyn.draw <- loyn.rstanarm3 %>% gather_draws(`.Intercept.*|.*AREA.*|.*DIST.*|.*GRAZE.*|.*ALT.*|.*YR.*`,  regex=TRUE)
loyn.draw


## ----summariseModel1d1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
loyn.rstanarm3 %>% plot(plotfun='mcmc_intervals') 


## ----summariseModel1e1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
loyn.rstanarm3 %>% tidy_draws() 


## ----summariseModel1f1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
loyn.rstanarm3 %>% spread_draws(`.Intercept.*|.*DIST.*|.*AREA.*|.*GRAZE.*|.*ALT.*|.*YR.*`,  regex=TRUE) 


## ----summariseModel1g1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
loyn.rstanarm3 %>% posterior_samples() %>% as.tibble() 


## ----summariseModel1h1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
loyn.rstanarm3 %>% bayes_R2() %>% median_hdci 


## ----summariseModel2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
summary(loyn.brm3)


## ----summariseModel2a1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, echo=FALSE----
loyn.sum <- summary(loyn.brm3)


## ----summariseModel2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
tidyMCMC(loyn.brm3$fit, estimate.method='median',  conf.int=TRUE,  conf.method='HPDinterval',  rhat=TRUE, ess=TRUE)

## ----summariseModel2b1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
loyn.tidy <- tidyMCMC(loyn.brm3$fit, estimate.method='median',  conf.int=TRUE,  conf.method='HPDinterval',  rhat=TRUE, ess=TRUE)


## ----summariseModel2c1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
loyn.brm3 %>% get_variables()


## ----summariseModel2c2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
loyn.draw <- loyn.brm3 %>% gather_draws(`^b_.*`,  regex=TRUE)
loyn.draw


## ----summariseModel2d1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
loyn.brm3 %>% mcmc_plot(type='intervals') 


## ----summariseModel2e1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
loyn.brm3 %>% tidy_draws() 


## ----summariseModel2f1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
loyn.brm3 %>% spread_draws(`^b_.*`,  regex=TRUE) 


## ----summariseModel2g1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
loyn.rstanarm3 %>% posterior_samples() %>% as.tibble() 


## ----summariseModel2h1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
loyn.brm3 %>% bayes_R2(summary=FALSE) %>% median_hdci 


## ----furtherModel1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
loyn.rstanarm4a <- update(loyn.rstanarm3,  .~scale(log(DIST))*scale(log(LDIST)),
                          diagnostic_file = file.path(tempdir(), "dfa.csv"))
loyn.rstanarm4b <- update(loyn.rstanarm3,  .~scale(log(AREA)) * fGRAZE,
                          diagnostic_file = file.path(tempdir(), "dfb.csv"))
loyn.rstanarm4c <- update(loyn.rstanarm3,  .~scale(log(AREA)) * fGRAZE * scale(YR.ISOL),
                          diagnostic_file = file.path(tempdir(), "dfc.csv"))
loyn.rstanarm4d <- update(loyn.rstanarm3,  .~scale(ALT),
                          diagnostic_file = file.path(tempdir(), "dfd.csv"))
loyn.rstanarm4e <- update(loyn.rstanarm3,  .~1,
                          diagnostic_file = file.path(tempdir(), "dfe.csv"))
loo_compare(loo(loyn.rstanarm4a),
            loo(loyn.rstanarm4e)
            )
loo_compare(loo(loyn.rstanarm4b),
            loo(loyn.rstanarm4e)
            )
loo_compare(loo(loyn.rstanarm4c),
            loo(loyn.rstanarm4e)
            )
loo_compare(loo(loyn.rstanarm4d),
            loo(loyn.rstanarm4e)
            )


## ----furtherModel1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
bayes_factor(bridge_sampler(loyn.rstanarm4a),
             bridge_sampler(loyn.rstanarm4e))
bayes_factor(bridge_sampler(loyn.rstanarm4b),
             bridge_sampler(loyn.rstanarm4e))
bayes_factor(bridge_sampler(loyn.rstanarm4c),
             bridge_sampler(loyn.rstanarm4e))
bayes_factor(bridge_sampler(loyn.rstanarm4d),
             bridge_sampler(loyn.rstanarm4e))


## ----furtherModel2a, results='markdown', cache=TRUE, eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
loyn.brm4a <- update(loyn.brm3,  .~scale(log(DIST))*scale(log(LDIST)), save_pars=save_pars(all=TRUE), refresh=0)
loyn.brm4b <- update(loyn.brm3,  .~scale(log(AREA)) * fGRAZE, save_pars=save_pars(all=TRUE), refresh=0)
loyn.brm4c <- update(loyn.brm3,  .~scale(log(AREA)) * fGRAZE * scale(YR.ISOL), save_pars=save_pars(all=TRUE), refresh=0)
loyn.brm4d <- update(loyn.brm3,  .~scale(ALT), save_pars=save_pars(all=TRUE), refresh=0)
loyn.brm4e <- update(loyn.brm3,  .~1, save_pars=save_pars(all=TRUE), refresh=0)
loo_compare(loo(loyn.brm4a),
            loo(loyn.brm4e)
            )
loo_compare(loo(loyn.brm4b),
            loo(loyn.brm4e)
            )
loo_compare(loo(loyn.brm4c),
            loo(loyn.brm4e)
            )
loo_compare(loo(loyn.brm4d),
            loo(loyn.brm4e)
            )


## ----furtherModel2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
bayes_factor(loyn.brm4a,
            loyn.brm4e)
bayes_factor(loyn.brm4b,
             loyn.brm4e)
bayes_factor(loyn.brm4c,
             loyn.brm4e)
bayes_factor(loyn.brm4d,
             loyn.brm4e)


## ----summaryFigure1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, echo=TRUE----
loyn.list = with(loyn, list(AREA = seq(min(AREA), max(AREA), len=100)))

newdata = emmeans(loyn.rstanarm3, ~AREA|fGRAZE, at = loyn.list, type='response') %>%
    as.data.frame 
head(newdata)

ggplot(newdata, aes(y=response, x=AREA)) +
  geom_ribbon(aes(ymin=lower.HPD, ymax=upper.HPD, fill=fGRAZE), alpha=0.3) +
  geom_line(aes(color=fGRAZE)) +
  theme_bw() +
  scale_x_log10() + 
  scale_y_log10()

spaghetti = emmeans(loyn.rstanarm3, ~AREA|fGRAZE, at = loyn.list, type='response') %>%
  gather_emmeans_draws() %>%
  mutate(Fit=exp(.value))
wch = sample(1:max(spaghetti$.draw), 100,replace=FALSE)
spaghetti = spaghetti %>%
  filter(.draw %in% wch)
ggplot(newdata) +
  geom_line(data=spaghetti, aes(y=Fit, x=AREA, color=fGRAZE,
                                group=interaction(fGRAZE,.draw)), alpha=0.05) +
  geom_line(aes(y=response, x=AREA, color=fGRAZE)) +
  theme_bw() +
  scale_x_log10() + scale_y_log10()


## ----summaryFigure1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, echo=TRUE----
loyn.list = with(loyn, list(AREA = seq(min(AREA), max(AREA), len=100)))

newdata = emmeans(loyn.rstanarm4b, ~AREA|fGRAZE, at = loyn.list, type='response') %>%
    as.data.frame 
head(newdata)

ggplot(newdata, aes(y=response, x=AREA)) +
  geom_ribbon(aes(ymin=lower.HPD, ymax=upper.HPD, fill=fGRAZE), alpha=0.3) +
  geom_line(aes(color=fGRAZE)) +
  theme_bw() +
  scale_x_log10() + 
  scale_y_log10()

spaghetti = emmeans(loyn.rstanarm4b, ~AREA|fGRAZE, at = loyn.list, type='response') %>%
  gather_emmeans_draws() %>% mutate(Fit=exp(.value))
wch = sample(1:max(spaghetti$.draw), 100,replace=FALSE)
spaghetti = spaghetti %>% filter(.draw %in% wch)
ggplot(newdata) +
  geom_line(data=spaghetti, aes(y=Fit, x=AREA, color=fGRAZE,
                                group=interaction(fGRAZE,.draw)), alpha=0.05) +
  geom_line(aes(y=response, x=AREA, color=fGRAZE)) +
  theme_bw() +
  scale_x_log10() + scale_y_log10()


## ----summaryFigure2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, echo=TRUE----
loyn.list = with(loyn, list(AREA = seq(min(AREA), max(AREA), len=100)))

newdata = emmeans(loyn.brm3, ~AREA|fGRAZE, at = loyn.list, type='response') %>%
    as.data.frame 
head(newdata)

ggplot(newdata, aes(y=response, x=AREA)) +
  geom_ribbon(aes(ymin=lower.HPD, ymax=upper.HPD, fill=fGRAZE), alpha=0.3) +
  geom_line(aes(color=fGRAZE)) +
  theme_bw() +
  scale_x_log10() + 
  scale_y_log10()

spaghetti = emmeans(loyn.brm3, ~AREA|fGRAZE, at = loyn.list, type='response') %>%
  gather_emmeans_draws() %>% mutate(Fit=exp(.value))
wch = sample(1:max(spaghetti$.draw), 100,replace=FALSE)
spaghetti = spaghetti %>% filter(.draw %in% wch)
ggplot(newdata) +
  geom_line(data=spaghetti, aes(y=Fit, x=AREA, color=fGRAZE,
                                group=interaction(fGRAZE,.draw)), alpha=0.05) +
  geom_line(aes(y=response, x=AREA, color=fGRAZE)) +
  theme_bw() +
  scale_x_log10() + scale_y_log10()


## ----summaryFigure2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, echo=TRUE----
loyn.list = with(loyn, list(AREA = seq(min(AREA), max(AREA), len=100)))

newdata = emmeans(loyn.brm4b, ~AREA|fGRAZE, at = loyn.list, type='response') %>%
    as.data.frame 
head(newdata)

ggplot(newdata, aes(y=response, x=AREA)) +
  geom_ribbon(aes(ymin=lower.HPD, ymax=upper.HPD, fill=fGRAZE), alpha=0.3) +
  geom_line(aes(color=fGRAZE)) +
  theme_bw() +
  scale_x_log10() + 
  scale_y_log10()

spaghetti = emmeans(loyn.brm4b, ~AREA|fGRAZE, at = loyn.list, type='response') %>%
  gather_emmeans_draws() %>% mutate(Fit=exp(.value))
wch = sample(1:max(spaghetti$.draw), 100,replace=FALSE)
spaghetti = spaghetti %>% filter(.draw %in% wch)
ggplot(newdata) +
  geom_line(data=spaghetti, aes(y=Fit, x=AREA, color=fGRAZE,
                                group=interaction(fGRAZE,.draw)), alpha=0.05) +
  geom_line(aes(y=response, x=AREA, color=fGRAZE)) +
  theme_bw() +
  scale_x_log10() + scale_y_log10()

