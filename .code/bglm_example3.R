## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE)


## ----libraries, results='markdown', eval=TRUE---------------------------------
library(rstanarm)   #for fitting models in STAN
library(brms)       #for fitting models in STAN
library(coda)       #for diagnostics
library(bayesplot)  #for diagnostics
library(ggmcmc)     #for MCMC diagnostics
library(DHARMa)     #for residual diagnostics
library(rstan)      #for interfacing with STAN
library(emmeans)    #for marginal means etc
library(broom)      #for tidying outputs
library(tidybayes)  #for more tidying outputs
library(ggeffects)  #for partial plots
library(tidyverse)  #for data wrangling etc
library(broom.mixed)#for summarising models
library(ggeffects)  #for partial effects plots
theme_set(theme_grey()) #put the default ggplot theme back


## ----readData, results='markdown', eval=TRUE----------------------------------
peake = read_csv('../data/peakquinn.csv', trim_ws=TRUE)
glimpse(peake)


## ----lm, results='markdown', eval=TRUE, hidden=TRUE---------------------------
summary(glm(INDIV ~ log(AREA), data=peake, family=poisson()))
summary(MASS::glm.nb(INDIV ~ log(AREA), data=peake))


## ----fitModel1a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
peake.rstanarm = stan_glm(INDIV ~ log(AREA), data=peake,
                          family=poisson(), 
                         iter = 5000, warmup = 1000,
                         chains = 3, thin = 5, refresh = 0)


## ----fitModel1b, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
prior_summary(peake.rstanarm)


## ----fitModel1d, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
2.5/sd(log(peake$AREA))


## ----fitModel1f, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
peake.rstanarm1 <- update(peake.rstanarm,  prior_PD=TRUE)

## ----fitModel1g, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
ggemmeans(peake.rstanarm1,  ~AREA) %>% plot(add.data=TRUE)


## ----fitModel1h, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
peake.rstanarm2= stan_glm(INDIV ~ log(AREA), data=peake,
                          family=poisson(), 
                          prior_intercept = normal(0, 5, autoscale=FALSE),
                          prior = normal(0, 2, autoscale=FALSE),
                          prior_PD=TRUE, 
                          iter = 5000, warmup = 1000,
                          chains = 3, thin = 5, refresh = 0
                          )


## ----fitModel1i, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
ggemmeans(peake.rstanarm2,  ~AREA) %>%
  plot(add.data=TRUE)


## ----fitModel1j, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
peake.rstanarm3= update(peake.rstanarm2,  prior_PD=FALSE)  


## ----modelFit1k, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
posterior_vs_prior(peake.rstanarm3, color_by='vs', group_by=TRUE,
                   facet_args=list(scales='free_y'))


## ----modelFit1l, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggemmeans(peake.rstanarm3,  ~AREA) %>% plot(add.data=TRUE)


## ----fitModel2a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
peake.brm = brm(bf(INDIV ~ log(AREA), family=poisson()),
                data=peake,
                iter = 5000, warmup = 1000,
                chains = 3, thin = 5, refresh = 0)


## ----fitModel2b, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE, paged.print=FALSE,tidy.opts = list(width.cutoff = 80), echo=2----
options(width=100)
prior_summary(peake.brm)
options(width=80)


## ----fitModel2c, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE, paged.print=FALSE,tidy.opts = list(width.cutoff = 80), echo=2----
median(log(peake$INDIV))
mad(log(peake$INDIV))


## ----fitModel2d, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
peake.brm1 = brm(bf(INDIV ~ log(AREA), family=poisson()),
                 data=peake,
                prior=c(
                  prior(normal(0, 2), class='b')), 
                sample_prior = 'only', 
                iter = 5000, warmup = 1000,
                chains = 3, thin = 5, refresh = 0)


## ----fitModel2e, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
ggemmeans(peake.brm1,  ~AREA) %>% plot(add.data=TRUE)
conditional_effects(peake.brm1) %>%  plot(points=TRUE)


## ----fitModel2h, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
peake.brm2 = brm(bf(INDIV ~ log(AREA), family=poisson()),
                 data=peake,
                 prior=c(
                   prior(normal(0, 5),  class='Intercept'),
                   prior(normal(0, 2), class='b')
                 ), 
                 sample_prior = 'only', 
                 iter = 5000, warmup = 1000,
                 chains = 3, thin = 5, refresh = 0)


## ----fitModel2i, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
ggemmeans(peake.brm2,  ~AREA) %>%
  plot(add.data=TRUE)


## ----fitModel2j, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
peake.brm3 <- update(peake.brm2,  sample_prior=TRUE, refresh=0)


## ----fitModel2k, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
peake.brm3 %>% get_variables()
peake.brm3 %>%
  posterior_samples %>%
  select(-`lp__`) %>%
  gather %>%
  mutate(Type=ifelse(str_detect(key, 'prior'), 'Prior', 'b'),
         Class=ifelse(str_detect(key, 'Intercept'),  'Intercept',
               ifelse(str_detect(key, 'b'),  'b', NA))) %>%
  ggplot(aes(x=Type,  y=value)) +
  stat_pointinterval()+
  facet_wrap(~Class,  scales='free')


## ----fitModel2l, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
standata(peake.brm3)
stancode(peake.brm3)


## ----modelValidation1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
available_mcmc()


## ----modelValidation1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(peake.rstanarm3, plotfun='mcmc_trace')


## ----modelValidation1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(peake.rstanarm3, 'acf_bar')


## ----modelValidation1d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(peake.rstanarm3, 'rhat_hist')


## ----modelValidation1e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(peake.rstanarm3, 'neff_hist')


## ----Validation1f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(peake.rstanarm3, 'combo')
plot(peake.rstanarm3, 'violin')


## ----modelValidation1g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_trace(peake.rstanarm3)


## ----modelValidation1h, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_ac(peake.rstanarm3) 


## ----modelValidation1i, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_rhat(peake.rstanarm3) 


## ----modelValidation1j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_ess(peake.rstanarm3)


## ----modelValidation1k, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_dens(peake.rstanarm3, separate_chains = TRUE)


## ----modelValidation1l, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
peake.ggs <- ggs(peake.rstanarm3)
ggs_traceplot(peake.ggs)


## ----modelValidation1m, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_autocorrelation(peake.ggs)


## ----modelValidation1n, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_Rhat(peake.ggs)


## ----modelValidation1o, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_effective(peake.ggs)


## ----modelValidation1p, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_crosscorrelation(peake.ggs)


## ----modelValidation1q, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_grb(peake.ggs)


## ----modelValidation2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
available_mcmc()


## ----modelValidation2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
mcmc_plot(peake.brm3, type='trace')


## ----modelValidation2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
mcmc_plot(peake.brm3, type='acf_bar')


## ----modelValidation2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
mcmc_plot(peake.brm3, type='rhat_hist')


## ----modelValidation2e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
mcmc_plot(peake.brm3, type='neff_hist')


## ----modelValidation2f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
mcmc_plot(peake.brm3, type='combo')
mcmc_plot(peake.brm3, type='violin')


## ----modelValidation2g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_trace(peake.brm3$fit)


## ----modelValidation2h, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_ac(peake.brm3$fit) 


## ----modelValidation2i, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_rhat(peake.brm3$fit) 


## ----modelValidation2j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_ess(peake.brm3$fit)


## ----modelValidation2k, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_dens(peake.brm3$fit, separate_chains = TRUE)


## ----modelValidation2l, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=7----
peake.ggs <- ggs(peake.brm3)
ggs_traceplot(peake.ggs)


## ----modelValidation2m, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=7----
ggs_autocorrelation(peake.ggs)


## ----modelValidation2n, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_Rhat(peake.ggs)


## ----modelValidation2o, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_effective(peake.ggs)


## ----modelValidation2p, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_crosscorrelation(peake.ggs)


## ----modelValidation2q, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_grb(peake.ggs)


## ----modelValidation3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
available_ppc()


## ----modelValidation3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(peake.rstanarm3,  plotfun='dens_overlay')


## ----modelValidation3c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(peake.rstanarm3, plotfun='error_scatter_avg')


## ----modelValidation3d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(peake.rstanarm3, x=peake$AREA, plotfun='error_scatter_avg_vs_x')


## ----modelValidation3e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(peake.rstanarm3, x=peake$AREA, plotfun='intervals')


## ----modelValidation3f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(peake.rstanarm3, x=peake$AREA, plotfun='ribbon')


## ----modelValidation3g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
#library(shinystan)
#launch_shinystan(peake.rstanarm3)


## ----modelValidation4a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
preds <- posterior_predict(peake.rstanarm3,  nsamples=250,  summary=FALSE)
peake.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = peake$INDIV,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = TRUE)
plot(peake.resids)


## ----modelValidation5a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
available_ppc()


## ----modelValidation5b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(peake.brm3,  type='dens_overlay')


## ----modelValidation5c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(peake.brm3, type='error_scatter_avg')


## ----modelValidation5d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(peake.brm3, x='AREA', type='error_scatter_avg_vs_x')


## ----modelValidation5e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(peake.brm3, x='AREA', type='intervals')


## ----modelValidation5f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(peake.brm3, x='AREA', type='ribbon')


## ----modelValidation5g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
#library(shinystan)
#launch_shinystan(peake.brm3)


## ----modelValidation6a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
preds <- posterior_predict(peake.brm3,  nsamples=250,  summary=FALSE)
peake.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = peake$INDIV,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = TRUE)
plot(peake.resids)


## ----fitNBModel1a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-----
peake.rstanarm4= stan_glm(INDIV ~ log(AREA), data=peake,
                          family=neg_binomial_2(), 
                          prior_intercept = normal(0, 5, autoscale=FALSE),
                          prior = normal(0, 2, autoscale=FALSE),
                          prior_aux = rstanarm::exponential(rate=1, autoscale=FALSE), 
                          iter = 5000, warmup = 1000,
                          chains = 3, thin = 5, refresh = 0
                          )


## ----fitNBModel1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
posterior_vs_prior(peake.rstanarm4, color_by='vs', group_by=TRUE,
                   facet_args=list(scales='free_y'))


## ----fitNBModel1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggemmeans(peake.rstanarm4,  ~AREA) %>% plot(add.data=TRUE)


## ----fitNBModel1d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(peake.rstanarm4, plotfun='mcmc_trace')
plot(peake.rstanarm4, 'acf_bar')
plot(peake.rstanarm4, 'rhat_hist')
plot(peake.rstanarm4, 'neff_hist')
pp_check(peake.rstanarm4, x=peake$AREA, plotfun='error_scatter_avg_vs_x')
pp_check(peake.rstanarm4, x=peake$AREA, plotfun='intervals')


## ----fitNBModel1e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
preds <- posterior_predict(peake.rstanarm4,  nsamples=250,  summary=FALSE)
peake.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = peake$INDIV,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = TRUE)
plot(peake.resids)


## ----fitNBModel1f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
(peake.rstanarm3.loo =loo(peake.rstanarm3))


## ----fitNBModel1g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
(peake.rstanarm4.loo =loo(peake.rstanarm4))


## ----fitNBModel1h, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
loo_compare(peake.rstanarm3.loo, peake.rstanarm4.loo)


## ----fitNBModel2a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-----
peake.brm4 = brm(bf(INDIV ~ log(AREA), family=negbinomial()),
                 data=peake,
                 prior=c(
                   prior(normal(0, 5),  class='Intercept'),
                   prior(normal(0, 2), class='b'),
                   prior(gamma(0.01, 0.01), class='shape')
                 ),
                 sample_prior=TRUE, 
                 iter = 5000, warmup = 1000,
                 chains = 3, thin = 5, refresh = 0)
                          


## ----fitNBModel2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
peake.brm4 %>%
  posterior_samples %>%
  select(-`lp__`) %>%
  gather %>%
  mutate(Type=ifelse(str_detect(key, 'prior'), 'Prior', 'b'),
         Class=ifelse(str_detect(key, 'Intercept'),  'Intercept',
               ifelse(str_detect(key, 'b'),  'b', 'shape'))) %>%
  ggplot(aes(x=Type,  y=value)) +
  stat_pointinterval()+
  facet_wrap(~Class,  scales='free')


## ----fitNBModel2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggemmeans(peake.brm4,  ~AREA) %>% plot(add.data=TRUE)


## ----fitNBModel2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(peake.brm4, type='mcmc_trace')
plot(peake.brm4, type='acf_bar')
plot(peake.brm4, type='rhat_hist')
plot(peake.brm4, type='neff_hist')
pp_check(peake.brm4, x='AREA',type='error_scatter_avg_vs_x')
pp_check(peake.brm4, x='AREA', type='intervals')


## ----fitNBModel2e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
preds <- posterior_predict(peake.brm4,  nsamples=250,  summary=FALSE)
peake.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = peake$INDIV,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = TRUE)
plot(peake.resids)


## ----fitNBModel2f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
(peake.brm3.loo =loo(peake.brm3))


## ----fitNBModel2g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
(peake.brm4.loo =loo(peake.brm4))


## ----fitNBModel2h, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
loo_compare(peake.brm3.loo, peake.brm4.loo)


## ----partialPlot1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.rstanarm4 %>% ggpredict() %>% plot(add.data=TRUE)


## ----partialPlot1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.rstanarm4 %>% ggemmeans(~AREA,  type='fixed', transform='response') %>% plot(add.data=TRUE)


## ----partialPlot1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.rstanarm4 %>% fitted_draws(newdata=peake) %>%
  median_hdci() %>%
  ggplot(aes(x=AREA, y=.value)) +
  geom_ribbon(aes(ymin=.lower, ymax=.upper), fill='blue', alpha=0.3) + 
  geom_line() +
  geom_point(data=peake,  aes(y=INDIV,  x=AREA))


## ----partialPlot2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.brm4 %>% conditional_effects() 
peake.brm4 %>% conditional_effects(spaghetti=TRUE,nsamples=200) 


## ----partialPlot2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.brm4 %>% ggpredict() %>% plot(add.data=TRUE)


## ----partialPlot2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.brm4 %>% ggemmeans(~AREA) %>% plot(add.data=TRUE)


## ----partialPlot2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.brm4 %>% fitted_draws(newdata=peake) %>%
  median_hdci() %>%
  ggplot(aes(x=AREA, y=.value)) +
  geom_ribbon(aes(ymin=.lower, ymax=.upper), fill='blue', alpha=0.3) + 
  geom_line() +
  geom_point(data=peake,  aes(y=INDIV,  x=AREA))


## ----summariseModel1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
summary(peake.rstanarm4)


## ----summariseModel1a1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, echo=FALSE----
peake.sum <- summary(peake.rstanarm4)


## ----summariseModel1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
tidyMCMC(peake.rstanarm4$stanfit, estimate.method='median',  conf.int=TRUE,  conf.method='HPDinterval',  rhat=TRUE, ess=TRUE)

## ----summariseModel1b1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
peake.tidy <- tidyMCMC(peake.rstanarm4$stanfit, estimate.method='median',  conf.int=TRUE,  conf.method='HPDinterval',  rhat=TRUE, ess=TRUE)


## ----summariseModel1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.rstanarm4 %>% get_variables()
peake.draw <- peake.rstanarm4 %>% gather_draws(`.Intercept.*|.*AREA.*`,  regex=TRUE)
peake.draw


## ----summariseModel1c1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.draw %>% median_hdci


## ----summariseModel1c3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
peake.gather <- peake.rstanarm4 %>% gather_draws(`.Intercept.*|.*AREA.*`,  regex=TRUE) %>%
  median_hdci


## ----summariseModel1c4, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
peake.rstanarm4 %>% 
  gather_draws(`.Intercept.*|.*AREA.*`, regex=TRUE) %>% 
  ggplot() + 
  stat_halfeye(aes(x=.value,  y=.variable)) +
  facet_wrap(~.variable, scales='free')


## ----summariseModel1c5, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
peake.rstanarm4 %>% 
  gather_draws(`.Intercept.*|.*AREA.*`, regex=TRUE) %>%
  group_by(.variable) %>%
  mutate(.value=exp(.value)) %>%
  median_hdci


## ----summariseModel1j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.rstanarm4 %>% plot(plotfun='mcmc_intervals') 


## ----summariseModel1d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.rstanarm4 %>% tidy_draws()


## ----summariseModel1e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.rstanarm4 %>% spread_draws(`.Intercept.*|.*AREA.*`,  regex=TRUE)


## ----summariseModel1f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.rstanarm4 %>% posterior_samples() %>% as_tibble()


## ----summariseModel1g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
#peake.rstanarm4 %>% bayes_R2() %>% median_hdci


## ----summariseModel2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
summary(peake.brm4)


## ----summariseModel2a1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, echo=FALSE----
peake.sum <- summary(peake.brm4)


## ----summariseModel2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
tidyMCMC(peake.brm4$fit, estimate.method='median',  conf.int=TRUE,  conf.method='HPDinterval',  rhat=TRUE, ess=TRUE)

## ----summariseModel2b1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
peake.tidy <- tidyMCMC(peake.brm4$fit, estimate.method='median',  conf.int=TRUE,  conf.method='HPDinterval',  rhat=TRUE, ess=TRUE)


## ----summariseModel2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.brm4 %>% get_variables()
peake.draw <- peake.brm4 %>% gather_draws(`b_.*`,  regex=TRUE)
peake.draw


## ----summariseModel2c1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.draw %>% median_hdci


## ----summariseModel2c3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
peake.gather <- peake.brm4 %>% gather_draws(`b_.*`,  regex=TRUE) %>%
  median_hdci


## ----summariseModel2c4, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
peake.brm4 %>% 
  gather_draws(`b_.*`, regex=TRUE) %>% 
  ggplot() + 
  stat_halfeye(aes(x=.value,  y=.variable)) +
  facet_wrap(~.variable, scales='free')


## ----summariseModel2c5, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
peake.brm4 %>% 
  gather_draws(`.Intercept.*|.*AREA.*`, regex=TRUE) %>%
  group_by(.variable) %>%
  mutate(.value=exp(.value)) %>%
  median_hdci


## ----summariseModel2j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.brm4 %>% plot(plotfun='mcmc_intervals') 


## ----summariseModel2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.brm4 %>% tidy_draws()


## ----summariseModel2e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.brm4 %>% spread_draws(`b_.*`,  regex=TRUE)


## ----summariseModel2f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.brm4 %>% posterior_samples() %>% as_tibble()


## ----summariseModel2g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.brm4 %>% bayes_R2(summary=FALSE) %>% median_hdci


## ----Probability1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.rstanarm4 %>% as.data.frame() %>% rename(lAREA=`log(AREA)`) %>% hypothesis('lAREA>0')


## ----Probability1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,paged.print=FALSE----
peake.rstanarm4 %>% tidy_draws() %>% summarise(P=sum(`log(AREA)`>0)/n())


## ----Probability1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, echo=1:2----
newdata = list(AREA=c(5000, 10000)) 
peake.rstanarm4 %>% emmeans(~AREA,  at=newdata) %>% pairs()
peake.mcmc <- peake.rstanarm4 %>% emmeans(~AREA,  at=newdata) %>% pairs() %>% as.data.frame()


## ----Probability1d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.mcmc <- peake.rstanarm4 %>% emmeans(~AREA,  at=newdata) %>% 
  tidy_draws() %>%
  rename_with(~str_replace(., 'AREA ', 'p')) %>%
  mutate(Eff=p10000 - p5000,
         PEff=100*Eff/p5000)
peake.mcmc %>% head


## ----Probability1e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.mcmc %>% tidyMCMC(estimate.method='median',
                       conf.int=TRUE, conf.method='HPDinterval')


## ----Probability1f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.mcmc %>% median_hdci(PEff)


## ----Probability1g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, paged.print=FALSE----
peake.mcmc %>% summarise(P=sum(PEff>50)/n())


## ----Probability1h, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.mcmc %>% hypothesis('PEff>50')


## ----Probability2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.brm4 %>% hypothesis('logAREA>0')


## ----Probability2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,paged.print=FALSE----
peake.brm4 %>% tidy_draws() %>% summarise(P=sum(b_logAREA>0)/n())


## ----Probability2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, echo=1:2----
newdata = list(AREA=c(5000, 10000)) 
peake.brm4 %>% emmeans(~AREA,  at=newdata) %>% pairs()
peake.mcmc <- peake.brm4 %>% emmeans(~AREA,  at=newdata) %>% pairs() %>% as.data.frame()


## ----Probability2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.mcmc <- peake.brm4 %>% emmeans(~AREA,  at=newdata) %>% 
  tidy_draws() %>%
  rename_with(~str_replace(., 'AREA ', 'p')) %>%
  mutate(Eff=p10000 - p5000,
         PEff=100*Eff/p5000)
peake.mcmc %>% head


## ----Probability2e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.mcmc %>% tidyMCMC(estimate.method='median',
                       conf.int=TRUE, conf.method='HPDinterval')


## ----Probability2f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.mcmc %>% median_hdci(PEff)


## ----Probability2g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, paged.print=FALSE----
peake.mcmc %>% summarise(P=sum(PEff>50)/n())


## ----Probability2h, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
peake.mcmc %>% hypothesis('PEff>50')


## ----figureModel1a, results='markdown', eval=TRUE, hidden=TRUE----------------
## Using emmeans
peake.grid = with(peake, list(AREA = seq(min(AREA), max(AREA), len=100)))

newdata = emmeans(peake.rstanarm4, ~AREA, at=peake.grid, type='response') %>% as.data.frame
head(newdata)

ggplot(newdata, aes(y=prob, x=AREA)) + 
geom_point(data=peake, aes(y=INDIV)) +
geom_line() + 
geom_ribbon(aes(ymin=lower.HPD, ymax=upper.HPD), fill='blue', alpha=0.3) +
scale_y_continuous('Individuals') +
scale_x_continuous('Mussel clump area') +
    theme_classic()


## ----figureModel2a, results='markdown', eval=TRUE, hidden=TRUE----------------
## Using emmeans
peake.grid = with(peake, list(AREA = seq(min(AREA), max(AREA), len=100)))

newdata = emmeans(peake.brm4, ~AREA, at=peake.grid, type='response') %>% as.data.frame
head(newdata)

ggplot(newdata, aes(y=prob, x=AREA)) + 
geom_point(data=peake, aes(y=INDIV)) +
geom_line() + 
geom_ribbon(aes(ymin=lower.HPD, ymax=upper.HPD), fill='blue', alpha=0.3) +
scale_y_continuous('Individuals') +
scale_x_continuous('Mussel clump area') +
    theme_classic()

