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
polis = read_csv('../data/polis.csv', trim_ws=TRUE)
glimpse(polis)
head(polis)
str(polis)


## ----lm, results='markdown', eval=TRUE, hidden=TRUE---------------------------
summary(glm(PA ~ RATIO, data=polis, family=binomial()))


## ----fitModel1a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
polis.rstanarm = stan_glm(PA ~ RATIO, data=polis,
                          family=binomial(), 
                         iter = 5000, warmup = 1000,
                         chains = 3, thin = 5, refresh = 0)


## ----fitModel1b, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
prior_summary(polis.rstanarm)


## ----fitModel1c, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
mean(polis$PA)
sd(polis$PA)
2.5*sd(polis$PA)


## ----fitModel1d, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
2.5/sd(polis$RATIO)


## ----fitModel1f, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
polis.rstanarm1 <- update(polis.rstanarm,  prior_PD=TRUE)

## ----fitModel1g, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
ggemmeans(polis.rstanarm1,  ~RATIO) %>% plot(add.data=TRUE)


## ----fitModel1h, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
polis.rstanarm2= stan_glm(PA ~ RATIO, data=polis,
                          family=binomial(), 
                          prior_intercept = normal(0.5, 2, autoscale=FALSE),
                          prior = normal(0, 0.2, autoscale=FALSE),
                          prior_PD=TRUE, 
                          iter = 5000, warmup = 1000,
                          chains = 3, thin = 5, refresh = 0
                          )


## ----fitModel1i, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
ggemmeans(polis.rstanarm2,  ~RATIO) %>%
  plot(add.data=TRUE)


## ----fitModel1j, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
polis.rstanarm3= update(polis.rstanarm2,  prior_PD=FALSE)  


## ----modelFit1k, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
posterior_vs_prior(polis.rstanarm3, color_by='vs', group_by=TRUE,
                   facet_args=list(scales='free_y'))


## ----modelFit1l, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggemmeans(polis.rstanarm3,  ~RATIO) %>% plot(add.data=TRUE)


## ----fitModel2a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
polis.brm = brm(bf(PA|trials(1) ~ RATIO, family=binomial()),
                data=polis,
                iter = 5000, warmup = 1000,
                chains = 3, thin = 5, refresh = 0)


## ----fitModel2b, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE, paged.print=FALSE,tidy.opts = list(width.cutoff = 80), echo=2----
options(width=100)
prior_summary(polis.brm)
options(width=80)


## ----fitModel2d, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
polis.brm1 = brm(bf(PA|trials(1) ~ RATIO, family=binomial()),
                 data=polis,
                prior=c(
                  prior(normal(0, 10), class='Intercept'), 
                  prior(normal(0, 1), class='b')), 
                sample_prior = 'only', 
                iter = 5000, warmup = 1000,
                chains = 3, thin = 5, refresh = 0)


## ----fitModel2e, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
ggemmeans(polis.brm1,  ~RATIO) %>% plot(add.data=TRUE)
conditional_effects(polis.brm1) %>%  plot(points=TRUE)


## ----fitModel2h, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
polis.brm2 = brm(bf(PA|trials(1) ~ RATIO, family=binomial()),
                 data=polis,
                 prior=c(prior(normal(0, 10),  class='Intercept'),
                            prior(normal(0, 1), class='b')
                            ), 
                 sample_prior = 'only', 
                 iter = 5000, warmup = 1000,
                 chains = 3, thin = 5, refresh = 0)


## ----fitModel2i, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
ggemmeans(polis.brm2,  ~RATIO) %>%
  plot(add.data=TRUE)


## ----fitModel2j, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
polis.brm3 <- update(polis.brm2,  sample_prior=TRUE, refresh=0)


## ----fitModel2k, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
polis.brm3 %>% get_variables()
polis.brm3 %>%
  posterior_samples %>%
  select(-`lp__`) %>%
  gather %>%
  mutate(Type=ifelse(str_detect(key, 'prior'), 'Prior', 'b'),
         Class=ifelse(str_detect(key, 'Intercept'),  'Intercept',
               ifelse(str_detect(key, 'sigma'),  'Sigma',  'b'))) %>%
  ggplot(aes(x=Type,  y=value)) +
  stat_pointinterval()+
  facet_wrap(~Class,  scales='free')


## ----fitModel2l, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
standata(polis.brm3)
stancode(polis.brm3)


## ----modelValidation1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
available_mcmc()


## ----modelValidation1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(polis.rstanarm3, plotfun='mcmc_trace')


## ----modelValidation1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(polis.rstanarm3, 'acf_bar')


## ----modelValidation1d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(polis.rstanarm3, 'rhat_hist')


## ----modelValidation1e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(polis.rstanarm3, 'neff_hist')


## ----Validation1f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(polis.rstanarm3, 'combo')
plot(polis.rstanarm3, 'violin')


## ----modelValidation1g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_trace(polis.rstanarm3)


## ----modelValidation1h, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_ac(polis.rstanarm3) 


## ----modelValidation1i, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_rhat(polis.rstanarm3) 


## ----modelValidation1j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_ess(polis.rstanarm3)


## ----modelValidation1k, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_dens(polis.rstanarm3, separate_chains = TRUE)


## ----modelValidation1l, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
polis.ggs <- ggs(polis.rstanarm3)
ggs_traceplot(polis.ggs)


## ----modelValidation1m, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_autocorrelation(polis.ggs)


## ----modelValidation1n, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_Rhat(polis.ggs)


## ----modelValidation1o, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_effective(polis.ggs)


## ----modelValidation1p, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_crosscorrelation(polis.ggs)


## ----modelValidation1q, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_grb(polis.ggs)


## ----modelValidation2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
available_mcmc()


## ----modelValidation2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
mcmc_plot(polis.brm3, type='trace')


## ----modelValidation2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
mcmc_plot(polis.brm3, type='acf_bar')


## ----modelValidation2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
mcmc_plot(polis.brm3, type='rhat_hist')


## ----modelValidation2e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
mcmc_plot(polis.brm3, type='neff_hist')


## ----modelValidation2f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
mcmc_plot(polis.brm3, type='combo')
mcmc_plot(polis.brm3, type='violin')


## ----modelValidation2g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_trace(polis.brm3$fit)


## ----modelValidation2h, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_ac(polis.brm3$fit) 


## ----modelValidation2i, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_rhat(polis.brm3$fit) 


## ----modelValidation2j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_ess(polis.brm3$fit)


## ----modelValidation2k, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_dens(polis.brm3$fit, separate_chains = TRUE)


## ----modelValidation2l, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=7----
polis.ggs <- ggs(polis.brm3)
ggs_traceplot(polis.ggs)


## ----modelValidation2m, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=7----
ggs_autocorrelation(polis.ggs)


## ----modelValidation2n, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_Rhat(polis.ggs)


## ----modelValidation2o, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_effective(polis.ggs)


## ----modelValidation2p, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_crosscorrelation(polis.ggs)


## ----modelValidation2q, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_grb(polis.ggs)


## ----modelValidation3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
available_ppc()


## ----modelValidation3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(polis.rstanarm3,  plotfun='dens_overlay')


## ----modelValidation3c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(polis.rstanarm3, plotfun='error_scatter_avg')


## ----modelValidation3d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(polis.rstanarm3, x=polis$RATIO, plotfun='error_scatter_avg_vs_x')


## ----modelValidation3e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(polis.rstanarm3, x=polis$RATIO, plotfun='intervals')


## ----modelValidation3f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(polis.rstanarm3, x=polis$RATIO, plotfun='ribbon')


## ----modelValidation3g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
#library(shinystan)
#launch_shinystan(polis.rstanarm3)


## ----modelValidation4a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
preds <- posterior_predict(polis.rstanarm3,  nsamples=250,  summary=FALSE)
polis.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = polis$PA,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = TRUE)
plot(polis.resids)


## ----modelValidation5a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
available_ppc()


## ----modelValidation5b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(polis.brm3,  type='dens_overlay')


## ----modelValidation5c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(polis.brm3, type='error_scatter_avg')


## ----modelValidation5d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(polis.brm3, x='RATIO', type='error_scatter_avg_vs_x')


## ----modelValidation5e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(polis.brm3, x='RATIO', type='intervals')


## ----modelValidation5f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(polis.brm3, x='RATIO', type='ribbon')


## ----modelValidation5g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
#library(shinystan)
#launch_shinystan(polis.brm3)


## ----modelValidation6a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
preds <- posterior_predict(polis.brm3,  nsamples=250,  summary=FALSE)
polis.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = polis$PA,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = TRUE)
plot(polis.resids)


## ----partialPlot1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
polis.rstanarm3 %>% ggpredict() %>% plot(add.data=TRUE)


## ----partialPlot1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
polis.rstanarm3 %>% ggemmeans(~RATIO) %>% plot(add.data=TRUE)


## ----partialPlot1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
polis.rstanarm3 %>% fitted_draws(newdata=polis) %>%
 median_hdci() %>%
 ggplot(aes(x=RATIO, y=.value)) +
 geom_ribbon(aes(ymin=.lower, ymax=.upper), fill='blue', alpha=0.3) + 
 geom_line()


## ----partialPlot2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
polis.brm3 %>% conditional_effects() 
polis.brm3 %>% conditional_effects(spaghetti=TRUE,nsamples=200) 


## ----partialPlot2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
polis.brm3 %>% ggpredict() %>% plot(add.data=TRUE)


## ----partialPlot2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
polis.brm3 %>% ggemmeans(~RATIO) %>% plot(add.data=TRUE)


## ----partialPlot2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
polis.brm3 %>% fitted_draws(newdata=polis) %>%
 median_hdci() %>%
 ggplot(aes(x=RATIO, y=.value)) +
 geom_ribbon(aes(ymin=.lower, ymax=.upper), fill='blue', alpha=0.3) + 
 geom_line()


## ----summariseModel1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
summary(polis.rstanarm3)


## ----summariseModel1a1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, echo=FALSE----
polis.sum <- summary(polis.rstanarm3)


## ----summariseModel1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
tidyMCMC(polis.rstanarm3$stanfit, estimate.method='median',  conf.int=TRUE,
         conf.method='HPDinterval',  rhat=TRUE, ess=TRUE)


## ----summariseModel1b1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
polis.tidy <- tidyMCMC(polis.rstanarm3$stanfit, estimate.method='median',  conf.int=TRUE,  conf.method='HPDinterval',  rhat=TRUE, ess=TRUE)


## ----summariseModel1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
polis.draw <- polis.rstanarm3 %>% gather_draws(`(Intercept)`, RATIO)
## OR via regex
polis.draw <- polis.rstanarm3 %>% gather_draws(`.Intercept.*|RATIO.*`,  regex=TRUE)
polis.draw


## ----summariseModel1c1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
polis.draw %>% median_hdci


## ----summariseModel1c3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
polis.gather <- polis.rstanarm3 %>% gather_draws(`(Intercept)`,RATIO) %>%
  median_hdci


## ----summariseModel1c4, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
polis.rstanarm3 %>% 
  gather_draws(`(Intercept)`, RATIO) %>% 
  ggplot() + 
  stat_halfeye(aes(x=.value,  y=.variable)) +
  facet_wrap(~.variable, scales='free')


## ----summariseModel1c5, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
polis.rstanarm3 %>% 
  gather_draws(`(Intercept)`, RATIO) %>%
  group_by(.variable) %>%
  mutate(.value=exp(.value)) %>%
  median_hdci


## ----summariseModel1j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
polis.rstanarm3 %>% plot(plotfun='mcmc_intervals') 


## ----summariseModel1d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
polis.rstanarm3 %>% tidy_draws()


## ----summariseModel1e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
polis.rstanarm3 %>% spread_draws(`(Intercept)`, RATIO)
# OR via regex
polis.rstanarm3 %>% spread_draws(`.Intercept.*|RATIO.*`,  regex=TRUE)


## ----summariseModel1f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
polis.rstanarm3 %>% posterior_samples() %>% as_tibble()


## ----summariseModel1g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
polis.rstanarm3 %>% bayes_R2() %>% median_hdci


## ----summariseModel2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
summary(polis.brm3)


## ----summariseModel2a1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, echo=FALSE----
polis.sum <- summary(polis.brm3)


## ----summariseModel2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
tidyMCMC(polis.brm3$fit, estimate.method='median',  conf.int=TRUE,  conf.method='HPDinterval',  rhat=TRUE, ess=TRUE)

## ----summariseModel2b1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
polis.tidy <- tidyMCMC(polis.brm3$fit, estimate.method='median',  conf.int=TRUE,  conf.method='HPDinterval',  rhat=TRUE, ess=TRUE)


## ----summariseModel2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
polis.draw <- polis.brm3 %>% gather_draws(b_Intercept, b_RATIO)
## OR via regex
polis.draw <- polis.brm3 %>% gather_draws(`b_.*`,  regex=TRUE)
polis.draw


## ----summariseModel2c1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
polis.draw %>% median_hdci


## ----summariseModel2c3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
polis.gather <- polis.brm3 %>% gather_draws(b_Intercept, b_RATIO) %>%
  median_hdci


## ----summariseModel2c4, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
polis.brm3 %>% 
  gather_draws(b_Intercept, b_RATIO) %>% 
  ggplot() + 
  stat_halfeye(aes(x=.value,  y=.variable)) +
  facet_wrap(~.variable, scales='free')


## ----summariseModel2c5, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
polis.brm3 %>% 
  gather_draws(b_Intercept, b_RATIO) %>%
  group_by(.variable) %>%
  mutate(.value=exp(.value)) %>%
  median_hdci


## ----summariseModel2j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
polis.brm3 %>% mcmc_plot(type='intervals') 


## ----summariseModel2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
polis.brm3 %>% tidy_draws()


## ----summariseModel2e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
polis.brm3 %>% spread_draws(b_Intercept, b_RATIO)
# OR via regex
polis.brm3 %>% spread_draws(`b_.*`,  regex=TRUE)


## ----summariseModel2f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
polis.brm3 %>% posterior_samples() %>% as_tibble()


## ----summariseModel2g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
polis.brm3 %>% bayes_R2(summary=FALSE) %>% median_hdci


## ----LD501a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
polis.rstanarm3 %>% tidy_draws() %>%
  mutate(LD50 = -1*`(Intercept)`/RATIO) %>%
  pull(LD50) %>%
  median_hdci


## ----LD502a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
polis.brm3 %>% tidy_draws() %>%
  mutate(LD50 = -1*b_Intercept/b_RATIO) %>%
  pull(LD50) %>%
  median_hdci


## ----figureModel1a, results='markdown', eval=TRUE, hidden=TRUE----------------
## Using emmeans
polis.grid = with(polis, list(RATIO = seq(min(RATIO), max(RATIO), len=100)))

newdata = emmeans(polis.rstanarm3, ~RATIO, at=polis.grid, type='response') %>% as.data.frame
head(newdata)

ggplot(newdata, aes(y=prob, x=RATIO)) + 
geom_point(data=polis, aes(y=PA)) +
geom_line() + 
geom_ribbon(aes(ymin=lower.HPD, ymax=upper.HPD), fill='blue', alpha=0.3) +
scale_y_continuous('PA') +
scale_x_continuous('RATIO') +
    theme_classic()


## ----figureModel2a, results='markdown', eval=TRUE, hidden=TRUE----------------
## Using emmeans
polis.grid = with(polis, list(RATIO = seq(min(RATIO), max(RATIO), len=100)))

newdata = emmeans(polis.brm3, ~RATIO, at=polis.grid, type='response') %>% as.data.frame
head(newdata)

ggplot(newdata, aes(y=prob, x=RATIO)) + 
geom_point(data=polis, aes(y=PA)) +
geom_line() + 
geom_ribbon(aes(ymin=lower.HPD, ymax=upper.HPD), fill='blue', alpha=0.3) +
scale_y_continuous('PA') +
scale_x_continuous('RATIO') +
    theme_classic()

