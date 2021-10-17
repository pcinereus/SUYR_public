## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE,cache.lazy = FALSE, tidy='styler')


## ----libraries, results='markdown', eval=TRUE---------------------------------
library(rstanarm)   #for fitting models in STAN
library(brms)       #for fitting models in STAN
library(coda)       #for diagnostics
library(ggmcmc)     #for MCMC diagnostics
library(bayesplot)  #for diagnostics
library(rstan)      #for interfacing with STAN
library(DHARMa)     #for residual diagnostics
library(emmeans)    #for marginal means etc
library(broom)      #for tidying outputs
library(broom.mixed)
library(tidybayes)  #for more tidying outputs
library(ggeffects)  #for partial plots
library(tidyverse)  #for data wrangling etc
library(patchwork)


## ----readData, results='markdown', eval=TRUE----------------------------------
day <- read_csv('../data/day.csv', trim_ws = TRUE)
day %>% glimpse()


## ----prepare, results='markdown', eval=TRUE, hidden=TRUE----------------------
day <- day %>% mutate(TREAT = factor(TREAT))


## ----EDA, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6, hidden=TRUE----
day %>% ggplot(aes(y = BARNACLE, x = TREAT)) +
    geom_boxplot()+
    geom_point(color = 'red')
day %>% ggplot(aes(y = BARNACLE, x = TREAT)) +
    geom_violin()+
    geom_point(color = 'red')


## ----fitModel1a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
day.rstanarm = stan_glm(BARNACLE ~ TREAT, data=day,
                        family=poisson(link='log'), 
                         iter = 5000, warmup = 1000,
                         chains = 3, thin = 5, refresh = 0)


## ----fitModel1b, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
prior_summary(day.rstanarm)


## ----fitModel1d, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
2.5/sd(model.matrix(~TREAT, day)[, 2])


## ----fitModel1f, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
day.rstanarm1 <- update(day.rstanarm,  prior_PD=TRUE)


## ----fitModel1g, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
ggpredict(day.rstanarm1) %>% plot(add.data=TRUE)


## ----fitModel1h, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
day.rstanarm2= stan_glm(BARNACLE ~ TREAT, data=day,
                        family=poisson(link='log'), 
                         prior_intercept = normal(3, 5, autoscale=FALSE),
                         prior = normal(0, 2, autoscale=FALSE),
                         prior_PD=TRUE, 
                         iter = 5000, warmup = 1000,
                         chains = 3, thin = 5, refresh = 0
                         )


## ----fitModel1i, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
ggpredict(day.rstanarm2) %>%
  plot(add.data=TRUE)


## ----fitModel1j, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
day.rstanarm3= update(day.rstanarm2,  prior_PD=FALSE) 


## ----modelFit1k, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
posterior_vs_prior(day.rstanarm3, color_by='vs', group_by=TRUE,
                   facet_args=list(scales='free_y'))


## ----modelFit1l, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggpredict(day.rstanarm3) %>% plot(add.data=TRUE)


## ----fitModel2a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
day.brm <- brm(bf(BARNACLE ~ TREAT,
                  family = poisson(link = 'log')), 
               data = day,
               iter = 5000,
               warmup = 1000,
               chains = 3,
               thin = 5,
               refresh = 0)


## ----fitModel2b, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE, paged.print=FALSE,tidy.opts = list(width.cutoff = 80), echo=2----
options(width=100)
day.brm %>% prior_summary()
options(width=80)


## ----fitModel2d, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
priors <- prior(normal(0, 1),  class = 'b')
day.brm1 <- brm(bf(BARNACLE ~ TREAT,
                  family = poisson(link = 'log')), 
               data = day,
               prior = priors, 
               sample_prior = 'only', 
               iter = 5000,
               warmup = 1000,
               chains = 3,
               thin = 5,
               refresh = 0)


## ----fitModel2e, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
day.brm1 %>% ggpredict() %>% plot(add.data=TRUE)
day.brm1 %>% ggpredict() %>% plot(add.data=TRUE) %>% `[[`(1) + scale_y_log10()

day.brm1 %>% ggemmeans(~TREAT) %>% plot(add.data=TRUE)
day.brm1 %>% ggemmeans(~TREAT) %>% plot(add.data=TRUE) + scale_y_log10()

day.brm1 %>% conditional_effects() %>%  plot(points=TRUE)
day.brm1 %>% conditional_effects() %>%  plot(points=TRUE) %>% `[[`(1) + scale_y_log10()


## ----fitModel2h, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
priors <- prior(normal(3, 5),  class = 'Intercept') +
    prior(normal(0, 5), class = 'b')
day.brm2 <- brm(bf(BARNACLE ~ TREAT,
                   family = poisson(link = 'log')), 
                data = day,
                prior = priors, 
                sample_prior = 'only', 
                iter = 5000,
                warmup = 2500,
                chains = 4,
                thin = 5,
                refresh = 0)


## ----fitModel2i, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
day.brm2 %>% ggpredict() %>% plot(add.data=TRUE)
day.brm2 %>% ggpredict() %>% plot(add.data=TRUE) %>% `[[`(1) + scale_y_log10()

day.brm2 %>% ggemmeans(~TREAT) %>% plot(add.data=TRUE)
day.brm2 %>% ggemmeans(~TREAT) %>% plot(add.data=TRUE) + scale_y_log10()

day.brm2 %>% conditional_effects() %>%  plot(points=TRUE)
day.brm2 %>% conditional_effects() %>%  plot(points=TRUE) %>% `[[`(1) + scale_y_log10()


## ----fitModel2j, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
day.brm3 <- day.brm2 %>% update(sample_prior = 'yes', refresh = 0)


## ----fitModel2k, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
day.brm3 %>% get_variables()
day.brm3 %>% hypothesis('TREATALG2<0') %>% plot()
day.brm3 %>% hypothesis('TREATNB<0') %>% plot()
day.brm3 %>% hypothesis('TREATS<0') %>% plot()
day.brm3 %>%
  posterior_samples %>%
  dplyr::select(-`lp__`) %>%
  pivot_longer(everything(), names_to = 'key') %>% 
  mutate(Type = ifelse(str_detect(key, 'prior'), 'Prior', 'b'),
         Class = ifelse(str_detect(key, 'Intercept'),  'Intercept',
               ifelse(str_detect(key, 'b'),  'b', 'sigma')),
         Par = str_replace(key, 'b_', '')) %>%
  ggplot(aes(x = Type,  y = value, color = Par)) +
  stat_pointinterval(position = position_dodge())+
  facet_wrap(~Class,  scales = 'free')


## ----fitModel2l, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
day.brm3 %>% standata()
day.brm3 %>% stancode()


## ----modelValidation1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
available_mcmc()


## ----modelValidation1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(day.rstanarm3, plotfun='mcmc_trace')


## ----modelValidation1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(day.rstanarm3, 'acf_bar')


## ----modelValidation1d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(day.rstanarm3, 'rhat_hist')


## ----modelValidation1e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(day.rstanarm3, 'neff_hist')


## ----Validation1f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(day.rstanarm3, 'combo')
plot(day.rstanarm3, 'violin')


## ----modelValidation1g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_trace(day.rstanarm3)


## ----modelValidation1h, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_ac(day.rstanarm3) 


## ----modelValidation1i, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_rhat(day.rstanarm3) 


## ----modelValidation1j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_ess(day.rstanarm3)


## ----modelValidation1k, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_dens(day.rstanarm3, separate_chains = TRUE)


## ----modelValidation1l, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
day.ggs <- ggs(day.rstanarm3)
ggs_traceplot(day.ggs)


## ----modelValidation1m, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_autocorrelation(day.ggs)


## ----modelValidation1n, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_Rhat(day.ggs)


## ----modelValidation1o, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_effective(day.ggs)


## ----modelValidation1p, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_crosscorrelation(day.ggs)


## ----modelValidation1q, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_grb(day.ggs)


## ----modelValidation2g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
day.brm3$fit %>% stan_trace()
day.brm3$fit %>% stan_trace(inc_warmup=TRUE)


## ----modelValidation2h, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
day.brm3$fit %>% stan_ac() 


## ----modelValidation2i, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
day.brm3$fit %>% stan_rhat() 


## ----modelValidation2j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
day.brm3$fit %>% stan_ess()


## ----modelValidation2k, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
day.brm3$fit %>% stan_dens(separate_chains = TRUE)


## ----modelValidation3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
available_ppc()


## ----modelValidation3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(day.rstanarm3,  plotfun='dens_overlay')


## ----modelValidation3c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(day.rstanarm3, plotfun='error_scatter_avg')


## ----modelValidation3d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(day.rstanarm3, x=as.numeric(day$TREAT), plotfun='error_scatter_avg_vs_x')


## ----modelValidation3e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(day.rstanarm3, x=as.numeric(day$TREAT), plotfun='intervals')


## ----modelValidation3g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
#library(shinystan)
#launch_shinystan(day.rstanarm3)


## ----modelValidation4a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
preds <- posterior_predict(day.rstanarm3,  nsamples=250,  summary=FALSE)
day.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = day$BARNACLE,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = TRUE)
plot(day.resids)


## ----modelValidation5a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
available_ppc()


## ----modelValidation5b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
day.brm3 %>% pp_check(type = 'dens_overlay')


## ----modelValidation5c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
day.brm3 %>% pp_check(type = 'error_scatter_avg')


## ----modelValidation5e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
day.brm3 %>% pp_check(group='TREAT', type='intervals')


## ----modelValidation5g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
#library(shinystan)
#launch_shinystan(day.brm3)


## ----modelValidation6a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
preds <- day.brm3 %>% posterior_predict(nsamples = 250,  summary = FALSE)
day.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = day$BARNACLE,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = TRUE)
day.resids %>% plot()


## ----partialPlot1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
day.rstanarm3 %>% ggpredict() %>% plot(add.data=TRUE)


## ----partialPlot1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
day.rstanarm3 %>% ggemmeans(~TREAT,  type='fixed') %>% plot(add.data=TRUE)


## ----partialPlot1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
day.rstanarm3 %>% fitted_draws(newdata=day) %>%
  median_hdci() %>%
  ggplot(aes(x=TREAT, y=.value)) +
  geom_pointrange(aes(ymin=.lower, ymax=.upper)) + 
  geom_line() +
  geom_point(data=day,  aes(y=BARNACLE,  x=TREAT))


## ----partialPlot2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
day.brm3 %>% conditional_effects() %>% plot(points = TRUE)


## ----partialPlot2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
day.brm3 %>% ggpredict() %>% plot(add.data = TRUE)


## ----partialPlot2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
day.brm3 %>% ggemmeans(~TREAT) %>% plot(add.data = TRUE)


## ----partialPlot2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
day.brm3 %>% fitted_draws(newdata = day) %>%
  median_hdci() %>%
  ggplot(aes(x = TREAT, y = .value)) +
  geom_pointrange(aes(ymin = .lower, ymax = .upper)) + 
  geom_line() +
  geom_point(data = day,  aes(y = BARNACLE,  x = TREAT))


## ----summariseModel1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
summary(day.rstanarm3)


## ----summariseModel1a1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, echo=FALSE----
day.sum <- summary(day.rstanarm3)


## ----summariseModel1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
tidyMCMC(day.rstanarm3$stanfit, estimate.method='median',  conf.int=TRUE,  conf.method='HPDinterval',  rhat=TRUE, ess=TRUE)

## ----summariseModel1b1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
day.tidy <- tidyMCMC(day.rstanarm3$stanfit, estimate.method='median',  conf.int=TRUE,  conf.method='HPDinterval',  rhat=TRUE, ess=TRUE)


## ----summariseModel1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
day.rstanarm3 %>% get_variables()
day.draw <- day.rstanarm3 %>% gather_draws(`.Intercept.*|.*TREAT.*`,  regex=TRUE)
day.draw


## ----summariseModel1c1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
day.draw %>% median_hdci


## ----summariseModel1c3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
day.gather <- day.rstanarm3 %>% gather_draws(`.Intercept.*|.*TREAT.*`,  regex=TRUE) %>%
  median_hdci


## ----summariseModel1c4, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
day.rstanarm3 %>% 
  gather_draws(`.Intercept.*|.*TREAT.*`, regex=TRUE) %>% 
  ggplot() + 
  stat_halfeye(aes(x=.value,  y=.variable)) +
  facet_wrap(~.variable, scales='free')


## ----summariseModel1c5, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
day.rstanarm3 %>% 
  gather_draws(`.Intercept.*|.*TREAT.*`, regex=TRUE) %>%
  group_by(.variable) %>%
  mutate(.value=exp(.value)) %>%
  median_hdci


## ----summariseModel1j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
day.rstanarm3 %>% plot(plotfun='mcmc_intervals') 


## ----summariseModel1d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
day.rstanarm3 %>% tidy_draws()


## ----summariseModel1e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
day.rstanarm3 %>% spread_draws(`.Intercept.*|.*TREAT.*`,  regex=TRUE)


## ----summariseModel1f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
day.rstanarm3 %>% posterior_samples() %>% as_tibble()


## ----summariseModel1g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
#day.rstanarm3 %>% bayes_R2() %>% median_hdci


## ----summariseModel2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
day.brm3 %>% summary()


## ----summariseModel2a1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, echo=FALSE----
day.sum <- summary(day.brm3)


## ----summariseModel2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
day.brm3$fit %>% tidyMCMC(estimate.method = 'median',  conf.int = TRUE,  conf.method = 'HPDinterval',  rhat = TRUE, ess = TRUE)

## ----summariseModel2b1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
day.tidy <- tidyMCMC(day.brm3$fit, estimate.method='median',  conf.int=TRUE,  conf.method='HPDinterval',  rhat=TRUE, ess=TRUE)


## ----summariseModel2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
day.brm3 %>% get_variables()
day.draw <- day.brm3 %>%
    gather_draws(`.Intercept.*|.*TREAT.*`,  regex = TRUE)
day.draw


## ----summariseModel2c1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
day.draw %>% median_hdci


## ----summariseModel2c3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
day.gather <- day.brm3 %>% gather_draws(`.Intercept.*|.*TREAT.*`,  regex = TRUE) %>%
  median_hdci


## ----summariseModel2c4, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
day.brm3 %>%
    gather_draws(`.Intercept.*|.*TREAT.*`, regex=TRUE) %>% 
    ggplot() +
    geom_vline(xintercept=0, linetype='dashed') +
    stat_slab(aes(x = .value, y = .variable,
                  fill = stat(ggdist::cut_cdf_qi(cdf,
                           .width = c(0.5, 0.8, 0.95), 
                           labels = scales::percent_format())
                           )), color='black') + 
    scale_fill_brewer('Interval', direction = -1, na.translate = FALSE) 

day.brm3 %>% 
  gather_draws(`.Intercept.*|.*TREAT.*`, regex=TRUE) %>% 
  ggplot() + 
  stat_halfeye(aes(x=.value,  y=.variable)) +
  facet_wrap(~.variable, scales='free')


## ----summariseModel2c5, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
day.brm3 %>% 
  gather_draws(`.Intercept.*|.*TREAT.*`, regex=TRUE) %>%
  group_by(.variable) %>%
  mutate(.value=exp(.value)) %>%
  median_hdci

day.brm3 %>%
    gather_draws(`.Intercept.*|.*TREAT.*`, regex=TRUE) %>%
    mutate(.value=exp(.value)) %>%
    ggplot() +
    geom_vline(xintercept=1, linetype='dashed') +
    stat_slab(aes(x = .value, y = .variable,
                  fill = stat(ggdist::cut_cdf_qi(cdf,
                           .width = c(0.5, 0.8, 0.95), 
                           labels = scales::percent_format())
                           )), color='black') + 
    scale_fill_brewer('Interval', direction = -1, na.translate = FALSE) 


## ----summariseModel2j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
day.brm3$fit %>% plot(type='intervals') 


## ----summariseModel2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
day.brm3 %>% tidy_draws()


## ----summariseModel2e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
day.brm3 %>% spread_draws(`.Intercept.*|.*TREAT.*`,  regex=TRUE)


## ----summariseModel2f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
day.brm3 %>% posterior_samples() %>% as_tibble()


## ----summariseModel2g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
day.brm3 %>% bayes_R2(summary=FALSE) %>% median_hdci


## ----Probability1a, results='markdown', echo=1,eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
day.rstanarm3 %>% emmeans(pairwise ~TREAT, type='response')
day.pairwise <- (day.rstanarm3 %>%
  emmeans(pairwise ~TREAT, type='response') %>%
  confint())$contrasts %>%
  as.data.frame


## ----Probability1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
day.em = emmeans(day.rstanarm3, pairwise~TREAT, type='link')$contrasts %>%
    gather_emmeans_draws() %>%
    mutate(Fit=exp(.value))
day.em %>% head
day.em %>% group_by(contrast) %>%
    ggplot(aes(x=Fit)) +
    geom_histogram() +
    geom_vline(xintercept=1, color='red') + 
    facet_wrap(~contrast, scales='free')
day.em %>% group_by(contrast) %>% median_hdi()
# Probability of effect
day.em %>% group_by(contrast) %>% summarize(P=sum(Fit>1)/n())
day.em %>% group_by(contrast) %>% summarize(P=sum(Fit<1)/n())
##Probability of effect greater than 10%
day.em %>% group_by(contrast) %>% summarize(P=sum(Fit>1.1)/n())


## ----Probability2a, results='markdown', echo=1,eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
day.brm3 %>%
    emmeans(~TREAT, type = 'response') %>%
    pairs()
#OR
day.pairwise <- day.brm3 %>%
    emmeans(~TREAT, type = 'response') %>%
    pairs() %>%
    as.data.frame()


## ----Probability2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
day.brm3 %>% emmeans(~TREAT, type = 'response') %>%
    pairs()
day.em <- day.brm3 %>%
    emmeans(~TREAT, type = 'link') %>%
    pairs() %>%
    gather_emmeans_draws() %>%
    mutate(Fit = exp(.value))
day.em %>% head
day.em %>%
    group_by(contrast) %>%
    ggplot(aes(x = Fit)) +
    ## geom_histogram() +
    geom_halfeyeh() +
    geom_vline(xintercept = 1, color = 'red') + 
    facet_wrap(~contrast, scales = 'free')
day.em %>%
    group_by(contrast) %>%
    median_hdi()
# Probability of effect
day.em %>% group_by(contrast) %>% summarize(P = sum(Fit>1)/n())
##Probability of effect greater than 10%
day.em %>% group_by(contrast) %>% summarize(P = sum(Fit>1.1)/n())

## Effect size on absolute scale
day.em <- day.brm3 %>%
    emmeans(~TREAT, type = 'link') %>%
    regrid() %>%
    pairs() %>%
    gather_emmeans_draws() %>%
    median_hdci(.value)

day.brm3 %>%
    emmeans(~TREAT, type = 'link') %>%
    regrid() %>%
    pairs() %>%
    gather_emmeans_draws() %>%
    group_by(contrast) %>%
    ggplot(aes(x = .value)) +
    geom_halfeyeh() +
    geom_vline(xintercept = 0, color = 'red') + 
    facet_wrap(~contrast, scales = 'free')


## ----Probability1c, results='markdown', echo=1,eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
##Planned contrasts
cmat<-cbind('Alg2_Alg1'=c(-1,1,0,0),
              'NB_S'=c(0,0,1,-1),
             'Alg_Bare'=c(0.5,0.5,-0.5,-0.5),
             'Alg_NB'=c(0.5,0.5,-1,0))
# On the link scale
emmeans(day.rstanarm3, ~TREAT, contr=list(TREAT=cmat), type='link')
# On the response scale
emmeans(day.rstanarm3, ~TREAT, contr=list(TREAT=cmat), type='response')

day.em = emmeans(day.rstanarm3, ~TREAT, contr=list(TREAT=cmat), type='link')$contrasts %>%
      gather_emmeans_draws() %>% mutate(Fit=exp(.value)) 
day.em %>% group_by(contrast) %>% mean_hdi()
# Probability of effect
day.em %>% group_by(contrast) %>% summarize(P=sum(Fit>1)/n())
##Probability of effect greater than 10%
day.em %>% group_by(contrast) %>% summarize(P=sum(Fit>1.5)/n())


day.sum <- day.em %>%
  group_by(contrast) %>%
  median_hdci(.width=c(0.8, 0.95))
day.sum
ggplot(day.sum) +
  geom_hline(yintercept=1, linetype='dashed') +
  geom_pointrange(aes(x=contrast, y=Fit, ymin=Fit.lower, ymax=Fit.upper, size=factor(.width)),
                  show.legend = FALSE) +
  scale_size_manual(values=c(1, 0.5)) +
  coord_flip()

g1 <- ggplot(day.sum) +
  geom_hline(yintercept=1) +
  geom_pointrange(aes(x=contrast, y=Fit, ymin=Fit.lower, ymax=Fit.upper, size=factor(.width)), show.legend = FALSE) +
  scale_size_manual(values=c(1, 0.5)) +
  scale_y_continuous(trans=scales::log2_trans(),  breaks=c(0.5, 1, 2, 4)) +
  coord_flip() + 
  theme_classic()
g1   


## ----Probability2c, results='markdown', echo=1,eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
##Planned contrasts
cmat<-cbind('Alg2_Alg1' = c(-1,1,0,0),
            'NB_S' = c(0,0,1,-1),
            'Alg_Bare' = c(0.5,0.5,-0.5,-0.5),
            'Alg_NB' = c(0.5,0.5,-1,0))
# On the link scale
day.brm3 %>% emmeans(~TREAT, type = 'link') %>%
    contrast(method = list(TREAT = cmat))
# On the response scale
day.brm3 %>%
    emmeans(~TREAT, type = 'response') %>%
    contrast(method = list(TREAT = cmat))

day.em <- day.brm3 %>%
    emmeans(~TREAT, type = 'link') %>%
    contrast(method = list(TREAT = cmat)) %>%
    gather_emmeans_draws() %>%
    mutate(Fit = exp(.value)) 
day.em %>% median_hdi(Fit)
# Probability of effect
day.em %>% summarize(P = sum(Fit>1)/n())
##Probability of effect greater than 50%
day.em %>% summarize(P = sum(Fit>1.5)/n())

day.sum <- day.em %>%
  group_by(contrast) %>%
  median_hdci(.value, .width = c(0.8, 0.95))
day.sum
g1 <- ggplot(day.sum) +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_pointrange(aes(y = contrast, x = .value, xmin = .lower, xmax = .upper,
                      size = factor(.width)),
                  show.legend  =  FALSE) +
  scale_size_manual(values = c(1, 0.5)) 

day.sum <- day.em %>%
  group_by(contrast) %>%
  median_hdci(Fit, .width = c(0.8, 0.95))
day.sum
g1 <- ggplot(day.sum) +
  geom_vline(xintercept = 1, linetype='dashed') +
  geom_pointrange(aes(y = contrast, x = Fit, xmin = .lower, xmax = .upper, size = factor(.width)), show.legend = FALSE) +
  scale_size_manual(values = c(1, 0.5)) +
  scale_x_continuous(trans = scales::log2_trans(),  breaks = c(0.5, 0.8,1,1.2, 1.5, 2, 4)) +
  theme_classic()
g1   

g1a <- 
    day.em %>%
    ggplot() +
    geom_vline(xintercept = 1, linetype = 'dashed') +
    geom_vline(xintercept = 1.5, alpha=0.3, linetype = 'dashed') +
    stat_slab(aes(x = Fit, y = contrast,
                  fill = stat(ggdist::cut_cdf_qi(cdf,
                            .width = c(0.5, 0.8, 0.95), 
                            labels = scales::percent_format())
                            )), color = 'black') +
    scale_fill_brewer('Interval', direction  =  -1, na.translate = FALSE) +
    scale_x_continuous(trans = scales::log2_trans(),  breaks = c(0.5, 0.8,1,1.2, 1.5, 2, 4)) +
    theme_classic()
g1 + g1a


## ----summaryFig1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=3----
newdata = emmeans(day.rstanarm3, ~TREAT, type='response') %>%
    as.data.frame
newdata
## A quick version
g2 <- ggplot(newdata, aes(y=rate, x=TREAT)) +
    geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD)) +
    theme_classic()
    
g2 + g1    


## ----summaryFig2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=3----
newdata <- day.brm3 %>%
    emmeans(~TREAT, type='response') %>%
    as.data.frame
newdata
## A quick version
g2 <- ggplot(newdata, aes(y=rate, x=TREAT)) +
    geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD)) +
    theme_classic()
    
g2 + g1    
g2 + g1a    


## ----fitModel, results='markdown', echo=FALSE,eval=FALSE, hidden=TRUE---------
## day.rstanarm <- stan_glm(BARNACLE ~ TREAT, data=day,
##                       family=poisson(link='log'),
##                       chains = 3,iter = 5000, warmup=2000, thin=5,
##                       refresh=0)
## prior_summary(day.rstanarm)
## 
## day.rstanarm <- stan_glm(BARNACLE ~ TREAT, data=day, family='poisson',
##                       prior = normal(c(0,0,0), c(2.5,2.5,2.5)),
##                       prior_intercept = normal(0,10),
##                       chains = 3,iter = 2000, thin=2, refresh=0)
## prior_summary(day.rstanarm)
## 
## plot(day.rstanarm,  'mcmc_trace')
## plot(day.rstanarm,  'mcmc_acf_bar')
## plot(day.rstanarm,  'mcmc_rhat_hist')
## plot(day.rstanarm,  'mcmc_neff_hist')
## 
## 
## preds <- posterior_predict(day.rstanarm,  nsamples=250,  summary=FALSE)
## day.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = day$BARNACLE,
##                            fittedPredictedResponse = apply(preds, 2, median),
##                            integerResponse = TRUE)
## plot(day.resids)
## 
## 
## #pp_check(day.rstanarm, x=as.numeric(day$TREAT),'intervals')
## 
## 
## 
## 
## ## Compare the proportion of zeros in the observed and expected data
## #yrep = posterior_predict(day.rstan)
## prop_zero <- function(y) mean(y == 0)
## (prop_zero_test1 <- pp_check(day.rstanarm, plotfun = "stat", stat = "prop_zero"))
##                                         # no zeros - so not zero inflated
## 
## 
## day.rstanarmNB <- stan_glm(BARNACLE ~ TREAT, data=day,
##                       family='neg_binomial_2',
##                       chains = 3,iter = 5000, thin=5, warmup=2000, refresh=0)
## preds <- posterior_predict(day.rstanarmNB,  nsamples=250,  summary=FALSE)
## day.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = day$BARNACLE,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(day.resids)
## 
## 
## day.rstanNB <- update(day.rstan, family = neg_binomial_2)
## (loo.P=loo(day.rstan))
## (loo.NB=loo(day.rstanNB))
## compare_models(loo.P, loo.NB)
## 
## ggpredict(day.rstanarm, term='TREAT') %>% plot
## ggpredict(day.rstanarm, ~TREAT) %>% plot
## ggemmeans(day.rstanarm, ~TREAT) %>% plot
## 
## 
## summary(day.rstanarm)
## library(tidybayes)
## tidyMCMC(day.rstanarm$stanfit, conf.int=TRUE,
##          conf.method='HPDinterval', rhat=TRUE,ess=TRUE)
## 
## 
## # Pairwise comparisons
## library(emmeans)
## ## factor statements
## emmeans(day.rstanarm, pairwise~TREAT, type='response')
## ## what about probabilities
## day.em = emmeans(day.rstanarm, pairwise~TREAT, type='link')$contrasts %>%
##     gather_emmeans_draws() %>%
##     mutate(Fit=exp(.value))
## day.em %>% head
## day.em %>% group_by(contrast) %>%
##     ggplot(aes(x=Fit)) +
##     geom_histogram() +
##     geom_vline(xintercept=1, color='red') +
##     facet_wrap(~contrast, scales='free')
## day.em %>% group_by(contrast) %>% median_hdi()
## # Probability of effect
## day.em %>% group_by(contrast) %>% summarize(P=sum(Fit>1)/n())
## ##Probability of effect greater than 10%
## day.em %>% group_by(contrast) %>% summarize(P=sum(Fit>1.1)/n())
## 
## 
## ##Planned contrasts
## cmat<-cbind('Alg2_Alg1'=c(-1,1,0,0),
##               'NB_S'=c(0,0,1,-1),
##              'Alg_Bare'=c(0.5,0.5,-0.5,-0.5),
##              'Alg_NB'=c(0.5,0.5,-1,0))
## #crossprod(cmat)
## emmeans(day.rstanarm, ~TREAT, contr=list(TREAT=cmat), type='link')
## emmeans(day.rstanarm, ~TREAT, contr=list(TREAT=cmat), type='response')
## day.em = emmeans(day.rstanarm, ~TREAT, contr=list(TREAT=cmat), type='link')$contrasts %>%
##       gather_emmeans_draws() %>% mutate(Fit=exp(.value))
## day.em %>% group_by(contrast) %>% mean_hdi()
## # Probability of effect
## day.em %>% group_by(contrast) %>% summarize(P=sum(Fit>1)/n())
## ##Probability of effect greater than 10%
## day.em %>% group_by(contrast) %>% summarize(P=sum(Fit>1.5)/n())
## 
## hist(bayes_R2(day.rstanarmNB))
## 
## bayes_R2(day.rstanarm) %>% median_hdi
## bayes_R2(day.rstanarmNB) %>% hist
## 
## 
## ## Summary plot
## day.grid = with(day, list(TREAT=levels(TREAT)))
## newdata = emmeans(day.rstanarm, ~TREAT, type='response') %>% as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=rate, x=TREAT)) +
##     geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD))


## ----fitModel.brms, results='markdown', eval=FALSE, hidden=TRUE---------------
## day.form <- bf(BARNACLE ~ TREAT,  family=poisson(link='log'))
## get_prior(day.form,  data=day)
## day.priors <- c(
##   prior(normal(0, 10),  class='Intercept'),
##   prior(normal(0, 2.5), class='b')
## )
## day.brms <- brm(day.form, data=day,
##                 prior=day.priors,
##                  chains=3,  iter=5000,  warmup=2000, thin=5,
##                  refresh=0)
## 
## plot(day.brms)
## mcmc_plot(day.brms,  type='acf_bar')
## mcmc_plot(day.brms,  type='rhat_hist')
## mcmc_plot(day.brms,  type='neff_hist')
## 
## preds <- posterior_predict(day.brms,  nsamples=250,  summary=FALSE)
## day.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = day$BARNACLE,
##                            fittedPredictedResponse = apply(preds, 2, median),
##                            integerResponse = TRUE)
## plot(day.resids)
## 
##                                         #pp_check(day.brmsP, x=as.numeric(day$TREAT),'intervals')
## 
## 
## ggpredict(day.brms, term='TREAT') %>% plot
## ggpredict(day.brms, ~TREAT) %>% plot
## ggemmeans(day.brms, ~TREAT) %>% plot
## 
## summary(day.brms)
## 
## tidyMCMC(day.brms$fit, conf.int=TRUE,
##          conf.method='HPDinterval', rhat=TRUE,ess=TRUE)
## 
## # Pairwise comparisons
## library(emmeans)
## ## factor statements
## emmeans(day.brms, pairwise~TREAT, type='response')
## ## what about probabilities
## day.em = emmeans(day.brms, pairwise~TREAT, type='link')$contrasts %>%
##     gather_emmeans_draws() %>%
##     mutate(Fit=exp(.value))
## day.em %>% head
## day.em %>% group_by(contrast) %>%
##     ggplot(aes(x=Fit)) +
##     geom_histogram() +
##     geom_vline(xintercept=1, color='red') +
##     facet_wrap(~contrast, scales='free')
## day.em %>% group_by(contrast) %>% median_hdi(.width=c(0.8, 0.95))
## 
## day.sum <- day.em %>%
##   group_by(contrast) %>%
##   median_hdci(.width=c(0.8, 0.95))
## day.sum
## ggplot(day.sum) +
##   geom_hline(yintercept=1, linetype='dashed') +
##   geom_pointrange(aes(x=contrast, y=Fit, ymin=Fit.lower, ymax=Fit.upper, size=factor(.width)),
##                   show.legend = FALSE) +
##   scale_size_manual(values=c(1, 0.5)) +
##   coord_flip()
## 
## g1 <- ggplot(day.sum) +
##   geom_hline(yintercept=1) +
##   geom_pointrange(aes(x=contrast, y=Fit, ymin=Fit.lower, ymax=Fit.upper, size=factor(.width)), show.legend = FALSE) +
##   scale_size_manual(values=c(1, 0.5)) +
##   scale_y_continuous(trans=scales::log2_trans(),  breaks=c(0.5, 1, 2, 4)) +
##   coord_flip()
## g1
##                                         # Probability of effect
## day.em %>% group_by(contrast) %>% summarize(P=sum(.value>0)/n())
## day.em %>% group_by(contrast) %>% summarize(P=sum(Fit>1)/n())
## ##Probability of effect greater than 10%
## day.em %>% group_by(contrast) %>% summarize(P=sum(Fit>1.1)/n())
## 
## ##Planned contrasts
## cmat<-cbind('Alg2_Alg1'=c(-1,1,0,0),
##               'NB_S'=c(0,0,1,-1),
##              'Alg_Bare'=c(0.5,0.5,-0.5,-0.5),
##              'Alg_NB'=c(0.5,0.5,-1,0))
## #crossprod(cmat)
## emmeans(day.brms, ~TREAT, contr=list(TREAT=cmat), type='link')
## emmeans(day.brms, ~TREAT, contr=list(TREAT=cmat), type='response')
## day.em = emmeans(day.brms, ~TREAT, contr=list(TREAT=cmat), type='link')$contrasts %>%
##       gather_emmeans_draws() %>%
##       mutate(Fit=exp(.value))
## day.em %>% group_by(contrast) %>% median_hdci()
## # Probability of effect
## day.em %>% group_by(contrast) %>% summarize(P=sum(Fit>1)/n())
## ##Probability of effect greater than 10%
## day.em %>% group_by(contrast) %>% summarize(P=sum(Fit>1.1)/n())
## 
## hist(bayes_R2(day.brms, summary=FALSE))
## 
## bayes_R2(day.brms, summary=FALSE) %>% median_hdi
## 
## 
## ## Summary plot
## day.grid = with(day, list(TREAT=levels(TREAT)))
## newdata = emmeans(day.brms, ~TREAT, type='response') %>% as.data.frame
## head(newdata)
## g2 <- ggplot(newdata, aes(y=rate, x=TREAT)) +
##   geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD))
## 
## library(patchwork)
## g1 + g2

