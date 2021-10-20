## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE,cache.lazy = FALSE, tidy='styler')


## ----libraries, results='markdown', eval=TRUE, message=FALSE, warning=FALSE----
library(rstanarm)   #for fitting models in STAN
library(brms)       #for fitting models in STAN
library(coda)       #for diagnostics
library(bayesplot)  #for diagnostics
library(ggmcmc)     #for diagnostics
library(rstan)      #for interfacing with STAN
library(DHARMa)     #for residual diagnostics
library(emmeans)    #for marginal means etc
library(broom)      #for tidying outputs
library(broom.mixed) #for tidying MCMC outputs
library(tidybayes)  #for more tidying outputs
library(ggeffects)  #for partial plots
library(tidyverse)  #for data wrangling etc
library(patchwork)  #for multiple figures


## ----readData, results='markdown', eval=TRUE----------------------------------
quinn <- read_csv('../data/quinn.csv', trim_ws = TRUE)
quinn %>% glimpse()
quinn %>% summary()


## ----dataprep, results='markdown', eval=TRUE----------------------------------
quinn <- quinn %>%
  mutate(SEASON = factor(SEASON,
                         levels = c('Spring', 'Summer', 'Autumn', 'Winter')),
                         DENSITY = factor(DENSITY))


## ----EDA, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6, hidden=TRUE----
quinn %>% head()
quinn %>% ggplot(aes(y=RECRUITS, x=SEASON, fill=DENSITY)) + geom_boxplot()


## ----fitModel1a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
quinn.rstanarmP <- stan_glm(RECRUITS~SEASON*DENSITY,
                            data = quinn,
                            family = poisson(link = 'log'),
                            refresh = 0,
                            chains = 3, iter = 5000, thin = 5, warmup = 2000)


## ----fitModel1b, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
quinn.rstanarmP %>% prior_summary()


## ----fitModel1d, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
2.5/apply(model.matrix(~SEASON*DENSITY, quinn)[,-1], 2, sd)


## ----fitModel1f, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
quinn.rstanarm1 <- stan_glm(RECRUITS~SEASON*DENSITY, data = quinn,
                            family = poisson(link = 'log'),
                            prior_PD = TRUE, 
                            refresh = 0,
                            chains = 3, iter = 5000, thin = 5, warmup = 2000)


## ----fitModel1g, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
ggpredict(quinn.rstanarm1,  ~SEASON+DENSITY) %>% plot()
ggemmeans(quinn.rstanarm1,  ~SEASON+DENSITY) %>%
  plot(add.data=TRUE)


## ----fitModel1h, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
quinn.rstanarm2 <- stan_glm(RECRUITS~SEASON*DENSITY, data = quinn,
                            family = poisson(link = 'log'),
                            prior_intercept = normal(2.3, 5, autoscale = FALSE),
                            prior = normal(0, 2, autoscale = FALSE),
                            prior_PD = TRUE, 
                            refresh = 0,
                            chains = 3, iter = 5000, thin = 5, warmup = 2000)


## ----fitModel1i, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
quinn.rstanarm2 %>% ggpredict(~SEASON+DENSITY) %>%
  plot(add.data = TRUE)


## ----fitModel1j, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
quinn.rstanarm3 <- quinn.rstanarm2 %>% update(prior_PD = FALSE) 


## ----modelFit1k, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
quinn.rstanarm3 %>% posterior_vs_prior(color_by = 'vs', group_by = TRUE,
                   facet_args = list(scales = 'free_y'))


## ----modelFit1l, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
quinn.rstanarm3 %>% ggpredict(~SEASON+DENSITY) %>% plot(add.data = TRUE)
quinn.rstanarm3 %>% ggemmeans(~SEASON+DENSITY) %>% plot(add.data = TRUE)


## ----fitModel2a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
quinn.form <- bf(RECRUITS ~ SEASON*DENSITY,  family = poisson(link = 'log'))


## ----fitModel2a1, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE, echo=2----
options(width=100)
get_prior(quinn.form,  data = quinn)
options(width=80)


## ----fitModel2d1, results='markdown', eval=TRUE-------------------------------
quinn %>%
    group_by(SEASON, DENSITY) %>%
    summarise(Mean = mean(RECRUITS),
              MAD = mad(RECRUITS)) %>%
    mutate(log(Mean),
           log(MAD))


## ----fitModel2h, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
priors <- prior(normal(2.3, 2), class = 'Intercept') +
    prior(normal(0, 1), class = 'b')
quinn.brm2 <- brm(quinn.form,
                  data = quinn,
                  prior = priors,
                  sample_prior = "only",
                  refresh = 0,
                  chains = 3,
                  iter = 5000,
                  thin = 5,
                  warmup = 2000)


## ----fitModel2i, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
quinn.brm2 %>% ggpredict(~SEASON+DENSITY) %>% plot(add.data=TRUE)
quinn.brm2 %>% ggpredict(~SEASON+DENSITY) %>% plot(add.data=TRUE) + scale_y_log10()

quinn.brm2 %>% ggemmeans(~SEASON+DENSITY) %>% plot(add.data=TRUE)
quinn.brm2 %>% ggemmeans(~SEASON+DENSITY) %>% plot(add.data=TRUE) + scale_y_log10()

quinn.brm2 %>% conditional_effects('SEASON:DENSITY') %>%  plot(points=TRUE)
quinn.brm2 %>% conditional_effects('SEASON:DENSITY') %>%  plot(points=TRUE) %>% `[[`(1) + scale_y_log10()


## ----fitModel2j, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
quinn.brmP <- quinn.brm2 %>% update(sample_prior = 'yes', refresh = 0)


## ----fitModel2k, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
quinn.brmP %>% get_variables()
quinn.brmP %>% hypothesis('SEASONSummer<0') %>% plot()
quinn.brmP %>% hypothesis('DENSITYLow<0') %>% plot()
quinn.brmP %>% hypothesis('SEASONSummer:DENSITYLow<0') %>% plot()
quinn.brmP %>%
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
quinn.brmP %>% standata()
quinn.brmP %>% stancode()


## ----modelValidation1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
available_mcmc()


## ----modelValidation1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(quinn.rstanarm3, plotfun='mcmc_trace')


## ----modelValidation1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(quinn.rstanarm3, 'acf_bar')


## ----modelValidation1d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(quinn.rstanarm3, 'rhat_hist')


## ----modelValidation1e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(quinn.rstanarm3, 'neff_hist')


## ----Validation1f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(quinn.rstanarm3, 'combo')
plot(quinn.rstanarm3, 'violin')


## ----modelValidation1g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_trace(quinn.rstanarm3)


## ----modelValidation1h, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_ac(quinn.rstanarm3) 


## ----modelValidation1i, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_rhat(quinn.rstanarm3) 


## ----modelValidation1j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_ess(quinn.rstanarm3)


## ----modelValidation1k, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_dens(quinn.rstanarm3, separate_chains = TRUE)


## ----modelValidation1l, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
quinn.ggs <- ggs(quinn.rstanarm3, burnin = FALSE, inc_warmup = FALSE)
ggs_traceplot(quinn.ggs)


## ----modelValidation1m, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_autocorrelation(quinn.ggs)


## ----modelValidation1n, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_Rhat(quinn.ggs)


## ----modelValidation1o, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_effective(quinn.ggs)


## ----modelValidation1p, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_crosscorrelation(quinn.ggs)


## ----modelValidation1q, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_grb(quinn.ggs)


## ----modelValidation2g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
quinn.brmP$fit %>% stan_trace()
quinn.brmP$fit %>% stan_trace(inc_warmup=TRUE)


## ----modelValidation2h, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
quinn.brmP$fit %>% stan_ac() 


## ----modelValidation2i, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
quinn.brmP$fit %>% stan_rhat() 


## ----modelValidation2j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
quinn.brmP$fit %>% stan_ess()


## ----modelValidation2k, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
quinn.brmP$fit %>% stan_dens(separate_chains = TRUE)


## ----modelValidation3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
available_ppc()


## ----modelValidation3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(quinn.rstanarm3,  plotfun='dens_overlay')


## ----modelValidation3c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(quinn.rstanarm3, plotfun='error_scatter_avg')


## ----modelValidation3d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(quinn.rstanarm3, x=as.numeric(quinn$SEASON), plotfun='error_scatter_avg_vs_x')
pp_check(quinn.rstanarm3, x=as.numeric(quinn$DENSITY), plotfun='error_scatter_avg_vs_x')


## ----modelValidation3e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(quinn.rstanarm3, x=as.numeric(quinn$SEASON), plotfun='intervals')


## ----modelValidation3g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
#library(shinystan)
#launch_shinystan(quinn.rstanarm3)


## ----modelValidation4a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
preds <- posterior_predict(quinn.rstanarm3,  nsamples=250,  summary=FALSE)
quinn.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = quinn$RECRUITS,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = TRUE)
plot(quinn.resids)


## ----modelValidation5a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
available_ppc()


## ----modelValidation5b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
quinn.brmP %>% pp_check(type = 'dens_overlay')


## ----modelValidation5c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
quinn.brmP %>% pp_check(type = 'error_scatter_avg')


## ----modelValidation5e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
quinn.brmP %>% pp_check(type='intervals')
## quinn.brmP %>% pp_check(group='DENSITY', type='intervals')


## ----modelValidation5g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
#library(shinystan)
#launch_shinystan(quinn.brmP)


## ----modelValidation6a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
preds <- quinn.brmP %>% posterior_predict(nsamples = 250,  summary = FALSE)
day.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = quinn$RECRUITS,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = TRUE)
day.resids %>% plot()

quinn.resids %>% testDispersion()
quinn.resids %>% testZeroInflation()


## ----fitModel3a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
quinn.rstanarmNB <- stan_glm(RECRUITS~SEASON*DENSITY, data = quinn,
                            family = neg_binomial_2(link = 'log'),
                            prior_intercept = normal(2.3, 5, autoscale = FALSE),
                            prior = normal(0, 2, autoscale = FALSE),
                            prior_aux = rstanarm::exponential(rate = 1, autoscale = FALSE),
                            prior_PD = FALSE, 
                            refresh = 0,
                            chains = 3, iter = 5000, thin = 5, warmup = 2000)


## ----fitModel3b, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
posterior_vs_prior(quinn.rstanarmNB, color_by='vs', group_by=TRUE,
                   facet_args=list(scales='free_y'))

quinn.rstanarmNB %>% ggpredict(~SEASON+DENSITY) %>% plot(add.data = TRUE)

quinn.rstanarmNB %>% ggemmeans(~SEASON+DENSITY, transform = TRUE) %>% plot(add.data=TRUE)

quinn.rstanarmNB %>% plot('mcmc_trace')
quinn.rstanarmNB %>% plot('mcmc_acf_bar')
quinn.rstanarmNB %>% plot('mcmc_rhat_hist')
quinn.rstanarmNB %>% plot('mcmc_neff_hist')


preds <- posterior_predict(quinn.rstanarmNB,  nsamples=250,  summary=FALSE)
quinn.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = quinn$RECRUITS,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse=TRUE)
plot(quinn.resids)


## ----fitModel3c, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
(loo.P = loo(quinn.rstanarmP))
(loo.NB = loo(quinn.rstanarmNB))
loo_compare(loo.P, loo.NB)


## ----fitModel4a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
quinn.form <- bf(RECRUITS ~ SEASON*DENSITY,  family = negbinomial(link = 'log'))
get_prior(quinn.form,  data = quinn)

priors <- prior(normal(2.3, 2), class = 'Intercept') +
    prior(normal(0, 1), class = 'b') +
    prior(gamma(0.01, 0.01), class = "shape")
quinn.brmsNB <- brm(quinn.form,
                    data = quinn,
                    prior = priors,
                    refresh = 0,
                    chains = 3,
                    iter = 5000,
                    thin = 5,
                    warmup = 2000) 


## ----fitModel4a1, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE-----
preds <- posterior_predict(quinn.brmsNB,  nsamples = 250,  summary = FALSE)
quinn.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = quinn$RECRUITS,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = TRUE)
plot(quinn.resids)


## ----partialPlot1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
quinn.rstanarmNB %>% ggpredict(~SEASON+DENSITY) %>% plot(add.data=TRUE)


## ----partialPlot1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
quinn.rstanarmNB %>%
    ggemmeans(~SEASON|DENSITY,  type='fixed', transform = TRUE) %>%
    plot(add.data=TRUE)


## ----partialPlot1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
quinn.rstanarmNB %>%
    fitted_draws(newdata=quinn) %>%
    median_hdci() %>%
    ggplot(aes(x=SEASON, colour=DENSITY, y=.value)) +
    geom_pointrange(aes(ymin=.lower, ymax=.upper), position = position_dodge(width=0.2)) + 
    geom_line(position = position_dodge(width=0.2)) +
    geom_point(data=quinn,  aes(y=RECRUITS,  x=SEASON, colour = DENSITY), position = position_dodge(width=0.2))


## ----partialPlot2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
quinn.brmsNB %>% conditional_effects("SEASON:DENSITY") %>% plot(points = TRUE)


## ----partialPlot2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
quinn.brmsNB %>% ggpredict(~SEASON+DENSITY) %>% plot(add.data = TRUE)


## ----partialPlot2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
quinn.brmsNB %>% ggemmeans(~SEASON|DENSITY) %>% plot(add.data = TRUE)


## ----partialPlot2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
quinn.brmsNB %>%
    fitted_draws(newdata=quinn) %>%
    median_hdci() %>%
    ggplot(aes(x=SEASON, colour=DENSITY, y=.value)) +
    geom_pointrange(aes(ymin=.lower, ymax=.upper), position = position_dodge(width=0.2)) + 
    geom_line(position = position_dodge(width=0.2)) +
    geom_point(data=quinn,  aes(y=RECRUITS,  x=SEASON, colour = DENSITY), position = position_dodge(width=0.2))


## ----summariseModel1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
quinn.rstanarmNB %>% summary()


## ----summariseModel1a1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, echo=FALSE----
quinn.sum <- summary(quinn.rstanarmNB)


## ----summariseModel1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
tidyMCMC(quinn.rstanarmNB$stanfit, estimate.method='median',  conf.int=TRUE,  conf.method='HPDinterval',  rhat=TRUE, ess=TRUE)

## ----summariseModel1b1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
quinn.tidy <- tidyMCMC(quinn.rstanarmNB$stanfit, estimate.method='median',  conf.int=TRUE,  conf.method='HPDinterval',  rhat=TRUE, ess=TRUE)


## ----summariseModel1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
quinn.rstanarmNB %>% get_variables()
quinn.draw <- quinn.rstanarmNB %>% gather_draws(`.Intercept.*|.*SEASON.*|.*DENSITY.*`,  regex=TRUE)
quinn.draw


## ----summariseModel1c1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
quinn.draw %>% median_hdci


## ----summariseModel1c3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
quinn.gather <- quinn.rstanarmNB %>% gather_draws(`.Intercept.*|.*SEASON.*|.*DENSITY.*`,  regex=TRUE) %>%
  median_hdci


## ----summariseModel1c4, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
quinn.rstanarmNB %>% 
  gather_draws(`.Intercept.*|.*SEASON.*|.*DENSITY.*`, regex=TRUE) %>% 
  ggplot() + 
  stat_halfeye(aes(x=.value,  y=.variable)) +
  facet_wrap(~.variable, scales='free')
quinn.rstanarmNB %>% 
  gather_draws(`.Intercept.*|.*SEASON.*|.*DENSITY.*`, regex=TRUE) %>% 
  ggplot() + 
    geom_vline(xintercept=0, linetype='dashed') +
    stat_halfeye(aes(x=.value,  y=.variable)) +
    theme_classic()


## ----summariseModel1c5, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
quinn.rstanarmNB %>% 
  gather_draws(`.Intercept.*|.*SEASON.*|.*DENSITY.*`, regex=TRUE) %>%
  group_by(.variable) %>%
  mutate(.value=exp(.value)) %>%
  median_hdci
quinn.rstanarmNB %>% 
  gather_draws(`.Intercept.*|.*SEASON.*|.*DENSITY.*`, regex=TRUE) %>% 
  mutate(.value=exp(.value)) %>%
  ggplot() + 
    geom_vline(xintercept=1, linetype='dashed') +
    stat_halfeye(aes(x=.value,  y=.variable)) +
    scale_x_continuous('', trans = scales::log2_trans(), breaks=unique(as.vector(2^(0:4 %o% c(-1,1))))) +
    theme_classic()


## ----summariseModel1j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
quinn.rstanarmNB %>% plot(plotfun='mcmc_intervals') 


## ----summariseModel1d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
quinn.rstanarmNB %>% tidy_draws()


## ----summariseModel1e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
quinn.rstanarmNB %>% spread_draws(`.Intercept.*|.*SEASON.*|.*DENSITY.*`,  regex=TRUE)


## ----summariseModel1f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
quinn.rstanarmNB %>% posterior_samples() %>% as_tibble()


## ----summariseModel1g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
#quinn.rstanarmNB %>% bayes_R2() %>% median_hdci


## ----summariseModel2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
quinn.brmsNB %>% summary()


## ----summariseModel2a1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, echo=FALSE----
quinn.sum <- quinn.brmsNB %>% summary()
quinn.sum <- quinn.sum$fixed


## ----summariseModel2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
quinn.brmsNB$fit %>% tidyMCMC(estimate.method = 'median',  conf.int = TRUE,  conf.method = 'HPDinterval',  rhat = TRUE, ess = TRUE)

## ----summariseModel2b1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
quinn.tidy <- tidyMCMC(quinn.brmsNB$fit, estimate.method='median',  conf.int=TRUE,  conf.method='HPDinterval',  rhat=TRUE, ess=TRUE)


## ----summariseModel2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
quinn.brmsNB %>% get_variables()
quinn.draw <- quinn.brmsNB %>%
    gather_draws(`.Intercept.*|.*SEASON.*|.*DENSITY.*`,  regex = TRUE)
quinn.draw


## ----summariseModel2c1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
quinn.draw %>% median_hdci


## ----summariseModel2c3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
quinn.gather <- quinn.brmsNB %>% gather_draws(`.Intercept.*|.*SEASON.*|.*DENSITY.*`,  regex = TRUE) %>%
  median_hdci


## ----summariseModel2c4, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
quinn.brmsNB %>%
    gather_draws(`.Intercept.*|.*SEASON.*|.*DENSITY.*`, regex=TRUE) %>% 
    ggplot() +
    geom_vline(xintercept=0, linetype='dashed') +
    stat_slab(aes(x = .value, y = .variable,
                  fill = stat(ggdist::cut_cdf_qi(cdf,
                           .width = c(0.5, 0.8, 0.95), 
                           labels = scales::percent_format())
                           )), color='black') + 
    scale_fill_brewer('Interval', direction = -1, na.translate = FALSE) 

quinn.brmsNB %>% 
  gather_draws(`.Intercept.*|.*SEASON.*|.*DENSITY.*`, regex=TRUE) %>% 
  ggplot() + 
    geom_vline(xintercept=0, linetype='dashed') +
    stat_halfeye(aes(x=.value,  y=.variable)) +
    theme_classic()


## ----summariseModel2c5, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
quinn.brmsNB %>% 
  gather_draws(`.Intercept.*|.*SEASON.*|.*DENSITY.*`, regex=TRUE) %>%
  group_by(.variable) %>%
  mutate(.value=exp(.value)) %>%
  median_hdci

quinn.brmsNB %>%
    gather_draws(`.Intercept.*|.*SEASON.*|.*DENSITY.*`, regex=TRUE) %>%
    mutate(.value=exp(.value)) %>%
    ggplot() +
    geom_vline(xintercept=1, linetype='dashed') +
    stat_slab(aes(x = .value, y = .variable,
                  fill = stat(ggdist::cut_cdf_qi(cdf,
                           .width = c(0.5, 0.8, 0.95), 
                           labels = scales::percent_format())
                           )), color='black') + 
    scale_x_continuous('', trans = scales::log2_trans(), breaks=unique(as.vector(2^(0:4 %o% c(-1,1))))) +
    scale_fill_brewer('Interval', direction = -1, na.translate = FALSE) +
    theme_classic()


## ----summariseModel2j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
quinn.brmsNB$fit %>% plot(type='intervals') 


## ----summariseModel2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
quinn.brmsNB %>% tidy_draws()


## ----summariseModel2e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
quinn.brmsNB %>% spread_draws(`.Intercept.*|.*SEASON.*|.*DENSITY.*`,  regex=TRUE)


## ----summariseModel2f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
quinn.brmsNB %>% posterior_samples() %>% as_tibble()


## ----summariseModel2g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
quinn.brmsNB %>% bayes_R2(summary=FALSE) %>% median_hdci


## ----mainEffects1a, results='markdown', eval=TRUE, hidden=TRUE----------------
## fold scale
quinn.rstanarmNB %>%
    emmeans(~DENSITY|SEASON, type='response') %>%
    pairs()
## absolute response scale
quinn.rstanarmNB %>%
    emmeans(~DENSITY|SEASON, type='link') %>%
    regrid() %>%
    pairs()


## ----mainEffects1b, results='markdown', eval=TRUE, hidden=TRUE----------------
quinn.em <- quinn.rstanarmNB %>%
    emmeans(~DENSITY|SEASON, type='link') %>%
    pairs() %>%
    gather_emmeans_draws() %>% 
    mutate(Fit=exp(.value))
head(quinn.em)

g2 <- quinn.em %>%
  group_by(contrast, SEASON) %>%
  median_hdci %>%
  ggplot() +
  geom_vline(xintercept=1, linetype='dashed') +
  geom_pointrange(aes(x=Fit, y=SEASON, xmin=Fit.lower, xmax=Fit.upper)) + 
  scale_x_continuous('Effect size (High/Low)', trans = scales::log2_trans(), breaks=unique(as.vector(2^(0:4 %o% c(-1,1))))) +
  theme_classic()
g2

ggplot(quinn.em, aes(x=Fit)) +
    geom_histogram() +
    geom_vline(xintercept = 1, linetype='dashed') +
    scale_x_continuous('Effect size (High/Low)', trans = scales::log2_trans(), breaks=unique(as.vector(2^(0:4 %o% c(-1,1))))) +
    facet_wrap(SEASON~contrast, scales='free')
quinn.em %>% group_by(contrast, SEASON) %>% median_hdci(Fit)
# Probability of effect
quinn.em %>% group_by(contrast,SEASON) %>% summarize(P=sum(Fit>1)/n())
##Probability of effect greater than 10%
quinn.em %>% group_by(contrast,SEASON) %>% summarize(P=sum(Fit>1.1)/n())



## ----mainEffects1c, results='markdown', eval=TRUE, hidden=TRUE----------------
newdata <- with(quinn, expand.grid(SEASON = levels(SEASON),
                                  DENSITY = levels(DENSITY)))
Xmat<- model.matrix(~SEASON*DENSITY, data = newdata)
as.matrix(quinn.rstanarmNB) %>% head
## coefs <- as.matrix(quinn.rstanarmNB)
coefs <- as.matrix(as.data.frame(quinn.rstanarmNB) %>%
                  dplyr:::select(-reciprocal_dispersion)) %>%
    as.matrix
fit <- exp(coefs %*% t(Xmat))
newdata <- newdata %>%
    cbind(tidyMCMC(fit, conf.int = TRUE, conf.method = 'HPDinterval'))
head(newdata)

ggplot(newdata, aes(y = estimate, x = SEASON, fill = DENSITY)) +
    geom_blank() +
    geom_line(aes(x=as.numeric(SEASON), ymin=conf.low, ymax=conf.high, linetype=DENSITY),
              position = position_dodge(0.2))+
    geom_pointrange(aes(ymin=conf.low, ymax=conf.high), shape=21,
                    position = position_dodge(0.2)) 

#Compare high and low in each season
#via contrasts
newdata <- with(quinn, expand.grid(SEASON = levels(SEASON),
                                   DENSITY = levels(DENSITY)))
## factor differences
Xmat<- model.matrix(~SEASON*DENSITY, data=newdata)
Xmat.high <- Xmat[newdata$DENSITY=="High",]
Xmat.low <- Xmat[newdata$DENSITY=="Low",]
Xmat.density <- Xmat.high-Xmat.low
rownames(Xmat.density) <- levels(quinn$SEASON)
coefs = as.matrix(as.data.frame(quinn.rstanarmNB) %>% dplyr:::select(-reciprocal_dispersion))
fit = exp(coefs %*% t(Xmat.density))
tidyMCMC(fit, conf.int=TRUE, conf.method='HPDinterval')
## or absolute
fit.high = coefs %*% t(Xmat.high)
fit.low = coefs %*% t(Xmat.low)
fit = exp(fit.high) - exp(fit.low)
#fit = exp(fit.high - fit.low)
tidyMCMC(fit, conf.int=TRUE, conf.method='HPDinterval')


## ----mainEffects2a, results='markdown', eval=TRUE, hidden=TRUE----------------
quinn.brmsNB %>%
    emmeans(~DENSITY|SEASON, type='response') %>%
    pairs()
## absolute response scale
quinn.brmsNB %>%
    emmeans(~DENSITY|SEASON, type='link') %>%
    regrid() %>%
    pairs()


## ----mainEffects2b, results='markdown', eval=TRUE, hidden=TRUE----------------
quinn.em <- quinn.brmsNB %>%
    emmeans(~DENSITY|SEASON, type='link') %>%
    pairs() %>%
    gather_emmeans_draws() %>% 
    mutate(Fit=exp(.value))
head(quinn.em)

g2 <- quinn.em %>%
  group_by(contrast, SEASON) %>%
  median_hdci %>%
  ggplot() +
  geom_vline(xintercept=1, linetype='dashed') +
  geom_pointrange(aes(x=Fit, y=SEASON, xmin=Fit.lower, xmax=Fit.upper)) + 
  scale_x_continuous('Effect size (High/Low)', trans = scales::log2_trans(), breaks=unique(as.vector(2^(0:4 %o% c(-1,1))))) +
  theme_classic()
g2

ggplot(quinn.em, aes(x=Fit)) +
    geom_histogram() +
    geom_vline(xintercept = 1, linetype='dashed') +
    scale_x_continuous('Effect size (High/Low)', trans = scales::log2_trans(), breaks=unique(as.vector(2^(0:4 %o% c(-1,1))))) +
    facet_wrap(SEASON~contrast, scales='free')
quinn.em %>% group_by(contrast, SEASON) %>% median_hdci(Fit)
# Probability of effect
quinn.em %>% group_by(contrast,SEASON) %>% summarize(P=sum(Fit>1)/n())
##Probability of effect greater than 10%
quinn.em %>% group_by(contrast,SEASON) %>% summarize(P=sum(Fit>1.1)/n())



## ----summaryFig1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
newdata <- quinn.rstanarmNB %>%
    emmeans(~SEASON|DENSITY, type='response') %>%
    as.data.frame
head(newdata)
g1 <- ggplot(newdata, aes(y=prob, x=SEASON, color=DENSITY)) +
    geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD),
                    position=position_dodge(width=0.2)) + 
    theme_classic()
g1 + g2


## ----summaryFig2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
newdata <- quinn.brmsNB %>%
    emmeans(~SEASON|DENSITY, type='response') %>%
    as.data.frame
head(newdata)
g1 <- ggplot(newdata, aes(y=prob, x=SEASON, color=DENSITY)) +
    geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD),
                    position=position_dodge(width=0.2)) + 
    theme_classic()
g1 + g2


## ----fitModel.brms, results='markdown', eval=TRUE, hidden=TRUE----------------
quinn <- quinn %>%
  group_by(SEASON, DENSITY) %>%
  mutate(Obs = factor(1:n()))

quinn.form <- bf(RECRUITS ~ SEASON*DENSITY + (1|Obs),  family = poisson(link = 'log'))
get_prior(quinn.form,  data = quinn)

quinn.brmsU <- brm(quinn.form,
                   data = quinn,
                   refresh = 0,
                   chains = 3,
                   iter = 5000,
                   thin = 5,
                   warmup = 2000)

preds <- posterior_predict(quinn.brmsU,  nsamples=250,  summary=FALSE)
quinn.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = quinn$RECRUITS,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = TRUE)
plot(quinn.resids)
newdata = emmeans(quinn.brmsU, ~SEASON|DENSITY, type='response') %>% as.data.frame
newdata
ggplot(newdata, aes(y=rate, x=SEASON, color=DENSITY)) +
    geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD),
                    position=position_dodge(width=0.2))


