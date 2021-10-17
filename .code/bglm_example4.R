## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE,cache.lazy = FALSE, tidy='styler')


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
library(patchwork)  #for multiple plots


## ----readData, results='markdown', eval=TRUE----------------------------------
loyn <- read_csv('../data/loyn.csv', trim_ws = TRUE)
glimpse(loyn)


## ----processData, results='markdown', eval=TRUE-------------------------------
loyn <- loyn %>% mutate(fGRAZE = factor(GRAZE))


## ----EDA1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
scatterplotMatrix(~ABUND+DIST+LDIST+AREA+GRAZE+ALT+YR.ISOL, data = loyn,
                  diagonal = list(method = 'boxplot'))


## ----EDA1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
scatterplotMatrix(~ABUND+log(DIST)+log(LDIST)+log(AREA)+GRAZE+ALT+YR.ISOL, data = loyn,
                  diagonal = list(method = 'boxplot'))


## ----lm, results='markdown', eval=TRUE, hidden=TRUE---------------------------
loyn.glm <- glm(ABUND~scale(log(DIST))+scale(log(LDIST))+scale(log(AREA))+
                    fGRAZE + scale(ALT) + scale(YR.ISOL),
                data = loyn,
                family = gaussian(link='log'))
loyn.glm %>% summary()


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
loyn.brm <- brm(bf(ABUND ~ scale(log(DIST))+
                       scale(log(LDIST))+
                       scale(log(AREA))+
                       fGRAZE+
                       scale(ALT)+
                       scale(YR.ISOL),
                   family = lognormal()),
                data = loyn,
                iter = 5000,
                warmup = 1000,
                chains = 3,
                thin = 5,
                refresh = 0)


## ----fitModel2b, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE, paged.print=FALSE,tidy.opts = list(width.cutoff = 80), echo=2----
options(width=100)
loyn.brm %>% prior_summary()
options(width=80)


## ----fitModel2d, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
priors <- prior(normal(0, 2.5), class = 'b')

loyn.brm1 <- brm(bf(ABUND ~ scale(log(DIST))+
                        scale(log(LDIST))+
                        scale(log(AREA))+
                        fGRAZE+
                        scale(ALT)+
                        scale(YR.ISOL),
                    family = gaussian(link = 'log')),
                 data = loyn, 
                 prior = priors,
                 sample_prior = 'only', 
                 iter = 5000,
                 warmup = 1000,
                 chains = 3,
                 thin = 5,
                 refresh = 0)


## ----fitModel2e, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
## Individual plots
loyn.brm1 %>% ggemmeans(~AREA) %>% plot(add.data = TRUE) + scale_y_log10()
## All effects
loyn.brm1 %>%
    conditional_effects() %>%
    plot(points = TRUE, ask = FALSE, plot = FALSE) %>% 
    wrap_plots()
## All effects log y axis
## Do above, but then modify each list item
loyn.brm1 %>%
    conditional_effects() %>%
    plot(points = TRUE, ask = FALSE, plot = FALSE) %>%
    lapply(function(x) x + scale_y_log10()) %>%
    wrap_plots()


## ----fitModel2h, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
priors <- prior(normal(0, 10),  class = 'Intercept') +
    prior(normal(0, 1), class = 'b') +
    prior(gamma(2, 1), class = 'sigma')
 
loyn.brm2 <- brm(bf(ABUND ~ scale(log(DIST))+
                     scale(log(LDIST))+
                     scale(log(AREA))+
                     fGRAZE+
                     scale(ALT)+
                     scale(YR.ISOL),
                   family = gaussian(link = 'log')),
                data = loyn, 
                prior = priors,
                sample_prior = 'only', 
                iter = 5000,
                warmup = 1000,
                chains = 3,
                thin = 5,
                refresh = 0)


## ----fitModel2i, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
loyn.brm2 %>% ggemmeans(~AREA) %>%
    plot(add.data = TRUE) +
    scale_y_log10()

loyn.brm2 %>%
    conditional_effects() %>%
    plot(points = TRUE, ask = FALSE, plot = FALSE) %>%
    lapply(function(x) x + scale_y_log10()) %>%
    wrap_plots()


## ----fitModel2j, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
loyn.brm3 <- update(loyn.brm2,  sample_prior = 'yes', refresh = 0)


## ----fitModel2k, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
loyn.brm3 %>% get_variables()
loyn.brm3 %>% hypothesis('Intercept = 0', class = 'b') %>% plot
loyn.brm3 %>% hypothesis('Intercept = 0', class = 'prior') %>% plot
loyn.brm3 %>% hypothesis('scalelogAREA = 0') %>% plot
loyn.brm3 %>% hypothesis('sigma = 0', class = '') %>% plot
loyn.brm3 %>%
  posterior_samples %>%
  select(-`lp__`) %>%
  pivot_longer(everything(), names_to = 'key') %>%
  mutate(Type = ifelse(str_detect(key, 'prior'), 'Prior', 'b'),
         Class = ifelse(str_detect(key, 'Intercept'),  'Intercept',
               ifelse(str_detect(key, 'b'),  'b', 'sigma')),
         Par = str_replace(key, 'b_', '')) %>%
  ggplot(aes(x = Type,  y = value, color = Par)) +
  stat_pointinterval(position = position_dodge())+
  facet_wrap(~Class,  scales = 'free')


## ----fitModel2l, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
loyn.brm3 %>% standata()
loyn.brm3 %>% stancode()


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
loyn.brm3 %>% mcmc_plot(type = 'trace')


## ----modelValidation2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
loyn.brm3 %>% mcmc_plot(type = 'acf_bar')


## ----modelValidation2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
loyn.brm3 %>% mcmc_plot(type = 'rhat_hist')


## ----modelValidation2e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
loyn.brm3 %>% mcmc_plot(type = 'neff_hist')


## ----Validation2f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
loyn.brm3 %>% mcmc_plot(type = 'combo')
loyn.brm3 %>% mcmc_plot(type = 'violin')


## ----modelValidation2g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
loyn.brm3$fit %>% stan_trace()


## ----modelValidation2h, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
loyn.brm3$fit %>% stan_ac() 


## ----modelValidation2i, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
loyn.brm3$fit %>% stan_rhat() 


## ----modelValidation2j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
loyn.brm3$fit %>% stan_ess()


## ----modelValidation2k, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
loyn.brm3$fit %>% stan_dens(separate_chains = TRUE)


## ----modelValidation2l, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
loyn.ggs <- loyn.brm3 %>% ggs(inc_warmup = FALSE, burnin = FALSE)
loyn.ggs %>% ggs_traceplot()


## ----modelValidation2m, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
loyn.ggs %>% ggs_autocorrelation()


## ----modelValidation2n, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
loyn.ggs %>% ggs_Rhat()


## ----modelValidation2o, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
loyn.ggs %>% ggs_effective()


## ----modelValidation2p, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
loyn.ggs %>% ggs_crosscorrelation()


## ----modelValidation2q, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
loyn.ggs %>% ggs_grb()


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
loyn.brm3 %>% pp_check(type = 'dens_overlay')


## ----modelValidation5c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
loyn.brm3 %>% pp_check(type = 'error_scatter_avg')


## ----modelValidation5d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
loyn.brm3 %>% pp_check(x = 'AREA', type = 'error_scatter_avg_vs_x')


## ----modelValidation5e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
loyn.brm3 %>% pp_check(x = 'AREA', type = 'intervals')


## ----modelValidation5f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
loyn.brm3 %>% pp_check(x = 'AREA', type = 'ribbon')


## ----modelValidation5g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
#library(shinystan)
#launch_shinystan(loyn.brm3)


## ----modelValidation6a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
preds <- loyn.brm3 %>% posterior_predict(nsamples = 250,  summary = FALSE)
loyn.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = loyn$ABUND,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = FALSE)
loyn.resids %>% plot()


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
  plot(ask = FALSE, points = TRUE, plot = FALSE) %>%
  wrap_plots()

g <- loyn.brm3 %>%
  conditional_effects() %>%
  plot(ask = FALSE, points = TRUE, plot = FALSE) 
library(patchwork)
length(g)
(g[[1]] + scale_x_log10()) +
    (g[[2]] + scale_x_log10()) +
    (g[[3]] + scale_x_log10()) +
    g[[4]] +
    g[[5]] +
    g[[6]]


## ----partialPlot2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
loyn.brm3 %>%
    ggpredict() %>%
    plot(add.data=TRUE) %>%
    wrap_plots()


## ----partialPlot2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
## loyn.brm3 %>%
##     ggemmeans(~AREA) %>%
##     plot(add.data=TRUE) %>%
##     wrap_plots()


## ----partialPlot2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
loyn.brm3 %>%
    fitted_draws(newdata = loyn) %>%
    median_hdci() %>%
    ggplot(aes(x = AREA, y = .value, colour = fGRAZE, fill = fGRAZE)) +
    geom_ribbon(aes(ymin = .lower, ymax = .upper), colour = NA, alpha = 0.3) + 
    geom_line() +
    geom_point(data = loyn,  aes(y = ABUND,  x = AREA)) +
    scale_y_log10() +
    scale_x_log10()


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
loyn.brm3 %>% summary()


## ----summariseModel2a1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, echo=FALSE----
loyn.sum <- summary(loyn.brm3)


## ----summariseModel2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
loyn.brm3$fit %>%
    tidyMCMC(estimate.method = 'median',
             conf.int = TRUE,  conf.method = 'HPDinterval',
             rhat = TRUE, ess = TRUE)

## ----summariseModel2b1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
loyn.tidy <- tidyMCMC(loyn.brm3$fit, estimate.method='median',  conf.int=TRUE,  conf.method='HPDinterval',  rhat=TRUE, ess=TRUE)


## ----summariseModel2c1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
loyn.brm3 %>% get_variables()


## ----summariseModel2c2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
loyn.draw <- loyn.brm3 %>% gather_draws(`^b_.*`,  regex = TRUE)
loyn.draw


## ----summariseModel2d1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
loyn.brm3 %>% mcmc_plot(type = 'intervals') 


## ----summariseModel2e1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
loyn.brm3 %>% tidy_draws() 


## ----summariseModel2f1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
loyn.brm3 %>% spread_draws(`^b_.*`,  regex = TRUE) 


## ----summariseModel2g1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
loyn.brm3 %>% posterior_samples() %>% as_tibble() 


## ----summariseModel2h1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
loyn.brm3 %>% bayes_R2(summary = FALSE) %>% median_hdci 


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
loyn.brm4a <- update(loyn.brm3,  .~scale(log(DIST))*scale(log(LDIST)),
                     save_pars = save_pars(all = TRUE), refresh = 0)
loyn.brm4b <- update(loyn.brm3,  .~scale(log(AREA)) * fGRAZE,
                     save_pars = save_pars(all = TRUE), refresh = 0)
loyn.brm4c <- update(loyn.brm3,  .~scale(log(AREA)) * fGRAZE * scale(YR.ISOL),
                     save_pars = save_pars(all = TRUE), refresh = 0)
loyn.brm4d <- update(loyn.brm3,  .~scale(ALT),
                     save_pars = save_pars(all = TRUE), refresh = 0)
loyn.brm4e <- update(loyn.brm3,  .~1,
                     save_pars = save_pars(all = TRUE), refresh = 0)
waic(loyn.brm4a)
loo(loyn.brm4a)
loo_compare(loo(loyn.brm4a),
            loo(loyn.brm4e)
            )
# -2 * -2.5
loo_compare(loo(loyn.brm4b),
            loo(loyn.brm4e)
            )
loo_compare(loo(loyn.brm4b, moment_match = TRUE),
            loo(loyn.brm4e)
            )
loo_compare(loo(loyn.brm4c, moment_match = TRUE),
            loo(loyn.brm4e)
            )
loo_compare(loo(loyn.brm4d),
            loo(loyn.brm4e)
            )


## ----furtherModel2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
bayes_factor(loyn.brm4a,
            loyn.brm4e)
#OR
bayes_factor(loyn.brm4e,
            loyn.brm4a)

bayes_factor(loyn.brm4b,
             loyn.brm4e)
#OR
bayes_factor(loyn.brm4e,
             loyn.brm4b)

bayes_factor(loyn.brm4c,
             loyn.brm4e)
#OR
bayes_factor(loyn.brm4e,
             loyn.brm4c)

bayes_factor(loyn.brm4d,
             loyn.brm4e)
#OR
bayes_factor(loyn.brm4e,
             loyn.brm4d)


## ----furtherModel2c, results='markdown', cache=TRUE, eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
loyn.list <- with(loyn, list(AREA = c(min(AREA), mean(AREA), max(AREA))))

newdata <- loyn.brm4b %>%
    emmeans(~fGRAZE|AREA, at = loyn.list, type = 'response') %>%
    pairs() %>%
    as.data.frame 
head(newdata)

loyn.brm4b %>%
    emmeans(~fGRAZE|AREA, at = loyn.list) %>%
    regrid() %>%
    pairs()

newdata <- loyn.brm4b %>% 
    emmeans(~fGRAZE|AREA, at = loyn.list, type = 'response') %>%
    pairs() %>%
    gather_emmeans_draws()

newdata %>% median_hdci() %>%
    ggplot() +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    geom_pointrange(aes(y = .value, ymin = .lower, ymax = .upper, x = contrast)) +
    facet_wrap(~AREA) +
    coord_flip()

emmeans(loyn.brm4b, ~fGRAZE|AREA, at = loyn.list, type = 'response') %>%
    gather_emmeans_draws() 
newdata.p <- newdata %>% summarise(P = sum(.value>1)/n())
g <- newdata %>%
    ggplot() +
    geom_vline(xintercept = 1, linetype = 'dashed') +
    stat_slab(aes(x  =  .value, y = contrast,
                  fill = stat(ggdist::cut_cdf_qi(cdf,
                           .width = c(0.5, 0.8, 0.95), 
                           labels = scales::percent_format())
                           )), color = 'black') + 
    scale_fill_brewer('Interval', direction = -1, na.translate = FALSE) +
    facet_grid(~round(AREA,1))

g + geom_text(data = newdata.p, aes(y = contrast, x = 1, label = round(P,3)))

g + geom_text(data = newdata.p, aes(y = contrast, x = 1, label = paste('P = ',round(P,3))), hjust = -0.2, position = position_nudge(y = 0.5))


## ----summaryFigure1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, echo=TRUE----
loyn.list <- with(loyn, list(AREA = modelr::seq_range(AREA, n=100)))

newdata <- emmeans(loyn.brm3, ~AREA|fGRAZE, at = loyn.list, type='response') %>%
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
loyn.list <- with(loyn, list(AREA = modelr::seq_range(AREA, n=100)))

newdata <- emmeans(loyn.brm3, ~AREA|fGRAZE, at = loyn.list, type='response') %>%
    as.data.frame 
head(newdata)

ggplot(newdata, aes(y = response, x = AREA)) +
  geom_ribbon(aes(ymin = lower.HPD, ymax = upper.HPD, fill = fGRAZE), alpha = 0.3) +
  geom_line(aes(color = fGRAZE)) +
  theme_bw() +
  scale_x_log10() + 
  scale_y_log10()

spaghetti = emmeans(loyn.brm3, ~AREA|fGRAZE, at = loyn.list, type = 'response') %>%
  gather_emmeans_draws() %>% mutate(Fit = exp(.value))
wch <- sample(1:max(spaghetti$.draw), 100,replace = FALSE)
spaghetti <- spaghetti %>% filter(.draw %in% wch)
ggplot(newdata) +
  geom_line(data = spaghetti, aes(y = Fit, x = AREA, color = fGRAZE,
                                group = interaction(fGRAZE,.draw)), alpha = 0.1) +
  geom_line(aes(y = response, x = AREA, color = fGRAZE)) +
  theme_bw() +
  scale_x_log10() + scale_y_log10()


## ----summaryFigure2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, echo=TRUE----
loyn.list <- with(loyn, list(AREA = modelr::seq_range(AREA, n = 100)))

newdata <- emmeans(loyn.brm4b, ~AREA|fGRAZE, at = loyn.list, type='response') %>%
    as.data.frame 
head(newdata)

ggplot(newdata, aes(y = response, x = AREA)) +
  geom_ribbon(aes(ymin = lower.HPD, ymax = upper.HPD, fill = fGRAZE), alpha = 0.3) +
  geom_line(aes(color = fGRAZE)) +
  theme_bw() +
  scale_x_log10() + 
  scale_y_log10()

spaghetti = emmeans(loyn.brm4b, ~AREA|fGRAZE, at = loyn.list, type = 'response') %>%
  gather_emmeans_draws() %>% mutate(Fit = exp(.value))
wch <- sample(1:max(spaghetti$.draw), 100,replace = FALSE)
spaghetti <- spaghetti %>% filter(.draw %in% wch)
ggplot(newdata) +
  geom_line(data = spaghetti, aes(y = Fit, x = AREA, color = fGRAZE,
                                group = interaction(fGRAZE,.draw)), alpha = 0.1) +
  geom_line(aes(y = response, x = AREA, color = fGRAZE)) +
  theme_bw() +
  scale_x_log10() + scale_y_log10()

