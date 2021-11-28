## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE,cache.lazy = FALSE, tidy='styler')


## ----libraries, results='markdown', eval=TRUE, message=FALSE, warning=FALSE----
library(car)       #for regression diagnostics
library(broom)     #for tidy output
library(ggfortify) #for model diagnostics
library(sjPlot)    #for outputs
library(knitr)     #for kable
library(effects)   #for partial effects plots
library(emmeans)   #for estimating marginal means
library(MASS)      #for glm.nb
library(MuMIn)     #for AICc
library(tidyverse) #for data wrangling
library(brms)
library(tidybayes)
library(bayesplot)
library(broom.mixed)
library(rstan)
library(patchwork)
library(DHARMa)
library(standist)   #for visualizing distributions
library(rstanarm)
library(ggeffects)
library(DHARMa)


## ----readData, results='markdown', eval=TRUE----------------------------------
owls <- read_csv('../data/owls.csv', trim_ws = TRUE)
owls %>% glimpse()


## ----dataProcessing, results='markdown', eval=TRUE, hidden=TRUE---------------
## Amount of Sibling negotiation (vocalizations when parents are absent)
## Foot treatment (deprived or satiated
## Sex of parent
## Arrival time of parent
## Nest as random
## Brood size offset
owls <- owls %>% mutate(Nest =factor(Nest),
                       FoodTreatment = factor(FoodTreatment),
                       SexParent = factor(SexParent),
                       NCalls = SiblingNegotiation)


## ----eda1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
ggplot(data = owls, aes(y = NCalls, x = FoodTreatment,  color=SexParent)) +
  geom_violin() +
  geom_point()
ggplot(data = owls, aes(y = NCalls, x = FoodTreatment,  color=SexParent)) +
  geom_violin() +
  geom_point(position=position_jitterdodge(jitter.width=0.2, dodge.width=0.9))
ggplot(data = owls, aes(y = NCalls, x = FoodTreatment,  color=SexParent)) +
  geom_violin() +
  geom_point(position=position_jitterdodge(jitter.height=0,  dodge.width=1))+
  scale_y_continuous(trans=scales::pseudo_log_trans())


## ----eda2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10----
ggplot(data=owls) +
  geom_point(aes(y=NCalls,  x=FoodTreatment,  color=SexParent),  position=position_dodge(0.5)) +
  facet_wrap(~Nest)


## ----eda3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=5----
ggplot(data = owls,aes(y = NCalls, x = BroodSize, color=SexParent)) +
  geom_point() + 
  geom_smooth(method='lm') +
  facet_grid(~FoodTreatment) +
  scale_y_continuous(trans=scales::pseudo_log_trans()) +
  scale_x_log10()


## ----fitModel1a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
owls.rstanP <- stan_glmer(NCalls ~ FoodTreatment*SexParent +
                           offset(log(BroodSize)) + (1|Nest),
                          data = owls,
                          family = poisson(link = 'log'),
                          refresh = 0, 
                          iter = 5000,
                          warmup = 2000,
                          thin = 10,
                          chains = 3,
                          cores = 3)


## ----fitModel1b, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
owls.rstanP %>% prior_summary()


## ----fitModel1c, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
2.5*sd(owls$NCalls)


## ----fitModel1d, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
model.matrix(~FoodTreatment*SexParent, data = owls) %>%
    apply(2, sd) %>%
    (function(x) 2.5/x)


## ----fitModel1f, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
owls.rstanarmP1 <- update(owls.rstanP,  prior_PD=TRUE)


## ----fitModel1g, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
owls.rstanarmP1 %>%
    ggpredict(~FoodTreatment*SexParent) %>%
    plot(add.data=TRUE, jitter=c(0.25,0)) + 
    scale_y_continuous('', trans=scales::pseudo_log_trans())


## ----fitModel1h, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
owls.rstanarmP2 <- stan_glmer(NCalls ~ FoodTreatment*SexParent +
                               offset(log(BroodSize)) + (1|Nest),
                           data = owls,
                           family = poisson(link = 'log'),
                           prior_intercept = normal(0, 2.5, autoscale = FALSE),
                           prior = normal(0, 5, autoscale = FALSE),
                           prior_covariance = decov(1, 1, 1, 1), 
                           refresh = 0, 
                           iter = 5000,
                           prior_PD = TRUE,
                           warmup = 2000,
                           thin = 10,
                           chains = 3,
                           cores = 3)


## ----fitModel1i, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
owls.rstanarmP2 %>%
    ggpredict(~FoodTreatment*SexParent) %>%
    plot(add.data = TRUE, jitter = c(0.25, 0)) +
    scale_y_continuous('', trans=scales::pseudo_log_trans())


## ----fitModel2a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE, paged.print=FALSE, tidy.opts = list(width.cutoff = 80)----
owls.form <- bf(NCalls ~ FoodTreatment*SexParent +
                    offset(log(BroodSize)) + (1|Nest),
                family=poisson(link='log'))
options(width=150)
owls.form %>% get_prior(data = owls)
options(width=80)


## ----fitModel2h, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
owls %>% 
    group_by(FoodTreatment, SexParent) %>%
    summarise(log(median(NCalls)),
              log(mad(NCalls)))

standist::visualize("normal(1.8,5)", xlim=c(0,20))
standist::visualize("student_t(3, 0, 2.5)",
                    "cauchy(0,1)",
                    xlim=c(-10,25))


## ----fitModel2h1, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE------
priors <- prior(normal(1.8,5), class = 'Intercept') +
    prior(normal(0, 2), class = 'b') +
    prior(cauchy(0,1), class = 'sd') 
owls.form <- bf(NCalls ~ FoodTreatment*SexParent +
                    offset(log(BroodSize)) + (1|Nest),
                family=poisson(link='log'))
owls.brm2 <- brm(owls.form, 
                 data = owls,
                 prior = priors,
                 sample_prior = 'only',
                 iter = 5000,
                 warmup =2500,
                 chains = 3,
                 cores = 3,
                 thin = 10
                 )
owls.brm2 <- brm(owls.form, 
                 data = owls,
                 prior = priors,
                 sample_prior = 'yes',
                 iter = 5000,
                 warmup =2500,
                 chains = 3,
                 cores = 3,
                 thin = 10,
                 refresh = 0,
                 seed = 123
                 )

owls.form <- bf(NCalls ~ FoodTreatment*SexParent +
                    offset(log(BroodSize)) + (FoodTreatment*SexParent|Nest),
                family=poisson(link='log'))
owls.brm3 <-  brm(owls.form, 
                  data = owls,
                  prior = priors,
                  sample_prior = 'yes',
                  iter = 5000,
                  warmup = 2500,
                  chains = 3,
                  cores = 3,
                  thin = 10,
                  refresh = 0,
                  seed = 123, 
                  control = list(adapt_delta=0.99)
                  )

(l.1 <- owls.brm2 %>% loo())
(l.2 <- owls.brm3 %>% loo())
loo_compare(l.1, l.2)


## ----posterior2k, results='markdown', eval=TRUE-------------------------------
owls.brm3 %>% get_variables()
owls.brm3 %>% hypothesis('FoodTreatmentSatiated=0') %>% plot
owls.brm3 %>% hypothesis('SexParentMale=0') %>% plot


## ----posterior2k2, results='markdown', eval=TRUE, fig.width=10, fig.height=4----
owls.brm3 %>%
  posterior_samples %>%
  dplyr::select(-`lp__`) %>%
  pivot_longer(everything(), names_to = 'key') %>% 
  filter(!str_detect(key, '^r')) %>%
  mutate(Type = ifelse(str_detect(key, 'prior'), 'Prior', 'Posterior'),
         ## Class = ifelse(str_detect(key, 'Intercept'),  'Intercept',
         ##         ifelse(str_detect(key, 'b'),  'b', 'sigma')),
         Class = case_when(
             str_detect(key, '(^b|^prior).*Intercept$') ~ 'Intercept',
             str_detect(key, 'b_FoodTreatment.*|b_SexParent.*|prior_b') ~ 'TREATMENT',
             str_detect(key, 'sd') ~ 'sd',
             str_detect(key, '^cor|prior_cor') ~ 'cor',
             str_detect(key, 'sigma') ~ 'sigma'),
         Par = str_replace(key, 'b_', '')) %>%
  ggplot(aes(x = Type,  y = value, color = Par)) +
  stat_pointinterval(position = position_dodge(), show.legend = FALSE)+
  facet_wrap(~Class,  scales = 'free')



## ----modelValidation2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
available_mcmc()


## ----modelValidation2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pars <- owls.brm3 %>% get_variables()
pars <- pars %>% str_extract('^b.Intercept|^b_FoodTreatment.*|^b_SexParent.*|[sS]igma|^sd.*') %>%
    na.omit()
pars
owls.brm3 %>% mcmc_plot(type='trace', pars = pars)


## ----modelValidation2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
owls.brm3 %>% mcmc_plot(type='acf_bar', pars = pars)


## ----modelValidation2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
owls.brm3 %>% mcmc_plot(type='rhat_hist')


## ----modelValidation2e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
owls.brm3 %>% mcmc_plot(type='neff_hist')


## ----modelValidation2f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
owls.brm3 %>% mcmc_plot(type='combo', pars = pars)
owls.brm3 %>% mcmc_plot(type='violin', pars = pars)


## ----modelValidation2g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
owls.brm3 %>% get_variables()
pars <- owls.brm3 %>% get_variables()
pars <- str_extract(pars, '^b_.*|^sigma$|^sd.*') %>% na.omit()

owls.brm3$fit %>%
    stan_trace(pars = pars)


## ----modelValidation2h, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
owls.brm3$fit %>%
    stan_ac(pars = pars)


## ----modelValidation2i, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
owls.brm3$fit %>% stan_rhat() 


## ----modelValidation2j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
owls.brm3$fit %>% stan_ess()


## ----modelValidation2k, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
owls.brm3$fit %>%
    stan_dens(separate_chains = TRUE, pars = pars)


## ----modelValidation2l, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=7----
## owls.ggs <- owls.brm3 %>% ggs(burnin = FALSE, inc_warmup = FALSE)
## owls.ggs %>% ggs_traceplot()


## ----modelValidation2m, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=7----
## ggs_autocorrelation(owls.ggs)


## ----modelValidation2n, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
## ggs_Rhat(owls.ggs)


## ----modelValidation2o, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
## ggs_effective(owls.ggs)


## ----modelValidation2p, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
## ggs_crosscorrelation(owls.ggs)


## ----modelValidation2q, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
## ggs_grb(owls.ggs)


## ----modelValidation5a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
available_ppc()


## ----modelValidation5b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
owls.brm3 %>% pp_check(type = 'dens_overlay', nsamples = 100)


## ----modelValidation5c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
owls.brm3 %>% pp_check(type = 'error_scatter_avg')


## ----modelValidation5e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
owls.brm3 %>% pp_check(group = 'Nest', type = 'intervals')


## ----modelValidation5g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
#library(shinystan)
#launch_shinystan(owls.brm2)


## ----modelValidation6a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
preds <- owls.brm3 %>% posterior_predict(nsamples = 250,  summary = FALSE)
owls.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = owls$NCalls,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = TRUE)
plot(owls.resids)


## ----validation2g, results='markdown', eval=TRUE, error=TRUE,hidden=TRUE, fig.width=7, fig.height=5, cache=FALSE, message=FALSE, warning=FALSE----
owls.resids %>% testZeroInflation()


## ----validation2h, results='markdown', eval=TRUE, error=TRUE,hidden=TRUE, fig.width=7, fig.height=5, cache=FALSE, message=FALSE, warning=FALSE----
owls.resids %>% testTemporalAutocorrelation(time=owls$ArrivalTime)
owls.resid1 <- owls.resids %>% recalculateResiduals(group=interaction(owls$ArrivalTime,  owls$Nest),  aggregateBy = mean)
owls.resid1 %>% testTemporalAutocorrelation(time=unique(owls$ArrivalTime))


## ----fitModel3a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
priors <- prior(normal(1.8, 5), class='Intercept') +
    prior(normal(0, 5), class='b') +
    prior(cauchy(0,1), class='sd') +
    prior(logistic(0,1), class='Intercept', dpar='zi') 
owls.form <- bf(NCalls ~ FoodTreatment*SexParent +
                    offset(log(BroodSize)) +
                    (FoodTreatment*SexParent|Nest),
                zi ~ 1,
                family=zero_inflated_poisson(link='log'))

owls.brm4 <- brm(owls.form,
                 data=owls,
                 prior = priors,
                 sample_prior = 'yes',
                 iter=5000,
                 warmup=2500,
                 thin=10,
                 chains=3,
                 refresh=0,
                 cores=3)


## ----fitModel3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=4----
preds <- owls.brm4 %>% posterior_predict(nsamples = 250,  summary = FALSE)
owls.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = owls$NCalls,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = TRUE)
plot(owls.resids, quantreg = TRUE)

owls.resids %>% testZeroInflation()
owls.resids %>% testDispersion()
owls.resids %>% testUniformity()
owls.resids %>% testQuantiles()
owls.resids %>% testResiduals()


## ----fitModel4a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
priors <- prior(normal(1.8, 5), class='Intercept') +
    prior(normal(0, 5), class='b') +
    prior(cauchy(0,1), class='sd') +
    prior(logistic(0,1), class='Intercept', dpar='zi') +
    prior(normal(0,1), class='b', dpar='zi')
owls.form <- bf(NCalls ~ FoodTreatment*SexParent +
                    offset(log(BroodSize)) +
                    (FoodTreatment*SexParent|Nest),
                zi ~ FoodTreatment*SexParent,
                family=zero_inflated_poisson(link='log'))

owls.brm5 <- brm(owls.form,
                 data=owls,
                 prior = priors,
                 sample_prior = 'yes',
                 iter=5000,
                 warmup=2500,
                 thin=10,
                 chains=3,
                 refresh=0,
                 cores=3)



## ----fitModel4b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=4----
preds <- owls.brm5 %>% posterior_predict(nsamples = 250,  summary = FALSE)
owls.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = owls$NCalls,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = TRUE)
plot(owls.resids, quantreg = TRUE)

owls.resids %>% testZeroInflation()
owls.resids %>% testDispersion()
owls.resids %>% testUniformity()
owls.resids %>% testQuantiles()
owls.resids %>% testResiduals()


## ----fitModel6, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE--------
priors <- prior(normal(1.8, 5), class='Intercept') +
    prior(normal(0, 5), class='b') +
    prior(cauchy(0,1), class='sd') +
    prior(logistic(0,1), class='Intercept', dpar='zi') +
    prior(gamma(0.01, 0.01), class='shape')
owls.form <- bf(NCalls ~ FoodTreatment*SexParent +
                    offset(log(BroodSize)) +
                    (FoodTreatment*SexParent|Nest),
                zi ~ 1,
                family=zero_inflated_negbinomial(link='log'))

owls.brm6 <- brm(owls.form,
                 data=owls,
                 prior = priors,
                 sample_prior = 'yes',
                 iter=5000,
                 warmup=2500,
                 thin=10,
                 chains=3,
                 refresh=0,
                 cores=3)


## ----fitModel6b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=4----
preds <- owls.brm6 %>% posterior_predict(nsamples = 250,  summary = FALSE)
owls.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = owls$NCalls,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = TRUE)
plot(owls.resids, quantreg = TRUE)

owls.resids %>% testZeroInflation()
owls.resids %>% testDispersion()
owls.resids %>% testUniformity()
owls.resids %>% testQuantiles()
owls.resids %>% testResiduals()


## ----fitModel5, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE--------
priors <- prior(normal(1.8, 5), class='Intercept') +
    prior(normal(0, 5), class='b') +
    prior(cauchy(0,1), class='sd') +
    prior(logistic(0,1), class='Intercept', dpar='zi') +
    prior(normal(0,1), class='b', dpar='zi') +
    prior(gamma(0.01, 0.01), class='shape')
owls.form <- bf(NCalls ~ FoodTreatment*SexParent +
                    offset(log(BroodSize)) +
                    (FoodTreatment*SexParent|Nest),
                zi ~ FoodTreatment*SexParent,
                family=zero_inflated_negbinomial(link='log'))

owls.brm7 <- brm(owls.form,
                 data=owls,
                 prior = priors,
                 sample_prior = 'yes',
                 iter=5000,
                 warmup=2500,
                 thin=10,
                 chains=3,
                 refresh=0,
                 cores=3)


## ----fitModel5b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=4----
preds <- owls.brm7 %>% posterior_predict(nsamples = 250,  summary = FALSE)
owls.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = owls$NCalls,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = TRUE)
plot(owls.resids, quantreg = TRUE)

owls.resids %>% testZeroInflation()
owls.resids %>% testDispersion()
owls.resids %>% testUniformity()
owls.resids %>% testQuantiles()
owls.resids %>% testResiduals()


## ----fitModel7a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=4----
(l.1 <- loo(owls.brm3))
(l.2 <- loo(owls.brm4))
(l.3 <- loo(owls.brm5))
(l.4 <- loo(owls.brm6))
(l.5 <- loo(owls.brm7))
loo_compare(l.1, l.2, l.3, l.4, l.5)


## ----partialPlot2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
owls.brm7 %>%
    conditional_effects("FoodTreatment:SexParent") %>%
    plot(points = TRUE, jitter = c(0.25,0))


## ----partialPlot2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
owls.brm7 %>%
    ggpredict(~FoodTreatment*SexParent) %>%
    plot(add.data = TRUE, jitter = c(0.25, 0))


## ----partialPlot2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
off <- owls %>% summarize(Mean=mean(BroodSize))
as.numeric(off)
owls.brm7 %>%
    ggemmeans(~FoodTreatment*SexParent, offset=off$Mean) %>%
    plot() 
owls.brm7 %>%
    ggemmeans(~FoodTreatment*SexParent, offset=log(off$Mean)) %>%
    plot() 
owls.brm7 %>%
    ggemmeans(~FoodTreatment*SexParent, offset=log(1)) %>%
    plot() 


## ----summariseModel2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
owls.brm7 %>% summary()


## ----summariseModel2a1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, echo=FALSE----
owls.sum <- summary(owls.brm7)


## ----summariseModel2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
owls.brm7$fit %>%
    tidyMCMC(estimate.method = 'median',
             conf.int = TRUE,  conf.method = 'HPDinterval',
             rhat = TRUE, ess = TRUE)

## ----summariseModel2b1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
owls.tidy <- tidyMCMC(owls.brm7$fit, estimate.method='median',
                         conf.int=TRUE,  conf.method='HPDinterval',
                         rhat=TRUE, ess=TRUE)


## ----summariseModel2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
owls.brm7 %>% get_variables()
owls.draw <- owls.brm7 %>%
    gather_draws(`b.Intercept.*|b_FoodTreatment.*|b_SexParent.*`,  regex=TRUE)
owls.draw


## ----summariseModel2c1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
owls.draw %>% median_hdci


## ----summariseModel2c3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
owls.gather <- owls.brm7 %>%
    gather_draws(`b_Intercept.*|b_FoodTreatment.*|b_SexParent.*`,  regex=TRUE) %>%
    median_hdci


## ----summariseModel2c4, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
owls.brm7 %>%
    gather_draws(`b_Intercept.*|b_FoodTreatment.*|b_SexParent.*`, regex=TRUE) %>% 
    ggplot() +
    geom_vline(xintercept=0, linetype='dashed') +
    stat_slab(aes(x = .value, y = .variable,
                  fill = stat(ggdist::cut_cdf_qi(cdf,
                                                 .width = c(0.5, 0.8, 0.95), 
                                                 labels = scales::percent_format())
                              )), color='black') + 
    scale_fill_brewer('Interval', direction = -1, na.translate = FALSE) 

owls.brm7 %>% 
    gather_draws(`.Intercept.*|b_FoodTreatment.*|b_SexParent.*`, regex=TRUE) %>% 
    ggplot() + 
    geom_vline(xintercept = 0, linetype='dashed') +
    stat_halfeye(aes(x=.value,  y=.variable)) +
    theme_classic()


## ----summariseModel2j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
owls.brm7$fit %>% plot(type='intervals') 


## ----summariseModel2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
owls.brm7 %>% tidy_draws()


## ----summariseModel2e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
owls.brm7 %>% spread_draws(`.*Intercept.*|b_FoodTreatment.*|b_SexParent.*`,  regex=TRUE)


## ----summariseModel2f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
owls.brm7 %>% posterior_samples() %>% as_tibble()


## ----summariseModel2g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
owls.brm7 %>%
    bayes_R2(re.form = NA, summary=FALSE) %>%
    median_hdci
owls.brm7 %>%
    bayes_R2(re.form = ~(1|Nest), summary=FALSE) %>%
    median_hdci
owls.brm7 %>%
    bayes_R2(re.form = ~(FoodTreatment*SexParent|Nest), summary=FALSE) %>%
    median_hdci


## ----postHoc1a, results='markdown', eval=TRUE, echo=1,hidden=TRUE-------------
owls.brm7 %>%
    emmeans(~FoodTreatment, type='response') 
owls.brm7 %>%
    emmeans(~FoodTreatment, offset=0, type='response') 

newdata <- owls.brm7 %>%
    emmeans(~FoodTreatment|SexParent, offset=0, type='response') %>%
    as.data.frame
head(newdata)
ggplot(newdata) +
    geom_pointrange(aes(y=prob,  x=FoodTreatment,  color=SexParent,
                        ymin=lower.HPD,  ymax=upper.HPD),
                    position=position_dodge(width=0.2)) +
    theme_classic()


## ----fitmodelrstanarm, results='markdown', eval=FALSE,hidden=TRUE, echo=FALSE----
## starling.rstan %>% get_variables()
## plot(starling.rstan,  'mcmc_trace', regex_pars = '^.intercept|^situation|^month|[ss]igma')
## plot(starling.rstan,  'mcmc_acf_bar', regex_pars = '^.intercept|^situation|^month|[ss]igma')
## plot(starling.rstan,  'mcmc_rhat_hist', regex_pars = '^.intercept|^situation|^month|[ss]igma')
## plot(starling.rstan,  'mcmc_neff_hist', regex_pars = '^.intercept|^situation|^month|[ss]igma')
## 
## 
## preds <- posterior_predict(starling.rstan,  nsamples=250,  summary=false)
## starling.resids <- createdharma(simulatedresponse = t(preds),
##                             observedresponse = starling$mass,
##                             fittedpredictedresponse = apply(preds, 2, median))
## plot(starling.resids)
## 
## 
## starling.rstan1 = stan_glmer(mass ~ month*situation+(month|bird),data=starling,
##                             iter=5000, warmup=2000, thin=5, chains=3, refresh=0)
## starling.rstan1 = stan_glmer(mass ~ month*situation+(month|bird),data=starling,
##                              iter=5000, warmup=2000, thin=5, chains=3, refresh=0,
##                              adapt_delta = 0.99)
## #pairs(starling.rstan1,  pars=c('(intercept)', 'monthnov'))
## starling.rstan1 %>% get_variables()
## pairs(starling.rstan1,  regex_pars=c('situation', 'sigma'))
## prior_summary(starling.rstan1)
## 
## plot(starling.rstan1,  'mcmc_trace', regex_pars = '^.intercept|^situation|^month|[ss]igma')
## plot(starling.rstan1,  'mcmc_acf_bar', regex_pars = '^.intercept|^situation|^month|[ss]igma')
## plot(starling.rstan1,  'mcmc_rhat_hist', regex_pars = '^.intercept|^situation|^month|[ss]igma')
## plot(starling.rstan1,  'mcmc_neff_hist', regex_pars = '^.intercept|^situation|^month|[ss]igma')
## 
## starling.rstan1 = stan_glmer(mass ~ month*situation+(month|bird),data=starling,
##                              iter=10000, warmup=5000, thin=15, chains=3, refresh=0,
##                              adapt_delta = 0.99)
## 
## plot(starling.rstan1,  'mcmc_trace', regex_pars = '^.intercept|^situation|^month|[ss]igma')
## plot(starling.rstan1,  'mcmc_acf_bar', regex_pars = '^.intercept|^situation|^month|[ss]igma')
## plot(starling.rstan1,  'mcmc_rhat_hist', regex_pars = '^.intercept|^situation|^month|[ss]igma')
## plot(starling.rstan1,  'mcmc_neff_hist', regex_pars = '^.intercept|^situation|^month|[ss]igma')
## preds <- posterior_predict(starling.rstan1,  nsamples=250,  summary=FALSE)
## starling.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = starling$MASS,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(starling.resids)
## 
## (l.1 <- loo(starling.rstan))
## (l.2 <- loo(starling.rstan1))
## loo_compare(l.1, l.2)
## 
## as.matrix(starling.rstan) %>% colnames
## posterior_vs_prior(starling.rstan1, color_by='vs', group_by=TRUE, regex_pars=c('^MONTH','^SITUATION','^[sS]igma'),
##                    facet_args=list(scales='free_y'))
## 
## 
## g=ggpredict(starling.rstan1) %>% plot
## do.call('grid.arrange',  g)
## ggemmeans(starling.rstan1, ~SITUATION|MONTH) %>% plot
## 
## summary(starling.rstan1)
## 
## nms <- starling.rstan1 %>% get_variables()
## nms
## wch <- grep('^.Intercept|^MONTH|^SITUATION|[sS]igma', nms)
## tidyMCMC(starling.rstan1$stanfit,conf.int=TRUE, conf.method='HPDinterval',
##          rhat=TRUE, ess=TRUE, pars=nms[wch])
## 
## emmeans(starling.rstan1, pairwise~MONTH|SITUATION)
## starling.em = emmeans(starling.rstan1, ~MONTH|SITUATION) %>%
##     gather_emmeans_draws() %>% spread(key=MONTH, value=.value) %>%
##     mutate(Eff=Jan-Nov,
##            PEff=100*(Jan-Nov)/Nov)
## starling.em %>% head
## 
## starling.em %>% ungroup %>%
##     dplyr::select(SITUATION,Eff,PEff) %>% group_by(SITUATION) %>% median_hdi
## 
## starling.em %>% ungroup %>%
##     dplyr::select(SITUATION,Eff,PEff) %>% group_by(SITUATION) %>%
##     summarize(Prob=sum(PEff>10)/n())
## 
## bayes_R2(starling.rstan1, re.form=NA) %>% median_hdi
## bayes_R2(starling.rstan1, re.form=~(1|BIRD)) %>% median_hdi
## bayes_R2(starling.rstan1, re.form=~(MONTH|BIRD)) %>% median_hdi
## 
## newdata = emmeans(starling.rstan1, ~MONTH|SITUATION) %>% as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=emmean, x=SITUATION)) +
##     geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD, fill=MONTH),
##                     position=position_dodge(width=0.3), shape=21)


## ----fitModel, results='markdown', eval=FALSE, hidden=TRUE, echo=FALSE--------
## owls.rstanP <- stan_glmer(NCalls ~ FoodTreatment+SexParent +
##                            offset(log(BroodSize)) + (1|Nest),
##                          dat=owls,  family=poisson(link='log'), refresh=0,
##                          iter=5000,  warmup=2000,  thin=10,  chains=3, cores=3)
## 
## 
## owls.rstanP <- stan_glmer(NCalls ~ FoodTreatment+scale(ArrivalTime) +
##                            offset(log(BroodSize)) + (1|Nest),
##                          dat=owls,  family=poisson(link='log'), refresh=0,
##                          iter=5000,  warmup=2000,  thin=10,  chains=3, cores=3)
## 
## owls.rstanP %>% get_variables()
## plot(owls.rstanP,  'mcmc_trace', regex_pars='^.Intercept|Food|Arrival|[sS]igma')
## plot(owls.rstanP,  'mcmc_acf_bar', regex_pars='^.Intercept|Food|Arrival|[sS]igma')
## plot(owls.rstanP,  'mcmc_rhat_hist', regex_pars='^.Intercept|Food|Arrival|[sS]igma')
## plot(owls.rstanP,  'mcmc_neff_hist', regex_pars='^.Intercept|Food|Arrival|[sS]igma')
## 
## 
## preds <- posterior_predict(owls.rstanP,  nsamples=250,  summary=FALSE)
## owls.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = owls$NCalls,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(owls.resids)
## 
## 
## owls.rstanNB <- stan_glmer(NCalls ~ FoodTreatment+SexParent +
##                            offset(log(BroodSize)) + (1|Nest),
##                          dat=owls,  family=neg_binomial_2(link='log'), refresh=0,
##                          iter=5000,  warmup=2000,  thin=10,  chains=3, cores=3)
## 
## owls.rstanNB <- stan_glmer(NCalls ~ FoodTreatment+scale(ArrivalTime) +
##                            offset(log(BroodSize)) + (1|Nest),
##                          dat=owls,  family=neg_binomial_2(link='log'), refresh=0,
##                          iter=5000,  warmup=2000,  thin=10,  chains=3, cores=3)
## 
## owls.rstanNB %>% get_variables()
## plot(owls.rstanNB,  'mcmc_trace', regex_pars='^.Intercept|Food|Arrival|[sS]igma')
## plot(owls.rstanNB,  'mcmc_acf_bar', regex_pars='^.Intercept|Food|Arrival|[sS]igma')
## plot(owls.rstanNB,  'mcmc_rhat_hist', regex_pars='^.Intercept|Food|Arrival|[sS]igma')
## plot(owls.rstanNB,  'mcmc_neff_hist', regex_pars='^.Intercept|Food|Arrival|[sS]igma')
## 
## 
## preds <- posterior_predict(owls.rstanNB,  nsamples=250,  summary=FALSE)
## owls.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = owls$NCalls,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(owls.resids)
## testZeroInflation(owls.resids)
## 
## testTemporalAutocorrelation(owls.resids,  time=owls$ArrivalTime)
## owls.resids1 <- recalculateResiduals(owls.resids,  group=interaction(owls$ArrivalTime,  owls$Nest),  aggregateBy = mean)
## testTemporalAutocorrelation(owls.resids1,  time=unique(owls$ArrivalTime))
## 
## ##Cant use zero inflation with glmer
## 
## owls.rstan1 <- stan_glmer(NCalls ~ FoodTreatment+scale(ArrivalTime) +
##                            offset(log(BroodSize)) + (FoodTreatment*scale(ArrivalTime)|Nest),
##                          dat=owls,  family=neg_binomial_2(link='log'), refresh=0,
##                          iter=5000,  warmup=2000,  thin=10,  chains=3, cores=3)
## 
## owls.rstan1 %>% get_variables()
## plot(owls.rstan1,  'mcmc_trace', regex_pars='^.Intercept|^Food|^scale.Arrival|[sS]igma')
## plot(owls.rstan1,  'mcmc_acf_bar', regex_pars='^.Intercept|^Food|^scale.Arrival|[sS]igma')
## plot(owls.rstan1,  'mcmc_rhat_hist', regex_pars='^.Intercept|^Food|^scale.Arrival|[sS]igma')
## plot(owls.rstan1,  'mcmc_neff_hist', regex_pars='^.Intercept|^Food|^scale.Arrival|[sS]igma')
## 
## preds <- posterior_predict(owls.rstan1,  nsamples=250,  summary=FALSE)
## owls.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = owls$NCalls,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(owls.resids)
## 


## ----fitModel.brms, results='markdown', eval=FALSE, hidden=TRUE, echo=FALSE----
## owls.form <- bf(NCalls ~ FoodTreatment*SexParent +
##                     offset(log(BroodSize)) + (1|Nest),
##                 family=poisson(link='log'))
## owls.brm1 <- brm(owls.form,
##                   data=owls,
##                   sample_prior = 'yes',
##                   iter=5000,  warmup=2000,
##                   thin=10,  chains=3, cores=3,
##                   refresh=0,
##                   )
## prior_summary(owls.brm1)
## 
## owls %>%
##     group_by(FoodTreatment, SexParent) %>%
##     summarise(log(median(NCalls)),
##               log(mad(NCalls)))
## priors <- prior(normal(1.8, 5), class='Intercept') +
##     prior(normal(0, 2), class='b') +
##     prior(gamma(2,1), class='sd')
## owls.form <- bf(NCalls ~ FoodTreatment*SexParent +
##                     offset(log(BroodSize)) + (1|Nest),
##                 family=poisson(link='log'))
## owls.brm1 <- brm(owls.form,
##                  data=owls,
##                  prior = priors,
##                  sample_prior = 'yes',
##                  iter=5000,  warmup=2000,
##                  thin=10,  chains=3, cores=3,
##                  refresh=0,
##                  )
## 
## owls.form <- bf(NCalls ~ FoodTreatment*SexParent +
##                     offset(log(BroodSize)) + (FoodTreatment*SexParent|Nest),
##                 family=poisson(link='log'))
## owls.brm2 <- brm(owls.form,
##                  data=owls,
##                  prior = priors,
##                  sample_prior = 'yes',
##                  iter=5000,  warmup=2500,
##                  thin=10,  chains=3, cores=3,
##                  refresh=0,
##                  )
## 
## (l.1 <- loo(owls.brm1))
## (l.2 <- loo(owls.brm2))
## loo_compare(l.1, l.2)
## 
## 
## owls.brm2 %>% get_variables()
## owls.brm2 %>% hypothesis('FoodTreatmentSatiated=0') %>% plot()
## owls.brm2 %>% hypothesis('b_FoodTreatmentSatiated=0', class='') %>% plot()
## 
## pars <- owls.brm2 %>% get_variables()
## wch <- grepl('^b_.*|^sd_.*', pars, perl=TRUE)
## wch <- grepl('^[bsd]{1,2}_.*', pars, perl=TRUE)
## 
## 
## g <- vector('list', length=sum(wch))
## names(g) <- pars[wch]
## 
## for (i in pars[wch]) {
##     print(i)
## }
## for (i in pars[wch]) {
##     print(i)
##     if (i == 'b_Intercept') next
##     p <- owls.brm2 %>% hypothesis(paste0(i,'=0'), class='') %>% plot()
##     g[[i]] <- p[[1]]
## }
## patchwork::wrap_plots(g)
## 
## p <- owls.brm2 %>% hypothesis('FoodTreatmentSatiated=0') %>% plot()
## 
## g <- vector('list', length=sum(wch)-1)
## names(g) <- pars[wch][-1]
## for (i in pars[wch]) {
##     print(i)
##     if (i == 'b_Intercept') next
##     p <- owls.brm2 %>% hypothesis(paste0(i,'=0'), class='') %>% plot()
##     g[[i]] <- p[[1]]
## }
## patchwork::wrap_plots(g)
## 
## stan_trace(owls.brm2$fit, pars = pars[wch])
## stan_ac(owls.brm2$fit, pars = pars[wch])
## stan_rhat(owls.brm2$fit, pars = pars[wch])
## stan_rhat(owls.brm2$fit)
## stan_ess(owls.brm2$fit)
## 
## ##mcmc_plot(owls.brmsP,  type='trace')
## ##mcmc_plot(owls.brmsP,  type='acf_bar')
## ##mcmc_plot(owls.brmsP,  type='rhat_hist')
## ##mcmc_plot(owls.brmsP,  type='neff_hist')
## ##mcmc_plot(owls.brmsP,  type='trace', regex_pars='^b.Intercept|Food|Arrival|sd')
## ##mcmc_plot(owls.brmsP,  type='acf_bar', regex_pars='^b.Intercept|Food|Arrival|sd')
## ##mcmc_plot(owls.brmsP,  type='rhat_hist', regex_pars='^b.Intercept|Food|Arrival|sd')
## ##mcmc_plot(owls.brmsP,  type='neff_hist', regex_pars='^b.Intercept|Food|Arrival|sd')
## 
## preds <- posterior_predict(owls.brm2,  nsamples=250,  summary=FALSE)
## owls.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = owls$NCalls,
##                             fittedPredictedResponse = apply(preds, 2, median),
##                             integerResponse = TRUE)
## plot(owls.resids)
## testZeroInflation(owls.resids)
## 
## 
## 
## ##owls.form <- bf(NCalls ~ FoodTreatment*SexParent +
## ##                    offset(log(BroodSize)) + (FoodTreatment*SexParent|Nest),
## ##                family=negbinomial(link='log'))
## ##owls.brmsNB <- brm(owls.form,  data=owls, refresh=0,
## ##                  iter=5000,  warmup=2000,  thin=10,  chains=3, cores=3)
## 
## ##mcmc_plot(owls.brmsNB,  type='trace')
## ##mcmc_plot(owls.brmsNB,  type='acf_bar')
## ##mcmc_plot(owls.brmsNB,  type='rhat_hist')
## ##mcmc_plot(owls.brmsNB,  type='neff_hist')
## ##preds <- posterior_predict(owls.brmsNB,  nsamples=250,  summary=FALSE)
## ##owls.resids <- createDHARMa(simulatedResponse = t(preds),
## ##                            observedResponse = owls$NCalls,
## ##                            fittedPredictedResponse = apply(preds, 2, median),
## ##                            integerResponse = TRUE)
## ##plot(owls.resids)
## ##testZeroInflation(owls.resids)
## 
## priors <- prior(normal(1.8, 5), class='Intercept') +
##     prior(normal(0, 5), class='b') +
##     prior(gamma(2,1), class='sd') +
##     prior(logistic(0,1), class='Intercept', dpar='zi') +
##     prior(normal(0,1), class='b', dpar='zi')
## owls.form <- bf(NCalls ~ FoodTreatment*SexParent +
##                     offset(log(BroodSize)) +
##                     (FoodTreatment*SexParent|Nest),
##                 zi ~ FoodTreatment+SexParent,
##                 family=zero_inflated_poisson(link='log'))
##                 ##family=zero_inflated_negbinomial(link='log'))
## 
## owls.brm3 <- brm(owls.form,
##                  data=owls,
##                  prior = priors,
##                  sample_prior = 'yes',
##                  iter=5000,  warmup=2500,
##                  thin=10,  chains=3, cores=3,
##                  refresh=0)
## prior_summary(owls.brm3)
## 
## pars <- owls.brm3 %>% get_variables()
## pars
## wch <- grepl('^b_.*|^sd_.*', pars, perl=TRUE)
## 
## g <- vector('list', length=sum(wch))
## names(g) <- pars[wch]
## for (i in pars[wch]) {
##     print(i)
##     p <- owls.brm3 %>% hypothesis(paste0(i,'=0'), class='') %>% plot()
##     g[[i]] <- p[[1]]
## }
## patchwork::wrap_plots(g)
## 
## g <- vector('list', length=sum(wch)-1)
## names(g) <- pars[wch][-1]
## for (i in pars[wch]) {
##     print(i)
##     if (i == 'b_Intercept') next
##     p <- owls.brm3 %>% hypothesis(paste0(i,'=0'), class='') %>% plot()
##     g[[i]] <- p[[1]]
## }
## patchwork::wrap_plots(g[[2:5]])
## 
## stan_trace(owls.brm3$fit, pars = pars[wch])
## stan_ac(owls.brm3$fit, pars = pars[wch])
## stan_rhat(owls.brm3$fit, pars = pars[wch])
## stan_rhat(owls.brm3$fit)
## stan_ess(owls.brm3$fit)
## 
## 
## 
## preds <- posterior_predict(owls.brm3,  nsamples=250,  summary=FALSE)
## owls.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = owls$NCalls,
##                             fittedPredictedResponse = apply(preds, 2, median),
##                             integerResponse = TRUE)
## plot(owls.resids)
## testZeroInflation(owls.resids)
## testDispersion(owls.resids)
## 
## priors <- prior(normal(1.8, 5), class='Intercept') +
##     prior(normal(0, 5), class='b') +
##     prior(gamma(2,1), class='sd') +
##     prior(logistic(0,1), class='Intercept', dpar='zi') +
##     prior(normal(0,1), class='b', dpar='zi') +
##     prior(gamma(0.01, 0.01), class='shape')
## owls.form <- bf(NCalls ~ FoodTreatment*SexParent +
##                     offset(log(BroodSize)) +
##                     (FoodTreatment*SexParent|Nest),
##                 zi ~ FoodTreatment+SexParent,
##                 family=zero_inflated_negbinomial(link='log'))
## 
## owls.brm4 <- brm(owls.form,
##                  data=owls,
##                  prior = priors,
##                  sample_prior = 'yes',
##                  iter=5000,  warmup=2500,
##                  thin=10,  chains=3, cores=3,
##                  refresh=0)
## 
## pars <- owls.brm4 %>% get_variables()
## pars
## wch <- grepl('^b_.*|^sd_.*', pars, perl=TRUE)
## 
## g <- vector('list', length=sum(wch))
## names(g) <- pars[wch]
## for (i in pars[wch]) {
##     print(i)
##     p <- owls.brm4 %>% hypothesis(paste0(i,'=0'), class='') %>% plot()
##     g[[i]] <- p[[1]]
## }
## patchwork::wrap_plots(g)
## 
## wch <- grepl('^b_.*|^sd_.*|.*shape.*', pars, perl=TRUE)
## stan_trace(owls.brm4$fit, pars = pars[wch])
## stan_ac(owls.brm4$fit, pars = pars[wch])
## stan_rhat(owls.brm4$fit, pars = pars[wch])
## stan_rhat(owls.brm4$fit)
## stan_ess(owls.brm4$fit)
## 
## preds <- posterior_predict(owls.brm4,  nsamples=250,  summary=FALSE)
## owls.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = owls$NCalls,
##                             fittedPredictedResponse = apply(preds, 2, median),
##                             integerResponse = TRUE)
## plot(owls.resids)
## testZeroInflation(owls.resids)
## testDispersion(owls.resids)
## 
## testTemporalAutocorrelation(owls.resids,  time=owls$ArrivalTime)
## owls.resids1 <- recalculateResiduals(owls.resids,  group=interaction(owls$ArrivalTime,  owls$Nest),  aggregateBy = mean)
## testTemporalAutocorrelation(owls.resids1,  time=unique(owls$ArrivalTime))
## 
## g <- owls.brm4 %>%
##     conditional_effects() %>%
##     plot(ask=FALSE, plot=FALSE, points=TRUE)
## patchwork::wrap_plots(g)
## 
## summary(owls.brm4)
## 
## tidyMCMC(owls.brmsZINB2$fit,  estimate.method='median',
##          conf.int=TRUE,  conf.method='HPDinterval',
##          rhat=TRUE,  ess=TRUE)
## 
## owls.brm4 %>% bayes_R2(re.form = NA, summary=FALSE) %>% median_hdci()
## owls.brm4 %>% bayes_R2(re.form = ~(1|Nest), summary=FALSE) %>% median_hdci()
## owls.brm4 %>% bayes_R2(re.form = ~(FoodTreatment*SexParent|Nest), summary=FALSE) %>% median_hdci()
## 
## owls.brm4 %>%
##     emmeans(~FoodTreatment, type='response')
## owls.brm4 %>%
##     emmeans(~FoodTreatment, offset=0, type='response')
## 
## newdata <- owls.brm4 %>%
##     emmeans(~FoodTreatment, offset=0, type='response') %>%
##     as.data.frame
## head(newdata)
## ggplot(newdata) +
##     geom_pointrange(aes(y=prob,  x=FoodTreatment,  color=SexParent,
##                         ymin=lower.HPD,  ymax=upper.HPD),
##                     position=position_dodge(width=0.2))
## ggplot(newdata, aes(y=prob, x=as.numeric(SexParent)) +
##     geom_line(aes(color=FoodTreatment)) +
##     geom_ribbon(aes(ymin=lower.HPD, ymax=upper.HPD, fill=FoodTreatment), alpha=0.2)
## 

