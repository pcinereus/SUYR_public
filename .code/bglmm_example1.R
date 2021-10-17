## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE,cache.lazy = FALSE, tidy='styler')


## ----libraries, results='markdown', eval=TRUE, message=FALSE, warning=FALSE----
library(rstanarm)   #for fitting models in STAN
library(brms)       #for fitting models in STAN
library(coda)       #for diagnostics
library(bayesplot)  #for diagnostics
library(ggmcmc)     #for MCMC diagnostics
library(rstan)      #for interfacing with STAN
library(emmeans)    #for marginal means etc
library(broom)      #for tidying outputs
library(DHARMa)     #for residual diagnostics
library(tidybayes)  #for more tidying outputs
library(ggeffects)  #for partial plots
library(broom.mixed)#for tidying MCMC outputs
library(tidyverse)  #for data wrangling etc
library(patchwork)  #for multiple plots


## ----readData, results='markdown', eval=TRUE----------------------------------
tobacco <- read_csv('../data/tobacco.csv', trim_ws = TRUE)
tobacco %>% glimpse()


## ----processData, results='markdown', eval=TRUE-------------------------------
tobacco <- tobacco %>% mutate(LEAF = factor(LEAF),
                             TREATMENT = factor(TREATMENT))
tobacco %>% head()


## ----tobaccoEDA2, results='markdown', eval=TRUE, hidden=TRUE------------------
ggplot(tobacco,  aes(y = NUMBER,  x = TREATMENT)) +
  geom_boxplot()


## ----tobaccoEDA3, results='markdown', eval=TRUE, hidden=TRUE------------------
ggplot(tobacco,  aes(y = NUMBER,  x = as.numeric(LEAF))) +
  geom_line(aes(linetype = TREATMENT))

## If we want to retain the original LEAF labels
ggplot(tobacco,  aes(y = NUMBER,  x = as.numeric(LEAF))) +
  geom_blank(aes(x = LEAF)) +
  geom_line(aes(linetype = TREATMENT))


## ----tobaccoEDA4, results='markdown', eval=TRUE, hidden=TRUE------------------
ggplot(tobacco,  aes(y = NUMBER,  x = TREATMENT,  group = LEAF)) +
  geom_point() +
  geom_line(aes(x = as.numeric(TREATMENT))) 


## ----fitModel1a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
tobacco.rstanarm <- stan_glmer(NUMBER ~ (1|LEAF) + TREATMENT,
                               data = tobacco,
                               family = gaussian(), 
                               iter = 5000,
                               warmup = 2000,
                               chains = 3,
                               thin = 5,
                               refresh = 0)


## ----fitModel1b, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
tobacco.rstanarm %>% prior_summary()


## ----fitModel1c, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
2.5*sd(tobacco$NUMBER)


## ----fitModel1d, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
2.5*sd(tobacco$NUMBER)/sd(model.matrix(~TREATMENT, tobacco)[, 2])


## ----fitModel1e, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
1/sd(tobacco$NUMBER)


## ----fitModel1f, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
tobacco.rstanarm1 <- update(tobacco.rstanarm,  prior_PD=TRUE)


## ----fitModel1g, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
ggpredict(tobacco.rstanarm1) %>% plot(add.data=TRUE)


## ----fitModel1h, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
tobacco.rstanarm2 <- stan_glmer(NUMBER ~ (1|LEAF) + TREATMENT,
                                data = tobacco,
                                family = gaussian(), 
                                prior_intercept = normal(35, 10, autoscale = FALSE),
                                prior = normal(0, 10, autoscale = FALSE),
                                prior_aux=rstanarm::exponential(0.1, autoscale = FALSE),
                                prior_covariance = decov(1, 1, 1, 1), 
                                prior_PD = TRUE, 
                                iter = 5000,
                                warmup = 1000,
                                chains = 3,
                                thin = 5,
                                refresh = 0
                                )


## ----fitModel1i, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
tobacco.rstanarm2 %>%
    ggpredict() %>%
    plot(add.data = TRUE)


## ----fitModel2a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE, paged.print=FALSE, tidy.opts = list(width.cutoff = 80), echo=c(-4,-6)----
tobacco.form <- bf(NUMBER ~ (1|LEAF) + TREATMENT,
                   family = gaussian() 
                   )
options(width=100)
tobacco.form %>% get_prior(data=tobacco)
options(width=80)
## tobacco.brm <- brm(tobacco.form,
##                   data=tobacco,
##                   iter = 5000,
##                   warmup = 1000,
##                   chains = 3,
##                   thin = 5,
##                   refresh = 0)


## ----fitModel2h, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
tobacco %>% 
    group_by(TREATMENT) %>%
    summarise(median(NUMBER),
              mad(NUMBER))

standist::visualize("normal(35,5)", xlim=c(-10,100))
standist::visualize("student_t(3, 0, 6.5)", xlim=c(-10,100))
standist::visualize("student_t(3, 0, 6.5)",
                    "gamma(2,1)",
                    "gamma(2,0.5)",
                    "gamma(5,0.1)",
                    "exponential(1)",
                    "cauchy(0,2)",
                    xlim=c(-10,25))


## ----fitModel2h1, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE------
priors <- prior(normal(35,20), class = 'Intercept') +
    prior(normal(0, 10), class = 'b') +
    prior(gamma(2,0.5), class = 'sigma') +
    prior(cauchy(0,2), class = 'sd') 
tobacco.form <- bf(NUMBER ~ (1|LEAF) + TREATMENT,
                     family = gaussian()
                   )
tobacco.brm2 <- brm(tobacco.form, 
                  data = tobacco,
                  prior = priors,
                  sample_prior = 'yes',
                  iter = 5000,
                  warmup = 1000,
                  chains = 3,
                  thin = 5,
                  refresh = 0
                  )

tobacco.form <- bf(NUMBER ~ (TREATMENT|LEAF) + TREATMENT,
                     family = gaussian()
                   )
tobacco.brm3 <-  brm(tobacco.form, 
                  data = tobacco,
                  prior = priors,
                  sample_prior = 'yes',
                  iter = 5000,
                  warmup = 1000,
                  chains = 3,
                  thin = 5,
                  refresh = 0,
                  control = list(adapt_delta=0.99)
                  )

(l.1 <- tobacco.brm2 %>% loo())
(l.2 <- tobacco.brm3 %>% loo())
loo_compare(l.1, l.2)


## ----posterior2k, results='markdown', eval=TRUE-------------------------------
tobacco.brm3 %>% get_variables()
tobacco.brm3 %>% hypothesis('TREATMENTWeak=0') %>% plot


## ----posterior2k2, results='markdown', eval=TRUE, fig.width=10, fig.height=4----
tobacco.brm3 %>%
  posterior_samples %>%
  dplyr::select(-`lp__`) %>%
  pivot_longer(everything(), names_to = 'key') %>% 
  filter(!str_detect(key, '^r')) %>%
  mutate(Type = ifelse(str_detect(key, 'prior'), 'Prior', 'Posterior'),
         ## Class = ifelse(str_detect(key, 'Intercept'),  'Intercept',
         ##         ifelse(str_detect(key, 'b'),  'b', 'sigma')),
         Class = case_when(
             str_detect(key, '(^b|^prior).*Intercept$') ~ 'Intercept',
             str_detect(key, 'b_TREATMENT|prior_b') ~ 'TREATMENT',
             str_detect(key, 'sd') ~ 'sd',
             str_detect(key, '^cor|prior_cor') ~ 'cor',
             str_detect(key, 'sigma') ~ 'sigma'),
         Par = str_replace(key, 'b_', '')) %>%
  ggplot(aes(x = Type,  y = value, color = Par)) +
  stat_pointinterval(position = position_dodge())+
  facet_wrap(~Class,  scales = 'free')



## ----modelValidation2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
available_mcmc()


## ----modelValidation2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
tobacco.brm3 %>% mcmc_plot(type='trace')


## ----modelValidation2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
tobacco.brm3 %>% mcmc_plot(type='acf_bar')


## ----modelValidation2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
tobacco.brm3 %>% mcmc_plot(type='rhat_hist')


## ----modelValidation2e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
tobacco.brm2 %>% mcmc_plot(type='neff_hist')


## ----modelValidation2f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
tobacco.brm3 %>% mcmc_plot(type='combo')
tobacco.brm3 %>% mcmc_plot(type='violin')


## ----modelValidation2g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
tobacco.brm3 %>% get_variables()
pars <- tobacco.brm3 %>% get_variables()
pars <- str_extract(pars, '^b_.*|^sigma$|^sd.*') %>% na.omit()

tobacco.brm3$fit %>%
    stan_trace(pars = pars)


## ----modelValidation2h, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
tobacco.brm3$fit %>%
    stan_ac(pars = pars)


## ----modelValidation2i, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
tobacco.brm3$fit %>% stan_rhat() 


## ----modelValidation2j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
tobacco.brm2$fit %>% stan_ess()


## ----modelValidation2k, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
tobacco.brm3$fit %>%
    stan_dens(separate_chains = TRUE, pars = pars)


## ----modelValidation2l, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=7----
tobacco.ggs <- tobacco.brm3 %>% ggs(burnin = FALSE, inc_warmup = FALSE)
tobacco.ggs %>% ggs_traceplot()


## ----modelValidation2m, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=7----
ggs_autocorrelation(tobacco.ggs)


## ----modelValidation2n, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_Rhat(tobacco.ggs)


## ----modelValidation2o, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_effective(tobacco.ggs)


## ----modelValidation2p, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_crosscorrelation(tobacco.ggs)


## ----modelValidation2q, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_grb(tobacco.ggs)


## ----modelValidation5a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
available_ppc()


## ----modelValidation5b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
tobacco.brm3 %>% pp_check(type = 'dens_overlay', nsamples = 100)


## ----modelValidation5c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
tobacco.brm3 %>% pp_check(type = 'error_scatter_avg')


## ----modelValidation5e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
tobacco.brm3 %>% pp_check(group = 'TREATMENT', type = 'intervals')


## ----modelValidation5g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
#library(shinystan)
#launch_shinystan(tobacco.brm2)


## ----modelValidation6a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
preds <- tobacco.brm3 %>% posterior_predict(nsamples = 250,  summary = FALSE)
tobacco.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = tobacco$NUMBER,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = FALSE)
plot(tobacco.resids, quantreg = FALSE)


## ----partialPlot2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
tobacco.brm3 %>%
    conditional_effects() %>%
    plot(points = TRUE)


## ----partialPlot2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
tobacco.brm3 %>%
    ggpredict() %>%
    plot(add.data = TRUE)


## ----partialPlot2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
tobacco.brm3 %>%
    ggemmeans(~TREATMENT) %>%
    plot(add.data = TRUE) +
    geom_point(data = tobacco, aes(y = NUMBER, x = as.numeric(TREATMENT)))


## ----partialPlot2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
Partial.obs <- tobacco.brm3$data %>%
    mutate(Pred = predict(tobacco.brm3)[,'Estimate'],
           Resid = resid(tobacco.brm3)[,'Estimate'],
           Obs = Pred + Resid)

tobacco.brm3 %>%
    fitted_draws(newdata = tobacco) %>%
    median_hdci() %>%
    ggplot(aes(x = TREATMENT, y = .value)) +
    geom_pointrange(aes(ymin = .lower, ymax = .upper)) + 
    geom_line() +
    geom_point(data = Partial.obs,  aes(y = Obs,  x = TREATMENT), color = 'red',
               position = position_nudge(x = 0.1)) +
    geom_point(data = tobacco,  aes(y = NUMBER,  x = TREATMENT),
               position = position_nudge(x = 0.05))


## ----summariseModel2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
tobacco.brm3 %>% summary()


## ----summariseModel2a1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, echo=FALSE----
tobacco.sum <- summary(tobacco.brm3)


## ----summariseModel2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
tobacco.brm3$fit %>%
    tidyMCMC(estimate.method = 'median',
             conf.int = TRUE,  conf.method = 'HPDinterval',
             rhat = TRUE, ess = TRUE)

## ----summariseModel2b1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
tobacco.tidy <- tidyMCMC(tobacco.brm3$fit, estimate.method='median',
                         conf.int=TRUE,  conf.method='HPDinterval',
                         rhat=TRUE, ess=TRUE)


## ----summariseModel2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
tobacco.brm3 %>% get_variables()
tobacco.draw <- tobacco.brm3 %>%
    gather_draws(`b.Intercept.*|b_TREAT.*`,  regex=TRUE)
tobacco.draw


## ----summariseModel2c1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
tobacco.draw %>% median_hdci


## ----summariseModel2c3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
tobacco.gather <- tobacco.brm3 %>% gather_draws(`b_Intercept.*|b_TREAT.*`,  regex=TRUE) %>%
  median_hdci


## ----summariseModel2c4, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
tobacco.brm3 %>%
    gather_draws(`b_Intercept.*|b_TREAT.*`, regex=TRUE) %>% 
    ggplot() +
    geom_vline(xintercept=0, linetype='dashed') +
    stat_slab(aes(x = .value, y = .variable,
                  fill = stat(ggdist::cut_cdf_qi(cdf,
                           .width = c(0.5, 0.8, 0.95), 
                           labels = scales::percent_format())
                           )), color='black') + 
    scale_fill_brewer('Interval', direction = -1, na.translate = FALSE) 

tobacco.brm3 %>% 
  gather_draws(`.Intercept.*|.*TREAT.*`, regex=TRUE) %>% 
  ggplot() + 
  stat_halfeye(aes(x=.value,  y=.variable)) +
  facet_wrap(~.variable, scales='free')

tobacco.brm3 %>% 
  gather_draws(`.Intercept.*|.*TREAT.*`, regex=TRUE) %>% 
  ggplot() + 
    stat_halfeye(aes(x=.value,  y=.variable)) +
    theme_classic()


## ----summariseModel2c5, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
tobacco.brm3 %>% 
  gather_draws(`b.Intercept.*|.*TREAT.*`, regex=TRUE) %>%
  group_by(.variable) %>%
  median_hdci

tobacco.brm3 %>%
    gather_draws(`b.Intercept.*|.*TREAT.*`, regex=TRUE) %>%
    ggplot() +
    geom_vline(xintercept=1, linetype='dashed') +
    stat_slab(aes(x = .value, y = .variable,
                  fill = stat(ggdist::cut_cdf_qi(cdf,
                           .width = c(0.5, 0.8, 0.95), 
                           labels = scales::percent_format())
                           )), color='black') + 
    scale_fill_brewer('Interval', direction = -1, na.translate = FALSE) 


## ----summariseModel2j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
tobacco.brm3$fit %>% plot(type='intervals') 


## ----summariseModel2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
tobacco.brm3 %>% tidy_draws()


## ----summariseModel2e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
tobacco.brm3 %>% spread_draws(`.*Intercept.*|.*TREAT.*`,  regex=TRUE)


## ----summariseModel2f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
tobacco.brm3 %>% posterior_samples() %>% as_tibble()


## ----summariseModel2g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
tobacco.brm3 %>%
    bayes_R2(re.form = NA, summary=FALSE) %>%
    median_hdci
tobacco.brm3 %>%
    bayes_R2(re.form = ~(1|LEAF), summary=FALSE) %>%
    median_hdci
tobacco.brm3 %>%
    bayes_R2(re.form = ~(TREATMENT|LEAF), summary=FALSE) %>%
    median_hdci


## ----predictions2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
newdata <- tobacco.brm3 %>%
    emmeans(~TREATMENT) %>%
    gather_emmeans_draws() %>%
    pivot_wider(names_from=TREATMENT,values_from=.value) %>%
    mutate(Eff = Strong - Weak,
           PEff = 100*Eff/Weak)
newdata %>% median_hdci(PEff)
newdata %>% summarise(P = sum(PEff>0)/n())
newdata %>% summarise(P = sum(PEff>20)/n())
newdata %>% hypothesis('PEff>20')

newdata <- tobacco.brm2 %>% emmeans(~TREATMENT) %>% as.data.frame
head(newdata)
ggplot(newdata, aes(y=emmean, x=TREATMENT)) +
    geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD))



## ----fitModel, results='markdown', eval=FALSE, hidden=TRUE, echo=FALSE--------
## 
## ggplot(tobacco, aes(y=NUMBER,x=TREATMENT)) + geom_boxplot()
## ggplot(tobacco, aes(y=NUMBER,x=as.numeric(LEAF), linetype=TREATMENT)) + geom_line()
## ggplot(tobacco, aes(y=NUMBER,x=TREATMENT, group=LEAF)) + geom_line()
## 
## tobacco.rstanarm <- stan_glmer(NUMBER~(1|LEAF)+TREATMENT, data=tobacco,
##                            family=gaussian,refresh=0,
##                             prior_PD=TRUE,
##                            chains=3, iter=5000, warmup=2000, thin=5)
## prior_summary(tobacco.rstanarm)
## ## decov is prior on covariance
## 
## tobacco.rstanarm <- stan_glmer(NUMBER~(1|LEAF)+TREATMENT, data=tobacco,
##                             family=gaussian,refresh=0,
##                             prior_intercept=normal(35, 10, autoscale=FALSE),
##                             prior=normal(0, 10, autoscale=FALSE),
##                             prior_aux=rstanarm::exponential(0.1, autoscale=FALSE),
##                             prior_covariance=decov(1, 1, 1, 1),
##                             prior_PD=TRUE,
##                            chains=3, iter=5000, warmup=2000, thin=5)
## 
## prior_summary(tobacco.rstanarm)
## 
## ggpredict(tobacco.rstanarm) %>% plot(add.data=TRUE)
## 
## tobacco.rstanarm <- update(tobacco.rstanarm, prior_PD=FALSE)
## 
## ggpredict(tobacco.rstanarm) %>% plot(add.data=TRUE)
## 
## posterior_vs_prior(tobacco.rstanarm, color_by='vs', group_by=TRUE,
##                    facet_args=list(scales='free_y'))
## 
## 
## 
## tobacco.rstanarm1 <- stan_glmer(NUMBER~(TREATMENT|LEAF)+TREATMENT, data=tobacco,
##                             family=gaussian,refresh=0,
##                             prior_PD=TRUE,
##                            chains=3, iter=5000, warmup=2000, thin=5)
## prior_summary(tobacco.rstanarm1)
## 
## ggpredict(tobacco.rstanarm1) %>% plot(add.data=TRUE)
## 
## tobacco.rstanarm1 <- stan_glmer(NUMBER~(TREATMENT|LEAF)+TREATMENT, data=tobacco,
##                             family=gaussian,refresh=0,
##                             prior_intercept=normal(35, 10, autoscale=FALSE),
##                             prior=normal(0, 10, autoscale=FALSE),
##                             prior_aux=rstanarm::exponential(0.1, autoscale=FALSE),
##                             prior_covariance=decov(1, 1, 1, 1),
##                             prior_PD=TRUE,
##                            chains=3, iter=5000, warmup=2000, thin=5)
## 
## tobacco.rstanarm1 <- stan_glmer(NUMBER~(TREATMENT|LEAF)+TREATMENT, data=tobacco,
##                             family=gaussian,refresh=0,
##                             prior_intercept=normal(35, 10, autoscale=FALSE),
##                             prior=normal(0, 10, autoscale=FALSE),
##                             prior_aux=rstanarm::exponential(0.1, autoscale=FALSE),
##                             prior_covariance=decov(1, 1, 1, 1),
##                             prior_PD=TRUE,
##                             chains=3, iter=5000, warmup=2000, thin=5,
##                             adapt_delta=0.99)
## 
## ggpredict(tobacco.rstanarm1) %>% plot(add.data=TRUE)
## tobacco.rstanarm1 <- update(tobacco.rstanarm1, prior_PD=FALSE)
## tobacco.rstanarm1 <- update(tobacco.rstanarm1, prior_PD=FALSE, adapt_delta=0.99)
## 
## ggpredict(tobacco.rstanarm1) %>% plot(add.data=TRUE)
## 
## posterior_vs_prior(tobacco.rstanarm1, color_by='vs', group_by=TRUE,
##                    facet_args=list(scales='free_y'))
## 
## 
## plot(tobacco.rstanarm1,  'mcmc_trace')
## plot(tobacco.rstanarm1,  'mcmc_acf_bar')
## plot(tobacco.rstanarm1,  'mcmc_rhat_hist')
## plot(tobacco.rstanarm1,  'mcmc_neff_hist')
## 
## pp_check(tobacco.rstan)
## pp_check(tobacco.rstan, group='TREATMENT', plotfun='violin_grouped')
## 
## ## tobacco.rstan1 <- stan_glmer(NUMBER~(TREATMENT|LEAF)+TREATMENT, data=tobacco,
## ##                            family=gaussian,refresh=0,
## ##                            chains=3, iter=5000, warmup=2000, thin=5)
## 
## ## tobacco.rstan1 <- stan_glmer(NUMBER~(TREATMENT|LEAF)+TREATMENT, data=tobacco,
## ##                            family=gaussian,refresh=0,
## ##                            chains=3, iter=5000, warmup=2000, thin=5,
## ##                            adapt_delta=0.99)
## 
## ## plot(tobacco.rstan1,  'mcmc_trace')
## ## plot(tobacco.rstan1,  'mcmc_acf_bar')
## ## plot(tobacco.rstan1,  'mcmc_rhat_hist')
## ## plot(tobacco.rstan1,  'mcmc_neff_hist')
## 
## 
## preds <- posterior_predict(tobacco.rstanarm1,  nsamples=250,  summary=FALSE)
## tobacco.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = tobacco$NUMBER,
##                             fittedPredictedResponse = apply(preds, 2, median),
##                             integerResponse=FALSE)
## plot(tobacco.resids)
## 
## (l.1 <- loo(tobacco.rstanarm))
## (l.2 <- loo(tobacco.rstanarm1))
## loo_compare(l.1, l.2)
## 
## preds <- posterior_predict(tobacco.rstanarm,  nsamples=250,  summary=FALSE)
## tobacco.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = tobacco$NUMBER,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(tobacco.resids)
## 
## 
## ggpredict(tobacco.rstanarm) %>% plot
## ##ggemmeans(tobacco.rstan, ~TREATMENT) %>% plot
## 
## summary(tobacco.rstanarm)
## tidyMCMC(tobacco.rstanarm$stanfit,conf.int=TRUE, conf.method='HPDinterval',
##          rhat=TRUE, ess=TRUE, estimate.method='median') %>%
##   data.frame
## 
## bayes_R2(tobacco.rstanarm, re.form=NA) %>% median_hdci
## bayes_R2(tobacco.rstanarm, re.form=~(1|LEAF)) %>% median_hdci
## 
## newdata = emmeans(tobacco.rstanarm, ~TREATMENT) %>%
##     gather_emmeans_draws() %>%
##     spread(key=TREATMENT, value=.value)
## newdata
## newdata = newdata %>% mutate(Eff = Strong-Weak,
##                    PEff = 100*(Strong-Weak)/Weak)
## newdata %>% dplyr::select(Eff,PEff) %>% median_hdci
## newdata %>% summarize(Prob = sum(PEff>0)/n())
## newdata %>% summarize(Prob = sum(PEff>10)/n())
## hypothesis(newdata,  'PEff>0')
## hypothesis(newdata,  'PEff>10')
## 
## 
## newdata = emmeans(tobacco.rstanarm, ~TREATMENT) %>% as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=emmean, x=TREATMENT)) +
##     geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD))


## ----fitModel.brms, results='markdown', eval=FALSE, hidden=TRUE, echo=FALSE----
## tobacco = tobacco %>% mutate(LEAF=factor(LEAF),
##                              TREATMENT=factor(TREATMENT))
## head(tobacco)
## 
## ggplot(tobacco, aes(y=NUMBER,x=TREATMENT)) + geom_boxplot()
## ggplot(tobacco, aes(y=NUMBER,x=as.numeric(LEAF), linetype=TREATMENT)) + geom_line()
## ggplot(tobacco, aes(y=NUMBER,x=as.numeric(LEAF), linetype=TREATMENT)) + geom_blank(aes(x=LEAF)) + geom_line()
## 
## tobacco.form <- bf(NUMBER~(1|LEAF)+TREATMENT,  family=gaussian)
## 
## tobacco.brms <- brm(tobacco.form, data=tobacco,
##                            refresh=0,
##                            chains=3, iter=5000, warmup=2000, thin=5)
## tobacco.brms <- brm(tobacco.form, data=tobacco,
##                            refresh=0,
##                     chains=3, iter=5000, warmup=2000, thin=5,
##                     control=list(adapt_delta=0.99))
## 
## 
## mcmc_plot(tobacco.brms,  type='trace')
## mcmc_plot(tobacco.brms,  type='acf_bar')
## mcmc_plot(tobacco.brms,  type='rhat_hist')
## mcmc_plot(tobacco.brms,  type='neff_hist')
## 
## tobacco.form <- bf(NUMBER~(TREATMENT|LEAF)+TREATMENT,  family=gaussian)
## tobacco.brms1 <- brm(tobacco.form, data=tobacco,
##                      refresh=0,
##                      chains=3, iter=5000, warmup=2000, thin=5,
##                      control=list(adapt_delta=0.99))
## 
## mcmc_plot(tobacco.brms1,  type='trace')
## mcmc_plot(tobacco.brms1,  type='acf_bar')
## mcmc_plot(tobacco.brms1,  type='rhat_hist')
## mcmc_plot(tobacco.brms1,  type='neff_hist')
## 
## (l.1 <- loo(tobacco.brms))
## (l.2 <- loo(tobacco.brms1))
## loo_compare(l.1, l.2)
## 
## 
## preds <- posterior_predict(tobacco.brms,  nsamples=250,  summary=FALSE)
## tobacco.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = tobacco$NUMBER,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(tobacco.resids)
## 
## preds <- posterior_predict(tobacco.brms1,  nsamples=250,  summary=FALSE)
## tobacco.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = tobacco$NUMBER,
##                             fittedPredictedResponse = apply(preds, 2, median),
##                             integerResponse = TRUE)
## plot(tobacco.resids)
## 
## 
## ggpredict(tobacco.brms1) %>% plot
## ##ggemmeans(tobacco.rstan, ~TREATMENT) %>% plot
## 
## summary(tobacco.brms)
## tidyMCMC(tobacco.brms1$fit,
##          conf.int=TRUE, conf.method='HPDinterval',
##          rhat=TRUE, ess=TRUE,
##          estimate.method='median')
## 
## bayes_R2(tobacco.brms1, re.form=~(1|LEAF), summary=FALSE) %>% median_hdi
## bayes_R2(tobacco.brms, re.form=~(1|LEAF), summary=FALSE) %>% median_hdi
## bayes_R2(tobacco.brms1, re.form=~(TREATMENT|LEAF), summary=FALSE) %>% median_hdi
## bayes_R2(tobacco.brms1, re.form=NA,  summary=FALSE) %>% median_hdi
## bayes_R2(tobacco.brms1, re.form=NA,  summary=FALSE) %>% median_hdi
## 
## newdata = emmeans(tobacco.brms1, ~TREATMENT) %>%
##     gather_emmeans_draws() %>%
##     spread(key=TREATMENT, value=.value)
## newdata
## newdata = newdata %>% mutate(Eff = Strong-Weak,
##                    PEff = 100*(Strong-Weak)/Weak)
## newdata %>% dplyr::select(Eff,PEff) %>% median_hdi
## newdata %>% summarize(Prob = sum(PEff>0)/n())
## newdata %>% summarize(Prob = sum(PEff>20)/n())
## hypothesis(newdata,  'PEff>0')
## newdata %>% summarize(Prob = sum(PEff>50)/n())
## hypothesis(newdata,  'PEff>20')
## 
## 
## newdata = emmeans(tobacco.brms1, ~TREATMENT) %>% as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=emmean, x=TREATMENT)) +
##     geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD))

