## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE,cache.lazy = FALSE, tidy='styler')


## ----libraries, results='markdown', eval=TRUE---------------------------------
library(rstanarm)   #for fitting models in STAN
library(brms)       #for fitting models in STAN
library(standist)   #for exploring distributions
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
fert <- read_csv('../data/fertilizer.csv', trim_ws = TRUE)
glimpse(fert)


## ----lm, results='markdown', eval=TRUE, hidden=TRUE---------------------------
summary(lm(YIELD ~ FERTILIZER, data = fert))
summary(lm(YIELD ~ scale(FERTILIZER, scale=FALSE), data = fert))


## ----fitModel1a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
fert.rstanarm <- stan_glm(YIELD ~ FERTILIZER,
                          data=fert,
                          iter = 5000,
                          warmup = 1000,
                          chains = 3,
                          thin = 5,
                          refresh = 0)


## ----fitModel1b, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
prior_summary(fert.rstanarm)


## ----fitModel1c, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
mean(fert$YIELD)
sd(fert$YIELD)
2.5*sd(fert$YIELD)


## ----fitModel1d, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
2.5*sd(fert$YIELD)/sd(fert$FERTILIZER)


## ----fitModel1e, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
1/sd(fert$FERTILIZER)


## ----fitModel1f, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
fert.rstanarm1 <- update(fert.rstanarm,  prior_PD=TRUE)


## ----fitModel1g, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
ggpredict(fert.rstanarm1) %>% plot(add.data=TRUE)


## ----fitModel1h, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
fert.rstanarm2= stan_glm(YIELD ~ FERTILIZER, data=fert,
                         prior_intercept = normal(164, 10, autoscale=FALSE),
                         prior = normal(0, 1, autoscale=FALSE),
                         prior_aux = cauchy(0, 2, autoscale=FALSE),
                         prior_PD=TRUE, 
                         iter = 5000, warmup = 1000,
                         chains = 3, thin = 5, refresh = 0
                         )


## ----fitModel1i, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
ggpredict(fert.rstanarm2) %>%
  plot(add.data=TRUE)


## ----fitModel1j, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
fert.rstanarm3= update(fert.rstanarm2,  prior_PD=FALSE) 


## ----modelFit1k, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
posterior_vs_prior(fert.rstanarm3, color_by='vs', group_by=TRUE,
                   facet_args=list(scales='free_y'))


## ----modelFit1l, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggpredict(fert.rstanarm3) %>% plot(add.data=TRUE)


## ----fitModel2a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
fert.brm <- brm(bf(YIELD ~ FERTILIZER),
               data = fert,
               iter = 5000,
               warmup = 1000,
               chains = 3,
               thin = 5,
               refresh = 0)


## ----fitModel2a2, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE-----
fert.brm %>% names


## ----fitModel2b, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE, paged.print=FALSE,tidy.opts = list(width.cutoff = 80), echo=2----
options(width=100)
prior_summary(fert.brm)
options(width=80)


## ----fitModel2c, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
median(fert$YIELD)
mad(fert$YIELD)


## ----student_t_dist, results='markdown', eval=TRUE----------------------------
standist::visualize("student_t(3,161.5,90.4)", xlim=c(-100,1000))


## ----student_t_dist1, results='markdown', eval=TRUE---------------------------
standist::visualize("student_t(3,0,90.4)", xlim=c(-10,500))


## ----normal_prior, results='markdown', eval=TRUE------------------------------
standist::visualize("normal(0, 10)", xlim = c(-50,50))
standist::visualize("normal(0, 0.1)", xlim = c(-1,1))


## ----fitModel2d, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
fert.brm1 <- brm(bf(YIELD ~ FERTILIZER),
                 data = fert,
                 prior = prior(normal(0, 10), class = 'b'), 
                 sample_prior = 'only', 
                 iter = 5000,
                 warmup = 1000,
                 chains = 3,
                 thin = 5,
                 refresh = 0)


## ----fitModel2e, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
fert.brm1 %>% ggpredict() %>% plot(add.data=TRUE)
fert.brm1 %>% ggemmeans(~FERTILIZER) %>% plot(add.data=TRUE)
fert.brm1 %>% conditional_effects() %>%  plot(points=TRUE)


## ----priors, results='markdown', eval=TRUE------------------------------------
standist::visualize("normal(164, 10)", xlim = c(-100, 300))
standist::visualize("normal(0, 1)", xlim = c(-10, 10))
standist::visualize("cauchy(0, 2)", xlim = c(-10, 10))
standist::visualize("gamma(2, 1)", xlim = c(0, 10))
standist::visualize(
              "student_t(3, 0, 90.4)",
              "cauchy(0, 2)",
              "gamma(2, 1)",
              "gamma(2, 0.5)",
              "gamma(2, 0.2)",
              xlim = c(0,20))


## ----fitModel2h, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
priors <- prior(normal(164, 10),  class='Intercept') +
    prior(normal(0, 1), class='b') +
    prior(gamma(2, 1),  class='sigma')

fert.brm2 = brm(bf(YIELD ~ FERTILIZER),
                data=fert,
                prior=priors,
                sample_prior = 'only', 
                iter = 5000,
                warmup = 1000,
                chains = 3,
                thin = 5,
                refresh = 0)


## ----fitModel2i, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
fert.brm2 %>%
    ggemmeans(~FERTILIZER) %>%
    plot(add.data = TRUE)
#OR
fert.brm2 %>%
    conditional_effects() %>%
    plot(points = TRUE)


## ----fitModel2j, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
fert.brm3 <- update(fert.brm2, sample_prior = 'yes', refresh = 0)


## ----posterior2, results='markdown', eval=TRUE--------------------------------
fert.brm3 %>% get_variables()
## fert.brm3 %>% hypothesis('Intercept=0', class='b') %>% plot
fert.brm3 %>% hypothesis('FERTILIZER = 0') %>% plot
## fert.brm3 %>% hypothesis('scaleFERTILIZERscaleEQFALSE=0') %>% plot
fert.brm3 %>% hypothesis('sigma = 0', class = '') %>% plot


## ----fitModel2k, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
fert.brm3 %>% get_variables()
fert.brm3 %>%
  posterior_samples %>%
  select(-`lp__`) %>%
  pivot_longer(cols=everything(), names_to='key', values_to='value') %>% 
  mutate(Type=ifelse(str_detect(key, 'prior'), 'Prior', 'b'),
         Class=ifelse(str_detect(key, 'Intercept'),  'Intercept',
               ifelse(str_detect(key, 'sigma'),  'Sigma',  'b'))) %>%
  ggplot(aes(x=Type,  y=value)) +
  stat_pointinterval()+
  facet_wrap(~Class,  scales='free')


## ----fitModel2l, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
fert.brm3 %>% standata()
fert.brm3 %>% stancode()


## ----modelValidation1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
available_mcmc()


## ----modelValidation1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(fert.rstanarm3, plotfun='mcmc_trace')


## ----modelValidation1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(fert.rstanarm3, 'acf_bar')


## ----modelValidation1d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(fert.rstanarm3, 'rhat_hist')


## ----modelValidation1e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(fert.rstanarm3, 'neff_hist')


## ----Validation1f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(fert.rstanarm3, 'combo')
plot(fert.rstanarm3, 'violin')


## ----modelValidation1g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_trace(fert.rstanarm3)


## ----modelValidation1h, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_ac(fert.rstanarm3) 


## ----modelValidation1i, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_rhat(fert.rstanarm3) 


## ----modelValidation1j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_ess(fert.rstanarm3)


## ----modelValidation1k, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_dens(fert.rstanarm3, separate_chains = TRUE)


## ----modelValidation1l, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
fert.ggs <- ggs(fert.rstanarm3)
ggs_traceplot(fert.ggs)


## ----modelValidation1m, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_autocorrelation(fert.ggs)


## ----modelValidation1n, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_Rhat(fert.ggs)


## ----modelValidation1o, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_effective(fert.ggs)


## ----modelValidation1p, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_crosscorrelation(fert.ggs)


## ----modelValidation1q, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
ggs_grb(fert.ggs)


## ----modelValidation2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
available_mcmc()


## ----modelValidation2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
fert.brm3 %>% mcmc_plot(type = 'combo')
fert.brm3 %>% mcmc_plot(type = 'trace')
fert.brm3 %>% mcmc_plot(type = 'dens_overlay')


## ----modelValidation2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
fert.brm3 %>% mcmc_plot(type = 'acf_bar')


## ----modelValidation2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
fert.brm3 %>% mcmc_plot(type = 'rhat_hist')


## ----modelValidation2e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
fert.brm3 %>% mcmc_plot(type = 'neff_hist')


## ----modelValidation2f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
fert.brm3 %>% mcmc_plot(type = 'combo')
fert.brm3 %>% mcmc_plot(type = 'violin')


## ----modelValidation2g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
fert.brm3$fit %>% stan_trace()
fert.brm3$fit %>% stan_trace(inc_warmup = TRUE)


## ----modelValidation2h, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
fert.brm3$fit %>% stan_ac() 


## ----modelValidation2i, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
fert.brm3$fit %>% stan_rhat() 


## ----modelValidation2j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
fert.brm3$fit %>% stan_ess()


## ----modelValidation2k, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
fert.brm3$fit %>% stan_dens(separate_chains = TRUE)


## ----modelValidation2l, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=7----
fert.ggs <- fert.brm3$fit %>% ggs(inc_warmup = TRUE, burnin = TRUE)  # does not seem to ignore burnin?
fert.ggs %>% ggs_traceplot()
fert.ggs <- fert.brm3$fit %>% ggs(inc_warmup = FALSE, burnin = TRUE)  # does not seem to ignore burnin?
fert.ggs %>% ggs_traceplot()


## ----modelValidation2m, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=7----
fert.ggs %>% ggs_autocorrelation()


## ----modelValidation2n, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
fert.ggs %>% ggs_Rhat()


## ----modelValidation2o, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
fert.ggs %>% ggs_effective()


## ----modelValidation2p, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
fert.ggs %>% ggs_crosscorrelation()


## ----modelValidation2q, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
fert.ggs %>% ggs_grb()


## ----modelValidation3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
available_ppc()


## ----modelValidation3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(fert.rstanarm3,  plotfun='dens_overlay')


## ----modelValidation3c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(fert.rstanarm3, plotfun='error_scatter_avg')


## ----modelValidation3d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(fert.rstanarm3, x=fert$FERTILIZER, plotfun='error_scatter_avg_vs_x')


## ----modelValidation3e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(fert.rstanarm3, x=fert$FERTILIZER, plotfun='intervals')


## ----modelValidation3f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(fert.rstanarm3, x=fert$FERTILIZER,plotfun='ribbon')


## ----modelValidation3g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
#library(shinystan)
#launch_shinystan(fert.rstanarm3)


## ----modelValidation4a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
preds <- posterior_predict(fert.rstanarm3,  nsamples=250,  summary=FALSE)
fert.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = fert$YIELD,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = 'gaussian')
plot(fert.resids)


## ----modelValidation5a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
available_ppc()


## ----modelValidation5b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
fert.brm3 %>% pp_check( type='dens_overlay', nsamples=100)


## ----modelValidation5c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
fert.brm3 %>% pp_check(type='error_scatter_avg')


## ----modelValidation5d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
fert.brm3 %>% pp_check(x='FERTILIZER', type='error_scatter_avg_vs_x')


## ----modelValidation5e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
fert.brm3 %>% pp_check(x='FERTILIZER', type='intervals')


## ----modelValidation5f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
pp_check(fert.brm3, x='FERTILIZER',type='ribbon')


## ----modelValidation5g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
#library(shinystan)
#launch_shinystan(fert.brm3)


## ----modelValidation6a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
preds <- fert.brm3 %>% posterior_predict(nsamples=250,  summary=FALSE)
fert.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = fert$YIELD,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = FALSE)
fert.resids %>% plot()


## ----partialPlot1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.rstanarm3 %>% ggpredict() %>% plot(add.data=TRUE)


## ----partialPlot1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.rstanarm3 %>% ggemmeans(~FERTILIZER) %>% plot(add.data=TRUE)


## ----partialPlot1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.rstanarm3 %>% fitted_draws(newdata=fert) %>%
 median_hdci() %>%
 ggplot(aes(x=FERTILIZER, y=.value)) +
 geom_ribbon(aes(ymin=.lower, ymax=.upper), fill='blue', alpha=0.3) + 
 geom_line()


## ----partialPlot2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.brm3 %>% ggemmeans(~FERTILIZER) %>% plot(add.data = TRUE) 
fert.brm3 %>% conditional_effects() 
fert.brm3 %>% conditional_effects() %>% plot(points = TRUE)
fert.brm3 %>% conditional_effects(spaghetti = TRUE, nsamples = 200) %>% plot(points = TRUE) 


## ----partialPlot2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.brm3 %>% ggpredict() %>% plot(add.data=TRUE)


## ----partialPlot2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.brm3 %>% ggemmeans(~FERTILIZER) %>% plot(add.data=TRUE)


## ----partialPlot2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.brm3 %>%
    fitted_draws(newdata = fert) %>%
    median_hdci() %>%
    ggplot(aes(x = FERTILIZER, y=.value)) +
    geom_ribbon(aes(ymin = .lower, ymax = .upper), fill = 'blue', alpha = 0.3) + 
    geom_line()


## ----summariseModel1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.rstanarm3 %>% summary()


## ----summariseModel1a1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, echo=FALSE----
fert.sum <- summary(fert.rstanarm3)


## ----summariseModel1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
tidyMCMC(fert.rstanarm3$stanfit, estimate.method='median',  conf.int=TRUE,
         conf.method='HPDinterval',  rhat=TRUE, ess=TRUE)

## ----summariseModel1b1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
fert.tidy <- tidyMCMC(fert.rstanarm3$stanfit, estimate.method='median',  conf.int=TRUE,  conf.method='HPDinterval',  rhat=TRUE, ess=TRUE)


## ----summariseModel1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.draw <- fert.rstanarm3 %>% gather_draws(`(Intercept)`, FERTILIZER, sigma)
## OR via regex
fert.draw <- fert.rstanarm3 %>% gather_draws(`.Intercept.*|FERT.*|sigma`,  regex=TRUE)
fert.draw


## ----summariseModel1c1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.draw %>% median_hdci


## ----summariseModel1c3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
fert.gather <- fert.rstanarm3 %>% gather_draws(`(Intercept)`,FERTILIZER,sigma) %>%
  median_hdci


## ----summariseModel1c4, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
fert.rstanarm3 %>% 
  gather_draws(`(Intercept)`, FERTILIZER, sigma) %>% 
  ggplot() + 
  stat_halfeye(aes(x=.value,  y=.variable)) +
  facet_wrap(~.variable, scales='free')


## ----summariseModel1j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.rstanarm3 %>% plot(plotfun='mcmc_intervals') 


## ----summariseModel1d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.rstanarm3 %>% tidy_draws()


## ----summariseModel1e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.rstanarm3 %>% spread_draws(`(Intercept)`, FERTILIZER, sigma)
# OR via regex
fert.rstanarm3 %>% spread_draws(`.Intercept.*|FERT.*|sigma`,  regex=TRUE)


## ----summariseModel1f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.rstanarm3 %>% posterior_samples() %>% as_tibble()


## ----summariseModel1g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.rstanarm3 %>% bayes_R2() %>% median_hdci


## ----summariseModel1h, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
mcmcpvalue <- function(samp)
{
    ## elementary version that creates an empirical p-value for the
    ## hypothesis that the columns of samp have mean zero versus a
    ## general multivariate distribution with elliptical contours.
    
    ## differences from the mean standardized by the observed
    ## variance-covariance factor
    
    ## Note, I put in the bit for single terms
    if (length(dim(samp))==0) {
        std <- backsolve(chol(var(samp)),cbind(0, t(samp)) - mean(samp),transpose = TRUE)
        sqdist <- colSums(std * std)
        sum(sqdist[-1] > sqdist[1])/length(samp)
    }
    else {
        std <- backsolve(chol(var(samp)),cbind(0, t(samp)) - colMeans(samp),transpose = TRUE)
        sqdist <- colSums(std * std)
        sum(sqdist[-1] > sqdist[1])/nrow(samp)
    }
    
}

mcmcpvalue(as.matrix(fert.rstanarm3)[, c("FERTILIZER")])


## ----summariseModel2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.brm3 %>% summary()


## ----summariseModel2a1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, echo=FALSE----
fert.sum <- summary(fert.brm3)


## ----summariseModel2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.brm3$fit %>%
    tidyMCMC(estimate.method = 'median',
             conf.int = TRUE,
             conf.method = 'HPDinterval',
             rhat = TRUE, ess = TRUE)

## ----summariseModel2b1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
fert.tidy <- tidyMCMC(fert.brm3$fit, estimate.method='median',  conf.int=TRUE,  conf.method='HPDinterval',  rhat=TRUE, ess=TRUE)


## ----summariseModel2d1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.brm3 %>% tidy_draws()


## ----summariseModel2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.brm3 %>%
    tidy_draws() %>%
    gather_variables() %>%
    median_hdci()


## ----summariseModel2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.brm3 %>% get_variables()
fert.brm3 %>%
    gather_draws(b_Intercept, b_FERTILIZER, sigma) %>%
    median_hdci
## OR via regex
fert.brm3 %>%
    gather_draws(`b_.*|sigma`,  regex=TRUE) %>%
    median_hdci

## ----summariseModel2c1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
fert.gather <- fert.brm3 %>%
    gather_draws(b_Intercept, b_FERTILIZER, sigma) %>%
    median_hdci


## ----summariseModel2c2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
fert.brm3 %>% 
  gather_draws(b_Intercept, b_FERTILIZER, sigma) %>% 
  ggplot() + 
  stat_halfeye(aes(x = .value,  y = .variable)) +
  facet_wrap(~.variable, scales = 'free')

fert.brm3 %>% 
  gather_draws(b_Intercept, b_FERTILIZER, sigma) %>% 
  ggplot() + 
  stat_halfeye(aes(x = .value,  y = .variable)) 


## ----summariseModel2e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.brm3 %>% spread_draws(b_Intercept, b_FERTILIZER, sigma)
# OR via regex
fert.brm3 %>% spread_draws(`b_.*|sigma`,  regex=TRUE)


## ----summariseModel2j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.brm3 %>% mcmc_plot(type='intervals') 


## ----summariseModel2f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.brm3 %>% posterior_samples() %>% as_tibble()


## ----summariseModel2g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.brm3 %>% bayes_R2(summary=FALSE) %>% median_hdci


## ----summariseModel2h, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
mcmcpvalue <- function(samp)
{
    ## elementary version that creates an empirical p-value for the
    ## hypothesis that the columns of samp have mean zero versus a
    ## general multivariate distribution with elliptical contours.
    
    ## differences from the mean standardized by the observed
    ## variance-covariance factor
    
    ## Note, I put in the bit for single terms
    if (length(dim(samp))==0) {
        std <- backsolve(chol(var(samp)),cbind(0, t(samp)) - mean(samp),transpose = TRUE)
        sqdist <- colSums(std * std)
        sum(sqdist[-1] > sqdist[1])/length(samp)
    }
    else {
        std <- backsolve(chol(var(samp)),cbind(0, t(samp)) - colMeans(samp),transpose = TRUE)
        sqdist <- colSums(std * std)
        sum(sqdist[-1] > sqdist[1])/nrow(samp)
    }
    
}

mcmcpvalue(as.matrix(fert.brm3)[, c("b_FERTILIZER")])


## ----predictions0a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
## establish a data set that defines the new data to predict against
newdata = data.frame(FERTILIZER = 110)


## ----predictions1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.rstanarm3 %>% fitted_draws(newdata=newdata) %>%
  median_hdci


## ----predictions1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.rstanarm3 %>% emmeans(~FERTILIZER,  at=newdata)


## ----predictions1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.rstanarm3 %>% posterior_predict(newdata=newdata) %>%
  tidyMCMC(estimate.method='median', conf.int=TRUE, conf.method='HPDinterval')


## ----predictions1d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.rstanarm3 %>% posterior_epred(newdata=newdata) %>%
  tidyMCMC(estimate.method='median', conf.int=TRUE, conf.method='HPDinterval')


## ----predictions1e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.rstanarm3 %>% posterior_linpred(newdata=newdata) %>%
  tidyMCMC(estimate.method='median', conf.int=TRUE, conf.method='HPDinterval')


## ----predictions1f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
newdata %>% add_predicted_draws(fert.rstanarm3) %>%
  median_hdci


## ----predictions1g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
newdata %>% add_fitted_draws(fert.rstanarm3) %>%
  median_hdci


## ----predictions1h, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
## Establish an appropriate model matrix
Xmat = model.matrix(~FERTILIZER, data=newdata)
### get the posterior draws for the linear predictor
coefs <- fert.rstanarm3 %>% posterior_samples(pars=c('(Intercept)','FERTILIZER')) %>% as.matrix()
fit <- coefs %*% t(Xmat)
fit %>% median_hdci


## ----predictions2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.brm3 %>%
    fitted_draws(newdata = newdata) %>%
    median_hdci()
## OR
newdata %>%
    add_fitted_draws(fert.brm3) %>%
    median_hdci()


## ----predictions2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.brm3 %>%
    emmeans(~FERTILIZER, at = newdata)
## OR
fert.brm3 %>%
    emmeans(~FERTILIZER, at = newdata) %>%
    tidy_draws() %>%
    median_hdci()


## ----predictions2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.brm3 %>%
    posterior_predict(newdata = newdata) %>%
    as.mcmc() %>%
    tidyMCMC(estimate.method = 'median',
             conf.int = TRUE, conf.method = 'HPDinterval')


## ----predictions2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.brm3 %>%
    posterior_epred(newdata = newdata) %>%
    as.mcmc() %>%
    tidyMCMC(estimate.method = 'median',
             conf.int = TRUE, conf.method = 'HPDinterval')


## ----predictions2e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.brm3 %>%
    posterior_linpred(newdata = newdata) %>%
    as.mcmc() %>%
    tidyMCMC(estimate.method = 'median',
             conf.int = TRUE, conf.method = 'HPDinterval')


## ----predictions2f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
newdata %>%
    add_predicted_draws(fert.brm3) %>%
    median_hdci


## ----predictions2g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
newdata %>%
    add_fitted_draws(fert.brm3) %>%
    median_hdci


## ----predictions2h, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
## Establish an appropriate model matrix
Xmat = model.matrix(~FERTILIZER, data=newdata)
### get the posterior draws for the linear predictor
coefs <- fert.brm3 %>% posterior_samples(pars=c('b_Intercept','b_FERTILIZER')) %>% as.matrix()
fit <- coefs %*% t(Xmat)
fit %>% median_hdci


## ----Probability1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.rstanarm3 %>% hypothesis('FERTILIZER>0')


## ----Probability1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,paged.print=FALSE----
fert.rstanarm3 %>% tidy_draws() %>% summarise(P=sum(FERTILIZER>0)/n())


## ----Probability1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, echo=1:2----
newdata = list(FERTILIZER=c(200, 100)) 
fert.rstanarm3 %>% emmeans(~FERTILIZER,  at=newdata) %>% pairs()
fert.mcmc <- fert.rstanarm3 %>% emmeans(~FERTILIZER,  at=newdata) %>% pairs() %>%
  as.data.frame()


## ----Probability1d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.mcmc <- fert.rstanarm3 %>% emmeans(~FERTILIZER,  at=newdata) %>% 
  tidy_draws() %>%
  rename_with(~str_replace(., 'FERTILIZER ', 'p')) %>%
  mutate(Eff=p200 - p100,
         PEff=100*Eff/p100)
fert.mcmc %>% head


## ----Probability1e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.mcmc %>% tidyMCMC(estimate.method='median',
                       conf.int=TRUE, conf.method='HPDinterval')


## ----Probability1f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.mcmc %>% median_hdci(PEff)


## ----Probability1g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, paged.print=FALSE----
fert.mcmc %>% summarise(P=sum(PEff>50)/n())


## ----Probability1h, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.mcmc %>% hypothesis('PEff>50')


## ----Probability2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.brm3 %>% hypothesis('FERTILIZER > 0')
fert.brm3 %>% hypothesis('FERTILIZER > 0') %>% plot (ignore_prior= TRUE)
fert.brm3 %>% hypothesis('FERTILIZER > 0') %>% `[[`('samples') %>% median_hdci()


## ----Probability2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,paged.print=FALSE----
fert.brm3 %>%
    gather_draws(b_FERTILIZER) %>%
    summarise(P=sum(.value>0)/n())
#OR
fert.brm3 %>%
    tidy_draws() %>%
    summarise(P=sum(b_FERTILIZER>0)/n())


## ----Probability2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,paged.print=FALSE----
fert.brm3 %>% hypothesis('FERTILIZER>0.9')
fert.brm3 %>%
    hypothesis('FERTILIZER>0.9') %>%
    plot(ignore_prior = TRUE)
# This returns a list
fert.brm3 %>%
    hypothesis('FERTILIZER>0.9') %>%
    plot(ignore_prior = TRUE) %>%
    `[[`(1) +
    geom_vline(xintercept=0, linetype='dashed')

#OR
fert.brm3 %>%
    tidy_draws() %>%
    summarise(P=sum(b_FERTILIZER>0.9)/n())
fert.brm3 %>%
    tidy_draws() %>%
    ggplot(aes(x=b_FERTILIZER)) +
    geom_density(fill='orange') +
    geom_vline(xintercept=0.9, linetype='dashed')


## ----Probability2c1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, echo=1:2----
newdata <- list(FERTILIZER = c(200, 100)) 
fert.brm3 %>%
    emmeans(~FERTILIZER,  at = newdata) %>%
    pairs()
fert.mcmc <- fert.brm3 %>%
    emmeans(~FERTILIZER,  at = newdata) %>%
    pairs() %>%
    as.data.frame()


## ----Probability2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.mcmc <- fert.brm3 %>%
    emmeans(~FERTILIZER,  at = newdata) %>% 
    tidy_draws() %>%
    rename_with(~str_replace(., 'FERTILIZER ', 'p')) %>%
    mutate(Eff=p200 - p100,
           PEff=100*Eff/p100)
fert.mcmc %>% head



## ----Probability2e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.mcmc %>%
    tidyMCMC(estimate.method='median',
             conf.int=TRUE, conf.method='HPDinterval')


## ----Probability2f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.mcmc %>% median_hdci(PEff)


## ----Probability2ff, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.mcmc %>%
    ggplot() +
    geom_density(aes(x=PEff))


## ----Probability2g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, paged.print=FALSE----
fert.mcmc %>% summarise(P=sum(PEff>50)/n())


## ----Probability2h, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
fert.mcmc %>% hypothesis('PEff>50')


## ----summaryFig1a, results='markdown', eval=TRUE, hidden=TRUE-----------------
fert.list = with(fert, list(FERTILIZER = seq(min(FERTILIZER), max(FERTILIZER), len=100)))
newdata = emmeans(fert.rstanarm3, ~FERTILIZER, at=fert.list) %>% as.data.frame
head(newdata)

ggplot(newdata, aes(y=emmean, x=FERTILIZER)) + 
geom_point(data=fert, aes(y=YIELD)) +
geom_line() + 
geom_ribbon(aes(ymin=lower.HPD, ymax=upper.HPD), fill='blue', alpha=0.3) +
scale_y_continuous('YIELD') +
scale_x_continuous('FERTILIZER') +
theme_classic()

## spaghetti plot
newdata = emmeans(fert.rstanarm3, ~FERTILIZER, at=fert.list) %>%
  gather_emmeans_draws()
newdata %>% head
ggplot(newdata,  aes(y=.value,  x=FERTILIZER)) +
  geom_line(aes(group=.draw),  alpha=0.01) +
  geom_point(data=fert,  aes(y=YIELD))


## ----summaryFig2a, results='markdown', eval=TRUE, hidden=TRUE-----------------
fert.grid <- with(fert, list(FERTILIZER = modelr::seq_range(FERTILIZER, n = 100)))
newdata <- fert.brm3 %>% emmeans(~FERTILIZER, at=fert.grid) %>% as.data.frame
head(newdata)

ggplot(newdata, aes(y=emmean, x=FERTILIZER)) + 
    geom_point(data=fert, aes(y=YIELD)) +
    geom_line() + 
    geom_ribbon(aes(ymin=lower.HPD, ymax=upper.HPD), fill='blue', alpha=0.3) +
    scale_y_continuous('YIELD') +
    scale_x_continuous('FERTILIZER') +
    theme_classic()

## spaghetti plot
newdata = emmeans(fert.brm3, ~FERTILIZER, at = fert.grid) %>%
    gather_emmeans_draws()
newdata %>% head
ggplot(newdata,  aes(y = .value,  x = FERTILIZER)) +
    geom_line(aes(group = .draw),  color = 'blue', alpha = 0.01) +
    geom_point(data = fert,  aes(y = YIELD))
  

