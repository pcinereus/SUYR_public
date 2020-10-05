## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE)


## ----libraries, results='markdown', eval=TRUE---------------------------------
library(rstanarm)   #for fitting models in STAN
library(brms)       #for fitting models in STAN
library(coda)       #for diagnostics
library(bayesplot)  #for diagnostics
library(DHARMa)     #for residual diagnostics
library(rstan)      #for interfacing with STAN
library(emmeans)    #for marginal means etc
library(broom)      #for tidying outputs
library(tidybayes)  #for more tidying outputs
library(ggeffects)  #for partial plots
library(tidyverse)  #for data wrangling etc
theme_set(theme_grey()) #put the default ggplot theme back


## ----readData, results='markdown', eval=TRUE----------------------------------
polis = read_csv('../data/polis.csv', trim_ws=TRUE)
glimpse(polis)
head(polis)
str(polis)


## ----lm, results='markdown', eval=TRUE, hidden=TRUE---------------------------
summary(glm(PA ~ RATIO, data=polis, family=binomial()))


## ----stan, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------------
## Use the default priors which are designed to be weakly informative.
## provide moderate regularlization and help stabilize computation
polis.rstanarm = stan_glm(PA ~ RATIO, data=polis, family=binomial(),
						iter = 5000, warmup = 1000,
                        chains = 3, thin = 5, refresh = 0)
prior_summary(polis.rstanarm)
polis.rstanarmopt = stan_glm(PA ~ RATIO, data=polis, family=binomial(),
						algorithm='optimizing')
prior_summary(polis.rstanarmopt)
summary(polis.rstanarmopt)

polis.rstanarm = stan_glm(PA ~ RATIO, data=polis, family=binomial(),
                          iter = 5000, warmup = 2000,
                          chains = 3, thin = 5, refresh = 0,
                         prior_intercept = normal(0, 10,autoscale=FALSE),
                         prior = normal(0, 1, autoscale=FALSE))
prior_summary(polis.rstanarm)


## ----stan.brms, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE--------
## start with defaults for all - perhaps use refresh=0
polis.form <- bf(PA|trials(1) ~ RATIO,  family=binomial())
get_prior(polis.form,  data=polis)
polis$PA
mad(polis$PA)
p=mean(polis$PA)
log(p/(1-p))
sd(polis$PA)
polis.priors <- c(
  prior(normal(0, 10), class='Intercept'),
  prior(normal(0, 1), class='b')
)

polis.samples = brm(polis.form,
                    data=polis,
                    prior=polis.priors,
                   sample_prior = 'only')
conditional_effects(polis.samples) %>% plot(points=TRUE)

polis.brm = brm(polis.form,
                data=polis,
                prior=polis.priors,
                sample_prior = TRUE,
                iter=5000, warmup=2000, chains=3,
                thin=5)
conditional_effects(polis.brm) %>% plot(points=TRUE)


## ----modelValidation, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
posterior_vs_prior(polis.rstanarm, color_by='vs', group_by=TRUE,
                   facet_args=list(scales='free_y'))



## ----modelValidation.brms, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
posterior_summary(polis.brm)
mcmc_plot(polis.brm,  pars='prior.*|b.*|sigma')


## ----modelValidation2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
stan_trace(polis.rstanarm)
stan_dens(polis.rstanarm,separate_chains = TRUE)
stan_ess(polis.rstanarm)

polis.rstanarm = stan_glm(PA ~ RATIO, data=polis, family=binomial(),
                          iter = 5000, warmup = 500,
                          chains = 3, thin = 4, refresh = 0,
                         prior_intercept = normal(0, 10,autoscale=FALSE),
                         prior = normal(0, 1, autoscale=FALSE))
stan_ac(polis.rstanarm) 
stan_rhat(polis.rstanarm) 
stan_ess(polis.rstanarm)

#pp_check(polis.rstanarm)
##available_ppc()
#pp_check(polis.rstanarm, plotfun='scatter')
pp_check(polis.rstanarm, plotfun='error_scatter_avg')
pp_check(polis.rstanarm, x=polis$RATIO,plotfun='error_scatter_avg_vs_x')
#pp_check(polis.rstanarm, x=polis$RATIO,plotfun='ribbon')
pp_check(polis.rstanarm, x=polis$RATIO,plotfun='intervals')

#library(shinystan)
                                        #launch_shinystan(fert.rstanarm)

## DHARMa residuals
preds <- posterior_predict(polis.rstanarm,  nsamples=250,  summary=FALSE)
polis.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = polis$PA,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = 'gaussian')
plot(polis.resids)


fit =fitted(polis.rstanarm)
resid=residuals(polis.rstanarm)
ggplot(data=NULL) + geom_point(aes(y=resid, x=fit))

## Partial plot
ggpredict(polis.rstanarm) %>% plot
#polis.grid = with(polis, data.frame(RATIO=seq(min(polis$RATIO, na.rm=TRUE), max(polis$RATIO, na.rm=TRUE), len=1000)))
ggemmeans(polis.rstanarm, ~RATIO) %>% plot



## ----modelValidation2.brms, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
plot(polis.brm)
                                        #mcmc_plot(polis.brms)

mcmc_plot(polis.brm, type='acf_bar')
mcmc_plot(polis.brm,  type='combo')
mcmc_plot(polis.brm, type='rhat_hist')
mcmc_plot(polis.brm, type='neff_hist')
preds <- posterior_predict(polis.brm,  nsamples=250,  summary=FALSE)
polis.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = polis$PA,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = 'gaussian')
plot(polis.resids)



## ----Samples, results='markdown', eval=TRUE, hidden=TRUE----------------------
polis.mcmc = as.matrix(polis.rstanarm)
nrow(polis.mcmc)
head(polis.mcmc)


## ----modelSummary, results='markdown', eval=TRUE, hidden=TRUE-----------------
summary(polis.rstanarm)
tidyMCMC(polis.rstanarm$stanfit, conf.int=TRUE, conf.method='HPDinterval',
         rhat=TRUE, ess=TRUE)

posterior_interval(polis.rstanarm, prob=0.95)

polis.r2 = bayes_R2(polis.rstanarm)
hist(polis.r2)
tidyMCMC(as.mcmc(bayes_R2(polis.rstanarm)), conf.int=TRUE)


## ----modelSummary.brms, results='markdown', eval=TRUE, hidden=TRUE------------
summary(polis.brm)
tidyMCMC(polis.brm$fit, conf.int=TRUE, conf.method='HPDinterval',
         rhat=TRUE, ess=TRUE)
posterior_interval(polis.brm, prob=0.95)

## To express this on odds (odds/ratio) scale
polis.brm %>% get_variables
polis.brm %>% gather_draws(`b.*`,  regex=TRUE) %>%
  group_by(.variable) %>%
  mutate(.value=exp(.value)) %>%
  median_hdci

polis.r2 = bayes_R2(polis.brm)
hist(polis.r2)
tidyMCMC(as.mcmc(bayes_R2(polis.rstanarm)), conf.int=TRUE)


## ----summaryFig, results='markdown', eval=TRUE, hidden=TRUE-------------------
polis.list = with(polis, list(RATIO = seq(min(RATIO), max(RATIO), len=100)))
newdata = emmeans(polis.rstanarm, ~RATIO, at=polis.list, type='response') %>% as.data.frame
head(newdata)

ggplot(newdata, aes(y=prob, x=RATIO)) + 
geom_point(data=polis, aes(y=PA)) +
geom_line() + 
geom_ribbon(aes(ymin=lower.HPD, ymax=upper.HPD), fill='blue', alpha=0.3) +
scale_y_continuous('PA') +
scale_x_continuous('RATIO') +
    theme_classic()


## ld50
polis.mcmc = polis.rstanarm %>%  posterior_samples()
polis.mcmc %>%  head()
as.vector(-polis.mcmc[,1]/polis.mcmc[,2] ) %>% median_hdi
hist(-polis.mcmc[,1]/polis.mcmc[,2] )


## ----summaryFig.brms, results='markdown', eval=TRUE, hidden=TRUE--------------
polis.list = with(polis, list(RATIO = seq(min(RATIO), max(RATIO), len=100)))
newdata = emmeans(polis.brm, ~RATIO, at=polis.list, type='response') %>% as.data.frame
head(newdata)

ggplot(newdata, aes(y=prob, x=RATIO)) + 
geom_point(data=polis, aes(y=PA)) +
geom_line() + 
geom_ribbon(aes(ymin=lower.HPD, ymax=upper.HPD), fill='blue', alpha=0.3) +
scale_y_continuous('PA') +
scale_x_continuous('RATIO') +
    theme_classic()


## ld50
polis.mcmc = polis.brm %>%  posterior_samples()
as.vector(-polis.mcmc[,1]/polis.mcmc[,2] ) %>% median_hdi
hist(-polis.mcmc[,1]/polis.mcmc[,2] )

