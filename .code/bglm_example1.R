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
theme_set(theme_grey()) #put the default ggplot theme back


## ----readData, results='markdown', eval=TRUE----------------------------------
fert = read_csv('../data/fertilizer.csv', trim_ws=TRUE)
glimpse(fert)


## ----lm, results='markdown', eval=TRUE, hidden=TRUE---------------------------
summary(lm(YIELD ~ FERTILIZER, data=fert))
summary(lm(YIELD ~ scale(FERTILIZER, scale=FALSE), data=fert))


## ----stan, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------------
## Use the default priors which are designed to be weakly informative.
## provide moderate regularlization and help stabilize computation
fert.rstanarm = stan_glm(YIELD ~ FERTILIZER, data=fert,
                         iter = 5000, warmup = 1000,
                         chains = 3, thin = 5, refresh = 0)
prior_summary(fert.rstanarm)
sd(fert$YIELD)
## autoscale
## for Gaussian, beta_0, beta_n and sigma are multiplied by sd(y)

## We can sample only from the prior predictive distribution (and not condition on the observed data)
## and explore the predictions from this in order to see the impacts of the priors
fert.rstanarm1 <- update(fert.rstanarm,  prior_PD=TRUE)
ggpredict(fert.rstanarm1) %>% plot


## ----stan.brms, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE--------
## start with defaults for all - perhaps use refresh=0
fert.brms = brm(YIELD ~ FERTILIZER,  data=fert)
## The above will automatically center all continuous predictors
#fert.brms = brm(YIELD ~ scale(FERTILIZER),  data=fert)
prior_summary(fert.brms)
## Note,  population-effects ('fixed effects') by default have inproper (flat) priors
## over the reals
## 'Random' effects are are often called 'group-level' or 'varying' effects.
median(fert$YIELD)
mad(fert$YIELD)
# Note the above are applied on the link scale..
standata(fert.brms)
stancode(fert.brms)
fert.samples = brm(YIELD ~ FERTILIZER, data=fert,
                   prior=prior(normal(0, 10), class='b'), 
                   sample_prior = 'only')
conditional_effects(fert.samples) %>% plot(points=TRUE)



## ----stan1, results='markdown', eval=FALSE, hidden=TRUE, cache=TRUE-----------
## fert.rstanopt = stan_glm(YIELD ~ FERTILIZER, data=fert,
##                          algorithm = 'optimizing',
##                          prior_intercept = normal(0, 100,autoscale=FALSE),
##                          prior = normal(0, 10, autoscale=FALSE),
##                          prior_aux = cauchy(0, 5, autoscale=FALSE))
## summary(fert.rstanopt)
## plot(fert.rstanopt, 'hist')
## plot(fert.rstanopt, 'scat', pars = c("(Intercept)", "FERTILIZER"))
## #emmeans(fert.rstanopt, ~FERTILIZER)
## 
## prior_summary(fert.rstanopt)
## predict(fert.rstanarmopt)
## 
## fert.rstanarm = stan_glm(YIELD ~ FERTILIZER, data=fert,
##                          iter = 5000, warmup = 1000,
##                          chains = 3, thin = 5, refresh = 0,
##                          prior_intercept = normal(0, 100,autoscale=FALSE),
##                          prior = normal(0, 10, autoscale=FALSE),
##                          prior_aux = cauchy(0, 5, autoscale=FALSE), )
## prior_summary(fert.rstanarm)
## 
## fert.rstanarm %>% get_variables()
## fert.rstanarm %>% as.matrix() %>% dim
## fert.rstanarm %>% as.matrix() %>% head
## 
## fert.rstanarm %>% as.matrix %>% as_tibble()
## ## OR
## fert.rstanarm %>% tidy_draws()
## ## OR
## fert.rstanarm %>% posterior_samples() %>% as_tibble()
## ## OR
## fert.rstanarm %>% get_variables()
## fert.rstanarm %>% spread_draws(`.Intercept.*|FERT.*`,  regex=TRUE)
## 
## ## We can sample only from the prior predictive distribution (and not condition on the observed data)
## ## and explore the predictions from this in order to see the impacts of the priors
## fert.rstanarm1 <- update(fert.rstanarm,  prior_PD=TRUE)
## ggpredict(fert.rstanarm1) %>% plot


## ----stan1.brms, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
## For brms,  it is best to start with a separate statements about the formula and priors
## fert.form <- bf(YIELD ~ scale(FERTILIZER),  family=gaussian())
fert.form <- bf(YIELD ~ FERTILIZER,  family=gaussian())
## start by checking which parameters can have priors
get_prior(fert.form,  data=fert)
median(fert$YIELD)
mad(fert$YIELD)
fert.priors <- c(
  prior(normal(160, 90),  class='Intercept'),
  prior(normal(0, 10), class='b'),
  prior(cauchy(0, 5),  class='sigma')
)

## For this simple model,  we could simulate what estimates are possible
## based purely on the priors..
#Xmat <- model.matrix(~FERTILIZER,  data=fert)
#coefs <- cbind(rnorm(1000, 160, 90),
#               rnorm(1000, 0, 10))
#preds <- coefs %*% t(Xmat)
#preds <- sweep(preds, 1, rcauchy(1000, 0, 5),  FUN='+')
#preds <- preds %>% t %>%
#  as.data.frame %>%
#  bind_cols(fert) %>%
#  pivot_longer(cols=V1:V1000)
#head(preds)
#ggplot(preds) +
#  geom_line(aes(y=value, x=FERTILIZER,  group=name))

fert.samples = brm(fert.form,
                   data=fert,
                   prior=fert.priors,
                   sample_prior = 'only')
conditional_effects(fert.samples) %>% plot(points=TRUE)

fert.brms = brm(fert.form,
                data=fert,
                prior=fert.priors,
                sample_prior = TRUE,
                iter=5000, warmup=1000,
                chains=3,  thin=5,  refresh=0)
#fert.brms = update(fert.samples,  sample_prior=TRUE, recompile=FALSE)
conditional_effects(fert.brms) %>%  plot(points=TRUE)

#fert.brms = brm(fert.form,
#                   data=fert, 
#                   prior=fert.priors,
#                sample_prior=TRUE)
## We are now going to go through a quick run down of a few ways to retrieve and visualise the data
## generated from stan
## Keeping in mind,  we are going to want to do the following:
## - explore MCMC sampling diagnostics
## - explore priors and posteriors
## - explore regular model diagnostics (residuals etc)
## - explore effects
## what has been returned
fert.brms %>% get_variables()
fert.brms %>% as.matrix() %>% dim 
fert.brms %>% as.matrix() %>% head 
## There are some additional useful ways to get the draws etc
fert.brms %>% tidy_draws()
## OR
fert.brms %>% posterior_samples() %>% as_tibble()
## OR
fert.brms %>% get_variables()
fert.brms %>% spread_draws(`b.*`,  regex=TRUE)

## Compare posterior and prior
fert.post = fert.brms %>% posterior_samples() 
ggplot(fert.post) +
  geom_density(aes(x=b_Intercept,  fill='Posterior'),  alpha=0.5) +
  geom_density(aes(x=prior_Intercept,  fill='Prior'),  alpha=0.5)
ggplot(fert.post) +
  geom_density(aes(x=b_FERTILIZER,  fill='Posterior'),  alpha=0.5) +
  #geom_density(aes(x=b_scaleFERTILIZER,  fill='Posterior'),  alpha=0.5) +
  geom_density(aes(x=prior_b,  fill='Prior'),  alpha=0.5)
ggplot(fert.post) +
  geom_density(aes(x=sigma,  fill='Posterior'),  alpha=0.5) +
  geom_density(aes(x=prior_sigma,  fill='Prior'),  alpha=0.5) +
  scale_x_log10()

## Prediction - manually
standata(fert.brms)$X
coefs = fert.brms %>% posterior_samples(pars=c('b_Intercept', 'b_FERTILIZER'),  as.matrix=TRUE)
#coefs = fert.brms %>% posterior_samples(pars=c('b_Intercept', 'b_scaleFERTILIZER'),  as.matrix=TRUE)
Xmat = standata(fert.brms)$X
coefs %*% t(Xmat) %>% head

## Get the predictions
## - A row per MCMC sample
## - as many columns as observations
fert.brms %>% posterior_epred() %>%  as_tibble


## Each of the above provide all the parameter estimates
## The following does the actual predictions for you and is
## useful for plotting trends
fert %>%  add_fitted_draws(fert.brms)
fert.fit <- fert %>%
  add_fitted_draws(fert.brms) %>%
  median_hdci() 
#  ggplot(aes(x=FERTILIZER)) +
#  geom_pointrange(aes(y=.value, ymin=.lower, ymax=.upper))+
#  geom_point(data=fert,  aes(y=YIELD),  color='red')

## The following is useful for comparing to observed values
fert %>%  add_predicted_draws(fert.brms)
fert.pred <- fert %>%
  add_predicted_draws(fert.brms) %>%
  median_hdci()

ggplot() +
  geom_pointrange(data=fert.pred,  aes(y=.prediction, x=FERTILIZER, ymin=.lower, ymax=.upper))+
  geom_pointrange(data=fert.fit,  aes(y=.value, x=FERTILIZER+2, ymin=.lower, ymax=.upper))+
  geom_point(data=fert,  aes(y=YIELD,  x=FERTILIZER),  color='red')


## for predictions
#fert %>% add_predicted_draws(fert.brms)
                                        #fert %>% add_fitted_draws(fert.brms)
fert.brms %>% get_variables()
fert.brms %>%
  spread_draws(b_Intercept, b_FERTILIZER)
fert.brms %>%
  spread_draws(b_Intercept, b_FERTILIZER) %>%
  median_hdci
fert.brms %>%
  gather_draws(`b.*`, regex=TRUE) %>%
  median_hdci

fert.brms %>%
  gather_draws(`b.*`,  regex=TRUE) %>%
  ggplot(aes(y=.variable, x=.value)) +
  geom_halfeyeh() +
  facet_wrap(~.variable,  scales='free')

## What about the effects
fert.brms %>% get_variables()
fert.brms %>%
  gather_draws(`b.*`, sigma, regex=TRUE) %>%
  #gather_draws(b_Intercept, b_FERTILIZER, sigma) %>%
  median_hdi()
#fert.brms %>%
#  gather_draws(b_Intercept, b_FERTILIZER, sigma) %>%
#  ggplot(aes(x=.value,  y=.variable)) +
#  stat_halfeyeh()

fert.brms %>% tidy_draws()

library(modelr)
fert %>%
  data_grid(FERTILIZER=seq_range(FERTILIZER, n=100)) %>%
  add_fitted_draws(fert.brms,  n=100) %>%
  ggplot() +
  stat_lineribbon(aes(x=FERTILIZER,  y=.value)) +
  geom_point(data=fert,  aes(x=FERTILIZER, y=YIELD))

## Spaghetti plot
fert %>%
  data_grid(FERTILIZER=seq_range(FERTILIZER, n=100)) %>%
  add_fitted_draws(fert.brms,  n=1000) %>%
  ggplot() +
  geom_line(aes(y=.value, x=FERTILIZER, group=.draw),  alpha=0.1)+
  geom_point(data=fert,  aes(x=FERTILIZER, y=YIELD))  


## ----modelValidation, results='markdown', eval=FALSE, hidden=TRUE, fig.width=6, fig.height=4----
## posterior_vs_prior(fert.rstanarm, color_by='vs', group_by=TRUE,
##                    facet_args=list(scales='free_y'))
## #mcmc_plot(fert.brms)


## ----modelValidation.brms, results='markdown', eval=FALSE, hidden=TRUE, fig.width=6, fig.height=4----
## posterior_summary(fert.brms)
## mcmc_plot(fert.brms,  pars='prior.*|b.*|sigma')
## 
## ## fert.brms %>% get_variables()
## ## fert.brms %>%
## ##   #gather_draws(b_Intercept,  n=100) %>%
## ##   gather_draws(`^b_.*|.*sigma|prior.*`,  regex=TRUE) %>%
## ##   mutate(P_P = ifelse(str_detect(.variable, 'prior'), 'Prior', 'Posterior'),
## ##          Var=str_replace(.variable, '.*_', '')) %>%
## ##   head
## ##   ggplot(aes(y=factor(Var),  x=.value,  color=P_P)) +
## ##   stat_pointintervalh(.width=c(0.66, 0.95)) +
## ##   facet_grid(~Var,  scales='free')
## 
## ## posterior_vs_prior(fert.brms, color_by='vs', group_by=TRUE,
## ##                    facet_args=list(scales='free_y'))


## ----modelValidation2, results='markdown', eval=FALSE, hidden=TRUE, fig.width=6, fig.height=4----
## plot(fert.rstanarm, plotfun='mcmc_trace')
## ##available_mcmc()
## plot(fert.rstanarm, 'acf_bar')
## plot(fert.rstanarm, 'combo')
## plot(fert.rstanarm, 'rhat_hist')
## plot(fert.rstanarm, 'neff_hist')
## plot(fert.rstanarm, 'mcmc_neff_hist')
## plot(fert.rstanarm, 'violin')
## 
## stan_trace(fert.rstanarm)
## stan_dens(fert.rstanarm,separate_chains = TRUE)
## stan_ess(fert.rstanarm)
## stan_ac(fert.rstanarm)
## stan_rhat(fert.rstanarm)
## stan_ess(fert.rstanarm)
## 
## fert.ggs <- ggs(fert.rstanarm)
## ggs_traceplot(fert.ggs)
## ggs_Rhat(fert.ggs)
## ggs_autocorrelation(fert.ggs)
## ggs_crosscorrelation(fert.ggs)
## ggs_effective(fert.ggs)
## ggs_grb(fert.ggs)
##                                         #mcmc_trace(as.array(fert.rstanarm))
## #mcmc_trace(as.array(fert.rstanarm),regex_pars='Intercept|FERT|sigma')
## #mcmc_dens(as.array(fert.rstanarm))
## #mcmc_combo(as.array(fert.rstanarm))
## 
## pp_check(fert.rstanarm)
## ##available_ppc()
## pp_check(fert.rstanarm, plotfun='scatter')
## pp_check(fert.rstanarm, plotfun='error_scatter_avg')
## pp_check(fert.rstanarm, x=fert$FERTILIZER,plotfun='error_scatter_avg_vs_x')
## pp_check(fert.rstanarm, x=fert$FERTILIZER,plotfun='ribbon')
## pp_check(fert.rstanarm, x=fert$FERTILIZER,plotfun='intervals')
## 
## #library(shinystan)
## #launch_shinystan(fert.rstanarm)


## ----modelValidation2.brms, results='markdown', eval=FALSE, hidden=TRUE, fig.width=6, fig.height=4----
## plot(fert.brms)
## mcmc_plot(fert.brms)
## available_mcmc()
## mcmc_plot(fert.brms, type='acf_bar')
## mcmc_plot(fert.brms,  type='combo')
## mcmc_plot(fert.brms, type='rhat_hist')
## mcmc_plot(fert.brms, type='neff_hist')
## mcmc_plot(fert.brms, type='violin')
## 
## stan_ess(fert.brms$fit)
## stan_ac(fert.brms$fit)
## 
## fert.ggs <- ggs(fert.brms, inc_warmup=FALSE)
## ggs_traceplot(fert.ggs)
## ggs_Rhat(fert.ggs)
## ggs_autocorrelation(fert.ggs)
## ggs_crosscorrelation(fert.ggs)
## #ggs_effective(fert.ggs)
## ggs_grb(fert.ggs)
## #ggs_pcp(fert.ggs)
## #ggs_compare_partial(fert.ggs)
## #ggmcmc::ggs_diagnostics(fert.ggs)
## #ggmcmc::ggs_caterpillar(fert.ggs)
## 
## pp_check(fert.brms, nsamples=100)
## ##available_ppc()
## pp_check(fert.brms, type='scatter')
## pp_check(fert.brms, type='error_scatter_avg')
## pp_check(fert.brms, x='FERTILIZER', type='error_scatter_avg_vs_x')
## pp_check(fert.brms, x='FERTILIZER', type='ribbon')
## pp_check(fert.brms, x='FERTILIZER', type='intervals')
## 
## #library(shinystan)
## #launch_shinystan(fert.brms)

## ----modelValidation3, results='markdown', eval=FALSE, hidden=TRUE------------
## preds <- posterior_predict(fert.rstanarm,  nsamples=250,  summary=FALSE)
## fert.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = fert$YIELD,
##                             fittedPredictedResponse = apply(preds, 2, median),
##                             integerResponse = 'gaussian')
## plot(fert.resids)
## 
## ggplot() +
##   geom_point(data=NULL,  aes(x=fert$FERTILIZER,  y=fert.resids$scaledResiduals))
## 
## fert %>%
##   add_residual_draws(fert.brms) %>%
##   ggplot(aes(y=.residual, x=FERTILIZER)) +
##   geom_point()
## 
## resid=resid(fert.rstanarm)
## fit=fitted(fert.rstanarm)
## ggplot() + geom_point(data=NULL, aes(y=resid, x=fit))
## ggplot() + geom_point(data=NULL, aes(y=resid, x=fert$FERTILIZER))
## sresid = resid/sd(resid)
## ggplot() + geom_point(data=NULL, aes(y=sresid, x=fit))
## 
## #violin plot
## y_pred = posterior_predict(fert.rstanarm)
## newdata = fert %>% cbind(t(y_pred)) %>% gather(key='Rep',value='Value',-YIELD,-FERTILIZER)
## ggplot(newdata, aes(Value,x=FERTILIZER)) + geom_violin(color='blue', fill='blue',alpha=0.5) +
##     geom_violin(data=fert, aes(y=YIELD, x=FERTILIZER), fill='red',color='red', alpha=0.5)
## 
## ## Partial plots
## ggpredict(fert.rstanarm) %>% plot
## 
## ## Prediction intervals
## library(broom)
## newdata = data.frame(FERTILIZER=seq(min(fert$FERTILIZER, na.rm=TRUE), max(fert$FERTILIZER, na.rm=TRUE), len=1000))
## fit = posterior_predict(fert.rstanarm, newdata=newdata)
## newdata = newdata %>%
##     cbind(tidyMCMC(as.mcmc(fit), conf.int=TRUE, conf.method='HPDinterval'))
## ggplot(newdata, aes(y=estimate, x=FERTILIZER)) +
## geom_point(data=fert, aes(y=YIELD)) +
## geom_line() +
## geom_ribbon(aes(ymin=conf.low, ymax=conf.high), fill='blue', alpha=0.3) +
## scale_y_continuous('YIELD') +
## scale_x_continuous('FERTILIZER') +
## theme_classic()
## 


## ----modelValidation3.brms, results='markdown', eval=FALSE, hidden=TRUE-------
## preds <- posterior_predict(fert.brms, nsamples = 250, summary = FALSE)
## fert.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = fert$YIELD,
##                             fittedPredictedResponse = apply(preds, 2, median),
##                             integerResponse = 'gaussian')
## plot(fert.resids)
## 
## ggplot() +
##   geom_point(data=NULL,  aes(x=fert$FERTILIZER,  y=fert.resids$scaledResiduals))
## 
## fert %>%
##   add_residual_draws(fert.brms) %>%
##   ggplot(aes(y=.residual, x=FERTILIZER)) +
##   geom_point()
## 
## ## Partial plots
## ggpredict(fert.brms) %>% plot


## ----Samples, results='markdown', eval=FALSE, hidden=TRUE---------------------
## fert.mcmc = fert.rstanarm %>% as.matrix()
## nrow(fert.mcmc)
## head(fert.mcmc)


## ----modelSummary, results='markdown', eval=FALSE, hidden=TRUE----------------
## summary(fert.rstanarm)
## tidyMCMC(fert.rstanarm, conf.int=TRUE, conf.method='HPDinterval')
## #fert.mcmc %>% median_hdi
## fert.rstanarm %>%
##   tidy_draws() %>%
##   gather_variables() %>%
##   median_hdci(.width=c(0.66, 0.95)) %>%
##   to_broom_names
## posterior_interval(fert.rstanarm, prob=0.95)


## ----modelSummary.brms, results='markdown', eval=FALSE, hidden=TRUE-----------
## summary(fert.brms)
## tidyMCMC(fert.brms, conf.int=TRUE, conf.method='HPDinterval')
## fert.brms %>%
##   tidy_draws() %>%
##   gather_variables() %>%
##   median_hdci(.width=c(0.66, 0.95)) %>%
##   to_broom_names
## posterior_interval(fert.brms, prob=0.95)


## ----mcmcpvalue, results='markdown', eval=FALSE-------------------------------
## mcmcpvalue <- function(samp)
## {
##     ## elementary version that creates an empirical p-value for the
##     ## hypothesis that the columns of samp have mean zero versus a
##     ## general multivariate distribution with elliptical contours.
## 
##     ## differences from the mean standardized by the observed
##     ## variance-covariance factor
## 
##     ## Note, I put in the bit for single terms
##     if (length(dim(samp))==0) {
##         std <- backsolve(chol(var(samp)),cbind(0, t(samp)) - mean(samp),transpose = TRUE)
##         sqdist <- colSums(std * std)
##         sum(sqdist[-1] > sqdist[1])/length(samp)
##     }
##     else {
##         std <- backsolve(chol(var(samp)),cbind(0, t(samp)) - colMeans(samp),transpose = TRUE)
##         sqdist <- colSums(std * std)
##         sum(sqdist[-1] > sqdist[1])/nrow(samp)
##     }
## 
## }


## ----mcmcpvalue1, results='markdown', eval=FALSE, hidden=TRUE-----------------
## mcmcpvalue(as.matrix(fert.rstanarm)[, c("FERTILIZER")])


## ----posteriors, results='markdown', eval=FALSE, hidden=TRUE------------------
## #probability that slope greater than 1
## fert.mcmc = fert.rstanarm %>%
##   posterior_samples()
## fert.mcmc %>% head
## #fert.mcmc = as.matrix(fert.rstanarm)
## sum(fert.mcmc[,2]>0)
## sum(fert.mcmc[,2]>0)/nrow(fert.mcmc)
## 
## hypothesis(fert.rstanarm,  'FERTILIZER>0')
## hypothesis(fert.rstanarm,  'FERTILIZER>1')
## hypothesis(fert.rstanarm,  'FERTILIZER>1') %>%  plot
## hypothesis(fert.rstanarm,  'FERTILIZER>0.81')
## 
## #hist(fert.mcmc[,2])
## #sum(fert.mcmc[,2]>0)
## #sum(fert.mcmc[,2]>0)/length(fert.mcmc[,2])
## #sum(fert.mcmc[,2]>.5)/length(fert.mcmc[,2])
## #sum(fert.mcmc[,2]>.8)/length(fert.mcmc[,2])


## ----posteriors.brms, results='markdown', eval=FALSE, hidden=TRUE-------------
## #probability that slope greater than 1
## fert.mcmc = fert.brms %>%
##   posterior_samples()
## fert.mcmc %>% head
## 
## sum(fert.mcmc[,2]>0)
## sum(fert.mcmc[,2]>0)/nrow(fert.mcmc)
## 
## hypothesis(fert.mcmc,  'b_FERTILIZER>0')
## hypothesis(fert.mcmc,  'b_FERTILIZER>1')
## hypothesis(fert.mcmc,  'b_FERTILIZER>1') %>% plot()
## hypothesis(fert.mcmc,  'b_FERTILIZER>0.81')
## 
## ## hist(fert.mcmc[,2])
## ## sum(fert.mcmc[,2]>0)
## ## sum(fert.mcmc[,2]>0)/length(fert.mcmc[,2])
## ## sum(fert.mcmc[,2]>.5)/length(fert.mcmc[,2])
## ## sum(fert.mcmc[,2]>.8)/length(fert.mcmc[,2])


## ----posteriors2, results='markdown', eval=FALSE, hidden=TRUE-----------------
## ggpredict(fert.rstanarm) %>% plot
## ## How much has yield increased if fertilizer increases from 100 to 200
## fert.grid = with(fert, list(FERTILIZER=c(200,100)))
## emmeans(fert.rstanarm, pairwise~FERTILIZER, at=fert.grid)$contrast %>% as.data.frame
## ## The above expresses the change in absolute units,  what if weekdays
## ## would like to express this as a percentage change
## 
## ##mcmc=emmeans(fert.rstanarm, ~FERTILIZER, at=fert.grid) %>%
## #     gather_emmeans_draws() %>%
## #  spread(key=FERTILIZER, value=.value)
## mcmc <- emmeans(fert.rstanarm,  ~FERTILIZER,  at=fert.grid) %>%
##   tidy_draws() %>%
##   rename_with(~gsub('FERTILIZER ', 'p', .))
## mcmc %>% head
## mcmc.1= mcmc %>% mutate(Eff=p200 - p100,
##                         PEff=100*(p200 - p100)/p100)
## mcmc.1 %>% tidyMCMC(pars=c('Eff', 'PEff'),
##                     estimate.method='median',
##                     conf.int = TRUE,  conf.method = 'HPDinterval')
## mcmc.1 %>% median_hdci(Eff)
## mcmc.1 %>% median_hdi(PEff)
## hist(mcmc.1$PEff)
## mcmc.1 %>% summarize(Prob=sum(PEff>50)/n())
## mcmc.1 %>% hypothesis('PEff>50')
## #mean(mcmc.1$`200`)
## #sd(mcmc.1$`200`)
## 
## mcmc.1$p100 %>% median_hdi
## 
## ## How much has it changed from the lowest to the highest?
## fert.grid = with(fert, list(FERTILIZER=c(min(FERTILIZER), max(FERTILIZER))))
## #mcmc=emmeans(fert.rstanarm, eff~FERTILIZER, at=fert.grid)$emmeans %>%
## #        gather_emmeans_draws() %>%
##                                         #        spread(key=FERTILIZER, value=.value)
## mcmc <- emmeans(fert.rstanarm,  ~FERTILIZER,  at=fert.grid) %>%
##   tidy_draws() %>%
##   rename_with(~gsub('FERTILIZER ', 'p', .))
## mcmc
## mcmc = mcmc %>%
##         mutate(Fit=p250 - p25,
##                PEff=100*(p250 - p25)/p25)
## mcmc %>% mean_hdi(Fit)
## mcmc %>% mean_hdi(PEff)
## mcmc %>% summarize(Prob=sum(PEff>50)/n())
## ## Cohen's D
## mcmc = mcmc %>%
##   mutate(cohenD=(p250-p25)/fert.rstanarm %>% posterior_samples('sigma') %>% pull(sigma))
## mcmc %>% median_hdi(cohenD)
## 
## 
## (full=loo(fert.rstanarm))
## a = update(fert.rstanarm,formula=.~1)
## (reduced=loo(a))
## #(reduced=loo(update(fert.rstanarm,formula=.~1)))
## par(mfrow = 1:2, mar = c(5,3.8,1,0) + 0.1, las = 3)
## plot(full, label_points = TRUE)
## plot(reduced, label_points = TRUE)
## loo_compare(full,reduced)
## 
## 
## ##Effect size
## fert.mcmc = as.matrix(fert.rstanarm)
## ## NOTE - not a sequence this time.
## ## We want to look at the two extremes
## newdata = with(fert, data.frame(FERTILIZER=c(min(FERTILIZER,na.rm=TRUE), max(FERTILIZER,na.rm=TRUE))))
## Xmat = model.matrix(~FERTILIZER, newdata)
## coefs = fert.mcmc[,c('(Intercept)','FERTILIZER')]
## fit = coefs %*% t(Xmat)
## ## Raw effect size
## (RES=tidyMCMC(as.mcmc(fit[,2] - fit[,1]), conf.int = TRUE, conf.method = "HPDinterval"))
## ## Cohen's D
## cohenD = (fit[,2] - fit[,1])/fert.mcmc[,'sigma']
## library(coda)
## (cohenDES=tidyMCMC(as.mcmc(cohenD), conf.int=TRUE, conf.method='HPDinterval'))
## ## Percentage change
## ESp = 100*(fit[,2] - fit[,1])/fit[,1]
## (PES=tidyMCMC(as.mcmc(ESp), conf.int=TRUE, conf.method='HPDinterval'))
## ## Probability that the effect is greater than 25% (a decline of >25%)
## sum(ESp > 150)/length(ESp)
## ## fractional change
## fit = fit[fit[,2]>0,]
## (FES=tidyMCMC(as.mcmc(fit[,2]/fit[,1]), conf.int=TRUE, conf.method='HPDinterval'))
## 
## 
## ## Population standard deviations
## fert.mcmc = as.matrix(fert.rstanarm)
## sd.FERTILIZER = abs(fert.mcmc[,'FERTILIZER'])*sd(fert$FERTILIZER)
##                                         # generate a model matrix
## newdata = data.frame(FERTILIZER = fert$FERTILIZER)
## Xmat = model.matrix(~FERTILIZER, newdata)
## ## get median parameter estimates
## coefs = fert.mcmc[, c("(Intercept)", "FERTILIZER")]
## fit = coefs %*% t(Xmat)
## resid = sweep(fit,2,fert$YIELD,'-')
## sd.resid = apply(resid,1,sd)
## sd.all = cbind(sd.FERTILIZER,sd.resid)
## (fpsd = tidyMCMC(sd.all, conf.int=TRUE, conf.method='HPDinterval'))
##                                         #OR expressed as a percentage
## (fpsd.p = tidyMCMC(100*sd.all/rowSums(sd.all), estimate.method='median',conf.int=TRUE, conf.method='HPDinterval'))
## 
## ## we can even plot this as a Bayesian ANOVA table
## ggplot(fpsd, aes(y=estimate, x=term)) +
## geom_pointrange(aes(ymin=conf.low, ymax=conf.high)) +
## geom_text(aes(label=sprintf('%.2f%%', fpsd.p$estimate), vjust=-1)) +
## scale_y_continuous('Finite population standard deviation') +
## scale_x_discrete()+
## coord_flip() +
##     theme_classic()
## 
## ## ## R2
## ## fert.mcmc <- as.matrix(fert.rstanarm)
## ## Xmat = model.matrix(~FERTILIZER, data=fert)
## ## coefs = fert.mcmc[,c('(Intercept)','FERTILIZER')]
## ## fit = coefs %*% t(Xmat)
## ## resid = sweep(fit, 2, fert$FERTILIZER, "-")
## 
## ## rss <- rowSums(resid^2)
## ## totalss <- sum((fert$YIELD-mean(fert$YIELD))^2)
## ## R2 <- 1 - rss/totalss
## ## tidyMCMC(as.mcmc(R2), conf.int = TRUE, conf.method = "HPDinterval")
## 
## 
## fert.r2 = bayes_R2(fert.rstanarm)
## hist(fert.r2)
## fert.r2 %>% median_hdi
## tidyMCMC(as.mcmc(bayes_R2(fert.rstanarm)), conf.int=TRUE)


## ----posteriors2.brms, results='markdown', eval=FALSE, hidden=TRUE------------
## ggpredict(fert.brms) %>% plot
## ## How much has yield increased if fertilizer increases from 100 to 200
## fert.grid = with(fert, list(FERTILIZER=c(200,100)))
## emmeans(fert.brms, pairwise~FERTILIZER, at=fert.grid)$contrast %>% as.data.frame
## 
## #mcmc=emmeans(fert.brms, ~FERTILIZER, at=fert.grid) %>%
## #     gather_emmeans_draws() %>%
## #    spread(key=FERTILIZER, value=.value)
## mcmc <- emmeans(fert.brms, ~FERTILIZER, at=fert.grid) %>%
##   tidy_draws() %>%
##   rename_with(~gsub('FERTILIZER ', 'p', .))
## mcmc
## 
## mcmc.1 = mcmc %>%
##         mutate(Eff=p200 - p100,
##                PEff=100*(p200 - p100)/p100)
## mcmc.1 %>% mean_hdi(Eff)
## mcmc.1 %>% mean_hdi(PEff)
## hist(mcmc.1$PEff)
## mcmc.1 %>% summarize(Prob=sum(PEff>50)/n())
## mcmc.1 %>% hypothesis('PEff>50')
## #mean(mcmc.1$`200`)
## #sd(mcmc.1$`200`)
## 
## mcmc.1$p100 %>% median_hdi
## 
## ## How much has it changed from the lowest to the highest?
## fert.grid = with(fert, list(FERTILIZER=c(min(FERTILIZER), max(FERTILIZER))))
## #mcmc=emmeans(fert.brms, eff~FERTILIZER, at=fert.grid)$emmeans %>%
## #        gather_emmeans_draws() %>%
## #        spread(key=FERTILIZER, value=.value) %>%
## mcmc <- emmeans(fert.rstanarm,  ~FERTILIZER,  at=fert.grid) %>%
##   tidy_draws() %>%
##   rename('Min'=`FERTILIZER 25`, 'Max'=`FERTILIZER 250`)
##   #rename_with(~gsub('FERTILIZER ', 'p', .))
## 
## mcmc
## mcmc = mcmc %>%
##         mutate(Eff=Max-Min,
##                PEff=100*(Max-Min)/Min)
## 
## mcmc %>% mean_hdi(Eff)
## mcmc %>% mean_hdi(PEff)
## mcmc %>% summarize(Prob=sum(PEff>50)/n())
## ## Cohen's D
## mcmc = mcmc %>%
##   mutate(cohenD=(Max-Min)/fert.brms %>% posterior_samples('sigma') %>% pull(sigma))
## mcmc %>% median_hdi(cohenD)
## 
## fert.brms.loo <- fert.brms %>%
##   add_criterion(c('loo', 'waic'))
## loo(fert.brms.loo)
## waic(fert.brms.loo)
## 
## fert.null.brms <- update(fert.brms,  formula=.~1)
## fert.null.brms.loo <- fert.null.brms %>%
##   add_criterion(c('loo', 'waic'))
## 
## loo_compare(fert.brms.loo,  fert.null.brms.loo)
## waic(fert.brms.loo,  fert.null.brms.loo)
## 
## 
## ## ## R2
## bayes_R2(fert.brms)
## fert.r2 = bayes_R2(fert.brms, summary=FALSE)
## hist(fert.r2)
## fert.r2 %>% median_hdci
## tidyMCMC(as.mcmc(bayes_R2(fert.brms, summary=FALSE)), conf.int=TRUE)


## ----summaryFig, results='markdown', eval=FALSE, hidden=TRUE------------------
## fert.list = with(fert, list(FERTILIZER = seq(min(FERTILIZER), max(FERTILIZER), len=100)))
## newdata = emmeans(fert.rstanarm, ~FERTILIZER, at=fert.list) %>% as.data.frame
## head(newdata)
## 
## ggplot(newdata, aes(y=emmean, x=FERTILIZER)) +
## geom_point(data=fert, aes(y=YIELD)) +
## geom_line() +
## geom_ribbon(aes(ymin=lower.HPD, ymax=upper.HPD), fill='blue', alpha=0.3) +
## scale_y_continuous('YIELD') +
## scale_x_continuous('FERTILIZER') +
## theme_classic()
## 
## ## spaghetti plot
## newdata = emmeans(fert.rstanarm, ~FERTILIZER, at=fert.list) %>%
##   gather_emmeans_draws()
## newdata %>% head
## ggplot(newdata,  aes(y=.value,  x=FERTILIZER)) +
##   geom_line(aes(group=.draw),  alpha=0.01) +
##   geom_point(data=fert,  aes(y=YIELD))


## ----summaryFig.brms, results='markdown', eval=FALSE, hidden=TRUE-------------
## fert.list = with(fert, list(FERTILIZER = seq(min(FERTILIZER), max(FERTILIZER), len=100)))
## newdata = emmeans(fert.brms, ~FERTILIZER, at=fert.list) %>% as.data.frame
## head(newdata)
## 
## ggplot(newdata, aes(y=emmean, x=FERTILIZER)) +
## geom_point(data=fert, aes(y=YIELD)) +
## geom_line() +
## geom_ribbon(aes(ymin=lower.HPD, ymax=upper.HPD), fill='blue', alpha=0.3) +
## scale_y_continuous('YIELD') +
## scale_x_continuous('FERTILIZER') +
## theme_classic()
## 
## ## spaghetti plot
## newdata = emmeans(fert.brms, ~FERTILIZER, at=fert.list) %>%
##   gather_emmeans_draws()
## newdata %>% head
## ggplot(newdata,  aes(y=.value,  x=FERTILIZER)) +
##   geom_line(aes(group=.draw),  alpha=0.01) +
##   geom_point(data=fert,  aes(y=YIELD))
## 

