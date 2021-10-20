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
library(broom.mixed)
library(tidybayes)
library(bayesplot)
library(standist)   #for visualizing distributions
library(rstanarm)
library(ggeffects)
library(rstan)
library(DHARMa)


## ----readData, results='markdown', eval=TRUE----------------------------------
mckeon <- read_csv('../data/mckeon.csv', trim_ws = TRUE)
mckeon %>% glimpse()


## ----processData, results='markdown', eval=TRUE, hidden=TRUE------------------
mckeon = mckeon %>%
  mutate(BLOCK=factor(BLOCK),
         SYMBIONT=factor(SYMBIONT, levels=c('none','crabs','shrimp','both')))


## ----eda1a, results='markdown', eval=TRUE, hidden=TRUE------------------------
ggplot(mckeon, aes(y=PREDATION, x=SYMBIONT)) +
    geom_point(position=position_jitter(width=0.2, height=0))+
    facet_wrap(~BLOCK)


## ----fitModel1a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
mckeon.rstanarm <- stan_glmer(PREDATION ~ SYMBIONT + (1|BLOCK),
                           data = mckeon,
                           family = binomial(link = 'logit'),
                           iter = 5000,
                           warmup = 2000,
                           chains = 3,
                           thin = 5,
                           refresh = 0,
                           cores = 3)


## ----fitModel1b, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
mckeon.rstanarm %>% prior_summary()


## ----fitModel1c, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
2.5*sd(mckeon$PREDATION)


## ----fitModel1d, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
model.matrix(~SYMBIONT, data=mckeon) %>%
    apply(2,sd) %>%
    (function(x) 2.5/x)


## ----fitModel1f, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
mckeon.rstanarm1 <- update(mckeon.rstanarm,  prior_PD=TRUE)


## ----fitModel1g, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
ggpredict(mckeon.rstanarm1) %>% plot(add.data=TRUE, jitter=c(0.5,0))


## ----fitModel1h, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
mckeon.rstanarm2 <- stan_glmer(PREDATION ~ SYMBIONT + (1|BLOCK),
                                data = mckeon,
                                family = binomial(link='logit'), 
                                prior_intercept = normal(0, 2, autoscale = FALSE),
                                prior = normal(0, 10, autoscale = FALSE),
                                prior_covariance = decov(1, 1, 1, 1), 
                                prior_PD = TRUE, 
                                iter = 5000,
                                warmup = 1000,
                                chains = 3,
                                thin = 5,
                                refresh = 0
                                )


## ----fitModel1i, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
mckeon.rstanarm2 %>%
    ggpredict(~SYMBIONT) %>%
    plot(add.data = TRUE, jitter = c(0.5, 0))


## ----fitModel2a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE, paged.print=FALSE, tidy.opts = list(width.cutoff = 80)----
mckeon.form <- bf(PREDATION | trials(1) ~ SYMBIONT + (1|BLOCK),
                  family=binomial(link='logit'))
options(width=150)
mckeon.form %>% get_prior(data = mckeon)
options(width=80)


## ----fitModel2h, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
mckeon %>% 
    group_by(SYMBIONT) %>%
    summarise(mean(PREDATION),
              var(PREDATION))

standist::visualize("normal(0,2)", xlim=c(0,200))
standist::visualize("student_t(3, 0, 2.5)",
                    "gamma(2,0.5)",
                    "cauchy(0,1)",
                    xlim=c(-10,25))


## ----fitModel2h1, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE------
priors <- prior(normal(0, 2), class = 'Intercept') +
    prior(normal(0, 10), class = 'b') +
    prior(cauchy(0,1), class = 'sd') 

mckeon.form <- bf(PREDATION | trials(1) ~ SYMBIONT + (1|BLOCK),
                  family=binomial(link='logit'))
mckeon.brm2 <- brm(mckeon.form, 
                  data = mckeon,
                  prior = priors,
                  sample_prior = 'yes',
                  iter = 5000,
                  warmup = 1000,
                  chains = 3,
                  thin = 5,
                  refresh = 0
                  )

mckeon.form <- bf(PREDATION | trials(1) ~ SYMBIONT + (SYMBIONT|BLOCK),
                  family=binomial(link='logit'))
mckeon.brm3 <-  brm(mckeon.form, 
                  data = mckeon,
                  prior = priors,
                  sample_prior = 'yes',
                  iter = 5000,
                  warmup = 1000,
                  chains = 3,
                  thin = 5,
                  refresh = 0,
                  control = list(adapt_delta=0.99)
                  )

(l.1 <- mckeon.brm2 %>% loo())
(l.2 <- mckeon.brm3 %>% loo())
loo_compare(l.1, l.2)


## ----posterior2k, results='markdown', eval=TRUE-------------------------------
mckeon.brm3 %>% get_variables()
mckeon.brm3 %>% hypothesis('SYMBIONTcrabs=0') %>% plot


## ----posterior2k2, results='markdown', eval=TRUE, fig.width=10, fig.height=4----
mckeon.brm3 %>%
  posterior_samples %>%
  dplyr::select(-`lp__`) %>%
  pivot_longer(everything(), names_to = 'key') %>% 
  filter(!str_detect(key, '^r')) %>%
  mutate(Type = ifelse(str_detect(key, 'prior'), 'Prior', 'Posterior'),
         Class = case_when(
             str_detect(key, '(^b|^prior).*Intercept$') ~ 'Intercept',
             str_detect(key, 'b_SYMBIONT.*|prior_b') ~ 'TREATMENT',
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
pars <- mckeon.brm3 %>% get_variables()
pars <- pars %>% str_extract('^b.Intercept|^b_SYMBIONT.*|[sS]igma|^sd.*') %>%
    na.omit()
pars
mckeon.brm3 %>% mcmc_plot(type='trace', pars = pars)


## ----modelValidation2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
mckeon.brm3 %>% mcmc_plot(type='acf_bar', pars = pars)


## ----modelValidation2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
mckeon.brm3 %>% mcmc_plot(type='rhat_hist')


## ----modelValidation2e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
mckeon.brm3 %>% mcmc_plot(type='neff_hist')


## ----modelValidation2f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
mckeon.brm3 %>% mcmc_plot(type='combo', pars = pars)
mckeon.brm3 %>% mcmc_plot(type='violin', pars = pars)


## ----modelValidation2g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
mckeon.brm3 %>% get_variables()
pars <- mckeon.brm3 %>% get_variables()
pars <- str_extract(pars, '^b_.*|^sigma$|^sd.*') %>% na.omit()

mckeon.brm3$fit %>%
    stan_trace(pars = pars)


## ----modelValidation2h, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
mckeon.brm3$fit %>%
    stan_ac(pars = pars)


## ----modelValidation2i, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
mckeon.brm3$fit %>% stan_rhat() 


## ----modelValidation2j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
mckeon.brm3$fit %>% stan_ess()


## ----modelValidation2k, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
mckeon.brm3$fit %>%
    stan_dens(separate_chains = TRUE, pars = pars)


## ----modelValidation2l, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=7----
## mckeon.ggs <- mckeon.brm3 %>% ggs(burnin = FALSE, inc_warmup = FALSE)
## mckeon.ggs %>% ggs_traceplot()


## ----modelValidation2m, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=7----
## ggs_autocorrelation(mckeon.ggs)


## ----modelValidation2n, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
## ggs_Rhat(mckeon.ggs)


## ----modelValidation2o, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
## ggs_effective(mckeon.ggs)


## ----modelValidation2p, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
## ggs_crosscorrelation(mckeon.ggs)


## ----modelValidation2q, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
## ggs_grb(mckeon.ggs)


## ----modelValidation5a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
available_ppc()


## ----modelValidation5b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
mckeon.brm3 %>% pp_check(type = 'dens_overlay', nsamples = 100)


## ----modelValidation5c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
mckeon.brm3 %>% pp_check(type = 'error_scatter_avg')


## ----modelValidation5e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
mckeon.brm3 %>% pp_check(group = 'BIRD', type = 'intervals')


## ----modelValidation5g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
#library(shinystan)
#launch_shinystan(mckeon.brm2)


## ----modelValidation6a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
preds <- mckeon.brm3 %>% posterior_predict(nsamples = 250,  summary = FALSE)
mckeon.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = mckeon$PREDATION,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = TRUE)
plot(mckeon.resids, quantreg = TRUE)


## ----partialPlot2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
mckeon.brm3 %>%
    conditional_effects() %>%
    plot(points = TRUE)


## ----partialPlot2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
mckeon.brm3 %>%
    ggpredict() %>%
    plot(add.data = TRUE, jitter=c(0.5,0))


## ----partialPlot2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
mckeon.brm3 %>%
    ggemmeans(~SYMBIONT) %>%
    plot(add.data = TRUE, jitter=c(0.5,0)) 


## ----partialPlot2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
Partial.obs <- mckeon.brm3$data %>%
    mutate(Pred = predict(mckeon.brm3, re.form=NA)[,'Estimate'],
           Resid = resid(mckeon.brm3)[,'Estimate'],
           Obs = Pred + Resid)

mckeon.brm3 %>%
    fitted_draws(newdata = mckeon, re_formula = NA) %>%
    median_hdci() %>%
    ggplot(aes(x = SYMBIONT, y = .value)) +
    geom_pointrange(aes(ymin = .lower, ymax = .upper)) + 
    geom_line() +
    geom_point(data = Partial.obs,  aes(y = Obs,  x = SYMBIONT),
               position = position_nudge(x = 0.1)) +
    geom_point(data = mckeon,  aes(y = PREDATION,  x = SYMBIONT), alpha=0.2,
               position = position_nudge(x = 0.05))


## ----summariseModel2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
mckeon.brm3 %>% summary()


## ----summariseModel2a1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, echo=FALSE----
mckeon.sum <- summary(mckeon.brm3)


## ----summariseModel2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
mckeon.brm3$fit %>%
    tidyMCMC(estimate.method = 'median',
             conf.int = TRUE,  conf.method = 'HPDinterval',
             rhat = TRUE, ess = TRUE)

## ----summariseModel2b1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
mckeon.tidy <- tidyMCMC(mckeon.brm3$fit, estimate.method='median',
                         conf.int=TRUE,  conf.method='HPDinterval',
                         rhat=TRUE, ess=TRUE)


## ----summariseModel2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
mckeon.brm3 %>% get_variables()
mckeon.draw <- mckeon.brm3 %>%
    gather_draws(`b.Intercept.*|b_SYMBIONT.*`,  regex=TRUE)
mckeon.draw


## ----summariseModel2c1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
mckeon.draw %>% median_hdci
mckeon.draw %>%
    mutate(.value = exp(.value)) %>%
    median_hdci


## ----summariseModel2c3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
mckeon.gather <- mckeon.brm3 %>%
    gather_draws(`b_Intercept.*|b_SYMBIONT.*`,  regex=TRUE) %>%
    median_hdci


## ----summariseModel2c4, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
mckeon.brm3 %>%
    gather_draws(`b_Intercept.*|b_SYMBIONT.*`, regex=TRUE) %>% 
    ggplot() +
    geom_vline(xintercept=0, linetype='dashed') +
    stat_slab(aes(x = .value, y = .variable,
                  fill = stat(ggdist::cut_cdf_qi(cdf,
                                                 .width = c(0.5, 0.8, 0.95), 
                                                 labels = scales::percent_format())
                              )), color='black') + 
    scale_fill_brewer('Interval', direction = -1, na.translate = FALSE) 

mckeon.brm3 %>% 
    gather_draws(`.Intercept.*|b_SYMBIONT.*`, regex=TRUE) %>% 
    ggplot() + 
    geom_vline(xintercept = 0, linetype='dashed') +
    stat_halfeye(aes(x=.value,  y=.variable)) +
    theme_classic()


## ----summariseModel2j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
mckeon.brm3$fit %>% plot(type='intervals') 


## ----summariseModel2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
mckeon.brm3 %>% tidy_draws()


## ----summariseModel2e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
mckeon.brm3 %>% spread_draws(`.*Intercept.*|b_SYMBIONT.*`,  regex=TRUE)


## ----summariseModel2f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
mckeon.brm3 %>% posterior_samples() %>% as_tibble()


## ----summariseModel2g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
mckeon.brm3 %>%
    bayes_R2(re.form = NA, summary=FALSE) %>%
    median_hdci
mckeon.brm3 %>%
    bayes_R2(re.form = ~(1|BLOCK), summary=FALSE) %>%
    median_hdci
mckeon.brm3 %>%
    bayes_R2(re.form = ~(SYMBIONT|BLOCK), summary=FALSE) %>%
    median_hdci


## ----posthoc1a, results='markdown', eval=TRUE, hidden=TRUE--------------------
mckeon.brm3 %>%
    emmeans(~SYMBIONT, type='response') %>%
    pairs()
mckeon.em <- mckeon.brm3 %>%
    emmeans(~SYMBIONT, type='link') %>%
    pairs() %>%
    gather_emmeans_draws() %>%
    mutate(PEff=exp(.value))#,
             #Prob = plogis(.value))
mckeon.em %>% head
mckeon.em %>%
  group_by(contrast) %>%
  dplyr::select(contrast, PEff) %>%
  median_hdi
mckeon.em %>%
  group_by(contrast) %>%
  summarize(Prob=sum(PEff>1)/n())

mckeon.em = emmeans(mckeon.brm3, ~SYMBIONT, type='link') %>%
      gather_emmeans_draws()
mckeon.em %>% mutate(P=plogis(.value)) %>% median_hdci(P)


## ----posteriors1a, results='markdown', eval=TRUE, hidden=TRUE-----------------
cmat=cbind(
    crab_vs_shrimp=c(0,1,-1,0),
    one_vs_both=c(0,1/2,1/2,-1),
    symbiont=c(1, -1/3, -1/3,-1/3)
)

mckeon.em <- mckeon.brm3 %>%
    emmeans(~SYMBIONT, type='link') %>%
    contrast(method=list(cmat)) %>%
    gather_emmeans_draws() %>%
    mutate(Fit=exp(.value))

mckeon.em %>% median_hdci(Fit)

mckeon.em %>%
  group_by(contrast) %>%
  median_hdi(Fit)

mckeon.em %>%
  group_by(contrast) %>%
  summarize(sum(Fit>1)/n())                                                                      

newdata = emmeans(mckeon.brm3, ~SYMBIONT, type='response') %>% as.data.frame
head(newdata)
ggplot(newdata, aes(y=prob, x=SYMBIONT)) +
    geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD))


## ----R21a, results='markdown', eval=TRUE, hidden=TRUE-------------------------
mckeon.brm3 %>% bayes_R2(re.form=NA)
mckeon.brm3 %>% bayes_R2(re.form=NA, summary=FALSE) %>% median_hdci()
mckeon.brm3 %>% bayes_R2(re.form=~(1|BLOCK), summary=FALSE) %>% median_hdci()
mckeon.brm3 %>% bayes_R2(re.form=~(SYMBIONT|BLOCK), summary=FALSE) %>% median_hdci()


## ----fitModel, results='markdown', eval=FALSE, hidden=TRUE, echo=FALSE--------
## mckeon.rstan = stan_glmer(PREDATION ~ SYMBIONT + (1|BLOCK),
##                           data=mckeon, family=binomial(link='logit'),
##                           iter=5000, warmup=2000, chains=3, thin=5, refresh=0,
##                           cores=3)
## mckeon.rstan %>% get_variables()
## plot(mckeon.rstan,  'mcmc_trace', regex_pars='^.Intercept|SYMBIONT|[sS]igma')
## plot(mckeon.rstan,  'mcmc_acf_bar', regex_pars='^.Intercept|SYMBIONT|[sS]igma')
## plot(mckeon.rstan,  'mcmc_rhat_hist', regex_pars='^.Intercept|SYMBIONT|[sS]igma')
## plot(mckeon.rstan,  'mcmc_neff_hist', regex_pars='^.Intercept|SYMBIONT|[sS]igma')
## 
## 
## preds <- posterior_predict(mckeon.rstan,  nsamples=250,  summary=FALSE)
## mckeon.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = mckeon$PREDATION,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(mckeon.resids)
## 
## mckeon.rstan1 = stan_glmer(PREDATION ~ SYMBIONT + (SYMBIONT|BLOCK),
##                            data=mckeon, family=binomial(link='logit'),
##                            iter=5000, warmup=2000, chains=3, thin=5, refresh=0,
##                           cores=3)
## 
## mckeon.rstan1 %>% get_variables()
## plot(mckeon.rstan1,  'mcmc_trace', regex_pars='^.Intercept|^SYMBIONT|[sS]igma')
## plot(mckeon.rstan1,  'mcmc_acf_bar', regex_pars='^.Intercept|^SYMBIONT|[sS]igma')
## plot(mckeon.rstan1,  'mcmc_rhat_hist', regex_pars='^.Intercept|^SYMBIONT|[sS]igma')
## plot(mckeon.rstan1,  'mcmc_neff_hist', regex_pars='^.Intercept|^SYMBIONT|[sS]igma')
## 
## 
## preds <- posterior_predict(mckeon.rstan1,  nsamples=250,  summary=FALSE)
## mckeon.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = mckeon$PREDATION,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(mckeon.resids)
## 
## #prior_summary(mckeon.rstan1)
## #posterior_vs_prior(mckeon.rstan1, color_by='vs', group_by=TRUE,
## #                   facet_args=list(scales='free_y'))
## 
## loo(mckeon.rstan)
## loo(mckeon.rstan1)
## 
## #as.matrix(mckeon.rstan1) %>% colnames
## #posterior_vs_prior(mckeon.rstan1, color_by='vs', group_by=TRUE,
## #                   regex_pars=c('^(Intercept)','^SYMBIONT','^[sS]igma'),
## #                   facet_args=list(scales='free_y'))
## 
## ##mckeon.rstan1 = stan_glmer(PREDATION ~ SYMBIONT + (SYMBIONT|BLOCK),
## ##                          data=mckeon, family=binomial(link='logit'),
## ##                          iter=4000, warmup=1000, chains=3, thin=5, refresh=0,
## ##                          cores=3,
## ##                          prior_intercept = normal(0,10),
## ##                          prior=normal(0,2.5),
## ##                          prior_covariance = decov(1,1,1,4))
## ##posterior_vs_prior(mckeon.rstan1, color_by='vs', group_by=TRUE, regex_pars=c('^(Intercept)','^SYMBIONT','^[sS]igma'),
## ##                  facet_args=list(scales='free_y'))
## ggpredict(mckeon.rstan1) %>% plot
## summary(mckeon.rstan1)
## 
## mckeon.rstan1 %>% get_variables()
## nms=colnames(as.matrix(mckeon.rstan1))
## wch = grep('^.Intercept|^SYMBIONT',nms)
## #posterior_vs_prior(mckeon.rstan1, color_by='vs', group_by=TRUE,
## #                   facet_args=list(scales='free_y'), pars=nms[wch])
## 
## 
## 
## tidyMCMC(mckeon.rstan1$stanfit,conf.int=TRUE, conf.method='HPDinterval',
##          rhat=TRUE, ess=TRUE,  pars=nms[wch])
## 
## emmeans(mckeon.rstan1, pairwise~SYMBIONT, type='response')
## mckeon.em = emmeans(mckeon.rstan1, pairwise~SYMBIONT, type='link')$contrasts %>%
##       gather_emmeans_draws() %>%
##       mutate(PEff=exp(.value))
## mckeon.em %>% head
## mckeon.em %>%
##   group_by(contrast) %>%
##   dplyr::select(contrast, PEff) %>%
##   median_hdci
## mckeon.em %>%
##   group_by(contrast) %>%
##   summarize(Prob=sum(PEff>1)/n())
## 
## cmat=cbind(
##     crab_vs_shrimp=c(0,1,-1,0),
##     one_vs_both=c(0,-1/2,-1/2,1),
##     symbiont=c(1, -1/3, -1/3,-1/3)
## )
## mckeon.em = emmeans(mckeon.rstan1, ~SYMBIONT, contr=list(cmat), type='link')$contrast %>%
##                                                                  gather_emmeans_draws() %>%
##                                                                  mutate(Fit=exp(.value))
## mckeon.em %>%
##   group_by(contrast) %>%
##   median_hdci(Fit)
## 
## mckeon.em %>%
##   group_by(contrast) %>%
##   summarize(sum(Fit>1)/n())
## 
## newdata = emmeans(mckeon.rstan1, ~SYMBIONT, type='response') %>% as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=prob, x=SYMBIONT)) +
##     geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD))
## 

