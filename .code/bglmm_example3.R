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
library(broom.mixed)#for tidying MCMC outputs
library(patchwork)  #for multiple plots
library(standist)   #for visualizing distributions
library(rstanarm)
library(ggeffects)
library(bayesplot)
library(rstan)
library(DHARMa)


## ----readData, results='markdown', eval=TRUE----------------------------------
starling <- read_csv('../data/starling_full.csv', trim_ws = TRUE)
starling %>% glimpse()


## ----processData, results='markdown', eval=TRUE-------------------------------
starling <- starling %>%
    mutate(BIRD = factor(BIRD),
           SITUATION = factor(SITUATION),
           MONTH = factor(MONTH, levels=c('Nov', 'Jan')))


## ----eda1, results='markdown', eval=TRUE, hidden=TRUE-------------------------
ggplot(starling, aes(y=MASS, x=MONTH)) +
    geom_boxplot() +
    facet_grid(~SITUATION)
## Better still
ggplot(starling, aes(y=MASS, x=MONTH, group=BIRD)) +
    geom_point() +
    geom_line() +
    facet_grid(~SITUATION) 


## ----fitModel1a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
starling.rstanarm <- stan_glmer(MASS ~ MONTH*SITUATION+(1|BIRD),
                                data = starling,
                                family = gaussian(), 
                                iter = 5000,
                                warmup = 2000,
                                chains = 3,
                                thin = 5,
                                refresh = 0)


## ----fitModel1b, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
starling.rstanarm %>% prior_summary()


## ----fitModel1c, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
2.5*sd(starling$MASS)


## ----fitModel1d, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
2.5*sd(starling$MASS)/sd(model.matrix(~MONTH*SITUATION, starling)[, 2])


## ----fitModel1e, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
1/sd(starling$MASS)


## ----fitModel1f, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
starling.rstanarm1 <- update(starling.rstanarm,  prior_PD=TRUE)


## ----fitModel1g, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
ggpredict(starling.rstanarm1) %>% plot(add.data=TRUE)


## ----fitModel1h, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
tobacco.rstanarm2 <- stan_glmer(MASS ~ MONTH*SITUATION+(1|BIRD),
                                data = starling,
                                family = gaussian(), 
                                prior_intercept = normal(84, 20, autoscale = FALSE),
                                prior = normal(0, 40, autoscale = FALSE),
                                prior_aux=rstanarm::exponential(0.15, autoscale = FALSE),
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
    ggpredict(~SITUATION*MONTH) %>%
    plot(add.data = TRUE)


## ----fitModel2a, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE, paged.print=FALSE, tidy.opts = list(width.cutoff = 80)----
starling.form <- bf(MASS ~ MONTH*SITUATION+(1|BIRD),
                   family = gaussian() 
                   )
options(width=150)
starling.form %>% get_prior(data = starling)
options(width=80)


## ----fitModel2h, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
starling %>% 
    group_by(SITUATION, MONTH) %>%
    summarise(median(MASS),
              mad(MASS))

standist::visualize("normal(84,20)", xlim=c(0,200))
standist::visualize("student_t(3, 0, 5.9)",
                    "gamma(2,0.5)",
                    "cauchy(0,1)",
                    xlim=c(-10,25))


## ----fitModel2h1, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE------
priors <- prior(normal(84,5), class = 'Intercept') +
    prior(normal(0, 5), class = 'b') +
    prior(gamma(2,0.5), class = 'sigma') +
    prior(cauchy(0,1), class = 'sd') 
starling.form <- bf(MASS ~ MONTH*SITUATION+(1|BIRD),
                     family = gaussian()
                   )
starling.brm2 <- brm(starling.form, 
                  data = starling,
                  prior = priors,
                  sample_prior = 'yes',
                  iter = 5000,
                  warmup = 1000,
                  chains = 3,
                  thin = 5,
                  refresh = 0
                  )

starling.form <- bf(MASS ~ MONTH*SITUATION+(MONTH|BIRD),
                     family = gaussian()
                   )
starling.brm3 <-  brm(starling.form, 
                  data = starling,
                  prior = priors,
                  sample_prior = 'yes',
                  iter = 5000,
                  warmup = 1000,
                  chains = 3,
                  thin = 5,
                  refresh = 0,
                  control = list(adapt_delta=0.99)
                  )

(l.1 <- starling.brm2 %>% loo())
(l.2 <- starling.brm3 %>% loo())
loo_compare(l.1, l.2)


## ----posterior2k, results='markdown', eval=TRUE-------------------------------
starling.brm3 %>% get_variables()
starling.brm3 %>% hypothesis('MONTHJan=0') %>% plot


## ----posterior2k2, results='markdown', eval=TRUE, fig.width=10, fig.height=4----
starling.brm3 %>%
  posterior_samples %>%
  dplyr::select(-`lp__`) %>%
  pivot_longer(everything(), names_to = 'key') %>% 
  filter(!str_detect(key, '^r')) %>%
  mutate(Type = ifelse(str_detect(key, 'prior'), 'Prior', 'Posterior'),
         ## Class = ifelse(str_detect(key, 'Intercept'),  'Intercept',
         ##         ifelse(str_detect(key, 'b'),  'b', 'sigma')),
         Class = case_when(
             str_detect(key, '(^b|^prior).*Intercept$') ~ 'Intercept',
             str_detect(key, 'b_SITUATION.*|b_MONTH.*|prior_b') ~ 'TREATMENT',
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
pars <- starling.brm3 %>% get_variables()
pars <- pars %>% str_extract('^b.Intercept|^b_SITUTATION.*|^b_MONTH.*|[sS]igma|^sd.*') %>%
    na.omit()
pars
starling.brm3 %>% mcmc_plot(type='trace', pars = pars)


## ----modelValidation2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
starling.brm3 %>% mcmc_plot(type='acf_bar', pars = pars)


## ----modelValidation2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
starling.brm3 %>% mcmc_plot(type='rhat_hist')


## ----modelValidation2e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
starling.brm2 %>% mcmc_plot(type='neff_hist')


## ----modelValidation2f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
starling.brm3 %>% mcmc_plot(type='combo', pars = pars)
starling.brm3 %>% mcmc_plot(type='violin', pars = pars)


## ----modelValidation2g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
starling.brm3 %>% get_variables()
pars <- starling.brm3 %>% get_variables()
pars <- str_extract(pars, '^b_.*|^sigma$|^sd.*') %>% na.omit()

starling.brm3$fit %>%
    stan_trace(pars = pars)


## ----modelValidation2h, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
starling.brm3$fit %>%
    stan_ac(pars = pars)


## ----modelValidation2i, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
starling.brm3$fit %>% stan_rhat() 


## ----modelValidation2j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
starling.brm3$fit %>% stan_ess()


## ----modelValidation2k, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
starling.brm3$fit %>%
    stan_dens(separate_chains = TRUE, pars = pars)


## ----modelValidation2l, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=7----
## starling.ggs <- starling.brm3 %>% ggs(burnin = FALSE, inc_warmup = FALSE)
## starling.ggs %>% ggs_traceplot()


## ----modelValidation2m, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=7----
## ggs_autocorrelation(starling.ggs)


## ----modelValidation2n, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
## ggs_Rhat(starling.ggs)


## ----modelValidation2o, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
## ggs_effective(starling.ggs)


## ----modelValidation2p, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
## ggs_crosscorrelation(starling.ggs)


## ----modelValidation2q, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
## ggs_grb(starling.ggs)


## ----modelValidation5a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
available_ppc()


## ----modelValidation5b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
starling.brm3 %>% pp_check(type = 'dens_overlay', nsamples = 100)


## ----modelValidation5c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
starling.brm3 %>% pp_check(type = 'error_scatter_avg')


## ----modelValidation5e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
starling.brm3 %>% pp_check(group = 'BIRD', type = 'intervals')


## ----modelValidation5g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4----
#library(shinystan)
#launch_shinystan(starling.brm2)


## ----modelValidation6a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
preds <- starling.brm3 %>% posterior_predict(nsamples = 250,  summary = FALSE)
starling.resids <- createDHARMa(simulatedResponse = t(preds),
                            observedResponse = starling$MASS,
                            fittedPredictedResponse = apply(preds, 2, median),
                            integerResponse = FALSE)
plot(starling.resids, quantreg = TRUE)


## ----partialPlot2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
starling.brm3 %>%
    conditional_effects("SITUATION:MONTH") %>%
    plot(points = TRUE)


## ----partialPlot2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
starling.brm3 %>%
    ggpredict(~SITUATION*MONTH) %>%
    plot(add.data = TRUE)


## ----partialPlot2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
starling.brm3 %>%
    ggemmeans(~SITUATION|MONTH) %>%
    plot(add.data = TRUE) 


## ----partialPlot2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
Partial.obs <- starling.brm3$data %>%
    mutate(Pred = predict(starling.brm3)[,'Estimate'],
           Resid = resid(starling.brm3)[,'Estimate'],
           Obs = Pred + Resid)

starling.brm3 %>%
    fitted_draws(newdata = starling, re_formula = NA) %>%
    median_hdci() %>%
    ggplot(aes(x = SITUATION, y = .value, color = MONTH)) +
    geom_pointrange(aes(ymin = .lower, ymax = .upper)) + 
    geom_line() +
    geom_point(data = Partial.obs,  aes(y = Obs,  x = SITUATION, color = MONTH),
               position = position_nudge(x = 0.1)) +
    geom_point(data = starling,  aes(y = MASS,  x = SITUATION, color = MONTH), alpha=0.2,
               position = position_nudge(x = 0.05))


## ----summariseModel2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
starling.brm3 %>% summary()


## ----summariseModel2a1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, echo=FALSE----
starling.sum <- summary(starling.brm3)


## ----summariseModel2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
starling.brm3$fit %>%
    tidyMCMC(estimate.method = 'median',
             conf.int = TRUE,  conf.method = 'HPDinterval',
             rhat = TRUE, ess = TRUE)

## ----summariseModel2b1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
starling.tidy <- tidyMCMC(starling.brm3$fit, estimate.method='median',
                         conf.int=TRUE,  conf.method='HPDinterval',
                         rhat=TRUE, ess=TRUE)


## ----summariseModel2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
starling.brm3 %>% get_variables()
starling.draw <- starling.brm3 %>%
    gather_draws(`b.Intercept.*|b_SITUATION.*|b_MONTH.*`,  regex=TRUE)
starling.draw


## ----summariseModel2c1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
starling.draw %>% median_hdci


## ----summariseModel2c3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=FALSE----
starling.gather <- starling.brm3 %>%
    gather_draws(`b_Intercept.*|b_SITUATION.*|b_MONTH.*`,  regex=TRUE) %>%
    median_hdci


## ----summariseModel2c4, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5,echo=TRUE----
starling.brm3 %>%
    gather_draws(`b_Intercept.*|b_SITUATION.*|b_MONTH.*`, regex=TRUE) %>% 
    ggplot() +
    geom_vline(xintercept=0, linetype='dashed') +
    stat_slab(aes(x = .value, y = .variable,
                  fill = stat(ggdist::cut_cdf_qi(cdf,
                                                 .width = c(0.5, 0.8, 0.95), 
                                                 labels = scales::percent_format())
                              )), color='black') + 
    scale_fill_brewer('Interval', direction = -1, na.translate = FALSE) 

starling.brm3 %>% 
    gather_draws(`.Intercept.*|b_SITUATION.*|b_MONTH.*`, regex=TRUE) %>% 
    ggplot() + 
    geom_vline(xintercept = 0, linetype='dashed') +
    stat_halfeye(aes(x=.value,  y=.variable)) +
    theme_classic()


## ----summariseModel2j, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
starling.brm3$fit %>% plot(type='intervals') 


## ----summariseModel2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
starling.brm3 %>% tidy_draws()


## ----summariseModel2e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
starling.brm3 %>% spread_draws(`.*Intercept.*|b_SITUATION.*|b_MONTH.*`,  regex=TRUE)


## ----summariseModel2f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
starling.brm3 %>% posterior_samples() %>% as_tibble()


## ----summariseModel2g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5----
starling.brm3 %>%
    bayes_R2(re.form = NA, summary=FALSE) %>%
    median_hdci
starling.brm3 %>%
    bayes_R2(re.form = ~(1|BIRD), summary=FALSE) %>%
    median_hdci
starling.brm3 %>%
    bayes_R2(re.form = ~(MONTH|BIRD), summary=FALSE) %>%
    median_hdci


## ----postHoc1a, results='markdown', eval=TRUE, echo=1,hidden=TRUE-------------
starling.brm3 %>%
    emmeans(~SITUATION) %>%
    pairs() 

starling.brm3 %>%
    emmeans(~MONTH) %>%
    pairs() 

starling.brm3 %>%
    emmeans(~MONTH|SITUATION) %>%
    pairs() 

starling.em <- starling.brm3 %>%
    emmeans(~MONTH|SITUATION) %>%
    gather_emmeans_draws() %>%
    spread(key=MONTH, value=.value) %>%
    mutate(Eff=Jan-Nov,
           PEff=100*(Jan-Nov)/Nov)
starling.em %>% head

starling.em %>%
    ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 10, linetype = "dashed", color='red') +
    geom_halfeyeh(aes(x = PEff, y = SITUATION)) +
    theme_classic()

starling.em %>% median_hdci(PEff)

starling.em %>%
    summarize(
        Prob=sum(PEff>0)/n(),
        `Prob>10`=sum(PEff>10)/n())


## ----postHoc2a, results='markdown', eval=TRUE, echo=1,hidden=TRUE-------------
levels(starling$SITUATION)
cmat <- cbind(Comp1=c(0.5, 0.5, -1, 0),
              Comp2=c(1, -0.5, -0.5, 0))
starling.brm3 %>%
    emmeans(~SITUATION|MONTH) %>%
    contrast(list(SITUATION=cmat))

starling.em <- starling.brm3 %>%
    emmeans(~SITUATION|MONTH) %>%
    contrast(list(SITUATION=cmat)) %>%
    gather_emmeans_draws() %>%
    spread(key=MONTH, value=.value) %>%
    mutate(Eff=Jan-Nov,
           PEff=100*(Jan-Nov)/Nov)
starling.em %>% head


## ----fitModel, results='markdown', eval=FALSE,hidden=TRUE, echo=FALSE---------
## starling.rstan %>% get_variables()
## plot(starling.rstan,  'mcmc_trace', regex_pars = '^.Intercept|^SITUATION|^MONTH|[sS]igma')
## plot(starling.rstan,  'mcmc_acf_bar', regex_pars = '^.Intercept|^SITUATION|^MONTH|[sS]igma')
## plot(starling.rstan,  'mcmc_rhat_hist', regex_pars = '^.Intercept|^SITUATION|^MONTH|[sS]igma')
## plot(starling.rstan,  'mcmc_neff_hist', regex_pars = '^.Intercept|^SITUATION|^MONTH|[sS]igma')
## 
## 
## preds <- posterior_predict(starling.rstan,  nsamples=250,  summary=FALSE)
## starling.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = starling$MASS,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(starling.resids)
## 
## 
## starling.rstan1 = stan_glmer(MASS ~ MONTH*SITUATION+(MONTH|BIRD),data=starling,
##                             iter=5000, warmup=2000, thin=5, chains=3, refresh=0)
## starling.rstan1 = stan_glmer(MASS ~ MONTH*SITUATION+(MONTH|BIRD),data=starling,
##                              iter=5000, warmup=2000, thin=5, chains=3, refresh=0,
##                              adapt_delta = 0.99)
## #pairs(starling.rstan1,  pars=c('(Intercept)', 'MONTHNov'))
## starling.rstan1 %>% get_variables()
## pairs(starling.rstan1,  regex_pars=c('SITUATION', 'sigma'))
## prior_summary(starling.rstan1)
## 
## plot(starling.rstan1,  'mcmc_trace', regex_pars = '^.Intercept|^SITUATION|^MONTH|[sS]igma')
## plot(starling.rstan1,  'mcmc_acf_bar', regex_pars = '^.Intercept|^SITUATION|^MONTH|[sS]igma')
## plot(starling.rstan1,  'mcmc_rhat_hist', regex_pars = '^.Intercept|^SITUATION|^MONTH|[sS]igma')
## plot(starling.rstan1,  'mcmc_neff_hist', regex_pars = '^.Intercept|^SITUATION|^MONTH|[sS]igma')
## 
## starling.rstan1 = stan_glmer(MASS ~ MONTH*SITUATION+(MONTH|BIRD),data=starling,
##                              iter=10000, warmup=5000, thin=15, chains=3, refresh=0,
##                              adapt_delta = 0.99)
## 
## plot(starling.rstan1,  'mcmc_trace', regex_pars = '^.Intercept|^SITUATION|^MONTH|[sS]igma')
## plot(starling.rstan1,  'mcmc_acf_bar', regex_pars = '^.Intercept|^SITUATION|^MONTH|[sS]igma')
## plot(starling.rstan1,  'mcmc_rhat_hist', regex_pars = '^.Intercept|^SITUATION|^MONTH|[sS]igma')
## plot(starling.rstan1,  'mcmc_neff_hist', regex_pars = '^.Intercept|^SITUATION|^MONTH|[sS]igma')
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

