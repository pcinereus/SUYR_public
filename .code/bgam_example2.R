## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE)


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
library(brms)       #for fitting models in STAN
library(coda)       #for diagnostics
library(bayesplot)  #for diagnostics
library(tidybayes)
library(DHARMa)
library(rstan)


## ----readData, results='markdown', eval=TRUE----------------------------------
reed = read_csv('../data/reed.csv', trim_ws=TRUE)
glimpse(reed)


## ----EDA, results='markdown', eval=TRUE, hidden=TRUE--------------------------
ggplot(reed, aes(y=Moorhen.Kauai, x=Year)) +
    geom_point()

ggplot(reed, aes(y=Moorhen.Kauai, x=Year)) +
    geom_point() + geom_smooth()

ggplot(reed, aes(y=Moorhen.Kauai, x=Year)) +
    geom_point() +
    geom_smooth(method='gam', formula=y~s(x),
                method.args=list(family='poisson'))

ggplot(reed, aes(y=Moorhen.Kauai, x=Rainfall)) +
    geom_point() +
    geom_smooth()



## ----fitModel, results='markdown', eval=FALSE, hidden=TRUE--------------------
## ## reed.rstan <- stan_gamm4(Moorhen.Kauai ~ s(Year,  bs='cr') + s(Rainfall,  bs='cr'),
## ##                          family=poisson(link='log'), data=reed,
## ##                          iter=5000,  warmup=2000,  chains=3, thin=5,  refresh=0, cores=3
## ##                          )
## 
## reed %>% summarise(log(median(Moorhen.Kauai, na.rm=TRUE)),
##                    log(mad(Moorhen.Kauai, na.rm=TRUE)))
## priors <- prior(normal(4, 10), class='Intercept') +
##     prior(normal(0,10), class='b') +
##     prior(normal(0,10), class='sds')
## reed.form <- bf(Moorhen.Kauai ~ s(Year,  bs='cr') +
##                     s(Rainfall,  bs='cr'),
##                 family=poisson(link='log'))
## reed.brm1 <- brm(reed.form,
##                  data=reed,
##                  prior = priors,
##                  sample_prior='yes',
##                  iter=5000,  warmup=2500,
##                  chains=3, thin=5,  refresh=0, cores=3)
## prior_summary(reed.brm1)
## 
## (pars <- reed.brm1 %>% get_variables())
## wch <- grepl('^b_.*|^bs_.*|^sds_.*', pars, perl=TRUE)
## g <- vector('list', length=sum(wch))
## names(g) <- pars[wch]
## for (i in pars[wch]) {
##     print(i)
##     p <- reed.brm1 %>% hypothesis(paste0(i,'=0'), class='') %>% plot()
##     g[[i]] <- p[[1]]
## }
## patchwork::wrap_plots(g)
## 
## wch <- grepl('^b_.*|^bs_.*|^sds_.*|^s_.*', pars, perl=TRUE)
## stan_trace(reed.brm1$fit, pars = pars[wch])
## stan_ac(reed.brm1$fit, pars = pars[wch])
## stan_rhat(reed.brm1$fit, pars = pars[wch])
## stan_rhat(reed.brm1$fit)
## stan_ess(reed.brm1$fit)
## mcmc_plot(reed.brmsP,  type='trace')
## mcmc_plot(reed.brmsP,  type='acf_bar')
## mcmc_plot(reed.brmsP,  type='rhat_hist')
## mcmc_plot(reed.brmsP,  type='neff_hist')
## 
## preds <- posterior_predict(reed.brm1,  nsamples=250,  summary=FALSE)
## reed.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = reed.brm1$data$Moorhen.Kauai,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(reed.resids)
## 
## 
## 
## priors <- prior(normal(4, 5), class='Intercept') +
##     prior(normal(0,10), class='b') +
##     prior(normal(0,10), class='sds') +
##     prior(gamma(0.01,0.01), class='shape')
## reed.form <- bf(Moorhen.Kauai ~ s(Year,  bs='cr') +
##                     s(Rainfall,  bs='cr'),
##                 family=negbinomial(link='log'))
## reed.brm2 <- brm(reed.form,
##                  data=reed,
##                  prior = priors,
##                  sample_prior='yes',
##                  iter=5000,  warmup=2500,
##                  chains=3, thin=5,  refresh=0, cores=3,
##                  control = list(adapt_delta=0.99)
##                  )
## ## prior_summary(reed.brm1)
## 
## 
## reed.brm2 %>% get_variables()
## reed.brm2 %>% hypothesis('b_FoodTreatmentSatiated=0', class='') %>% plot()
## 
## (pars <- reed.brm2 %>% get_variables())
## wch <- grepl('^b_.*|^bs_.*|^sds_.*', pars, perl=TRUE)
## 
## g <- vector('list', length=sum(wch))
## names(g) <- pars[wch]
## for (i in pars[wch]) {
##     print(i)
##     p <- reed.brm2 %>% hypothesis(paste0(i,'=0'), class='') %>% plot()
##     g[[i]] <- p[[1]]
## }
## patchwork::wrap_plots(g)
## 
## wch <- grepl('^b_.*|^bs_.*|^sds_.*|^s_.*', pars, perl=TRUE)
## stan_trace(reed.brm2$fit, pars = pars[wch])
## stan_ac(reed.brm2$fit, pars = pars[wch])
## stan_rhat(reed.brm2$fit, pars = pars[wch])
## stan_rhat(reed.brm2$fit)
## stan_ess(reed.brm2$fit)
## 
## preds <- posterior_predict(reed.brm2,  nsamples=250,  summary=FALSE)
## reed.resids <- createDHARMa(simulatedResponse = t(preds),
##                             observedResponse = reed.brm2$data$Moorhen.Kauai,
##                             fittedPredictedResponse = apply(preds, 2, median))
## plot(reed.resids)
## 
## #performance::check_model(reed.brmsP)
## g <- reed.brm2 %>%
##     conditional_smooths() %>%
##     plot(ask=FALSE, plot=FALSE)
## patchwork::wrap_plots(g)
## g[[1]] + g[[2]]
## 
## 
## ## reed.form <- bf(Moorhen.Kauai ~ s(Year,  bs='cr') + s(Rainfall,  bs='cr'),
## ##            family=negbinomial(link='log'))
## ## reed.brmsNB <- brm(reed.form,  data=reed,
## ##                  iter=5000,  warmup=2000,  chains=3, thin=5,  refresh=0, cores=3)
## ## mcmc_plot(reed.brmsNB,  type='trace')
## ## mcmc_plot(reed.brmsNB,  type='acf_bar')
## ## mcmc_plot(reed.brmsNB,  type='rhat_hist')
## ## mcmc_plot(reed.brmsNB,  type='neff_hist')
## 
## ## preds <- posterior_predict(reed.brmsNB,  nsamples=250,  summary=FALSE)
## ## reed.resids <- createDHARMa(simulatedResponse = t(preds),
## ##                             observedResponse = reed.brmsNB$data$Moorhen.Kauai,
## ##                             fittedPredictedResponse = apply(preds, 2, median))
## ## plot(reed.resids)
## ## testZeroInflation(reed.resids)
## 
## ## Year = reed %>%
## ##   filter(!is.na(Moorhen.Kauai)) %>%
## ##   pull(Year)
## ## testTemporalAutocorrelation(reed.resids, time=Year)
## 
## 
## ## reed.form <- bf(Moorhen.Kauai ~ s(Year, bs='cr') +
## ##                     s(Rainfall,  bs='cr'),
## ##                 autocor=~ar(Year, cov=TRUE),
## ##            family=negbinomial(link='log'))
## ## reed.brmsNB1 <- brm(reed.form,  data=reed,
## ##                     iter=5000,  warmup=2000,  chains=3, thin=5,  refresh=0, cores=3,
## ##                     control=list(adapt_delta=0.99))
## ## preds <- posterior_predict(reed.brmsNB1,  nsamples=250,  summary=FALSE)
## ## reed.resids <- createDHARMa(simulatedResponse = t(preds),
## ##                             observedResponse = reed.brmsNB$data$Moorhen.Kauai,
## ##                             fittedPredictedResponse = apply(preds, 2, median))
## ## plot(reed.resids)
## 
## ## save(reed.brmsNB1, file='~/Downloads/gam.RData')
## ## conditional_smooths(reed.brmsNB1)
## ## summary(reed.brmsNB1)
## ## a=conditional_smooths(reed.brmsNB1)
## ## a[[1]] %>% mutate(Fit=exp(3.8+estimate__),
## ##                   Lower=exp(3.8+lower__),
## ##                   Upper=exp(3.8+upper__)) %>%
## ##   ggplot() +
## ##   geom_ribbon(aes(ymin=Lower,  ymax=Upper,  x=Year),  fill='blue',  alpha=0.3)+
## ##   geom_line(aes(y=Fit,  x=Year)) +
## ##   geom_point(data=reed,aes(y=Moorhen.Kauai,  x=Year))
## 
## 
## ## predict(reed.brmsNB1) %>% head
## 
## summary(reed.brm2)
## 
## 
## reed.list <- with(reed, list(Year = modelr::seq_range(Year, n=10)))
## reed.brm2 %>% emmeans(~s(Year), at=reed.list, type='response') %>%
##     gather_emmeans_draws()
## 
## reed.grid <- with(reed,  data.frame(Year=seq(min(Year),  max(Year),  len=100),
##                                     Rainfall=mean(Rainfall)))
## reed.grid %>% head
## newdata <- posterior_epred(reed.brm2,  newdata=reed.grid) %>%
##     as.data.frame() %>%
##     pivot_longer(cols=everything(), names_to='Yr', values_to='Fit') %>%
##     group_by(Yr) %>%
##     median_hdci() %>%
##     mutate(Yr = as.numeric(gsub('V','',Yr))) %>%
##     arrange(Yr) %>%
##     bind_cols(reed.grid)
## 
##   as.mcmc %>%
##   tidyMCMC(conf.int=TRUE) %>%
##   cbind(newdata)
## head(newdata)
## ggplot(newdata) +
##   geom_ribbon(aes(ymin=.lower,  ymax=.upper,  x=Year),  fill='blue',  alpha=0.3)+
##   geom_line(aes(y=Fit,  x=Year)) +
##   geom_point(data=reed,  aes(y=Moorhen.Kauai,  x=Year))
## 
## reed.grid %>%
##     add_fitted_draws(reed.brm2)
## 
## reed.grid %>%
##     add_fitted_draws(reed.brm2) %>%
##     median_hdci() %>%
##   ggplot() +
##   geom_ribbon(aes(ymin=.lower,  ymax=.upper,  x=Year),  fill='blue',  alpha=0.3) +
##   geom_line(aes(y=.value,  x=Year)) +
##   geom_point(data=reed,  aes(y=Moorhen.Kauai,  x=Year))
## 
## reed.fitted %>%
##     median_hdci() %>%
##     ggplot() +
##     geom_ribbon(aes(ymin=.lower,  ymax=.upper,  x=Year),  fill='blue',  alpha=0.3) +
##     geom_line(aes(y=.value,  x=Year)) +
##     geom_point(data=reed,  aes(y=Moorhen.Kauai,  x=Year))
## a=reed.grid %>% add_fitted_draws(reed.brm2)
## #posterior_linpred(newdata=reed.grid)
## 
## 
## g = conditional_smooths(reed.brm2, method='posterior_predict',  transform=log)
## plot(g, method='posterior_predict',  transform=log)
## 
## g = g %>% mutate(across(estimate__,  exp))
## g[[1]] + g[[2]]
## 
## 
## reed.list= with(reed, list(Year=seq(min(Year), max(Year), len=100)))
## reed.brmsNB1 <- reed.brm2
## newdata = emmeans(reed.brmsNB1, ~Year, data=reed, at=reed.list, type='response') %>%
##     as.data.frame
## newdata = emmeans(reed.brmsNB1, ~sYear_1, type='response') %>%
##     as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=response, x=Year)) +
##     geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), fill='blue', alpha=0.3) +
##     geom_line() +
##     theme_bw()
## 
## reed.fitted %>% head
## 
## reed.grid2 <- reed.brm2$data %>% mutate(Rainfall=mean(Rainfall))
## reed.fitted2 <- reed.grid2 %>%
##     ## filter(!is.na(Moorhen.Kauai)) %>%
##     add_fitted_draws(reed.brm2) %>%
##     median_hdci(.value) %>%
##     dplyr::select(.value) %>%
##     ungroup %>%
##     dplyr::mutate(Resid = residuals(reed.brm2)[,'Estimate'],
##            Obs = ((.value) + Resid))
## 
## reed.grid %>%
##     add_fitted_draws(reed.brm2) %>%
##     median_hdci() %>%
##   ggplot() +
##   geom_ribbon(aes(ymin=.lower,  ymax=.upper,  x=Year),  fill='blue',  alpha=0.3) +
##   geom_line(aes(y=.value,  x=Year)) +
##   geom_point(data=reed,  aes(y=Moorhen.Kauai,  x=Year)) +
##   geom_point(data=reed.fitted2,  aes(y=Obs,  x=Year), color='red')

