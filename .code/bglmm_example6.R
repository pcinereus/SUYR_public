## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE)


## ----libraries, results='markdown', eval=TRUE, message=FALSE, warning=FALSE----
library(tidyverse) #for data wrangling


## ----readData, results='markdown', eval=TRUE----------------------------------
copper = read_csv('../data/copper.csv', trim_ws=TRUE)
glimpse(copper)


## ----name, results='markdown', eval=FALSE, hidden=TRUE------------------------
## copper = copper %>% mutate(COPPER=factor(COPPER),
##                            PLATE=factor(PLATE),
##                            DIST=factor(DIST))
## 
## ggplot(copper, aes(y=WORMS, x=DIST, fill=COPPER)) +
##   geom_boxplot()
## 
## ggplot(copper, aes(y=WORMS, x=DIST, fill=COPPER)) +
##     geom_boxplot() +
##     scale_y_log10()
## 
## ggplot(copper, aes(y=WORMS, x=DIST, fill=COPPER)) +
##     geom_boxplot() +
##     scale_y_sqrt()
## 
## 
## ggplot(copper, aes(y=log(WORMS), x=DIST, fill=COPPER)) +
##     geom_boxplot()
## 
## copper.rstan = stan_glmer(I(WORMS+0.5)~COPPER*DIST + (1|PLATE),
##                           data=copper,
##                           family=Gamma(link='log'),
##                           chains = 3, iter = 5000, warmup=1000, thin=5,
##                           prior_intercept = normal(0,1, adjust=FALSE),
##                           prior=normal(0,1,adjust=FALSE),
##                           prior_aux = cauchy(0,2,adjust=FALSE))
## posterior_vs_prior(copper.rstan, color_by='vs', group_by=TRUE, regex_pars=c('^MONTH','^SITUATION','^[sS]igma'),
##                    facet_args=list(scales='free_y'))
## 
## copper.rstan = stan_glmer(I(WORMS+0)~COPPER*DIST + (1|PLATE),
##                           data=copper,
##                           family=gaussian(),
##                           chains = 3, iter = 5000, warmup=1000, thin=5)
## 
## copper.rstan1 = stan_glmer(WORMS~COPPER*DIST + (1|PLATE),
##                           data=copper,
##                           family=Gamma(link='log'),
##                           chains = 3, iter = 5000, warmup=1000, thin=5)
## 
## copper.rstan = stan_glmer(WORMS~COPPER*DIST + (1|PLATE),
##                           data=copper,
##                           family=gaussian(link='log'),
##                           chains = 3, iter = 5000, warmup=1000, thin=5)
## prior_summary(copper.rstan)
## 
## stan_ess(copper.rstan)
## 
## newdata = emmeans(copper.rstan1, ~DIST|COPPER, type='response') %>% as.data.frame
## ggplot(newdata, aes(y=response, x=DIST, color=COPPER)) +
##     geom_pointrange(aes(ymin=lower.HPD, ymax=upper.HPD))
## 
## 
## mullens <- read.csv('../data/mullens.csv', strip.white=T)
## head(mullens)
## 
## mullens = mullens %>%
##     mutate(pFREQBUC = ifelse(FREQBUC==0, 0.01, FREQBUC/100),
##            BREATH=factor(BREATH),
##            TOAD=factor(TOAD))
## head(mullens)
## 
## library(INLA)
## 
## mullens.rstan = stan_glmer(pFREQBUC~BREATH*poly(O2LEVEL,3)+(1|TOAD),
##                           data=mullens,
##                           family=mgcv::betar,
##                           chains = 3, warmup=1000,
##                           iter = 2000, thin=3, refresh=0)
## stan_ac(mullens.rstan)
## stan_trace(mullens.rstan)
## stan_ess(mullens.rstan)
## stan_rhat(mullens.rstan)
## 
## plot(mullens.rstan)
## 
## posterior_vs_prior(mullens.rstan, group_by_parameter = TRUE,
##                    facet_args = list(scales = "free_y"),prob = .95,
##                    pars=nms[wch])
## 
## nms = colnames(as.matrix(mullens.rstan))
## wch = grep("", nms)
## 
## tidyMCMC(mullens.stan$stanfit, conf.int=TRUE,conf.method='HPDinterval',rhat=TRUE,ess=TRUE)
## g=ggpredict(mullens.stan) %>% plot
## do.call('grid.arrange', g)
## 
## 
## mullens.grid = with(mullens, list(O2LEVEL = seq(min(O2LEVEL),max(O2LEVEL), len=100)))
## emmeans(mullens.rstan, ~BREATH|O2LEVEL, at=mullens.grid)
## 
## emmeans(mullens.rstan, ~BREATH
##         ggemmeans(mullens.stan, ~BREATH|O2LEVEL) %>% plot
## 
## 
## 
## newdata = with(mullens, expand.grid(BREATH=levels(BREATH),
##               O2LEVEL = seq(min(O2LEVEL), max(O2LEVEL), len=100)))
## colnames(as.matrix(mullens.stan))
## nms=colnames(as.matrix(mullens.stan))
## wch=grep('^.Intercept|^BREATH|^poly',nms)
## wch
## 
## coefs = as.matrix(mullens.stan)[,nms[wch]]
## colMeans(coefs)
## head(coefs)
## 
## Xmat = model.matrix(~BREATH*poly(O2LEVEL,3), data=newdata)
## colMeans(Xmat)
## 
## fit = binomial()$linkinv(coefs %*% t(Xmat))
## colMeans(fit) %>% head
##                                         #fit = binomial()$linkinv(posterior_linpred(mullens.stan, newdata=newdata, re.form=~0))
## #fit = posterior_linpred(mullens.stan, newdata=newdata, re.form=~0, transform=TRUE)
## newdata = newdata %>% cbind(tidyMCMC(fit, conf.int=TRUE,
##     conf.method = 'HPDinterval'))
## ggplot(newdata, aes(y=estimate, x=O2LEVEL)) +
##     geom_ribbon(aes(ymin=conf.low, ymax=conf.high, fill=BREATH),alpha=0.2) +
##         geom_line(aes(color=BREATH))
## 
## tidyMCMC(mullens.stan$stanfit, conf.int=TRUE,conf.method='HPDinterval',
##          rhat=TRUE,ess=TRUE)
## bayes_R2(mullens.rstan, re.form=NA) %>% median_hdi
## bayes_R2(mullens.rstan, re.form=~(1|TOAD)) %>% median_hdi

