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
library(nlme)
library(lme4)
library(glmmTMB)
library(broom.mixed)
library(glmmTMB)   #for glmmTMB
library(DHARMa)   #for residuals and diagnostics
library(performance) #for diagnostic plots
library(see)         #for diagnostic plots


## ----readData, results='markdown', eval=TRUE----------------------------------
norin = read_csv('../data/norin.csv', trim_ws=TRUE)
glimpse(norin)


## ----name, results='markdown', eval=FALSE, hidden=TRUE------------------------
## norin = norin %>% mutate(FISHID=factor(FISHID),
##                          TRIAL=factor(TRIAL))
## 
## ggplot(norin, aes(y=CHANGE, x=TRIAL)) + geom_boxplot()
## ggplot(norin, aes(y=CHANGE, x=SMR_contr, shape=TRIAL, color=TRIAL)) +
##     geom_smooth(method='lm') + geom_point()
## ggplot(norin, aes(y=CHANGE, x=as.numeric(FISHID), color=TRIAL)) +
##     geom_point() + geom_line()
## 
## ggplot(norin, aes(y=MASS, x=TRIAL)) + geom_boxplot()
## ggplot(norin, aes(y=CHANGE, x=MASS, color=TRIAL)) + geom_point() + geom_smooth(method='lm')
## 
## 
## ##Compare models that estimate partial slope for MASS vs an offset for MASS
## ##must use ML to compare models that vary in fixed effects
## norin.lme = lme(CHANGE ~ TRIAL*SMR_contr+MASS, random=~1|FISHID, data=norin, method='ML')
## norin.lme1 = lme(CHANGE ~ TRIAL*SMR_contr+offset(MASS), random=~1|FISHID, data=norin, method='ML')
## norin.lme2 = lme(CHANGE ~ TRIAL*SMR_contr, random=~1|FISHID, data=norin, method='ML')
## AICc(norin.lme, norin.lme1, norin.lme2)
## anova(norin.lme, norin.lme1, norin.lme2)
## ##Now explore the random effects structure
## norin.lme2a = update(norin.lme2, method='REML')
## norin.lme2b = update(norin.lme2a, random=~SMR_contr|FISHID, method='REML')
## ## The newer nlmnb optimizer can be a bit flaky, try the BFGS optimizer instead
## norin.lme2 = update(norin.lme2a, random=~SMR_contr|FISHID, method='REML',
##                     control=lmeControl(opt='optim'))
## AICc(norin.lme2a, norin.lme2b)
## anova(norin.lme2a, norin.lme2b)
## 
## norin.lme2c = update(norin.lme2a, random=~TRIAL|FISHID,
##                     method='REML',control=lmeControl(opt='optim'))
## ## norin.lme2d = update(norin.lme2a, random=~TRIAL*SMR_contr|FISHID,
## ##                     method='REML',control=lmeControl(opt='optim'))
## AICc(norin.lme2a, norin.lme2c)
## anova(norin.lme2a, norin.lme2c)
## 
## ##Random intercept model
## plot(norin.lme2a)
## qqnorm(resid(norin.lme2a))
## qqline(resid(norin.lme2a))
## plot_grid(plot_model(norin.lme2a, type='diag'))
## ## should also look at the residuals vs predictors
## ggplot(data=NULL) + geom_point(aes(y=resid(norin.lme2a, type='normalized'),
##                                    x=norin.lme2a$data$SMR_contr))
## ggplot(data=NULL) + geom_point(aes(y=resid(norin.lme, type='normalized'),
##                                    x=norin.lme2a$data$TRIAL))
## ##Random intercept/slope model
## plot(norin.lme2c)
## qqnorm(resid(norin.lme2c))
## qqline(resid(norin.lme2c))
## ggplot(data=NULL) + geom_point(aes(y=resid(norin.lme2c, type='normalized'),
##                                    x=norin.lme2c$data$SMR_contr))
## ggplot(data=NULL) + geom_point(aes(y=resid(norin.lme2c, type='normalized'),
##                                    x=norin.lme2c$data$TRIAL))
## 
## 
## plot(allEffects(norin.lme2c, residuals=TRUE))
## summary(norin.lme2c)
## intervals(norin.lme2c, which='fixed')
## tidy(norin.lme2c, effects='fixed', conf.int=TRUE)
## glance(norin.lme2c)
## anova(norin.lme2c, type='marginal')
## 
## ## Compare slopes
## emtrends(norin.lme2c, ~TRIAL, var='SMR_contr')
## emtrends(norin.lme2c, pairwise~TRIAL, var='SMR_contr')
## emmip(norin.lme2c,  TRIAL~SMR_contr,  cov.reduce=range,  CIs=TRUE)
## ## What if we wanted to look at the confidence over the entire length of the trend?
## norin.grid = ref_grid(norin.lme2c,
##                       at=list(SMR_contr=modelr::seq_range(norin$SMR_contr,  n=100)))
## emmip(norin.grid, TRIAL~SMR_contr,  cov.reduce=range,  CIs=TRUE)
## ## What about the categorical variable
## emmeans(norin.lme2c, ~TRIAL)
## emmeans(norin.lme2c, pairwise~TRIAL)
## ## emmeans(norin.lme2c, ~SMR_contr|TRIAL)
## ## norin.grid=with(norin,
## ##                 ref_grid(norin.lme2c,
## ##                     at=list(SMR_contr=c(min(SMR_contr), mean(SMR_contr), max(SMR_contr)))))
## norin.grid=with(norin, list(SMR_contr=
##                               c(min(SMR_contr), mean(SMR_contr), max(SMR_contr))))
## emmeans(norin.lme2c, pairwise~SMR_contr|TRIAL, at=norin.grid)
## 
## r.squaredGLMM(norin.lme2c)
## 
## norin.grid=with(norin, list(SMR_contr=seq(min(SMR_contr), max(SMR_contr), len=100)))
## newdata = emmeans(norin.lme2c, ~SMR_contr|TRIAL, at=norin.grid) %>%
##     as.data.frame
## head(newdata)
## ggplot(data=newdata, aes(y=emmean, x=SMR_contr)) +
##     geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL, fill=TRIAL), alpha=0.3) +
##     geom_line(aes(, color=TRIAL))
## 
## 
## ##LMER =========================================================================
## ##Compare models that estimate partial slope for MASS vs an offset for MASS
## ##must use ML to compare models that vary in fixed effects
## norin.lmer = lmer(CHANGE ~ TRIAL*SMR_contr+MASS + (1|FISHID), data=norin, REML=FALSE)
## norin.lmer1 = lmer(CHANGE ~ TRIAL*SMR_contr+offset(MASS) + (1|FISHID), data=norin, REML=FALSE)
## norin.lmer2 = lmer(CHANGE ~ TRIAL*SMR_contr + (1|FISHID), data=norin, REML=FALSE)
## AICc(norin.lmer,norin.lmer1, norin.lmer2)
## anova(norin.lmer, norin.lmer1, norin.lmer2)
## 
## norin.lmer2a = update(norin.lmer2, REML=TRUE)
## norin.lmer2b = update(norin.lmer2a, ~ TRIAL*SMR_contr + (SMR_contr|FISHID), REML=TRUE)
## norin.lmer2c = update(norin.lmer2a, ~ TRIAL*SMR_contr + (TRIAL|FISHID), REML=TRUE)
## anova(norin.lmer2a, norin.lmer2b)
## AICc(norin.lmer2a,  norin.lmer2b)
## 
## plot_grid(plot_model(norin.lmer2a, type='diag')[-2])
## ps <- plot_model(norin.lmer2a,  type='diag')
## ps[[2]] <- ps[[2]][[1]]
## plot_grid(ps)
## check_model(norin.lmer2a)
## 
## norin.resid = simulateResiduals(norin.lmer2a,  plot=TRUE)
## #norin.lmer3 = update(norin.lmer1, ~ TRIAL*SMR_contr + (TRIAL|FISHID), offset=MASS, control=lmerControl(check.nobs.vs.nRE="ignore"))
## #anova(norin.lmer1, norin.lmer3)
## 
## ##GLMMTMB =========================================================================
## ##Compare models that estimate partial slope for MASS vs an offset for MASS
## norin.glmmTMB = glmmTMB(CHANGE ~ TRIAL*SMR_contr+MASS + (1|FISHID), data=norin, REML=FALSE)
## norin.glmmTMB1 = glmmTMB(CHANGE ~ TRIAL*SMR_contr+offset(MASS) + (1|FISHID), data=norin,  REML=FALSE)
## norin.glmmTMB2 = glmmTMB(CHANGE ~ TRIAL*SMR_contr + (1|FISHID), data=norin,  REML=FALSE)
## AICc(norin.glmmTMB, norin.glmmTMB1, norin.glmmTMB2)
## anova(norin.glmmTMB, norin.glmmTMB1, norin.glmmTMB2)
## 
## norin.glmmTMB2a = update(norin.glmmTMB2, REML=TRUE)
## norin.glmmTMB2b = update(norin.glmmTMB2a, ~ TRIAL*SMR_contr + (SMR_contr|FISHID))
## norin.glmmTMB2c = update(norin.glmmTMB2a, ~ TRIAL*SMR_contr + (TRIAL|FISHID))
## AICc(norin.glmmTMB2a, norin.glmmTMB2b, norin.glmmTMB2c)
## anova(norin.glmmTMB2a,  norin.glmmTMB2c)
## 
## 
## plot_model(norin.glmmTMB2c,  type='diag')[-2] %>% plot_grid
## ps <- plot_model(norin.glmmTMB2c,  type='diag')
## ps[[2]] <- ps[[2]][[1]]
## plot_grid(ps)
## check_model(norin.glmmTMB2c)
## norin.resid = simulateResiduals(norin.glmmTMB2c,  plot=TRUE)
## 
## plot(allEffects(norin.glmmTMB2c), multiline=TRUE,  ci.style='bands')
## plot_model(norin.glmmTMB2c,  type='eff') %>% plot_grid
## plot_model(norin.glmmTMB2c,  type='eff',  terms=c('SMR_contr', 'TRIAL'),  show.data=TRUE)
## 
## plot_model(norin.glmmTMB2c,  type='est')
## plot_model(norin.glmmTMB2c,  type='re')
## summary(norin.glmmTMB2c)
## ## Compare slopes
## emtrends(norin.glmmTMB2c, ~TRIAL, var='SMR_contr')
## emtrends(norin.glmmTMB2c, pairwise~TRIAL, var='SMR_contr')
## emmip(norin.glmmTMB2c,  TRIAL~SMR_contr,  cov.reduce=range,  CIs=TRUE)
## ## What if we wanted to look at the confidence over the entire length of the trend?
## norin.grid = ref_grid(norin.glmmTMB2c,
##                       at=list(SMR_contr=modelr::seq_range(norin$SMR_contr,  n=100)))
## emmip(norin.grid, TRIAL~SMR_contr,  cov.reduce=range,  CIs=TRUE)
## ## What about the categorical variable
## emmeans(norin.glmmTMB2c, ~TRIAL)
## emmeans(norin.glmmTMB2c, pairwise~TRIAL)
## ## emmeans(norin.glmmTMB2c, ~SMR_contr|TRIAL)
## ## norin.grid=with(norin,
## ##                 ref_grid(norin.glmmTMB2c,
## ##                     at=list(SMR_contr=c(min(SMR_contr), mean(SMR_contr), max(SMR_contr)))))
## norin.grid=with(norin, list(SMR_contr=
##                               c(min(SMR_contr), mean(SMR_contr), max(SMR_contr))))
## emmeans(norin.glmmTMB2c, pairwise~SMR_contr|TRIAL, at=norin.grid)
## 
## r.squaredGLMM(norin.glmmTMB2c)
## performance::r2_nakagawa(norin.glmmTMB2c)
## 
## norin.grid=with(norin, list(SMR_contr=seq(min(SMR_contr), max(SMR_contr), len=100)))
## newdata = emmeans(norin.glmmTMB2c, ~SMR_contr|TRIAL, at=norin.grid) %>%
##     as.data.frame
## head(newdata)
## ggplot(data=newdata, aes(y=emmean, x=SMR_contr)) +
##     geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL, fill=TRIAL), alpha=0.3) +
##     geom_line(aes(, color=TRIAL))
## 

