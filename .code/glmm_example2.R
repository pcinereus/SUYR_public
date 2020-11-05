## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE,  message=FALSE,  warning=FALSE)


## ----libraries, results='markdown', eval=TRUE, message=FALSE, warning=FALSE----
library(car)       #for regression diagnostics
library(broom)     #for tidy output
library(ggfortify) #for model diagnostics
library(sjPlot)    #for outputs
library(knitr)     #for kable
library(effects)   #for partial effects plots
library(ggeffects) #for partial effects plots
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


## ----eda1, results='markdown', eval=TRUE, hidden=TRUE-------------------------
norin = norin %>% mutate(FISHID=factor(FISHID),
                         TRIAL=factor(TRIAL))


## ----eda2, results='markdown', eval=TRUE, hidden=TRUE-------------------------
ggplot(norin, aes(y=CHANGE, x=TRIAL)) + geom_boxplot()


## ----eda3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=4----
ggplot(norin, aes(y=CHANGE, x=SMR_contr, shape=TRIAL, color=TRIAL)) +
    geom_smooth(method='lm') + geom_point()
ggplot(norin, aes(y=CHANGE, x=SMR_contr, shape=TRIAL, color=TRIAL)) +
  geom_smooth() + geom_point()


## ----eda4, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=4----
ggplot(norin, aes(y=CHANGE, x=as.numeric(FISHID), color=TRIAL)) +
    geom_point() + geom_line()


## ----eda5, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=4----
ggplot(norin, aes(y=CHANGE, x=MASS, color=TRIAL)) +
  geom_point() +
  geom_smooth(method='lm')


## ----fitModel1a, results='markdown', eval=TRUE, hidden=TRUE-------------------
##Compare models that estimate partial slope for MASS vs an offset for MASS
##must use ML to compare models that vary in fixed effects
norin.lme1 <- lme(CHANGE ~ TRIAL*scale(SMR_contr, scale=FALSE) + offset(MASS), random=~1|FISHID, data=norin, method='ML')
## Unfortunately,  update() does not remove offset().
## We will just have to write the other model out in full as well.
norin.lme2 <- lme(CHANGE ~ TRIAL*scale(SMR_contr, scale=FALSE) + MASS,  random=~1|FISHID, data=norin, method='ML')
## Now without MASS altogether
norin.lme3 <- lme(CHANGE ~ TRIAL*scale(SMR_contr, scale=FALSE),  random=~1|FISHID, data=norin, method='ML')

## Compare these models via AICc
AICc(norin.lme1,norin.lme2, norin.lme3)
## Alternatively,  we can use sequential Likelihood Ratio Tests (LRT)
anova(norin.lme1, norin.lme2, norin.lme3)


## ----fitModel1b, results='markdown', eval=TRUE, hidden=TRUE-------------------
norin.lme3a = update(norin.lme3, method='REML')
norin.lme3b = update(norin.lme3a, random=~TRIAL|FISHID)
AICc(norin.lme3a,  norin.lme3b)
anova(norin.lme3a, norin.lme3b)


## ----fitModel2a, results='markdown', eval=TRUE, hidden=TRUE-------------------
##Compare models that estimate partial slope for MASS vs an offset for MASS
##must use ML to compare models that vary in fixed effects
norin.lmer1 <- lmer(CHANGE ~ TRIAL*scale(SMR_contr, scale=FALSE) + offset(MASS) + (1|FISHID), data=norin, REML=FALSE)
## Unfortunately,  update() does not remove offset().
## We will just have to write the other model out in full as well.
norin.lmer2 <- lmer(CHANGE ~ TRIAL*scale(SMR_contr, scale=FALSE) + MASS + (1|FISHID), data=norin, REML=FALSE)
## Now without MASS altogether
norin.lmer3 <- lmer(CHANGE ~ TRIAL*scale(SMR_contr, scale=FALSE)+ (1|FISHID), data=norin, REML=FALSE)

## Compare these models via AICc
AICc(norin.lmer1,norin.lmer2, norin.lmer3)
## Alternatively,  we can use sequential Likelihood Ratio Tests (LRT)
anova(norin.lmer1, norin.lmer2, norin.lmer3)


## ----fitModel2b, results='markdown', eval=TRUE, hidden=TRUE-------------------
norin.lmer3a = update(norin.lmer3, REML=TRUE)
## unfortunately,  the following random intercept/slope model cannot be fit
##norin.lmer3b = update(norin.lmer3a, ~ . -(1|FISHID) +(TRIAL|FISHID))
##AICc(norin.lmer3a,  norin.lmer3b)
##anova(norin.lmer3a, norin.lmer3b)


## ----fitModel3a, results='markdown', eval=TRUE, hidden=TRUE-------------------
##Compare models that estimate partial slope for MASS vs an offset for MASS
norin.glmmTMB1 = glmmTMB(CHANGE ~ TRIAL*scale(SMR_contr, scale=FALSE) + offset(MASS) + (1|FISHID), data=norin, REML=FALSE)
## Unfortunately,  update() does not remove offset().
## We will just have to write the other model out in full as well.
norin.glmmTMB2 <- lmer(CHANGE ~ TRIAL*scale(SMR_contr, scale=FALSE) + MASS + (1|FISHID), data=norin, REML=FALSE)
## Now without MASS altogether
norin.glmmTMB3 <- lmer(CHANGE ~ TRIAL*scale(SMR_contr, scale=FALSE) + (1|FISHID), data=norin, REML=FALSE)

## Compare these models via AICc
AICc(norin.glmmTMB1,norin.glmmTMB2, norin.glmmTMB3)


## ----fitModel3b, results='markdown', eval=TRUE, hidden=TRUE-------------------
norin.glmmTMB3a <- update(norin.glmmTMB3, REML=TRUE)
## unfortunately,  the following random intercept/slope model cannot be fit
## norin.glmmTMB3b <- update(norin.glmmTMB3a, ~ . - (1|FISHID)+ (TRIAL|FISHID))
##AICc(norin.glmmTMB1a,  norin.glmmTMB1b)


## ----modelValidation1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
plot_grid(plot_model(norin.lme3b, type='diag')[-2])


## ----modelValidation1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
performance::check_model(norin.lme3b)


## ----modelValidation1c, results='markdown', eval=TRUE, hidden=TRUE------------
#norin.resid = simulateResiduals(norin.lme3a,  plot=TRUE)


## ----modelValidation2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
plot_grid(plot_model(norin.lmer3a, type='diag')[-2])


## ----modelValidation2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
performance::check_model(norin.lmer3a)


## ----modelValidation2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
norin.resid = simulateResiduals(norin.lmer3a,  plot=TRUE)


## ----modelValidation3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
plot_grid(plot_model(norin.glmmTMB3a, type='diag')[-2])


## ----modelValidation3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
performance::check_model(norin.glmmTMB3a)


## ----modelValidation3c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
norin.resid = simulateResiduals(norin.glmmTMB3a,  plot=TRUE)


## ----partialPlots1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
plot_model(norin.lme3b,  type='eff',  terms=c('SMR_contr', 'TRIAL'),  show.data=TRUE)


## ----partialPlots1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
plot_model(norin.lme3a,  type='est')


## ----partialPlots1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
#plot_model(norin.lme3a,  type='re')


## ----partialPlots1d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
plot(allEffects(norin.lme3b), multiline=TRUE,  ci.style='bands')


## ----partialPlots1e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
ggpredict(norin.lme3b,  c('SMR_contr', 'TRIAL')) %>% plot(add.data=TRUE)


## ----partialPlots1f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
#ggemmeans(norin.lme3a,  ~SMR_contr*TRIAL) %>% plot(add.data=TRUE)


## ----partialPlots2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
plot_model(norin.lmer3a,  type='eff',  show.data=TRUE) %>% plot_grid
plot_model(norin.lmer3a,  type='eff',  terms=c('SMR_contr', 'TRIAL'),  show.data=TRUE)


## ----partialPlots2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
plot_model(norin.lmer3a,  type='est')


## ----partialPlots2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
plot_model(norin.lmer3a,  type='re')


## ----partialPlots2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
plot(allEffects(norin.lmer3a), multiline=TRUE,  ci.style='bands')


## ----partialPlots2e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
ggpredict(norin.lmer3a,  c('SMR_contr', 'TRIAL')) %>% plot(add.data=TRUE)


## ----partialPlots2f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
ggemmeans(norin.lmer3a,  ~SMR_contr*TRIAL) %>% plot(add.data=TRUE)


## ----partialPlots3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
plot_model(norin.glmmTMB3a,  type='eff',  show.data=TRUE) %>% plot_grid
plot_model(norin.glmmTMB3a,  type='eff',  terms=c('SMR_contr', 'TRIAL'),  show.data=TRUE)


## ----partialPlots3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
plot_model(norin.glmmTMB3a,  type='est')


## ----partialPlots3c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
plot_model(norin.glmmTMB3a,  type='re')


## ----partialPlots3d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
plot(allEffects(norin.glmmTMB3a), multiline=TRUE,  ci.style='bands')


## ----partialPlots3e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
ggpredict(norin.glmmTMB3a,  c('SMR_contr', 'TRIAL')) %>% plot(add.data=TRUE)


## ----partialPlots3f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
ggemmeans(norin.glmmTMB3a,  ~SMR_contr*TRIAL) %>% plot(add.data=TRUE)


## ----summarizeModel1a, results='markdown', eval=TRUE, hidden=TRUE-------------
summary(norin.lme3b)


## ----summarizeModel1b, results='markdown', eval=TRUE, hidden=TRUE-------------
intervals(norin.lme3a)


## ----summarizeModel1c, results='markdown', eval=TRUE, hidden=TRUE-------------
tidy(norin.lme3a,  conf.int=TRUE)
tidy(norin.lme3a,  conf.int=TRUE) %>% kable


## ----summarizeModel1d, results='markdown', eval=TRUE, hidden=TRUE-------------
# warning this is only appropriate for html output
sjPlot::tab_model(norin.lme3a, show.se=TRUE, show.aic=TRUE)


## ----summarizeModel2a, results='markdown', eval=TRUE, hidden=TRUE-------------
summary(norin.lmer3a)


## ----summarizeModel2b, results='markdown', eval=TRUE, hidden=TRUE-------------
confint(norin.lmer3a)


## ----summarizeModel2c, results='markdown', eval=TRUE, hidden=TRUE-------------
tidy(norin.lmer3a,  conf.int=TRUE)
tidy(norin.lmer3a,  conf.int=TRUE) %>% kable


## ----summarizeModel2d, results='markdown', eval=TRUE, hidden=TRUE-------------
# warning this is only appropriate for html output
sjPlot::tab_model(norin.lmer3a, show.se=TRUE, show.aic=TRUE)


## ----summarizeModel3a, results='markdown', eval=TRUE, hidden=TRUE-------------
summary(norin.glmmTMB3a)


## ----summarizeModel3b, results='markdown', eval=TRUE, hidden=TRUE-------------
confint(norin.glmmTMB3a)


## ----summarizeModel3c, results='markdown', eval=TRUE, hidden=TRUE-------------
tidy(norin.glmmTMB3a,  conf.int=TRUE)
tidy(norin.glmmTMB3a,  conf.int=TRUE) %>% kable


## ----summarizeModel3d, results='markdown', eval=TRUE, hidden=TRUE-------------
# warning this is only appropriate for html output
sjPlot::tab_model(norin.glmmTMB3a, show.se=TRUE, show.aic=TRUE)


## ----posteriors1a, results='markdown', eval=TRUE, echo=1,hidden=TRUE----------
emtrends(norin.lme3a, pairwise~TRIAL, var='SMR_contr')
norin.emt <- emtrends(norin.lme3a, pairwise~TRIAL, var='SMR_contr')$contrasts %>% as.data.frame


## ----posteriors1b, results='markdown', eval=TRUE, hidden=TRUE-----------------
norin.grid <- with(norin,  list(SMR_contr=c(min(SMR_contr), mean(SMR_contr), max(SMR_contr))))
norin.grid
emmeans(norin.lme3a, pairwise~TRIAL|SMR_contr,  at=norin.grid)


## ----posteriors1c, results='markdown', eval=TRUE, hidden=TRUE-----------------
r.squaredGLMM(norin.lme3a)
## Nakagawa's R2
performance::r2_nakagawa(norin.lme3a)


## ----posteriors2a, results='markdown', eval=TRUE, hidden=TRUE-----------------
emtrends(norin.lmer3a, pairwise~TRIAL, var='SMR_contr')
norin.emt <- emtrends(norin.lmer3a, pairwise~TRIAL, var='SMR_contr')$contrasts %>% as.data.frame


## ----posteriors2b, results='markdown', eval=TRUE, hidden=TRUE-----------------
norin.grid <- with(norin,  list(SMR_contr=c(min(SMR_contr), mean(SMR_contr), max(SMR_contr))))
norin.grid
emmeans(norin.lmer3a, pairwise~TRIAL|SMR_contr,  at=norin.grid)


## ----posteriors2c, results='markdown', eval=TRUE, hidden=TRUE-----------------
r.squaredGLMM(norin.lmer3a)
## Nakagawa's R2
performance::r2_nakagawa(norin.lmer3a)


## ----posteriors3a, results='markdown', eval=TRUE, hidden=TRUE-----------------
emtrends(norin.glmmTMB3a, pairwise~TRIAL, var='SMR_contr')
norin.emt <- emtrends(norin.glmmTMB3a, pairwise~TRIAL, var='SMR_contr')$contrasts %>% as.data.frame


## ----posteriors3b, results='markdown', eval=TRUE, hidden=TRUE-----------------
norin.grid <- with(norin,  list(SMR_contr=c(min(SMR_contr), mean(SMR_contr), max(SMR_contr))))
norin.grid
emmeans(norin.glmmTMB3a, pairwise~TRIAL|SMR_contr,  at=norin.grid)


## ----posteriors3c, results='markdown', eval=TRUE, hidden=TRUE-----------------
r.squaredGLMM(norin.glmmTMB3a)
## Nakagawa's R2
performance::r2_nakagawa(norin.glmmTMB3a)


## ----summaryFigure1a, results='markdown', eval=TRUE, hidden=TRUE--------------
norin.grid=with(norin, list(SMR_contr=seq(min(SMR_contr), max(SMR_contr), len=100)))
newdata = emmeans(norin.lme3a, ~SMR_contr|TRIAL, at=norin.grid) %>%
    as.data.frame
head(newdata)
ggplot(data=newdata, aes(y=emmean, x=SMR_contr)) +
  geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL, fill=TRIAL), alpha=0.3) +
  geom_line(aes(, color=TRIAL)) +
  theme_classic() +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1))

obs <- norin.lme3a %>%
  augment() %>%
  mutate(PartialObs=.fitted + .resid)

ggplot(data=newdata, aes(y=emmean, x=SMR_contr)) +
  geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL, fill=TRIAL), alpha=0.3) +
  geom_line(aes(, color=TRIAL)) +
  geom_point(data=obs,  aes(y=PartialObs,  color=TRIAL)) +
  theme_classic()



## ----summaryFigure2a, results='markdown', eval=TRUE, hidden=TRUE--------------
norin.grid=with(norin, list(SMR_contr=seq(min(SMR_contr), max(SMR_contr), len=100)))
newdata = emmeans(norin.lmer3a, ~SMR_contr|TRIAL, at=norin.grid) %>%
    as.data.frame
head(newdata)
ggplot(data=newdata, aes(y=emmean, x=SMR_contr)) +
  geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL, fill=TRIAL), alpha=0.3) +
  geom_line(aes(, color=TRIAL)) +
  theme_classic() +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1))

obs <- norin.lmer3a %>%
  augment() %>%
  bind_cols(norin %>% select(SMR_contr)) %>%
  mutate(PartialObs=.fitted + .resid)

ggplot(data=newdata, aes(y=emmean, x=SMR_contr)) +
  geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL, fill=TRIAL), alpha=0.3) +
  geom_line(aes(, color=TRIAL)) +
  geom_point(data=obs,  aes(y=PartialObs,  color=TRIAL)) +
  theme_classic()



## ----summaryFigure3a, results='markdown', eval=TRUE, hidden=TRUE--------------
norin.grid=with(norin, list(SMR_contr=seq(min(SMR_contr), max(SMR_contr), len=100)))
newdata = emmeans(norin.glmmTMB3a, ~SMR_contr|TRIAL, at=norin.grid) %>%
    as.data.frame
head(newdata)
ggplot(data=newdata, aes(y=emmean, x=SMR_contr)) +
  geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL, fill=TRIAL), alpha=0.3) +
  geom_line(aes(, color=TRIAL)) +
  theme_classic() +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1))

obs <- norin.glmmTMB3a %>%
  augment() %>%
  bind_cols(norin %>% select(SMR_contr)) %>%
  mutate(PartialObs=.fitted + .resid)

ggplot(data=newdata, aes(y=emmean, x=SMR_contr)) +
  geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL, fill=TRIAL), alpha=0.3) +
  geom_line(aes(, color=TRIAL)) +
  geom_point(data=obs,  aes(y=PartialObs,  color=TRIAL)) +
  theme_classic()


