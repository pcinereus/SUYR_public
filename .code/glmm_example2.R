## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE,cache.lazy = FALSE, tidy='styler')


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
library(patchwork)  #for multiplot layouts


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


## ----fitModel1b, results='markdown', eval=TRUE, hidden=TRUE-------------------
## Start with random intercept model
norin.lme1 <- lme(CHANGE ~ 1 + offset(MASS),
                  random = ~1|FISHID,
                  data=norin,
                  method='REML')
## Then random intercept and slope model
norin.lme2 <- lme(CHANGE ~ 1 + offset(MASS),
                  random = ~TRIAL|FISHID,
                  data=norin,
                  method='REML')
AICc(norin.lme1, norin.lme2)
anova(norin.lme1, norin.lme2)


## ----fitModel1a, results='markdown', eval=TRUE, hidden=TRUE-------------------
##Compare models that estimate partial slope for MASS vs an offset for MASS
##must use ML to compare models that vary in fixed effects
norin.lme2a <- update(norin.lme2,
                      .~TRIAL*scale(SMR_contr, scale=FALSE) + offset(MASS),
                      method = 'ML')

norin.lme2b <- update(norin.lme2,
                      .~TRIAL*scale(SMR_contr, scale=FALSE) + MASS,
                      method = 'ML')
norin.lme2c <- update(norin.lme2,
                      .~TRIAL*scale(SMR_contr, scale=FALSE),
                      method = 'ML')

AICc(norin.lme2a, norin.lme2b, norin.lme2c)
anova(norin.lme2a, norin.lme2b, norin.lme2c)
## Now that we have decided on the structure, we need to ensure that the
## model has been fit with REML
norin.lme2c <- update(norin.lme2c, method='REML')


## ----fitModel2b, results='markdown', eval=TRUE, hidden=TRUE-------------------
## Start with random intercept model
norin.lmer1 <- lmer(CHANGE ~ 1 + offset(MASS) + (1|FISHID),
                    data = norin,
                    REML = TRUE)
## Then random intercept and slope model
norin.lmer2 <- lmer(CHANGE ~ 1 + offset(MASS) + (TRIAL|FISHID),
                    data = norin,
                    REML = TRUE,
                    control = lmerControl(check.nobs.vs.nRE = 'ignore'))
AICc(norin.lmer1,  norin.lmer2)
anova(norin.lmer1, norin.lmer2)


## ----fitModel2a, results='markdown', eval=TRUE, hidden=TRUE-------------------
##Compare models that estimate partial slope for MASS vs an offset for MASS
##must use ML to compare models that vary in fixed effects
norin.lmer2a <- update(norin.lmer2,
                       .~TRIAL*scale(SMR_contr, scale = FALSE) + offset(MASS)
                       + (TRIAL|FISHID),
                       data = norin,
                       REML = FALSE)
norin.lmer2b <- update(norin.lmer2,
                       .~TRIAL*scale(SMR_contr, scale = FALSE) + MASS
                       + (TRIAL|FISHID),
                       data = norin,
                       REML = FALSE)
norin.lmer2c <- update(norin.lmer2,
                       .~TRIAL*scale(SMR_contr, scale = FALSE)
                       + (TRIAL|FISHID),
                       data = norin,
                       REML = FALSE)
## Compare these models via AICc
AICc(norin.lmer2a, norin.lmer2b, norin.lmer2c)
## Alternatively,  we can use sequential Likelihood Ratio Tests (LRT)
anova(norin.lmer2a, norin.lmer2b, norin.lmer2c)
## Now that we have decided on the structure, we need to ensure that the
## model has been fit with REML
norin.lmer2c <- update(norin.lmer2c, REML=TRUE)


## ----fitModel3b, results='markdown', eval=TRUE, hidden=TRUE-------------------
## Start with random intercept model
norin.glmmTMB1 <- glmmTMB(CHANGE ~ 1 + offset(MASS) + (1|FISHID),
                          data = norin,
                          REML = TRUE)
## Then random intercept and slope model
norin.glmmTMB2 <- glmmTMB(CHANGE ~ 1 + offset(MASS) + (TRIAL|FISHID),
                          data = norin,
                          REML = TRUE)
## norin.glmmTMB2 <- glmmTMB(CHANGE ~ 1 + offset(MASS) + (TRIAL|FISHID),
##                           data = as.data.frame(norin),
##                           REML = TRUE,
##                           control=glmmTMBControl(optimizer=optim,
##                                                  optArgs=list(method='BFGS')
##                                                  )
##                           )
norin.glmmTMB2 <- glmmTMB(CHANGE ~ 1 + offset(scale(MASS, scale=FALSE)) + (TRIAL|FISHID),
                          data = norin,
                          REML = TRUE)
AICc(norin.glmmTMB1, norin.glmmTMB2)
anova(norin.glmmTMB1, norin.glmmTMB2)


## ----fitModel3a, results='markdown', eval=TRUE, hidden=TRUE-------------------
##Compare models that estimate partial slope for MASS vs an offset for MASS
norin.glmmTMB2a <- glmmTMB(CHANGE~TRIAL*scale(SMR_contr, scale=FALSE) +
                               offset(MASS) +
                               (TRIAL|FISHID),
                           data = norin, 
                           REML=FALSE)
## OR with update
norin.glmmTMB2a <- update(norin.glmmTMB2,
                          .~TRIAL*scale(SMR_contr, scale=FALSE) +
                              offset(scale(MASS, scale=FALSE)) +
                              (TRIAL|FISHID),
                          REML=FALSE)
## now with MASS as a partial slope
norin.glmmTMB2b <- glmmTMB(CHANGE~TRIAL*scale(SMR_contr, scale=FALSE) +
                               scale(MASS, scale=FALSE) +
                               (TRIAL|FISHID),
                           data = norin, 
                           REML=FALSE)
## OR with update
norin.glmmTMB2b <- update(norin.glmmTMB2,
                          .~TRIAL*scale(SMR_contr, scale=FALSE) +
                              scale(MASS, scale=FALSE) +
                              (TRIAL|FISHID),
                         REML=FALSE)
## Finally without MASS
norin.glmmTMB2c <- glmmTMB(CHANGE~
                               TRIAL*scale(SMR_contr, scale=FALSE) +
                               (TRIAL|FISHID),
                           data = norin, 
                           REML=FALSE)
## I have explored a range of alternative optimizers etc and
## none of them resolve the issue.
## The current model will be good enough to explore AICc and it turns out that the model
## converges fine once we run with REML

## Note the following does resolve it (not sure why), yet causes downstream issues
## norin.glmmTMB2c <- glmmTMB(CHANGE~
##                                TRIAL*scale(SMR_contr, scale=FALSE) +
##                               (TRIAL|FISHID),
##                           data = norin, 
##                           control=glmmTMBControl(profile=TRUE),
##                          REML=FALSE)
## ## OR with update
## norin.glmmTMB2c <- update(norin.glmmTMB2,
##                           .~TRIAL*scale(SMR_contr, scale=FALSE) +
##                               (TRIAL|FISHID),
##                           control=glmmTMBControl(profile=TRUE),
##                          REML=FALSE)
## Compare these models via AICc
AICc(norin.glmmTMB2a, norin.glmmTMB2b, norin.glmmTMB2c)
## Now that we have decided on the structure, we need to ensure that the
## model has been fit with REML
## It turns out that when the model withough MASS is run using REML, the
## response type residuals are tiny - not sure why.
## We will instead go with the offset(MASS) model (2a)
norin.glmmTMB2c <- update(norin.glmmTMB2a, REML=TRUE)


## ----modelValidation1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
plot_model(norin.lme2c, type='diag')[-2] %>% plot_grid()


## ----modelValidation1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
norin.lme2c %>% performance::check_model()


## ----modelValidation1c, results='markdown', eval=TRUE, hidden=TRUE------------
## norin.resid = simulateResiduals(norin.lme2c,  plot=TRUE)


## ----modelValidation2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
plot_model(norin.lmer2c, type='diag')[-2] %>% plot_grid()


## ----modelValidation2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
norin.lmer2c %>% performance::check_model()


## ----modelValidation2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
norin.resid <- norin.lmer2c %>% simulateResiduals(plot=TRUE)


## ----modelValidation3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
plot_model(norin.glmmTMB2c, type='diag')[-2] %>% plot_grid()


## ----modelValidation3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
norin.glmmTMB2c %>% performance::check_model()


## ----modelValidation3c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
norin.resid <- norin.glmmTMB2c %>% simulateResiduals(plot=TRUE)


## ----partialPlots1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
norin.lme2c %>% plot_model(type='eff',  terms=c('SMR_contr', 'TRIAL'),  show.data=TRUE)


## ----partialPlots1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
norin.lme2c %>% plot_model(type='est')


## ----partialPlots1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
#plot_model(norin.lme3a,  type='re')


## ----partialPlots1d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
norin.lme2c %>% allEffects() %>% plot(multiline=TRUE,  ci.style='bands')
## Unfortunately, the use of scale() seems to prevent partial.residuals being calculated
## norin.lme2c %>% allEffects() %>% plot(multiline=TRUE,  ci.style='bands', partial.residuals=TRUE)


## ----partialPlots1e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
norin.lme2c %>% ggpredict(c('SMR_contr', 'TRIAL')) %>% plot(add.data=TRUE)


## ----partialPlots1f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
norin.lme2c %>% ggemmeans(~SMR_contr*TRIAL) %>% plot(add.data=TRUE)


## ----partialPlots2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
norin.lmer2c %>% plot_model(type='eff',  show.data=TRUE) %>% plot_grid()
norin.lmer2c %>% plot_model(type='eff',  terms=c('SMR_contr', 'TRIAL'),  show.data=TRUE)


## ----partialPlots2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
norin.lmer2c %>% plot_model(type='est')


## ----partialPlots2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
norin.lmer2c %>% plot_model(type='re')


## ----partialPlots2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
norin.lmer2c %>% allEffects() %>% plot(multiline=TRUE,  ci.style='bands')


## ----partialPlots2e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
norin.lmer2c %>% ggpredict(c('SMR_contr', 'TRIAL')) %>% plot(add.data=TRUE)


## ----partialPlots2f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
norin.lmer2c %>% ggemmeans(~SMR_contr*TRIAL) %>% plot(add.data=TRUE)


## ----partialPlots3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
norin.glmmTMB2c %>% plot_model(type='eff',  show.data=TRUE) %>% plot_grid()
norin.glmmTMB2c %>%
    plot_model(type='eff',  terms=c('SMR_contr', 'TRIAL'), show.data=TRUE)


## ----partialPlots3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
norin.glmmTMB2c %>% plot_model(type='est')


## ----partialPlots3c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
norin.glmmTMB2c %>% plot_model(type='re')


## ----partialPlots3d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
norin.glmmTMB2c %>%allEffects() %>% plot(multiline=TRUE,  ci.style='bands')


## ----partialPlots3e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
norin.glmmTMB2c %>% ggpredict(c('SMR_contr', 'TRIAL')) %>% plot(add.data=TRUE)


## ----partialPlots3f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
norin.glmmTMB2c %>% ggemmeans(~SMR_contr*TRIAL) %>% plot(add.data=TRUE)


## ----summarizeModel1a, results='markdown', eval=TRUE, hidden=TRUE-------------
norin.lme2c %>% summary()


## ----summarizeModel1b, results='markdown', eval=TRUE, hidden=TRUE-------------
norin.lme2c %>% intervals()


## ----summarizeModel1c, results='markdown', eval=TRUE, hidden=TRUE-------------
norin.lme2c %>% tidy(conf.int=TRUE)
norin.lme2c %>% tidy(conf.int=TRUE) %>% kable


## ----summarizeModel1d, results='markdown', eval=TRUE, hidden=TRUE-------------
# warning this is only appropriate for html output
norin.lme2c %>% sjPlot::tab_model(show.se=TRUE, show.aic=TRUE)


## ----summarizeModel2a, results='markdown', eval=TRUE, hidden=TRUE-------------
norin.lmer2c %>% summary()


## ----summarizeModel2b, results='markdown', eval=TRUE, hidden=TRUE-------------
norin.lmer2c %>% confint()


## ----summarizeModel2c, results='markdown', eval=TRUE, hidden=TRUE-------------
norin.lmer2c %>% tidy(conf.int=TRUE)
norin.lmer2c %>% tidy(conf.int=TRUE) %>% kable


## ----summarizeModel2d, results='markdown', eval=TRUE, hidden=TRUE-------------
# warning this is only appropriate for html output
norin.lmer2c %>% sjPlot::tab_model(show.se=TRUE, show.aic=TRUE)


## ----summarizeModel3a, results='markdown', eval=TRUE, hidden=TRUE-------------
norin.glmmTMB2c %>% summary()
cov2cor(vcov(norin.glmmTMB2c)$cond)
norin.glmmTMB2c %>% vcov() %>% `[[`('cond') %>% cov2cor() %>% round(3)


## ----summarizeModel3b, results='markdown', eval=TRUE, hidden=TRUE-------------
norin.glmmTMB2c %>% confint()


## ----summarizeModel3c, results='markdown', eval=TRUE, hidden=TRUE-------------
norin.glmmTMB2c %>% tidy()
## There seems to be a bug, doesn't work with random effects here
norin.glmmTMB2c %>% tidy(effects='fixed', conf.int=TRUE)
norin.glmmTMB2c %>% tidy(effects='fixed', conf.int=TRUE) %>% kable


## ----summarizeModel3d, results='markdown', eval=TRUE, hidden=TRUE-------------
# warning this is only appropriate for html output
norin.glmmTMB2c %>% sjPlot::tab_model(show.se=TRUE, show.aic=TRUE)


## ----posteriors1a, results='markdown', eval=TRUE, echo=1,hidden=TRUE----------
norin.lme2c %>% emtrends(~TRIAL, var='SMR_contr') %>% pairs()
norin.emt <- norin.lme2c %>% emtrends(~TRIAL, var='SMR_contr') %>% pairs() %>% as.data.frame


## ----posteriors1b, results='markdown', eval=TRUE, hidden=TRUE-----------------
norin.grid <- with(norin,  list(SMR_contr=Hmisc::smean.sdl(SMR_contr)))
norin.grid
norin.lme2c %>% emmeans(~TRIAL|SMR_contr,  at=norin.grid) %>% pairs()


## ----posteriors1c, results='markdown', eval=TRUE, hidden=TRUE-----------------
norin.lme2c %>% r.squaredGLMM()
## Nakagawa's R2
norin.lme2c %>% performance::r2_nakagawa()


## ----posteriors2a, results='markdown', eval=TRUE, hidden=TRUE-----------------
norin.lmer2c %>% emtrends(~TRIAL, var='SMR_contr') %>% pairs()
norin.emt <- norin.lmer2c %>% emtrends(~TRIAL, var='SMR_contr') %>% pairs() %>% as.data.frame


## ----posteriors2b, results='markdown', eval=TRUE, hidden=TRUE-----------------
norin.grid <- with(norin,  list(SMR_contr=Hmisc::smean.sdl(SMR_contr)))
norin.grid
norin.lmer2c %>% emmeans(~TRIAL|SMR_contr,  at=norin.grid) %>% pairs()


## ----posteriors2c, results='markdown', eval=TRUE, hidden=TRUE-----------------
norin.lmer2c %>% r.squaredGLMM()
## Nakagawa's R2
norin.lmer2c %>% performance::r2_nakagawa()


## ----posteriors3a, results='markdown', eval=TRUE, hidden=TRUE-----------------
norin.glmmTMB2c %>% emtrends(~TRIAL, var='SMR_contr') %>%  pairs() %>% summary(infer=TRUE)
norin.emt <- norin.glmmTMB2c %>% emtrends(~TRIAL, var='SMR_contr') %>% pairs() %>% as.data.frame


## ----posteriors3b, results='markdown', eval=TRUE, hidden=TRUE-----------------
norin.grid <- with(norin,  list(SMR_contr=Hmisc::smean.sdl(SMR_contr)))
norin.grid
norin.glmmTMB2c %>% emmeans(~TRIAL|SMR_contr,  at=norin.grid) %>% pairs() %>% summary(infer=TRUE)


## ----posteriors3c, results='markdown', eval=TRUE, hidden=TRUE-----------------
norin.glmmTMB2c %>% r.squaredGLMM()
## Nakagawa's R2
norin.glmmTMB2c %>% performance::r2_nakagawa()


## ----summaryFigure1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=5----
norin.grid <- with(norin, list(SMR_contr = modelr::seq_range(SMR_contr, n = 100)))
newdata <- norin.lme2c %>% emmeans(~SMR_contr|TRIAL, at = norin.grid) %>%
    as.data.frame
head(newdata)

ggplot(data = newdata, aes(y = emmean, x = SMR_contr)) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, fill = TRIAL), alpha = 0.3) +
  geom_line(aes(, color = TRIAL)) +
  theme_classic() +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1))

## The .fixed values are the predicted values without random effects
obs <- norin.lme2c %>%
  augment() %>%
  mutate(PartialObs=.fixed + .resid)

ggplot(data=newdata, aes(y=emmean, x=SMR_contr)) +
  geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL, fill=TRIAL), alpha=0.3) +
  geom_line(aes(, color=TRIAL)) +
  geom_point(data=obs,  aes(y=PartialObs,  color=TRIAL)) +
  geom_point(data=norin,  aes(y=CHANGE), color='gray') +
  theme_classic()


## ----summaryFigure1a2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=9, fig.height=4----
g1 <- ggplot(data=newdata, aes(y=emmean, x=SMR_contr)) +
  geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL, fill=TRIAL), alpha=0.3) +
  geom_line(aes(, color=TRIAL)) +
  geom_point(data=obs,  aes(y=PartialObs,  color=TRIAL)) +
  theme_classic()
g2 <- ggplot(data=newdata, aes(y=emmean, x=SMR_contr)) +
  geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL, fill=TRIAL), alpha=0.3) +
  geom_line(aes(, color=TRIAL)) +
  geom_point(data=norin,  aes(y=CHANGE,  color=TRIAL)) +
  theme_classic()
g1 + g2


## ----summaryFigure2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=5----
norin.grid <- with(norin, list(SMR_contr = modelr::seq_range(SMR_contr, n = 100)))
newdata <- norin.lmer2c %>% emmeans(~SMR_contr|TRIAL, at = norin.grid) %>%
    as.data.frame
head(newdata)

ggplot(data = newdata, aes(y = emmean, x = SMR_contr)) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, fill = TRIAL), alpha = 0.3) +
  geom_line(aes(, color = TRIAL)) +
  theme_classic() +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1))

## The .fixed values are the predicted values without random effects
obs <- norin.lmer2c %>%
  augment() %>%    ## unfortunately, augment.lmer does not back-transform the scale()
    dplyr::rename(SMR_contr=contains('SMR_contr')) %>%
    mutate(PartialObs=.fixed + .resid,
           unscale(SMR_contr),
           SMR_contr = V1) #because ggfortify::unscale() returns a data.frame

ggplot(data=newdata, aes(y=emmean, x=SMR_contr)) +
  geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL, fill=TRIAL), alpha=0.3) +
  geom_line(aes(, color=TRIAL)) +
  geom_point(data=obs,  aes(y=PartialObs,  color=TRIAL)) +
  geom_point(data=norin,  aes(y=CHANGE), color='gray') +
    theme_classic()


## ----summaryFigure2a2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=9, fig.height=4----
g1 <- ggplot(data=newdata, aes(y=emmean, x=SMR_contr)) +
  geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL, fill=TRIAL), alpha=0.3) +
  geom_line(aes(, color=TRIAL)) +
  geom_point(data=obs,  aes(y=PartialObs,  color=TRIAL)) +
  theme_classic()
g2 <- ggplot(data=newdata, aes(y=emmean, x=SMR_contr)) +
  geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL, fill=TRIAL), alpha=0.3) +
  geom_line(aes(, color=TRIAL)) +
  geom_point(data=norin,  aes(y=CHANGE,  color=TRIAL)) +
  theme_classic()
g1 + g2


## ----summaryFigure3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=5----
norin.grid <- with(norin, list(SMR_contr = modelr::seq_range(SMR_contr, n = 100)))
newdata <- norin.glmmTMB2c %>% emmeans(~SMR_contr|TRIAL, at = norin.grid) %>%
    as.data.frame
head(newdata)

ggplot(data = newdata, aes(y = emmean, x = SMR_contr)) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, fill = TRIAL), alpha = 0.3) +
  geom_line(aes(, color = TRIAL)) +
  theme_classic() +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c(1, 1))

## The .fixed values are the predicted values without random effects
obs <- norin %>%
    mutate(.fixed = predict(norin.glmmTMB2c, re.form=NA),
           .resid = residuals(norin.glmmTMB2c),
           PartialObs=.fixed + .resid)

ggplot(data=newdata, aes(y=emmean, x=SMR_contr)) +
  geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL, fill=TRIAL), alpha=0.3) +
  geom_line(aes(, color=TRIAL)) +
  geom_point(data=obs,  aes(y=PartialObs,  color=TRIAL)) +
  geom_point(data=norin,  aes(y=CHANGE), color='gray') +
  theme_classic()


## ----summaryFigure3a2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=9, fig.height=4----

g1 <- ggplot(data=newdata, aes(y=emmean, x=SMR_contr)) +
  geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL, fill=TRIAL), alpha=0.3) +
  geom_line(aes(, color=TRIAL)) +
  geom_point(data=obs,  aes(y=PartialObs,  color=TRIAL)) +
  theme_classic()
g2 <- ggplot(data=newdata, aes(y=emmean, x=SMR_contr)) +
  geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL, fill=TRIAL), alpha=0.3) +
  geom_line(aes(, color=TRIAL)) +
  geom_point(data=norin,  aes(y=CHANGE,  fill=TRIAL), color='black', shape=21) +
  theme_classic()
g1 + g2

