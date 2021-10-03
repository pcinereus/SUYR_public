## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE,cache.lazy = FALSE, tidy='styler')


## ----libraries, results='markdown', eval=TRUE, message=FALSE, warning=FALSE----
library(car)       #for regression diagnostics
library(broom)     #for tidy output
library(broom.mixed) #for tidy output
library(ggfortify) #for model diagnostics
library(sjPlot)    #for outputs
library(knitr)     #for kable
library(effects)   #for partial effects plots
library(ggeffects) #for effects plots in ggplot
library(emmeans)   #for estimating marginal means
library(MASS)      #for glm.nb
library(MuMIn)     #for AICc
library(tidyverse) #for data wrangling
library(DHARMa)    #for assessing dispersion etc
library(lme4)      #for lmer
library(lmerTest)  #for degrees of freedom in lmer
library(glmmTMB)    #for glmmTMB
library(performance) #for diagnostic plots
library(see)         #for diagnostic plots


## ----readData, results='markdown', eval=TRUE----------------------------------
mullens <- read_csv('../data/mullens.csv', trim_ws = TRUE)
mullens %>% glimpse()


## ----dataPreparation, results='markdown', eval=TRUE, hidden=FALSE-------------
mullens <- mullens %>%
  mutate(BREATH = factor(BREATH),
         TOAD = factor(TOAD),
         pBUC = FREQBUC/100,
         pzBUC = ifelse(pBUC == 0,0.01,pBUC))


## ----eda1a, results='markdown', eval=TRUE, hidden=FALSE-----------------------
ggplot(mullens,aes(y = FREQBUC, x = factor(O2LEVEL), color = BREATH)) +
    geom_boxplot()


## ----eda1b, results='markdown', eval=TRUE, hidden=FALSE-----------------------
ggplot(mullens,aes(y = pzBUC, x = O2LEVEL, color = BREATH)) +
    geom_smooth() +
    geom_point()


## ----eda1c, results='markdown', eval=TRUE, hidden=FALSE, fig.width=7, fig.height=7----
ggplot(mullens,aes(y = pzBUC, x = O2LEVEL, color = BREATH)) +
    geom_smooth() +
    geom_point() +
    facet_wrap(~BREATH+TOAD, scales = 'free')
##facet_grid(TOAD~BREATH)


## ----fitModel1b, results='markdown', eval=TRUE, hidden=FALSE------------------
mullens.lmer1a <- lmer(SFREQBUC ~ BREATH*poly(O2LEVEL, 3) + (1|TOAD),
                      data = mullens,
                      REML = TRUE)
mullens.lmer1b <- lmer(SFREQBUC ~ BREATH*poly(O2LEVEL, 3) + (poly(O2LEVEL, 3)|TOAD),
                      data = mullens,
                      REML = TRUE)
# OR
mullens.lmer1b = update(mullens.lmer1a,  .~.-(1|TOAD) + (poly(O2LEVEL, 3)|TOAD))
mullens.allFit <- allFit(mullens.lmer1b)
mullens.allFit
## Check which of the models are considered valid (OK)
is.OK <- sapply(mullens.allFit, is, "merMod")
is.OK
AICc(mullens.lmer1a,  mullens.lmer1b)

## mullens.lmer1c <- update(mullens.lmer1a,  ~. -(1|TOAD) + (poly(O2LEVEL, 3)||TOAD))
## anova(mullens.lmer1c)
## mullens.lmer1d <- update(mullens.lmer1a,  ~. + (0+poly(O2LEVEL, 3)||TOAD))
## anova(mullens.lmer1d)


## ----fitModel1a, results='markdown', eval=TRUE, hidden=FALSE------------------
mullens.lmer1c = lmer(SFREQBUC ~ BREATH*poly(O2LEVEL, 3) + (1|TOAD), data=mullens,
                            REML=FALSE)
mullens.lmer1d = update(mullens.lmer1c,  .~.-BREATH:poly(O2LEVEL,  3))
AICc(mullens.lmer1c,  mullens.lmer1d)


## ----fitModel2b, results='markdown', eval=TRUE, hidden=FALSE------------------
mullens.glmmTMB1a <- glmmTMB(SFREQBUC ~ BREATH*poly(O2LEVEL, 3) + (1|TOAD),
                            data = mullens,
                            family = gaussian(),
                            REML = TRUE)
mullens.glmmTMB1b <- glmmTMB(SFREQBUC ~ BREATH*poly(O2LEVEL, 3) + (poly(O2LEVEL, 3)|TOAD),
                            data = mullens,
                            family = gaussian(),
                            REML = TRUE,
                            control = glmmTMBControl(optimizer = optim,
                                                     optArgs = list(method = 'BFGS'))
                            )
##OR
mullens.glmmTMB1b <- update(mullens.glmmTMB1a, ~ . - (1|TOAD) + (poly(O2LEVEL, 3)|TOAD),
                            control = glmmTMBControl(optimizer = optim,
                                                     optArgs = list(method = 'BFGS'))
                    )
AICc(mullens.glmmTMB1a,  mullens.glmmTMB1b)


## ----fitModel2a, results='markdown', eval=TRUE, hidden=FALSE------------------
## mullens.glmmTMB1c <- glmmTMB(SFREQBUC ~ BREATH*poly(O2LEVEL, 3) + (1|TOAD), data=mullens,
##                             family=gaussian(),  REML=FALSE)
## mullens.glmmTMB1d = update(mullens.glmmTMB1a,  .~.-BREATH:poly(O2LEVEL,  3))
## AICc(mullens.glmmTMB1a,  mullens.glmmTMB1b)


## ----fitModel3b, results='markdown', eval=TRUE, hidden=FALSE------------------
mullens.glmmTMB2a <- glmmTMB(pzBUC ~ BREATH*poly(O2LEVEL, 3) + (1|TOAD),
                            data = mullens,
                            family = beta_family(link = 'logit'),
                            REML = TRUE)
mullens.glmmTMB2b <- glmmTMB(pzBUC ~ BREATH*poly(O2LEVEL, 3) + (poly(O2LEVEL, 3)|TOAD),
                            data = mullens,
                            family = beta_family(link = 'logit'),
                            REML = TRUE,
                            control = glmmTMBControl(optimizer = optim,
                                                     optArgs = list(method = 'BFGS'))
                            )
##OR
mullens.glmmTMB2b <- update(mullens.glmmTMB2a, ~ . - (1|TOAD) + (poly(O2LEVEL, 3)|TOAD),
                            control = glmmTMBControl(optimizer = optim,
                                                     optArgs = list(method = 'BFGS'))
                    )
AICc(mullens.glmmTMB2a,  mullens.glmmTMB2b)



## ----fitModel3a, results='markdown', eval=TRUE, hidden=FALSE------------------
## mullens.glmmTMB2a = glmmTMB(pzBUC ~ BREATH*poly(O2LEVEL, 3) + (1|TOAD), data=mullens,
##                             family=beta_family(link = "logit"),  REML=FALSE)
## mullens.glmmTMB2b = update(mullens.glmmTMB2a,  .~.-BREATH:poly(O2LEVEL,  3))
## AICc(mullens.glmmTMB2a,  mullens.glmmTMB2b)


## ----modelValidation1a, results='markdown', eval=TRUE, hidden=FALSE, fig.width=7, fig.height=7----
plot_model(mullens.lmer1a, type='diag')[-2] %>% plot_grid


## ----modelValidation1b, results='markdown', eval=TRUE, hidden=FALSE, fig.width=7, fig.height=7----
mullens.lmer1a %>% performance::check_model()


## ----modelValidation1c, results='markdown', eval=TRUE, hidden=FALSE, fig.width=8, fig.height=4----
mullens.resid <- mullens.lmer1a %>% simulateResiduals(plot=TRUE)


## ----modelValidation2a, results='markdown', eval=TRUE, hidden=FALSE, fig.width=7, fig.height=7----
plot_model(mullens.glmmTMB1b, type='diag')[-2] %>% plot_grid


## ----modelValidation2b, results='markdown', eval=TRUE, hidden=FALSE, fig.width=7, fig.height=7----
mullens.glmmTMB1b %>% performance::check_model()


## ----modelValidation2c, results='markdown', eval=TRUE, hidden=FALSE, fig.width=8, fig.height=4----
mullens.resid <- mullens.glmmTMB1b %>% simulateResiduals(plot=TRUE)


## ----modelValidation3a, results='markdown', eval=TRUE, hidden=FALSE, fig.width=7, fig.height=7----
plot_model(mullens.glmmTMB2b, type='diag')[-2] %>% plot_grid


## ----modelValidation3b, results='markdown', eval=TRUE, hidden=FALSE, fig.width=7, fig.height=7----
mullens.glmmTMB2b %>% performance::check_model()


## ----modelValidation3c, results='markdown', eval=TRUE, hidden=FALSE, fig.width=8, fig.height=4----
## mullens.resid <- mullens.glmmTMB2a %>% simulateResiduals(plot=TRUE)
mullens.resid <- mullens.glmmTMB2b %>% simulateResiduals(plot=TRUE)


## ----modelValidation3cdd, results='markdown', eval=TRUE, hidden=FALSE, fig.width=8, fig.height=4----
mullens.glmmTMB2b %>% ggemmeans(~O2LEVEL|BREATH) %>% plot(add.data=TRUE)


## ----summary1a, results='markdown', eval=TRUE, hidden=FALSE, fig.width=7, fig.height=7----
mullens.lmer1a %>% summary()


## ----summary1a1, results='markdown', eval=TRUE, hidden=FALSE, echo=FALSE------
mullens.tidy <- mullens.lmer1a %>% tidy()


## ----summary1b, results='markdown', eval=TRUE, hidden=FALSE, fig.width=7, fig.height=7----
mullens.lmer1a %>% tidy(conf.int=TRUE)
mullens.lmer1a %>% tidy(conf.int=TRUE) %>% kable


## ----summary1c, results='markdown', eval=TRUE, hidden=FALSE, fig.width=7, fig.height=7----
# warning this is only appropriate for html output
mullens.lmer1a %>% sjPlot::tab_model(show.se=TRUE, show.aic=TRUE)


## ----summary1d, results='markdown', eval=TRUE, hidden=FALSE, fig.width=7, fig.height=7----
# with Satterthwaite degrees of freedom calculations
mullens.lmer1a %>% anova()
#OR with Keyward-Roger degrees of freedom calculations
mullens.lmer1a %>% anova(ddf='Kenward-Roger')


## ----summary2a, results='markdown', eval=TRUE, hidden=FALSE, fig.width=7, fig.height=7----
mullens.glmmTMB1b %>% summary()


## ----summary2a1, results='markdown', eval=TRUE, hidden=FALSE, echo=FALSE------
mullens.tidy <- mullens.glmmTMB1b %>% tidy()


## ----summary2b, results='markdown', eval=TRUE, hidden=FALSE, fig.width=7, fig.height=7----
mullens.glmmTMB1b %>% tidy()
mullens.glmmTMB1b %>% tidy(effects='fixed', conf.int=TRUE)
mullens.glmmTMB1b %>% tidy(effects='fixed', conf.int=TRUE) %>% kable


## ----summary2c, results='markdown', eval=TRUE, hidden=FALSE, fig.width=7, fig.height=7----
# warning this is only appropriate for html output
mullens.glmmTMB1b %>% sjPlot::tab_model(show.se=TRUE, show.aic=TRUE)


## ----summary3a, results='markdown', eval=TRUE, hidden=FALSE, fig.width=7, fig.height=7----
mullens.glmmTMB2b %>% summary()


## ----summary3a1, results='markdown', eval=TRUE, hidden=FALSE, echo=FALSE------
mullens.tidy <- mullens.glmmTMB2b %>% tidy()


## ----summary3b, results='markdown', eval=TRUE, hidden=FALSE, fig.width=7, fig.height=7----
mullens.glmmTMB2b %>% tidy(conf.int = TRUE)
mullens.glmmTMB2b %>% tidy(conf.int = TRUE, exponentiate = TRUE)
mullens.glmmTMB2b %>% tidy(conf.int = TRUE, exponentiate = TRUE) %>% kable


## ----summary3c, results='markdown', eval=TRUE, hidden=FALSE, fig.width=7, fig.height=7----
# warning this is only appropriate for html output
mullens.glmmTMB2b %>% sjPlot::tab_model(show.se=TRUE, show.aic=TRUE)


## ----predictions1a, results='markdown', eval=TRUE, hidden=FALSE, fig.width=7, fig.height=7----
mullens.lmer1a %>% emtrends(specs = 'BREATH',  var = 'O2LEVEL',  max.degree = 3) %>%
    summary(infer = TRUE)
## OR
mullens.lmer1a %>% emtrends(specs = 'BREATH',  var = 'O2LEVEL',  max.degree = 3,  infer = c(TRUE, TRUE))


newdata <- with(mullens,  list(O2LEVEL = modelr::seq_range(O2LEVEL, n = 100),
                               BREATH = levels(BREATH)))
mullens.grid <- mullens.lmer1a %>% emmeans(~O2LEVEL|BREATH,  at = newdata) %>% as.data.frame
mullens.grid %>%
    group_by(BREATH) %>%
    summarise(value  =  O2LEVEL[which.max(emmean)])


## ----predictions2a, results='markdown', eval=TRUE, hidden=FALSE, fig.width=7, fig.height=7----
mullens.glmmTMB1b %>% emtrends(specs = 'BREATH',  var = 'O2LEVEL',  max.degree = 3) %>%
    summary(infer = TRUE)
## OR
emtrends(mullens.glmmTMB1b,  specs='BREATH',  var='O2LEVEL',  max.degree=3,  infer=c(TRUE, TRUE))

newdata <- with(mullens,  list(O2LEVEL = modelr::seq_range(O2LEVEL, n = 1000),
                               BREATH = levels(BREATH)))
mullens.grid <- mullens.glmmTMB1b %>% emmeans(~O2LEVEL|BREATH,  at=newdata) %>% as.data.frame
mullens.grid %>% group_by(BREATH) %>%
  summarise(value = O2LEVEL[which.max(emmean)])


## ----predictions3a, results='markdown', eval=TRUE, hidden=FALSE, fig.width=7, fig.height=7----
mullens.glmmTMB2b %>% emtrends(specs = 'BREATH',  var = 'O2LEVEL',  max.degree = 3) %>%
    summary(infer = TRUE)
## OR
mullens.glmmTMB2b %>% emtrends(specs='BREATH',  var='O2LEVEL',  max.degree=3,  infer=c(TRUE, TRUE))

## newdata <- with(mullens,  list(O2LEVEL=seq(min(O2LEVEL),  max(O2LEVEL),  len=100),
##                                BREATH=levels(BREATH)))
mullens.grid <- with(mullens,  list(O2LEVEL = modelr::seq_range(O2LEVEL, n = 1000),
                               BREATH = levels(BREATH)))
newdata <- emmeans(mullens.glmmTMB2b,  ~O2LEVEL|BREATH,  at = mullens.grid) %>% as.data.frame
newdata %>% group_by(BREATH) %>%
  summarise(value = O2LEVEL[which.max(emmean)])
## r.squaredGLMM(mullens.glmmTMB2a)
performance::r2_nakagawa(mullens.glmmTMB2b)


## ----summaryFigures1a, results='markdown', eval=TRUE, hidden=FALSE, fig.width=7, fig.height=7----
mullens.grid <- with(mullens,
   list(BREATH = levels(BREATH),
     O2LEVEL = modelr::seq_range(O2LEVEL, n=100)
     )
)
newdata <- mullens.lmer1a %>%
    emmeans(~O2LEVEL|BREATH, at = mullens.grid, type = 'response') %>%
    as.data.frame %>%
    mutate(across(c(emmean, lower.CL, upper.CL), function(x) x^2))
    ## mutate(emmean = emmean^2,
    ##        lower.CL=lower.CL^2,
    ##        upper.CL=upper.CL^2)
head(newdata)

ggplot() +
    geom_ribbon(data = newdata,
                aes(ymin = lower.CL,ymax = upper.CL,
                    x = O2LEVEL, fill = BREATH), alpha = 0.3)+    
    geom_line(data = newdata,
              aes(y = emmean, x = O2LEVEL, color = BREATH)) +
    theme_classic()


## ----summaryFigures2a, results='markdown', eval=TRUE, hidden=FALSE, fig.width=7, fig.height=7----
mullens.grid <- with(mullens,
   list(BREATH = levels(BREATH),
     O2LEVEL = modelr::seq_range(O2LEVEL, n=100)
     )
)
newdata <- mullens.glmmTMB1b %>%
    emmeans(~O2LEVEL|BREATH, at = mullens.grid, type = 'response') %>%
    as.data.frame %>%
    mutate(across(c(emmean, lower.CL, upper.CL), function(x) x^2))
    ## mutate(emmean = emmean^2,
    ##        lower.CL=lower.CL^2,
    ##        upper.CL=upper.CL^2)
head(newdata)

ggplot() +
    geom_ribbon(data = newdata,
                aes(ymin = lower.CL,ymax = upper.CL,
                    x = O2LEVEL, fill = BREATH), alpha = 0.3)+    
    geom_line(data = newdata,
              aes(y = emmean, x = O2LEVEL, color = BREATH)) +
    theme_classic()


## ----summaryFigures3a, results='markdown', eval=TRUE, hidden=FALSE, fig.width=7, fig.height=7----
mullens.grid <- with(mullens,
   list(BREATH = levels(BREATH),
     O2LEVEL = modelr::seq_range(O2LEVEL, n=100)
     )
)
newdata <- mullens.glmmTMB2b %>%
    emmeans(~O2LEVEL|BREATH, at = mullens.grid, type = 'response') %>%
    as.data.frame()
head(newdata)

ggplot() +
    geom_ribbon(data = newdata,
                aes(ymin = lower.CL,ymax = upper.CL,
                    x = O2LEVEL, fill = BREATH), alpha = 0.3)+    
    geom_line(data = newdata,
              aes(y = response, x = O2LEVEL, color = BREATH)) +
    scale_y_continuous('Buccal breathing rate', labels = function(x) 100*x) +
    theme_classic()

obs <- mullens %>%
    mutate(.fixed = predict(mullens.glmmTMB2b, re.form = NA),
           .resid = residuals(mullens.glmmTMB2b),
           PartialObs = plogis(.fixed + .resid))

ggplot() +
    geom_point(data = mullens, aes(y=pzBUC, x = O2LEVEL, color = BREATH), alpha=0.2) + 
    geom_point(data=obs, aes(y=PartialObs, x=O2LEVEL, color=BREATH)) +
    geom_ribbon(data=newdata,
                aes(ymin=lower.CL,ymax=upper.CL,
                    x=O2LEVEL, fill=BREATH), alpha=0.3)+    
    geom_line(data=newdata,
              aes(y=response, x=O2LEVEL, color=BREATH)) +
    scale_y_continuous('Buccal breathing rate', labels = function(x) 100*x) +
    theme_classic()

