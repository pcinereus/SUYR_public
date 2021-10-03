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
library(ggeffects) #for effects plots in ggplotjk
library(emmeans)   #for estimating marginal means
library(MASS)      #for glm.nb
library(MuMIn)     #for AICc
library(tidyverse) #for data wrangling
library(DHARMa)    #for assessing dispersion etc
library(glmmTMB)    #for glmmTMB
library(performance) #for diagnostic plots
library(see)         #for diagnostic plots
library(patchwork)   #for multiple plots


## ----readData, results='markdown', eval=TRUE----------------------------------
copper = read_csv('../data/copper.csv', trim_ws=TRUE)
glimpse(copper)


## ----dataProcessing, results='markdown', eval=TRUE, hidden=TRUE---------------
copper = copper %>%
    mutate(COPPER = factor(COPPER),
           PLATE = factor(PLATE),
           DIST = factor(DIST))


## ----eda1, results='markdown', eval=TRUE, hidden=TRUE-------------------------
ggplot(copper, aes(y = WORMS, x = DIST, fill = COPPER)) +
  geom_boxplot()


## ----eda2, results='markdown', eval=TRUE, hidden=TRUE-------------------------
ggplot(copper, aes(y = WORMS, x = DIST, fill = COPPER)) +
    geom_boxplot() +
    scale_y_continuous(trans = scales::pseudo_log_trans())


## ----eda3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=2.5----
g1 <- ggplot(copper, aes(y = WORMS, x = DIST, colour = COPPER)) +
  geom_point(position = position_jitter(height = 0, width = 0.1)) +
  scale_y_continuous() 
g2 <- ggplot(copper, aes(y = WORMS, x = DIST, colour = COPPER)) +
  geom_point(position = position_jitter(height = 0, width = 0.1)) +
  scale_y_continuous(trans = scales::pseudo_log_trans()) 
g3 <- ggplot(copper, aes(y = WORMS, x = DIST, colour = COPPER)) +
  geom_point(position = position_jitter(height = 0, width = 0.1)) +
  scale_y_sqrt() 
g1 + g2 + g3


## ----eda4, results='markdown', eval=TRUE, hidden=TRUE-------------------------
ggplot(copper, aes(y = WORMS, x = as.numeric(PLATE), color = DIST)) +
    geom_line() +
    facet_wrap(~COPPER)


## ----dataProcessing1, results='markdown', eval=TRUE, hidden=TRUE--------------
copper %>% filter(WORMS > 0) %>% summarize(min = min(WORMS)/2)
copper = copper %>% mutate(WORMSp = ifelse(WORMS == 0, 0.125, WORMS))


## ----github, results='markdown', eval=FALSE-----------------------------------
## remotes::install_github("glmmTMB/glmmTMB/glmmTMB")


## ----fitModel4b, results='markdown', eval=TRUE, hidden=TRUE-------------------
copper.glmmTMB1a <- glmmTMB(WORMS ~ COPPER*DIST + (1|PLATE),
                           dispformula = ~COPPER*DIST, 
                           data = copper,
                           family = gaussian(), 
                           REML = TRUE,
                           control=glmmTMBControl(optimizer=optim,
                                                  optArgs = list(method = 'BFGS'))
                           )

copper.glmmTMB1b <- glmmTMB(WORMS ~ COPPER*DIST + (DIST|PLATE),
                           dispformula = ~COPPER*DIST, 
                           data = copper,
                           family = gaussian(), 
                           REML = TRUE,
                           control=glmmTMBControl(optimizer=optim,
                                                  optArgs = list(method = 'BFGS'))
                           )
AICc(copper.glmmTMB1a, copper.glmmTMB1b)
## copper.glmmTMB1 <- update(copper.glmmTMB1, REML=TRUE)


## ----fitModel5b, results='markdown', eval=TRUE, hidden=TRUE-------------------
copper.glmmTMB2a <- glmmTMB(sqrt(WORMS) ~ COPPER*DIST + (1|PLATE),
                             dispformula = ~COPPER*DIST, 
                            data = copper,
                            family = gaussian(), 
                            REML = TRUE)
copper.glmmTMB2b <- glmmTMB(sqrt(WORMS) ~ COPPER*DIST + (DIST|PLATE),
                            dispformula = ~COPPER*DIST, 
                            data = copper,
                            family = gaussian(), 
                            REML = TRUE,
                            control=glmmTMBControl(optimizer=optim,
                                                  optArgs = list(method = 'BFGS'))
                            )
AICc(copper.glmmTMB2a,  copper.glmmTMB2b)


## ----fitModel1b, results='markdown', eval=TRUE, hidden=TRUE-------------------
copper.glmmTMB3a <- glmmTMB(WORMSp ~ COPPER*DIST + (1|PLATE),
                            dispformula = ~COPPER*DIST, 
                            data = copper,
                            family = gaussian(link='log'), 
                            REML=TRUE)
copper.glmmTMB3b <- glmmTMB(WORMSp ~ COPPER*DIST + (DIST|PLATE),
                            dispformula = ~COPPER*DIST, 
                            data = copper,
                            family = gaussian(link='log'), 
                            REML=TRUE,
                            control=glmmTMBControl(optimizer=optim,
                                                   optArgs = list(method = 'BFGS'))
                            )
AICc(copper.glmmTMB3a,  copper.glmmTMB3b)


## ----fitModel2b, results='markdown', eval=TRUE, hidden=TRUE-------------------
copper.glmmTMB4a <- glmmTMB(WORMSp ~ COPPER*DIST + (1|PLATE),
                            dispformula = ~COPPER*DIST, 
                            data = copper,
                            family = Gamma(link = 'log'),
                            REML = TRUE)
copper.glmmTMB4b <- glmmTMB(WORMSp ~ COPPER*DIST + (DIST|PLATE),
                            dispformula = ~COPPER*DIST, 
                            data = copper,
                            family = Gamma(link = 'log'),
                            REML = TRUE,
                            control=glmmTMBControl(optimizer=optim,
                                                   optArgs = list(method = 'BFGS'))
                            )
AICc(copper.glmmTMB4a,  copper.glmmTMB4b)


## ----fitModel3b, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
copper.glmmTMB5a <- glmmTMB(WORMS ~ COPPER*DIST + (1|PLATE),
                             dispformula = ~COPPER*DIST, 
                            data = copper,
                            family = tweedie(link='log'), 
                           REML = TRUE)
copper.glmmTMB5b <- glmmTMB(WORMS ~ COPPER*DIST + (DIST|PLATE),
                             dispformula = ~COPPER*DIST, 
                            data = copper,
                            family = tweedie(link='log'), 
                           REML = TRUE)
AICc(copper.glmmTMB5a,  copper.glmmTMB5b) 


## ----fitModel3bb, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE-----
copper.glmmTMB6a <- glmmTMB(WORMS ~ COPPER*DIST + (1|PLATE),
                            zi = ~1, 
                            dispformula = ~ COPPER*DIST,
                            data=copper,
                            family=ziGamma(link='log'), 
                            REML = TRUE)
copper.glmmTMB6b <- glmmTMB(WORMS ~ COPPER*DIST + (DIST|PLATE),
                            zi = ~1, 
                            dispformula = ~ COPPER*DIST,
                            data=copper,
                            family=ziGamma(link='log'), 
                            REML = TRUE,
                            control=glmmTMBControl(optimizer=optim,
                                                   optArgs = list(method = 'BFGS'))
                            )

AICc(copper.glmmTMB6a,  copper.glmmTMB6b) 


## ----validation4a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7, message=FALSE, warning=FALSE----
plot_model(copper.glmmTMB1a,  type='diag')[-2] %>% plot_grid


## ----validation4b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
copper.glmmTMB1a %>% performance::check_model()


## ----validation4c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
copper.glmmTMB1a %>% performance::check_distribution()


## ----validation4d, results='markdown', eval=TRUE, error=TRUE,hidden=TRUE, fig.width=7, fig.height=5, cache=FALSE, message=FALSE, warning=FALSE----
copper.resid <- copper.glmmTMB1a %>% simulateResiduals(plot=TRUE, integerResponse = TRUE)  


## ----validation5a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7, message=FALSE, warning=FALSE----
plot_model(copper.glmmTMB2a, type='diag')[-2] %>% plot_grid()


## ----validation5b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
copper.glmmTMB2a %>% performance::check_model()


## ----validation5c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
copper.glmmTMB2a %>% performance::check_distribution()


## ----validation5d, results='markdown', eval=TRUE, error=TRUE,hidden=TRUE, fig.width=7, fig.height=5, cache=FALSE, message=FALSE, warning=FALSE----
copper.resid <- copper.glmmTMB2a %>% simulateResiduals(plot=TRUE, integerResponse = FALSE)  


## ----validation1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7, message=FALSE, warning=FALSE----
plot_model(copper.glmmTMB3a,  type='diag')[-2] %>% plot_grid


## ----validation1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
copper.glmmTMB3a %>% performance::check_model()


## ----validation1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
copper.glmmTMB3a %>% performance::check_distribution()


## ----validation1d, results='markdown', eval=TRUE, error=TRUE,hidden=TRUE, fig.width=7, fig.height=5, cache=FALSE, message=FALSE, warning=FALSE----
copper.resid <- copper.glmmTMB3a %>% simulateResiduals(plot=TRUE, integerResponse = FALSE)  


## ----validation2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7, message=FALSE, warning=FALSE----
plot_model(copper.glmmTMB4a,  type='diag')[-2] %>% plot_grid


## ----validation2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
copper.glmmTMB4a %>% performance::check_model()


## ----validation2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
copper.glmmTMB4a %>% performance::check_distribution()


## ----validation2d, results='markdown', eval=TRUE, error=TRUE,hidden=TRUE, fig.width=7, fig.height=5, cache=FALSE, message=FALSE, warning=FALSE----
copper.resid <- copper.glmmTMB4a %>% simulateResiduals(  plot=TRUE, integerResponse = TRUE)  


## ----validation3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7, message=FALSE, warning=FALSE----
plot_model(copper.glmmTMB5a,  type='diag')[-2] %>% plot_grid


## ----validation3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
copper.glmmTMB5a %>% performance::check_model()


## ----validation3c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
copper.glmmTMB5a %>% performance::check_distribution()


## ----validation3d, results='markdown', eval=TRUE, error=TRUE,hidden=TRUE, fig.width=7, fig.height=5, cache=FALSE, message=FALSE, warning=FALSE----
copper.resid <- copper.glmmTMB5a %>% simulateResiduals(plot=TRUE, integerResponse = TRUE)


## ----validation3e, results='markdown', eval=TRUE, error=TRUE,hidden=TRUE, fig.width=7, fig.height=5, cache=FALSE, message=FALSE, warning=FALSE----
copper.resid <- copper.glmmTMB6a %>% simulateResiduals(  plot=TRUE, integerResponse = TRUE)


## ----partialPlot4a, results='markdown', eval=TRUE, hidden=TRUE----------------
copper.glmmTMB1a %>% plot_model(type='eff',  terms=c('DIST', 'COPPER'))


## ----partialPlot4b, results='markdown', eval=TRUE, hidden=TRUE----------------
copper.glmmTMB1a %>% allEffects() %>% plot(multiline=TRUE, ci.style='bars')


## ----partialPlot4c, results='markdown', eval=TRUE, hidden=TRUE----------------
copper.glmmTMB1a %>% ggpredict(c('DIST', 'COPPER')) %>% plot


## ----partialPlot4d, results='markdown', eval=TRUE, hidden=TRUE----------------
copper.glmmTMB1a %>% ggemmeans(~DIST+COPPER) %>% plot


## ----partialPlot5a, results='markdown', eval=TRUE, hidden=TRUE----------------
copper.glmmTMB2a %>% plot_model(type='eff',  terms=c('DIST', 'COPPER'))


## ----partialPlot5b, results='markdown', eval=TRUE, hidden=TRUE----------------
copper.glmmTMB2a %>% allEffects() %>% plot(multiline=TRUE, ci.style='bars')


## ----partialPlot5c, results='markdown', eval=TRUE, hidden=TRUE----------------
copper.glmmTMB2a %>% ggpredict(c('DIST', 'COPPER')) %>% plot


## ----partialPlot5d, results='markdown', eval=TRUE, hidden=TRUE----------------
copper.glmmTMB2a %>% ggemmeans(~DIST+COPPER) %>% plot


## ----partialPlot1a, results='markdown', eval=TRUE, hidden=TRUE----------------
plot_model(copper.glmmTMB3a,  type='eff',  terms=c('DIST', 'COPPER'))


## ----partialPlot1b, results='markdown', eval=TRUE, hidden=TRUE----------------
copper.glmmTMB3a %>% allEffects() %>%  plot(multiline=TRUE, ci.style='bars')
copper.glmmTMB3a %>% allEffects(transformation=NULL) %>% plot(multiline=TRUE, ci.style='bars')


## ----partialPlot1c, results='markdown', eval=TRUE, hidden=TRUE----------------
copper.glmmTMB3a %>% ggpredict(c('DIST', 'COPPER')) %>% plot


## ----partialPlot1d, results='markdown', eval=TRUE, hidden=TRUE----------------
copper.glmmTMB3a %>% ggemmeans(~DIST+COPPER) %>% plot


## ----partialPlot2a, results='markdown', eval=TRUE, hidden=TRUE----------------
copper.glmmTMB4a %>% plot_model(type='eff',  terms=c('DIST', 'COPPER'))


## ----partialPlot2b, results='markdown', eval=TRUE, hidden=TRUE----------------
copper.glmmTMB4a %>%allEffects() %>% plot(multiline=TRUE, ci.style='bars')
copper.glmmTMB4a %>% allEffects(transformation=NULL) %>% plot(multiline=TRUE, ci.style='bars')


## ----partialPlot2c, results='markdown', eval=TRUE, hidden=TRUE----------------
copper.glmmTMB4a %>% ggpredict(c('DIST', 'COPPER')) %>% plot


## ----partialPlot2d, results='markdown', eval=TRUE, hidden=TRUE----------------
copper.glmmTMB4a %>% ggemmeans(~DIST+COPPER) %>% plot


## ----partialPlot3a, results='markdown', eval=TRUE, hidden=TRUE----------------
copper.glmmTMB5a %>% plot_model(type='eff',  terms=c('DIST', 'COPPER'))


## ----partialPlot3b, results='markdown', eval=TRUE, hidden=TRUE----------------
copper.glmmTMB5a %>% allEffects() %>% plot(multiline=TRUE, ci.style='bars')
copper.glmmTMB5a %>% allEffects(transformation=NULL) %>% plot(multiline=TRUE, ci.style='bars')


## ----partialPlot3c, results='markdown', eval=TRUE, hidden=TRUE----------------
copper.glmmTMB5a %>% ggpredict(c('DIST', 'COPPER')) %>% plot


## ----partialPlot3d, results='markdown', eval=TRUE, hidden=TRUE----------------
copper.glmmTMB5a %>% ggemmeans(~DIST+COPPER) %>% plot


## ----summary4a, results='markdown', eval=TRUE, hidden=TRUE--------------------
copper.glmmTMB1a %>% summary()


## ----summary4b, results='markdown', eval=TRUE, echo=FALSE,hidden=TRUE---------
copper.tidy <- copper.glmmTMB1a %>% tidy()


## ----summary4c, results='markdown', eval=TRUE, hidden=TRUE--------------------
copper.glmmTMB1a %>% tidy(conf.int=TRUE)
copper.glmmTMB1a %>% tidy(conf.int=TRUE) %>% kable


## ----summary4d, results='markdown', eval=TRUE, hidden=TRUE--------------------
# warning this is only appropriate for html output
copper.glmmTMB1a %>% sjPlot::tab_model(show.se=TRUE, show.aic=TRUE)


## ----summary5a, results='markdown', eval=TRUE, hidden=TRUE--------------------
copper.glmmTMB2a %>% summary()


## ----summary5b, results='markdown', eval=TRUE, echo=FALSE,hidden=TRUE---------
copper.tidy <- copper.glmmTMB2a %>% tidy()


## ----summary5c, results='markdown', eval=TRUE, hidden=TRUE--------------------
copper.glmmTMB2a %>% tidy(conf.int=TRUE)
copper.glmmTMB2a %>% tidy(conf.int=TRUE) %>% kable


## ----summary5d, results='markdown', eval=TRUE, hidden=TRUE--------------------
# warning this is only appropriate for html output
copper.glmmTMB2a %>% sjPlot::tab_model(show.se=TRUE, show.aic=TRUE)


## ----summary1a, results='markdown', eval=TRUE, hidden=TRUE--------------------
copper.glmmTMB3a %>% summary()


## ----summary1b, results='markdown', eval=TRUE, echo=FALSE,hidden=TRUE---------
copper.tidy <- copper.glmmTMB3a %>% tidy()


## ----summary1c, results='markdown', eval=TRUE, hidden=TRUE--------------------
copper.glmmTMB3a %>% tidy(  conf.int=TRUE)
## or on the response scale
copper.glmmTMB3a %>% tidy(conf.int=TRUE, exponentiate = TRUE)
copper.glmmTMB3a %>% tidy(conf.int=TRUE, exponentiate = TRUE) %>% kable


## ----summary1d, results='markdown', eval=TRUE, hidden=TRUE--------------------
# warning this is only appropriate for html output
copper.glmmTMB3a %>% sjPlot::tab_model(show.se=TRUE, show.aic=TRUE)


## ----summary2a, results='markdown', eval=TRUE, hidden=TRUE--------------------
copper.glmmTMB4a %>% summary()


## ----summary2b, results='markdown', eval=TRUE, echo=FALSE,hidden=TRUE---------
copper.tidy <- copper.glmmTMB4a %>% tidy()


## ----summary2c, results='markdown', eval=TRUE, hidden=TRUE--------------------
copper.glmmTMB4a %>% tidy(conf.int=TRUE)
## or on the response scale
copper.glmmTMB4a %>% tidy(conf.int=TRUE, exponentiate = TRUE)
copper.glmmTMB4a %>% tidy(conf.int=TRUE, exponentiate = TRUE) %>% kable


## ----summary2d, results='markdown', eval=TRUE, hidden=TRUE--------------------
# warning this is only appropriate for html output
copper.glmmTMB4a %>% sjPlot::tab_model(show.se=TRUE, show.aic=TRUE)


## ----summary3a, results='markdown', eval=TRUE, hidden=TRUE--------------------
copper.glmmTMB5a %>% summary()


## ----summary3b, results='markdown', eval=TRUE, echo=FALSE,hidden=TRUE---------
copper.tidy <- copper.glmmTMB5a %>% tidy()


## ----summary3c, results='markdown', eval=TRUE, hidden=TRUE--------------------
copper.glmmTMB5a %>% tidy(conf.int=FALSE)
## or on the response scale
copper.glmmTMB5a %>% tidy(conf.int=FALSE, exponentiate = TRUE)
copper.glmmTMB5a %>% tidy(conf.int=FALSE, exponentiate = TRUE) %>% kable


## ----summary3d, results='markdown', eval=TRUE, hidden=TRUE--------------------
# warning this is only appropriate for html output
copper.glmmTMB5a %>% sjPlot::tab_model(show.se=TRUE, show.aic=TRUE)


## ----furtherAnalysis1a, results='markdown', echo=TRUE, eval=TRUE, hidden=TRUE----
copper.glmmTMB4a %>% emmeans(~COPPER|DIST, type='response') %>% pairs()
## copper.glmmTMB4a %>% emmeans(~COPPER|DIST, type='response') %>% contrast(method='pairwise')


## ----furtherAnalysis1b, results='markdown', echo=TRUE, eval=TRUE, hidden=TRUE----
copper.glmmTMB4a %>% emmeans(~DIST|COPPER, type='response') %>% contrast(method='poly')
## copper.glmmTMB4a %>% emmeans(~DIST|COPPER) %>% regrid() %>% contrast(method='poly')


## ----summaryFigure, results='markdown', echo=TRUE, eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
newdata <- copper.glmmTMB4a %>%
    emmeans(~COPPER|DIST, type = 'response') %>%
    summary(infer=TRUE) %>% 
    as.data.frame

head(newdata)
g1 = ggplot(newdata, aes(y=response, x=DIST, fill=COPPER)) +
    geom_pointrange(aes(ymin=lower.CL, ymax=upper.CL),shape=21,
                    position=position_dodge(width=0.2)) +
    theme_bw()
copper.comp <- copper.glmmTMB4a %>%
    emmeans(~COPPER|DIST,  type='response') %>%
    pairs() %>% 
    ## contrast(method='pairwise') %>%
    summary(infer=TRUE) %>%
    as.data.frame                                                                           
head(copper.comp)

g2 <- ggplot(copper.comp, aes(y = ratio, x = contrast, color = DIST)) +
    geom_pointrange(aes(ymin = lower.CL, ymax = upper.CL),
                    position = position_dodge(width = 0.6)) +
    geom_hline(yintercept = 1) +
    scale_y_continuous(trans = scales::log2_trans(),  limits = c(0.25, 500)) +
    coord_flip() +
    theme_bw()

g1 + g2

