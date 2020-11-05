## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE)


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
copper = copper %>% mutate(COPPER=factor(COPPER),
                           PLATE=factor(PLATE),
                           DIST=factor(DIST))


## ----eda1, results='markdown', eval=TRUE, hidden=TRUE-------------------------
ggplot(copper, aes(y=WORMS, x=DIST, fill=COPPER)) +
  geom_boxplot()


## ----eda2, results='markdown', eval=TRUE, hidden=TRUE-------------------------
ggplot(copper, aes(y=WORMS, x=DIST, fill=COPPER)) +
    geom_boxplot() +
    scale_y_continuous(trans=scales::pseudo_log_trans())


## ----eda3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=2.5----
g1 <- ggplot(copper, aes(y=WORMS, x=DIST, colour=COPPER)) +
  geom_point(position=position_jitter(height=0, width=0.1)) +
  scale_y_continuous() 
g2 <- ggplot(copper, aes(y=WORMS, x=DIST, colour=COPPER)) +
  geom_point(position=position_jitter(height=0, width=0.1)) +
  scale_y_continuous(trans=scales::pseudo_log_trans()) 
g3 <- ggplot(copper, aes(y=WORMS, x=DIST, colour=COPPER)) +
  geom_point(position=position_jitter(height=0, width=0.1)) +
  scale_y_sqrt() 
g1 + g2 + g3


## ----eda4, results='markdown', eval=TRUE, hidden=TRUE-------------------------
ggplot(copper, aes(y=WORMS, x=as.numeric(PLATE), color=DIST)) +
    geom_line() + facet_wrap(~COPPER)


## ----dataProcessing1, results='markdown', eval=TRUE, hidden=TRUE--------------
copper %>% filter(WORMS>0) %>% summarize(min=min(WORMS)/2)
copper = copper %>% mutate(WORMSp = ifelse(WORMS==0, 0.125, WORMS))


## ----github, results='markdown', eval=FALSE-----------------------------------
## remotes::install_github("glmmTMB/glmmTMB/glmmTMB")


## ----fitModel4a, results='markdown', eval=TRUE, hidden=TRUE-------------------
copper.glmmTMB4.1 <- glmmTMB(WORMS ~ COPPER*DIST + (1|PLATE),
                             dispformula = ~COPPER*DIST, 
                             data=copper, family=gaussian(), 
                           REML=FALSE)
copper.glmmTMB4.2 <- update(copper.glmmTMB4.1,  ~.-COPPER:DIST)
AICc(copper.glmmTMB4.1,  copper.glmmTMB4.2)


## ----fitModel4b, results='markdown', eval=TRUE, hidden=TRUE-------------------
copper.glmmTMB4.2a <- update(copper.glmmTMB4.2, REML=TRUE)
copper.glmmTMB4.2b <- update(copper.glmmTMB4.2a,  ~.-(1|PLATE) + (DIST|PLATE))
AICc(copper.glmmTMB4.2a,  copper.glmmTMB4.2b)


## ----fitModel5a, results='markdown', eval=TRUE, hidden=TRUE-------------------
copper.glmmTMB5.1 <- glmmTMB(sqrt(WORMS) ~ COPPER*DIST + (1|PLATE),
                             dispformula = ~COPPER*DIST, 
                             data=copper, family=gaussian(), 
                           REML=FALSE)
copper.glmmTMB5.2 <- update(copper.glmmTMB5.1,  ~.-COPPER:DIST)
AICc(copper.glmmTMB5.1,  copper.glmmTMB5.2)


## ----fitModel5b, results='markdown', eval=TRUE, hidden=TRUE-------------------
copper.glmmTMB5.1a <- update(copper.glmmTMB5.1, REML=TRUE)
copper.glmmTMB5.1b <- update(copper.glmmTMB5.1a,  ~.-(1|PLATE) + (DIST|PLATE))
AICc(copper.glmmTMB5.1a,  copper.glmmTMB5.1b)


## ----fitModel1a, results='markdown', eval=TRUE, hidden=TRUE-------------------
copper.glmmTMB1.1 <- glmmTMB(WORMSp ~ COPPER*DIST + (1|PLATE),
                             dispformula = ~COPPER*DIST, 
                             data=copper, family=gaussian(link='log'), 
                           REML=FALSE)
copper.glmmTMB1.2 <- update(copper.glmmTMB1.1,  ~.-COPPER:DIST)
AICc(copper.glmmTMB1.1,  copper.glmmTMB1.2)


## ----fitModel1b, results='markdown', eval=TRUE, hidden=TRUE-------------------
copper.glmmTMB1.1a <- update(copper.glmmTMB1.1, REML=TRUE)
copper.glmmTMB1.1b <- update(copper.glmmTMB1.1a,  ~.-(1|PLATE) + (DIST|PLATE))
AICc(copper.glmmTMB1.1a,  copper.glmmTMB1.1b)


## ----fitModel2a, results='markdown', eval=TRUE, hidden=TRUE-------------------
copper.glmmTMB2.1 <- glmmTMB(WORMSp ~ COPPER*DIST + (1|PLATE),
                             dispformula = ~COPPER*DIST, 
                           data=copper, family=Gamma(link='log'),
                           REML=FALSE)
copper.glmmTMB2.2 <- update(copper.glmmTMB2.1,  ~.-COPPER:DIST)
AICc(copper.glmmTMB2.1,  copper.glmmTMB2.2)


## ----fitModel2b, results='markdown', eval=TRUE, hidden=TRUE-------------------
copper.glmmTMB2.1a <- update(copper.glmmTMB2.1, REML=TRUE)
copper.glmmTMB2.1b <- update(copper.glmmTMB2.1a,  ~.-(1|PLATE) + (DIST|PLATE))
AICc(copper.glmmTMB2.1a,  copper.glmmTMB2.1b)


## ----fitModel3a, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
copper.glmmTMB3.1 <- glmmTMB(WORMS ~ COPPER*DIST + (1|PLATE),
                             dispformula = ~COPPER*DIST, 
                             data=copper, family=tweedie(link='log'), 
                           REML=FALSE)
copper.glmmTMB3.2 <- update(copper.glmmTMB3.1,  ~.-COPPER:DIST)
AICc(copper.glmmTMB3.1,  copper.glmmTMB3.2) 


## ----fitModel3b, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE------
copper.glmmTMB3.1a <- update(copper.glmmTMB3.1, REML=TRUE)
copper.glmmTMB3.1b <- update(copper.glmmTMB3.1a,  ~.-(1|PLATE) + (DIST|PLATE))
AICc(copper.glmmTMB3.1a,  copper.glmmTMB3.1b) 


## ----validation4a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7, message=FALSE, warning=FALSE----
plot_model(copper.glmmTMB4.2a,  type='diag')[-2] %>% plot_grid


## ----validation4b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
performance::check_model(copper.glmmTMB4.2a)


## ----validation4c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
performance::check_distribution(copper.glmmTMB4.2a)


## ----validation4d, results='markdown', eval=TRUE, error=TRUE,hidden=TRUE, fig.width=7, fig.height=5, cache=FALSE, message=FALSE, warning=FALSE----
owls.resid = simulateResiduals(copper.glmmTMB4.2a,  plot=TRUE, integerResponse = TRUE)  


## ----validation5a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7, message=FALSE, warning=FALSE----
plot_model(copper.glmmTMB5.1a,  type='diag')[-2] %>% plot_grid


## ----validation5b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
performance::check_model(copper.glmmTMB5.1a)


## ----validation5c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
performance::check_distribution(copper.glmmTMB5.1a)


## ----validation5d, results='markdown', eval=TRUE, error=TRUE,hidden=TRUE, fig.width=7, fig.height=5, cache=FALSE, message=FALSE, warning=FALSE----
owls.resid = simulateResiduals(copper.glmmTMB5.1a,  plot=TRUE, integerResponse = TRUE)  


## ----validation1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7, message=FALSE, warning=FALSE----
plot_model(copper.glmmTMB1.1a,  type='diag')[-2] %>% plot_grid


## ----validation1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
performance::check_model(copper.glmmTMB1.1a)


## ----validation1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
performance::check_distribution(copper.glmmTMB1.1a)


## ----validation1d, results='markdown', eval=TRUE, error=TRUE,hidden=TRUE, fig.width=7, fig.height=5, cache=FALSE, message=FALSE, warning=FALSE----
owls.resid = simulateResiduals(copper.glmmTMB1.1a,  plot=TRUE, integerResponse = TRUE)  


## ----validation2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7, message=FALSE, warning=FALSE----
plot_model(copper.glmmTMB2.1a,  type='diag')[-2] %>% plot_grid


## ----validation2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
performance::check_model(copper.glmmTMB2.1a)


## ----validation2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
performance::check_distribution(copper.glmmTMB2.1a)


## ----validation2d, results='markdown', eval=TRUE, error=TRUE,hidden=TRUE, fig.width=7, fig.height=5, cache=FALSE, message=FALSE, warning=FALSE----
owls.resid = simulateResiduals(copper.glmmTMB2.1a,  plot=TRUE, integerResponse = TRUE)  


## ----validation3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7, message=FALSE, warning=FALSE----
plot_model(copper.glmmTMB3.1a,  type='diag')[-2] %>% plot_grid


## ----validation3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
performance::check_model(copper.glmmTMB3.1a)


## ----validation3c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
performance::check_distribution(copper.glmmTMB3.1a)


## ----validation3d, results='markdown', eval=TRUE, error=TRUE,hidden=TRUE, fig.width=7, fig.height=5, cache=FALSE, message=FALSE, warning=FALSE----
owls.resid = simulateResiduals(copper.glmmTMB3.1a,  plot=TRUE, integerResponse = TRUE)


## ----partialPlot4a, results='markdown', eval=TRUE, hidden=TRUE----------------
plot_model(copper.glmmTMB4.2a,  type='eff',  terms=c('DIST', 'COPPER'))


## ----partialPlot4b, results='markdown', eval=TRUE, hidden=TRUE----------------
plot(allEffects(copper.glmmTMB4.2a),  multiline=TRUE, ci.style='bars')


## ----partialPlot4c, results='markdown', eval=TRUE, hidden=TRUE----------------
ggpredict(copper.glmmTMB4.2a,  c('DIST', 'COPPER')) %>% plot


## ----partialPlot4d, results='markdown', eval=TRUE, hidden=TRUE----------------
ggemmeans(copper.glmmTMB4.2a,  ~DIST+COPPER) %>% plot


## ----partialPlot5a, results='markdown', eval=TRUE, hidden=TRUE----------------
plot_model(copper.glmmTMB5.1a,  type='eff',  terms=c('DIST', 'COPPER'))


## ----partialPlot5b, results='markdown', eval=TRUE, hidden=TRUE----------------
plot(allEffects(copper.glmmTMB5.1a),  multiline=TRUE, ci.style='bars')


## ----partialPlot5c, results='markdown', eval=TRUE, hidden=TRUE----------------
ggpredict(copper.glmmTMB5.1a,  c('DIST', 'COPPER')) %>% plot


## ----partialPlot5d, results='markdown', eval=TRUE, hidden=TRUE----------------
ggemmeans(copper.glmmTMB5.1a,  ~DIST+COPPER) %>% plot


## ----partialPlot1a, results='markdown', eval=TRUE, hidden=TRUE----------------
plot_model(copper.glmmTMB1.1a,  type='eff',  terms=c('DIST', 'COPPER'))


## ----partialPlot1b, results='markdown', eval=TRUE, hidden=TRUE----------------
plot(allEffects(copper.glmmTMB1.1a),  multiline=TRUE, ci.style='bars')
plot(allEffects(copper.glmmTMB1.1a, transformation=NULL),  multiline=TRUE, ci.style='bars')


## ----partialPlot1c, results='markdown', eval=TRUE, hidden=TRUE----------------
ggpredict(copper.glmmTMB1.1a,  c('DIST', 'COPPER')) %>% plot


## ----partialPlot1d, results='markdown', eval=TRUE, hidden=TRUE----------------
ggemmeans(copper.glmmTMB1.1a,  ~DIST+COPPER) %>% plot


## ----partialPlot2a, results='markdown', eval=TRUE, hidden=TRUE----------------
plot_model(copper.glmmTMB2.1a,  type='eff',  terms=c('DIST', 'COPPER'))


## ----partialPlot2b, results='markdown', eval=TRUE, hidden=TRUE----------------
plot(allEffects(copper.glmmTMB2.1a),  multiline=TRUE, ci.style='bars')
plot(allEffects(copper.glmmTMB2.1a, transformation=NULL),  multiline=TRUE, ci.style='bars')


## ----partialPlot2c, results='markdown', eval=TRUE, hidden=TRUE----------------
ggpredict(copper.glmmTMB2.1a,  c('DIST', 'COPPER')) %>% plot


## ----partialPlot2d, results='markdown', eval=TRUE, hidden=TRUE----------------
ggemmeans(copper.glmmTMB2.1a,  ~DIST+COPPER) %>% plot


## ----partialPlot3a, results='markdown', eval=TRUE, hidden=TRUE----------------
plot_model(copper.glmmTMB3.1a,  type='eff',  terms=c('DIST', 'COPPER'))


## ----partialPlot3b, results='markdown', eval=TRUE, hidden=TRUE----------------
plot(allEffects(copper.glmmTMB3.1a),  multiline=TRUE, ci.style='bars')
plot(allEffects(copper.glmmTMB3.1a, transformation=NULL),  multiline=TRUE, ci.style='bars')


## ----partialPlot3c, results='markdown', eval=TRUE, hidden=TRUE----------------
ggpredict(copper.glmmTMB3.1a,  c('DIST', 'COPPER')) %>% plot


## ----partialPlot3d, results='markdown', eval=TRUE, hidden=TRUE----------------
ggemmeans(copper.glmmTMB3.1a,  ~DIST+COPPER) %>% plot


## ----summary4a, results='markdown', eval=TRUE, hidden=TRUE--------------------
summary(copper.glmmTMB4.2a)


## ----summary4b, results='markdown', eval=TRUE, echo=FALSE,hidden=TRUE---------
copper.tidy <- tidy(copper.glmmTMB4.2a)


## ----summary4c, results='markdown', eval=TRUE, hidden=TRUE--------------------
tidy(copper.glmmTMB4.2a,  conf.int=TRUE)
tidy(copper.glmmTMB4.2a,  conf.int=TRUE) %>% kable


## ----summary4d, results='markdown', eval=TRUE, hidden=TRUE--------------------
# warning this is only appropriate for html output
sjPlot::tab_model(copper.glmmTMB4.2a, show.se=TRUE, show.aic=TRUE)


## ----summary5a, results='markdown', eval=TRUE, hidden=TRUE--------------------
summary(copper.glmmTMB5.1a)


## ----summary5b, results='markdown', eval=TRUE, echo=FALSE,hidden=TRUE---------
copper.tidy <- tidy(copper.glmmTMB5.1a)


## ----summary5c, results='markdown', eval=TRUE, hidden=TRUE--------------------
tidy(copper.glmmTMB5.1a,  conf.int=TRUE)
tidy(copper.glmmTMB5.1a,  conf.int=TRUE) %>% kable


## ----summary5d, results='markdown', eval=TRUE, hidden=TRUE--------------------
# warning this is only appropriate for html output
sjPlot::tab_model(copper.glmmTMB5.1a, show.se=TRUE, show.aic=TRUE)


## ----summary1a, results='markdown', eval=TRUE, hidden=TRUE--------------------
summary(copper.glmmTMB1.1a)


## ----summary1b, results='markdown', eval=TRUE, echo=FALSE,hidden=TRUE---------
copper.tidy <- tidy(copper.glmmTMB1.1a)


## ----summary1c, results='markdown', eval=TRUE, hidden=TRUE--------------------
tidy(copper.glmmTMB1.1a,  conf.int=TRUE)
## or on the response scale
tidy(copper.glmmTMB1.1a,  conf.int=TRUE, exponentiate = TRUE)
tidy(copper.glmmTMB1.1a,  conf.int=TRUE, exponentiate = TRUE) %>% kable


## ----summary1d, results='markdown', eval=TRUE, hidden=TRUE--------------------
# warning this is only appropriate for html output
sjPlot::tab_model(copper.glmmTMB1.1a, show.se=TRUE, show.aic=TRUE)


## ----summary2a, results='markdown', eval=TRUE, hidden=TRUE--------------------
summary(copper.glmmTMB2.1a)


## ----summary2b, results='markdown', eval=TRUE, echo=FALSE,hidden=TRUE---------
copper.tidy <- tidy(copper.glmmTMB2.1a)


## ----summary2c, results='markdown', eval=TRUE, hidden=TRUE--------------------
tidy(copper.glmmTMB2.1a,  conf.int=TRUE)
## or on the response scale
tidy(copper.glmmTMB2.1a,  conf.int=TRUE, exponentiate = TRUE)
tidy(copper.glmmTMB2.1a,  conf.int=TRUE, exponentiate = TRUE) %>% kable


## ----summary2d, results='markdown', eval=TRUE, hidden=TRUE--------------------
# warning this is only appropriate for html output
sjPlot::tab_model(copper.glmmTMB2.1a, show.se=TRUE, show.aic=TRUE)


## ----summary3a, results='markdown', eval=TRUE, hidden=TRUE--------------------
summary(copper.glmmTMB3.1a)


## ----summary3b, results='markdown', eval=TRUE, echo=FALSE,hidden=TRUE---------
copper.tidy <- tidy(copper.glmmTMB3.1a)


## ----summary3c, results='markdown', eval=TRUE, hidden=TRUE--------------------
tidy(copper.glmmTMB3.1a,  conf.int=FALSE)
## or on the response scale
tidy(copper.glmmTMB3.1a,  conf.int=FALSE, exponentiate = TRUE)
tidy(copper.glmmTMB3.1a,  conf.int=FALSE, exponentiate = TRUE) %>% kable


## ----summary3d, results='markdown', eval=TRUE, hidden=TRUE--------------------
# warning this is only appropriate for html output
sjPlot::tab_model(copper.glmmTMB3.1a, show.se=TRUE, show.aic=TRUE)


## ----furtherAnalysis1a, results='markdown', echo=TRUE, eval=TRUE, hidden=TRUE----
emmeans(copper.glmmTMB3.1a, ~COPPER|DIST, type='response') %>% contrast(method='pairwise')


## ----furtherAnalysis1b, results='markdown', echo=TRUE, eval=TRUE, hidden=TRUE----
emmeans(copper.glmmTMB3.1a, ~DIST|COPPER, type='response') %>% contrast(method='poly')


## ----summaryFigure, results='markdown', echo=TRUE, eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
copper.comp <- emmeans(copper.glmmTMB3.1a,  ~COPPER|DIST,  type='response') %>%
  contrast(method='pairwise') %>%
    confint() %>%
    as.data.frame                                                                           
head(copper.comp)

g1=ggplot(copper.comp, aes(y=ratio, x=contrast, color=DIST)) +
    geom_pointrange(aes(ymin=lower.CL, ymax=upper.CL),
                    position=position_dodge(width=0.6)) +
    geom_hline(yintercept=1) +
  scale_y_continuous(trans=scales::log2_trans(),  limits=c(0.25, 500)) +
    coord_flip() +
    theme_bw()

newdata = emmeans(copper.glmmTMB3.1a, ~COPPER|DIST,
                  type='response') %>%
  confint() %>% 
  as.data.frame

head(newdata)
g2 = ggplot(newdata, aes(y=response, x=DIST, fill=COPPER)) +
  geom_pointrange(aes(ymin=lower.CL, ymax=upper.CL),shape=21, position=position_dodge(width=0.2)) +
  theme_bw()
g2 + g1


