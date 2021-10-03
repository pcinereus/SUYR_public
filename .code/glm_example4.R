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
library(ggeffects) #for partial effects plots
library(MASS)      #for glm.nb
library(MuMIn)     #for AICc
library(DHARMa)    #for residual diagnostics plots
library(modelr)    #for auxillary modelling functions
library(performance) #for residuals diagnostics
library(see)         #for plotting residuals
library(patchwork)   #for grids of plots
library(tidyverse) #for data wrangling


## ----readData, results='markdown', eval=TRUE----------------------------------
loyn = read_csv('../data/loyn.csv', trim_ws=TRUE)
glimpse(loyn)


## ----EDA, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
scatterplotMatrix(~ABUND+DIST+LDIST+AREA+GRAZE+ALT+YR.ISOL, data=loyn,
                  diagonal = list(method='boxplot'))


## ----EDA1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
scatterplotMatrix(~ABUND+log(DIST)+log(LDIST)+log(AREA)+GRAZE+ALT+YR.ISOL, data=loyn,
                  diagonal = list(method='boxplot'))


## ----prepareData, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
loyn = loyn %>% mutate(fGRAZE=factor(GRAZE))


## ----EDA2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, warning=FALSE, message=FALSE----
p1=ggplot(loyn, aes(y=ABUND, x=DIST, color=fGRAZE)) +
    geom_smooth(method='lm') +
  scale_x_log10()

p2=ggplot(loyn, aes(y=ABUND, x=AREA, color=fGRAZE)) +
    geom_smooth(method='lm') +
  scale_x_log10()

p1 + p2


## ----EDA3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=5, warning=FALSE, message=FALSE----
## might need to consider scaling separtely within each group
loyn = loyn %>%
  group_by(fGRAZE) %>%
  mutate(rslAREA = scales::rescale(log(AREA)),
         slAREA = scale(log(AREA))) %>%
  ungroup()

ggplot(loyn, aes(y=ABUND, x=slAREA, color=fGRAZE)) +
    geom_smooth(method='lm') #+

ggplot(loyn, aes(y=ABUND, x=rslAREA, color=fGRAZE)) +
    geom_smooth(method='lm') #+
     


## ----fitModel1, results='markdown', eval=TRUE, hidden=TRUE--------------------
loyn.glm <- glm(log(ABUND)~scale(log(DIST), scale=FALSE) + scale(log(LDIST), scale=FALSE) +
                scale(log(AREA), scale=FALSE)+
                fGRAZE + scale(ALT, scale=FALSE) + scale(YR.ISOL, scale=FALSE),
              data=loyn, family=gaussian())


## ----fitModel2, results='markdown', eval=TRUE, hidden=TRUE--------------------
loyn.glm1 <- glm(ABUND~scale(log(DIST), scale=FALSE) + scale(log(LDIST), scale=FALSE) +
                  scale(log(AREA), scale=FALSE)+
                  fGRAZE + scale(ALT, scale=FALSE) + scale(YR.ISOL, scale=FALSE),
              data=loyn, family=gaussian(link='log'))


## ----fitModel3, results='markdown', eval=TRUE, hidden=TRUE--------------------
loyn.glm2 <- glm(ABUND~scale(log(DIST), scale=FALSE) + scale(log(LDIST), scale=FALSE) +
                   scale(log(AREA), scale=FALSE)+
                   fGRAZE + scale(ALT, scale=FALSE) + scale(YR.ISOL, scale=FALSE),
               data=loyn, family=Gamma(link='log'))


## ----validateModel1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
loyn.glm %>% vif()
## If any terms have df>1, then Generalized VIF calculated
## This is the inflation in size compared to what would be expected if orthogonal


## ----validateModel1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
loyn.glm1 %>% vif()
## If any terms have df>1, then Generalized VIF calculated
## This is the inflation in size compared to what would be expected if orthogonal


## ----validateModel1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
loyn.glm2 %>% vif()
## If any terms have df>1, then Generalized VIF calculated
## This is the inflation in size compared to what would be expected if orthogonal


## ----validateModel2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6, warning=FALSE, message=FALSE----
loyn.glm %>% autoplot(which=1:6)


## ----validateModel2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6, warning=FALSE, message=FALSE----
loyn.glm1 %>% autoplot(which=1:6)


## ----validateModel2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6, warning=FALSE, message=FALSE----
loyn.glm2 %>% autoplot(which=1:6)


## ----validateModel3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6, warning=FALSE, message=FALSE----
loyn.glm %>% performance::check_model()


## ----validateModel3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6, warning=FALSE, message=FALSE----
loyn.glm1 %>% performance::check_model()


## ----validateModel3c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6, warning=FALSE, message=FALSE----
loyn.glm2 %>% performance::check_model()


## ----validateModel4a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=6, warning=FALSE, message=FALSE----
loyn.resid <- loyn.glm %>% simulateResiduals(plot=TRUE)


## ----validateModel4b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=6, warning=FALSE, message=FALSE----
loyn.resid1 <- loyn.glm1 %>% simulateResiduals(plot=TRUE)


## ----validateModel4c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=6, warning=FALSE, message=FALSE----
loyn.resid2 <- loyn.glm2 %>% simulateResiduals(plot=TRUE)


## ----validateModel5a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=9, fig.height=6, warning=FALSE, message=FALSE----
loyn = loyn %>%
  mutate(Resids = loyn.resid$scaledResiduals)

loyn %>%
  mutate(clDIST=scale(log(DIST), scale=FALSE),
         clLDIST=scale(log(LDIST), scale=FALSE),
         clAREA=scale(log(AREA), scale=FALSE),
         cALT=scale(ALT, scale=FALSE),
         cYR.ISOL=scale(YR.ISOL, scale=FALSE)) %>%
  dplyr::select(clDIST, clLDIST, clAREA, fGRAZE, cALT, cYR.ISOL, Resids) %>%
  pivot_longer(c(clDIST, clLDIST, clAREA, cALT, cYR.ISOL)) %>%
  ggplot() +
  geom_point(aes(y=Resids,  x=value, color=fGRAZE)) +
  facet_wrap(~name, scales='free')



## ----validateModel5b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=9, fig.height=6, warning=FALSE, message=FALSE----
loyn = loyn %>%
  mutate(Resids = loyn.resid1$scaledResiduals)

loyn %>%
  mutate(clDIST=scale(log(DIST), scale=FALSE),
         clLDIST=scale(log(LDIST), scale=FALSE),
         clAREA=scale(log(AREA), scale=FALSE),
         cALT=scale(ALT, scale=FALSE),
         cYR.ISOL=scale(YR.ISOL, scale=FALSE)) %>%
  dplyr::select(clDIST, clLDIST, clAREA, fGRAZE, cALT, cYR.ISOL, Resids) %>%
  pivot_longer(c(clDIST, clLDIST, clAREA, cALT, cYR.ISOL)) %>%
  ggplot() +
  geom_point(aes(y=Resids,  x=value, color=fGRAZE)) +
  facet_wrap(~name, scales='free')



## ----validateModel5c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=9, fig.height=6, warning=FALSE, message=FALSE----
loyn = loyn %>%
  mutate(Resids = loyn.resid2$scaledResiduals)

loyn %>%
  mutate(clDIST=scale(log(DIST), scale=FALSE),
         clLDIST=scale(log(LDIST), scale=FALSE),
         clAREA=scale(log(AREA), scale=FALSE),
         cALT=scale(ALT, scale=FALSE),
         cYR.ISOL=scale(YR.ISOL, scale=FALSE)) %>%
  dplyr::select(clDIST, clLDIST, clAREA, fGRAZE, cALT, cYR.ISOL, Resids) %>%
  pivot_longer(c(clDIST, clLDIST, clAREA, cALT, cYR.ISOL)) %>%
  ggplot() +
  geom_point(aes(y=Resids,  x=value, color=fGRAZE)) +
  facet_wrap(~name, scales='free')



## ----plotModel3a1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8, message=FALSE, warning=FALSE----
## Effects
loyn.glm %>% plot_model(type='eff', show.data=TRUE, dot.size=0.5) %>% plot_grid
## Predictions
loyn.glm %>% plot_model(type='pred', show.data=TRUE, dot.size=0.5) %>% plot_grid


## ----plotModel3a2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8, message=FALSE, warning=FALSE----
## We can sort of do part of the backtransforms - but only partly..
plot_grid(list(
    loyn.glm %>% plot_model(type='eff',  terms='DIST') + scale_x_log10(),
    loyn.glm %>% plot_model(type='eff',  terms='LDIST') + scale_x_log10(),
    loyn.glm %>% plot_model(type='eff',  terms='AREA') + scale_x_log10(),
    loyn.glm %>% plot_model(type='eff',  terms='fGRAZE'),
    loyn.glm %>% plot_model(type='eff',  terms='ALT'),
    loyn.glm %>% plot_model(type='eff',  terms='YR.ISOL')  
))


## ----plotModel3a3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8, message=FALSE, warning=FALSE----
## Effects
loyn.glm1 %>% plot_model(type='eff', show.data=TRUE, dot.size=0.5) %>% plot_grid
## Predictions
loyn.glm1 %>% plot_model(type='pred', show.data=TRUE, dot.size=0.5) %>% plot_grid


## ----plotModel3a4, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8, message=FALSE, warning=FALSE----
## We can sort of do part of the backtransforms - but only partly..
plot_grid(list(
    loyn.glm1 %>% plot_model(type='eff',  terms='DIST') + scale_x_log10(),
    loyn.glm1 %>% plot_model(type='eff',  terms='LDIST') + scale_x_log10(),
    loyn.glm1 %>% plot_model(type='eff',  terms='AREA') + scale_x_log10(),
    loyn.glm1 %>% plot_model(type='eff',  terms='fGRAZE'),
    loyn.glm1 %>% plot_model(type='eff',  terms='ALT'),
    loyn.glm1 %>% plot_model(type='eff',  terms='YR.ISOL')  
))


## ----plotModel3a5, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8, message=FALSE, warning=FALSE----
## Effects
loyn.glm2 %>% plot_model(type='eff', show.data=TRUE, dot.size=0.5) %>% plot_grid
## Predictions
loyn.glm2 %>% plot_model(type='pred', show.data=TRUE, dot.size=0.5) %>% plot_grid


## ----plotModel3a6, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8, message=FALSE, warning=FALSE----
## We can sort of do part of the backtransforms - but only partly..
plot_grid(list(
    loyn.glm2 %>% plot_model(type='eff',  terms='DIST') + scale_x_log10(),
    loyn.glm2 %>% plot_model(type='eff',  terms='LDIST') + scale_x_log10(),
    loyn.glm2 %>% plot_model(type='eff',  terms='AREA') + scale_x_log10(),
    loyn.glm2 %>% plot_model(type='eff',  terms='fGRAZE'),
    loyn.glm2 %>% plot_model(type='eff',  terms='ALT'),
    loyn.glm2 %>% plot_model(type='eff',  terms='YR.ISOL')  
))


## ----plotModel3c1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8, message=FALSE, warning=FALSE----
loyn.glm %>% allEffects(residuals=TRUE) %>% plot(type='response')


## ----plotModel3c2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8, message=FALSE, warning=FALSE----
loyn.glm1 %>% allEffects(residuals=TRUE) %>% plot(type='response')


## ----plotModel3c3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8, message=FALSE, warning=FALSE----
loyn.glm2 %>% allEffects(residuals=TRUE) %>% plot(type='response')


## ----plotModel3d1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8, message=FALSE, warning=FALSE----
loyn.glm %>% ggpredict() %>%
  plot(add.data=TRUE, facet=TRUE, jitter=FALSE)


## ----plotModel3d2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8, message=FALSE, warning=FALSE----
loyn.glm1 %>% ggpredict() %>%
  plot(add.data=TRUE, facet=TRUE, jitter=FALSE)


## ----plotModel3d3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8, message=FALSE, warning=FALSE----
loyn.glm2 %>% ggpredict() %>%
  plot(add.data=TRUE, facet=TRUE, jitter=FALSE)


## ----plotModel3e1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8, message=FALSE, warning=FALSE----
loyn.glm %>% ggemmeans(~AREA|fGRAZE) %>%
    plot(add.data=TRUE, jitter=FALSE)

loyn.glm %>% ggemmeans(~AREA|fGRAZE) %>%
    plot(add.data=TRUE, jitter=FALSE) +
    scale_x_log10()
## unfortunately, it is not currently possible to influence the
## prediction grid so we are stuck with awful looking figures.


## ----plotModel3e2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8, message=FALSE, warning=FALSE----
loyn.glm1 %>% ggemmeans(~AREA|fGRAZE) %>%
    plot(add.data=TRUE, jitter=FALSE) 

loyn.glm1 %>% ggemmeans(~AREA|fGRAZE) %>%
    plot(add.data=TRUE, jitter=FALSE) +
    scale_x_log10()
## unfortunately, it is not currently possible to influence the
## prediction grid so we are stuck with awful looking figures.


## ----plotModel3e3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8, message=FALSE, warning=FALSE----
loyn.glm2 %>% ggemmeans(~AREA|fGRAZE) %>%
    plot(add.data=TRUE, jitter=FALSE)

loyn.glm2 %>% ggemmeans(~AREA|fGRAZE) %>%
    plot(add.data=TRUE, jitter=FALSE) +
    scale_x_log10()
## unfortunately, it is not currently possible to influence the
## prediction grid so we are stuck with awful looking figures.


## ----plotModel4a1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=4, fig.height=4, message=FALSE, warning=FALSE----
loyn.glm %>% plot_model(type='est')
loyn.glm %>% plot_model(type='est', transform='exp', show.values=TRUE)


## ----plotModel4a2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=4, fig.height=4, message=FALSE, warning=FALSE----
loyn.glm1 %>% plot_model(type='est')
loyn.glm1 %>% plot_model(type='est', transform='exp', show.values=TRUE)


## ----plotModel4a3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8, message=FALSE, warning=FALSE----
loyn.glm2 %>% plot_model(type='est')
loyn.glm2 %>% plot_model(type='est', transform='exp', show.values=TRUE)


## ----summaryModel1a, results='markdown', eval=TRUE, hidden=TRUE---------------
loyn.glm %>% summary()


## ----summaryModel1b, results='markdown', eval=TRUE, hidden=TRUE---------------
loyn.glm1 %>% summary()


## ----summaryModel1c, results='markdown', eval=TRUE, hidden=TRUE---------------
loyn.glm2 %>% summary()


## ----summaryModel2a, results='markdown', eval=TRUE, hidden=TRUE---------------
loyn.glm %>% confint()
loyn.glm %>% confint() %>% exp()


## ----summaryModel2b, results='markdown', eval=TRUE, hidden=TRUE---------------
loyn.glm1 %>% confint()
loyn.glm1 %>% confint() %>% exp()


## ----summaryModel2c, results='markdown', eval=TRUE, hidden=TRUE---------------
loyn.glm2 %>% confint()
loyn.glm2 %>% confint() %>% exp()


## ----summaryModel3a, results='markdown', eval=TRUE, hidden=TRUE---------------
loyn.glm %>% tidy(conf.int=TRUE, exponentiate = TRUE)
loyn.glm %>% glance()


## ----summaryModel3b, results='markdown', eval=TRUE, hidden=TRUE---------------
loyn.glm1 %>% tidy(conf.int=TRUE, exponentiate = TRUE)
loyn.glm1 %>% glance()


## ----summaryModel3c, results='markdown', eval=TRUE, hidden=TRUE---------------
loyn.glm2 %>% tidy(conf.int=TRUE, exponentiate = TRUE)
loyn.glm2 %>% glance()


## ----summaryModel4a, results='markdown', eval=TRUE, hidden=TRUE---------------
loyn.glm %>% std.coef(partial.sd=TRUE)


## ----summaryModel4b, results='markdown', eval=TRUE, hidden=TRUE---------------
loyn.glm1 %>% std.coef(partial.sd=TRUE)


## ----summaryModel4c, results='markdown', eval=TRUE, hidden=TRUE---------------
loyn.glm2 %>% std.coef(partial.sd=TRUE)


## ----modelSelection1, results='markdown', eval=TRUE, hidden=TRUE--------------
loyn.glm1 %>% MuMIn::r.squaredLR()
loyn.glm1 %>% AIC()
loyn.glm1 %>% AICc()


## ----modelSelection2, results='markdown', eval=TRUE, hidden=TRUE--------------
# Option 1 - dredge
loyn.glm1 <- loyn.glm1 %>% update(na.action=na.fail)
#options(width=1000)
loyn.glm1 %>% dredge(rank = "AICc")


## ----modelSelection3, results='markdown', eval=TRUE, hidden=TRUE--------------
## 2. model averaging
loyn.av <- loyn.glm1 %>%
    dredge(rank = "AICc") %>%
    model.avg(subset=delta<=4)
loyn.av %>% summary()
loyn.av %>% confint()


## ----modelSelection4, results='markdown', eval=TRUE, hidden=TRUE--------------
loyn.glm1a <- loyn.glm1 %>% update(.~scale(log(DIST), scale=FALSE)*scale(log(LDIST), scale=FALSE))
loyn.glm1b <- loyn.glm1 %>% update(.~scale(log(AREA), scale=FALSE) * fGRAZE)
loyn.glm1c <- loyn.glm1 %>% update(.~scale(log(AREA), scale=FALSE) * fGRAZE * scale(YR.ISOL, scale=FALSE))
loyn.glm1d <- loyn.glm1 %>% update(.~scale(ALT, scale=FALSE))
loyn.null <- loyn.glm1 %>% update(.~1)
AICc(loyn.glm1a, loyn.glm1b, loyn.glm1c, loyn.glm1d, loyn.null)
AICc(loyn.glm1a, loyn.glm1b, loyn.glm1c, loyn.glm1d, loyn.null) %>%
    mutate(Model=row.names(.), delta=AICc-AICc[5]) %>%
    dplyr::select(Model, everything())
## Support for model 2, 3 and 4, no support for model 1


## ----modelSelection5, results='markdown', eval=TRUE, hidden=TRUE--------------
loyn.glm1b %>% summary()


## ----emmtrends, results='markdown', eval=TRUE---------------------------------
loyn.glm1b %>% emtrends(pairwise~fGRAZE, var='log(AREA)')
loyn.glm1b %>% emtrends(pairwise~fGRAZE, var='AREA')


## ----emeans, results='markdown', eval=TRUE------------------------------------
loyn.glm1b %>% emmeans(pairwise~fGRAZE)


## ----figureModel, results='markdown', eval=TRUE, hidden=TRUE------------------
## loyn_grid <- loyn.glm1b %>%
##     insight::get_data() %>%
##     modelr::data_grid(AREA=modelr::seq_range(AREA, n=10)) %>%
##     as.list()

## ref_grid(loyn.glm1b, cov.reduce=function(x) seq_range(x, n=10)) %>%
##     emmeans(~AREA|fGRAZE)



## Using emmeans
loyn.grid <- with(loyn,  list(fGRAZE = levels(fGRAZE),
                              AREA = seq(min(AREA),  max(AREA),  len=100)))
## OR
loyn.grid <- with(loyn,  list(fGRAZE = levels(fGRAZE),
                            AREA = seq_range(AREA,  n=100)))
newdata <- loyn.glm1b %>%
    emmeans(~AREA|fGRAZE,  at=loyn.grid,  type='response') %>%
  as.data.frame
head(newdata)

ggplot(newdata, aes(y=response, x=AREA, color=fGRAZE, fill=fGRAZE)) +
  geom_point(data=loyn,  aes(y=ABUND,  color=fGRAZE)) +
  geom_ribbon(aes(ymin=asymp.LCL, ymax=asymp.UCL), color=NA, alpha=0.3) +
  geom_line() +
  scale_x_log10(labels=scales::comma)+
  scale_y_log10('Abundance', breaks=as.vector(c(1,2,5,10) %o% 10^(0:2))) +
  theme_classic()



## ----figureModel3, results='markdown', eval=TRUE, hidden=TRUE-----------------
loyn.grid <- loyn %>%
    filter(fGRAZE==1) %>%
    with(list(fGRAZE='1', AREA=seq_range(AREA,  n=100)))
newdata.1 = emmeans(loyn.glm1b,  ~AREA|fGRAZE,  at=loyn.grid,  type='response') %>%
  as.data.frame

loyn.grid <- loyn %>%
    filter(fGRAZE==2) %>%
    with(list(fGRAZE='2', AREA=seq_range(AREA,  n=100)))
newdata.2 = emmeans(loyn.glm1b,  ~AREA|fGRAZE,  at=loyn.grid,  type='response') %>%
  as.data.frame

loyn.grid <- loyn %>%
    filter(fGRAZE==3) %>%
    with(list(fGRAZE='3', AREA=seq_range(AREA,  n=100)))
newdata.3 = emmeans(loyn.glm1b,  ~AREA|fGRAZE,  at=loyn.grid,  type='response') %>%
  as.data.frame

loyn.grid <- loyn %>%
    filter(fGRAZE==4) %>%
    with(list(fGRAZE='4', AREA=seq_range(AREA,  n=100)))
newdata.4 = emmeans(loyn.glm1b,  ~AREA|fGRAZE,  at=loyn.grid,  type='response') %>%
  as.data.frame

loyn.grid <- loyn %>%
    filter(fGRAZE==5) %>%
    with(list(fGRAZE='5', AREA=seq_range(AREA,  n=100)))
newdata.5 = emmeans(loyn.glm1b,  ~AREA|fGRAZE,  at=loyn.grid,  type='response') %>%
  as.data.frame

newdata.1 %>%
    bind_rows(newdata.2) %>%
    bind_rows(newdata.3) %>%
    bind_rows(newdata.4) %>%
    bind_rows(newdata.5) %>%
    ggplot(aes(y=response, x=AREA, color=fGRAZE, fill=fGRAZE)) +
    geom_ribbon(aes(ymin=asymp.LCL, ymax=asymp.UCL), color=NA, alpha=0.3) +
    geom_line() +
    scale_x_log10(labels=scales::comma)+
    scale_y_log10('Abundance') +
  theme_classic() +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  geom_point(data=loyn,  aes(y=ABUND,  color=fGRAZE))
    


## ----figureModel3a, results='markdown', eval=TRUE, hidden=TRUE----------------
loyn %>%
    group_by(fGRAZE) %>%
    nest() %>%
    mutate(Grid = map2(.x=data, .y=fGRAZE, ~list(fGRAZE=unique(.y), AREA=seq_range(.x$AREA, n=100))),
           Pred = map(.x=Grid, ~emmeans(loyn.glm1b, ~AREA|fGRAZE, at=.x, type='response') %>% as.data.frame())
           ) %>%
    ungroup() %>%
    dplyr::select(Pred) %>%
    unnest(Pred) %>%
    ggplot(aes(y=response, x=AREA, color=fGRAZE, fill=fGRAZE)) +
    geom_ribbon(aes(ymin=asymp.LCL, ymax=asymp.UCL), color=NA, alpha=0.3) +
    geom_line() +
    scale_x_log10(labels=scales::comma)+
    scale_y_log10('Abundance') +
    theme_classic() +
    scale_color_viridis_d() +
    scale_fill_viridis_d() +
    geom_point(data=loyn,  aes(y=ABUND,  color=fGRAZE))



## ----figureModel2, results='markdown', eval=TRUE, hidden=TRUE-----------------
loyn.grid = with(loyn,  list(fGRAZE=levels(fGRAZE),
                             AREA=as.vector(seq(1, 10, len=10) %o% 10^(-1:3)) %>% unique))
                             

newdata = emmeans(loyn.glm1b, ~AREA|fGRAZE,
                  at=loyn.grid,
                  type='response') %>%
    as.data.frame
head(newdata)
loyn.limits = loyn %>%
  group_by(fGRAZE) %>%
  summarise(Min=min(AREA),  Max=max(AREA))
newdata1 = newdata %>%
  full_join(loyn.limits) %>%
  filter(AREA>=Min,  AREA<=Max)
head(newdata1)
ggplot(newdata1, aes(y=response, x=AREA, color=fGRAZE, fill=fGRAZE)) +
    geom_ribbon(aes(ymin=asymp.LCL, ymax=asymp.UCL), color=NA, alpha=0.3) +
    geom_line() +
    scale_x_log10(labels=scales::comma)+
    scale_y_log10('Abundance') +
  theme_classic() +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  geom_point(data=loyn,  aes(y=ABUND,  color=fGRAZE))


