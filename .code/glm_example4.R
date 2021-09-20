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
     


## ----fitModel, results='markdown', eval=TRUE, hidden=TRUE---------------------
loyn.glm<-glm(ABUND~scale(log(DIST), scale=FALSE) + scale(log(LDIST), scale=FALSE) +
                scale(log(AREA), scale=FALSE)+
                fGRAZE + scale(ALT, scale=FALSE) + scale(YR.ISOL, scale=FALSE),
              data=loyn, family=gaussian())

loyn.glm<-glm(ABUND~scale(log(DIST), scale=FALSE) + scale(log(LDIST), scale=FALSE) +
                scale(log(AREA), scale=FALSE)+
                fGRAZE + scale(ALT, scale=FALSE) + scale(YR.ISOL, scale=FALSE),
              data=loyn, family=gaussian(link='log'))
loyn.glm1<-glm(ABUND~scale(log(DIST), scale=FALSE) + scale(log(LDIST), scale=FALSE) +
                 scale(log(AREA), scale=FALSE)+
                 fGRAZE + scale(ALT, scale=FALSE) + scale(YR.ISOL, scale=FALSE),
               data=loyn, family=Gamma(link='log'))


## ----validateModel1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
vif(loyn.glm)
## If any terms have df>1, then Generalized VIF calculated
## This is the inflation in size compared to what would be expected if orthogonal
vif(loyn.glm1)


## ----validateModel2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6, warning=FALSE, message=FALSE----
autoplot(loyn.glm, which=1:6)
autoplot(loyn.glm1, which=1:6)


## ----validateModel3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6, warning=FALSE, message=FALSE----
performance::check_model(loyn.glm)
performance::check_model(loyn.glm1)


## ----validateModel4a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=6, warning=FALSE, message=FALSE----
loyn.resid <- simulateResiduals(loyn.glm,  plot=TRUE)
loyn.resid1 <- simulateResiduals(loyn.glm1,  plot=TRUE)


## ----validateModel2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=9, fig.height=6, warning=FALSE, message=FALSE----
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



## ----plotModel3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8, message=FALSE, warning=FALSE----
## Effects
plot_model(loyn.glm,  type='eff', show.data=TRUE, dot.size=0.5) %>% plot_grid
## Predictions
plot_model(loyn.glm,  type='pred', show.data=TRUE, dot.size=0.5) %>% plot_grid


## ----plotModel3e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8, message=FALSE, warning=FALSE----
## We can sort of do part of the backtransforms - but only partly..
plot_grid(list(
  plot_model(loyn.glm,  type='eff',  terms='DIST [exp]'),
  plot_model(loyn.glm,  type='eff',  terms='LDIST [exp]'),
  plot_model(loyn.glm,  type='eff',  terms='AREA [exp]'),
  plot_model(loyn.glm,  type='eff',  terms='fGRAZE'),
  plot_model(loyn.glm,  type='eff',  terms='ALT'),
  plot_model(loyn.glm,  type='eff',  terms='YR.ISOL')  
))


## ----plotModel3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8, message=FALSE, warning=FALSE----
plot(allEffects(loyn.glm, residuals=TRUE), type='response')


## ----plotModel3c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8, message=FALSE, warning=FALSE----
ggpredict(loyn.glm) %>%
  plot(add.data=TRUE, facet=TRUE)


## ----plotModel3d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8, message=FALSE, warning=FALSE----
ggemmeans(loyn.glm,  ~AREA|fGRAZE) %>%
  plot(add.data=TRUE)


## ----plotModel4a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8, message=FALSE, warning=FALSE----
plot_model(loyn.glm,  type='est')
plot_model(loyn.glm,  type='est', transform='exp', show.values=TRUE)


## ----summaryModel1, results='markdown', eval=TRUE, hidden=TRUE----------------
summary(loyn.glm)


## ----summaryModel2, results='markdown', eval=TRUE, hidden=TRUE----------------
confint(loyn.glm)
exp(confint(loyn.glm))


## ----summaryModel3, results='markdown', eval=TRUE, hidden=TRUE----------------
tidy(loyn.glm, conf.int=TRUE, exponentiate = TRUE)
glance(loyn.glm)


## ----summaryModel4, results='markdown', eval=TRUE, hidden=TRUE----------------
std.coef(loyn.glm,  partial.sd=TRUE)


## ----modelSelection1, results='markdown', eval=TRUE, hidden=TRUE--------------
MuMIn::r.squaredLR(loyn.glm)
AIC(loyn.glm)
AICc(loyn.glm)


## ----modelSelection2, results='markdown', eval=TRUE, hidden=TRUE--------------
# Option 1 - dredge
loyn.glm = update(loyn.glm, na.action=na.fail)
#options(width=1000)
dredge(loyn.glm, rank = "AICc")


## ----modelSelection3, results='markdown', eval=TRUE, hidden=TRUE--------------
## 2. model averaging
loyn.av<-model.avg(dredge(loyn.glm, rank = "AICc"),
                     subset=delta<=4)
summary(loyn.av)
confint(loyn.av)


## ----modelSelection4, results='markdown', eval=TRUE, hidden=TRUE--------------
loyn.glm1<-update(loyn.glm, .~scale(log(DIST), scale=FALSE)*scale(log(LDIST), scale=FALSE))
loyn.glm2<-update(loyn.glm, .~scale(log(AREA), scale=FALSE) * fGRAZE)
loyn.glm3<-update(loyn.glm, .~scale(log(AREA), scale=FALSE) * fGRAZE * scale(YR.ISOL, scale=FALSE))
loyn.glm4<-update(loyn.glm, .~scale(ALT, scale=FALSE))
loyn.null<-update(loyn.glm, .~1)
AICc(loyn.glm1, loyn.glm2, loyn.glm3, loyn.glm4, loyn.null)
AICc(loyn.glm1, loyn.glm2, loyn.glm3, loyn.glm4, loyn.null) %>%
    mutate(Model=row.names(.), delta=AICc-AICc[5]) %>%
    dplyr::select(Model, everything())
## Support for model 2, 3 and 4, no support for model 1


## ----modelSelection5, results='markdown', eval=TRUE, hidden=TRUE--------------
summary(loyn.glm2)


## ----emmtrends, results='markdown', eval=TRUE---------------------------------
emtrends(loyn.glm2,  pairwise~fGRAZE, var='log(AREA)')
emtrends(loyn.glm2,  pairwise~fGRAZE, var='AREA')


## ----emeans, results='markdown', eval=TRUE------------------------------------
emmeans(loyn.glm2, pairwise~fGRAZE)


## ----figureModel, results='markdown', eval=TRUE, hidden=TRUE------------------
## Using emmeans
loyn.grid <- with(loyn,  list(fGRAZE=levels(fGRAZE),
                              AREA = seq(min(AREA),  max(AREA),  len=100)))
## OR
loyn.grid = with(loyn,  list(fGRAZE=levels(fGRAZE),
                            AREA=seq_range(AREA,  n=100)))
newdata = emmeans(loyn.glm2,  ~AREA|fGRAZE,  at=loyn.grid,  type='response') %>%
  as.data.frame
## OR
#newdata = emmeans(loyn.lm3, ~AREA|fGRAZE,
#        at=list(fGRAZE=levels(loyn$fGRAZE),
#                AREA=seq(min(loyn$AREA), max(loyn$AREA), len=100))) %>%
#    as.data.frame
head(newdata)

ggplot(newdata, aes(y=emmean, x=AREA, color=fGRAZE, fill=fGRAZE)) +
  geom_point(data=loyn,  aes(y=ABUND,  color=fGRAZE)) +
  geom_ribbon(aes(ymin=asymp.LCL, ymax=asymp.UCL), color=NA, alpha=0.3) +
  geom_line() +
  scale_x_log10(labels=scales::comma)+
  scale_y_continuous('Abundance') +
  theme_classic()

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
newdata.1 = emmeans(loyn.glm2,  ~AREA|fGRAZE,  at=loyn.grid,  type='response') %>%
  as.data.frame

loyn.grid <- loyn %>%
    filter(fGRAZE==2) %>%
    with(list(fGRAZE='2', AREA=seq_range(AREA,  n=100)))
newdata.2 = emmeans(loyn.glm2,  ~AREA|fGRAZE,  at=loyn.grid,  type='response') %>%
  as.data.frame

loyn.grid <- loyn %>%
    filter(fGRAZE==3) %>%
    with(list(fGRAZE='3', AREA=seq_range(AREA,  n=100)))
newdata.3 = emmeans(loyn.glm2,  ~AREA|fGRAZE,  at=loyn.grid,  type='response') %>%
  as.data.frame

loyn.grid <- loyn %>%
    filter(fGRAZE==4) %>%
    with(list(fGRAZE='4', AREA=seq_range(AREA,  n=100)))
newdata.4 = emmeans(loyn.glm2,  ~AREA|fGRAZE,  at=loyn.grid,  type='response') %>%
  as.data.frame

loyn.grid <- loyn %>%
    filter(fGRAZE==5) %>%
    with(list(fGRAZE='5', AREA=seq_range(AREA,  n=100)))
newdata.5 = emmeans(loyn.glm2,  ~AREA|fGRAZE,  at=loyn.grid,  type='response') %>%
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
    


## ----figureModel2, results='markdown', eval=TRUE, hidden=TRUE-----------------
loyn.grid = with(loyn,  list(fGRAZE=levels(fGRAZE),
                             AREA=as.vector(seq(1, 10, len=10) %o% 10^(-1:3)) %>% unique))
                             

newdata = emmeans(loyn.glm2, ~AREA|fGRAZE,
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


