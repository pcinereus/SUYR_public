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
library(DHARMa)    #for assessing dispersion etc
library(glmmTMB)    #for glmmTMB
library(performance) #for diagnostic plots
library(see)         #for diagnostic plots


## ----readData, results='markdown', eval=TRUE----------------------------------
elston = read_csv('../data/elston.csv', trim_ws=TRUE)
glimpse(elston)


## ----EDA, results='markdown', eval=FALSE, hidden=FALSE------------------------
## ##Response = TICKS
## ## LOCATION (R)
## ## BROOD (R)
## ## ALTITUDE
## ## YEAR
## elston = elston %>%
##     mutate(fYear=factor(YEAR),
##            LOCATION=factor(LOCATION),
##            BROOD = factor(paste0(LOCATION,BROOD)))
## 
## ggplot(elston, aes(y=TICKS, x=ALTITUDE, color=fYear)) +
##   geom_smooth() +
##   geom_point() +
##   scale_y_log10()
## 
## elston.glmmTMB <- glmmTMB(TICKS ~ fYear*scale(ALTITUDE) + (1|LOCATION/BROOD),
##                           data=elston,  family=poisson(link='log'),  REML=FALSE)
## elston.glmmTMB1 <- glmmTMB(TICKS ~ fYear+scale(ALTITUDE) + (1|LOCATION/BROOD),
##                            data=elston,  family=poisson(link='log'),  REML=FALSE)
## AICc(elston.glmmTMB,  elston.glmmTMB1)
## 
## elston.glmmTMB <- update(elston.glmmTMB,  REML=TRUE)
## elston.glmmTMB1 <- update(elston.glmmTMB, .~.-(1|LOCATION/BROOD)+(fYear|LOCATION/BROOD))
## elston.glmmTMB1 <- update(elston.glmmTMB, .~.-(1|LOCATION/BROOD)+(scale(ALTITUDE)|LOCATION/BROOD))
## #elston.glmmTMB1 <- update(elston.glmmTMB, .~.-(1|LOCATION/BROOD)+(ALTITUDE|LOCATION/BROOD))
## AICc(elston.glmmTMB,  elston.glmmTMB1)
## 
## plot_model(elston.glmmTMB, type='diag') %>% plot_grid
## elston.resid <- simulateResiduals(elston.glmmTMB,  plot=TRUE)
## performance::check_model(elston.glmmTMB)
## 
## plot(allEffects(elston.glmmTMB),  multiline=TRUE,  ci.style='bands')
## ##plot_model(elston.glmmTMB, type='eff', terms=c('ALTITUDE', 'fYear'))
## 
## summary(elston.glmmTMB)
## tidy(elston.glmmTMB,  conf.int=TRUE,  exponentiate=TRUE)
## 
## emmeans(elston.glmmTMB,  pairwise~fYear|ALTITUDE,  type='response',
##         at=list(ALTITUDE= quantile(elston$ALTITUDE)))
## emmeans(elston.glmmTMB,  pairwise~fYear|ALTITUDE,  type='response',
##         at=list(ALTITUDE= quantile(elston$ALTITUDE)))$contrasts %>%
##   confint() %>%
##   as.data.frame %>%
##   ggplot(aes(y=ratio,  x=ALTITUDE,  color=contrast)) +
##   geom_hline(yintercept=1,  linetype='dashed') +
##   geom_pointrange(aes(ymin=lower.CL,  ymax=upper.CL),  position=position_dodge(width=0.2)) +
##   scale_y_log10() +
##   coord_flip()
## 
## elston.grid = with(elston,  list(ALTITUDE=modelr::seq_range(ALTITUDE,  n=100)))
## newdata = emmeans(elston.glmmTMB,  ~ALTITUDE|fYear, type='response',
##                   at = elston.grid) %>%
##   as.data.frame
## head(newdata)
## ggplot(newdata) +
##   geom_ribbon(aes(x=ALTITUDE, fill=fYear, ymin=lower.CL, ymax=upper.CL),  alpha=0.3) +
##   geom_line(aes(y=rate, x=ALTITUDE, color=fYear)) +
##   scale_y_log10()

