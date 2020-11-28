## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning=FALSE, message=FALSE)


## ----libraries, results='markdown', eval=TRUE, warning=TRUE, message=FALSE----
library(mgcv)      #for GAMs
library(gratia)    #for GAM plots
library(emmeans)   #for marginal means etc
library(broom)     #for tidy output
library(MuMIn)     #for model selection and AICc
library(lubridate) #for processing dates
library(mapdata)
library(maps)
library(tidyverse) #for data wrangling
library(DHARMa)    #for residual diagnostics
library(performance)
library(see)
library(sf)
library(stars)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)
library(ggspatial)
library(patchwork)


## ----readData, results='markdown', eval=TRUE----------------------------------
paruelo = read_csv('../data/paruelo.csv', trim_ws=TRUE)
glimpse(paruelo)


## ----spatial1, results='markdown', eval=TRUE, fig.width=8, fig.height=3-------
usa <- ne_countries(country=c('united states of america','canada'), 
                    scale = "medium", returnclass = "sf")
ggplot() +
  geom_sf(data=usa)


## ----spatial2, results='markdown', eval=TRUE, fig.width=8, fig.height=3-------
usa = usa %>% 
  st_crop(xmin=-130, xmax=-60,
          ymin=0, ymax=60)


## ----preparation, results='markdown', eval=TRUE, hidden=TRUE------------------
paruelo = paruelo %>%
  mutate(mC3=ifelse(C3==0, 0.01,C3),
         LONG=-1*LONG)
paruelo.sf = paruelo %>%
  st_as_sf(coords=c('LONG', 'LAT'),  crs=st_crs(usa)) 


## ----EDA1, results='markdown', eval=TRUE, fig.width=8, fig.height=6-----------
ggplot() +
  geom_sf(data=usa) +
  geom_sf(data=paruelo.sf, aes(color=C3, size=C3)) +
  scale_color_gradientn(colors=heat.colors(n=10)) +
  coord_sf(expand=FALSE)


## ----EDA2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
ggplot(paruelo) + geom_histogram(aes(x=C3)) +
ggplot(paruelo) + geom_histogram(aes(x=C3)) + scale_x_continuous(trans=scales::logit_trans())


## ----fitModel1a, results='markdown', eval=TRUE, hidden=TRUE-------------------
paruelo.gam1 = gam(mC3 ~ s(LONG,LAT), data=paruelo,
                   family=betar, method='REML')


## ----fitModel1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
k.check(paruelo.gam1)
appraise(paruelo.gam1)


## ----fitModel1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
paruelo.resids <- createDHARMa(simulatedResponse = simulate(paruelo.gam1,  nsim=250),
             observedResponse = paruelo$mC3,
             fittedPredictedResponse = predict(paruelo.gam1))
plot(paruelo.resids)


## ----fitMode2a, results='markdown', eval=TRUE, hidden=TRUE--------------------
paruelo.gam2 = gam(mC3 ~ te(LONG,LAT), data=paruelo, family=betar, method='REML')


## ----fitModel2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
k.check(paruelo.gam2)
appraise(paruelo.gam2)


## ----fitModel2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
paruelo.resids <- createDHARMa(simulatedResponse = simulate(paruelo.gam2,  nsim=250),
             observedResponse = paruelo$mC3,
             fittedPredictedResponse = predict(paruelo.gam1))
plot(paruelo.resids)


## ----fitMode3a, results='markdown', eval=TRUE, hidden=TRUE--------------------
paruelo.gam3 = gam(mC3 ~ ti(LONG) + ti(LAT) + ti(LONG,LAT),
                   data=paruelo, family=betar, method='REML')


## ----fitModel3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
k.check(paruelo.gam3)
appraise(paruelo.gam3)


## ----fitModel3c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
paruelo.resids <- createDHARMa(simulatedResponse = simulate(paruelo.gam3,  nsim=250),
             observedResponse = paruelo$mC3,
             fittedPredictedResponse = predict(paruelo.gam1))
plot(paruelo.resids)


## ----partialPlots1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
plot(paruelo.gam1,scheme = 2)


## ----partialPlots1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
vis.gam(paruelo.gam1, theta=30)


## ----partialPlots1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
draw(paruelo.gam1)


## ----partialPlots2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
plot(paruelo.gam2,scheme = 2)


## ----partialPlots2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
vis.gam(paruelo.gam2, theta=30)


## ----partialPlots2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
draw(paruelo.gam2)


## ----partialPlots3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=4, fig.height=4----
plot(paruelo.gam3,scheme = 2)


## ----partialPlots3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
vis.gam(paruelo.gam3, theta=30)


## ----partialPlots3c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
draw(paruelo.gam3)


## ----summary1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
summary(paruelo.gam1)


## ----summary1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
tidy(paruelo.gam1)


## ----summary2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
summary(paruelo.gam2)


## ----summary2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
tidy(paruelo.gam2)


## ----summary3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
summary(paruelo.gam3)


## ----summary3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
tidy(paruelo.gam3)


## ----summaryFig1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
paruelo.list = with(paruelo,
                    list(LAT=seq(min(LAT), max(LAT), len=100),
                         LONG=seq(min(LONG), max(LONG), len=100)))
newdata = emmeans(paruelo.gam1, ~LONG+LAT, at=paruelo.list, type='response') %>%
  as.data.frame
newdata %>% head
ggplot(newdata, aes(y=LAT, x=LONG)) +
  geom_tile(aes(fill=response)) +
  geom_contour(aes(z=response)) +
  scale_fill_gradientn(colors=heat.colors(10)) +
  geom_point(data=paruelo,aes(fill=C3), shape=21, size=5) +
  coord_equal()


## ----summaryFig1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
newdata.sf <- rasterFromXYZ(newdata[, c("LONG", "LAT", "response")]) %>% 
  st_as_stars() %>%
  st_set_crs(st_crs(usa))
## OR
#newdata.sf <- newdata %>% 
#  st_as_sf(coords=c("LONG", "LAT"),  crs=st_crs(usa)) %>%
#  st_rasterize()
ggplot() +
  geom_sf(data=usa) +
  geom_stars(data=newdata.sf)


## ----summaryFig1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
newdata.sf <- rasterFromXYZ(newdata[, c("LONG", "LAT", "response")]) %>%
  mask(usa) %>%
  st_as_stars() %>% 
  st_set_crs(st_crs(usa))

ggplot() +
  geom_sf(data=usa) +
  geom_stars(data=newdata.sf) +
  scale_fill_gradientn(colours=heat.colors(10), na.value=NA) + 
  geom_sf(data=paruelo.sf, aes(fill=C3), shape=21,  size=4) +
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "bl", which_north = "true", 
        pad_x = unit(0.25, "in"), pad_y = unit(0.2, "in"),
        style = north_arrow_fancy_orienteering) +
  coord_sf(expand=FALSE) +
  theme_bw()



## ----summaryFig2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
paruelo.list = with(paruelo,
                    list(LAT=seq(min(LAT), max(LAT), len=100),
                         LONG=seq(min(LONG), max(LONG), len=100)))
newdata = emmeans(paruelo.gam2, ~LONG+LAT, at=paruelo.list, type='response') %>%
  as.data.frame
newdata %>% head
ggplot(newdata, aes(y=LAT, x=LONG)) +
  geom_tile(aes(fill=response)) +
  geom_contour(aes(z=response)) +
  scale_fill_gradientn(colors=heat.colors(10)) +
  geom_point(data=paruelo,aes(fill=C3), shape=21, size=5) +
  coord_equal()


## ----summaryFig2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
newdata.sf <- rasterFromXYZ(newdata[, c("LONG", "LAT", "response")]) %>% 
  st_as_stars() %>%
  st_set_crs(st_crs(usa))
## OR
#newdata.sf <- newdata %>% 
#  st_as_sf(coords=c("LONG", "LAT"),  crs=st_crs(usa)) %>%
#  st_rasterize()
ggplot() +
  geom_sf(data=usa) +
  geom_stars(data=newdata.sf)


## ----summaryFig2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
newdata.sf <- rasterFromXYZ(newdata[, c("LONG", "LAT", "response")]) %>%
  mask(usa) %>%
  st_as_stars() %>% 
  st_set_crs(st_crs(usa))

ggplot() +
  geom_sf(data=usa) +
  geom_stars(data=newdata.sf) +
  scale_fill_gradientn(colours=heat.colors(10), na.value=NA) + 
  geom_sf(data=paruelo.sf, aes(fill=C3), shape=21,  size=4) +
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "bl", which_north = "true", 
        pad_x = unit(0.25, "in"), pad_y = unit(0.2, "in"),
        style = north_arrow_fancy_orienteering) +
  coord_sf(expand=FALSE) +
  theme_bw()



## ----summaryFig3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
paruelo.list = with(paruelo,
                    list(LAT=seq(min(LAT), max(LAT), len=100),
                         LONG=seq(min(LONG), max(LONG), len=100)))
newdata = emmeans(paruelo.gam3, ~LONG+LAT, at=paruelo.list, type='response') %>%
  as.data.frame
newdata %>% head
ggplot(newdata, aes(y=LAT, x=LONG)) +
  geom_tile(aes(fill=response)) +
  geom_contour(aes(z=response)) +
  scale_fill_gradientn(colors=heat.colors(10)) +
  geom_point(data=paruelo,aes(fill=C3), shape=21, size=5) +
  coord_equal()


## ----summaryFig3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
newdata.sf <- rasterFromXYZ(newdata[, c("LONG", "LAT", "response")]) %>% 
  st_as_stars() %>%
  st_set_crs(st_crs(usa))
## OR
#newdata.sf <- newdata %>% 
#  st_as_sf(coords=c("LONG", "LAT"),  crs=st_crs(usa)) %>%
#  st_rasterize()
ggplot() +
  geom_sf(data=usa) +
  geom_stars(data=newdata.sf)


## ----summaryFig3c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
newdata.sf <- rasterFromXYZ(newdata[, c("LONG", "LAT", "response")]) %>%
  mask(usa) %>%
  st_as_stars() %>% 
  st_set_crs(st_crs(usa))

ggplot() +
  geom_sf(data=usa) +
  geom_stars(data=newdata.sf) +
  scale_fill_gradientn(colours=heat.colors(10), na.value=NA) + 
  geom_sf(data=paruelo.sf, aes(fill=C3), shape=21,  size=4) +
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "bl", which_north = "true", 
        pad_x = unit(0.25, "in"), pad_y = unit(0.2, "in"),
        style = north_arrow_fancy_orienteering) +
  coord_sf(expand=FALSE) +
  theme_bw()


