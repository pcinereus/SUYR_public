## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


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


## ----readData, results='markdown', eval=TRUE----------------------------------
paruelo = read_csv('../data/paruelo.csv', trim_ws=TRUE)
glimpse(paruelo)


## ----preparation, results='markdown', eval=TRUE, hidden=TRUE------------------
summary(paruelo$C3)
paruelo = paruelo %>%
  mutate(mC3=ifelse(C3==0, 0.001,C3),
         LONG=-1*LONG)


## ----spatial, results='markdown', eval=TRUE-----------------------------------
usa <- ne_countries(country='united states of america',
                    scale = "medium", returnclass = "sf")
ggplot() +
  geom_sf(data=usa)

st_crs(usa)
usa = usa %>% 
  st_crop(xmin=-130, xmax=-60,
          ymin=0, ymax=50)
ggplot() +
  geom_sf(data=usa)

paruelo.sf <- paruelo %>%
  st_as_sf(coords=c("LONG", "LAT"),  crs=st_crs(usa))
ggplot() +
  geom_sf(data=usa) +
  geom_sf(data=paruelo.sf, aes(color=C3, size=C3)) +
  scale_color_gradientn(colors=heat.colors(n=10))



## ----EDA, results='markdown', eval=TRUE, hidden=TRUE--------------------------
#ggplot(paruelo, aes(y=LAT, x=LONG)) +
#  geom_point(aes(color=C3, size=C3))+
#  scale_color_gradientn(colors=heat.colors(10))
ggplot(paruelo) + geom_histogram(aes(x=C3))
ggplot(paruelo) + geom_histogram(aes(x=C3)) + scale_x_log10()



## ----fitModel, results='markdown', eval=TRUE, hidden=TRUE---------------------
paruelo.gam1 = gam(mC3 ~ s(LONG,LAT), data=paruelo,
                   family=betar, method='REML')
k.check(paruelo.gam1)
appraise(paruelo.gam1)
## paruelo.resid <- simulateResiduals(paruelo.gam1,  plot=TRUE)

paruelo.resids <- createDHARMa(simulatedResponse = simulate(paruelo.gam1,  nsim=250),
             observedResponse = paruelo$C3,
             fittedPredictedResponse = predict(paruelo.gam1))
plot(paruelo.resids)
testZeroInflation(paruelo.resids)

plot(paruelo.gam1,scheme = 2)
vis.gam(paruelo.gam1, theta=30)
draw(paruelo.gam1)
summary(paruelo.gam1)
                                        #ggemmeans(paruelo.gam1, ~LONG|LAT) %>% plot
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
  geom_point(data=paruelo,aes(fill=C3), shape=21, size=5)


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
        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
        style = north_arrow_fancy_orienteering)



## ----fitMode2, results='markdown', eval=TRUE, hidden=TRUE---------------------
paruelo.gam2 = gam(mC3 ~ te(LONG,LAT), data=paruelo, family=betar, method='REML')
k.check(paruelo.gam2)
appraise(paruelo.gam2)
plot(paruelo.gam2,scheme = 2)
vis.gam(paruelo.gam2, theta=30)
draw(paruelo.gam2)
summary(paruelo.gam2)

paruelo.list = with(paruelo, list(LAT=seq(min(LAT), max(LAT), len=100),
                                  LONG=seq(min(LONG), max(LONG), len=100)))
newdata = emmeans(paruelo.gam2, ~LONG+LAT, at=paruelo.list, type='response') %>%
    as.data.frame
ggplot(newdata, aes(y=LAT, x=LONG)) + geom_tile(aes(fill=response)) +
    geom_contour(aes(z=response)) +
    scale_fill_gradientn(colors=heat.colors(10)) +
    geom_point(data=paruelo,aes(fill=C3), shape=21, size=5)


## ----fitMode3, results='markdown', eval=TRUE, hidden=TRUE---------------------
paruelo.gam3 = gam(mC3 ~ ti(LONG) + ti(LAT) + ti(LONG,LAT),
                   data=paruelo, family=betar, method='REML')
k.check(paruelo.gam3)
appraise(paruelo.gam3)
plot(paruelo.gam3,scheme = 2)
vis.gam(paruelo.gam3, theta=30)
draw(paruelo.gam3)

paruelo.list = with(paruelo, list(LAT=seq(min(LAT), max(LAT), len=100),
                                  LONG=seq(min(LONG), max(LONG), len=100)))
newdata = emmeans(paruelo.gam3, ~LONG+LAT, at=paruelo.list, type='response') %>%
    as.data.frame
ggplot(newdata, aes(y=LAT, x=LONG)) + geom_tile(aes(fill=response)) +
    geom_contour(aes(z=response)) +
    scale_fill_gradientn(colors=heat.colors(10)) +
    geom_point(data=paruelo,aes(fill=C3), shape=21, size=5)


## ----map, results='markdown', eval=TRUE, hidden=TRUE,eval=FALSE---------------
## library(sf)
## library(rnaturalearth)
## #world <- ne_countries(continent='north america', scale = "medium", returnclass = "sf")
## usa <- ne_countries(country='united states of america',
##                     scale = "medium", returnclass = "sf")
## ggplot() +
##   geom_sf(data=usa)
## 
## st_crs(usa)
## usa = usa %>%
##   st_crop(xmin=-130, xmax=-60,
##           ymin=0, ymax=50)
## ggplot() +
##   geom_sf(data=usa)
## 
## paruelo.sf <- paruelo %>%
##   st_as_sf(coords=c("LONG", "LAT"),  crs=st_crs(usa))
## ggplot() +
##   geom_sf(data=usa) +
##   geom_sf(data=paruelo.sf, aes(color=C3)) +
##   scale_color_gradientn(colors=heat.colors(n=10))
## 
## library(raster)
## library(stars)
## library(ggspatial)
## 
## newdata.sf <- rasterFromXYZ(newdata[, c("LONG", "LAT", "response")]) %>%
##   st_as_stars() %>%
##   st_set_crs(st_crs(usa))
## ## OR
## #newdata.sf <- newdata %>%
## #  st_as_sf(coords=c("LONG", "LAT"),  crs=st_crs(usa)) %>%
## #  st_rasterize()
## ggplot() +
##   geom_sf(data=usa) +
##   geom_stars(data=newdata.sf)
## 
## newdata.sf <- rasterFromXYZ(newdata[, c("LONG", "LAT", "response")]) %>%
##   mask(usa) %>%
##   st_as_stars() %>%
##   st_set_crs(st_crs(usa))
## 
## ggplot() +
##   geom_sf(data=usa) +
##   geom_stars(data=newdata.sf) +
##   scale_fill_gradientn(colours=heat.colors(10), na.value=NA) +
##   geom_sf(data=paruelo.sf, aes(fill=C3), shape=21,  size=4) +
##   annotation_scale(location = "bl", width_hint = 0.25) +
##   annotation_north_arrow(location = "bl", which_north = "true",
##         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
##         style = north_arrow_fancy_orienteering)
## 
## ## Alternatively
## library(mapdata)
## library(maps)
## #US <- maps::map("worldHires", "USA",ylim=c(30,55),xlim=c(-120,-90))
## usa = maps:::map('usa') %>% fortify
## ggplot(usa, aes(y=lat, x=long)) +
##     geom_polygon(aes(group=group), fill='white', color='black') +
##     geom_point(data=paruelo, aes(y=LAT, x=LONG))
## 
## 
## ggplot(newdata, aes(y=LAT, x=LONG)) +
##     geom_polygon(data=usa, aes(y=lat, x=long, group=group), fill='white', color='black') +
##     geom_tile(aes(fill=response)) +
##     geom_contour(aes(z=response)) +
##     scale_fill_gradientn(colors=heat.colors(10)) +
##     geom_point(data=paruelo,aes(fill=C3), shape=21, size=5)
## 
## usa.poly = usa %>% filter(group==1)
## pts = sp::point.in.polygon(newdata$LONG, newdata$LAT, usa.poly$long, usa.poly$lat)
## newdata.1 = newdata[pts==1,]
## ggplot(newdata.1, aes(y=LAT, x=LONG)) +
##     geom_polygon(data=usa, aes(y=lat, x=long, group=group), fill='white', color='black') +
##     geom_tile(aes(fill=response)) +
##     geom_contour(aes(z=response)) +
##     scale_fill_gradientn(colors=heat.colors(10)) +
##     geom_point(data=paruelo,aes(fill=C3), shape=21, size=5) +
##     coord_map()
## 
## 
## ggplot(usa, aes(y=lat, x=long)) +
##     geom_polygon(aes(group=group), fill='white', color='black') +
##     geom_tile(data=newdata.1, aes(y=LAT, x=LONG, fill=response)) +
##     geom_point(data=paruelo, aes(y=LAT, x=LONG, size=C3))
## 
## paruelo.list = with(paruelo, list(LAT=seq(min(LAT), max(LAT), len=100),
##                                   LONG=c(min(LONG), mean(LONG), max(LONG))))
## newdata = emmeans(paruelo.gam3, ~LAT|LONG, at=paruelo.list, type='response') %>% as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=response, x=LAT)) +
##     geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL, fill=factor(LONG)), alpha=0.3) +
##     geom_line(aes(color=factor(LONG)))
## 
## 
## paruelo.list = with(paruelo, list(LONG=seq(min(LONG), max(LONG), len=100),
##                                   LAT=c(min(LAT), mean(LAT), max(LAT))))
## newdata = emmeans(paruelo.gam3, ~LONG|LAT, at=paruelo.list, type='response') %>% as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=response, x=LONG)) +
##     geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL, fill=factor(LAT)), alpha=0.3) +
##     geom_line(aes(color=factor(LAT)))
## 
## #ggemmeans(paruelo.gam3, ~LONG|LAT) %>% plot
## #ggemmeans(paruelo.gam3, ~LAT|LONG) %>% plot

