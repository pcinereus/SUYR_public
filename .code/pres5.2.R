## ----setup, include=FALSE,warning=FALSE, message=FALSE------------------------
knitr::opts_chunk$set(echo = TRUE, warning=FALSE, message=FALSE,
                      fig.path='resources/pres5.2-', dev='png', fig.ext='png')
options(tinytex.engine = 'xelatex')


## ----packages-----------------------------------------------------------------
library(ggplot2) #OR better still library(tidyverse)
library(grid)
library(gridExtra)
library(patchwork)
library(scales)


## ---- results='markup'--------------------------------------------------------
head(BOD)
summary(BOD)


## ----first, eval=FALSE--------------------------------------------------------
##  p <- ggplot() + #single layer - points
##   layer(data = BOD, #data.frame
##     mapping = aes(y = demand, x = Time),
##     stat = "identity", #use original data
##     geom = "point", #plot data as points
##     position = "identity",
##     params = list(na.rm = TRUE),
##     show.legend = FALSE
##   )+ #layer of lines
##   layer(data = BOD, #data.frame
##     mapping = aes(y = demand, x = Time),
##     stat = "identity", #use original data
##     geom = "line", #plot data as a line
##     position = "identity",
##     params = list(na.rm = TRUE),
##     show.legend = FALSE
##   ) +
##   coord_cartesian() + #cartesian coordinates
##   scale_x_continuous() + #continuous x axis
##   scale_y_continuous()  #continuous y axis
## p #print the plot


## ----first, echo=FALSE, fig.cap="", fig.width=5, fig.height=5,out.width='450px'----
 p <- ggplot() + #single layer - points
  layer(data = BOD, #data.frame
    mapping = aes(y = demand, x = Time),
    stat = "identity", #use original data
    geom = "point", #plot data as points
    position = "identity",
    params = list(na.rm = TRUE),
    show.legend = FALSE
  )+ #layer of lines
  layer(data = BOD, #data.frame
    mapping = aes(y = demand, x = Time),
    stat = "identity", #use original data
    geom = "line", #plot data as a line
    position = "identity",
    params = list(na.rm = TRUE),
    show.legend = FALSE
  ) +
  coord_cartesian() + #cartesian coordinates
  scale_x_continuous() + #continuous x axis
  scale_y_continuous()  #continuous y axis
p #print the plot


## ----next, fig.cap="", fig.width=6, fig.height=6,out.width='450px'------------
ggplot(data = BOD, map = aes(y = demand, x = Time)) +
    geom_point() +
    geom_line()


## ----decom1, fig.width=6, fig.height=6,out.width='450px'----------------------
p<-ggplot(data = BOD)


## ----decom2, fig.width=4, fig.height=4, fig.cap="",out.width='300px'----------
p<-p + geom_point(aes(y = demand, x = Time))
p


## ----decom1, fig.width=6, fig.height=6----------------------------------------
p<-ggplot(data = BOD)


## ----decom2a, fig.width=4, fig.height=4, fig.cap="", results="hide"-----------
p<-p + geom_point(aes(y = demand, x = Time))


## ----decom3, fig.width=3.5, fig.height=3.5, fig.cap="",out.width='200px'------
p <- p + scale_x_sqrt(name = "Time (days)")
p


## ----geom, fig.width=5, fig.height=5, fig.cap="",out.width='400px', fig.keep='last'----
ggplot(data = BOD, aes(y = demand, x = Time)) + geom_point()
#OR
ggplot(data = BOD) + geom_point(aes(y = demand, x = Time))
                                        #OR
ggplot() + geom_point(data = BOD, aes(y = demand, x = Time))


## ---- results='markup'--------------------------------------------------------
head(CO2)
summary(CO2)


## ----geompoint1, fig.cap="", fig.width=5, fig.height=5,out.width='400px'------
ggplot(CO2) + geom_point(aes(x = conc, y = uptake), colour = "red")


## ----geompoint2, fig.cap="", fig.width=5, fig.height=5,out.width='400px'------
ggplot(CO2) + geom_point(aes(x = conc, y = uptake, colour = Type))


## ----geompoint3, fig.cap="", fig.width=5, fig.height=5,out.width='400px', tidy=FALSE----
ggplot(CO2) + geom_point(aes(x = conc, y = uptake),
 stat = "summary", fun.y = mean)


## ----geomData, results='markup'-----------------------------------------------
head(diamonds)
summary(diamonds)


## ----geomBar1, results='markup',fig.width=4, fig.height=4, out.width='250px'----
ggplot(diamonds) + geom_bar(aes(x = carat))


## ----geomBar2, results='markup',fig.width=4, fig.height=4, out.width='250px'----
ggplot(diamonds) + geom_bar(aes(x = cut))


## ----geomBar3, results='markup',fig.width=6, fig.height=4, out.width='300px'----
ggplot(diamonds) + geom_bar(aes(x = cut, fill = clarity))


## ----geomBar5, results='markup',fig.width=6, fig.height=4, out.width='300px', tidy=FALSE----
ggplot(diamonds) + geom_bar(aes(x = cut, fill = clarity),
  position='dodge')


## ----geomBox1, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
ggplot(diamonds) + geom_boxplot(aes(y = carat))
ggplot(diamonds) + geom_boxplot(aes(x=carat))


## ----geomBox2, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
ggplot(diamonds) + geom_boxplot(aes(x = cut, y = carat))


## ----geomLine1, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
head(CO2, 3)
ggplot(CO2) + geom_line(aes(x = conc, y = uptake))


## ----geomLine2, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
head(CO2, 3)
ggplot(CO2) + geom_line(aes(x = conc, y = uptake, group = Plant))


## ----geomLine3, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
head(CO2, 3)
ggplot(CO2) + geom_line(aes(x = conc, y = uptake, color=Plant))


## ----geomLine4, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
head(CO2, 3)
ggplot(CO2) + geom_line(aes(x = conc, y = uptake),
    stat = "summary", fun.y = mean, color='blue')


## ----geomPoint1, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
head(CO2, 3)
ggplot(CO2) + geom_point(aes(x = conc, y = uptake))


## ----geomPoint2, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
head(CO2, 3)
ggplot(CO2) + geom_point(aes(x = conc, y = uptake, fill = Treatment),
  shape=21)


## ----geomSmooth1, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
head(CO2, 3)
ggplot(CO2) + geom_smooth(aes(x = conc, y = uptake), method = "lm")


## ----geomSmooth2, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
head(CO2, 3)
ggplot(CO2) + geom_smooth(aes(x = conc, y = uptake, fill = Treatment))


## ----geomPolygon, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
library(maps)
library(mapdata)
aus <- map_data("worldHires", region="Australia")
head(aus,3)
ggplot(aus, aes(x=long, y=lat, group=group)) +
    geom_polygon()


## ----geomTile, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
head(faithfuld,3) 
ggplot(faithfuld, aes(waiting, eruptions)) +
      geom_tile(aes(fill = density))


## ----geomRaster, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
head(faithfuld,3)
ggplot(faithfuld, aes(waiting, eruptions)) +
      geom_raster(aes(fill = density))


## ---- results='markup'--------------------------------------------------------
head(warpbreaks)
summary(warpbreaks)


## ----geomErrorbar1, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
library(dplyr)
library(gmodels)
warpbreaks.sum <- warpbreaks %>%
    group_by(wool) %>%
    summarise(Mean = mean(breaks),
              Lower=ci(breaks)[2],
              Upper=ci(breaks)[3])
warpbreaks.sum


## ---- eval=FALSE, echo=FALSE--------------------------------------------------
## warpbreaks.df <- plyr:::ddply(warpbreaks, ~wool, function(x) {
##     Hmisc:::smean.cl.boot(x$breaks)
## })


## ----geomErrorbar1a, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
ggplot(warpbreaks.sum) +
    geom_errorbar(aes(x = wool, ymin = Lower, ymax = Upper), width=0.2)


## ----geomErrorbar2, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
head(warpbreaks,3)
ggplot(warpbreaks) + geom_errorbar(aes(x = wool, y = breaks),
    stat = "summary", fun.data = "mean_cl_boot")


## ----geomErrorbar2a, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
ggplot(warpbreaks.sum) +
    geom_linerange(aes(x = wool, ymin = Lower, ymax = Upper))


## ----geomErrorbar3a, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
ggplot(warpbreaks.sum) +
    geom_pointrange(aes(y=Mean, x = wool, ymin = Lower, ymax = Upper))


## ----coord, results='markup',fig.cap="", fig.width=7, fig.height=5,out.width='500px'----
head(CO2,3)
ggplot(CO2) + geom_point(aes(x = conc, y = uptake))+
  coord_cartesian() #default


## ----coord2, results='markup',fig.cap="", fig.width=7, fig.height=5,out.width='500px'----
head(CO2,3)
ggplot(CO2) + geom_point(aes(x = conc, y = uptake))+
  coord_polar()


## ----coord3, results='markup',fig.cap="", fig.width=7, fig.height=5,out.width='500px'----
head(CO2,3)
ggplot(CO2) + geom_point(aes(x = conc, y = uptake))+
 coord_flip()


## ----coord4, results='markup',fig.cap="", fig.width=7, fig.height=5,out.width='450px', tidy=FALSE----
#Orthographic coordinates
library(maps)
library(mapdata)
aus <- map_data("worldHires", region = "Australia")
ggplot(aus, aes(x = long, y = lat, group = group)) +
  coord_map("ortho", orientation=c(-20, 125, 23.5))+
  geom_polygon()


## ----scalex, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
head(CO2,2)
ggplot(CO2, aes(y = uptake, x = conc)) + geom_point()+
  scale_x_continuous(name = "CO2 conc")


## ----scalex2, results='markup',fig.width=4, fig.height=4, out.width='260px', tidy=FALSE----
head(CO2,2)
ggplot(CO2, aes(y = uptake, x = conc)) + geom_point()+
    scale_x_continuous(name = expression(
         Ambient~CO[2]~concentration~(mg/l)))


## ----scalex3, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
ggplot(CO2, aes(y = uptake, x = conc)) + geom_point()+
  scale_x_continuous(name = "CO2 conc", expand = c(0,200))


## ----scalex4, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
ggplot(CO2, aes(y = uptake, x = conc)) + geom_point()+
  scale_x_log10(name = "CO2 conc",
    breaks = as.vector(c(1, 2, 5, 10) %o% 10^(-1:2)))


## ----scalex5, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
ggplot(CO2, aes(y = uptake, x = Treatment)) + geom_point()+
  scale_x_discrete(name = "Treatment")


## ----scaleSize1, results='markup',fig.width=4, fig.height=3.5, out.width='300px', tidy=FALSE----
state=data.frame(state.x77, state.region, state.division, state.center) %>%
    select(Illiteracy, state.region, x, y)
head(state,2)
ggplot(state, aes(y = y, x = x)) + geom_point(aes(size = Illiteracy))


## ----scaleSize2, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
head(state,2)
ggplot(state, aes(y = y,x = x)) + geom_point(aes(size = state.region))+
  scale_size_discrete(name = "Region", range = c(2, 10))


## ----scaleSize3, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
head(state,2)
ggplot(state, aes(y = y, x = x)) + geom_point(aes(size = state.region))+
    scale_size_manual(name = "Region", values = c(2, 5, 6, 10))


## ----scaleShape1, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
head(CO2,2)
ggplot(CO2, aes(y = uptake,x=conc)) + geom_point(aes(shape = Treatment))


## ----scaleShape2, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
CO2 = CO2 %>% mutate(Comb = interaction(Type, Treatment))
CO2 %>% pull(Comb) %>% levels
ggplot(CO2, aes(y = uptake,x = conc)) + geom_point(aes(shape = Comb))+
  scale_shape_discrete(name = "Type",
   labels=c("Quebec non-chilled","Quebec chilled",
            "Miss. non-chilled","Miss. chilled"))


## ----scaleLinetype, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
head(CO2,2)
ggplot(CO2, aes(y = uptake, x = conc)) + geom_smooth(aes(linetype = Comb)) +
  scale_linetype_discrete(name = "Type")


## ----scaleLinetype1, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
head(CO2,2)
ggplot(CO2, aes(y = uptake, x = conc)) +
  geom_smooth(aes(linetype = Treatment)) +
  scale_linetype_manual(name = "Treatment", values = c("dashed","dotted"))


## ----scaleFill1, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
head(faithfuld,2)
ggplot(faithfuld, aes(waiting, eruptions)) +
    geom_raster(aes(fill = density)) +
    scale_fill_continuous(low = 'red', high = 'blue')


## ----scaleFill2, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
head(faithfuld,2)
ggplot(faithfuld, aes(waiting, eruptions)) +
  geom_raster(aes(fill = density)) +
  scale_fill_gradient2(low = 'red', mid = 'white', high = 'blue',
                       midpoint = 0.02)


## ----scaleFill3, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
head(faithfuld,2)
ggplot(faithfuld, aes(waiting, eruptions)) +
  geom_raster(aes(fill = density)) +
  scale_fill_gradientn(colours = terrain.colors(10))


## ----scaleFill4, results='markup',fig.width=4, fig.height=4, out.width='300px', tidy=FALSE----
head(faithfuld,2)
ggplot(faithfuld, aes(waiting, eruptions)) +
  geom_raster(aes(fill = density)) +
  scale_fill_viridis_c(option = 'D')
##also try scale_fill_viridis_b
## also options= 'A', 'B', 'C'


## ----palettes, results='markdown', eval=TRUE,fig.width=7, fig.height=7, out.width='600px'----
RColorBrewer::display.brewer.all()


## ----palettes1, results='markdown', eval=TRUE,fig.width=7, fig.height=2, out.width='600px'----
RColorBrewer::brewer.pal(n=8, name='Set1')
RColorBrewer::display.brewer.pal(n=8, name='Set1')


## ----facet1, fig.cap="", fig.width=7, fig.height=5,out.width='500px'----------
ggplot(CO2) + geom_point(aes(x = conc, y = uptake, colour = Type))+ 
### <b>
    facet_wrap(~Plant)
### </b>


## ----facet1a, fig.cap="", fig.width=7, fig.height=5,out.width='500px'---------
ggplot(CO2) + geom_line(aes(x = conc, y = uptake, colour = Type))+ 
### <b>
    facet_wrap(~Plant, scales = 'free_y')
### </b>


## ----facet2, fig.cap="", fig.width=7, fig.height=5,out.width='500px'----------
ggplot(CO2) + geom_point(aes(x = conc, y = uptake, colour = Type))+ 
### <b>
    facet_grid(Type~Treatment)
### </b>


## ----multiplot, fig.cap="", fig.width=7, fig.height=5,out.width='500px'-------
g1 <- ggplot(CO2) + geom_point(aes(x = conc, y = uptake, colour = Type))
g2 <- ggplot(CO2) + geom_point(aes(x = Treatment, y = uptake))
### <b>
grid.arrange(g1,  g2)
### </b>


## ----multiplot2, fig.cap="", fig.width=7, fig.height=5,out.width='500px'------
g1 <- ggplot(CO2) + geom_point(aes(x = conc, y = uptake, colour = Type))
g2 <- ggplot(CO2) + geom_point(aes(x = Treatment, y = uptake))
### <b>
g1/g2
### </b>


## ----multiplot3, fig.cap="", fig.width=12, fig.height=5,out.width='500px'-----
g1 <- ggplot(CO2) + geom_point(aes(x = conc, y = uptake, colour = Type))
g2 <- ggplot(CO2) + geom_point(aes(x = Treatment, y = uptake))
### <b>
g1+g2
### </b>


## ----theme1, fig.cap="", fig.width=7, fig.height=5,out.width='500px'----------
ggplot(CO2, aes(y = uptake, x = conc)) + geom_smooth() +
    geom_point() +
### <b>
    theme_classic()
### </b>


## ----theme2, fig.cap="", fig.width=7, fig.height=5,out.width='500px'----------
ggplot(CO2, aes(y = uptake, x = conc)) + geom_smooth() +
    geom_point() +
### <b>
    theme_bw()
### </b>


## ----theme3, fig.cap="", fig.width=7, fig.height=5,out.width='500px'----------
ggplot(CO2, aes(y = uptake, x = conc)) + geom_smooth() +
    geom_point() +
### <b>
    theme_grey()
### </b>


## ----theme4, fig.cap="", fig.width=7, fig.height=5,out.width='500px'----------
ggplot(CO2, aes(y = uptake, x = conc)) + geom_smooth() +
    geom_point() +
### <b>
    theme_minimal()
### </b>


## ----theme5, fig.cap="", fig.width=7, fig.height=5,out.width='500px'----------
ggplot(CO2, aes(y = uptake, x = conc)) + geom_smooth() +
    geom_point() +
### <b>
    theme_linedraw()
### </b>


## ----theme6, fig.cap="", fig.width=7, fig.height=5,out.width='500px'----------
ggplot(CO2, aes(y = uptake, x = conc)) + geom_smooth() +
    geom_point() +
### <b>
    theme_light()
### </b>


## ----xkcd, fig.cap="", fig.width=3, fig.height=5,out.width='100px', results='hide'----
png('resources/xkcd.png', width=500, height=500, res=200)
library(xkcd)  
library(sysfonts)
library(extrafont)
download.file("http://simonsoftware.se/other/xkcd.ttf", dest="xkcd.ttf")
font_import(".")
loadfonts()
xrange <- range(CO2$conc)
yrange <- range(CO2$uptake)
ggplot(CO2, aes(y = uptake, x = conc)) + geom_smooth(position='jitter', size=1.5) +
    #geom_point() +
        theme_minimal()+theme(text=element_text(size=16, family='xkcd'))+
        xkcdaxis(xrange, yrange)

dev.off()        


## ----saving, results='markup', eval=FALSE, tidy=FALSE-------------------------
## g1 <- ggplot(CO2) + geom_point(aes(x = conc, y = uptake, colour = Type))
## ### <b>
## ggsave(filename='figure1.pdf', g1,  width=7,  height=5)
## ### </b>


## ---- results='markup', eval=TRUE, tidy=FALSE---------------------------------
head(state)


## ----statesum, results='markup', eval=TRUE, tidy=FALSE,fig.width=4, fig.height=4, out.width='300px', warning=FALSE----
library(gmodels)
state.sum = state %>% group_by(state.region) %>%
    summarise(Mean = mean(Illiteracy),
              Lower = ci(Illiteracy)[2],
              Upper = ci(Illiteracy)[3])
state.sum
ggplot(state.sum, aes(y = Mean, x = state.region)) + geom_point() +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), width=0.1)


## ----statesumA, results='markup', fig.cap='',eval=TRUE, tidy=FALSE,fig.width=4, fig.height=4, out.width='300px'----
ggplot(state.sum, aes(y = Mean, x = state.region)) + geom_point() +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1) +
    scale_x_discrete("Region") +
    scale_y_continuous("Illiteracy rate (%)")+
    theme_classic() +
    theme(axis.line.y = element_line(),
          axis.line.x = element_line())


## ----statesum1, results='markup', eval=TRUE, tidy=FALSE,fig.width=4, fig.height=4, out.width='300px', warning=FALSE----
library(gmodels)
state.sum = state %>% group_by(state.region) %>%
    mutate(mean_sdl(Illiteracy))
state.sum
ggplot(state.sum, aes(y = y, x = state.region)) + geom_point() +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.1)


## ----statesum2, results='markup', eval=TRUE, tidy=FALSE,fig.width=4, fig.height=4, out.width='300px', warning=FALSE----
library(gmodels)
state.sum = state %>% group_by(state.region) %>%
    mutate(mean_cl_boot(Illiteracy))
state.sum
ggplot(state.sum, aes(y = y, x = state.region)) + geom_point() +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.1)


## ----statemap, results='markup', fig.cap='',eval=TRUE, tidy=FALSE,fig.width=4, fig.height=4, out.width='300px'----
library(mapdata)
US <- map_data("worldHires", region = "USA")
ggplot(US) +
    geom_polygon(aes(x = long, y = lat, group = group)) +
    geom_point(data = state, aes(y = y, x = x, size = Illiteracy),
               color = "red")


## ----statemapA, results='markup', fig.cap='',eval=TRUE, tidy=FALSE,fig.width=4, fig.height=4, out.width='300px'----
library(mapdata) 
US <- map_data("worldHires", region="USA")
ggplot(US) +
    geom_polygon(aes(x = long, y = lat, group = group)) +
    geom_point(data = state,aes(y = y, x = x, size = Illiteracy),
               color = "red")+
    coord_map(xlim = c(-150, -50), ylim = c(20, 60)) +
    theme_minimal()


## ---- results='markup', eval=TRUE, tidy=FALSE---------------------------------
MACNALLY <- read.csv('../data/macnally.csv',
  header = TRUE, row.names = 1, strip.white = TRUE)
head(MACNALLY)


## ----ggMacnally1, results='markup', eval=TRUE, tidy=FALSE, out.width='300px'----
library(gmodels)
ci(MACNALLY$GST)
MACNALLY.agg = MACNALLY %>% group_by(HABITAT) %>%
  summarize(Mean = mean(GST), Lower = ci(GST)[2], Upper = ci(GST)[3])
ggplot(MACNALLY.agg, aes(y = Mean, x = HABITAT)) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.1)+
    geom_point() + theme_classic()


## ----ggMacnally3, results='markup', eval=TRUE, tidy=FALSE, out.width='300px'----
library(tidyverse)
MACNALLY.melt = MACNALLY %>%
  pivot_longer(-HABITAT, names_to = "variable",  values_to = "value")
ggplot(MACNALLY.melt, aes(y = value, x = HABITAT)) +
    stat_summary(fun.y = "mean", geom = "point")+
    stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",
                 width = 0.1)+
    facet_grid(~variable)


## ----ggMacnally4, results='markup', eval=TRUE, tidy=FALSE, out.width='300px'----
#and bootstrapped means..
ggplot(MACNALLY.melt, aes(y = value, x = HABITAT)) +
    stat_summary(fun.y = "mean", geom = "point")+
    stat_summary(fun.data = "mean_cl_boot", geom = "errorbar",
                 width = 0.1)+
    facet_grid(~variable)



