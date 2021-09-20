## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE)


## ----libraries, results='markdown', eval=TRUE, message=FALSE, warning=FALSE----
library(tidyverse) #for data wrangling


## ----getData, results='markdown', eval=TRUE-----------------------------------
load(file='../data/manipulationDatasets.RData')
dat.1 %>% head


## ---- results='markup'--------------------------------------------------------
head(dat.1)
#OR
dat.1 %>% head()
#OR
dat.1 %>% head

