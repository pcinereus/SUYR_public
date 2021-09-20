## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE)


## ----libraries, results='markdown', eval=TRUE, message=FALSE, warning=FALSE----
library(tidyverse) #for data wrangling
library(grid)
library(patchwork)
library(scales)
library(gridExtra)


## ---- results='markup'--------------------------------------------------------
head(BOD)
summary(BOD)

