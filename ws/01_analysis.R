## ---- loadLibraries
library(tidyverse)
library(vegan)
library(GGally)
library(corrplot)
library(car)
library(mvabund)
library(scales)
## ----

## ---- ReadData
data <- read_csv('../data/data.csv', trim_ws=TRUE)
head(data) 
enviro <- read_csv('../data/enviro.csv', trim_ws=TRUE)
head(enviro)
## ----
