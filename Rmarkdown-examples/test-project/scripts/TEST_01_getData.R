## ---- loadLibraries
library(tidyverse)
## ----end

## ---- getData
data(BOD)
glimpse(BOD)
## ----end


## ---- processData
BOD <- BOD %>% filter(demand>10)
save(BOD,  file='../data/processed/BOD.RData')
## ----end


## ---- EDA
load(file='../data/processed/BOD.RData')
ggplot(BOD) + geom_point(aes(y=demand, x=Time))
## ----end


