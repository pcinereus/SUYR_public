## ---- loadLibraries
library(tidyverse)
## ----end

## ---- getData
data(BOD)
glimpse(BOD)
## ----end

## ---- EDA
ggplot(BOD) + geom_point(aes(y=demand, x=Time))
## ----end


