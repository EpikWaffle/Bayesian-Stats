list.of.packages <- c("TeachBayes", "ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(TeachBayes)
library(ggplot2)

beta_quantile(0.5, c(2, 8))

beta.select(list(x = 15, p = 0.5),
            list(x = 40, p = 0.9))

