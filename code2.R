setwd("/Users/kanp/AFP - Rayliant/Data - Rayliant")
require(data.table)
require(dplyr)
require(ggplot2)

cn_a = as.data.table(read.csv('China A Industry Returns.csv'))
cn_all = as.data.table(read.csv('China All Shares Industry Returns.csv'))
hrp = as.data.table(read.csv('HRP Industry Returns.csv'))
adr = as.data.table(read.csv('ADR Industry Returns.csv'))
linktable = as.data.table(read.csv('linkTable.csv'))

# 31G = 11,23
cn_a = dcast(cn_a, Date ~ iocd, value.var =  'vwRet')

regret = function(data,lag,industry){
  n = ncol(data)
  y_index = which(colnames(data) %in% c(industry))
  for (i in colnames(data)[2:ncol(data)]) {
    data[, paste0('lag',i) := shift(get(i),lag)]
  }
  data_reg = data[, .SD, .SDcols = setdiff(c(3,(n+1):ncol(data)),y_index+n-1)]
  colnames(data_reg)[1] = 'y'
  out = lm(y ~ ., data = data_reg)
  summary(out)
}
cn_a = na.omit(cn_a)
cn_a[, Date := as.Date(Date)]
#cn_a = cn_a[Date>'2000-01-01']
regret(cn_a[1:60],3,11)
regret(cn_a[61:120],3,11)
regret(cn_a[121:180],3,11)
regret(cn_a[181:254],5,23)









