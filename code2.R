setwd("/Users/kanp/AFP - Rayliant/Data - Rayliant")
require(data.table)
require(dplyr)
require(ggplot2)

cn_a = as.data.table(read.csv('China A Industry Returns.csv'))
cn_all = as.data.table(read.csv('China All Shares Industry Returns.csv'))
hrp = as.data.table(read.csv('HRP Industry Returns.csv'))
adr = as.data.table(read.csv('ADR Industry Returns.csv'))
linktable = as.data.table(read.csv('linkTable.csv'))

cn_a = dcast(cn_a, Date ~ iocd, value.var = 'vwRet')
cn_a[, Date := as.Date(Date)]

regret = function(data,lag,industry){
  n = ncol(data)
  y_index = which(colnames(data) %in% c(industry))
  if (length(y_index)==0) {stop("TypeError on Industry!")}
  for (i in colnames(data)[2:ncol(data)]) {
    data[, paste0('lag',i) := shift(get(i),lag)]
  }
  data_reg = data[, .SD, .SDcols = setdiff(c(3,(n+1):ncol(data)),y_index+n-1)]
  colnames(data_reg)[1] = 'y'
  out = lm(y ~ ., data = data_reg)
  summary(out)
}
#dtest = na.omit(cn_a)
#regret(dtest[1:60],3,14)

regret_sum = function(data,freq,lag,industry){
  output = hash()
  #output[['lag']] = paste(lag,'m')
  data = na.omit(data)
  n = nrow(data)
  for (i in 1:as.integer(n/freq)){
    data_ = data[(freq*(i-1)+1):(freq*i)]
    out = regret(data_,lag,industry)
    check = out$coefficients[,4]<0.05
    coefnames = names(out$coefficients[,1])[check]
    output_ = hash()
    coeflist = list()
    if (sum(check)>0) {
      for (j in 1:sum(check)){
      output_[[coefnames[j]]] = round(out$coefficients[check,1][j],2)
      }
    }
    output_[['R2']] = round(out$r.squared,2)
    output[[paste0(year(min(data_$Date)),'/',month(min(data_$Date)),
                   ' to ',year(max(data_$Date)),'/',month(max(data_$Date)))]] = output_
  }
  output
}

#regret_sum(cn_a,60,1,'FIRE')
#regret_sum(cn_a,60,1,24)
#tt=cbind(regret_sum(cn_a,60,1,6),regret_sum(cn_a,60,1,7))

regret_sum_all = function(data,freq,lag){
  out = hash()
  for (ind in colnames(data)[2:ncol(data)]){
    out[[ind]] = regret_sum(data,freq,lag,ind)
  }
  out
}

regsum = regret_sum_all(cn_a,60,1)




