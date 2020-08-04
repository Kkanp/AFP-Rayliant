setwd("/Users/kanp/AFP - Rayliant/Data - Rayliant")
require(data.table)
require(readxl)
require(dplyr)
require(ggplot2)
require(hash)

IO_ind = data.table(IO_Code = c('11','21','22','23','31G','42','44RT','48TW','51','FIRE','PROF','6','7','81','G'),
                    IO_Industry = c('Agriculture, forestry, fishing, and hunting',
                                    'Mining','Utilities','Construction','Manufacturing',
                                    'Wholesale trade','Retail trade','Transportation and warehousing',
                                    'Information','Finance, insurance, real estate, rental, and leasing',
                                    'Professional and business services',
                                    'Educational services, health care, and social assistance',
                                    'Arts, entertainment, recreation, accommodation, and food services',
                                    'Other services, except government','Government'))

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

regret_sum_all = function(data,freq,lag){
  out = hash()
  for (ind in colnames(data)[2:ncol(data)]){
    out[[ind]] = regret_sum(data,freq,lag,ind)
  }
  out
}
regsum = regret_sum_all(cn_a,60,1)

### Add currencies
setwd("/Users/kanp/AFP - Rayliant/Data_Currency")
cny_usd = as.data.table(read.csv('CNYUSD.csv'))[,.(Date,Close)]
cny_hkd = as.data.table(read.csv('CNYHKD.csv'))[,.(Date,Close)]
cny_jpy = as.data.table(read.csv('CNYJPY.csv'))[,.(Date,Close)]
cny_eur = as.data.table(read.csv('CNYEUR.csv'))[,.(Date,Close)]
cny_krw = as.data.table(read_excel('CNYKRW_daily.xlsx'))[,.(Date,KRW)]

#krw to monthly
cny_krw[,year:=year(Date)][,month:=month(Date)]
cny_krw = cny_krw[,list(Close=KRW[1]),by=.(year,month)]
cny_krw[,Date:=as.Date(paste0(year,"-",month,"-01"))]
cny_krw = cny_krw[,.(Date,Close)]; setorder(cny_krw,Date)

#merge currencies
for (tab in list(cny_usd,cny_hkd,cny_jpy,cny_krw,cny_eur)) {
  tab[,Date:=as.Date(Date, format="%Y-%m-%d")]
  setkey(tab, Date)
  print(str(tab))
}
str(cny_hkd)

currency = cny_usd[cny_hkd[cny_jpy[cny_krw[cny_eur]]]]
colnames(currency) = c('Date','USD','HKD','JPY','KRW','EUR')

#merge currency with industries
currency[,Date:=Date-1]
setkey(currency,Date); setkey(cn_a,Date)
cn_a2 = merge(cn_a,currency)

regsum2 = regret_sum_all(cn_a2,60,1)

###Visualization

plot_corr_lag = function(input,lag){
  data = na.omit(subset(input, select = -c(Date)))
  data_lead = data[2:.N]
  data_lag = data[1:(.N-1)]
  #colnames(data_lag) = paste0("lag",lag,"_",colnames(data))
  out = data.frame()
  for (colmn in colnames(data)[1:15]) { #only 15 industries for y
    dt = cbind(data_lead[,..colmn],data_lag)
    out = rbind(out,cor(dt)[1,-1])
  }
  rownames(out) = colnames(data)[1:15]
  colnames(out) = paste0("lag",lag,"_",colnames(data))
  out = round(out,4)
  #return(out)
  heatmaply::heatmaply(out,
                       dendrogram = "none",
                       xlab = "Industry Lags", ylab = "Industry Portfolio", 
                       main = "Correlation of Industry Portfolio to Industry Lags")
}

plot_corr_lag(cn_a,1)
plot_corr_lag(cn_a2,1)



