setwd("/Users/kanp/AFP - Rayliant/Data - Rayliant")
require(data.table)
require(readxl)
require(dplyr)
require(ggplot2)
require(hash)
require(grid)
require(gridExtra)

# Set up ----
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
#linktable = as.data.table(read.csv('linkTable.csv'))
nrow(cn_a)-nrow(na.omit(cn_a))
nrow(cn_all)-nrow(na.omit(cn_all)) #too many NAs
nrow(adr)-nrow(na.omit(adr))
nrow(hrp)-nrow(na.omit(hrp))

cn_a = dcast(cn_a, Date ~ iocd, value.var = 'vwRet')
cn_a[, Date := as.Date(Date)]

cn_all = dcast(cn_all, Date ~ iocd, value.var = 'vwRet')
cn_all[, Date := as.Date(Date)]

hrp = dcast(hrp, Date ~ iocd, value.var = 'vwRet')
hrp[, Date := as.Date(Date)]

adr = dcast(adr, Date ~ iocd, value.var = 'vwRet')
adr[, Date := as.Date(Date)]


# Add currencies ----
setwd("/Users/kanp/AFP - Rayliant/Data_Currency")
cny_usd = as.data.table(read.csv('CNYUSD.csv'))[,.(Date,Close)]
cny_hkd = as.data.table(read.csv('CNYHKD.csv'))[,.(Date,Close)]
cny_jpy = as.data.table(read_excel('CNYJPY_daily.xlsx'))[,.(Date,JPY)]
cny_eur = as.data.table(read.csv('CNYEUR.csv'))[,.(Date,Close)]
cny_krw = as.data.table(read_excel('CNYKRW_daily.xlsx'))[,.(Date,KRW)]

#krw to monthly
cny_krw[,year:=year(Date)][,month:=month(Date)]
cny_krw = cny_krw[,list(Close=KRW[1],Date=Date[1]),by=.(year,month)]
cny_krw[,Date:=as.Date(Date)]
cny_krw = cny_krw[,.(Date,Close)]; setkey(cny_krw,Date)

#jpy to monthly
cny_jpy[,year:=year(Date)][,month:=month(Date)]
cny_jpy = cny_jpy[,list(Close=JPY[1],Date=Date[1]),by=.(year,month)]
cny_jpy[,Date:=as.Date(Date)]
cny_jpy = cny_jpy[,.(Date,Close)]; setkey(cny_jpy,Date)

#transform to end of month
for (tab in list(cny_usd,cny_hkd,cny_eur)) {
  tab[,Date:=as.Date(Date, format="%Y-%m-%d")-1]
  setkey(tab, Date)
}
#merge currencies
currency = cny_usd[cny_hkd[cny_jpy[cny_krw[cny_eur]]]]
colnames(currency) = c('Date','USD','HKD','JPY','KRW','EUR')

#merge currency with industries
setkey(currency,Date); setkey(cn_a,Date); setkey(cn_all,Date); setkey(adr,Date); setkey(hrp,Date)
cn_a2 = merge(cn_a,currency)
cn_all2 = merge(cn_all,currency)
adr2 = merge(adr,currency)
hrp2 = merge(hrp,currency)

# Add Macro,Industry: ----

#Add CPI https://fred.stlouisfed.org/series/CHNCPIALLMINMEI, Add Export
setwd("/Users/kanp/AFP - Rayliant/Data_Macro")
CPI = as.data.table(read.csv('CPIpctchg.csv'))
Export = as.data.table(read.csv('Exportpctchg.csv'))
colnames(CPI) = c('Date','CPIchg'); CPI[,Date:=as.Date(Date)-1]
colnames(Export) = c('Date','Exportchg') #end of month to be consistent
Export[,Date:=as.Date(Date)-1] #[,Exportchg:=Exportchg/100] #pct to decimal
setkey(CPI,Date); setkey(Export,Date)
cn_a3 = cn_a2[CPI[Export]]
cn_all3 = cn_all2[CPI[Export]]
adr3 = adr2[CPI[Export]]
hrp3 = hrp2[CPI[Export]]

###add PPI, Shortrate, INDPRO
require(mondate)
#Add PPI
PPI = as.data.table(read.csv('PPI.csv')); colnames(PPI) = c('Date','PPI'); PPI[,Date:=as.Date(Date)-1]; setkey(PPI,Date)
#Add Short-rate
Short_US = as.data.table(read.csv('OECD_Shortrates.csv'))[LOCATION=='USA',.(TIME,Value)]; colnames(Short_US) = c('Date','ShortUS')
Short_US[,Date:=as.mondate(as.Date(paste(Date,"-01",sep=""))-1)+1][,Date:=as.Date(Date)]; setkey(Short_US,Date) #convert to end-of-month
#Add INDPRO
INDPRO = as.data.table(read.csv('INDPROpctchg.csv')); colnames(INDPRO) = c('Date','US_INDPROchg'); INDPRO[,Date:=as.Date(Date)-1]; setkey(INDPRO,Date)
#Add CCI
CCI = as.data.table(read.csv('CCI.csv'))[LOCATION=='CHN',.(TIME,Value)]; colnames(CCI) = c('Date','CCI')
CCI[,Date:=as.mondate(as.Date(paste(Date,"-01",sep=""))-1)+1][,Date:=as.Date(Date)]; setkey(CCI,Date) #convert to end-of-month
#ADD BCI
BCI = as.data.table(read.csv('BCI.csv'))[LOCATION=='CHN',.(TIME,Value)]; colnames(BCI) = c('Date','BCI')
BCI[,Date:=as.mondate(as.Date(paste(Date,"-01",sep=""))-1)+1][,Date:=as.Date(Date)]; setkey(BCI,Date) #convert to end-of-month

cn_a3 = cn_a3[PPI[Short_US[INDPRO[CCI[BCI]]]]]
cn_all3 = cn_all3[PPI[Short_US[INDPRO[CCI[BCI]]]]]
adr3 = adr3[PPI[Short_US[INDPRO[CCI[BCI]]]]]
hrp3 = hrp3[PPI[Short_US[INDPRO[CCI[BCI]]]]]

#Transform to %Change
lst = c('USD','HKD','JPY','KRW','EUR','PPI','ShortUS','CCI','BCI')
for (colmn in lst){
  cn_a3[, paste0(colmn,"chg"):=get(colmn)/shift(get(colmn))-1]
  cn_all3[, paste0(colmn,"chg"):=get(colmn)/shift(get(colmn))-1]
  adr3[, paste0(colmn,"chg"):=get(colmn)/shift(get(colmn))-1]
  hrp3[, paste0(colmn,"chg"):=get(colmn)/shift(get(colmn))-1]
}
cn_a3 = cn_a3[,-..lst]; cn_all3 = cn_all3[,-..lst]; adr3 = adr3[,-..lst]; hrp3 = hrp3[,-..lst]

# Add real estate: cn_a4 (Don't use it now, skip to plot) ----
library(stringr)
real_estate = data.table::fread('Real_Estate.csv')
real_estate = t(real_estate)[,3:5]
#Real Estate Cumulative Investment (in 0.1B) , Cumulative Percentage
colnames(real_estate) = c('Date', 'RE_Inv', 'RE_Inv_Cum%')
real_estate = real_estate[-1,]
rownames(real_estate) = NULL
real_estate = as.data.table(real_estate)

# takes care of the Chinese date character 
real_estate[, Date := str_replace(Date, '年', '年-0')]
real_estate[, Date := str_replace(Date, '月', '-01')]
real_estate[, Date := str_replace(Date, '年-010', '-10')]
real_estate[, Date := str_replace(Date, '年-011', '-11')]
real_estate[, Date := str_replace(Date, '年-012', '-12')]
real_estate[, Date := str_replace(Date, '年', '')]
real_estate[, Date := as.Date(Date)]
setorder(real_estate, Date)

# merge real estate data to entire dataset 
real_estate[, Year := year(Date)]
real_estate[, Mon := month(Date)]
real_estate$Date = NULL
cn_a3[, Year := year(Date)]; cn_all3[, Year := year(Date)]; adr3[, Year := year(Date)]; hrp3[, Year := year(Date)]
cn_a3[, Mon := month(Date)]; cn_all3[, Mon := month(Date)]; adr3[, Mon := month(Date)]; hrp3[, Mon := month(Date)]
for (colmn in subset(colnames(real_estate),!(colnames(real_estate) %in% c('Year','Month','Date')))){
  real_estate[,paste0(colmn):=as.numeric(get(colmn))]
}
cn_a4 = merge(cn_a3, real_estate, by = c('Year', 'Mon'), all.x = TRUE)
cn_all4 = merge(cn_all3, real_estate, by = c('Year', 'Mon'), all.x = TRUE)
adr4 = merge(adr3, real_estate, by = c('Year', 'Mon'), all.x = TRUE)
hrp4 = merge(hrp3, real_estate, by = c('Year', 'Mon'), all.x = TRUE)
cn_a4$`RE_Inv_Cum%` = cn_all4$`RE_Inv_Cum%` = adr4$`RE_Inv_Cum%` = hrp4$`RE_Inv_Cum%` =NULL

cn_a4$`RE_Inv` = zoo::na.locf(cn_a4$`RE_Inv`, fromLast = TRUE)
cn_all4$`RE_Inv` = zoo::na.locf(cn_all4$`RE_Inv`, fromLast = TRUE)
adr4$`RE_Inv` = zoo::na.locf(adr4$`RE_Inv`, fromLast = TRUE)
hrp4$`RE_Inv` = zoo::na.locf(hrp4$`RE_Inv`, fromLast = TRUE)

# cn_a4[, RE_INVchg:=RE_Inv/shift(RE_Inv)-1, by = .(Year)]
# cn_all4[, RE_INVchg:=RE_Inv/shift(RE_Inv)-1, by = .(Year)]
# adr4[, RE_INVchg:=RE_Inv/shift(RE_Inv)-1, by = .(Year)]
# hrp4[, RE_INVchg:=RE_Inv/shift(RE_Inv)-1, by = .(Year)]

cn_a3$Year = cn_a3$Mon = cn_a4$Year = cn_a4$Mon = NULL
cn_all3$Year = cn_all3$Mon = cn_all4$Year = cn_all4$Mon = NULL
adr3$Year = adr3$Mon = adr4$Year = adr4$Mon = NULL
hrp3$Year = hrp3$Mon = hrp4$Year = hrp4$Mon = NULL

cn_a4$RE_Inv = cn_all4$RE_Inv = adr4$RE_Inv = hrp4$RE_Inv = NULL
cn_a4[mapply(is.infinite, cn_a4)] = cn_all4[mapply(is.infinite, cn_a4)] = NA
adr4[mapply(is.infinite, adr4)] = hrp4[mapply(is.infinite, hrp4)] = NA

# Plot Function ----

plot_corr_lag = function(input,lag){
  data = na.omit(subset(input, select = -c(Date)))
  data_lead = data[2:.N]
  data_lag = data[1:(.N-1)]
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
                       scale_fill_gradient_fun = scale_fill_gradient2(low="blue",high="red",midpoint=0,limits=c(-1,1)),
                       xlab = "Industry Lags", ylab = "Industry Portfolio", 
                       main = paste0("Correlation of Industry Portfolio to Industry Lags ",
                                     year(min(input$Date)),"-",
                                     year(max(input$Date))))
}
plot_subperiod = function(y1,x1,lag,input){
  data = input
  colnames(data) = paste0('s',colnames(data))
  plt1 = ggplot(data[sDate>="2006-01-01"&sDate<="2009-12-31"],aes_string(x=x1,y=y1)) + geom_point() + geom_smooth() + ggtitle('2006-2009')
  plt2 = ggplot(data[sDate>="2010-01-01"&sDate<="2014-12-31"],aes_string(x=x1,y=y1)) + geom_point() + geom_smooth() + ggtitle('2010-2014')
  plt3 = ggplot(data[sDate>="2015-01-01"&sDate<="2019-12-31"],aes_string(x=x1,y=y1)) + geom_point() + geom_smooth() + ggtitle('2015-2019')
  plt4 = ggplot(data[sDate>="2006-01-01"&sDate<="2019-12-31"],aes_string(x=x1,y=y1)) + geom_point() + geom_smooth() + ggtitle('2006-2019')
  grid.arrange(plt1,plt2,plt3,plt4, ncol=2, nrow=2, top = paste0(substr(y1,2,nchar(x1)),' vs ',substr(x1,2,nchar(x1))))
}
plot_sig_lag = function(input,lag,heatmap=TRUE,y_industry=1:ncol(input)-1,regressor=1:ncol(input)-1){
  inputdateref = na.omit(input)
  data = subset(input, select = -c(Date))
  col_regressor = colnames(data)[regressor]
  data_lead = data[(lag+1):.N]; data_lead = subset(data_lead , select = y_industry)
  data_lag = data[1:(nrow(data)-lag)]; data_lag = subset(data_lag , select = col_regressor)
  out = out_coef = data.frame()
  for (colmn in colnames(data_lead)) { #max 15 industries for y
    dt = cbind(data_lead[,..colmn],data_lag)
    colnames(dt)[1] = 'y'
    reg = summary(lm(y~., data = dt))
    check = reg$coefficients[,4]<0.05 #pvalue < 0.05
    check = check[-1] #disregard intercept
    vec = as.numeric(check); vec_coef = reg$coefficients[-1,1]
    for (k in 1:length(vec)){
      if (vec[k]==1 && vec_coef[k]<0) {vec[k]=-1}
    }
    out = rbind(out,vec); out_coef = rbind(out_coef,vec_coef)
  }
  rownames(out) = rownames(out_coef) = colnames(data)[y_industry]
  colnames(out) = colnames(out_coef) = paste0("lag",lag,"_",colnames(data_lag))
  out_coef = round(out_coef,2)
  #out
  #nonzerocol = function(x) sum(x==0)!=length(x) # filter out columns that all are zeroes(insignificant)
  #out_nonzero = out %>% select_if(nonzerocol); clmnlist = colnames(out_nonzero)
  #out_coef_nonzero = out_coef[,clmnlist]
  out_coef_nonzero = out_coef #temp adj
  if (heatmap == TRUE) {
    heatmaply::heatmaply(out,
                         dendrogram = "none",
                         grid_color = "seashell2", grid_gap = 0.01,
                         scale_fill_gradient_fun = scale_fill_gradient2(low="orangered",high="dodgerblue4",midpoint=0,limit=c(-1,1)),
                         hide_colorbar = TRUE,
                         xlab = "Industry Lags", ylab = "Industry Portfolio", 
                         main = paste0("Significant Lags on Industry Portfolio (lag = ",lag,") ",
                                       year(min(inputdateref$Date)),"/",month((min(inputdateref$Date))),"-",
                                       year(max(inputdateref$Date)),"/",month((max(inputdateref$Date)))))
  } else {
    grid.arrange(tableGrob(out_coef_nonzero, theme = ttheme_default(7),
                           cols=gsub("\\.", "\\\n",names(out_coef_nonzero))),
                 top = paste0("Coefficients of Lag ",lag,"\n",
                              year(min(inputdateref$Date)),"/",month((min(inputdateref$Date))),"-",
                              year(max(inputdateref$Date)),"/",month((max(inputdateref$Date)))))
  }
}

# Clean ADR and HRP ----

#cn_a3[is.infinite(PPIchg), PPIchg:=0]
#cn_all[!is.na(`11`)]

#ADR 2008-2016: exclude 6
adr = subset(adr[Date>="2008-01-31" & Date<="2016-12-31"], select = -c(`6`))
adr_ = subset(adr4[Date>="2008-01-31" & Date<="2016-12-31"], select = -c(`6`))
adr_na = zoo::na.locf(adr_)
#library(imputeTS); adr_mean = na_mean(adr_)

#HRP 2000-2020: exclude 31G and FIRE >> 2005-2020 (Macro available from 2005)
hrp = subset(hrp[Date>="2000-01-31" & Date<="2020-12-31"], select = -c(`31G`,FIRE))
hrp_ = subset(hrp4[Date>="2000-01-31" & Date<="2020-12-31"], select = -c(`31G`,FIRE))
hrp_na = zoo::na.locf(hrp_)

# #filter na columns first :(
# nacol = function(x) sum(is.na(x))<0.3*length(x) # filter out columns NAs>30%
# cn_a_ = cn_a %>% select_if(nacol) # only cn_a has comprehensive data
# cn_all_ = cn_all %>% select_if(nacol)
# adr_ = adr %>% select_if(nacol)
# hrp_ = hrp %>% select_if(nacol)
# cn_all3_ = cn_all3 %>% select_if(nacol)
# adr3_ = adr3 %>% select_if(nacol)
# hrp3_ = hrp3 %>% select_if(nacol)

# #tables
# plot_sig_lag(cn_a,1,F) #choose column range of y and x (refer to input, exclude Date); default: all columns available
# plot_sig_lag(cn_all_,1,F,1:2,1:2)

# plot_sig_lag(cn_a3,1,F)
# plot_sig_lag(cn_all3,1,F)
# plot_sig_lag(adr3,1,F)
# plot_sig_lag(hrp3,1,F)

# Visualization ----

#plot
plot_sig_lag(cn_a4,3,F,1:15) #choose column range of y and x (refer to input)
plot_sig_lag(cn_a4[Date>='2015-01-31'&Date<='2019-12-31'],1,T,1:15)

plot_sig_lag(adr_na,1,T,1:14)
plot_sig_lag(adr_na,3,F,1:14)

plot_sig_lag(hrp_na,1,T,1:13)
plot_sig_lag(hrp_na,3,F,1:13)





plot_sig_lag(adr,1,T)
plot_sig_lag(adr,1,F)

plot_sig_lag(hrp,1,T)
plot_sig_lag(hrp,1,F)




#relationship
plot_subperiod('sFIRE','sUSDchg',1,cn_a3) #put 's' in front



## Don't have to use below

# Functions (Don't have to use this) ----

regret = function(data,lag,industry){
  n = ncol(data)
  y_index = which(colnames(data) %in% c(industry))
  if (length(y_index)==0) {stop("TypeError on Industry!")}
  for (i in colnames(data)[2:ncol(data)]) {
    data[, paste0('lag',i) := shift(get(i),lag)]
  }
  #data_reg = data[, .SD, .SDcols = setdiff(c(3,(n+1):ncol(data)),y_index+n-1)] #disregard its own lag
  data_reg = data[, .SD, .SDcols = c(y_index,(n+1):ncol(data))] #include its own lag
  colnames(data_reg)[1] = 'y'
  out = lm(y ~ ., data = data_reg)
  summary(out)
}

regret_sum = function(data,freq,lag,industry){
  output = hash()
  data = data[rowSums(is.na(data)) < ncol(data)/2, ]
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
#regsum = regret_sum_all(cn_a,15,1)


# lasso ----

require(lfe)
require(glmnet)
require(plotmo)
lasso = function(input,predcol,lag,norm=F){ #put 's' in front
  lst = c('Date')
  input = na.omit(input[,-..lst])
  if (norm==T) {input = as.data.table(apply(input,2,function(x) (x-mean(x))/sd(x)))}
  X = input[1:nrow(input)-lag]; lst_x = colnames(X) = paste0("lag",lag,'_',colnames(input))
  y = input[(1+lag):nrow(input),..predcol]; temp = y; colnames(temp) = c('y')
  M = cbind(y,X)
  fmla = as.formula(paste0(y," ~ ", paste(lst_x, collapse = "+")))
  X_train = model.matrix(fmla, data = M)[,-c(1)]
  lasso_model = cv.glmnet(x=X_train,y = temp$y, family = "gaussian", alpha=1, nfolds=5)
  par(mfrow = c(1,2))
  plot(lasso_model$glmnet.fit, 'lambda', label = TRUE, main = 'Coef')
  plot(lasso_model, main = 'MSE')
  b_min = coef(lasso_model,lasso_model$lambda.min)
  b_1se = coef(lasso_model,lasso_model$lambda.1se)
  print(cbind(b_min,b_1se))
  #print(X_train)
}
#input = cn_a4[,1:(ncol(cn_a4)-1)]
lasso(cn_a4[,1:(ncol(cn_a4)-1)],'11',1,T)

# Regression Summary (Should clean NAs/filer only non-NAs period for input) ----

#regsum4 = regret_sum_all(cn_a4,90,1)
#regsum4_last5 = regret_sum_all(cn_a4[Date>="2015-01-01"&Date<="2019-12-31"],30,1)


