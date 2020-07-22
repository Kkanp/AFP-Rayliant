setwd("/Users/kanp/AFP - Rayliant/Data")
require(data.table)
require(dplyr)
require(ggplot2)
require(directlabels)
require(ggpubr)

gc1 = as.data.table(read.csv('gc3.csv', stringsAsFactors = F))

data =  copy(gc1)
colnames(data)[1:9] = colnames(gc1)[1:9] = c('date','comp','exch','ind','sec','ticker','gticker','gvkey','prc')
exchange = c("Shanghai Stock Exchange",
             "Shenzhen Stock Exchange",
             "Hong Kong-Shanghai Stock Connect (SB)",
             "Hong Kong-Shenzhen Stock Connect (SB)",
             "Shanghai-Hong Kong Stock Connect (NB)",
             "Shenzhen-Hong Kong Stock Connect (NB)",
             "Hong Kong Exchanges and Clearing Ltd")

# clean company name shifts ' LTD.'
index1 = which(data$exch==' LTD.'); index2 = index1+1; index = append(index1,index2)
temp1 = data[index1]; temp2 = data[index2,1]; temp = cbind(temp1,temp2)
data = data[-index]
temp[,comp:=paste0(comp,exch)]; temp$exch = NULL; colnames(temp) = colnames(data)
data = rbind(data,temp) # put it back to data

# clean exch 'Broker'
index1 = which(data$exch=='Broker'); index2 = index1+1; index = append(index1,index2)
data = data[-index]

# clean industry shifts
index1 = which((data$sec==' Restaurants & Leisure')|
                 (data$sec==' Gas & Consumable Fuels')|
                 (data$sec==' Instruments & Components')|
                 (data$sec==' Storage & Peripherals')|
                 (data$sec==' Apparel & Luxury Goods')); index2 = index1+1; index = append(index1,index2)
temp1 = data[index1]; temp2 = data[index2,1]; temp = cbind(temp1,temp2)
data = data[-index]
temp[,ind:=paste0(ind,sec)]; temp$sec = NULL; colnames(temp) = colnames(data)
data = rbind(data,temp) # put it back to data

# check firms with ticker
integer = seq(0,9,1)
notickerfirm = unique(data[!grep(paste(integer, collapse="|"), data$ticker), comp]) #firms without ticker
data = data[grep(paste(integer, collapse="|"), data$ticker)]
tickerfirm = unique(data$comp)

# date
data[,date := as.Date(date, format = '%m/%d/%Y')]

# remove duplicates on each date
data[, dup:=.N>1, by=.(comp,date)][, no_of_exch:=length(unique(exch)), by=.(comp,date)]
data[, main_exch:=("Shanghai Stock Exchange" %in% unique(exch))
     |("Shenzhen Stock Exchange" %in% unique(exch))
     |("Hong Kong Exchanges and Clearing Ltd" %in% unique(exch))
       , by=.(comp,date)]
setorder(data,comp,date)
data[(dup==TRUE)&(no_of_exch==1), valid:=seq(1,.N,1), by=.(comp,date)] #duplicates but same exchange > use first observation
data[(dup==TRUE)&(no_of_exch>1)&(main_exch==TRUE), valid:=(exch %in% exchange[c(1,2,7)]), by=.(comp,date)] #duplicates with main exchange > use main exchange
data[(dup==TRUE)&(no_of_exch>1)&(main_exch==FALSE), valid:=seq(1,.N,1), by=.(comp,date)] #duplicates without main exchange > use only one
data[(dup==FALSE), valid:=1]
data = data[valid==1]; data$dup = data$no_of_exch = data$main_exch = data$valid = NULL

# # check
# dt1 = unique(data[exch==exchange[1],]$comp) #SSE-HK SB
# dt3 = unique(data[exch==exchange[3],]$comp) #SSE
# dt5 = unique(data[exch==exchange[5],]$comp) #SSE-HK NB
# intersect(dt1,dt5)
# intersect(dt1,dt3)
# intersect(dt3,dt5)

# filter exchange and date
dt = data[(exch=="Shanghai Stock Exchange")|(exch=="Shenzhen Stock Exchange")]
dt = dt[(date>"2010-01-01")]

all_na = function(x) any(!is.na(x)) # remove columns that have only NAs
dt = dt %>% select_if(all_na)
limit_na = function(x) sum(is.na(x))/length(x)<0.2 # filter out columns that have more than 20% NAs, otherwise too few observations
dt = dt %>% select_if(limit_na)

# setdiff(colnames(data),colnames(dt))

dt = dt[!is.na(Institution.Ownership.Level..IO_Level.)&(Institution.Ownership.Level..IO_Level.!=0)] # choose firms with institution ownership

length(unique(dt$ticker))
length(unique(dt$ind))
length(unique(dt$gvkey))
unique(dt$sec)
unique(dt$ind)
dt[,count:=.N, by=.(ticker)]
test=dt[,list(c=count[.N]), by = ticker]; mean(test$c) # avg months per firm


