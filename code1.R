setwd("/Users/kanp/AFP - Rayliant/Data")

require(data.table)
require(ggplot2)
require(directlabels)
require(ggpubr)

gc1 = as.data.table(read.csv('gc1.csv', stringsAsFactors = F))

data =  copy(gc1)
colnames(data)[1:7] = c('date','comp','exch','ind','sec','gvkey','prc')
exchange = c("Shanghai Stock Exchange",
             "Shenzhen Stock Exchange",
             "Hong Kong-Shanghai Stock Connect (SB)",
             "Hong Kong-Shenzhen Stock Connect (SB)",
             "Shanghai-Hong Kong Stock Connect (NB)",
             "Shenzhen-Hong Kong Stock Connect (NB)",
             "Hong Kong Exchanges and Clearing Ltd")

# clean company name shifts ' LTD.'
index1 = which(data$exch==' LTD.'); index2 = index1+1; index = append(index1,index2)
data = data[-index]
temp1 = data[index1]; temp2 = data[index2,1]; temp = cbind(temp1,temp2)
temp[,comp:=paste0(comp,exch)]; temp$exch = NULL; colnames(temp) = colnames(data)
data = rbind(data,temp) # put it back to data

# clean industry shifts
integer = seq(0,9,1)
temp = data[!grep(paste(integer, collapse="|"), data$gvkey),] #gvkey is not number cuz shifted by industry
data = data[grep(paste(integer, collapse="|"), data$gvkey),] #gvkey is number
index1 = which(temp$comp==''); temp1 = temp[index1,1]
temp[index1-1,newcol:=temp1]; temp = temp[-index1]
temp[,ind:=paste0(ind,sec)]; temp$sec = NULL; colnames(temp) = colnames(data)
data = rbind(data,temp)

# firms that have exch attached to the end of company name have incomplete information
data = data[!grep(paste(exchange, collapse="|"), data$comp),]
data = data[exch %in% exchange]
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

##



