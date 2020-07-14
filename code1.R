setwd("/Users/kanp/AFP - Rayliant")
require(data.table)
require(ggplot2)
require(directlabels)
require(ggpubr)

gc1 = as.data.table(read.csv('gc1.csv', stringsAsFactors = F))

data =  copy(gc1)
colnames(data)[1:7] = c('date','comp','exch','ind','sec','gvkey','prc')
exchange = c("Hong Kong-Shanghai Stock Connect (SB)",
             "Hong Kong-Shenzhen Stock Connect (SB)",
             "Hong Kong Exchanges and Clearing Ltd",
             "Shanghai Stock Exchange",
             "Shenzhen Stock Exchange",
             "Shanghai-Hong Kong Stock Connect (NB)",
             "Shenzhen-Hong Kong Stock Connect (NB)")

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

##








