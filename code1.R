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

dt = dt[!is.na(Institution.Ownership.Level..IO_Level.) & (Institution.Ownership.Level..IO_Level.!=0)] # choose firms with institution ownership
# write.csv(dt, "gc3_cleaned.csv")

N = length(unique(dt$ticker))
N_sse = length(unique(dt[exch=='Shanghai Stock Exchange',ticker])); N_sze = N-N_sse
length(unique(dt$ind))
length(unique(dt$gvkey))
dt[,count:=.N, by=.(ticker)]
test=dt[,list(c=count[.N]), by = ticker]; mean(test$c); dt$count=NULL # avg months per firm

# summary tables: by sector
test = dt[,list(sec=sec[1],ind=ind[1]),by=.(ticker,comp)]
t1 = ggplot(test, aes(reorder(sec, -table(sec)[sec]), ..count..)) +
  geom_bar(aes(fill = reorder(sec, -table(sec)[sec])), position = "dodge") +
  labs(title="Firm Count by Sector", y="Count") +
  geom_text(stat='count', aes(label=..count..), vjust=-0.7) +
  scale_fill_discrete(name="Sector") +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())

# summary tables: by industry
t2 = ggplot(test, aes(reorder(ind, -table(ind)[ind]), ..count..)) +
  geom_bar(aes(fill = reorder(sec, -table(sec)[sec])), position = "dodge") +
  labs(title="Firm Count by Industry") +
  scale_fill_discrete(name="Sector") +
  theme(axis.title.y=element_blank(),axis.ticks.y=element_blank()) +
  coord_flip()

ggarrange(t1, t2, 
          labels = c("", ""), 
          ncol = 2, nrow = 1)

# summary: Institutional Ownership
ggplot(dt, aes(Institution.Ownership.Level..IO_Level., color = exch)) +
  geom_histogram(binwidth = 5, fill = "white", alpha = 0.5) +
  labs(title=paste0("Institutional Ownership (N=",nrow(dt),")"), y="Count", x="% Institutional Ownership") +
  scale_fill_discrete(name="Exchange") +
  theme(axis.ticks.x=element_blank())

#################################################### INDUSTRY MAPPING ################################################################

# Sector & Industry code
GICS_sector = data.table(Code = seq(10,60,5),
                         Sector = c('Energy','Materials','Industrials','Consumer Discretionary',
                                    'Consumer Staples','Health Care','Financials','Information Technology',
                                    'Communication Services','Utilities','Real Estate'))
GICS_ind = data.table(Code = c('1010','1510','2010','2020','2030','2510','2520','2530','2550','3010','3020','3030','3510','3520',
                               '4010','4020','4030','4510','4520','4530','5010','5020','5510','6010'),
                      Industry_Group = c('Energy','Materials','Capital Goods','Commercial & Professional Services',
                                   'Transportation','Automobiles & Components','Consumers Durables & Apparel',
                                   'Consumer Services','Retailing','Food & Staples Retailing','Food, Beverage & Tobacco',
                                   'Household & Personal Products','Health Care Equipment & Services',
                                   'Pharmaceuticals, Biotechnology & Life Sciences','Banks','Diversified Financials',
                                   'Insurance','Software & Services','Technology Hardware & Equipment',
                                   'Semiconductors & Semiconductor Equipment','Communication Services',
                                   'Media & Entertainment','Utilities','Real Estate'))
#write.xlsx(GICS_ind, 'GICS_ind.xlsx')

# NAICS_ind = data.table(Code = c(11,21,22,23,31,42,44,48,51,52,53,54,55,56,61,62,71,72,81,92),
#                        Industry = c('Agriculture, Forestry, Fishing and Hunting',
#                                     'Mining','Utilities','Construction','Manufacturing',
#                                     'Wholesale Trade','Retail Trade','Transportation and Warehousing','Information',
#                                     'Finance and Insurance','Real Estate Rental and Leasing',
#                                     'Professional, Scientific, and Technical Services',
#                                     'Management of Companies and Enterprises',
#                                     'Administrative and Support and Waste Management and Remediation Services',
#                                     'Educational Services','Health Care and Social Assistance',
#                                     'Arts, Entertainment, and Recreation',
#                                     'Accommodation and Food Services','Other Services (except Public Administration)',
#                                     'Public Administration'))
# 
# 
# SIC_ind = data.table(Code = c('01-09','10-14','15-17','20-39','40-49','50-51','52-59','60-67','70-89','90-99'),
#                      Industry = c('Agriculture, Forestry, And Fishing','Mining','Construction','Manufacturing',
#                                   'Transportation, Communications, Electric, Gas, And Sanitary Services',
#                                   'Wholesale Trade','Retail Trade','Finance, Insurance, And Real Estate',
#                                   'Services','Public Administration'))

IO_ind = data.table(IO_Code = c('11','21','22','23','31G','42','44RT','48TW','51','FIRE','PROF','6','7','81','G'),
                    IO_Industry = c('Agriculture, forestry, fishing, and hunting',
                                 'Mining','Utilities','Construction','Manufacturing',
                                 'Wholesale trade','Retail trade','Transportation and warehousing',
                                 'Information','Finance, insurance, real estate, rental, and leasing',
                                 'Professional and business services',
                                 'Educational services, health care, and social assistance',
                                 'Arts, entertainment, recreation, accommodation, and food services',
                                 'Other services, except government','Government'),
                    NAICS_Code = c('11','21','22','23','31-33','42','44,45','48,492,493','51','52,53',
                                   '54,55,56','61,62','71,72','81','491'),
                    GICS_Code = c('3020,1510','1010,1510','5510','2520,2010',
                                  '3020,2520,1510,2020,1010,3520,3030,2510,2010,4520,4530,3510',
                                  '2550,3510,3010,4520','2550,3010','2030,1010,2020',
                                  '2540,4510,5010','4010,4020,4030,4040,2550,2010',
                                  '2530,2020,3520,2530,2010,4020',
                                  '2530,3510','2530','2530','2030'))
require(openxlsx)
write.xlsx(IO_ind, 'IO_ind.xlsx')

io = as.data.table(read.csv('sector-level.csv', stringsAsFactors = F, skip = 5))
io_coef = as.matrix(io[1:15,3:17])
coef_top2 = apply(io_coef, 1, quantile, probs = 0.8) #top2 by row (find top2 of strength with customer)
require(hash)
link_code = hash()
link_coef = hash()
for (i in 1:15) {
  customerlist = coeflist = list()
  for (j in 1:15) {
    if (io_coef[i,j]>coef_top2[i] && io_coef[i,j]!=max(io_coef[i,])) {
      customerlist = append(customerlist, io$IOCode[j])
      coeflist = append(coeflist, round(io_coef[i,j],4))
    }
  }
  link_code[[io$IOCode[i]]] = customerlist
  link_coef[[io$IOCode[i]]] = coeflist
}















