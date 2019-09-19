#Created by Xueyi Xing, Jan, 2019
#Save this file with other datasets in the same folder
#Set working directory
#CAUTION: SOURCE following five lines first, DO NOT use RUN!!!
#clean memories
rm(list=ls())
mydir <-dirname(parent.frame(2)$ofile)
setwd(mydir)
stop( "You already changed your working directory. Please disregard the error message and continue to run the rest codes. :)")
########################################


#read raw data extracted using outpatient event label filter (colonoscopy/upper GI endoscopy) from the portal
uoel<-read.csv("EndoscopyOEL110718newwithageid.csv", stringsAsFactors=F)
#n=55586, this number may vary depending on the time of data retrival
#install following libraries first if they have not been installed

require(dplyr)
library(plyr)
library(sqldf)

#data preparation
uall2<-uoel
#delete $ sign, covert NAs to 0s in numerical columns.
for (i in 12:23) {
uall2[,i]<-as.numeric(gsub('[$,]','',uall2[,i]))
}
for (i in 12:23) {
  uall2[is.na(uall2[,i]),i]<-0
}

#*check provider names
for (i in c(4,5,7)) {
  uall2[,i]<-toupper(uall2[,i])
}
#convert . to _ in column names.
oldnames<-names(uall2)
newnames<-gsub("[.]","_",oldnames)
names(uall2)<-newnames

#sql, get a big picture of distribution of providers, total-allowed-amount, etc.
s<-sqldf("select distinct Provider_Name_Claim, Provider_City,sum(Allowed_Amount) as total_allowed_amount, count(Claim_ID) as total_claims,
count(Person_ID) as total_persons from uall2 group by Provider_Name_Claim, Provider_City")
#get the average allowed-amount for each provider.
s$avg_allowed_amount<-s$total_allowed_amount/s$total_claims

#check provider name claim
#see how many records which had different Servicing.Provider.Name and Provider.Name.Claim, suggest to run all following # lines if it is the first time to explore the data.
#ualldiff<-uall2[uall2$Servicing.Provider.Name!=uall2$Provider.Name.Claim,]
#ualldiff2<-count(ualldiff,c("Provider.Name.Claim","Servicing.Provider.Name","Provider.City"))


#provider name cleanning
#ualldiff3<-count(uall2,c("Provider.Name.Claim","Provider.City"))
#ualldiff3<-ualldiff3[order(-ualldiff3$freq),]
ualldiff4<-s[order(-s$total_allowed_amount),]

#write.csv(ualldiff4, 'All_Providers.csv')

#uall2a<-count(uall2,c("Servicing.Provider.Name","Servicing.Provider.NPI.ID"))
#uall2a<-uall2a[order(-uall2a$freq),]

#standardize provider names

for (i in c(4,5,7)) {
  uall2[,i]<-gsub('GEISINGER GRAYS WOODS OUTPATIE','GEISINGER GRAYS WOODS OUTPATI',uall2[,i])
  uall2[,i]<-gsub('BRANDON M. CRAFT','BRANDON MICHAEL CRAFT',uall2[,i])
  uall2[,i]<-gsub('DUSTIN G CASE','DUSTIN GRAIG CASE',uall2[,i])
  uall2[,i]<-gsub('MOUNT NITTANY PHYSICIAN GROUP','MOUNT NITTANY MEDICAL CENTER',uall2[,i])
  
  #uall2[,i]<-gsub('MOUNT NITTANY MEDICAL CENTER H','MOUNT NITTANY MEDICAL CENTER',uall2[,i])
  uall2[,i]<-gsub('MILTON S. HERSHEY MEDICAL CENT','MILTON S HERSHEY MEDICAL CENTE',uall2[,i])
  uall2[,i]<-gsub('NICHOLAS A INVERSO','NICHOLAS ANTHONY INVERSO',uall2[,i])
  uall2[,i]<-gsub('[.|,|/|-|~]','',uall2[,i])
}
#ualldiff3<-count(uall2,c("Provider.Name.Claim","Provider.City"))
#ualldiff3<-ualldiff3[order(-ualldiff3$freq),]
#write.csv(ualldiff3, 'ualldiff3.csv')

for (i in 1:nrow(uall2)) {
  if (uall2$Provider_City[i]=="STATE COLLEGE" & grepl('HERSHEY ENDOSCOPY', uall2$Provider_Name_Claim[i])) {
    uall2$Provider_Name_Claim[i] <- 'PENN STATE HERSHEY ENDOSCOPY STATE COLLEGE'
  }
  else {uall2$Provider_Name_Claim[i] <-uall2$Provider_Name_Claim[i]}

}

for (i in 1:nrow(uall2)) {
  if (uall2$Provider_City[i]=="HERSHEY" & grepl('HERSHEY ENDOSCOPY', uall2$Provider_Name_Claim[i])) {
    uall2$Provider_Name_Claim[i] <- 'HERSHEY ENDOSCOPY, HERSHEY'
  }
  else {uall2$Provider_Name_Claim[i] <-uall2$Provider_Name_Claim[i]}
  
}


#see how many persons with the same service date but different locations had claims in both datasets (Geisinger, Danville/Port Matilda), n=1124
geisinger<-uall2[grepl("GEISINGER", uall2$Provider_Name_Claim),]
geisinger<-sqldf("select distinct * from geisinger") #n=10758
geisingerd<-geisinger[geisinger$Provider_City=="DANVILLE",] #The claims having Danville as provider location, n=7370
geisingerdd<-geisingerd[!duplicated(geisingerd[c("Person_ID","Service_Date_MMDDYYYY")]),] #deduplicate on PersonID and ServiceDate, n=1345
geisingerp<-geisinger[geisinger$Provider_City=="PORT MATILDA",]#The claims having PM as provider location, n=2881
geisingerpd<-geisingerp[!duplicated(geisingerp[c("Person_ID","Service_Date_MMDDYYYY")]),] #deduplicate on PersonID and ServiceDate, n=1252
geisingerboth<-sqldf("select * from geisingerdd a, geisingerpd b where 
                     a.Person_ID=b.Person_ID and a.Service_Date_MMDDYYYY=b.Service_Date_MMDDYYYY")

#it seems they were almost completely overlapped, so change the provider names to a unified name

for (i in 1:nrow(uall2)) {
  if (uall2$Provider_City[i]=="DANVILLE" & grepl('GEISINGER', uall2$Provider_Name_Claim[i])) {
    uall2$Provider_Name_Claim[i] <- 'GEISINGER GRAYS WOODS OUTPATI'
  }
  else {uall2$Provider_Name_Claim[i] <-uall2$Provider_Name_Claim[i]}
  
}


for (i in 1:nrow(uall2)) {
  if (uall2$Provider_City[i]=="PORT MATILDA") {
    uall2$Provider_Name_Claim[i] <- 'GEISINGER GRAYS WOODS OUTPATI'
  }
  else {uall2$Provider_Name_Claim[i] <-uall2$Provider_Name_Claim[i]}
  
}
#Dr. Case is affilated with MNMC
for (i in 1:nrow(uall2)) {
  if (uall2$Provider_City[i]=="STATE COLLEGE" & grepl('DUSTIN GRAIG CASE', uall2$Provider_Name_Claim[i])) {
    uall2$Provider_Name_Claim[i] <- 'MOUNT NITTANY MEDICAL CENTER'
  }
  else {uall2$Provider_Name_Claim[i] <-uall2$Provider_Name_Claim[i]}
  
}
#similar to Geisinger claims, MNMC, Hermitage and MNMC, State College had large overlaps.
#see how many persons with the same service date but different locations had 
#claims in both datasets (Mount Nittany Medical Center, State College/Hermitage), n=628
mnmc<-uall2[grepl("MOUNT NITTANY MEDICAL CENTER", uall2$Provider_Name_Claim),]
mnmc<-sqldf("select distinct * from mnmc") #n=14414
mnmcd<-mnmc[mnmc$Provider_City=="HERMITAGE",] #The claims having Danville as provider location, n=706
mnmcdd<-mnmcd[!duplicated(mnmcd[c("Person_ID","Service_Date_MMDDYYYY")]),] #deduplicate on PersonID and ServiceDate, n=629
mnmcp<-mnmc[mnmc$Provider_City=="STATE COLLEGE",]#The claims having PM as provider location, n=13708
mnmcpd<-mnmcp[!duplicated(mnmcp[c("Person_ID","Service_Date_MMDDYYYY")]),] #deduplicate on PersonID and ServiceDate, n=1130
mnmcboth<-sqldf("select * from mnmcdd a, mnmcpd b where 
                     a.Person_ID=b.Person_ID and a.Service_Date_MMDDYYYY = b.Service_Date_MMDDYYYY")

for (i in 1:nrow(uall2)) {
  if (grepl('MOUNT NITTANY MEDICAL CENTER H', uall2$Provider_Name_Claim[i])) {
    uall2$Provider_Name_Claim[i] <- 'MOUNT NITTANY MEDICAL CENTER'
  }
  else {uall2$Provider_Name_Claim[i] <-uall2$Provider_Name_Claim[i]}
  
}

#check again

s<-sqldf("select distinct Provider_Name_Claim, Provider_City, sum(Allowed_Amount) as total_allowed_amount, count(Claim_ID) as total_claims,
count(Person_ID) as total_persons from uall2 group by Provider_Name_Claim")
s$avg_allowed_amount<-s$total_allowed_amount/s$total_claims
#check provider name claim

ualldiff4<-s[order(-s$total_allowed_amount),]
#write.csv(uall2, file='allclaims16_18.csv')

#pick individual providers located in State College
s1<-sqldf("select * from ualldiff4 where Provider_City='STATE COLLEGE'")

#link to record-level data
#drop top 2 entries
s2<-s1[-c(1,2),]
s3<-sqldf("select a.* from uall2 a, s2 b where a.Provider_Name_Claim=b.Provider_Name_Claim")
s4<-count(s3, c("Provider_ID_and_Name","Provider_Name_Claim","Provider_City"))
#change all provider who had a record of mnmc to mount nittany. It's fine in endoscopy project, but be cautious of this change since MNMC did a lot examnations referred by other providers in State College. 
s5<-s4[grepl('MOUNT NITTANY MEDICAL CENTER',s4$Provider_ID_and_Name),]

for (i in 1:nrow(uall2)) {
  if (grepl('MOUNT NITTANY MEDICAL CENTER', uall2$Provider_ID_and_Name[i]) & uall2$Provider_Name_Claim[i] %in% s5$Provider_Name_Claim) {
    uall2$Provider_Name_Claim[i] <- 'MOUNT NITTANY MEDICAL CENTER'
  }
  else {uall2$Provider_Name_Claim[i] <-uall2$Provider_Name_Claim[i]}
  
}


uall2$Provider_Name_Claim<-gsub('PENN STATE HERSHEY MEDICAL GRO','PENN STATE HERSHEY ENDOSCOPY STATE COLLEGE',uall2$Provider_Name_Claim)
#get provider stats again
s<-sqldf("select distinct Provider_Name_Claim, Provider_City, sum(Allowed_Amount) as total_allowed_amount, count(distinct Claim_ID) as total_claims,
count(distinct Person_ID) as total_persons from uall2 group by Provider_Name_Claim")
s$avg_allowed_amount<-s$total_allowed_amount/s$total_persons

ualldiff4<-s[order(-s$total_allowed_amount),]
statecollege<-ualldiff4[1:3,]

#Determine an event. Alternatively, you can directly use outpatient event id to group claims of an event. Following codes were developped before I am awared of this.

uall2$date<-as.Date(uall2$Service_Date_MMDDYYYY, "%m/%d/%Y")
uall2$year<-format(as.Date(uall2$date, "%m/%d/%Y"),"%Y")
uall2$month<-format(as.Date(uall2$date, "%m/%d/%Y"),"%m")
uall2$day<-format(as.Date(uall2$date, "%m/%d/%Y"),"%d")
uall2<-uall2[order(uall2$Person_ID, uall2$date),]
#write.csv(uall2, file='allclaims16_18.csv')

library(data.table)
DT <- data.table(uall2)
#person id
setDT(DT)[, pnum := rleid(Person_ID)]
setDT(DT)[, datnumbyperson := rleid(date), by= Person_ID]
#count within person by date
setDT(DT)[, cntperperson := uniqueN(date), by = Person_ID]
setDT(DT)[, eventnum := rleid(c('Person_ID','date'))]
#sequntial index by date and person
DT[, iddateperson := seq_len(.N), by = c('date','Person_ID')]
#sequntial index by person
DT[, inperson := seq_len(.N), by = c('Person_ID')]
DT$datnumbyperson1<-as.character(DT$datnumbyperson)
#eventnum is the unique index of an event
DT$eventnum<-paste(DT$Person_ID,DT$datnumbyperson1,sep='_')
which(DT$iddateperson!=DT$inperson)
#create "myear" to make plotting easier
DT$myear<-paste(DT$year,DT$month,sep='') 

################End of determining an event ############

#limit the population to age50-52 group
DTcolon<-DT[DT$Outpatient_Event_Label=='Colonoscopy',]
DTcolon50<-DT[DT$Age_In_Years>=50 & DT$Age_In_Years<=52,]

#identify claims from other providers for three main SC providers 

#penn state hershey claims
pshclaims<-DTcolon50[DTcolon50$Provider_Name_Claim=='PENN STATE HERSHEY ENDOSCOPY STATE COLLEGE',]
#Geisinger claims
geiclaims<-DTcolon50[DTcolon50$Provider_Name_Claim=='GEISINGER GRAYS WOODS OUTPATI',]
#Mount Nittany claims
mntclaims<-DTcolon50[DTcolon50$Provider_Name_Claim=='MOUNT NITTANY MEDICAL CENTER',]

#claims from other providers, change the names accordingly
otherclaims<-DTcolon50[DTcolon50$Provider_Name_Claim!='PENN STATE HERSHEY ENDOSCOPY STATE COLLEGE',]
otherclaimsg<-DTcolon50[DTcolon50$Provider_Name_Claim!='GEISINGER GRAYS WOODS OUTPATI',]

#pd1, the ids existing in the provider of interest
perdate1<-sqldf('select * from pshclaims group by Person_ID, date')
perdate1g<-sqldf('select * from geiclaims group by Person_ID, date')

#pd2, the ids existing in otherclaims
perdate2<-sqldf('select * from otherclaims group by Person_ID, date')
perdate2g<-sqldf('select * from otherclaimsg group by Person_ID, date')

#pd4, common ids and dates existing in both files
perdate4<-sqldf('select a.Person_ID, b.date from perdate1 a, perdate2 b where a.Person_ID=b.Person_ID and a.date=b.date')
perdate4g<-sqldf('select a.Person_ID, b.date from perdate1g a, perdate2g b where a.Person_ID=b.Person_ID and a.date=b.date')
#perdatetest<-sqldf('select a.Person_ID, b.date from perdate1 a, perdate2 b where a.eventnum=b.eventnum')
#identical(perdate4, perdatetest) #true

#pd5, records with common ids and dates in otherclaims
perdate5<-sqldf('select a.* from otherclaims a, perdate4 b where a.Person_ID=b.Person_ID and a.date=b.date')
perdate5g<-sqldf('select a.* from otherclaimsg a, perdate4g b where a.Person_ID=b.Person_ID and a.date=b.date')

#pd6, combined list
countperdate<-count(perdate5, c('Provider_Name_Claim','eventnum'))
countperdateg<-count(perdate5g, c('Provider_Name_Claim','eventnum'))
perdate6<-rbind(pshclaims, perdate5)
perdate6g<-rbind(geiclaims, perdate5g)

#sort
perdate6<-perdate6[order(perdate6$Person_ID, perdate6$date),]
perdate6g<-perdate6g[order(perdate6g$Person_ID, perdate6g$date),]
write.csv(perdate6g,'geisinger_provider_claim.csv',row.names=F)
write.csv(perdate6,'psh_provider_claim.csv',row.names=F)

#get the records having mnmc as the provider
perdate6gm<-perdate5g[perdate5g$Provider_Name_Claim=='MOUNT NITTANY MEDICAL CENTER',]
perdate6pm<-perdate5[perdate5$Provider_Name_Claim=='MOUNT NITTANY MEDICAL CENTER',]
#write.csv(perdate6gm,'multiple_provider_claim_mn.csv',row.names=F)

#get ids for these records
mntgeiids<-sqldf('select Person_ID, date from perdate6gm group by Person_ID, date')
mntpshids<-sqldf('select Person_ID, date from perdate6pm group by Person_ID, date')

#delete these persons with combo providers from the raw DT50 dataset to reduce confusion

DTcolon50s<-anti_join(DTcolon50, mntpshids, by=c('Person_ID','date'))
DTcolon50s<-anti_join(DTcolon50, mntgeiids, by=c('Person_ID','date'))

#change provider names
DTcolon50s$newprovider<-NULL
for (i in 1:nrow(DTcolon50s)){
  ifelse(DTcolon50s$Person_ID[i] %in% perdate4$Person_ID, DTcolon50s$newprovider[i]<-'PENN STATE HERSHEY ENDOSCOPY STATE COLLEGE',DTcolon50s$newprovider[i]<-DTcolon50s$Provider_Name_Claim[i])
  ifelse(DTcolon50s$Person_ID[i] %in% perdate4g$Person_ID, DTcolon50s$newprovider[i]<-'GEISINGER GRAYS WOODS OUTPATI',DTcolon50s$newprovider[i])
  }
#get counts
pdc<-count(perdate5, c("Provider_Name_Claim","Provider_City","Procedure_w_Code"))
pdc<-pdc[order(-pdc$freq),]
write.csv(pdc,'pdc.csv',row.names=F)
##########################

#get stats by year

cost16<-sqldf("select distinct newprovider, year as Year, sum(Allowed_Amount) as total_allowed_amount, 
              sum(Out_of_Pocket) as total_cost_member, sum(Net_Payment) as total_cost_employer, count(distinct Claim_ID) as total_claims,
              count(distinct Person_ID) as total_persons, count(distinct eventnum) as Events from DTcolon50s where year='2016' and 
              newprovider in (select Provider_Name_Claim from statecollege)  
              group by newprovider")

cost16$allowed_amount_per_day<-cost16$total_allowed_amount/cost16$Events
cost16$allowed_amount_per_claim<-cost16$total_allowed_amount/cost16$total_claims
cost16$membercost_per_day<-cost16$total_cost_member/cost16$Events
cost16$membercost_per_claim<-cost16$total_cost_member/cost16$total_claims
cost16$employercost_per_day<-cost16$total_cost_employer/cost16$Events
cost16$employercost_per_claim<-cost16$total_cost_employer/cost16$total_claims
#check provider name claim
#cost16<-cost16[order(-cost16$total_allowed_amount),]


cost17<-sqldf("select distinct newprovider, year as Year, sum(Allowed_Amount) as total_allowed_amount, 
              sum(Out_of_Pocket) as total_cost_member, sum(Net_Payment) as total_cost_employer, count(distinct Claim_ID) as total_claims,
              count(distinct Person_ID) as total_persons, count(distinct eventnum) as Events from DTcolon50s where year='2017' and 
              newprovider in (select Provider_Name_Claim from statecollege)  
              group by newprovider ")

cost17$allowed_amount_per_day<-cost17$total_allowed_amount/cost17$Events
cost17$allowed_amount_per_claim<-cost17$total_allowed_amount/cost17$total_claims
cost17$membercost_per_day<-cost17$total_cost_member/cost17$Events
cost17$membercost_per_claim<-cost17$total_cost_member/cost17$total_claims
cost17$employercost_per_day<-cost17$total_cost_employer/cost17$Events
cost17$employercost_per_claim<-cost17$total_cost_employer/cost17$total_claims
#check provider name claim
#cost17<-cost17[order(-cost17$total_allowed_amount),]


cost18<-sqldf("select distinct newprovider, year as Year, sum(Allowed_Amount) as total_allowed_amount, 
              sum(Out_of_Pocket) as total_cost_member, sum(Net_Payment) as total_cost_employer, count(distinct Claim_ID) as total_claims,
              count(distinct Person_ID) as total_persons, count(distinct eventnum) as Events from DTcolon50s where year='2018' and 
              newprovider in (select Provider_Name_Claim from statecollege)  
              group by newprovider ")

cost18$allowed_amount_per_day<-cost18$total_allowed_amount/cost18$Events
cost18$allowed_amount_per_claim<-cost18$total_allowed_amount/cost18$total_claims
cost18$membercost_per_day<-cost18$total_cost_member/cost18$Events
cost18$membercost_per_claim<-cost18$total_cost_member/cost18$total_claims
cost18$employercost_per_day<-cost18$total_cost_employer/cost18$Events
cost18$employercost_per_claim<-cost18$total_cost_employer/cost18$total_claims
#check provider name claim
#cost18<-cost18[order(-cost18$total_allowed_amount),]

costall<-rbind(cost16,cost17,cost18)
#plotting
library(ggplot2)
j1<-ggplot() + geom_line(data=costall, aes(Year, allowed_amount_per_day, colour = newprovider, group = newprovider), 
                         size=1,linetype="solid")+ theme_light()
j1<-j1+geom_point(data=costall, aes(Year, allowed_amount_per_day, group = newprovider, colour = newprovider),size=1, shape = 2)+ 
  geom_text(data=costall,aes(Year, allowed_amount_per_day,label=floor(allowed_amount_per_day)),hjust=0, vjust=0)+
  labs(title = "Allowed amount per event, dropping combo-provider cases",
       subtitle = "01/2016-09/2018, Colonscopy, Age 50-52",
       x = "Year", y = "Cost") 
print(j1)
#count for each provider
j2<-ggplot() + geom_line(data=costall, aes(Year, Events, colour = newprovider, group = newprovider), 
                         size=1,linetype="solid")+ theme_light()
j2<-j2+geom_point(data=costall, aes(Year, Events, group = newprovider, colour = newprovider),
                  size=1, shape = 2)+ geom_text(data=costall,aes(Year, Events,label=Events),hjust=0, vjust=0)+
  labs(title = "Total counts of colonoscopy events, aged 50-52",
       subtitle = "01/2016-09/2018",
       x = "Year", y = "Number") 
print(j2)
library(ggpubr)
ggarrange(j1, j2,
          ncol = 2, nrow = 1)
#write.csv(costall, file='costall.csv')

#merge other two facilities to mask detailed information for them
costall2<-costall
for (i in 1:nrow(costall2)) {
  if(grepl('GEISINGER GRAYS WOODS OUTPATI',costall2$newprovider[i])) {costall2$newprovider[i]<-'Other_Providers'} 
  else {costall2$newprovider[i]<-costall2$newprovider[i]}
  ifelse(grepl('MOUNT NITTANY MEDICAL CENTER',costall2$newprovider[i]), costall2$newprovider[i]<-'Other_Providers',costall2$newprovider[i]<-costall2$newprovider[i])

}
costall3<-sqldf("select *, sum(total_allowed_amount) as Total_amount_c, sum(Events) as Event_c  from costall2   
              group by newprovider, year ")
costall3$Allowed_amount_per_event_c<-costall3$Total_amount_c/costall3$Event_c


j1<-ggplot() + geom_line(data=costall3, aes(Year, Allowed_amount_per_event_c, colour = newprovider, group = newprovider), 
                         size=1,linetype="solid")+ theme_light()
j1<-j1+geom_point(data=costall3, aes(Year, Allowed_amount_per_event_c, group = newprovider, colour = newprovider),
                  size=1, shape = 2)+ geom_text(data=costall3,aes(Year, Allowed_amount_per_event_c,label=floor(Allowed_amount_per_event_c)),hjust=0, vjust=0)+
  labs(title = "Allowed amount per event, Colonoscopy, Age 50-52",
       subtitle = "01/2016-09/2018",
       x = "Year", y = "Cost") 
print(j1)

#count for merged providers
costall4<-sqldf("select *, sum(Events) as Total_event_c  from costall2 group by newprovider, year ")
j1<-ggplot() + geom_line(data=costall4, aes(Year, Total_event_c, colour = newprovider, group = newprovider), 
                         size=1,linetype="solid")+ theme_light()
j1<-j1+geom_point(data=costall4, aes(Year, Total_event_c, group = newprovider, colour = newprovider),
                  size=1, shape = 2)+ geom_text(data=costall4,aes(Year, Total_event_c,label=Total_event_c),hjust=0, vjust=0)+
  labs(title = "Total counts of colonoscopy events, aged 50-52",
       subtitle = "01/2016-09/2018",
       x = "Year", y = "Number") 
print(j1)
#######
#monthly count
cost16month<-sqldf("select distinct Provider_Name_Claim, year, myear as Month, Outpatient_Event_Label, count(distinct Claim_ID) as total_claims,
              count(distinct Person_ID) as total_persons, count(distinct eventnum) as Events from DT where year='2016' 
              and Provider_Name_Claim in (select Provider_Name_Claim from statecollege)  
              group by Provider_Name_Claim, Outpatient_Event_Label, myear ")
cost17month<-sqldf("select distinct Provider_Name_Claim, year, myear as Month, Outpatient_Event_Label, count(distinct Claim_ID) as total_claims,
              count(distinct Person_ID) as total_persons, count(distinct eventnum) as Events from DT where year='2017' 
              and Provider_Name_Claim in (select Provider_Name_Claim from statecollege)  
              group by Provider_Name_Claim, Outpatient_Event_Label, myear ")
cost18month<-sqldf("select distinct Provider_Name_Claim, year, myear as Month, Outpatient_Event_Label, count(distinct Claim_ID) as total_claims,
              count(distinct Person_ID) as total_persons, count(distinct eventnum) as Events from DT where year='2018' 
                   and Provider_Name_Claim in (select Provider_Name_Claim from statecollege)  
                   group by Provider_Name_Claim, Outpatient_Event_Label, myear ")

costallmonth<-rbind(cost16month,cost17month,cost18month)
for (i in 1:nrow(costall)) {
    if (grepl('2018',costall$Year[i])) {costall$Cbym[i]<-costall$Events[i]/9}
    else {costall$Cbym[i]<-costall$Events[i]/12}
}
#ifelse(grepl('2018',costall$Year),costall$Cbym<-costall$Events/9,costall$Cbym<-costall$Events/12)
write.csv(costallmonth, file='costallmonth.csv')


##########################################
#the list of persons having endoscopies within 2 days
DT1<-DT[DT$cntperperson>=2]
DT2<-sqldf('select *, max(date)-min(date) as datediff from DT1 group by Person_ID')
DT3<-DT2[!(DT2$cntperperson=2 & DT2$datediff>1),]
test<-DT1[DT1$Person_ID %in% DT3$Person_ID,]
write.csv(test,file='Claims for the same persons within 2 days a.csv')

#do the same thing for aged 50-52 colonoscopy population
DT1<-DTcolon50[DTcolon50$cntperperson>=2,]
length(unique(DT1$Person_ID))
DT2<-sqldf('select *, max(date)-min(date) as datediff from DT1 group by Person_ID')
DT3<-DT2[(DT2$cntperperson>=2 & DT2$datediff>1& DT2$datediff<=3),]
test<-DT1[DT1$Person_ID %in% DT3$Person_ID,]
test<-test[order( test$Person_ID, test$date, test$Provider_Name_Claim),]
testcount<-count(test,c('eventnum','Provider_Name_Claim'))
write.csv(test,file='Claims for the same persons within 2 days b.csv')          

