# westernfront=read.csv("c:/users/sam/Google Drive/wiki'd battles/western_front_major_battles.csv")
# easternfront=read.csv("c:/users/sam/Google Drive/wiki'd battles/eastern_front_major_battles.csv")
# italianfront=read.csv("c:/users/sam/Google Drive/wiki'd battles/italian_front_major_battles.csv")
# noneuro=read.csv("c:/users/sam/Google Drive/wiki'd battles/noneurofronts.csv")
# Front=c(rep("western",nrow(westernfront)), rep("eastern", nrow(easternfront)),rep("italian", nrow(italianfront)))
# euro=cbind(rbind(westernfront,easternfront,italianfront),Front)
# colnames(noneuro)=colnames(euro)
# 
# 
# allfronts=rbind(euro,noneuro)
# 
# 
# allfronts$Front=as.character(allfronts$Front)
# allfronts$Front[which(allfronts$Front %in% c("Macedonian","Serbian"))]="Balkans"
# allfronts$Front[which(allfronts$Front =="eastern")]="Eastern"
# allfronts$Front[which(allfronts$Front =="italian")]="Italian"
# allfronts$Front[which(allfronts$Front =="western")]="Western"
# allfronts$casulties.numbers=unlist(apply(cbind(allfronts$new.casult,allfronts$casulties.numbers),1 ,function(x) max(x,na.rm=TRUE)))
# allfronts$casulties.numbers[which(allfronts$casulties.numbers==-Inf)]=NA
# 
# save(allfronts, file="c:/users/sam/Google Drive/wiki'd battles/allfronts_battles.csv")

load("c:/users/sam/Google Drive/wiki'd battles/allfronts_battles.csv")
time.days=as.Date(allfronts$end.date, "%m/%d/%y")-as.Date(allfronts$begin.date, "%m/%d/%y")
time.days[which(time.days<0)]=1

library(zoo)
dates=seq.Date(as.Date("1870-1-1"), to=as.Date("1950-1-1"), by="day")

allfronts$casulties.rate=allfronts$casulties.numbers/(as.numeric(time.days)+1)


battle.rate.mat=matrix(0,length(dates), length(allfronts$begin.date))
time.days=as.numeric(time.days)
allfronts$begin.date=as.Date(allfronts$begin.date, "%m/%d/%y")
allfronts$begin.date=unlist(lapply(allfronts$begin.date, function(x) seq(x, length=2, by="-100 years")[2]))
allfronts$end.date=as.Date(allfronts$end.date, "%m/%d/%y")
allfronts$end.date=unlist(lapply(allfronts$end.date, function(x) seq(x, length=2, by="-100 years")[2]))
allfronts$begin.date=as.Date(allfronts$begin.date)
allfronts$end.date=as.Date(allfronts$end.date)


for(i in 1:dim(allfronts)[1]){
  battle.rate.mat[which(dates==(allfronts$begin.date)[i]):(which(dates==allfronts$begin.date[i]+time.days[i])),i]=allfronts$casulties.rate[i]
}

agg.battle.rate=aggregate(battle.rate.mat,by=list(as.yearmon(dates)),FUN=sum)
agg.battle.rate[is.na(agg.battle.rate)]=0
battle.rate.mat.ww1=agg.battle.rate[which(agg.battle.rate[,1]=="Aug 1914"):which(agg.battle.rate[,1]=="Dec 1918"),-1]

battle.rate.mat.ww1.fronts.countries=aggregate(t(battle.rate.mat.ww1), by=list(allfronts$Front,allfronts$ParsedCountry), FUN=sum)

colnames(battle.rate.mat.ww1.fronts.countries)[3:55]=agg.battle.rate[which(agg.battle.rate[,1]=="Aug 1914"):which(agg.battle.rate[,1]=="Dec 1918"),1]
colnames(battle.rate.mat.ww1.fronts.countries)[1:2]=c("Front", "Country")
library(reshape)
battle.rate.mat.ww1.fronts.countries.melt=melt(battle.rate.mat.ww1.fronts.countries, id=(c("Front", "Country")))
colnames(battle.rate.mat.ww1.fronts.countries.melt)[3:4]=c("Date","MonthlyCas")

battle.cumulative=melt(cbind(battle.rate.mat.ww1.fronts.countries[,c(1,2)],t(apply(battle.rate.mat.ww1.fronts.countries[,-c(1,2)],1,cumsum))),id=c("Front", "Country"))
colnames(battle.cumulative)[3:4]=c("Date","MonthlyCas")
library(ggplot2)
library(ggthemes)
allies=c("American","Belgium", "England","France","Italy","Portugal","Russian Empire","Serbia","Greece","Romania")
central=c("Germany","Austria-Hungary","Bulgaria","Ottoman Empire")
library(plyr)
is.allies=battle.rate.mat.ww1.fronts.countries.melt$Country %in% allies
battle.rate.mat.ww1.fronts.countries.melt=cbind(battle.rate.mat.ww1.fronts.countries.melt,is.allies)
battle.rate.mat.ww1.fronts.countries.melt$dates=as.Date(as.yearmon(as.numeric(as.character(battle.rate.mat.ww1.fronts.countries.melt$Date))))




ggplot(battle.rate.mat.ww1.fronts.countries.melt, aes(dates, MonthlyCas, group=Country))+geom_bar(subset=.(is.allies==TRUE),stat="identity",aes(fill=Country), position="stack")+ labs(list(title = "World War One Monthly Casualties by Fronts and Belligerents", x = "Date", y = "Monthly Casualties (Right Side are Allies and Left Side are Central Powers"))+geom_bar(subset=.(is.allies==FALSE),stat="identity",aes(y=MonthlyCas*-1,fill=Country), position="stack")+
  facet_grid(.~Front)+coord_flip() 

battle.cumulative$Date=as.Date(as.yearmon(as.numeric(as.character(battle.cumulative$Date))))
ggplot(battle.cumulative, aes((Date), MonthlyCas, group=Country))+geom_bar(stat="identity",aes(fill=Country), position="stack")+labs(list(title = "World War One Cumulative Casualties by Fronts", x = "Date", y = "Cumulative Casualties"))+facet_grid(.~Front)

