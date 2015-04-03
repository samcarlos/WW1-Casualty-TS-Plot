
library(XML)
library(RCurl)
library(plotrix)

url="http://en.wikipedia.org/wiki/List_of_military_engagements_of_World_War_I"

doc = htmlTreeParse(url, useInternalNodes = T)
text = xpathSApply(doc, "//*/h2", xmlValue) 
src = xpathApply(doc, "//a[@href]", xmlGetAttr, "href")

links=unlist(src)

all.urls=paste("http://en.wikipedia.org",links,sep="")
all.urls=unique(all.urls)

next.to.vars=c("Date", "Location","Result")
below.vars=c("Belligerents", "Commanders and leaders", "Strength", "Casualties and losses")


meta.comp.data=data.frame()
meta.battle.data=rep("",5)
names(meta.comp.data)=c(next.to.vars,"site")
all.urls=all.urls[-grep("redlink", all.urls)]
all.urls=all.urls[-grep("firstworldwar.com",all.urls)]
all.urls=all.urls[-grep("org//",all.urls)]
all.urls=all.urls[-grep("orghttp",all.urls)]
all.urls=all.urls[-grep("UserLogin",all.urls)]


additional.urls1=vector()
for(i in 1:length(all.urls)){
  
  doc = htmlTreeParse(all.urls[i], useInternalNodes = T)
  text = xpathSApply(doc, "//*/h2", xmlValue) 
  src = xpathApply(doc, "//a[@href]", xmlGetAttr, "href")
  
  links=unlist(src)
  
  additional.urls=paste("http://en.wikipedia.org",links,sep="")
  additional.urls=unique(additional.urls)
  additional.urls=additional.urls[-grep("redlink", additional.urls)]
  additional.urls=additional.urls[-grep("firstworldwar.com",additional.urls)]
  additional.urls=additional.urls[-grep("org//",additional.urls)]
  additional.urls=additional.urls[-grep("orghttp",additional.urls)]
  additional.urls=additional.urls[-grep("UserLogin",additional.urls)]
  additional.urls1=unique(c(additional.urls1,additional.urls))
}




all.urls=additional.urls1

one.3.dimension.mat=function(x){dim(x)[2]==1 && dim(x)[1]==3}


for(q in 1:length(all.urls)){
  tables= readHTMLTable(all.urls[q])
  
  
  if(length(tables)!=0){  
    which.table=0
    for(m in 1:length(tables)){
      if(  sum(tables[[m]][,1] == "Casualties and losses")>0){
        which.table=m
        break
      }
    }
    
    
    
    temp.table=tables[[m]]  
    battle.data=vector(length=length(next.to.vars)+1)
    names(battle.data)=c(next.to.vars,"Site")
    for(i in 1:length(next.to.vars)){
      if(length(c(as.character(temp.table[which(temp.table[,1]==next.to.vars[i]),2])))==0){break}
      battle.data[i]= c(as.character(temp.table[which(temp.table[,1]==next.to.vars[i]),2]))
    }
    hiearchy.tables=which(lapply(tables,one.3.dimension.mat)==TRUE)
    hiearchy.data=""
    if(length(hiearchy.tables)>0){
      for(n in 1:length(hiearchy.tables)){
        temp.hiearchy=pasteCols(tables[[hiearchy.tables[n]]],sep="___")
        hiearchy.data=paste(hiearchy.data,temp.hiearchy, sep="***")
      }
    }
    
    battle.data[length(next.to.vars)+1]=as.character(all.urls[q])
    battle.data[length(next.to.vars)+2]=as.character(hiearchy.data)
    names(battle.data)=c(next.to.vars,"site","hiearchy.data")
    
    comp.data=matrix("",2,length(below.vars))
    for(i in 1:length(below.vars)){
      if(length(temp.table[which(temp.table[,1]==below.vars[i])+1,1])==0){break}
      comp.data[,i]=c(as.character(temp.table[which(temp.table[,1]==below.vars[i])+1,1]),as.character(temp.table[which(temp.table[,1]==below.vars[i])+1,2]))      
    }
    
    
    comp.data=cbind(comp.data,rep(all.urls[q],2))
    colnames(comp.data)=c(below.vars,"site")
    
    meta.comp.data=rbind(meta.comp.data,comp.data)
    meta.battle.data=rbind(meta.battle.data,battle.data)
    
  }
  
}

save(meta.comp.data, file="/Users/sweiss/Google Drive/wiki'd battles/metahier_12_7_14.rdata")
save(meta.battle.data, file="/Users/sweiss/Google Drive/wiki'd battles/battlehier_12_7_14.rdata")

