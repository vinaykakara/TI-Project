library(xlsx)
library(plyr)
library(pool)
library(dplyr)
library(DT)
library(pool)
library(dplyr)

mydb <- dbPool(
  RMySQL::MySQL(), 
  dbname = "mydb",
  host = "127.0.0.1",
  username = "tiproject",
  password = "tiproject"
)

b<-dbReadTable(mydb,"limits")
poolClose(mydb)

l<-read.csv("Shift.txt",sep="-",header = TRUE)
readpawl<-function(){
  invalidateLater(60000,session=NULL)
  mydb <- dbPool(
    RMySQL::MySQL(), 
    dbname = "mydb",
    host = "127.0.0.1",
    username = "tiproject",
    password = "tiproject"
  )
   ab<-dbReadTable(mydb,"newpawl")
 poolClose(mydb) 
 da<-Sys.Date()
 de<-paste(substr(da,6,7),substr(da,9,10),substr(da,1,4),sep="-")
 ab<-filter(ab,substr(ab$TIME_STAMP,1,10)==de)
 names(ab)[names(ab) == 'GRADE_1'] <- 'GD1'
 names(ab)[names(ab) == 'GRADE_2'] <- 'GD2'
 names(ab)[names(ab) == 'TIME_STAMP'] <- 'Date1'
 names(ab)[names(ab) == 'WIDTH_1'] <- 'WD1'
 names(ab)[names(ab) == 'WIDTH_2'] <- 'WD2'
  ab
}

readguide<-function(){
  invalidateLater(60000,session=NULL)
  mydb <- dbPool(
    RMySQL::MySQL(), 
    dbname = "mydb",
    host = "127.0.0.1",
    username = "tiproject",
    password = "tiproject"
  )
  a<-dbReadTable(mydb,"newguide")
  da<-Sys.Date()
  de<-paste(substr(da,6,7),substr(da,9,10),substr(da,1,4),sep="-")
  a<-filter(a,substr(a$TIME_STAMP,1,10)==de)
  names(a)[names(a) == 'GRADE_1'] <- 'GD1'
  names(a)[names(a) == 'GRADE_2'] <- 'GD2'
  names(a)[names(a) == 'TIME_STAMP'] <- 'Date1'
  names(a)[names(a) == 'WIDTH_1'] <- 'WD1'
  names(a)[names(a) == 'WIDTH_2'] <- 'WD2'
  
  poolClose(mydb)
  a
}

xy=1;

  Gsort<-reactive({
    a<-readguide()
    for(i in 1:nrow(a)){
      if(substr(a$Date1[i],21,22)=="AM"){
      ti=as.numeric(substr(a$Date1[i],12,13))
      t=substr(a$Date1[i],12,19)
      }
      if(substr(a$Date1[i],21,22)=="PM"){
        ti=as.numeric(substr(a$Date1[i],12,13))+12
      t=paste(ti,substr(a$Date1[i],14,19),sep="")
        }
      xt=paste(toString(ti),'-',toString(ti+1),sep='')
      
      yt='p'
      xy1=a$GD1[i]
      xy2=a$GD2[i]
      if((xy1=='NA')&(xy2=='NA'))
        yt='R'
      
      if((xy1=='NA')&(xy2!='NA'))
        yt=paste(xy2,'R',sep='')
      
      if((xy1!='NA')&(xy2=='NA'))
        yt=paste(xy1,'R',sep='')
      
      if((xy1!='NA')&(xy2!='NA'))
        yt=paste(xy1,xy2,sep='')
      
      
      ne=data.frame(a[i:i,1:ncol(a)],Hour=xt,GD3=yt,Date=substr(a$Date1[1],1,10),Time=t)
      if(ne$GD1[1]=='NA')
        ne$GD1[1]='R'
      if(ne$GD2[1]=='NA')
        ne$GD2[1]='R'

      if(i==1){
        Guide<-ne
      }
      else{
        Guide<-rbind.fill(Guide,ne)}
    }
    Guide$Date1<-NULL
    
    Guide
  })
  
  
  Psort<-reactive({
    ab<-readpawl()
    for(i in 1:nrow(ab)){
      if(substr(ab$Date1[i],21,22)=="AM"){
        ti=as.numeric(substr(ab$Date1[i],12,13))
        t=substr(ab$Date1[i],12,19)
      }
      if(substr(ab$Date1[i],21,22)=="PM"){
        ti=as.numeric(substr(ab$Date1[i],12,13))+12
        t=paste(ti,substr(ab$Date1[i],14,19),sep="")
      }
      xt=paste(toString(ti),'-',toString(ti+1),sep='')
      if(ab$GD1[i]=='NA')
        zx='R'
      else
        zx=ab$GD1[i]
      r1=data.frame(ab[i:i,1:ncol(ab)],GD3=zx,Hour=xt,Date=substr(ab$Date1[1],1,10),Time=t)
      
      
      if(i==1)
        Pawl<-r1
      if(i>1)
        Pawl<-rbind.fill(Pawl,r1)
      Pawl$ar[i]<-ti
    }
    Pawl<-Pawl[order(Pawl$ar),]
  
    Pawl$Date1<-NULL
    Pawl
    
  })
  
  

Total<-reactive({
  rbind.fill(Gsort(),Psort())
})

ggd3aa<-reactive({
  cou=0
  for(i in 1:nrow(Gsort())){
    if(Gsort()$GD3[i]=="AA")
      cou=cou+1;
  }
  cou
})
ggd3ab<-reactive({
  cou=0
  for(i in 1:nrow(Gsort())){
    if(Gsort()$GD3[i]=="AB")
      cou=cou+1;
    if(Gsort()$GD3[i]=="BA")
      cou=cou+1;
  }
  cou
})
ggd3ca<-reactive({
  cou=0
  for(i in 1:nrow(Gsort())){
    if(Gsort()$GD3[i]=="CA")
      cou=cou+1;
    if(Gsort()$GD3[i]=="AC")
      cou=cou+1;
  }
  cou
})
ggd3ar<-reactive({
  cou=0
  for(i in 1:nrow(Gsort())){
    if(Gsort()$GD3[i]=="AR")
      cou=cou+1;
    if(Gsort()$GD3[i]=="RA")
      cou=cou+1;
  }
  cou
})
ggd3bb<-reactive({
  cou=0
  for(i in 1:nrow(Gsort())){
    if(Gsort()$GD3[i]=="BB")
      cou=cou+1;
  }
  cou
})
ggd3bc<-reactive({
  cou=0
  for(i in 1:nrow(Gsort())){
    if(Gsort()$GD3[i]=="BC")
      cou=cou+1;
    if(Gsort()$GD3[i]=="CB")
      cou=cou+1;
  }
  cou
})
ggd3br<-reactive({
  cou=0
  for(i in 1:nrow(Gsort())){
    if(Gsort()$GD3[i]=="BR")
      cou=cou+1;
    if(Gsort()$GD3[i]=="RB")
      cou=cou+1;
  }
  cou
})
ggd3cc<-reactive({
  cou=0
  for(i in 1:nrow(Gsort())){
    if(Gsort()$GD3[i]=="CC")
      cou=cou+1;
  }
  cou
})
ggd3cr<-reactive({
  cou=0
  for(i in 1:nrow(Gsort())){
    if(Gsort()$GD3[i]=="CR")
      cou=cou+1;
    if(Gsort()$GD3[i]=="RC")
      cou=cou+1;
  }
  cou
})
ggd3r<-reactive({
  cou=0
  for(i in 1:nrow(Gsort())){
    if(Gsort()$GD3[i]=="R")
      cou=cou+1;
  }
  cou
})


ggd1a<-reactive({
  cou=0
  for(i in 1:nrow(Gsort())){
    if(Gsort()$GD1[i]=="A")
      cou=cou+1;
  }
  cou
})
ggd1b<-reactive({
  cou=0
  for(i in 1:nrow(Gsort())){
    if(Gsort()$GD1[i]=="B")
      cou=cou+1;
  }
  cou
})
ggd1c<-reactive({
  cou=0
  for(i in 1:nrow(Gsort())){
    if(Gsort()$GD1[i]=="C")
      cou=cou+1;
  }
  cou
})
ggd1r<-reactive({
  cou=0
  for(i in 1:nrow(Gsort())){
    if(Gsort()$GD1[i]=="R")
      cou=cou+1;
  }
  cou
})


ggd2a<-reactive({
  cou=0
  for(i in 1:nrow(Gsort())){
    if(Gsort()$GD2[i]=="A")
      cou=cou+1;
  }
  cou
})
ggd2b<-reactive({
  cou=0
  for(i in 1:nrow(Gsort())){
    if(Gsort()$GD2[i]=="B")
      cou=cou+1;
  }
  cou
})
ggd2c<-reactive({
  cou=0
  for(i in 1:nrow(Gsort())){
    if(Gsort()$GD2[i]=="C")
      cou=cou+1;
  }
  cou
})
ggd2r<-reactive({
  cou=0
  for(i in 1:nrow(Gsort())){
    if(Gsort()$GD2[i]=="R")
      cou=cou+1;
  }
  cou
})

guidep<-reactive({
  cou=0
  for(i in 1:nrow(Gsort())){
    if(Gsort()$GD3[i]!="R")
      cou=cou+1;
  }
  cou
})
guider<-reactive({
  cou=0
  for(i in 1:nrow(Gsort())){
    if(Gsort()$GD3[i]=="R")
      cou=cou+1;
  }
  cou
})
pgd3a<-reactive({
  cou=0
  for(i in 1:nrow(Psort())){
    if(Psort()$GD3[i]=="A")
      cou=cou+1;
  }
  cou
})
pgd3b<-reactive({
  cou=0
  for(i in 1:nrow(Psort())){
    if(Psort()$GD3[i]=="B")
      cou=cou+1;
  }
  cou
})
pgd3c<-reactive({
  cou=0
  for(i in 1:nrow(Psort())){
    if(Psort()$GD3[i]=="C")
      cou=cou+1;
  }
  cou
})
pgd3r<-reactive({
  cou=0
  for(i in 1:nrow(Psort())){
    if(Psort()$GD3[i]=="R")
      cou=cou+1;
  }
  cou
})


pawlp<-reactive({
  cou=0
  for(i in 1:nrow(Psort())){
    if(Psort()$GD3[i]!="R")
      cou=cou+1;
  }
  cou
})
pawlr<-reactive({
  cou=0
  for(i in 1:nrow(Psort())){
    if(Psort()$GD3[i]=="R")
      cou=cou+1;
  }
  cou
})
totalr<-reactive({
  cou=0
  for(i in 1:nrow(Total())){
    if(Total()$GD3[i]=="R")
      cou=cou+1;
  }
  cou
})

totalp<-reactive({
  cou=0
  for(i in 1:nrow(Total())){
    if(Total()$GD3[i]!="R")
      cou=cou+1;
  }
  cou
})


subdata <- reactive({
 
  Gsort() %>%
    filter(
      hms(Gsort()$Time)>=hms(l$start[1]),
      hms(Gsort()$Time)<=hms(l$end[1])
    )
})
subdata1 <- reactive({

  Gsort() %>%
    filter(
      hms(Gsort()$Time)>=hms(l$start[2]),
      hms(Gsort()$Time)<=hms(l$end[2])
    )
})
subdata2 <- reactive({
 
  Gsort() %>%
    filter(
      hms(Gsort()$Time)>=hms(l$start[3]),
      hms(Gsort()$Time)<=hms(l$end[3])
    )
})
