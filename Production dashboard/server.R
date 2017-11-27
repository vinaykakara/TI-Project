library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(lubridate)
library(shinythemes)
library(reshape)
library(dplyr)
library(tidyr)
library(xts)
library(shinyjs)
library(xlsx)
library(readxl)
library(DT)
library(pool)
library(dplyr)

shinyServer(function(input,output){
  
  
  if(!file.exists("data.xlsx")){
    date<-Sys.Date()
    month<-as.numeric(substr(date,6,7))
    year<-as.numeric(substr(date,1,4))
    if(month%%2!=0)
      end<-30
    if((month%%2==0)&&(month!=2))
      end<-31
    if((month%%2==0)&&(month==2)){
      if(year%%4==0)
        end<-29
      if(year%%4!=0)
        end<-28
    }
    for(i in 1:end){
      if(i<10)
        da=paste(substr(date,1,8),0,i,sep = "")
      if(i>=10)
        da=paste(substr(date,1,8),i,sep = "")
      
      if(i==1)
        a<-data.frame(Date=da,Guide=0,PAWL=0)
      if(i!=1){
        dw<-data.frame(Date=da,Guide=0,PAWL=0)
        a<-rbind(a,dw)
      }
    }
  
    write.xlsx(a,"data.xlsx",col.names = TRUE,row.names = FALSE)
  }
  
  
  if(file.exists("data.xlsx")){
    
    a<-read.xlsx("data.xlsx",sheetIndex = 1)
    a$NA.<-NULL
    date<-Sys.Date()
    month<-as.numeric(substr(date,6,7))
    year<-as.numeric(substr(date,1,4))
    if(month%%2!=0)
      end<-30
    if((month%%2==0)&&(month!=2))
      end<-31
    if((month%%2==0)&&(month==2)){
      if(year%%4==0)
        end<-29
      if(year%%4!=0)
        end<-28
    }
    for(i in 1:end){
      if(i<10)
        da=paste(substr(date,1,8),0,i,sep = "")
      if(i>=10)
        da=paste(substr(date,1,8),i,sep = "")
      yz<-0
      for(j in 1:nrow(a))
        if(de$Date[i]==da)
          yz<-1
        
        if(yz==0){
          dw<-data.frame(Date=da,Guide=0,PAWL=0)
          a<-rbind(a,dw)
          }
    }
    write.xlsx(a,"data.xlsx")
  }
  
  mydb <- dbPool(
    RMySQL::MySQL(), 
    dbname = "mydb",
    host = "127.0.0.1",
    username = "tiproject",
    password = "tiproject"
  )
  Gsort<-dbReadTable(mydb,"newguide") 
  Psort<-dbReadTable(mydb,"newpawl")
  poolClose(mydb)
  Gsort<-filter(Gsort,substr(Gsort$TIME_STAMP,7,10)==toString(substr(Sys.Date(),1,4)))
  Gsort<-filter(Gsort,substr(Gsort$TIME_STAMP,1,2)==toString(substr(Sys.Date(),6,7)))
  
  Psort<-filter(Psort,substr(Psort$TIME_STAMP,7,10)==toString(substr(Sys.Date(),1,4)))
  Psort<-filter(Psort,substr(Psort$TIME_STAMP,1,2)==toString(substr(Sys.Date(),6,7)))
  Gsort$Date=NULL
  for(i in 1:nrow(Gsort)){
    
   
     Gsort$Date[i]<-substr(Gsort$TIME_STAMP[i],1,10)
    na<-Gsort$Date[i]
    Gsort$Date[i]<-paste(substr(na,7,10),substr(na,1,2),substr(na,4,5),sep = "-")
    g1<-(Gsort$Grade_1[i])
    g2<-(Gsort$Grade_2[i])
    if((g1=="NA")&&(g2=="NA"))
      Gsort$GD3[i]<-"R"
    else if((g1!="NA")||(g2!="NA")){
      if(g1=="NA")
        Gsort$GD3[i]<-paste("R",g2,sep = "")
      else if(g2=="NA")
        Gsort$GD3[i]<-paste(g1,"R",sep = "")
      else
        Gsort$GD3[i]<-paste(g1,g2,sep = "")
    }
    
  }
  
  for(i in 1:nrow(Psort)){
    Psort$Date[i]<-substr(Psort$TIME_STAMP[i],1,10)
    na<-Psort$Date[i]
    Psort$Date[i]<-paste(substr(na,7,10),substr(na,1,2),substr(na,4,5),sep = "-")
    if(Psort$Grade_1[i]=="NA")
      Psort$GD3[i]<-"R"
    else
      Psort$GD3[i]<-Psort$Grade_1[i]
  }
 
  
  guidep=0
  pawlp=0
  totalp=0
  guider=0
  pawlr=0
  totalr=0
  for(i in 1:nrow(Gsort)){
   if(Gsort$GD3[i]=='R'){
     guider=guider+1
     totalr=totalr+1
   }
     else{
     guidep=guidep+1
     totalp=totalp+1
   }
     
  }
  for(i in 1:nrow(Psort)){
    if(Psort$GD3[i]=='R'){
      pawlr=pawlr+1
      totalr=totalr+1
    }
    else{
      pawlp=pawlp+1
      totalp=totalp+1
    }
     
  }
  output$table2 <- renderDataTable({
    Parts <- as.numeric(c(guidep,pawlp,totalp))
    Rej <- as.numeric(c(guider,pawlr,totalr))
    total <- as.numeric(c(guidep+guider,pawlp+pawlr,totalp+totalr))
    Grades =c(c("Guide Plate","PAWL","Total"),Parts,Rej,total)
    a= matrix(Grades,nrow= 3, ncol=4, dimnames=list(c("Guide Plate","PAWL","Total"),c("Part Name","Accepted","Rejected","Total")))
    a
  })
  
  output$guidetext1<-renderText({
    "Select Date range to Enter Production Planned"
  })
  
  output$guidetext2<-renderText({
    st<-"Enter Values of Production Planned"
    if(input$guidego){
      p<-as.numeric(substr(input$guidedate1,9,10))
      q<-as.numeric(substr(input$guidedate2,9,10))
      if(p>q)
        st<-"Choose proper Date Range"
      else
        st<-"Production Planned Values Changed"
    }
    st
  })
  
  output$guidetable = DT::renderDataTable({
    r<-input$guiderefresh
    a<-read.xlsx("data.xlsx",sheetIndex = 1)
    a$NA.<-NULL
    a$PAWL<-NULL
    a
  })
  
  observeEvent(input$guidego, {
    a<-read.xlsx("data.xlsx",sheetIndex = 1)
    a$NA.<-NULL
    p<-as.numeric(substr(input$guidedate1,9,10))
    q<-as.numeric(substr(input$guidedate2,9,10))
    if(p<q)
      for(z in p:q){
        a$Guide[z]=input$guideval
      }
    if(p==q)
      a$Guide[p]=input$guideval
    write.xlsx(a,"data.xlsx")
  })
  
  
  output$pawltext1<-renderText({
    " Select Date range to Enter Production Planned"
  })
  
  output$pawltext2<-renderText({
    st<-"Enter Values of Production Planned"
    if(input$pawlgo){
      p<-as.numeric(substr(input$pawldate1,9,10))
      q<-as.numeric(substr(input$pawldate2,9,10))
      if(p>q)
        st<-"Choose proper Date Range"
      else
        st<-"Production Planned Values Changed"
    }
    st
  })
  
  output$pawltable = DT::renderDataTable({
    r<-input$pawlrefresh
    a<-read.xlsx("data.xlsx",sheetIndex = 1)
    a$NA.<-NULL
    a$Guide<-NULL
    a
  })
  
  observeEvent(input$pawlgo, {
    a<-read.xlsx("data.xlsx",sheetIndex = 1)
    a$NA.<-NULL
    p<-as.numeric(substr(input$pawldate1,9,10))
    q<-as.numeric(substr(input$pawldate2,9,10))
    if(p<q)
      for(z in p:q){
        a$PAWL[z]=input$pawlval
      }
    if(p==q)
      a$PAWL[p]=input$pawlval
    write.xlsx(a,"data.xlsx")
  })
  guideproduced<-function(date){
    count=0
    for(i in 1:nrow(Gsort))
      if(date==Gsort$Date[i])
        if(Gsort$GD3[i]!='R')
        count=count+1
      count
  }
  output$table<-renderDataTable({
    xy<-input$guidedevrefresh
    a<-read.xlsx("data.xlsx",sheetIndex = 1)
    a$NA.<-NULL
    a$PAWL<-NULL
    for(i in 1:nrow(a)){
      x1<-guideproduced(a$Date[i])
      y1<-a$Guide[i]
      a$actual[i]<-x1
      if(y1>0)
      a$deviation[i]<-round((100*(x1-y1))/x1,digits = 3)
      else
        a$deviation[i]<-NA
    }
    a$Planned <- a$Guide
    a$Guide<-NULL
    a$Actual <- a$actual
    a$actual <- NULL
    a$Deviation <- a$deviation
    a$deviation <- NULL
    a
  })
  pawlproduced<-function(date){
    count=0
    for(i in 1:nrow(Psort))
      if(date==Psort$Date[i])
        if(Psort$GD3[i]!='R')
          count=count+1
        count
  }
  output$table1<-renderDataTable({
    xy<-input$pawldevrefresh
    a<-read.xlsx("data.xlsx",sheetIndex = 1)
    a$NA.<-NULL
    a$Guide<-NULL
    for(i in 1:nrow(a)){
      x1<-pawlproduced(a$Date[i])
      y1<-a$PAWL[i]
      a$actual[i]<-x1
      if(y1>0)
        a$deviation[i]<-round((100*(x1-y1))/x1,digits = 3)
      else
        a$deviation[i]<-NA
    }
    a$Planned <- a$PAWL
    a$PAWL<-NULL
    a$Actual <- a$actual
    a$actual <- NULL
    a$Deviation <- a$deviation
    a$deviation <- NULL
    a
  })
 
  
  guidedata1<-reactive({
    a<-read.xlsx("data.xlsx",sheetIndex = 1)
    for(i in 1:nrow(a)){
      x1<-guideproduced(a$Date[i])
      a$actual[i]<-x1
    }
    a
  })
  output$plot<-renderPlotly({
    xy<-input$guideplotrefresh
    Actual<-as.numeric(guidedata1()$actual)
    Day<- as.Date(guidedata1()$Date)
    Planned <- as.numeric(guidedata1()$Guide)
    x<- ggplotly(
      ggplot() + 
        geom_line(data = guidedata1(), aes(x =Day, y = Actual, colour = "Actual"))+geom_point()+
        geom_line(data = guidedata1(), aes(x =Day , y = Planned, colour = "Planned"))+
        scale_colour_manual("", 
                            breaks = c("Actual", "Planned"),
                            values = c("red", "blue"))
      +xlab("Month Days")+ylab("No.of Parts")+ggtitle("Planned Vs Actual")
    )
    x$elementId <- NULL
    x
  })

  pawldata1<-reactive({
    a<-read.xlsx("data.xlsx",sheetIndex = 1)
    for(i in 1:nrow(a)){
      x1<-pawlproduced(a$Date[i])
      a$actual[i]<-x1
    }
    a
  })
  output$plot2<-renderPlotly({
    xy<-input$pawlplotrefresh
        Actual<-as.numeric(pawldata1()$actual)
    Day<-as.Date(pawldata1()$Date)
    Planned <- as.numeric(pawldata1()$PAWL)
    x<- ggplotly(
      ggplot() + 
        geom_line(data = pawldata1(), aes(x =Day, y = Actual, colour = "Actual"))+geom_point()+
        geom_line(data = pawldata1(), aes(x =Day , y = Planned, colour = "Planned"))+
        scale_colour_manual("", 
                            breaks = c("Actual", "Planned"),
                            values = c("red", "blue"))
      +xlab("Month Days")+ylab("No.of Parts")+ggtitle("Planned Vs Actual")
    )
    x$elementId <- NULL
    x
  })
  
  
  output$partsdate<-renderText("enter date")
  guidedata<-function(date1){
   
    a<-read.xlsx("data.xlsx",sheetIndex = 1)
   da<-data.frame(Date=as.Date(a$Date),Planned=a$Guide)
   da
  }
  
  output$vbox<-renderValueBox({
     y<-guidedata()
   y1<-y$Planned[match(input$guideparts,y$Date)]
    x<-valueBox(
      h4("Planned"),
      y1,
      icon = icon("hourglass")
    )
    x
  })
  output$vbox1<-renderValueBox({
    r<-filter(Gsort,Date==toString(input$guideparts))
    r<-filter(r,GD3!="R")
    y1=nrow(r)
    x<-valueBox(
      h4("Actual"),
      y1,
      icon = icon("hourglass-2")
    )
    x
  })
  output$vbox2<-renderValueBox({
    r<-filter(Gsort,Date==toString(input$guideparts))
    r<-filter(r,GD3=="R")
    y1=nrow(r)
    x<-valueBox(
      h4("Rejected"),
      y1,
      icon = icon("thumbs-down")
    )
    x
  })
  
  output$partsdate1<-renderText("enter date")
  pawldata<-function(date1){
    
    a<-read.xlsx("data.xlsx",sheetIndex = 1)
    da<-data.frame(Date=as.Date(a$Date),Planned=a$PAWL)
    da
  }
  
  
  
  output$vbox3<-renderValueBox({
    y<-pawldata()
    y1<-y$Planned[match(input$pawlparts,y$Date)]
    x<-valueBox(
      h4("Planned"),
      y1,
      icon = icon("hourglass")
    )
    x
  })
  output$vbox4<-renderValueBox({
    r<-filter(Psort,Date==toString(input$pawlparts))
    r<-filter(r,GD3!="R")
    y1=nrow(r)
    x<-valueBox(
      h4("Actual"),
      y1,
      icon = icon("hourglass-2")
    )
    x
  })
  output$vbox5<-renderValueBox({
    r<-filter(Psort,Date==toString(input$pawlparts))
    r<-filter(r,GD3=="R")
    y1=nrow(r)
    x<-valueBox(
      h4("Rejected"),
      y1,
      icon = icon("thumbs-down")
    )
    x
  })
  output$ibox1<-renderInfoBox({
    r<-filter(Psort,Date==toString(input$pawlparts))
    r<-filter(r,GD3=="R")
    y1=nrow(r)
    r<-filter(Psort,Date==toString(input$pawlparts))
    r<-filter(r,GD3!="R")
    y2=nrow(r)
    x<-infoBox(
      h4("percentage of acceptance"),
      round(100*(y2-y1)/y2,digits = 3),
      icon = icon("thumbs-up")
    )
    x
  })
  output$ibox<-renderInfoBox({
    r<-filter(Gsort,Date==toString(input$guideparts))
    r<-filter(r,GD3=="R")
    y1=nrow(r)
    r<-filter(Gsort,Date==toString(input$guideparts))
    r<-filter(r,GD3!="R")
    y2=nrow(r)
    x<-infoBox(
      h4("percentage of acceptance"),
      round(100*(y2-y1)/y2,digits = 3),
      icon = icon("thumbs-up")
    )
    x
  })
})
