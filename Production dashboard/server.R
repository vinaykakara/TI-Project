#Attaching required libraries
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
library(bubbles)
library(shinySignals)
library(xlsx)
library(pool)

#ip address of sql server
ip<-"127.0.0.1"
#database name
database="mydb"
#username of sql table
username="tiproject"
#password of sql table
password="tiproject"

  shinyServer(function(input,output){
    
    #Used to create excel file for saving planned production  if it is not present  
    if(!file.exists("data.xlsx")){
      date<-Sys.Date()
      month<-as.numeric(substr(date,6,7))
      year<-as.numeric(substr(date,1,4))
      
      #used to find number of days in a month
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
      
      #used to create the planned production data for that particular month
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
      #Used to save the created planned production data in excel file
      write.xlsx(a,"data.xlsx",col.names = TRUE,row.names = FALSE)
    }
    
    #Used to append the new month planned data to the last month planned data
    if(file.exists("data.xlsx")){
      #used to read the last month planned data
      a<-read.xlsx("data.xlsx",sheetIndex = 1)
      a$NA.<-NULL
      date<-Sys.Date()
      month<-as.numeric(substr(date,6,7))
      year<-as.numeric(substr(date,1,4))
      #used to find number of days in that month
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
      
      #used to create the planned production data for that particular month
      for(i in 1:end){
        if(i<10)
          da=paste(substr(date,1,8),0,i,sep = "")
        if(i>=10)
          da=paste(substr(date,1,8),i,sep = "")
        
        yz=0
        for(j in 1:nrow(a))
          if(a$Date[j]==da)
            yz=1
        if(yz==0){
          dw<-data.frame(Date=da,Guide=0,PAWL=0)
          #Used to append the present month planned data to present month
          a<-rbind(a,dw)
        }
      }
      #used to save the planned data in excel file
      write.xlsx(a,"data.xlsx")
    }
    #To read SQL table 
    mydb <- dbPool(
      RMySQL::MySQL(), 
      dbname = database,
      host = ip,
      username = username,
      password = password
    )
    Gsort<-dbReadTable(mydb,"newguide") 
    Psort<-dbReadTable(mydb,"newpawl")
    poolClose(mydb)
    #Used to change the column names of the SQL tale
    names(Gsort)[names(Gsort) == 'TIME.STAMP'] <- 'TIME_STAMP'
    names(Psort)[names(Psort) == 'TIME.STAMP'] <- 'TIME_STAMP'
    names(Gsort)[names(Gsort) == 'GRADE.1'] <- 'GRADE_1'
    names(Psort)[names(Psort) == 'GRADE.1'] <- 'GRADE_1'
    names(Gsort)[names(Gsort) == 'GRADE.2'] <- 'GRADE_2'
    names(Psort)[names(Psort) == 'GRADE.2'] <- 'GRADE_2'
    
    names(Gsort)[names(Gsort) == 'TIME STAMP'] <- 'TIME_STAMP'
    names(Psort)[names(Psort) == 'TIME STAMP'] <- 'TIME_STAMP'
    names(Gsort)[names(Gsort) == 'GRADE 1'] <- 'GRADE_1'
    names(Psort)[names(Psort) == 'GRADE 1'] <- 'GRADE_1'
    names(Gsort)[names(Gsort) == 'GRADE 2'] <- 'GRADE_2'
    names(Psort)[names(Psort) == 'GRADE 2'] <- 'GRADE_2'
    
    #Used to filter the data of the particular month
    Gsort<-filter(Gsort,substr(Gsort$TIME_STAMP,7,10)==toString(substr(Sys.Date(),1,4)))
    Gsort<-filter(Gsort,substr(Gsort$TIME_STAMP,1,2)==toString(substr(Sys.Date(),6,7)))
    
    Psort<-filter(Psort,substr(Psort$TIME_STAMP,7,10)==toString(substr(Sys.Date(),1,4)))
    Psort<-filter(Psort,substr(Psort$TIME_STAMP,1,2)==toString(substr(Sys.Date(),6,7)))
    
    
    for(i in 1:nrow(Gsort)){
      
      #used to Create a column of date
      Gsort$Date[i]<-substr(Gsort$TIME_STAMP[i],1,10)
      na<-Gsort$Date[i]
      Gsort$Date[i]<-paste(substr(na,7,10),substr(na,1,2),substr(na,4,5),sep = "-")
      #Used to create a column(GD3) for combining both grades
      g1<-(Gsort$GRADE_1[i])
      g2<-(Gsort$GRADE_2[i])
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
      #Used to create a column for date
      Psort$Date[i]<-substr(Psort$TIME_STAMP[i],1,10)
      na<-Psort$Date[i]
      Psort$Date[i]<-paste(substr(na,7,10),substr(na,1,2),substr(na,4,5),sep = "-")
      if(Psort$GRADE_1[i]=="NA")
        Psort$GD3[i]<-"R"
      else
        Psort$GD3[i]<-Psort$GRADE_1[i]
    }
    
    #Used to count number of grades 
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
      if((input$guidego)){
        print(input$guidego[1]==1)
        p<-as.numeric(substr(input$guidedate1,9,10))
        q<-as.numeric(substr(input$guidedate2,9,10))
        if(p>q)
          st<-"Choose proper Date Range"
        else
          st<-"Production Planned Values Changed"
        ye1<-as.numeric(substr(input$guidedate1,1,4))
        ye2<-as.numeric(substr(input$guidedate2,1,4))
        ye<-as.numeric(substr(Sys.Date(),1,4))
        if((ye!=ye1)||(ye!=ye2))
          st<-"Date range must be this month"
        mn1<-as.numeric(substr(input$guidedate1,6,7))
        mn2<-as.numeric(substr(input$guidedate2,6,7))
        mn<-as.numeric(substr(Sys.Date(),6,7))
        if((mn!=mn1)||(mn!=mn2))
          st<-"Date range must be this month"
      }
      st
    })
    
    output$guidetable <-renderDataTable({
      r<-input$guiderefresh
      a<-read.xlsx("data.xlsx",sheetIndex = 1)
      a<-filter(a,substr(a$Date,1,7)==substr(Sys.Date(),1,7))
      a$NA.<-NULL
      a$PAWL<-NULL
      a
    })
    
    observeEvent(input$guidedatesave,{
      if(input$guidedate2>=input$guidedate1)
        showModal(modalDialog(
          title = "Enter production planned values for date",
          textOutput("textdate"),
          numericInput("adate"," ",0),
          actionButton("godate","change"),
          easyClose = FALSE
        ))
      
      if(input$guidedate2<input$guidedate1)
        showModal(modalDialog(
          title = "Enter correct dates",
          easyClose =TRUE
        ))
      m1<-as.numeric(substr(input$guidedate1,6,7))
      m2<-as.numeric(substr(input$guidedate2,6,7))
      m<-as.numeric(substr(Sys.Date(),6,7))
      y1<-as.numeric(substr(input$guidedate1,1,4))
      y2<-as.numeric(substr(input$guidedate2,1,4))
      y<-as.numeric(substr(Sys.Date(),1,4))
      if((y!=y1)||(y!=y2)||(m!=m1)||(m!=m2))
        showModal(modalDialog(
          title = "Enter this month dates only",
          easyClose =TRUE
        ))
    })
    
    output$textdate<-renderText({
      if(as.numeric(input$guidedate2-input$guidedate1)>=input$godate)
        toString(input$guidedate1+input$godate)
      else
        toString("Completed")
    })
    
    observeEvent(input$godate, {
      if(as.numeric(input$guidedate2-input$guidedate1)+1>=input$godate){
        p<-read.xlsx("data.xlsx",sheetIndex = 1)
        p$NA.<-NULL
        for(i in 1:nrow(p))
          if(toString(input$guidedate1+input$godate-1)==p$Date[i])
            p$Guide[i]=input$adate
        write.xlsx(p,"data.xlsx")
      }
      
    })
    
    observeEvent(input$guidego, {
     
      p<-as.numeric(substr(input$guidedate1,9,10))
      q<-as.numeric(substr(input$guidedate2,9,10))
      t1<-0
      ye1<-as.numeric(substr(input$guidedate1,1,4))
      ye2<-as.numeric(substr(input$guidedate2,1,4))
      ye<-as.numeric(substr(Sys.Date(),1,4))
      if((ye!=ye1)||(ye!=ye2))
        t1<-1
      mn1<-as.numeric(substr(input$guidedate1,6,7))
      mn2<-as.numeric(substr(input$guidedate2,6,7))
      mn<-as.numeric(substr(Sys.Date(),6,7))
      if((mn!=mn1)||(mn!=mn2))
        t1<-1
      
      if((p<=q)&&(t1==0))
      showModal(modalDialog(
        title = "Enter production planned values for the date range",
        textOutput("textdate1"),
        numericInput("adate1"," ",0),
        actionButton("godate1","change"),
        easyClose = FALSE
      ))
      else if((p>q)&&(t1==0))
        showModal(modalDialog(
          title = "Enter correct date range",
          easyClose = TRUE
        )) 
       else if(t1==1)
         showModal(modalDialog(
           title = "Enter this month dates",
           easyClose = TRUE
         )) 
    })
    
    output$textdate1<-renderText({
      if(input$godate1==0)
        "Enter the values"
      else
        "Changed"
    })
    
    observeEvent(input$godate1, {
      a<-read.xlsx("data.xlsx",sheetIndex = 1)
      a<-filter(a,substr(a$Date,1,7)==substr(Sys.Date(),1,7))
      a$NA.<-NULL
      p<-as.numeric(substr(input$guidedate1,9,10))
      q<-as.numeric(substr(input$guidedate2,9,10))
      t1<-0
      ye1<-as.numeric(substr(input$guidedate1,1,4))
      ye2<-as.numeric(substr(input$guidedate2,1,4))
      ye<-as.numeric(substr(Sys.Date(),1,4))
      if((ye!=ye1)||(ye!=ye2))
        t1<-1
      mn1<-as.numeric(substr(input$guidedate1,6,7))
      mn2<-as.numeric(substr(input$guidedate2,6,7))
      mn<-as.numeric(substr(Sys.Date(),6,7))
      if((mn!=mn1)||(mn!=mn2))
        t1<-1
      if(input$godate1>0)
        if((p<q)&&(t1==0))
          for(z in p:q){
            a$Guide[z]=input$adate1
          }
      if((p==q)&&(t1==0))
        a$Guide[p]=input$adate1
      write.xlsx(a,"data.xlsx")
      
    })
    
    #pawl data saving
    observeEvent(input$pawldatesave,{
      if(input$pawldate2>=input$pawldate1)
        showModal(modalDialog(
          title = "Enter production planned values for date",
          textOutput("textdate2"),
          numericInput("adate2"," ",0),
          actionButton("godate2","change"),
          easyClose = FALSE
        ))
      
      if(input$pawldate2<input$pawldate1)
        showModal(modalDialog(
          title = "Enter correct dates",
          easyClose =TRUE
        ))
      m1<-as.numeric(substr(input$pawldate1,6,7))
      m2<-as.numeric(substr(input$pawldate2,6,7))
      m<-as.numeric(substr(Sys.Date(),6,7))
      y1<-as.numeric(substr(input$pawldate1,1,4))
      y2<-as.numeric(substr(input$pawldate2,1,4))
      y<-as.numeric(substr(Sys.Date(),1,4))
      if((y!=y1)||(y!=y2)||(m!=m1)||(m!=m2))
        showModal(modalDialog(
          title = "Enter this month dates only",
          easyClose =TRUE
        ))
    })
    
    output$textdate2<-renderText({
      if(as.numeric(input$pawldate2-input$pawldate1)>=input$godate2)
        toString(input$pawldate1+input$godate2)
      else
        toString("Completed")
    })
    
    observeEvent(input$godate2, {
      if(as.numeric(input$pawldate2-input$pawldate1)+1>=input$godate2){
        p<-read.xlsx("data.xlsx",sheetIndex = 1)
        p$NA.<-NULL
        for(i in 1:nrow(p))
          if(toString(input$pawldate1+input$godate2-1)==p$Date[i])
            p$PAWL[i]=input$adate2
        write.xlsx(p,"data.xlsx")
      }
      
    })
    
    observeEvent(input$pawlgo, {
      
      p<-as.numeric(substr(input$pawldate1,9,10))
      q<-as.numeric(substr(input$pawldate2,9,10))
      t1<-0
      ye1<-as.numeric(substr(input$pawldate1,1,4))
      ye2<-as.numeric(substr(input$pawldate2,1,4))
      ye<-as.numeric(substr(Sys.Date(),1,4))
      if((ye!=ye1)||(ye!=ye2))
        t1<-1
      mn1<-as.numeric(substr(input$pawldate1,6,7))
      mn2<-as.numeric(substr(input$pawldate2,6,7))
      mn<-as.numeric(substr(Sys.Date(),6,7))
      if((mn!=mn1)||(mn!=mn2))
        t1<-1
      
      if((p<=q)&&(t1==0))
        showModal(modalDialog(
          title = "Enter production planned values for the date range",
          textOutput("textdate3"),
          numericInput("adate3"," ",0),
          actionButton("godate3","change"),
          easyClose = FALSE
        ))
      else if((p>q)&&(t1==0))
        showModal(modalDialog(
          title = "Enter correct date range",
          easyClose = TRUE
        )) 
      else if(t1==1)
        showModal(modalDialog(
          title = "Enter this month dates",
          easyClose = TRUE
        )) 
    })
    
    output$textdate3<-renderText({
      if(input$godate3==0)
        "Enter the values"
      else
        "Changed"
    })
    
    observeEvent(input$godate3, {
      a<-read.xlsx("data.xlsx",sheetIndex = 1)
      a<-filter(a,substr(a$Date,1,7)==substr(Sys.Date(),1,7))
      a$NA.<-NULL
      p<-as.numeric(substr(input$pawldate1,9,10))
      q<-as.numeric(substr(input$pawldate2,9,10))
      t1<-0
      ye1<-as.numeric(substr(input$pawldate1,1,4))
      ye2<-as.numeric(substr(input$pawldate2,1,4))
      ye<-as.numeric(substr(Sys.Date(),1,4))
      if((ye!=ye1)||(ye!=ye2))
        t1<-1
      mn1<-as.numeric(substr(input$pawldate1,6,7))
      mn2<-as.numeric(substr(input$pawldate2,6,7))
      mn<-as.numeric(substr(Sys.Date(),6,7))
      if((mn!=mn1)||(mn!=mn2))
        t1<-1
      if(input$godate3)
        if((p<q)&&(t1==0))
          for(z in p:q){
            a$PAWL[z]=input$adate3
          }
      if((p==q)&&(t1==0))
        a$PAWL[p]=input$adate3
      write.xlsx(a,"data.xlsx")
      
    })
    
    
    
    output$pawltable<- renderDataTable({
      r<-input$pawlrefresh
      a<-read.xlsx("data.xlsx",sheetIndex = 1)
      a<-filter(a,substr(a$Date,1,7)==substr(Sys.Date(),1,7))
      a$NA.<-NULL
      a$Guide<-NULL
      a
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
      a<-filter(a,substr(a$Date,1,7)==substr(Sys.Date(),1,7))
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
      a<-filter(a,substr(a$Date,1,7)==substr(Sys.Date(),1,7))
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
      a<-filter(a,substr(a$Date,1,7)==substr(Sys.Date(),1,7))
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
      a<-filter(a,substr(a$Date,1,7)==substr(Sys.Date(),1,7))
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
