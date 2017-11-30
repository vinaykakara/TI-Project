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
library(pool)
library(dplyr)
#used to read data from sql table
shinyServer(function(input,output){
  mydb <- dbPool(
    RMySQL::MySQL(), 
    dbname = "mydb",
    host = "127.0.0.1",
    username = "tiproject",
    password = "tiproject"
  )
  Psort<-dbReadTable(mydb,"newpawl")
  Gsort<-dbReadTable(mydb,"newguide")
  poolClose(mydb) 
  #used to change column names of a table
  names(Gsort)[names(Gsort) == 'GRADE_1'] <- 'GD1'
  names(Gsort)[names(Gsort) == 'GRADE_2'] <- 'GD2'
  names(Gsort)[names(Gsort) == 'TIME_STAMP'] <- 'Date'
  names(Gsort)[names(Gsort) == 'WIDTH_1'] <- 'WD1'
  names(Gsort)[names(Gsort) == 'WIDTH_2'] <- 'WD2'
  
  names(Psort)[names(Psort) == 'GRADE_1'] <- 'GD1'
  names(Psort)[names(Psort) == 'GRADE_2'] <- 'GD2'
  names(Psort)[names(Psort) == 'TIME_STAMP'] <- 'Date'
  names(Psort)[names(Psort) == 'WIDTH_1'] <- 'WD1'
  names(Psort)[names(Psort) == 'WIDTH_2'] <- 'WD2'
  
  #used to count the grades
  ggd1a = 0;
  ggd1b = 0;
  ggd1c = 0;
  ggd1r = 0;
  ggd2a = 0;
  ggd2b = 0;
  ggd2c = 0;
  ggd2r = 0;
  ggd3aa = 0;
  ggd3ab = 0;
  ggd3ar = 0;
  ggd3bb = 0;
  ggd3bc = 0;
  ggd3br = 0;
  ggd3ca = 0;
  ggd3cc = 0;
  ggd3cr = 0;
  ggd3r = 0;
  pgd3a = 0;
  pgd3b = 0;
  pgd3c = 0;
  pgd3r = 0;
  for(i in 1:nrow(Gsort)){
    #used to convert the date formant
    da<-Gsort$Date[i]
    if(substr(da,21,22)=="AM")
      Gsort$Time[i]=substr(da,12,19)
    if(substr(da,21,22)=="PM")
      Gsort$Time[i]=paste(12+as.numeric(substr(da,12,13)),substr(da,14,19),sep="")
    
    Gsort$Date[i]<-paste(substr(da,7,10),substr(da,1,2),substr(da,4,5),sep="-")
    #used to add the combined grade
    yt='p'
    xy1=Gsort$GD1[i]
    xy2=Gsort$GD2[i]
    if((xy1=='NA')&(xy2=='NA'))
      yt='R'
    
    if((xy1=='NA')&(xy2!='NA'))
      yt=paste(xy2,'R',sep='')
    
    if((xy1!='NA')&(xy2=='NA'))
      yt=paste(xy1,'R',sep='')
    
    if((xy1!='NA')&(xy2!='NA'))
      yt=paste(xy1,xy2,sep='')
    
    Gsort$GD3[i]=yt
    
    
    if(Gsort$GD1[1]=='NA')
      Gsort$GD1[1]='R'
    if(Gsort$GD2[1]=='NA')
      Gsort$GD2[1]='R'
    
    if(Gsort$GD1[i]=='A')
      ggd1a=ggd1a+1;
    if(Gsort$GD1[i]=='B')
      ggd1b=ggd1b+1;
    if(Gsort$GD1[i]=='C')
      ggd1c=ggd1c+1;
    if(Gsort$GD1[i]=='R')
      ggd1r=ggd1r+1;
    
    
    if(Gsort$GD2[i]=='R')
      ggd2r=ggd2r+1;
    if(Gsort$GD2[i]=='A')
      ggd2a=ggd2a+1;
    if(Gsort$GD2[i]=='B')
      ggd2b=ggd2b+1;
    if(Gsort$GD2[i]=='C')
      ggd2c=ggd2c+1;
    
    
    if(Gsort$GD3[i]=='AA')
      ggd3aa=ggd3aa+1;
    if(Gsort$GD3[i]=='AB')
      ggd3ab=ggd3ab+1;
    if(Gsort$GD3[i]=='AR')
      ggd3ar=ggd3ar+1;
    if(Gsort$GD3[i]=='BB')
      ggd3bb=ggd3bb+1;
    if(Gsort$GD3[i]=='BC')
      ggd3bc=ggd3bc+1;
    if(Gsort$GD3[i]=='BR')
      ggd3br=ggd3br+1;
    if(Gsort$GD3[i]=='CA')
      ggd3ca=ggd3ca+1;
    if(Gsort$GD3[i]=='CC')
      ggd3cc=ggd3cc+1;
    if(Gsort$GD3[i]=='CR')
      ggd3cr=ggd3cr+1;
    if(Gsort$GD3[i]=='R')
      ggd3r=ggd3r+1;
  }
  
  for(i in 1:nrow(Psort)){
    #used to convert date format
    da<-Psort$Date[i]
    if(substr(da,21,22)=="AM")
      Psort$Time[i]=substr(da,12,19)
    if(substr(da,21,22)=="PM")
      Psort$Time[i]=paste(12+as.numeric(substr(da,12,13)),substr(da,14,19),sep="")
    Psort$Date[i]<-paste(substr(da,7,10),substr(da,1,2),substr(da,4,5),sep="-")
    if(Psort$GD1[i]=='NA')
      Psort$GD1[i]='R'
    Psort$GD3[i]=Psort$GD1[i]
    #used to count number of grades
    if(Psort$GD3[i]=='A')
      pgd3a=pgd3a+1;
    if(Psort$GD3[i]=='B')
      pgd3b=pgd3b+1;
    if(Psort$GD3[i]=='C')
      pgd3c=pgd3c+1;
    if(Psort$GD3[i]=='R')
      pgd3r=pgd3r+1;
    
  }
  Total<-rbind(Gsort,Psort)
  
  output$text1<- renderUI({
    startdate =min(Gsort$Date)
  })
  output$text2<- renderUI({
    enddate   =max(Gsort$Date)
  })
  output$text3<- renderUI({
    startdate =min(Psort$Date)
  })
  output$text4<- renderUI({
    enddate   =max(Psort$Date)
  })
  subdata1 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(Gsort$Date[1]),
        as.Date(Gsort$Date)<= as.Date(Gsort$Date[1])
      )
  })
  subdata2 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(Gsort$Date[1]),
        as.Date(Gsort$Date)<= as.Date(Gsort$Date[1])+7
      )
  })
  subdata3 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(Gsort$Date[1]),
        as.Date(Gsort$Date)<= as.Date(Gsort$Date[1])+30
      )
  })
  output$Barchart <- renderPlotly({
    xyz <- data.frame(
      Date <- c(Gsort$Date,Gsort$Date),
      Grade <- c(Gsort$GD1,Gsort$GD2)
    )
    x<- ggplotly(
      ggplot(xyz, mapping= aes(Date,fill=Grade))+
        geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
    )
    if(input$radio=="2"){
      x<- ggplotly(
        ggplot(xyz, mapping= aes(Date,fill=Grade))+
          geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
    }
    if(input$radio=="3"){
      x<- ggplotly(
        ggplot(xyz, mapping= aes(wday(Date,label = T),fill=Grade))+
          geom_bar(position = "stack")+xlab("Day")
      )
    }
    if(input$radio=="4"){
      x<- ggplotly(
        ggplot(xyz, mapping= aes(wday(Date,label = T),fill=Grade))+
          geom_bar(position = "dodge")+xlab("Day")
      ) 
    }
    if(input$radio1=="2"){
      abc <- data.frame(
       Date <- c(subdata1()$Date,subdata1()$Date),
       Grade <- c(subdata1()$GD1,subdata1()$GD2)
      )
      x<- ggplotly(
        ggplot(abc, mapping= aes(Date,fill=Grade))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
      if(input$radio=="2"){
        x<- ggplotly(
          ggplot(abc, mapping= aes(Date,fill=Grade))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$radio=="3"){
        x<- ggplotly(
          ggplot(abc, mapping= aes(wday(Date,label = T),fill=Grade))+
            geom_bar(position = "stack")+xlab("Day")
        )
      }
      if(input$radio=="4"){
        x<- ggplotly(
          ggplot(abc, mapping= aes(wday(Date,label = T),fill=Grade))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      }
    }
    if(input$radio1=="3"){
      def <- data.frame(
        Date <- c(subdata2()$Date,subdata2()$Date),
        Grade <- c(subdata2()$GD1,subdata2()$GD2)
      )
      x<- ggplotly(
        ggplot(def, mapping= aes(Date,fill=Grade))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
      if(input$radio=="2"){
        x<- ggplotly(
          ggplot(def, mapping= aes(Date,fill=Grade))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$radio=="3"){
        x<- ggplotly(
          ggplot(def, mapping= aes(wday(Date,label = T),fill=Grade))+
            geom_bar(position = "stack")+xlab("Day")
        )
      }
      if(input$radio=="4"){
        x<- ggplotly(
          ggplot(def, mapping= aes(wday(Date,label = T),fill=Grade))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      }
    }
    if(input$radio1=="4"){
      ghi <- data.frame(
        Date <- c(subdata3()$Date,subdata3()$Date),
        Grade <- c(subdata3()$GD1,subdata3()$GD2)
      )
      x<- ggplotly(
        ggplot(ghi, mapping= aes(Date,fill=Grade))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
      if(input$radio=="2"){
        x<- ggplotly(
          ggplot(ghi, mapping= aes(Date,fill=Grade))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$radio=="3"){
        x<- ggplotly(
          ggplot(ghi, mapping= aes(wday(Date,label = T),fill=Grade))+
            geom_bar(position = "stack")+xlab("Day")
        )
      }
      if(input$radio=="4"){
        x<- ggplotly(
          ggplot(ghi, mapping= aes(wday(Date,label = T),fill=Grade))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      }
    }
    x$elementId <- NULL
    x
  })
  subdata4 <- reactive({
    Psort %>%
      filter(
        as.Date(Psort$Date)>= as.Date(Psort$Date[1]),
        as.Date(Psort$Date)<= as.Date(Psort$Date[1])
      )
  })
  subdata5 <- reactive({
    Psort %>%
      filter(
        as.Date(Psort$Date)>= as.Date(Psort$Date[1]),
        as.Date(Psort$Date)<= as.Date(Psort$Date[1])+7
      )
  })
  subdata6 <- reactive({
    Psort %>%
      filter(
        as.Date(Psort$Date)>= as.Date(Psort$Date[1]),
        as.Date(Psort$Date)<= as.Date(Psort$Date[1])+30
      )
  })
  output$Barchart1 <- renderPlotly({
    x<- ggplotly(
      ggplot(Psort, mapping= aes(Date,fill=GD3))+
        geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
    )
    if(input$p1=="2"){
      x<- ggplotly(
        ggplot(Psort, mapping= aes(Date,fill=GD3))+
          geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
    }
    if(input$p1=="3"){
      x<- ggplotly(
        ggplot(Psort, mapping= aes(wday(Date,label = T),fill=GD3))+
          geom_bar(position = "stack")+xlab("Day")
      )
    }
    if(input$p1=="4"){
      x<- ggplotly(
        ggplot(Psort, mapping= aes(wday(Date,label = T),fill=GD3))+
          geom_bar(position = "dodge")+xlab("Day")
      ) 
    }
    if(input$p2=="2"){
      x<- ggplotly(
        ggplot(subdata4(), mapping= aes(Date,fill=GD3))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
      if(input$p1=="2"){
        x<- ggplotly(
          ggplot(subdata4(), mapping= aes(Date,fill=GD3))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$p1=="3"){
        x<- ggplotly(
          ggplot(subdata4(), mapping= aes(wday(Date,label = T),fill=GD3))+
            geom_bar(position = "stack")+xlab("Day")
        )
      }
      if(input$p1=="4"){
        x<- ggplotly(
          ggplot(subdata4(), mapping= aes(wday(Date,label = T),fill=GD3))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      }
    }
    if(input$p2=="3"){
      x<- ggplotly(
        ggplot(subdata5(), mapping= aes(Date,fill=GD3))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
      if(input$p1=="2"){
        x<- ggplotly(
          ggplot(subdata5(), mapping= aes(Date,fill=GD3))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$p1=="3"){
        x<- ggplotly(
          ggplot(subdata5(), mapping= aes(wday(Date,label = T),fill=GD3))+
            geom_bar(position = "stack")+xlab("Day")
        )
      }
      if(input$p1=="4"){
        x<- ggplotly(
          ggplot(subdata5(), mapping= aes(wday(Date,label = T),fill=GD3))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      }
    }
    if(input$p2=="4"){
      x<- ggplotly(
        ggplot(subdata6(), mapping= aes(Date,fill=GD3))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
      if(input$p1=="2"){
        x<- ggplotly(
          ggplot(subdata6(), mapping= aes(Date,fill=GD3))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$p1=="3"){
        x<- ggplotly(
          ggplot(subdata6(), mapping= aes(wday(Date,label = T),fill=GD3))+
            geom_bar(position = "stack")+xlab("Day")
        )
      }
      if(input$p1=="4"){
        x<- ggplotly(
          ggplot(subdata6(), mapping= aes(wday(Date,label = T),fill=GD3))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      }
    }
    x$elementId <- NULL
    x
  })
  output$Barchart2 <- renderPlotly({
    x<- ggplotly(
      ggplot(Gsort, mapping= aes(Date,fill=GD1))+
        geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
    )
    if(input$x1=="2"){
      x<- ggplotly(
        ggplot(Gsort, mapping= aes(Date,fill=GD1))+
          geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
    }
    if(input$x1=="3"){
      x<- ggplotly(
        ggplot(Gsort, mapping= aes(wday(Date,label = T),fill=GD1))+
          geom_bar(position = "stack")+xlab("Day")
      )
    }
    if(input$x1=="4"){
      x<- ggplotly(
        ggplot(Gsort, mapping= aes(wday(Date,label = T),fill=GD1))+
          geom_bar(position = "dodge")+xlab("Day")
      ) 
    }
    if(input$x2=="2"){
      x<- ggplotly(
        ggplot(subdata1(), mapping= aes(Date,fill=GD1))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
      if(input$x1=="2"){
        x<- ggplotly(
          ggplot(subdata1(), mapping= aes(Date,fill=GD1))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$x1=="3"){
        x<- ggplotly(
          ggplot(subdata1(), mapping= aes(wday(Date,label = T),fill=GD1))+
            geom_bar(position = "stack")+xlab("Day")
        )
      }
      if(input$x1=="4"){
        x<- ggplotly(
          ggplot(subdata1(), mapping= aes(wday(Date,label = T),fill=GD1))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      }
    }
    if(input$x2=="3"){
      x<- ggplotly(
        ggplot(subdata2(), mapping= aes(Date,fill=GD1))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
      if(input$x1=="2"){
        x<- ggplotly(
          ggplot(subdata2(), mapping= aes(Date,fill=GD1))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$x1=="3"){
        x<- ggplotly(
          ggplot(subdata2(), mapping= aes(wday(Date,label = T),fill=GD1))+
            geom_bar(position = "stack")+xlab("Day")
        )
      }
      if(input$x1=="4"){
        x<- ggplotly(
          ggplot(subdata2(), mapping= aes(wday(Date,label = T),fill=GD1))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      }
    }
    if(input$x2=="4"){
      x<- ggplotly(
        ggplot(subdata3(), mapping= aes(Date,fill=GD1))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
      if(input$x1=="2"){
        x<- ggplotly(
          ggplot(subdata3(), mapping= aes(Date,fill=GD1))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$x1=="3"){
        x<- ggplotly(
          ggplot(subdata3(), mapping= aes(wday(Date,label = T),fill=GD1))+
            geom_bar(position = "stack")+xlab("Day")
        )
      }
      if(input$x1=="4"){
        x<- ggplotly(
          ggplot(subdata3(), mapping= aes(wday(Date,label = T),fill=GD1))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      }
    }
    x$elementId <- NULL
    x
  })
  output$Barchart3 <- renderPlotly({
    x<- ggplotly(
      ggplot(Gsort, mapping= aes(Date,fill=GD2))+
        geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
    )
    if(input$y1=="2"){
      x<- ggplotly(
        ggplot(Gsort, mapping= aes(Date,fill=GD2))+
          geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        
      )
    }
    if(input$y1=="3"){
      x<- ggplotly(
        ggplot(Gsort, mapping= aes(wday(Date,label = T),fill=GD2))+
          geom_bar(position = "stack")+xlab("Day")
      )
    }
    if(input$y1=="4"){
      x<- ggplotly(
        ggplot(Gsort, mapping= aes(wday(Date,label = T),fill=GD2))+
          geom_bar(position = "dodge")+xlab("Day")
      ) 
    }
    if(input$y2=="2"){
      x<- ggplotly(
        ggplot(subdata1(), mapping= aes(Date,fill=GD2))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
      if(input$y1=="2"){
        x<- ggplotly(
          ggplot(subdata1(), mapping= aes(Date,fill=GD2))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$y1=="3"){
        x<- ggplotly(
          ggplot(subdata1(), mapping= aes(wday(Date,label = T),fill=GD2))+
            geom_bar(position = "stack")+xlab("Day")
        )
      }
      if(input$y1=="4"){
        x<- ggplotly(
          ggplot(subdata1(), mapping= aes(wday(Date,label = T),fill=GD2))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      }
    }
    if(input$y2=="3"){
      x<- ggplotly(
        ggplot(subdata2(), mapping= aes(Date,fill=GD2))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
      if(input$y1=="2"){
        x<- ggplotly(
          ggplot(subdata2(), mapping= aes(Date,fill=GD2))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$y1=="3"){
        x<- ggplotly(
          ggplot(subdata2(), mapping= aes(wday(Date,label = T),fill=GD2))+
            geom_bar(position = "stack")+xlab("Day")
        )
      }
      if(input$y1=="4"){
        x<- ggplotly(
          ggplot(subdata2(), mapping= aes(wday(Date,label = T),fill=GD2))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      }
    }
    if(input$y2=="4"){
      x<- ggplotly(
        ggplot(subdata3(), mapping= aes(Date,fill=GD2))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
      if(input$y1=="2"){
        x<- ggplotly(
          ggplot(subdata3(), mapping= aes(Date,fill=GD2))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$y1=="3"){
        x<- ggplotly(
          ggplot(subdata3(), mapping= aes(wday(Date,label = T),fill=GD2))+
            geom_bar(position = "stack")+xlab("Day")
        )
      }
      if(input$y1=="4"){
        x<- ggplotly(
          ggplot(subdata3(), mapping= aes(wday(Date,label = T),fill=GD2))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      }
    }
    x$elementId <- NULL
    x
  })
  subdata7 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(input$date_range1[1]),
        as.Date(Gsort$Date)<= as.Date(input$date_range1[2])
      )
  })
  subdata8<- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(input$date_range1[1]),
        as.Date(Gsort$Date)<= as.Date(input$date_range1[2]),
        hms(Gsort$Time)>=hms("7H 0M 0S"),
        hms(Gsort$Time)<=hms("15H 0M 0S")
      )
  })
  subdata9 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(input$date_range1[1]),
        as.Date(Gsort$Date)<= as.Date(input$date_range1[2]),
        hms(Gsort$Time)>=hms("15H 0M 0S"),
        hms(Gsort$Time)<=hms("23H 0M 0S")
      )
  })
  subdata10 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(input$date_range1[1]),
        as.Date(Gsort$Date)<= as.Date(input$date_range1[2]),
        hms(Gsort$Time)>=hms("23H 0M 0S"),
        hms(Gsort$Time)<=hms("24H 0M 0S")
      )
  })
  output$plot1<- renderPlotly({
    jkl <- data.frame(
      Date <- c(subdata7()$Date,subdata7()$Date),
      Grade <- c(subdata7()$GD1,subdata7()$GD2)
    )
    x<- ggplotly(
      ggplot(jkl, mapping= aes(Date,fill=Grade))+
        geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
    )
    if(input$gd=="2"){
      x<- ggplotly(
        ggplot(jkl, mapping= aes(Date,fill=Grade))+
          geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
    }
    if(input$gd=="3"){
      x<- ggplotly(
        ggplot(jkl, mapping= aes(wday(Date,label = T),fill=Grade))+
          geom_bar(position = "stack")+xlab("Day")
      )
    }
    if(input$gd=="4"){
      x<- ggplotly(
        ggplot(jkl, mapping= aes(wday(Date,label = T),fill=Grade))+
          geom_bar(position = "dodge")+xlab("Day")
      ) 
    }
    if(input$gd1=="2"){
      mno <- data.frame(
        Date <- c(subdata8()$Date,subdata8()$Date),
        Grade <- c(subdata8()$GD1,subdata8()$GD2)
      )
      x<- ggplotly(
        ggplot(mno, mapping= aes(Date,fill=Grade))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
      if(input$gd=="2"){
        x<- ggplotly(
          ggplot(mno, mapping= aes(Date,fill=Grade))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$gd=="3"){
        x<- ggplotly(
          ggplot(mno, mapping= aes(wday(Date,label = T),fill=Grade))+
            geom_bar(position = "stack")+xlab("Day")
        )
      }
      if(input$gd=="4"){
        x<- ggplotly(
          ggplot(mno, mapping= aes(wday(Date,label = T),fill=Grade))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      }
    }
    if(input$gd1=="3"){
      pqr <- data.frame(
        Date <- c(subdata9()$Date,subdata9()$Date),
        Grade <- c(subdata9()$GD1,subdata9()$GD2)
      )
      x<- ggplotly(
        ggplot(pqr, mapping= aes(Date,fill=Grade))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
      if(input$gd=="2"){
        x<- ggplotly(
          ggplot(pqr, mapping= aes(Date,fill=Grade))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$gd=="3"){
        x<- ggplotly(
          ggplot(pqr, mapping= aes(wday(Date,label = T),fill=Grade))+
            geom_bar(position = "stack")+xlab("Day")
        )
      }
      if(input$gd=="4"){
        x<- ggplotly(
          ggplot(pqr, mapping= aes(wday(Date,label = T),fill=Grade))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      } 
    }
    if(input$gd1=="4"){
      stu <- data.frame(
        Date <- c(subdata10()$Date,subdata10()$Date),
        Grade <- c(subdata10()$GD1,subdata10()$GD2)
      )
      x<- ggplotly(
        ggplot(stu, mapping= aes(Date,fill=Grade))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
        
      )
      if(input$gd=="2"){
        x<- ggplotly(
          ggplot(stu, mapping= aes(Date,fill=Grade))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$gd=="3"){
        x<- ggplotly(
          ggplot(stu, mapping= aes(wday(Date,label = T),fill=Grade))+
            geom_bar(position = "stack")+xlab("Day")
        )
      }
      if(input$gd=="4"){
        x<- ggplotly(
          ggplot(stu, mapping= aes(wday(Date,label = T),fill=Grade))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      }
    }
    x$elementId <- NULL
    x
  })
  subdata11 <- reactive({
    Psort %>%
      filter(
        as.Date(Psort$Date)>= as.Date(input$date_range2[1]),
        as.Date(Psort$Date)<= as.Date(input$date_range2[2])
      )
  })
  subdata12 <- reactive({
    Psort %>%
      filter(
        as.Date(Psort$Date)>= as.Date(input$date_range2[1]),
        as.Date(Psort$Date)<= as.Date(input$date_range2[2]),
        hms(Psort$Time)>=hms("7H 0M 0S"),
        hms(Psort$Time)<=hms("15H 0M 0S")
      )
  })
  subdata13 <- reactive({
    Psort %>%
      filter(
        as.Date(Psort$Date)>= as.Date(input$date_range2[1]),
        as.Date(Psort$Date)<= as.Date(input$date_range2[2]),
        hms(Psort$Time)>=hms("15H 0M 0S"),
        hms(Psort$Time)<=hms("23H 0M 0S")
      )
  })
  subdata14 <- reactive({
    Psort %>%
      filter(
        as.Date(Psort$Date)>= as.Date(input$date_range2[1]),
        as.Date(Psort$Date)<= as.Date(input$date_range2[2]),
        hms(Psort$Time)>=hms("23H 0M 0S"),
        hms(Psort$Time)<=hms("24H 0M 0S")
      )
  })
  output$plot2<- renderPlotly({
    x<- ggplotly(
      ggplot(subdata11(), mapping= aes(Date,fill=GD3))+
        geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
    )
    if(input$pd=="2"){
      x<- ggplotly(
        ggplot(subdata11(), mapping= aes(Date,fill=GD3))+
          geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
    }
    if(input$pd=="3"){
      x<- ggplotly(
        ggplot(subdata11(), mapping= aes(wday(Date,label = T),fill=GD3))+
          geom_bar(position = "stack")+xlab("Day")
      )
    }
    if(input$pd=="4"){
      x<- ggplotly(
        ggplot(subdata11(), mapping= aes(wday(Date,label = T),fill=GD3))+
          geom_bar(position = "dodge")+xlab("Day")
      ) 
    }
    if(input$pd1=="2"){
      x<- ggplotly(
        ggplot(subdata12(), mapping= aes(Date,fill=GD3))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
      if(input$pd=="2"){
        x<- ggplotly(
          ggplot(subdata12(), mapping= aes(Date,fill=GD3))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$pd=="3"){
        x<- ggplotly(
          ggplot(subdata12(), mapping= aes(wday(Date,label = T),fill=GD3))+
            geom_bar(position = "stack")+xlab("Day")
        )
      }
      if(input$pd=="4"){
        x<- ggplotly(
          ggplot(subdata12(), mapping= aes(wday(Date,label = T),fill=GD3))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      }
    }
    if(input$pd1=="3"){
      x<- ggplotly(
        ggplot(subdata13(), mapping= aes(Date,fill=GD3))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
      if(input$pd=="2"){
        x<- ggplotly(
          ggplot(subdata13(), mapping= aes(Date,fill=GD3))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$pd=="3"){
        x<- ggplotly(
          ggplot(subdata13(), mapping= aes(wday(Date,label = T),fill=GD3))+
            geom_bar(position = "stack")+xlab("Day")
        )
      }
      if(input$pd=="4"){
        x<- ggplotly(
          ggplot(subdata13(), mapping= aes(wday(Date,label = T),fill=GD3))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      } 
    }
    if(input$pd1=="4"){
      x<- ggplotly(
        ggplot(subdata14(), mapping= aes(Date,fill=GD3))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
        
      )
      if(input$pd=="2"){
        x<- ggplotly(
          ggplot(subdata14(), mapping= aes(Date,fill=GD3))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$pd=="3"){
        x<- ggplotly(
          ggplot(subdata14(), mapping= aes(wday(Date,label = T),fill=GD3))+
            geom_bar(position = "stack")+xlab("Day")
        )
      }
      if(input$pd=="4"){
        x<- ggplotly(
          ggplot(subdata14(), mapping= aes(wday(Date,label = T),fill=GD3))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      }
    }
    x$elementId <- NULL
    x
  })
  subdata15 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(input$date_range3[1]),
        as.Date(Gsort$Date)<= as.Date(input$date_range3[2])
      )
  })
  subdata16 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(input$date_range3[1]),
        as.Date(Gsort$Date)<= as.Date(input$date_range3[2]),
        hms(Gsort$Time)>=hms("7H 0M 0S"),
        hms(Gsort$Time)<=hms("15H 0M 0S")
      )
  })
  subdata17 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(input$date_range3[1]),
        as.Date(Gsort$Date)<= as.Date(input$date_range3[2]),
        hms(Gsort$Time)>=hms("15H 0M 0S"),
        hms(Gsort$Time)<=hms("23H 0M 0S")
      )
  })
  subdata18 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(input$date_range3[1]),
        as.Date(Gsort$Date)<= as.Date(input$date_range3[2]),
        hms(Gsort$Time)>=hms("23H 0M 0S"),
        hms(Gsort$Time)<=hms("24H 0M 0S")
      )
  })
  output$plot3<- renderPlotly({
    x<- ggplotly(
      ggplot(subdata15(), mapping= aes(Date,fill=GD1))+
        geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
    )
    if(input$d=="2"){
      x<- ggplotly(
        ggplot(subdata15(), mapping= aes(Date,fill=GD1))+
          geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
    }
    if(input$d=="3"){
      x<- ggplotly(
        ggplot(subdata15(), mapping= aes(wday(Date,label = T),fill=GD1))+
          geom_bar(position = "stack")+xlab("Day")
      )
    }
    if(input$d=="4"){
      x<- ggplotly(
        ggplot(subdata15(), mapping= aes(wday(Date,label = T),fill=GD1))+
          geom_bar(position = "dodge")+xlab("Day")
      ) 
    }
    if(input$d1=="2"){
      x<- ggplotly(
        ggplot(subdata16(), mapping= aes(Date,fill=GD1))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
      if(input$d=="2"){
        x<- ggplotly(
          ggplot(subdata16(), mapping= aes(Date,fill=GD1))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$d=="3"){
        x<- ggplotly(
          ggplot(subdata16(), mapping= aes(wday(Date,label = T),fill=GD1))+
            geom_bar(position = "stack")+xlab("Day")
        )
      }
      if(input$d=="4"){
        x<- ggplotly(
          ggplot(subdata16(), mapping= aes(wday(Date,label = T),fill=GD1))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      }
    }
    if(input$d1=="3"){
      x<- ggplotly(
        ggplot(subdata17(), mapping= aes(Date,fill=GD1))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
      if(input$d=="2"){
        x<- ggplotly(
          ggplot(subdata17(), mapping= aes(Date,fill=GD1))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$d=="3"){
        x<- ggplotly(
          ggplot(subdata17(), mapping= aes(wday(Date,label = T),fill=GD1))+
            geom_bar(position = "stack")+xlab("Day")
        )
      }
      if(input$d=="4"){
        x<- ggplotly(
          ggplot(subdata17(), mapping= aes(wday(Date,label = T),fill=GD1))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      } 
    }
    if(input$d1=="4"){
      x<- ggplotly(
        ggplot(subdata18(), mapping= aes(Date,fill=GD1))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
        
      )
      if(input$d=="2"){
        x<- ggplotly(
          ggplot(subdata18(), mapping= aes(Date,fill=GD1))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$d=="3"){
        x<- ggplotly(
          ggplot(subdata18(), mapping= aes(wday(Date,label = T),fill=GD1))+
            geom_bar(position = "stack")+xlab("Day")
        )
      }
      if(input$d=="4"){
        x<- ggplotly(
          ggplot(subdata18(), mapping= aes(wday(Date,label = T),fill=GD1))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      }
    }
    x$elementId <- NULL
    x
  })
  subdata19 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(input$date_range4[1]),
        as.Date(Gsort$Date)<= as.Date(input$date_range4[2])
      )
  })
  subdata20 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(input$date_range4[1]),
        as.Date(Gsort$Date)<= as.Date(input$date_range4[2]),
        hms(Gsort$Time)>=hms("7H 0M 0S"),
        hms(Gsort$Time)<=hms("15H 0M 0S")
      )
  })
  subdata21 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(input$date_range4[1]),
        as.Date(Gsort$Date)<= as.Date(input$date_range4[2]),
        hms(Gsort$Time)>=hms("15H 0M 0S"),
        hms(Gsort$Time)<=hms("23H 0M 0S")
      )
  })
  subdata22 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(input$date_range4[1]),
        as.Date(Gsort$Date)<= as.Date(input$date_range4[2]),
        hms(Gsort$Time)>=hms("23H 0M 0S"),
        hms(Gsort$Time)<=hms("24H 0M 0S")
      )
  })
  output$plot4<- renderPlotly({
    x<- ggplotly(
      ggplot(subdata19(), mapping= aes(Date,fill=GD2))+
        geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
    )
    if(input$g=="2"){
      x<- ggplotly(
        ggplot(subdata19(), mapping= aes(Date,fill=GD2))+
          geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
    }
    if(input$g=="3"){
      x<- ggplotly(
        ggplot(subdata19(), mapping= aes(wday(Date,label = T),fill=GD2))+
          geom_bar(position = "stack")+xlab("Day")
      )
    }
    if(input$g=="4"){
      x<- ggplotly(
        ggplot(subdata19(), mapping= aes(wday(Date,label = T),fill=GD2))+
          geom_bar(position = "dodge")+xlab("Day")
      ) 
    }
    if(input$g1=="2"){
      x<- ggplotly(
        ggplot(subdata20(), mapping= aes(Date,fill=GD2))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
      if(input$g=="2"){
        x<- ggplotly(
          ggplot(subdata20(), mapping= aes(Date,fill=GD2))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$g=="3"){
        x<- ggplotly(
          ggplot(subdata20(), mapping= aes(wday(Date,label = T),fill=GD2))+
            geom_bar(position = "stack")+xlab("Day")
        )
      }
      if(input$g=="4"){
        x<- ggplotly(
          ggplot(subdata20(), mapping= aes(wday(Date,label = T),fill=GD2))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      }
    }
    if(input$g1=="3"){
      x<- ggplotly(
        ggplot(subdata21(), mapping= aes(Date,fill=GD2))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
      if(input$g=="2"){
        x<- ggplotly(
          ggplot(subdata21(), mapping= aes(Date,fill=GD2))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$g=="3"){
        x<- ggplotly(
          ggplot(subdata21(), mapping= aes(wday(Date,label = T),fill=GD2))+
            geom_bar(position = "stack")+xlab("Day")
        )
      }
      if(input$g=="4"){
        x<- ggplotly(
          ggplot(subdata21(), mapping= aes(wday(Date,label = T),fill=GD2))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      } 
    }
    if(input$g1=="4"){
      x<- ggplotly(
        ggplot(subdata22(), mapping= aes(Date,fill=GD2))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
        
      )
      if(input$g=="2"){
        x<- ggplotly(
          ggplot(subdata22(), mapping= aes(Date,fill=GD2))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$g=="3"){
        x<- ggplotly(
          ggplot(subdata22(), mapping= aes(wday(Date,label = T),fill=GD2))+
            geom_bar(position = "stack")+xlab("Day")
        )
      }
      if(input$g=="4"){
        x<- ggplotly(
          ggplot(subdata22(), mapping= aes(wday(Date,label = T),fill=GD2))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      }
    }
    x$elementId <- NULL
    x
  })
  output$table <- renderDataTable({
    PAWL <- as.numeric(c(pgd3a,pgd3b,pgd3c,pgd3r,pgd3a+pgd3b+pgd3c+pgd3r))
    GuidePlateR <- as.numeric(c(ggd2a,ggd2b,ggd2c,ggd2r,ggd2a+ggd2b+ggd2c+ggd2r))
    GuidePlateL <- as.numeric(c(ggd1a,ggd1b,ggd1c,ggd1r,ggd1a+ggd1b+ggd1c+ggd1r))
    Grades = (c(c("A","B","C","R","Total"),PAWL, GuidePlateR,GuidePlateL))
    a=matrix(Grades,nrow =5, ncol = 4, dimnames = list(c("A","B","C","R","Total"),c("Grade","PAWL","GuidePlate(GD2)","GuidePlate(GD1)")))
    a
  })
  output$table1 <- renderDataTable({
    GuidePlate <- as.numeric(c(ggd3aa,ggd3ab,ggd3ar,ggd3bb,ggd3bc,ggd3br,ggd3ca,ggd3cc,ggd3cr,ggd3r,ggd3aa+ggd3ab+ggd3ar+ggd3bb+ggd3bc+ggd3br+ggd3ca+ggd3cc+ggd3cr+ggd3r))
    Grades =c(c("AA","AB","AR","BB","BC","BR","CA","CC","CR","R","Total"),GuidePlate)
    a= matrix(Grades,nrow= 11, ncol=2, dimnames=list(c("AA","AB","AR","BB","BC","BR","CA","CC","CR","R","Total"),c("Grade","GuidePlate(Total Grade)")))
    a
  })
  guidep = 0;
  guider = 0;
  pawlp = 0;
  pawlr = 0;
  totalp = 0;
  totalr = 0;
  for(i in 1:nrow(Gsort)){
    if(Gsort$GD3[i]!="R")
      guidep=guidep+1;
    if(Gsort$GD3[i]=="R")
      guider=guider+1;
  }
  for(i in 1:nrow(Psort)){
    if(Psort$GD3[i]!="R")
      pawlp=pawlp+1;
    if(Psort$GD3[i]=="R")
      pawlr=pawlr+1;
  }
  for(i in 1:nrow(Total)){
    if(Total$GD3[i]!="R")
      totalp=totalp+1;
    if(Total$GD3[i]=="R")
      totalr=totalr+1;
  }
  output$table2 <- renderDataTable({
    Parts <- as.numeric(c(guidep,pawlp,totalp))
    Rej <- as.numeric(c(guider,pawlr,totalr))
    total <- as.numeric(c(guidep+guider,pawlp+pawlr,totalp+totalr))
    Grades =c(c("Guide Plate","PAWL","Total"),Parts,Rej,total)
    a= matrix(Grades,nrow= 3, ncol=4, dimnames=list(c("Guide Plate","PAWL","Total"),c("Part Name","Accepted","Rejected","Total")))
    a
  })
  observeEvent(input$generate, {
    output$pdfview <- renderUI({
      tags$iframe(style="height:600px; width:100%", src="R IN ACTION.pdf")
    })
  })
  output$tbl1 <- renderDataTable({
    if(input$radio1=="1"){
      count <- as.numeric(c(ggd1a+ggd2a,ggd1b+ggd2b,ggd1c+ggd2c,ggd1r+ggd2r,ggd1a+ggd2a+ggd1b+ggd2b+ggd1c+ggd2c+ggd1r+ggd2r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$radio1=="2"){
      a1=0
      b1=0
      c1=0
      r1=0
      a2=0
      b2=0
      c2=0
      r2=0
      for(i in 1:nrow(subdata1())){
        if(subdata1()$GD1[i]=="A")
          a1=a1+1
        if(subdata1()$GD1[i]=="B")
          b1=b1+1
        if(subdata1()$GD1[i]=="C")
          c1=c1+1
        if(subdata1()$GD1[i]=="R")
          r1=r1+1
        if(subdata1()$GD2[i]=="A")
          a2=a2+1
        if(subdata1()$GD2[i]=="B")
          b2=b2+1
        if(subdata1()$GD2[i]=="C")
          c2=c2+1
        if(subdata1()$GD2[i]=="R")
          r2=r2+1
      }
      count <- as.numeric(c(a1+a2,b1+b2,c1+c2,r1+r2,a1+a2+b1+b2+c1+c2+r1+r2))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$radio1=="3"){
      a1=0
      b1=0
      c1=0
      r1=0
      a2=0
      b2=0
      c2=0
      r2=0
      for(i in 1:nrow(subdata2())){
        if(subdata2()$GD1[i]=="A")
          a1=a1+1
        if(subdata2()$GD1[i]=="B")
          b1=b1+1
        if(subdata2()$GD1[i]=="C")
          c1=c1+1
        if(subdata2()$GD1[i]=="R")
          r1=r1+1
        if(subdata2()$GD2[i]=="A")
          a2=a2+1
        if(subdata2()$GD2[i]=="B")
          b2=b2+1
        if(subdata2()$GD2[i]=="C")
          c2=c2+1
        if(subdata2()$GD2[i]=="R")
          r2=r2+1
      }
      count <- as.numeric(c(a1+a2,b1+b2,c1+c2,r1+r2,a1+a2+b1+b2+c1+c2+r1+r2))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$radio1=="4"){
      a1=0
      b1=0
      c1=0
      r1=0
      a2=0
      b2=0
      c2=0
      r2=0
      for(i in 1:nrow(subdata3())){
        if(subdata3()$GD1[i]=="A")
          a1=a1+1
        if(subdata3()$GD1[i]=="B")
          b1=b1+1
        if(subdata3()$GD1[i]=="C")
          c1=c1+1
        if(subdata3()$GD1[i]=="R")
          r1=r1+1
        if(subdata3()$GD2[i]=="A")
          a2=a2+1
        if(subdata3()$GD2[i]=="B")
          b2=b2+1
        if(subdata3()$GD2[i]=="C")
          c2=c2+1
        if(subdata3()$GD2[i]=="R")
          r2=r2+1
      }
      count <- as.numeric(c(a1+a2,b1+b2,c1+c2,r1+r2,a1+a2+b1+b2+c1+c2+r1+r2))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    a
  })
  output$tbl2 <- renderDataTable({
    if(input$gd1=="1"){
      a1=0
      b1=0
      c1=0
      r1=0
      a2=0
      b2=0
      c2=0
      r2=0
      for(i in 1:nrow(subdata7())){
        if(subdata7()$GD1[i]=="A")
          a1=a1+1
        if(subdata7()$GD1[i]=="B")
          b1=b1+1
        if(subdata7()$GD1[i]=="C")
          c1=c1+1
        if(subdata7()$GD1[i]=="R")
          r1=r1+1
        if(subdata7()$GD2[i]=="A")
          a2=a2+1
        if(subdata7()$GD2[i]=="B")
          b2=b2+1
        if(subdata7()$GD2[i]=="C")
          c2=c2+1
        if(subdata7()$GD2[i]=="R")
          r2=r2+1
      }
      count <- as.numeric(c(a1+a2,b1+b2,c1+c2,r1+r2,a1+a2+b1+b2+c1+c2+r1+r2))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$gd1=="2"){
      a1=0
      b1=0
      c1=0
      r1=0
      a2=0
      b2=0
      c2=0
      r2=0
      for(i in 1:nrow(subdata8())){
        if(subdata8()$GD1[i]=="A")
          a1=a1+1
        if(subdata8()$GD1[i]=="B")
          b1=b1+1
        if(subdata8()$GD1[i]=="C")
          c1=c1+1
        if(subdata8()$GD1[i]=="R")
          r1=r1+1
        if(subdata8()$GD2[i]=="A")
          a2=a2+1
        if(subdata8()$GD2[i]=="B")
          b2=b2+1
        if(subdata8()$GD2[i]=="C")
          c2=c2+1
        if(subdata8()$GD2[i]=="R")
          r2=r2+1
      }
      count <- as.numeric(c(a1+a2,b1+b2,c1+c2,r1+r2,a1+a2+b1+b2+c1+c2+r1+r2))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$gd1=="3"){
      a1=0
      b1=0
      c1=0
      r1=0
      a2=0
      b2=0
      c2=0
      r2=0
      for(i in 1:nrow(subdata9())){
        if(subdata9()$GD1[i]=="A")
          a1=a1+1
        if(subdata9()$GD1[i]=="B")
          b1=b1+1
        if(subdata9()$GD1[i]=="C")
          c1=c1+1
        if(subdata9()$GD1[i]=="R")
          r1=r1+1
        if(subdata9()$GD2[i]=="A")
          a2=a2+1
        if(subdata9()$GD2[i]=="B")
          b2=b2+1
        if(subdata9()$GD2[i]=="C")
          c2=c2+1
        if(subdata9()$GD2[i]=="R")
          r2=r2+1
      }
      count <- as.numeric(c(a1+a2,b1+b2,c1+c2,r1+r2,a1+a2+b1+b2+c1+c2+r1+r2))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$gd1=="4"){
      a1=0
      b1=0
      c1=0
      r1=0
      a2=0
      b2=0
      c2=0
      r2=0
      for(i in 1:nrow(subdata10())){
        if(subdata10()$GD1[i]=="A")
          a1=a1+1
        if(subdata10()$GD1[i]=="B")
          b1=b1+1
        if(subdata10()$GD1[i]=="C")
          c1=c1+1
        if(subdata10()$GD1[i]=="R")
          r1=r1+1
        if(subdata10()$GD2[i]=="A")
          a2=a2+1
        if(subdata10()$GD2[i]=="B")
          b2=b2+1
        if(subdata10()$GD2[i]=="C")
          c2=c2+1
        if(subdata10()$GD2[i]=="R")
          r2=r2+1
      }
      count <- as.numeric(c(a1+a2,b1+b2,c1+c2,r1+r2,a1+a2+b1+b2+c1+c2+r1+r2))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    a
  })
  output$tbl3 <- renderDataTable({
    if(input$p2=="1"){
      count <- as.numeric(c(pgd3a,pgd3b,pgd3c,pgd3r,pgd3a+pgd3b+pgd3c+pgd3r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$p2=="2"){
      a=0
      b=0
      c=0
      r=0
      for(i in 1:nrow(subdata4())){
        if(subdata4()$GD3[i]=="A")
          a=a+1
        if(subdata4()$GD3[i]=="B")
          b=b+1
        if(subdata4()$GD3[i]=="C")
          c=c+1
        if(subdata4()$GD3[i]=="R")
          r=r+1
      }
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$p2=="3"){
      a=0
      b=0
      c=0
      r=0
      for(i in 1:nrow(subdata5())){
        if(subdata5()$GD3[i]=="A")
          a=a+1
        if(subdata5()$GD3[i]=="B")
          b=b+1
        if(subdata5()$GD3[i]=="C")
          c=c+1
        if(subdata5()$GD3[i]=="R")
          r=r+1
      }
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$p2=="4"){
      a=0
      b=0
      c=0
      r=0
      for(i in 1:nrow(subdata6())){
        if(subdata6()$GD3[i]=="A")
          a=a+1
        if(subdata6()$GD3[i]=="B")
          b=b+1
        if(subdata6()$GD3[i]=="C")
          c=c+1
        if(subdata6()$GD3[i]=="R")
          r=r+1
      }
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    a
  })
  output$tbl4 <- renderDataTable({
    if(input$pd1=="1"){
      a=0
      b=0
      c=0
      r=0
      for(i in 1:nrow(subdata11())){
        if(subdata11()$GD3[i]=="A")
          a=a+1
        if(subdata11()$GD3[i]=="B")
          b=b+1
        if(subdata11()$GD3[i]=="C")
          c=c+1
        if(subdata11()$GD3[i]=="R")
          r=r+1
      }
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$pd1=="2"){
      a=0
      b=0
      c=0
      r=0
      for(i in 1:nrow(subdata12())){
        if(subdata12()$GD3[i]=="A")
          a=a+1
        if(subdata12()$GD3[i]=="B")
          b=b+1
        if(subdata12()$GD3[i]=="C")
          c=c+1
        if(subdata12()$GD3[i]=="R")
          r=r+1
      }
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$pd1=="3"){
      a=0
      b=0
      c=0
      r=0
      for(i in 1:nrow(subdata13())){
        if(subdata13()$GD3[i]=="A")
          a=a+1
        if(subdata13()$GD3[i]=="B")
          b=b+1
        if(subdata13()$GD3[i]=="C")
          c=c+1
        if(subdata13()$GD3[i]=="R")
          r=r+1
      }
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$pd1=="4"){
      a=0
      b=0
      c=0
      r=0
      for(i in 1:nrow(subdata14())){
        if(subdata14()$GD3[i]=="A")
          a=a+1
        if(subdata14()$GD3[i]=="B")
          b=b+1
        if(subdata14()$GD3[i]=="C")
          c=c+1
        if(subdata14()$GD3[i]=="R")
          r=r+1
      }
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    a
  })
  output$tbl5 <- renderDataTable({
    if(input$x2=="1"){
      count <- as.numeric(c(ggd1a,ggd1b,ggd1c,ggd1r,ggd1a+ggd1b+ggd1c+ggd1r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$x2=="2"){
      a=0
      b=0
      c=0
      r=0
      for(i in 1:nrow(subdata1())){
        if(subdata1()$GD1[i]=="A")
          a=a+1
        if(subdata1()$GD1[i]=="B")
          b=b+1
        if(subdata1()$GD1[i]=="C")
          c=c+1
        if(subdata1()$GD1[i]=="R")
          r=r+1
      }
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$x2=="3"){
      a=0
      b=0
      c=0
      r=0
      for(i in 1:nrow(subdata2())){
        if(subdata2()$GD1[i]=="A")
          a=a+1
        if(subdata2()$GD1[i]=="B")
          b=b+1
        if(subdata2()$GD1[i]=="C")
          c=c+1
        if(subdata2()$GD1[i]=="R")
          r=r+1
      }
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$x2=="4"){
      a=0
      b=0
      c=0
      r=0
      for(i in 1:nrow(subdata3())){
        if(subdata3()$GD1[i]=="A")
          a=a+1
        if(subdata3()$GD1[i]=="B")
          b=b+1
        if(subdata3()$GD1[i]=="C")
          c=c+1
        if(subdata3()$GD1[i]=="R")
          r=r+1
      }
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    a
  })
  output$tbl6 <- renderDataTable({
    if(input$d1=="1"){
      a=0
      b=0
      c=0
      r=0
      for(i in 1:nrow(subdata15())){
        if(subdata15()$GD1[i]=="A")
          a=a+1
        if(subdata15()$GD1[i]=="B")
          b=b+1
        if(subdata15()$GD1[i]=="C")
          c=c+1
        if(subdata15()$GD1[i]=="R")
          r=r+1
      }
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$d1=="2"){
      a=0
      b=0
      c=0
      r=0
      for(i in 1:nrow(subdata16())){
        if(subdata16()$GD1[i]=="A")
          a=a+1
        if(subdata16()$GD1[i]=="B")
          b=b+1
        if(subdata16()$GD1[i]=="C")
          c=c+1
        if(subdata16()$GD1[i]=="R")
          r=r+1
      }
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$d1=="3"){
      a=0
      b=0
      c=0
      r=0
      for(i in 1:nrow(subdata17())){
        if(subdata17()$GD1[i]=="A")
          a=a+1
        if(subdata17()$GD1[i]=="B")
          b=b+1
        if(subdata17()$GD1[i]=="C")
          c=c+1
        if(subdata17()$GD1[i]=="R")
          r=r+1
      }
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$d1=="4"){
      a=0
      b=0
      c=0
      r=0
      for(i in 1:nrow(subdata18())){
        if(subdata18()$GD1[i]=="A")
          a=a+1
        if(subdata18()$GD1[i]=="B")
          b=b+1
        if(subdata18()$GD1[i]=="C")
          c=c+1
        if(subdata18()$GD1[i]=="R")
          r=r+1
      }
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    a
  })
  output$tbl7 <- renderDataTable({
    if(input$y2=="1"){
      count <- as.numeric(c(ggd2a,ggd2b,ggd2c,ggd2r,ggd2a+ggd2b+ggd2c+ggd2r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$y2=="2"){
      a=0
      b=0
      c=0
      r=0
      for(i in 1:nrow(subdata1())){
        if(subdata1()$GD2[i]=="A")
          a=a+1
        if(subdata1()$GD2[i]=="B")
          b=b+1
        if(subdata1()$GD2[i]=="C")
          c=c+1
        if(subdata1()$GD2[i]=="R")
          r=r+1
      }
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$y2=="3"){
      a=0
      b=0
      c=0
      r=0
      for(i in 1:nrow(subdata2())){
        if(subdata2()$GD2[i]=="A")
          a=a+1
        if(subdata2()$GD2[i]=="B")
          b=b+1
        if(subdata2()$GD2[i]=="C")
          c=c+1
        if(subdata2()$GD2[i]=="R")
          r=r+1
      }
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$y2=="4"){
      a=0
      b=0
      c=0
      r=0
      for(i in 1:nrow(subdata3())){
        if(subdata3()$GD2[i]=="A")
          a=a+1
        if(subdata3()$GD2[i]=="B")
          b=b+1
        if(subdata3()$GD2[i]=="C")
          c=c+1
        if(subdata3()$GD2[i]=="R")
          r=r+1
      }
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    a
  })
  output$tbl8 <- renderDataTable({
    if(input$g1=="1"){
      a=0
      b=0
      c=0
      r=0
      for(i in 1:nrow(subdata19())){
        if(subdata19()$GD2[i]=="A")
          a=a+1
        if(subdata19()$GD2[i]=="B")
          b=b+1
        if(subdata19()$GD2[i]=="C")
          c=c+1
        if(subdata19()$GD2[i]=="R")
          r=r+1
      }
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$g1=="2"){
      a=0
      b=0
      c=0
      r=0
      for(i in 1:nrow(subdata20())){
        if(subdata20()$GD2[i]=="A")
          a=a+1
        if(subdata20()$GD2[i]=="B")
          b=b+1
        if(subdata20()$GD2[i]=="C")
          c=c+1
        if(subdata20()$GD2[i]=="R")
          r=r+1
      }
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$g1=="3"){
      a=0
      b=0
      c=0
      r=0
      for(i in 1:nrow(subdata21())){
        if(subdata21()$GD2[i]=="A")
          a=a+1
        if(subdata21()$GD2[i]=="B")
          b=b+1
        if(subdata21()$GD2[i]=="C")
          c=c+1
        if(subdata21()$GD2[i]=="R")
          r=r+1
      }
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$g1=="4"){
      a=0
      b=0
      c=0
      r=0
      for(i in 1:nrow(subdata22())){
        if(subdata22()$GD2[i]=="A")
          a=a+1
        if(subdata22()$GD2[i]=="B")
          b=b+1
        if(subdata22()$GD2[i]=="C")
          c=c+1
        if(subdata22()$GD2[i]=="R")
          r=r+1
      }
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    a
  })
})
