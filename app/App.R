#To attach the required libraries
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
library(shinycssloaders)
library(shinyjs)
#ip address of sql server
ip<-"127.0.0.1"
#database name
database="data"
#username of sql table
username="root"
#password of sql table
password="ctc@12345"

appCSS <- "
#loading-content {
position: absolute;
background: #000000;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"
server<-
 
#used to read data from sql table
shinyServer(function(input,output){
  mydb <- dbPool(
    RMySQL::MySQL(), 
    dbname = database,
    host = ip,
    username = username,
    password = password
  )
  Psort<-dbReadTable(mydb,"guide")
  Gsort<-dbReadTable(mydb,"guide")
  poolClose(mydb) 
  #used to change column names of a table
  #Grade 1 for guide plate
  names(Gsort)[names(Gsort) == 'GRADE_1'] <- 'GD1'
  #Grade 2 for guide plate
  names(Gsort)[names(Gsort) == 'GRADE_2'] <- 'GD2'
  #column name for date
  names(Gsort)[names(Gsort) == 'TIME_STAMP'] <- 'Date'
  #width 1 for guide plate
  names(Gsort)[names(Gsort) == 'WIDTH_1'] <- 'WD1'
  #width 2 for guide plate
  names(Gsort)[names(Gsort) == 'WIDTH_2'] <- 'WD2'
  
  #Grade 1 for pawl
  names(Psort)[names(Psort) == 'GRADE_1'] <- 'GD1'
  #grade 2 for pawl
  names(Psort)[names(Psort) == 'GRADE_2'] <- 'GD2'
  #column name for pawl
  names(Psort)[names(Psort) == 'TIME_STAMP'] <- 'Date'
  #width 1 for pawl
  names(Psort)[names(Psort) == 'WIDTH_1'] <- 'WD1'
  #width 2 for pawl
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
  
  #To display minimum date in Guide Plate
  output$text1<- renderUI({
    startdate =min(Gsort$Date)
  })
  #To display maximum date in Guide Plate
  output$text2<- renderUI({
    enddate   =max(Gsort$Date)
  })
  #To display minimum date in PAWL
  output$text3<- renderUI({
    startdate =min(Psort$Date)
  })
  #To display maximum date in PAWL
  output$text4<- renderUI({
    enddate   =max(Psort$Date)
  })
  #To display minimum date in Guide Plate
  output$text5<- renderUI({
    startdate =min(Gsort$Date)
  })
  #To display maximum date in Guide Plate
  output$text6<- renderUI({
    enddate   =max(Gsort$Date)
  })
  #To display minimum date in PAWL
  output$text7<- renderUI({
    startdate =min(Psort$Date)
  })
  #To display maximum date in PAWL
  output$text8<- renderUI({
    enddate   =max(Psort$Date)
  })
  #To filter data to a particular date range (One day)
  subdata1 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(max(Gsort$Date)),
        as.Date(Gsort$Date)<= as.Date(max(Gsort$Date))
      )
  })
  #One week from recent date
  subdata2 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(max(Gsort$Date))-7,
        as.Date(Gsort$Date)<= as.Date(max(Gsort$Date))
      )
  })
  #One month from recent date
  subdata3 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(max(Gsort$Date))-30,
        as.Date(Gsort$Date)<= as.Date(max(Gsort$Date))
      )
  })
  #To plot Guide Plate data 
  output$Barchart <- renderPlotly({
    xyz <- data.frame(
      #To combine to grades as one
      Date <- c(Gsort$Date,Gsort$Date),
      Grade <- c(Gsort$GD1,Gsort$GD2)
    )
    #Datewise data as stack
    x<- ggplotly(
      ggplot(xyz, mapping= aes(Date,fill=Grade))+
        geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
    )
    #Datewise data as dodge
    if(input$radio=="2"){
      x<- ggplotly(
        ggplot(xyz, mapping= aes(Date,fill=Grade))+
          geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
    }
    #Daywise data as stack
    if(input$radio=="3"){
      x<- ggplotly(
        ggplot(xyz, mapping= aes(wday(Date,label = T),fill=Grade))+
          geom_bar(position = "stack")+xlab("Day")
      )
    }
    #Daywise data as dodge
    if(input$radio=="4"){
      x<- ggplotly(
        ggplot(xyz, mapping= aes(wday(Date,label = T),fill=Grade))+
          geom_bar(position = "dodge")+xlab("Day")
      ) 
    }
    #For differnt radio buttons
    if(input$radio1=="2"){
      #To combine GD1 and GD2
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
  #One day data starting from recent data - PAWL
  subdata4 <- reactive({
    Psort %>%
      filter(
        as.Date(Psort$Date)>= as.Date(max(Psort$Date)),
        as.Date(Psort$Date)<= as.Date(max(Psort$Date))
      )
  })
  #One week data from recent date - PAWL
  subdata5 <- reactive({
    Psort %>%
      filter(
        as.Date(Psort$Date)>= as.Date(max(Psort$Date))-7,
        as.Date(Psort$Date)<= as.Date(max(Psort$Date))
      )
  })
  #One month data from recent date - PAWL
  subdata6 <- reactive({
    Psort %>%
      filter(
        as.Date(Psort$Date)>= as.Date(max(Psort$Date))-30,
        as.Date(Psort$Date)<= as.Date(max(Psort$Date))
      )
  })
  #Plotting PAWL Data
  output$Barchart1 <- renderPlotly({
    #PAWL data datewise stack
    x<- ggplotly(
      ggplot(Psort, mapping= aes(Date,fill=GD3))+
        geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
    )
    #PAWL data datewise dodge
    if(input$p1=="2"){
      x<- ggplotly(
        ggplot(Psort, mapping= aes(Date,fill=GD3))+
          geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
    }
    #PAWL data daywise stack
    if(input$p1=="3"){
      x<- ggplotly(
        ggplot(Psort, mapping= aes(wday(Date,label = T),fill=GD3))+
          geom_bar(position = "stack")+xlab("Day")
      )
    }
    #PAWL data daywise Dodge
    if(input$p1=="4"){
      x<- ggplotly(
        ggplot(Psort, mapping= aes(wday(Date,label = T),fill=GD3))+
          geom_bar(position = "dodge")+xlab("Day")
      ) 
    }
    #For different radio buttons
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
    #To remove unwanted warnings
    x$elementId <- NULL
    x
  })
  #Plotting Guide Plate leftside grade 
  output$Barchart2 <- renderPlotly({
    #Datewise Stack
    x<- ggplotly(
      ggplot(Gsort, mapping= aes(Date,fill=GD1))+
        geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
    )
    #Datewise Dodge
    if(input$x1=="2"){
      x<- ggplotly(
        ggplot(Gsort, mapping= aes(Date,fill=GD1))+
          geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
    }
    #Daywise Stack
    if(input$x1=="3"){
      x<- ggplotly(
        ggplot(Gsort, mapping= aes(wday(Date,label = T),fill=GD1))+
          geom_bar(position = "stack")+xlab("Day")
      )
    }
    #Daywise Dodge
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
  #Guide Plate rightside Grade
  output$Barchart3 <- renderPlotly({
    #Datewise Stack
    x<- ggplotly(
      ggplot(Gsort, mapping= aes(Date,fill=GD2))+
        geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
    )
    #Datewise dodge
    if(input$y1=="2"){
      x<- ggplotly(
        ggplot(Gsort, mapping= aes(Date,fill=GD2))+
          geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
    }
    #Daywise Stack
    if(input$y1=="3"){
      x<- ggplotly(
        ggplot(Gsort, mapping= aes(wday(Date,label = T),fill=GD2))+
          geom_bar(position = "stack")+xlab("Day")
      )
    }
    #Daywise dodge
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
  #Filtering data by desired date range - GuidePlate
  subdata7 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(input$date_range1[1]),
        as.Date(Gsort$Date)<= as.Date(input$date_range1[2])
      )
  })
  #Filtering data by desired date range (Shift 1) - GuidePlate
  subdata8 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(input$date_range1[1]),
        as.Date(Gsort$Date)<= as.Date(input$date_range1[2]),
        hms(Gsort$Time)>=hms("7H 0M 0S"),
        hms(Gsort$Time)<=hms("15H 0M 0S")
      )
  })
  #Filtering data by desired date range (Shift 2) - GuidePlate 
  subdata9 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(input$date_range1[1]),
        as.Date(Gsort$Date)<= as.Date(input$date_range1[2]),
        hms(Gsort$Time)>=hms("15H 0M 0S"),
        hms(Gsort$Time)<=hms("23H 0M 0S")
      )
  })
  #Filtering data by desired date range (Shift 3) - GuidePlate
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
    #Combining GD1 and GD2 of GuidePlate
    jkl <- data.frame(
      Date <- c(subdata7()$Date,subdata7()$Date),
      Grade <- c(subdata7()$GD1,subdata7()$GD2)
    )
    #Datewise data stack
    x<- ggplotly(
      ggplot(jkl, mapping= aes(Date,fill=Grade))+
        geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
    )
    #Datewise data dodge
    if(input$gd=="2"){
      x<- ggplotly(
        ggplot(jkl, mapping= aes(Date,fill=Grade))+
          geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
    }
    #Daywise data stack
    if(input$gd=="3"){
      x<- ggplotly(
        ggplot(jkl, mapping= aes(wday(Date,label = T),fill=Grade))+
          geom_bar(position = "stack")+xlab("Day")
      )
    }
    #Daywise data dodge
    if(input$gd=="4"){
      x<- ggplotly(
        ggplot(jkl, mapping= aes(wday(Date,label = T),fill=Grade))+
          geom_bar(position = "dodge")+xlab("Day")
      ) 
    }
    #For different radiobuttons
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
  #Filtering data for desired daterange - PAWL
  subdata11 <- reactive({
    Psort %>%
      filter(
        as.Date(Psort$Date)>= as.Date(input$date_range2[1]),
        as.Date(Psort$Date)<= as.Date(input$date_range2[2])
      )
  })
  #Filtering data for desired daterange (Shift-1) - PAWL
  subdata12 <- reactive({
    Psort %>%
      filter(
        as.Date(Psort$Date)>= as.Date(input$date_range2[1]),
        as.Date(Psort$Date)<= as.Date(input$date_range2[2]),
        hms(Psort$Time)>=hms("7H 0M 0S"),
        hms(Psort$Time)<=hms("15H 0M 0S")
      )
  })
  #Filtering data for desired daterange (Shift-2) - PAWL
  subdata13 <- reactive({
    Psort %>%
      filter(
        as.Date(Psort$Date)>= as.Date(input$date_range2[1]),
        as.Date(Psort$Date)<= as.Date(input$date_range2[2]),
        hms(Psort$Time)>=hms("15H 0M 0S"),
        hms(Psort$Time)<=hms("23H 0M 0S")
      )
  })
  #Filtering data for desired daterange (Shift-3)- PAWL
  subdata14 <- reactive({
    Psort %>%
      filter(
        as.Date(Psort$Date)>= as.Date(input$date_range2[1]),
        as.Date(Psort$Date)<= as.Date(input$date_range2[2]),
        hms(Psort$Time)>=hms("23H 0M 0S"),
        hms(Psort$Time)<=hms("24H 0M 0S")
      )
  })
  #To Plot PAWL data
  output$plot2<- renderPlotly({
    #Datewise data stack
    x<- ggplotly(
      ggplot(subdata11(), mapping= aes(Date,fill=GD3))+
        geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
    )
    #Datewise data dodge
    if(input$pd=="2"){
      x<- ggplotly(
        ggplot(subdata11(), mapping= aes(Date,fill=GD3))+
          geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
    }
    #Daywise data stack
    if(input$pd=="3"){
      x<- ggplotly(
        ggplot(subdata11(), mapping= aes(wday(Date,label = T),fill=GD3))+
          geom_bar(position = "stack")+xlab("Day")
      )
    }
    #Daywise data dodge
    if(input$pd=="4"){
      x<- ggplotly(
        ggplot(subdata11(), mapping= aes(wday(Date,label = T),fill=GD3))+
          geom_bar(position = "dodge")+xlab("Day")
      ) 
    }
    #Similarly for various radiobuttons
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
  #To filter data to desired daterange - Guideplate GD1
  subdata15 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(input$date_range3[1]),
        as.Date(Gsort$Date)<= as.Date(input$date_range3[2])
      )
  })
  #To filter data to desired daterange (Shift 1)- Guideplate GD1
  subdata16 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(input$date_range3[1]),
        as.Date(Gsort$Date)<= as.Date(input$date_range3[2]),
        hms(Gsort$Time)>=hms("7H 0M 0S"),
        hms(Gsort$Time)<=hms("15H 0M 0S")
      )
  })
  #To filter data to desired daterange(Shift 2) - Guideplate GD1
  subdata17 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(input$date_range3[1]),
        as.Date(Gsort$Date)<= as.Date(input$date_range3[2]),
        hms(Gsort$Time)>=hms("15H 0M 0S"),
        hms(Gsort$Time)<=hms("23H 0M 0S")
      )
  })
  #To filter data to desired daterange(Shift 3) - Guideplate GD1
  subdata18 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(input$date_range3[1]),
        as.Date(Gsort$Date)<= as.Date(input$date_range3[2]),
        hms(Gsort$Time)>=hms("23H 0M 0S"),
        hms(Gsort$Time)<=hms("24H 0M 0S")
      )
  })
  #To plot guideplate GD1 data
  output$plot3<- renderPlotly({
    #Datewise data stack
    x<- ggplotly(
      ggplot(subdata15(), mapping= aes(Date,fill=GD1))+
        geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
    )
    #Datewise data dodge
    if(input$d=="2"){
      x<- ggplotly(
        ggplot(subdata15(), mapping= aes(Date,fill=GD1))+
          geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
    }
    #Daywise data stack
    if(input$d=="3"){
      x<- ggplotly(
        ggplot(subdata15(), mapping= aes(wday(Date,label = T),fill=GD1))+
          geom_bar(position = "stack")+xlab("Day")
      )
    }
    #Daywise data dodge
    if(input$d=="4"){
      x<- ggplotly(
        ggplot(subdata15(), mapping= aes(wday(Date,label = T),fill=GD1))+
          geom_bar(position = "dodge")+xlab("Day")
      ) 
    }
    #Similarly for various radiobuttons
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
  #To filter data to desired daterange - Guideplate GD2
  subdata19 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(input$date_range4[1]),
        as.Date(Gsort$Date)<= as.Date(input$date_range4[2])
      )
  })
  #To filter data to desired daterange(Shift 1) - Guideplate GD2
  subdata20 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(input$date_range4[1]),
        as.Date(Gsort$Date)<= as.Date(input$date_range4[2]),
        hms(Gsort$Time)>=hms("7H 0M 0S"),
        hms(Gsort$Time)<=hms("15H 0M 0S")
      )
  })
  #To filter data to desired daterange(Shift 2) - Guideplate GD2
  subdata21 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(input$date_range4[1]),
        as.Date(Gsort$Date)<= as.Date(input$date_range4[2]),
        hms(Gsort$Time)>=hms("15H 0M 0S"),
        hms(Gsort$Time)<=hms("23H 0M 0S")
      )
  })
  #To filter data to desired daterange(Shift 3) - Guideplate GD2
  subdata22 <- reactive({
    Gsort %>%
      filter(
        as.Date(Gsort$Date)>= as.Date(input$date_range4[1]),
        as.Date(Gsort$Date)<= as.Date(input$date_range4[2]),
        hms(Gsort$Time)>=hms("23H 0M 0S"),
        hms(Gsort$Time)<=hms("24H 0M 0S")
      )
  })
  #To plot guideplate GD2 data
  output$plot4<- renderPlotly({
    #Datewise data stack
    x<- ggplotly(
      ggplot(subdata19(), mapping= aes(Date,fill=GD2))+
        geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
    )
    #Datewise data dodge
    if(input$g=="2"){
      x<- ggplotly(
        ggplot(subdata19(), mapping= aes(Date,fill=GD2))+
          geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
    }
    #Daywise data stack
    if(input$g=="3"){
      x<- ggplotly(
        ggplot(subdata19(), mapping= aes(wday(Date,label = T),fill=GD2))+
          geom_bar(position = "stack")+xlab("Day")
      )
    }
    #Daywise data dodge
    if(input$g=="4"){
      x<- ggplotly(
        ggplot(subdata19(), mapping= aes(wday(Date,label = T),fill=GD2))+
          geom_bar(position = "dodge")+xlab("Day")
      ) 
    }
    #Similarly for various radiobuttons
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
  #To represent no.of parts in a table (Guideplate and PAWL)
  output$table <- renderDataTable({
    PAWL <- as.numeric(c(pgd3a,pgd3b,pgd3c,pgd3r,pgd3a+pgd3b+pgd3c+pgd3r))
    GuidePlateR <- as.numeric(c(ggd2a,ggd2b,ggd2c,ggd2r,ggd2a+ggd2b+ggd2c+ggd2r))
    GuidePlateL <- as.numeric(c(ggd1a,ggd1b,ggd1c,ggd1r,ggd1a+ggd1b+ggd1c+ggd1r))
    Grades = (c(c("A","B","C","R","Total"),PAWL, GuidePlateR,GuidePlateL))
    a=matrix(Grades,nrow =5, ncol = 4, dimnames = list(c("A","B","C","R","Total"),c("Grade","PAWL","GuidePlate(GD2)","GuidePlate(GD1)")))
    if(nrow(a)==0)
      return("No data is present")
     a
  })
  #To represent no.of parts in a table (Guideplate combinations)
  output$table1 <- renderDataTable({
    GuidePlate <- as.numeric(c(ggd3aa,ggd3ab,ggd3ar,ggd3bb,ggd3bc,ggd3br,ggd3ca,ggd3cc,ggd3cr,ggd3r,ggd3aa+ggd3ab+ggd3ar+ggd3bb+ggd3bc+ggd3br+ggd3ca+ggd3cc+ggd3cr+ggd3r))
    Grades =c(c("AA","AB","AR","BB","BC","BR","CA","CC","CR","R","Total"),GuidePlate)
    a= matrix(Grades,nrow= 11, ncol=2, dimnames=list(c("AA","AB","AR","BB","BC","BR","CA","CC","CR","R","Total"),c("Grade","GuidePlate(Total Grade)")))
     if(nrow(a)==0)
      return("No data is present")
     a
  })
  #To Count accepted and rejected parts in GuidePlate
  guidep = 0;
  guider = 0;
  pawlp = 0;
  pawlr = 0;
  totalp = 0;
  totalr = 0;
  for(i in 1:nrow(Gsort)){
    #To find no.of accepted parts in GuidePlate
    if(Gsort$GD3[i]!="R")
      guidep=guidep+1;
    #To find no.of rejected parts in GuidePlate
    if(Gsort$GD3[i]=="R")
      guider=guider+1;
  }
  for(i in 1:nrow(Psort)){
    #To find no.of accepted parts in PAWL
    if(Psort$GD3[i]!="R")
      pawlp=pawlp+1;
    #To find no.of rejected parts in PAWL
    if(Psort$GD3[i]=="R")
      pawlr=pawlr+1;
  }
  for(i in 1:nrow(Total)){
    #To find no.of accepted parts in Total (GuidePlate &PAWL)
    if(Total$GD3[i]!="R")
      totalp=totalp+1;
    #To find no.of rejected parts in Total (GuidePlate & PAWL)
    if(Total$GD3[i]=="R")
      totalr=totalr+1;
  }
  #To show overall accepted and rejected parts in table
  output$table2 <- renderDataTable({
    Parts <- as.numeric(c(guidep,pawlp,totalp))
    Rej <- as.numeric(c(guider,pawlr,totalr))
    total <- as.numeric(c(guidep+guider,pawlp+pawlr,totalp+totalr))
    Grades =c(c("Guide Plate","PAWL","Total"),Parts,Rej,total)
    a= matrix(Grades,nrow= 3, ncol=4, dimnames=list(c("Guide Plate","PAWL","Total"),c("Part Name","Accepted","Rejected","Total")))
    if(nrow(a)==0)
      return("No data is present")
      a
  })
  #Instructions to operate dashboards (Help)
  observeEvent(input$generate, {
    output$pdfview <- renderUI({
      tags$iframe(style="height:600px; width:100%", src="Offline Dashboard.pdf")
    })
  })
  #To show count of grades as table by varying radiobuttons
  output$tbl1 <- renderDataTable({
    if(input$radio1=="1"){
      count <- as.numeric(c(ggd1a+ggd2a,ggd1b+ggd2b,ggd1c+ggd2c,ggd1r+ggd2r,ggd1a+ggd2a+ggd1b+ggd2b+ggd1c+ggd2c+ggd1r+ggd2r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$radio1=="2"){
      #To count the no.of grades
      a1=0
      b1=0
      c1=0
      r1=0
      a2=0
      b2=0
      c2=0
      r2=0
      #To count parts in particular region
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
      #To represent in table
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
      #To count no.of parts in each grade in subdata3
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
    if(nrow(a)==0)
      return("No data is present")
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
      #To count no.of parts in each grade in subdata7
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
      #To represent in form of table
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
      #To count no.of parts in each grade in subdata8
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
      #To represent count inform of table
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
      #To count no.of parts in each grade in subdata9
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
      #To represent count inform of a table 
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
      #To count no.of parts in each grade in subdata10
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
      #To represent count inform of table
      count <- as.numeric(c(a1+a2,b1+b2,c1+c2,r1+r2,a1+a2+b1+b2+c1+c2+r1+r2))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(nrow(a)==0)
      return("No data is present")
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
      #To count no.of parts in each grade in subdata4
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
      #To represent count inform of a table
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$p2=="3"){
      a=0
      b=0
      c=0
      r=0
      #To count no.of parts in each grade in subdata5
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
      #To represent count inform of table
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$p2=="4"){
      a=0
      b=0
      c=0
      r=0
      #To count no.of parts in each grade in subdata6
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
      #To represent count inform of table
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(nrow(a)==0)
      return("No data is present")
    a
  })
  output$tbl4 <- renderDataTable({
    if(input$pd1=="1"){
      a=0
      b=0
      c=0
      r=0
      #To count no.of parts in each grade in subdata11
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
      #To represent count inform of table
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$pd1=="2"){
      a=0
      b=0
      c=0
      r=0
      #To count no.of parts in each grade in subdata12
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
      #To represent count inform of table
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$pd1=="3"){
      a=0
      b=0
      c=0
      r=0
      #To count no.of parts in each grade in subdata13
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
      #To represent count inform of a table
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$pd1=="4"){
      a=0
      b=0
      c=0
      r=0
      #To count no.of parts in each grade in subdata14
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
      #To represent count inform of table
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(nrow(a)==0)
      return("No data is present")
    a
  })
  #To show entire count in table
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
      #To count no.of parts in each grade in subdata1 - GD1
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
      #To show things in table
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$x2=="3"){
      a=0
      b=0
      c=0
      r=0
      #To count no.of parts in each grade in subdata2 - GD1
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
      #To show things inform of a table
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$x2=="4"){
      a=0
      b=0
      c=0
      r=0
      #To count no.of parts in each grade in subdata3 - GD1
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
      #To show things inform of a table
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(nrow(a)==0)
      return("No data is present")
    a
  })
  output$tbl6 <- renderDataTable({
    if(input$d1=="1"){
      a=0
      b=0
      c=0
      r=0
      #To count no.of parts in each grade in subdata15 - GD1
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
      #As a table
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$d1=="2"){
      a=0
      b=0
      c=0
      r=0
      #To count no.of parts in each grade in subdata16 - GD1
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
      #As a table
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$d1=="3"){
      a=0
      b=0
      c=0
      r=0
      #To count no.of parts in each grade in subdata17 - GD1
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
      #As a table
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$d1=="4"){
      a=0
      b=0
      c=0
      r=0
      #To count no.of parts in each grade in subdata18 - GD1
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
      #Inform of table
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(nrow(a)==0)
      return("No data is present")
    a
  })
  #Count of GuidePlate - GD2
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
      #To count no.of parts in each grade in subdata1 - GD2
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
      #Inform of a table
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$y2=="3"){
      a=0
      b=0
      c=0
      r=0
      #To count no.of parts in each grade in subdata2 - GD2
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
      #Inform of table
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$y2=="4"){
      a=0
      b=0
      c=0
      r=0
      #To count no.of parts in each grade in subdata3 - GD2
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
      #Inform of a table
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(nrow(a)==0)
      return("No data is present")
    a
  })
  output$tbl8 <- renderDataTable({
    if(input$g1=="1"){
      a=0
      b=0
      c=0
      r=0
      #To count no.of parts in each grade in subdata19 - GD2
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
      #Inform of table
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$g1=="2"){
      a=0
      b=0
      c=0
      r=0
      #To count no.of parts in each grade in subdata20 - GD2
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
      #Inform of table
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$g1=="3"){
      a=0
      b=0
      c=0
      r=0
      #To count no.of parts in each grade in subdata21 - GD2
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
      #Inform of table
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(input$g1=="4"){
      a=0
      b=0
      c=0
      r=0
      #To count no.of parts in each grade in subdata22 - GD2
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
      #Inform of table
      count <- as.numeric(c(a,b,c,r,a+b+c+r))
      Grades =c(c("A","B","C","R","Total"),count)
      a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
    }
    if(nrow(a)==0)
      return("No data is present")
    a
  })
  # Hide the loading message when the rest of the server function has executed
  hide(id = "loading-content", anim = TRUE, animType = "fade")    
  show("app")
})

ui<-fluidPage(
  useShinyjs(),
  inlineCSS(appCSS),
  # Loading message
  div(
    id = "loading-content",
    h2("Offline Analysis  is Loading...")
  ),
  # The main app code goes here
  hidden(
    div(
      id = "app",
      
  
  #User Interface (UI)
  shinyUI(
    dashboardPage(skin = "green",
                  #To fix dashboard page
                  dashboardHeader(title = "Offline Analysis",
                                  #Static notifications
                                  dropdownMenu(type="notifications", badgeStatus = "warning",
                                               notificationItem(icon = icon("warning"), status = "info",
                                                                "Batch Transfermation Happened")
                                  ),
                                  #Static messages
                                  dropdownMenu(type = "messages", badgeStatus = "info",
                                               messageItem(from = "Measuring Department", message = "Master detected", icon=icon("bookmark"))
                                  ),
                                  #Static Tasks
                                  dropdownMenu(type="tasks", badgeStatus = "info",
                                               taskItem(value = 30, color = "red", "Amount of R" )
                                  )
                  ),
                  #To fill things in sidebar
                  dashboardSidebar(
                    sidebarMenu(
                      menuItem("Dashboard", tabName = "dashboard",icon = icon("dashboard")),
                      menuItem("Company Production",tabName = "ti",icon = icon("list"),
                               menuSubItem("GuidePlate", tabName = "guide"),
                               menuSubItem("PAWL", tabName = "pawl")),
                      menuItem("Guide Plate",icon = icon("bullseye"),
                               menuSubItem("Guide Plate (GD1)",tabName = "gd1"),
                               menuSubItem("Guide Plate (GD2)",tabName = "gd2")),
                      menuItem("Summary",tabName = "ti-s",icon = icon("th-list")),
                      menuItem("Help",tabName = "h",icon = icon("question-circle"))
                    )),
                  #Dahboard body
                  dashboardBody(
                    #Tab Items
                    tabItems(
                      #Tab1 - Dahboard 
                      tabItem(tabName = "dashboard",
                              box(title = "Parts Produced",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE, withSpinner(dataTableOutput("table2")))),
                      #Tab2 - Company Production (GuidePlate & PAWL)
                      #GuidePlate
                      tabItem(tabName = "guide",
                              tabsetPanel(
                                #GuidePlate daterange 
                                tabPanel(title = "Guide Plate Shiftwise", icon = icon("user"),
                                         dateRangeInput("date_range1", label=h3("Date Range For Guide Plate"),start = Sys.Date()-20,end = Sys.Date()),
                                         h4(helpText("Sarting Date is(GP):"),htmlOutput("text1")),
                                         h4(helpText("Ending Date is(GP):"),htmlOutput("text2")),
                                         radioButtons("gd","Type of Barchart:",choices = c("Stack"="1","Dodge"="2","Week Days(Stack)"="3","Week Days(Dodge)"="4"),inline = TRUE),
                                         radioButtons("gd1","Choose Shift:", choices = c("All Shifts"="1","Shift 1"="2","Shift 2"="3","Shift 3"="4"),inline = TRUE),
                                         box(title = "Guide Plate",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE,column(8,withSpinner(plotlyOutput("plot1",height = 500))),column(4,withSpinner(dataTableOutput("tbl2"))))),
                                
                                #GuidePlate datewise
                                tabPanel(title = "Guide Plate",icon = icon("user"),
                                         radioButtons("radio","Type of Barchart:",choices = c("Stack"="1","Dodge"="2","Week Days(Stack)"="3","Week Days(Dodge)"="4"),inline = TRUE),
                                         radioButtons("radio1","Choose Visualization Period:",choices = c("One Day"="2","One Week"="3","One Month"="4"),inline = TRUE),
                                         box(title = "Guide Plate",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE, column(8,withSpinner(plotlyOutput("Barchart",height = 500))),column(4,withSpinner(dataTableOutput("tbl1")))))
                                  )),
                      #PAWL
                      tabItem(tabName = "pawl",
                              tabsetPanel(
                                
                                #Daterange
                                tabPanel(title = "PAWL Shiftwise", icon = icon("user-o"),
                                         dateRangeInput("date_range2", label=h3("Date Range For PAWL"),start = Sys.Date()-20,end = Sys.Date()),
                                         h4(helpText("Sarting Date is(P):"),htmlOutput("text3")),
                                         h4(helpText("Ending Date is(P):"),htmlOutput("text4")),
                                         radioButtons("pd","Type of Barchart:",choices = c("Stack"="1","Dodge"="2","Week Days(Stack)"="3","Week Days(Dodge)"="4"),inline = TRUE),
                                         radioButtons("pd1","Choose Shift:", choices = c("Shift 1"="2","Shift 2"="3","Shift 3"="4"),inline = TRUE),
                                         box(title = "PAWL",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE,column(8,withSpinner(plotlyOutput("plot2",height = 500))),column(4,withSpinner(dataTableOutput("tbl4"))))
                                ),
                                #Datewise
                                tabPanel(title = "PAWL",icon = icon("user-o"),
                                         radioButtons("p1","Type of Barchart:",choices = c("Stack"="1","Dodge"="2","Week Days(Stack)"="3","Week Days(Dodge)"="4"),inline = TRUE),
                                         radioButtons("p2","Choose Visualization Period:",choices = c("One Day"="2","One Week"="3","One Month"="4"),inline = TRUE),
                                         box(title = "PAWL",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE,column(8,withSpinner(plotlyOutput("Barchart1",height = 500))),column(4,withSpinner(dataTableOutput("tbl3"))))
                                )
                              )),
                      #Tab3 - GuidePlate
                      #GD1
                      tabItem(tabName = "gd1",
                              tabsetPanel(
                                #Daterange
                                tabPanel(title = "Guide Plate Shiftwise (GD1)", icon = icon("user"),
                                         dateRangeInput("date_range3", label=h3("Date Range For Guide Plate"),start = Sys.Date()-20,end = Sys.Date()),
                                         h4(helpText("Sarting Date is(GP):"),htmlOutput("text5")),
                                         h4(helpText("Ending Date is(GP):"),htmlOutput("text6")),
                                         radioButtons("d","Type of Barchart:",choices = c("Stack"="1","Dodge"="2","Week Days(Stack)"="3","Week Days(Dodge)"="4"),inline = TRUE),
                                         radioButtons("d1","Choose Shift:", choices = c("All Shifts"="1","Shift 1"="2","Shift 2"="3","Shift 3"="4"),inline = TRUE),
                                         box(title = "Guide Plate - GD1",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE,column(8,withSpinner(plotlyOutput("plot3",height = 500))),column(4,withSpinner(dataTableOutput("tbl6"))))
                                ),
                                #Datewise
                                tabPanel(title = "Guide Plate (GD1)",icon = icon("user"),
                                         radioButtons("x1","Type of Barchart:",choices = c("Stack"="1","Dodge"="2","Week Days(Stack)"="3","Week Days(Dodge)"="4"),inline = TRUE),
                                         radioButtons("x2","Choose Visualization Period:",choices = c("One Day"="2","One Week"="3","One Month"="4"),inline = TRUE),
                                         box(title = "Guide Plate - GD1",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE,column(8,withSpinner(plotlyOutput("Barchart2",height = 500))),column(4,withSpinner(dataTableOutput("tbl5")))))
                                
                              )),
                      #GD2
                      tabItem(tabName = "gd2",
                              tabsetPanel(
                                #Daterange
                                tabPanel(title = "Guide Plate Shiftwise (GD2)", icon = icon("user"),
                                         dateRangeInput("date_range4", label=h3("Date Range For Guide Plate"),start = Sys.Date()-20,end = Sys.Date()),
                                         h4(helpText("Sarting Date is(P):"),htmlOutput("text7")),
                                         h4(helpText("Ending Date is(P):"),htmlOutput("text8")),
                                         radioButtons("g","Type of Barchart:",choices = c("Stack"="1","Dodge"="2","Week Days(Stack)"="3","Week Days(Dodge)"="4"),inline = TRUE),
                                         radioButtons("g1","Choose Shift:", choices = c("All Shifts"="1","Shift 1"="2","Shift 2"="3","Shift 3"="4"),inline = TRUE),
                                         box(title = "Guide Plate - GD2",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE,column(8,withSpinner(plotlyOutput("plot4",height = 500))),column(4,withSpinner(dataTableOutput("tbl8"))))
                                ),
                                #Datewise
                                tabPanel(title = "Guide Plate (GD2)",icon = icon("user"),
                                         radioButtons("y1","Type of Barchart:",choices = c("Stack"="1","Dodge"="2","Week Days(Stack)"="3","Week Days(Dodge)"="4"),inline = TRUE),
                                         radioButtons("y2","Choose Visualization Period:",choices = c("One Day"="2","One Week"="3","One Month"="4"),inline = TRUE),
                                         box(title = "Guide Plate-GD2",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE,column(8,withSpinner(plotlyOutput("Barchart3",height = 500))),column(4,withSpinner(dataTableOutput("tbl7")))))
                                
                              )),
                      #Tab4 - Summary
                      tabItem(tabName = "ti-s",box(title = "Production Summary",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE, withSpinner(dataTableOutput("table"))),
                              box(title = "Production Summary",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE, withSpinner(dataTableOutput("table1")))
                      ),
                      #Tab5 - Help(Instructions to operate dashboard)
                      tabItem(tabName = "h",actionButton("generate", "Generate PDF"),uiOutput("pdfview"))
                    )
                  )
    ))
    ))
)
  
  
shinyApp(ui=ui,server = server)
