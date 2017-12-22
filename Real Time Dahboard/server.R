 #Attaching Libraries
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
library(plyr)
library(pool)
library(dplyr)
library(DT)
library(pool)


shinyServer(function(input,output){
   
    if(!file.exists("Shift.txt"))
      showModal(modalDialog(
        title = "Important message",
        "Shift timings file is not present!",
        easyClose = TRUE
      ))
    if(!file.exists("Guide and PAWL.txt"))
      showModal(modalDialog(
        title = "Important message",
        "Guide plate and Pawl limits file is not present!",
        easyClose = TRUE
      ))
    #Plot using ggplot2 and plotly  
    output$Hour <- renderPlotly({
      a1<-Gsort()
      a1$GD2<-NULL
      a2<-Gsort()
      a2$GD1<-NULL
      names(a2)[names(a2) == 'GD2'] <- 'GD1'
      a1<-rbind(a1,a2)
      #Plot function according to radio buttons
      x<- ggplotly(
        ggplot(a1, mapping= aes(Hour,fill= GD1))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
        
      )
      if(input$radio=="2"){
        x<- ggplotly(
          ggplot(a1, mapping= aes(Hour,fill=GD1))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$radio2=="2"){
        a1<-subdata()
        a1$GD2<-NULL
        a2<-subdata()
        a2$GD1<-NULL
        names(a2)[names(a2) == 'GD2'] <- 'GD1'
        abc<-rbind(a1,a2)
        x<- ggplotly(
          ggplot(abc, mapping= aes(Hour,fill=GD1))+
            geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
          
        )
        
        # For different radio buttons
        if(input$radio=="2"){
          x<- ggplotly(
            ggplot(abc, mapping= aes(Hour,fill=GD1))+
              geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
            
          )
        }
      }
      if(input$radio2=="3"){
        a1<-subdata1()
        a1$GD2<-NULL
        a2<-subdata1()
        a2$GD1<-NULL
        names(a2)[names(a2) == 'GD2'] <- 'GD1'
        def<-rbind(a1,a2)
        
        x<- ggplotly(
          ggplot(def, mapping= aes(Hour,fill=GD1))+
            geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
        if(input$radio=="2"){
          x<- ggplotly(
            ggplot(def, mapping= aes(Hour,fill=GD1))+
              geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
          )
        }
      }
      if(input$radio2=="4"){
        a1<-subdata2()
        a1$GD2<-NULL
        a2<-subdata2()
        a2$GD1<-NULL
        names(a2)[names(a2) == 'GD2'] <- 'GD1'
        ghi<-rbind(a1,a2)
        x<- ggplotly(
          ggplot(ghi, mapping= aes(Hour,fill=GD1))+
            geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
        if(input$radio=="2"){
          x<- ggplotly(
            ggplot(ghi, mapping= aes(Hour,fill=GD1))+
              geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
          )
        }
      }
      x$elementId <- NULL
      x
    })
    
    
    
    output$Barchart <- renderPlotly({
      a1<-Gsort()
      a1$GD2<-NULL
      a2<-Gsort()
      a2$GD1<-NULL
      names(a2)[names(a2) == 'GD2'] <- 'GD1'
      xyz<-rbind(a1,a2)
      
      #Plot type will be stack for date(one grade above other grade)
      x<- ggplotly(
        ggplot(xyz, mapping= aes(Date,fill=GD1))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))#+geom_line(data=Gsort(), aes(Date,count), colour="blue")
      )
      #Plot type will be dodge for date(one grade beside other grade)
      if(input$g1=="2"){
        x<- ggplotly(
          ggplot(xyz, mapping= aes(Date,fill=GD1))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      #Plot type will be stack for day(one grade above other grade)
      if(input$g1=="3"){
        x<- ggplotly(
          ggplot(xyz, mapping= aes(wday(Date,label = T),fill=GD1))+
            geom_bar(position = "stack")+xlab("Day")
        )
      }
      #Plot type will be dodge for day(one grade beside other grade)
      if(input$g1=="4"){
        x<- ggplotly(
          ggplot(xyz, mapping= aes(wday(Date,label = T),fill=GD1))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      }
      if(input$g2=="2"){
        a1<-subdata()
        a1$GD2<-NULL
        a2<-subdata()
        a2$GD1<-NULL
        names(a2)[names(a2) == 'GD2'] <- 'GD1'
        abc<-rbind(a1,a2)
        x<- ggplotly(
          ggplot(abc, mapping= aes(Date,fill=GD1))+
            geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
          
        )
        #For different radio buttons
        if(input$g1=="2"){
          x<- ggplotly(
            ggplot(abc, mapping= aes(Date,fill=GD1))+
              geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
          )
        }
        if(input$g1=="3"){
          x<- ggplotly(
            ggplot(abc, mapping= aes(wday(Date,label = T),fill=GD1))+
              geom_bar(position = "stack")+xlab("Day")
          )
        }
        if(input$g1=="4"){
          x<- ggplotly(
            ggplot(abc, mapping= aes(wday(Date,label = T),fill=GD1))+
              geom_bar(position = "dodge")+xlab("Day")
          ) 
        }
      }
      if(input$g2=="3"){
        a1<-subdata1()
        a1$GD2<-NULL
        a2<-subdata1()
        a2$GD1<-NULL
        names(a2)[names(a2) == 'GD2'] <- 'GD1'
        def<-rbind(a1,a2)
        
        x<- ggplotly(
          ggplot(def, mapping= aes(Date,fill=GD1))+
            geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
          
        )
        if(input$g1=="2"){
          x<- ggplotly(
            ggplot(def, mapping= aes(Date,fill=GD1))+
              geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
          )
        }
        if(input$g1=="3"){
          x<- ggplotly(
            ggplot(def, mapping= aes(wday(Date,label = T),fill=GD1))+
              geom_bar(position = "stack")+xlab("Day")
          )
        }
        if(input$g1=="4"){
          x<- ggplotly(
            ggplot(def, mapping= aes(wday(Date,label = T),fill=GD1))+
              geom_bar(position = "dodge")+xlab("Day")
          ) 
        }
      }
      if(input$g2=="4"){
        a1<-subdata2()
        a1$GD2<-NULL
        a2<-subdata2()
        a2$GD1<-NULL
        names(a2)[names(a2) == 'GD2'] <- 'GD1'
        ghi<-rbind(a1,a2)
        x<- ggplotly(
          ggplot(ghi, mapping= aes(Date,fill=GD1))+
            geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
        if(input$g1=="2"){
          x<- ggplotly(
            ggplot(ghi, mapping= aes(Date,fill=GD1))+
              geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
          )
        }
        if(input$g1=="3"){
          x<- ggplotly(
            ggplot(ghi, mapping= aes(wday(Date,label = T),fill=GD1))+
              geom_bar(position = "stack")+xlab("Day")
            
          )
        }
        if(input$g1=="4"){
          x<- ggplotly(
            ggplot(ghi, mapping= aes(wday(Date,label = T),fill=GD1))+
              geom_bar(position = "dodge")+xlab("Day")
          ) 
        }
      }
      x$elementId <- NULL
      x
    })
    
    
    #To plot data hourwise left side grade
    
    output$Hour2 <- renderPlotly({
      x<- ggplotly(
        ggplot(Gsort(), mapping= aes(Hour,fill=GD1))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
        
      )
      if(input$l1=="2"){
        x<- ggplotly(
          ggplot(Gsort(), mapping= aes(Hour,fill=GD1))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$l2=="2"){
        x<- ggplotly(
          ggplot(subdata(), mapping= aes(Hour,fill=GD1))+
            geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
          
        )
        if(input$l1=="2"){
          x<- ggplotly(
            ggplot(subdata(), mapping= aes(Hour,fill=GD1))+
              geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
            
          )
        }
      }
      if(input$l2=="3"){
        x<- ggplotly(
          ggplot(subdata1(), mapping= aes(Hour,fill=GD1))+
            geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
        if(input$l1=="2"){
          x<- ggplotly(
            ggplot(subdata1(), mapping= aes(Hour,fill=GD1))+
              geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
          )
        }
      }
      if(input$l2=="4"){
        x<- ggplotly(
          ggplot(subdata2(), mapping= aes(Hour,fill=GD1))+
            geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
        if(input$l1=="2"){
          x<- ggplotly(
            ggplot(subdata2(), mapping= aes(Hour,fill=GD1))+
              geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
          )
        }
      }
      x$elementId <- NULL
      x
    })
    #To plot left side grade of guide plate
    output$Left <- renderPlotly({
      x<- ggplotly(
        ggplot(Gsort(), mapping= aes(Date,fill=GD1))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
      if(input$l3=="2"){
        x<- ggplotly(
          ggplot(Gsort(), mapping= aes(Date,fill=GD1))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$l3=="3"){
        x<- ggplotly(
          ggplot(Gsort(), mapping= aes(wday(Date,label = T),fill=GD1))+
            geom_bar(position = "stack")+xlab("Day")
        )
      }
      if(input$l3=="4"){
        x<- ggplotly(
          ggplot(Gsort(), mapping= aes(wday(Date,label = T),fill=GD1))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      }
      if(input$l4=="2"){
        x<- ggplotly(
          ggplot(subdata(), mapping= aes(Date,fill=GD1))+
            geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
        if(input$l3=="2"){
          x<- ggplotly(
            ggplot(subdata(), mapping= aes(Date,fill=GD1))+
              geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
          )
        }
        if(input$l3=="3"){
          x<- ggplotly(
            ggplot(subdata(), mapping= aes(wday(Date,label = T),fill=GD1))+
              geom_bar(position = "stack")+xlab("Day")
          )
        }
        if(input$l3=="4"){
          x<- ggplotly(
            ggplot(subdata(), mapping= aes(wday(Date,label = T),fill=GD1))+
              geom_bar(position = "dodge")+xlab("Day")
          ) 
        }
      }
      if(input$l4=="3"){
        x<- ggplotly(
          ggplot(subdata1(), mapping= aes(Date,fill=GD1))+
            geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
        if(input$l3=="2"){
          x<- ggplotly(
            ggplot(subdata1(), mapping= aes(Date,fill=GD1))+
              geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
          )
        }
        if(input$l3=="3"){
          x<- ggplotly(
            ggplot(subdata1(), mapping= aes(wday(Date,label = T),fill=GD1))+
              geom_bar(position = "stack")+xlab("Day")
            
          )
        }
        if(input$l3=="4"){
          x<- ggplotly(
            ggplot(subdata1(), mapping= aes(wday(Date,label = T),fill=GD1))+
              geom_bar(position = "dodge")+xlab("Day")
            
          ) 
        }
      }
      if(input$l4=="4"){
        x<- ggplotly(
          ggplot(subdata2(), mapping= aes(Date,fill=GD1))+
            geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
          
        )
        if(input$l3=="2"){
          x<- ggplotly(
            ggplot(subdata2(), mapping= aes(Date,fill=GD1))+
              geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
          )
        }
        if(input$l3=="3"){
          x<- ggplotly(
            ggplot(subdata2(), mapping= aes(wday(Date,label = T),fill=GD1))+
              geom_bar(position = "stack")+xlab("Day")
          )
        }
        if(input$l3=="4"){
          x<- ggplotly(
            ggplot(subdata2(), mapping= aes(wday(Date,label = T),fill=GD1))+
              geom_bar(position = "dodge")+xlab("Day")
          ) 
        }
      }
      x$elementId <- NULL
      x
    })
    #To plot data hourwise - right side grade of guide plate
    output$Hour3 <- renderPlotly({
      x<- ggplotly(
        ggplot(Gsort(), mapping= aes(Hour,fill=GD2))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
        
      )
      if(input$r1=="2"){
        x<- ggplotly(
          ggplot(Gsort(), mapping= aes(Hour,fill=GD2))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$r2=="2"){
        x<- ggplotly(
          ggplot(subdata(), mapping= aes(Hour,fill=GD2))+
            geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
        if(input$r1=="2"){
          x<- ggplotly(
            ggplot(subdata(), mapping= aes(Hour,fill=GD2))+
              geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
          )
        }
      }
      if(input$r2=="3"){
        x<- ggplotly(
          ggplot(subdata1(), mapping= aes(Hour,fill=GD2))+
            geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
        if(input$r1=="2"){
          x<- ggplotly(
            ggplot(subdata1(), mapping= aes(Hour,fill=GD2))+
              geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
          )
        }
      }
      if(input$r2=="4"){
        x<- ggplotly(
          ggplot(subdata2(), mapping= aes(Hour,fill=GD2))+
            geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
        if(input$r1=="2"){
          x<- ggplotly(
            ggplot(subdata2(), mapping= aes(Hour,fill=GD2))+
              geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
          )
        }
      }
      x$elementId <- NULL
      x
    })
    #To plot right side grade of guideplate
    output$Right <- renderPlotly({
      x<- ggplotly(
        ggplot(Gsort(), mapping= aes(Date,fill=GD2))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
        
      )
      if(input$r3=="2"){
        x<- ggplotly(
          ggplot(Gsort(), mapping= aes(Date,fill=GD2))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$r3=="3"){
        x<- ggplotly(
          ggplot(Gsort(), mapping= aes(wday(Date,label = T),fill=GD2))+
            geom_bar(position = "stack")+xlab("Day")
          
        )
      }
      if(input$r3=="4"){
        x<- ggplotly(
          ggplot(Gsort(), mapping= aes(wday(Date,label = T),fill=GD2))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      }
      if(input$r4=="2"){
        x<- ggplotly(
          ggplot(subdata(), mapping= aes(Date,fill=GD2))+
            geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
        if(input$r3=="2"){
          x<- ggplotly(
            ggplot(subdata(), mapping= aes(Date,fill=GD2))+
              geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
          )
        }
        if(input$r3=="3"){
          x<- ggplotly(
            ggplot(subdata(), mapping= aes(wday(Date,label = T),fill=GD2))+
              geom_bar(position = "stack")+xlab("Day")
          )
        }
        if(input$r3=="4"){
          x<- ggplotly(
            ggplot(subdata(), mapping= aes(wday(Date,label = T),fill=GD2))+
              geom_bar(position = "dodge")+xlab("Day")
          ) 
        }
      }
      if(input$r4=="3"){
        x<- ggplotly(
          ggplot(subdata1(), mapping= aes(Date,fill=GD2))+
            geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
          
        )
        if(input$r3=="2"){
          x<- ggplotly(
            ggplot(subdata1(), mapping= aes(Date,fill=GD2))+
              geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
            
          )
        }
        if(input$r3=="3"){
          x<- ggplotly(
            ggplot(subdata1(), mapping= aes(wday(Date,label = T),fill=GD2))+
              geom_bar(position = "stack")+xlab("Day")
          )
        }
        if(input$r3=="4"){
          x<- ggplotly(
            ggplot(subdata1(), mapping= aes(wday(Date,label = T),fill=GD2))+
              geom_bar(position = "dodge")+xlab("Day")
          ) 
        }
      }
      if(input$r4=="4"){
        x<- ggplotly(
          ggplot(subdata2(), mapping= aes(Date,fill=GD2))+
            geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
        if(input$r3=="2"){
          x<- ggplotly(
            ggplot(subdata2(), mapping= aes(Date,fill=GD2))+
              geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
          )
        }
        if(input$r3=="3"){
          x<- ggplotly(
            ggplot(subdata2(), mapping= aes(wday(Date,label = T),fill=GD2))+
              geom_bar(position = "stack")+xlab("Day")
          )
        }
        if(input$r3=="4"){
          x<- ggplotly(
            ggplot(subdata2(), mapping= aes(wday(Date,label = T),fill=GD2))+
              geom_bar(position = "dodge")+xlab("Day")
            
          ) 
        }
      }
      x$elementId <- NULL
      x
    })
    #To filter data as shiftwise - Shift 1
    subdata3 <- reactive({
      Psort() %>%
        filter(
          hms(Psort()$Time)>=hms(l$start[1]),
          hms(Psort()$Time)<=hms(l$end[1])
        )
    })
    #To filter data as shiftwise - Shift 2
    subdata4 <- reactive({
      Psort() %>%
        filter(
          hms(Psort()$Time)>=hms(l$start[2]),
          hms(Psort()$Time)<=hms(l$end[2])
        )
    })
    #To filter data as shiftwise - Shift 3
    subdata5<- reactive({
      Psort() %>%
        filter(
          hms(Psort()$Time)>=hms(l$start[3]),
          hms(Psort()$Time)<=hms(l$end[3])
        ) 
    })
    #To plot pawl data hourwise
    output$Hour1 <- renderPlotly({
      #For stacked graph
      x<- ggplotly(
        ggplot(Psort(), mapping= aes(Hour,fill=GD3))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
      if(input$p1=="2"){
        #For dodge graph
        x<- ggplotly(
          ggplot(Psort(), mapping= aes(Hour,fill=GD3))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      #To access various radiobuttons 
      if(input$p2=="2"){
        x<- ggplotly(
          ggplot(subdata3(), mapping= aes(Hour,fill=GD3))+
            geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
          
        )
        if(input$p1=="2"){
          x<- ggplotly(
            ggplot(subdata3(), mapping= aes(Hour,fill=GD3))+
              geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
            
          )
        }
      }
      if(input$p2=="3"){
        x<- ggplotly(
          ggplot(subdata4(), mapping= aes(Hour,fill=GD3))+
            geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
        if(input$p1=="2"){
          x<- ggplotly(
            ggplot(subdata4(), mapping= aes(Hour,fill=GD3))+
              geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
          )
        }
      }
      if(input$p2=="4"){
        
        x<- ggplotly(
          ggplot(subdata5(), mapping= aes(Hour,fill=GD3))+
            geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
        if(input$p1=="2"){
          x<- ggplotly(
            ggplot(subdata5(), mapping= aes(Hour,fill=GD3))+
              geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
          )
        }
      }
      #To remove unwanted data
      x$elementId <- NULL
      x
    })
    #To plot pawl data datewise 
    output$Barchart1 <- renderPlotly({
      x<- ggplotly(
        ggplot(Psort(), mapping= aes(Date,fill=GD3))+
          geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
      )
      #For various radiobuttons
      if(input$p3=="2"){
        x<- ggplotly(
          ggplot(Psort(), mapping= aes(Date,fill=GD3))+
            geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
      }
      if(input$p3=="3"){
        x<- ggplotly(
          ggplot(Psort(), mapping= aes(wday(Date,label = T),fill=GD3))+
            geom_bar(position = "stack")+xlab("Day")
          
        )
      }
      if(input$p3=="4"){
        x<- ggplotly(
          ggplot(Psort(), mapping= aes(wday(Date,label = T),fill=GD3))+
            geom_bar(position = "dodge")+xlab("Day")
        ) 
      }
      if(input$p4=="2"){
        x<- ggplotly(
          ggplot(subdata3(), mapping= aes(Date,fill=GD3))+
            geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
        if(input$p3=="2"){
          x<- ggplotly(
            ggplot(subdata3(), mapping= aes(Date,fill=GD3))+
              geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
          )
        }
        if(input$p3=="3"){
          x<- ggplotly(
            ggplot(subdata3(), mapping= aes(wday(Date,label = T),fill=GD3))+
              geom_bar(position = "stack")+xlab("Day")
          )
        }
        if(input$p3=="4"){
          x<- ggplotly(
            ggplot(subdata3(), mapping= aes(wday(Date,label = T),fill=GD3))+
              geom_bar(position = "dodge")+xlab("Day")
          ) 
        }
      }
      if(input$p4=="3"){
        x<- ggplotly(
          ggplot(subdata4(), mapping= aes(Date,fill=GD3))+
            geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
        if(input$p3=="2"){
          x<- ggplotly(
            ggplot(subdata4(), mapping= aes(Date,fill=GD3))+
              geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
          )
        }
        if(input$p3=="3"){
          x<- ggplotly(
            ggplot(subdata4(), mapping= aes(wday(Date,label = T),fill=GD3))+
              geom_bar(position = "stack")+xlab("Day")
          )
        }
        if(input$p3=="4"){
          x<- ggplotly(
            ggplot(subdata4(), mapping= aes(wday(Date,label = T),fill=GD3))+
              geom_bar(position = "dodge")+xlab("Day")
          ) 
        }
      }
      if(input$p4=="4"){
        x<- ggplotly(
          ggplot(subdata5(), mapping= aes(Date,fill=GD3))+
            geom_bar(position = "stack")+theme(axis.text.x = element_text(angle = 45, size = 8))
        )
        if(input$p3=="2"){
          x<- ggplotly(
            ggplot(subdata5(), mapping= aes(Date,fill=GD3))+
              geom_bar(position = "dodge")+theme(axis.text.x = element_text(angle = 45, size = 8))
          )
        }
        if(input$p3=="3"){
          x<- ggplotly(
            ggplot(subdata5(), mapping= aes(wday(Date,label = T),fill=GD3))+
              geom_bar(position = "stack")+xlab("Day")
          )
        }
        if(input$p3=="4"){
          x<- ggplotly(
            ggplot(subdata5(), mapping= aes(wday(Date,label = T),fill=GD3))+
              geom_bar(position = "dodge")+xlab("Day")
          ) 
        }
      }
      x$elementId <- NULL
      x
    })
    #To display total count of components (Both guideplate and pawl)
    output$table <- renderTable({
      PAWL <- as.numeric(c(pgd3a(),pgd3b(),pgd3c(),pgd3r(),pgd3a()+pgd3b()+pgd3c()+pgd3r()))
      GuidePlateR <- as.numeric(c(ggd2a(),ggd2b(),ggd2c(),ggd2r(),ggd2a()+ggd2b()+ggd2c()+ggd2r()))
      GuidePlateL <- as.numeric(c(ggd1a(),ggd1b(),ggd1c(),ggd1r(),ggd1a()+ggd1b()+ggd1c()+ggd1r()))
      GuidePlate <- as.numeric(c(ggd1a()+ggd2a(),ggd1b()+ggd2b(),ggd1c()+ggd2c(),ggd1r()+ggd2r(),ggd1a()+ggd2a()+ggd1b()+ggd2b()+ggd1c()+ggd2c()+ggd1r()+ggd2r()))
      Grades = (c(c("A","B","C","R","Total"),PAWL, GuidePlateR,GuidePlateL,GuidePlate))
      a=matrix(Grades,nrow =5, ncol = 5, dimnames = list(c("A","B","C","R","Total"),c("Grade","PAWL","GuidePlate(GD2)","GuidePlate(GD1)","GuidePlate(Total)")))
      if(nrow(a)==0)
        return("No data is present")
      a
    })
    #Count of combination grade of guideplate
    output$table1 <- renderTable({
      GuidePlate <- as.numeric(c(ggd3aa(),ggd3ab(),ggd3ar(),ggd3bb(),ggd3bc(),ggd3br(),ggd3ca(),ggd3cc(),ggd3cr(),ggd3r(),ggd3aa()+ggd3ab()+ggd3ar()+ggd3bb()+ggd3bc()+ggd3br()+ggd3ca()+ggd3cc()+ggd3cr()+ggd3r()))
      Grades =c(c("AA","AB","AR","BB","BC","BR","CA","CC","CR","R","Total"),GuidePlate)
      a= matrix(Grades,nrow= 11, ncol=2, dimnames=list(c("AA","AB","AR","BB","BC","BR","CA","CC","CR","R","Total"),c("Grade","GuidePlate(Total Grade)")))
     if(nrow(a)==0)
       return("No data is present")
       a
    })
    #To view pdf file (Help)
    observeEvent(input$generate, {
      output$pdfview <- renderUI({
        tags$iframe(style="height:600px; width:100%", src="R IN ACTION.pdf")
      })
    })
    
    
    #To display accepted and rejected parts as a table
    output$table2 <- renderTable({
      Parts <- as.numeric(c(guidep(),pawlp(),totalp()))
      Rej <- as.numeric(c(guider(),pawlr(),totalr()))
      total <- as.numeric(c(guidep()+guider(),pawlp()+pawlr(),totalp()+totalr()))
      Grades =c(c("Guide Plate","PAWL","Total"),Parts,Rej,total)
      a= matrix(Grades,nrow= 3, ncol=4, dimnames=list(c("Guide Plate","PAWL","Total"),c("Part Name","Accepted","Rejected","Total")))
      if(nrow(a)==0)
        return("No data is present")
      a
    })
    #To diplay master data of Guide Plate
    output$table3 <- renderTable({
      from <- as.numeric(c(b[1:1,2:2],b[3:3,2:2],b[5:5,2:2]))
      to <- as.numeric(c(b[2:2,2:2],b[4:4,2:2],b[6:6,2:2]))
      Grades =c(c("A","B","C"),from,to)
      a= matrix(Grades,nrow= 3, ncol=3, dimnames=list(c("A","B","C"),c("Grade","From","To")))
      if(nrow(a)==0)
        return("No data is present")
      a
    }) 
    #To diplay master data of PAWL
    output$table4 <- renderTable({
      from <- as.numeric(c(b[7:7,2:2],b[9:9,2:2],b[11:11,2:2]))
      to <- as.numeric(c(b[8:8,2:2],b[10:10,2:2],b[12:12,2:2]))
      Grades =c(c("A","B","C"),from,to)
      a= matrix(Grades,nrow= 3, ncol=3, dimnames=list(c("A","B","C"),c("Grade","From","To")))
      if(nrow(a)==0)
        return("No data is present")
      a
    })
    #Boxplot of left side width of guideplate
    output$Graph <- renderPlotly({
      x<-ggplotly(
        ggplot(Gsort(), mapping = aes(x=Hour,y=WD1))+geom_boxplot()+ylim(23,24)
      )
      x
    })
    #Boxplot of right side width of guideplate
    output$Graph1 <- renderPlotly({
      x<-ggplotly(
        ggplot(Gsort(), mapping = aes(x=Hour,y=WD2))+geom_boxplot()+ylim(23,24)
      )
      x
    })
    #Boxplot of width of PAWL
    output$Graph2 <- renderPlotly({
      x<-ggplotly(
        ggplot(Psort(), mapping = aes(x=Hour,y=WD1))+geom_boxplot()+ylim(23,24)
      )
      x
    })
    #To plot bubble chart (Guide plate)
    output$Bubble1 <- renderBubbles({
      if (nrow(Gsort()) == 0)
        return()
      
      order <- unique(Gsort()$GD3)
      df <- Gsort() %>%
        group_by(GD3) %>%
        tally() %>%
        arrange(desc(n), tolower(GD3)) %>%
        head(12)
      bubbles(df$n, df$GD3, key = df$GD3)
    })
    #To diplay count and percentage of grades (Guide Plate)
    output$Real1 <- renderTable({
      if(nrow(Gsort())==0)
        return("No data is present")
      Gsort() %>%
        group_by(GD3) %>%
        tally() %>%
        arrange(desc(n), tolower(GD3)) %>%
        mutate(percentage = n / nrow(Gsort()) * 100) %>%
        mutate(count = n) %>%
        select("Grade" = GD3, "Count"= n,"% of Grade" = percentage) %>%
        as.data.frame() %>%
        head(15)
    }, digits = 1)
    #To plot bubble chart (PAWL)
    output$Bubble2 <- renderBubbles({
      if (nrow(Psort()) == 0)
        return()
      
      order <- unique(Psort()$GD3)
      df <- Psort() %>%
        group_by(GD3) %>%
        tally() %>%
        arrange(desc(n), tolower(GD3)) %>%
        head(12)
      bubbles(df$n, df$GD3, key = df$GD3)
    })
    #To diplay count and percentage of grades (PAWL)
    output$Real2 <- renderTable({
      Psort() %>%
        group_by(GD3) %>%
        tally() %>%
        arrange(desc(n), tolower(GD3)) %>%
        mutate(percentage = n / nrow(Psort()) * 100) %>%
        mutate(count = n) %>%
        select("Grade" = GD3, "Count"= n,"% of Grade" = percentage) %>%
        as.data.frame() %>%
        head(15)
    }, digits = 1)
    #To display count for graphs as radiobuttons varying
    output$tbl1 <- renderTable({
      if(input$radio2=="1"){
        count <- as.numeric(c(ggd1a()+ggd2a(),ggd1b()+ggd2b(),ggd1c()+ggd2c(),ggd1r()+ggd2r(),ggd1a()+ggd2a()+ggd1b()+ggd2b()+ggd1c()+ggd2c()+ggd1r()+ggd2r()))
        Grades =c(c("A","B","C","R","Total"),count)
        a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
        if(nrow(a)==0)
          return("No data is present")
        a
      }
      if(input$radio2=="2"){
        if(nrow(subdata())==0)
          return("No data is present")
        #To count no.of parts
        a1=0
        b1=0
        c1=0
        r1=0
        a2=0
        b2=0
        c2=0
        r2=0
        #Count in shift 1 (Guide Plate)
        for(i in 1:nrow(subdata())){
          if(subdata()$GD1[i]=="A")
            a1=a1+1
          if(subdata()$GD1[i]=="B")
            b1=b1+1
          if(subdata()$GD1[i]=="C")
            c1=c1+1
          if(subdata()$GD1[i]=="R")
            r1=r1+1
          if(subdata()$GD2[i]=="A")
            a2=a2+1
          if(subdata()$GD2[i]=="B")
            b2=b2+1
          if(subdata()$GD2[i]=="C")
            c2=c2+1
          if(subdata()$GD2[i]=="R")
            r2=r2+1
        }
        #Represent in table
        count <- as.numeric(c(a1+a2,b1+b2,c1+c2,r1+r2,a1+a2+b1+b2+c1+c2+r1+r2))
        Grades =c(c("A","B","C","R","Total"),count)
        a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
        if(nrow(a)==0)
          return("No data is present")
        a
      }
      if(input$radio2=="3"){
        if(nrow(subdata1())==0)
          return("No data is present")
        a1=0
        b1=0
        c1=0
        r1=0
        a2=0
        b2=0
        c2=0
        r2=0
        #Shift 2 (Guide Plate)
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
        if(nrow(a)==0)
          return("No data is present")
        a
      }
      if(input$radio2=="4"){
        if(nrow(subdata2())==0)
          return("No data is present")
        a1=0
        b1=0
        c1=0
        r1=0
        a2=0
        b2=0
        c2=0
        r2=0
        #For Shift3 (Guide Plate)
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
      if(nrow(a)==0)
        return("No data is present")
      a
    })
    output$tbl2 <- renderTable({
      if(input$p2=="1"){
        count <- as.numeric(c(pgd3a(),pgd3b(),pgd3c(),pgd3r(),pgd3a()+pgd3b()+pgd3c()+pgd3r()))
        Grades =c(c("A","B","C","R","Total"),count)
        a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count"))) 
        if((a)==0)
          return("No data is present")
    a
      }
      if(input$p2=="2"){
        if(nrow(subdata3())==0)
          return("No data is present")
        a=0
        b=0
        c=0
        r=0
        #Shift 1 PAWL
        for(i in 1:nrow(subdata3())){
          if(subdata3()$GD3[i]=="A")
            a=a+1
          if(subdata3()$GD3[i]=="B")
            b=b+1
          if(subdata3()$GD3[i]=="C")
            c=c+1
          if(subdata3()$GD3[i]=="R")
            r=r+1
        }
        count <- as.numeric(c(a,b,c,r,a+b+c+r))
        Grades =c(c("A","B","C","R","Total"),count)
        a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
        if(nrow(a)==0)
          return("No data is present")
        a
      }
      if(input$p2=="3"){
        if(nrow(subdata4())==0)
          return("No data is present")
        a=0
        b=0
        c=0
        r=0
        #Shift 2 PAWL
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
        if(nrow(a)==0)
          return("No data is present")
        a
      }
      if(input$p2=="4"){
        if(nrow(subdata5())==0)
          return("No data is present")
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
      if(nrow(a)==0)
        return("No data is present")
      a
    }) 
    output$tbl3 <- renderTable({
      if(input$l2=="1"){
        count <- as.numeric(c(ggd1a(),ggd1b(),ggd1c(),ggd1r(),ggd1a()+ggd1b()+ggd1c()+ggd1r()))
        Grades =c(c("A","B","C","R","Total"),count)
        a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count"))) 
        if(a==0)
          return("No data is present")
    a
      }
      if(input$l2=="2"){
        if(nrow(subdata())==0)
          return("No data is present")
        a=0
        b=0
        c=0
        r=0
        #Shift 1 - Guide plate left side grade
        for(i in 1:nrow(subdata())){
          if(subdata()$GD1[i]=="A")
            a=a+1
          if(subdata()$GD1[i]=="B")
            b=b+1
          if(subdata()$GD1[i]=="C")
            c=c+1
          if(subdata()$GD1[i]=="R")
            r=r+1
        }
        count <- as.numeric(c(a,b,c,r,a+b+c+r))
        Grades =c(c("A","B","C","R","Total"),count)
        a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
        if(nrow(a)==0)
          return("No data is present")
        a
      }
      if(input$l2=="3"){
        if(nrow(subdata1())==0)
          return("No data is present")
        a=0
        b=0
        c=0
        r=0
        #Shift 2 - Guide plate left side grade
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
        if(nrow(a)==0)
          return("No data is present")
        a
      }
      if(input$l2=="4"){
        if(nrow(subdata2())==0)
          return("No data is present")
        a=0
        b=0
        c=0
        r=0
        #Shift 3 - Guide plate left side grade
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
      if(nrow(a)==0)
        return("No data is present")
      a
    })
    output$tbl4 <- renderTable({
      if(input$r2=="1"){
        count <- as.numeric(c(ggd2a(),ggd2b(),ggd2c(),ggd2r(),ggd2a()+ggd2b()+ggd2c()+ggd2r()))
        Grades =c(c("A","B","C","R","Total"),count)
        a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count"))) 
        if(nrow(a)==0)
          return("No data is present")
      a
      }
      if(input$r2=="2"){
         if(nrow(subdata())==0)
          return("No data is present")
        a=0
        b=0
        c=0
        r=0
        #Shift 1 - Guide plate right side grade
        for(i in 1:nrow(subdata())){
          if(subdata()$GD2[i]=="A")
            a=a+1
          if(subdata()$GD2[i]=="B")
            b=b+1
          if(subdata()$GD2[i]=="C")
            c=c+1
          if(subdata()$GD2[i]=="R")
            r=r+1
        }
        count <- as.numeric(c(a,b,c,r,a+b+c+r))
        Grades =c(c("A","B","C","R","Total"),count)
        a= matrix(Grades,nrow= 5, ncol=2, dimnames=list(c("A","B","C","R","Total"),c("Grade","Count")))
        if(nrow(a)==0)
          return("No data is present")
        a
      }
      if(input$r2=="3"){
          if(nrow(subdata1())==0)
          return("No data is present")
        a=0
        b=0
        c=0
        r=0
        #Shift 2 - Guide plate right side grade
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
        if(nrow(a)==0)
          return("No data is present")
        a
      }
      if(input$r2=="4"){
        if(nrow(subdata2())==0)
          return("No data is present")
        a=0
        b=0
        c=0
        r=0
        #Shift 3 - Guide plate right side grade
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
      if(nrow(a)==0)
        return("No data is present")
      a
    })
    #To print raw data (Guide Plate)
    output$rawtable1 <- renderPrint({
      orig <- options(width = 10000)
      print(tail(Gsort(), input$maxrows1))
      options(orig)
    })
    #To print shift timings
    output$shifttable<-renderTable({
      if(nrow(l)==0)
        return("Shift timings text file is not present")
      l
    })
    #To download data as CSV file (Guide Plate)
    output$downloadCsv1 <- downloadHandler(
      filename = "Guide Plate.csv",
      content = function(file) {
        write.csv(Gsort(), file)
      },
      contentType = "text/csv"
    )
  })
     
