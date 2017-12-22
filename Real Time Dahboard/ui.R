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
  
        #User Interface Start from here
        shinyUI(
          dashboardPage(skin = "green",
                        dashboardHeader(title = "Real Time Analysis",
                                        #Static notifications
                                        dropdownMenu(type="notifications", badgeStatus = "warning",
                                                     notificationItem(icon = icon("warning"), status = "info",
                                                                      "Batch Transfermation Happened")
                                        ),
                                        #Static Messages
                                        dropdownMenu(type = "messages", badgeStatus = "info",
                                                     messageItem(from = "Measuring Department", message = "Master detected", icon=icon("bookmark"))
                                        ),
                                        #Static Tasks
                                        dropdownMenu(type="tasks", badgeStatus = "info",
                                                     taskItem(value = 30, color = "red", "Amount of R" )
                                        )
                        ),
                        #To put things in sidebar
                        dashboardSidebar(
                          sidebarMenu(
                            #Tab 1 - Dashboard
                            menuItem("Dashboard", tabName = "dashboard",icon = icon("dashboard")),
                            menuItem("TI Production",tabName = "ti",icon = icon("list"),
                                     menuSubItem("GuidePlate", tabName = "guide"),
                                     menuSubItem("PAWL", tabName = "pawl")),
                            menuItem("Guide Plate",icon = icon("bullseye"),
                                     menuSubItem("Guide Plate (GD1)",tabName = "gd1"),
                                     menuSubItem("Guide Plate (GD2)",tabName = "gd2")),
                            menuItem("Summary",tabName = "ti-s",icon = icon("th-list"),
                                     menuSubItem("GuidePlate", tabName = "guide-1"),
                                     menuSubItem("PAWL", tabName = "pawl-1"),
                                     menuSubItem("Overview",tabName = "o")),
                            menuItem("Master Data", tabName = "m", icon = icon("podcast")),
                            menuItem("Raw Data",tabName = "rd",icon = icon("hand-o-right")),
                            menuItem("Help",tabName = "h",icon = icon("question-circle")),
                            menuItem("Production Dashboard", tabName = "pd", icon = icon("info"))
                          )),
                        #Dahboard body manipulations and representation
                        dashboardBody(
                          tabItems(
                            tabItem(tabName = "dashboard",
                                    tabsetPanel(
                                      #Guide Plate
                                      tabPanel(title = "Guide Plate",icon = icon("user"),box(title = "Guide Plate",width = 8, status = "info",solidHeader = TRUE,collapsible = TRUE, bubblesOutput("Bubble1",width = "100%",height = 500)),
                                               box(title = "Guide Plate ",width = 4, status = "info",solidHeader = TRUE,collapsible = TRUE,style = "font-size: 100%; width: 100%", tableOutput("Real1")),
                                               box(title = "Parts Produced",width = 4, status = "info",solidHeader = TRUE,collapsible = TRUE,style = "font-size: 100%; width: 100%",tableOutput("table2"))),
                                      #PAWL
                                      tabPanel(title = "PAWL",icon = icon("user"),box(title = "PAWL",width = 8, status = "info",solidHeader = TRUE,collapsible = TRUE, bubblesOutput("Bubble2",width = "100%",height = 500)),
                                               box(title = "PAWL",width = 4, status = "info",solidHeader = TRUE,collapsible = TRUE,style = "font-size: 100%; width: 100%", tableOutput("Real2")))
                                      
                                    )),
                            #Tab 2 - Guide Plate & PAWL (Production of company)
                            tabItem(tabName = "guide",
                                    tabsetPanel(
                                      #Guide Plate hourwise data
                                      tabPanel("Guide Plate Data (Hourly)",radioButtons("radio","Type of Barchart:",choices = c("Stack"="1","Dodge"="2"),inline = TRUE),
                                               radioButtons("radio2","Shift data By Hourwise:",choices =c("All Shifts"="1","Shift 1"="2","Shift 2"="3","Shift 3"="4"),inline = TRUE),
                                               box(title = "Guide Plate - H",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE,column(8,plotlyOutput("Hour",height = 500)),column(4,tableOutput("tbl1")))),
                                      tabPanel("Guide Plate Data(Daily)", radioButtons("g1","Type of Barchart:",choices = c("Stack"="1","Dodge"="2","Week Days(Stack)"="3","Week Days(Dodge)"="4"),inline = TRUE),
                                               radioButtons("g2","Choose Shift:", choices = c("All Shifts"="1","Shift 1"="2","Shift 2"="3","Shift 3"="4"),inline = TRUE),
                                               box(title = "Guide Plate - D",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE, plotlyOutput("Barchart",width = "100%",height = 500)))
                                    )),
                            #PAWL
                            tabItem(tabName = "pawl",
                                    tabsetPanel(
                                      tabPanel("PAWL Data (Hourly)",radioButtons("p1","Type of Barchart:",choices = c("Stack"="1","Dodge"="2"),inline = TRUE),
                                               radioButtons("p2","Shift data By Hourwise:",choices =c("All Shifts"="1","Shift 1"="2","Shift 2"="3","Shift 3"="4"),inline = TRUE),
                                               box(title = "PAWL (Hourly)",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE, column(8,plotlyOutput("Hour1",height = 500)),column(4,tableOutput("tbl2")))),
                                      #Guide Plate Daywise data
                                      tabPanel("PAWL Data (Daily)",radioButtons("p3","Type of Barchart:",choices = c("Stack"="1","Dodge"="2","Week Days(Stack)"="3","Week Days(Dodge)"="4"),inline = TRUE),
                                               radioButtons("p4","Choose Shift:", choices = c("All Shifts"="1","Shift 1"="2","Shift 2"="3","Shift 3"="4"),inline = TRUE),
                                               box(title = "PAWL - D",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE, plotlyOutput("Barchart1",width = "100%",height = 500)))
                                    )),
                            
                            #Tab 3 - Guide Plate
                            #Left side grade - GD1
                            tabItem(tabName = "gd1",
                                    tabsetPanel(
                                      #GD1 hourwise data
                                      tabPanel("Guide Plate - Left (Hourly)",radioButtons("l1","Type of Barchart:",choices = c("Stack"="1","Dodge"="2"),inline = TRUE),
                                               radioButtons("l2","Shift data By Hourwise:",choices =c("All Shifts"="1","Shift 1"="2","Shift 2"="3","Shift 3"="4"),inline = TRUE),
                                               box(title = "Guide Plate-GD1 - H",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE,column(8,plotlyOutput("Hour2",height = 500)),column(4,tableOutput("tbl3")))),
                                      #GD1 daywise data
                                      tabPanel("Guide Plate - Left (Daily)",radioButtons("l3","Type of Barchart:",choices = c("Stack"="1","Dodge"="2","Week Days(Stack)"="3","Week Days(Dodge)"="4"),inline = TRUE),
                                               radioButtons("l4","Choose Shift:", choices = c("All Shifts"="1","Shift 1"="2","Shift 2"="3","Shift 3"="4"),inline = TRUE),
                                               box(title = "Guide Plate-GD1 - D",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE, plotlyOutput("Left",width = "100%",height = 500)))
                                    )),
                            #Rightside grade - GD2 
                            tabItem(tabName = "gd2",
                                    tabsetPanel(
                                      #GD2 hourwise data
                                      tabPanel("Guide Plate - Right (Hourly)",radioButtons("r1","Type of Barchart:",choices = c("Stack"="1","Dodge"="2"),inline = TRUE),
                                               radioButtons("r2","Shift data By Hourwise:",choices =c("All Shifts"="1","Shift 1"="2","Shift 2"="3","Shift 3"="4"),inline = TRUE),
                                               box(title = "Guide Plate-GD2 - H",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE, column(8,plotlyOutput("Hour3",height = 500)),column(4,tableOutput("tbl4")))),
                                      #GD2 daywise data
                                      tabPanel("Guide Plate - Right (Daily)",radioButtons("r3","Type of Barchart:",choices = c("Stack"="1","Dodge"="2","Week Days(Stack)"="3","Week Days(Dodge)"="4"),inline = TRUE),
                                               radioButtons("r4","Choose Shift:", choices = c("All Shifts"="1","Shift 1"="2","Shift 2"="3","Shift 3"="4"),inline = TRUE),
                                               box(title = "Guide Plate-GD2 - D",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE, plotlyOutput("Right",width = "100%",height = 500)))
                                    )),
                            #Tab 4 - Summary
                            #Guide Plate
                            tabItem(tabName = "guide-1",box(title = "Guide Plate (WD1)",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE, plotlyOutput("Graph",width = "100%",height = 500)),
                                    box(title = "Guide Plate (WD2)",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE, plotlyOutput("Graph1",width = "100%",height = 500))),
                            #PAWL
                            tabItem(tabName = "pawl-1",box(title = "PAWl (WD1)",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE, plotlyOutput("Graph2",width = "100%",height = 500))),
                            #Overall count 
                            tabItem(tabName = "o",box(title = "Guide Plate (GD1 & GD2)",width = 6, status = "info",solidHeader = TRUE,collapsible = TRUE,style = "font-size: 100%; width: 100%",tableOutput("table")),
                                    box(title = "Guide Plate (Combinations)",width = 3,status = "info",solidHeader = TRUE,collapsible = TRUE,style = "font-size: 100%; width: 100%", tableOutput("table1"))),
                            #Tab 8 - Help (Instructions to operate dashboard)
                            tabItem(tabName = "h",actionButton("generate", "Generate PDF"),uiOutput("pdfview")),
                            #Tab 9 - Production Dashboard
                            tabItem(tabName = "pd",helpText(   a("Click Here for Production Dashboard",     href="https://tiproject.shinyapps.io/PDashboard/")
                            )),
                            #Tab 5 - Master data (Guide Pate & PAWL)
                            tabItem(tabName = "m",box(title = "Guide Plate (Tolerance Band)",width = 4,style = "font-size: 100%; width: 100%", status = "info",solidHeader = TRUE,collapsible = TRUE,tableOutput("table3")),
                                    box(title = "PAWL (Tolerance Band)",width = 4,style = "font-size: 100%; width: 100%", status = "info",solidHeader = TRUE,collapsible = TRUE,tableOutput("table4"),valueBoxOutput("x")),box(title = "Shift timings",width = 4,style = "font-size: 100%; width: 100%", status = "info",solidHeader = TRUE,collapsible = TRUE,tableOutput("shifttable"))),
                            #Tab 6 - Raw data (View & Download)
                            tabItem(tabName = "rd",
                                    tabsetPanel(
                                      tabPanel("Guide Plate",icon = icon("user"),numericInput("maxrows1", "Rows to show", 25),box(title = "Guide Plate",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE, verbatimTextOutput("rawtable1")),downloadButton("downloadCsv1", "Download as CSV")),
                                      tabPanel("PAWL",icon = icon("user-o"),numericInput("maxrows2", "Rows to show", 25),box(title = "PAWL",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE, verbatimTextOutput("rawtable2")),downloadButton("downloadCsv2", "Download as CSV")))
                            )
                          )
                        )
          )
        )
      
      
      
