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
                              box(title = "Parts Produced",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE, dataTableOutput("table2"))),
                      #Tab2 - Company Production (GuidePlate & PAWL)
                      #GuidePlate
                      tabItem(tabName = "guide",
                              tabsetPanel(
                                #GuidePlate daterange 
                                tabPanel(title = "Guide Plate Shiftwise", icon = icon("user"),
                                         dateRangeInput("date_range1", label=h3("Date Range For Guide Plate")),
                                         h4(helpText("Sarting Date is(GP):"),htmlOutput("text1")),
                                         h4(helpText("Ending Date is(GP):"),htmlOutput("text2")),
                                         radioButtons("gd","Type of Barchart:",choices = c("Stack"="1","Dodge"="2","Week Days(Stack)"="3","Week Days(Dodge)"="4"),inline = TRUE),
                                         radioButtons("gd1","Choose Shift:", choices = c("All Shifts"="1","Shift 1"="2","Shift 2"="3","Shift 3"="4"),inline = TRUE),
                                         box(title = "Guide Plate",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE,column(8,plotlyOutput("plot1",height = 500)),column(4,dataTableOutput("tbl2")))),
                                
                                #GuidePlate datewise
                                tabPanel(title = "Guide Plate",icon = icon("user"),
                                         radioButtons("radio","Type of Barchart:",choices = c("Stack"="1","Dodge"="2","Week Days(Stack)"="3","Week Days(Dodge)"="4"),inline = TRUE),
                                         radioButtons("radio1","Choose Visualization Period:",choices = c("One Day"="2","One Week"="3","One Month"="4"),inline = TRUE),
                                         box(title = "Guide Plate",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE, column(8,plotlyOutput("Barchart",height = 500)),column(4,dataTableOutput("tbl1"))))
                                  )),
                      #PAWL
                      tabItem(tabName = "pawl",
                              tabsetPanel(
                                
                                #Daterange
                                tabPanel(title = "PAWL Shiftwise", icon = icon("user-o"),
                                         dateRangeInput("date_range2", label=h3("Date Range For PAWL")),
                                         h4(helpText("Sarting Date is(P):"),htmlOutput("text3")),
                                         h4(helpText("Ending Date is(P):"),htmlOutput("text4")),
                                         radioButtons("pd","Type of Barchart:",choices = c("Stack"="1","Dodge"="2","Week Days(Stack)"="3","Week Days(Dodge)"="4"),inline = TRUE),
                                         radioButtons("pd1","Choose Shift:", choices = c("Shift 1"="2","Shift 2"="3","Shift 3"="4"),inline = TRUE),
                                         box(title = "PAWL",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE,column(8,plotlyOutput("plot2",height = 500)),column(4,dataTableOutput("tbl4")))
                                ),
                                #Datewise
                                tabPanel(title = "PAWL",icon = icon("user-o"),
                                         radioButtons("p1","Type of Barchart:",choices = c("Stack"="1","Dodge"="2","Week Days(Stack)"="3","Week Days(Dodge)"="4"),inline = TRUE),
                                         radioButtons("p2","Choose Visualization Period:",choices = c("One Day"="2","One Week"="3","One Month"="4"),inline = TRUE),
                                         box(title = "PAWL",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE,column(8,plotlyOutput("Barchart1",height = 500)),column(4,dataTableOutput("tbl3")))
                                )
                              )),
                      #Tab3 - GuidePlate
                      #GD1
                      tabItem(tabName = "gd1",
                              tabsetPanel(
                                #Daterange
                                tabPanel(title = "Guide Plate Shiftwise (GD1)", icon = icon("user"),
                                         dateRangeInput("date_range3", label=h3("Date Range For Guide Plate")),
                                         h4(helpText("Sarting Date is(GP):"),htmlOutput("text5")),
                                         h4(helpText("Ending Date is(GP):"),htmlOutput("text6")),
                                         radioButtons("d","Type of Barchart:",choices = c("Stack"="1","Dodge"="2","Week Days(Stack)"="3","Week Days(Dodge)"="4"),inline = TRUE),
                                         radioButtons("d1","Choose Shift:", choices = c("All Shifts"="1","Shift 1"="2","Shift 2"="3","Shift 3"="4"),inline = TRUE),
                                         box(title = "Guide Plate - GD1",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE,column(8,plotlyOutput("plot3",height = 500)),column(4,dataTableOutput("tbl6")))
                                ),
                                #Datewise
                                tabPanel(title = "Guide Plate (GD1)",icon = icon("user"),
                                         radioButtons("x1","Type of Barchart:",choices = c("Stack"="1","Dodge"="2","Week Days(Stack)"="3","Week Days(Dodge)"="4"),inline = TRUE),
                                         radioButtons("x2","Choose Visualization Period:",choices = c("One Day"="2","One Week"="3","One Month"="4"),inline = TRUE),
                                         box(title = "Guide Plate - GD1",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE,column(8,plotlyOutput("Barchart2",height = 500)),column(4,dataTableOutput("tbl5"))))
                                
                              )),
                      #GD2
                      tabItem(tabName = "gd2",
                              tabsetPanel(
                                #Daterange
                                tabPanel(title = "Guide Plate Shiftwise (GD2)", icon = icon("user"),
                                         dateRangeInput("date_range4", label=h3("Date Range For Guide Plate")),
                                         h4(helpText("Sarting Date is(P):"),htmlOutput("text7")),
                                         h4(helpText("Ending Date is(P):"),htmlOutput("text8")),
                                         radioButtons("g","Type of Barchart:",choices = c("Stack"="1","Dodge"="2","Week Days(Stack)"="3","Week Days(Dodge)"="4"),inline = TRUE),
                                         radioButtons("g1","Choose Shift:", choices = c("All Shifts"="1","Shift 1"="2","Shift 2"="3","Shift 3"="4"),inline = TRUE),
                                         box(title = "Guide Plate - GD2",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE,column(8,plotlyOutput("plot4",height = 500)),column(4,dataTableOutput("tbl8")))
                                ),
                                #Datewise
                                tabPanel(title = "Guide Plate (GD2)",icon = icon("user"),
                                         radioButtons("y1","Type of Barchart:",choices = c("Stack"="1","Dodge"="2","Week Days(Stack)"="3","Week Days(Dodge)"="4"),inline = TRUE),
                                         radioButtons("y2","Choose Visualization Period:",choices = c("One Day"="2","One Week"="3","One Month"="4"),inline = TRUE),
                                         box(title = "Guide Plate-GD2",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE,column(8,plotlyOutput("Barchart3",height = 500)),column(4,dataTableOutput("tbl7"))))
                                
                              )),
                      #Tab4 - Summary
                      tabItem(tabName = "ti-s",box(title = "Production Summary",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE, dataTableOutput("table")),
                              box(title = "Production Summary",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE, dataTableOutput("table1"))
                      ),
                      #Tab5 - Help(Instructions to operate dashboard)
                      tabItem(tabName = "h",actionButton("generate", "Generate PDF"),uiOutput("pdfview"))
                    )
                  )
    ))
  
