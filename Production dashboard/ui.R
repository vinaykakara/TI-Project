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
  #User Interface will begin here
  shinyUI(
    #To fix dashboard page
    dashboardPage(skin = "green",
                  #Dashboard Header
                  dashboardHeader(title = "Production Dashboard",
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
                  #Dashboard Sidebar
                  dashboardSidebar(
                    #To maintain continuity between tabitems 
                    sidebarMenu(
                      #Tab1 - Dashboard
                      menuItem("Dashboard",tabName = "dashboard",icon = icon("dashboard")),
                      #Tab2 - Production Planned (GuidePlate & PAWL)
                      menuItem("Production Planned", icon = icon("podcast"),
                               menuSubItem("Guide Plate",tabName = "guide"),
                               menuSubItem("PAWL",tabName = "pawl")),
                      #Tab3 - Production Planned (GuidePlate & PAWl)
                      menuItem("TI Production",icon = icon("list"),
                               menuSubItem("Guide Plate",tabName = "guide1"),
                               menuSubItem("PAWL",tabName = "pawl1")),
                      #Tab4 - Parts Produced (GuidePlate & PAWL)
                      menuItem("Parts Produced", icon = icon("user"),
                               menuSubItem("Guide Plate", tabName = "guide2"),
                               menuSubItem("PAWL",tabName = "pawl2")),
                      #Tab5 - Planned Vs Actual (GuidePlate & PAWL)
                      menuItem("Planned vs Actual",icon = icon("bars"),
                               menuSubItem("Guide Plate",tabName = "guide3"),
                               menuSubItem("PAWL",tabName = "pawl3")),
                      #Tab6 - Help (Instructions to operate dashboards)
                      menuItem("Help",tabName = "h",icon = icon("question-circle"))
                    )
                  ),
                  #Dashboard Bosy
                  dashboardBody(
                    tabItems(
                      tabItem(tabName = "dashboard",box(title = "Parts Produced in Month",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE, dataTableOutput("table2")),verbatimTextOutput("text2")),
                      tabItem(tabName = "guide", box(title = "Guide Plate",width = 14,status = "info",solidHeader = T,collapsible = T, h4(textOutput("guidetext1")),
                                                     dateInput("guidedate1", label=NA, value = NULL, min = NULL, max = NULL,
                                                               format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                                               language = "en", width = 150),
                                                     dateInput("guidedate2", label=NA, value = NULL, min = NULL, max = NULL,
                                                               format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                                               language = "en", width = 150),
                                                     #Indication to perform a task
                                                     actionButton("guidego", "Save"),
                                                     actionButton("guidedatesave","Change Datewise"),
                                                     #Indication to perform a task
                                                     actionButton("guiderefresh", "Refresh data")),
                              #Plot
                              box(title = "Guide Plate",width = 14,status = "info",solidHeader = T,collapsible = T,dataTableOutput("guidetable"))),
                      tabItem(tabName = "pawl", box(title = "PAWL",width = 14,status = "info",solidHeader = T,collapsible = T,h4(textOutput("pawltext1")),
                                                    dateInput("pawldate1", label=NA, value = NULL, min = NULL, max = NULL,
                                                              format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                                              language = "en", width = 150),
                                                    dateInput("pawldate2", label=NA, value = NULL, min = NULL, max = NULL,
                                                              format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                                              language = "en", width = 150),
                                                    actionButton("pawlgo", "Save"),
                                                    actionButton("pawldatesave",'Change datewise'),
                                                    actionButton("pawlrefresh", "Refresh data")),
                              box(title = "PAWL",width = 14,status = "info",solidHeader = T,collapsible = T,dataTableOutput("pawltable"))),
                      tabItem(tabName = "guide1",h1("Guide Plate Production Performance"),actionButton("guidedevrefresh", "Refresh data"),box(title = "Guide Plate",width = 14,status = "info",solidHeader = TRUE,collapsible = TRUE,dataTableOutput("table"))),
                      tabItem(tabName = "pawl1",h1("PAWL Production Performance"),actionButton("pawldevrefresh", "Refresh data"),box(title = "PAWL",width = 14,status = "info",solidHeader = TRUE,collapsible = TRUE,dataTableOutput("table1"))),
                      tabItem(tabName = "guide2",textOutput("partsdate"),dateInput("guideparts", label=NA, value = NULL, min = NULL, max = NULL,format = "yyyy-mm-dd", startview = "month", weekstart = 0,language = "en", width = NULL),
                              box(title ="Guide Plate",width = 12,status = "info",solidHeader = TRUE,collapsible = TRUE,valueBoxOutput("vbox"),valueBoxOutput("vbox1"),valueBoxOutput("vbox2"),infoBoxOutput("ibox"))),
                      tabItem(tabName = "pawl2",textOutput("partsdate1"),dateInput("pawlparts", label=NA, value = NULL, min = NULL, max = NULL,format = "yyyy-mm-dd", startview = "month", weekstart = 0,language = "en", width = NULL),
                              box(title ="PAWL",width = 12,status = "info",solidHeader = TRUE,collapsible = TRUE,valueBoxOutput("vbox3"),valueBoxOutput("vbox4"),valueBoxOutput("vbox5"),infoBoxOutput("ibox1"))),
                      tabItem(tabName = "guide3",box(title = "Guide Plate",width = 14,status = "info",solidHeader = TRUE,collapsible = TRUE,plotlyOutput("plot",width = "100%",height = 500))),
                      tabItem(tabName = "pawl3",box(title = "PAWL",width = 14,status = "info",solidHeader = TRUE,collapsible = TRUE,plotlyOutput("plot2",width = "100%",height = 500))),
                      tabItem(tabName = "h",actionButton("generate", "Generate PDF"),uiOutput("pdfview"))
                    )
                  )
    ))
