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
library(readxl)
shinyUI(
  dashboardPage(skin = "green",
                dashboardHeader(title = "Production Dashboard",
                                dropdownMenu(type="notifications", badgeStatus = "warning",
                                             notificationItem(icon = icon("warning"), status = "info",
                                                              "Batch Transfermation Happened")
                                ),
                                dropdownMenu(type = "messages", badgeStatus = "info",
                                             messageItem(from = "Measuring Department", message = "Master detected", icon=icon("bookmark"))
                                ),
                                dropdownMenu(type="tasks", badgeStatus = "info",
                                             taskItem(value = 30, color = "red", "Amount of R" )
                                )
                ),
                dashboardSidebar(
                  sidebarMenu(
                    menuItem("Dashboard",tabName = "dashboard",icon = icon("dashboard")),
                    menuItem("Production Planned", icon = icon("podcast"),
                             menuSubItem("Guide Plate",tabName = "guide"),
                             menuSubItem("PAWL",tabName = "pawl")),
                    menuItem("TI Production",icon = icon("list"),
                             menuSubItem("Guide Plate",tabName = "guide1"),
                             menuSubItem("PAWL",tabName = "pawl1")),
                  menuItem("Parts Produced", icon = icon("user"),
                           menuSubItem("Guide Plate", tabName = "guide2"),
                           menuSubItem("PAWL",tabName = "pawl2")),
                  menuItem("Planned vs Actual",icon = icon("bars"),
                           menuSubItem("Guide Plate",tabName = "guide3"),
                           menuSubItem("PAWL",tabName = "pawl3")),
                  menuItem("Help",tabName = "h",icon = icon("question-circle"))
                  )
                ),
                dashboardBody(
                  tabItems(
                    tabItem(tabName = "dashboard",box(title = "Parts Produced in Week",width = 14, status = "info",solidHeader = TRUE,collapsible = TRUE, dataTableOutput("table2")),verbatimTextOutput("text2")),
                    tabItem(tabName = "guide", box(title = "Guide Plate",width = 14,status = "warning",solidHeader = T,collapsible = T, h4(textOutput("guidetext1")),
                            dateInput("guidedate1", label=NA, value = NULL, min = NULL, max = NULL,
                                      format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                      language = "en", width = NULL),
                            dateInput("guidedate2", label=NA, value = NULL, min = NULL, max = NULL,
                                      format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                      language = "en", width = NULL),
                            h3(textOutput("guidetext2")),
                            numericInput("guideval","Production Planned",0),
                            actionButton("guidego", "Save"),
                            actionButton("guiderefresh", "Refresh data")),
                            box(title = "Guide Plate",width = 14,status = "warning",solidHeader = T,collapsible = T,DT::dataTableOutput("guidetable"))),
                    tabItem(tabName = "pawl", box(title = "PAWL",width = 14,status = "warning",solidHeader = T,collapsible = T,h4(textOutput("pawltext1")),
                            dateInput("pawldate1", label=NA, value = NULL, min = NULL, max = NULL,
                                      format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                      language = "en", width = NULL),
                            dateInput("pawldate2", label=NA, value = NULL, min = NULL, max = NULL,
                                      format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                      language = "en", width = NULL),
                            h3(textOutput("pawltext2")),
                            numericInput("pawlval","Production Planned",0),
                            actionButton("pawlgo", "Save"),
                            actionButton("pawlrefresh", "Refresh data")),
                            box(title = "PAWL",width = 14,status = "warning",solidHeader = T,collapsible = T,DT::dataTableOutput("pawltable"))),
                    tabItem(tabName = "guide1",h1("Guide Plate Production Performance"),actionButton("guidedevrefresh", "Refresh data"),box(title = "Guide Plate",width = 14,status = "warning",solidHeader = TRUE,collapsible = TRUE,dataTableOutput("table"))),
                    tabItem(tabName = "pawl1",h1("PAWL Production Performance"),actionButton("pawldevrefresh", "Refresh data"),box(title = "PAWL",width = 14,status = "warning",solidHeader = TRUE,collapsible = TRUE,dataTableOutput("table1"))),
                    tabItem(tabName = "guide2",textOutput("partsdate"),dateInput("guideparts", label=NA, value = NULL, min = NULL, max = NULL,format = "yyyy-mm-dd", startview = "month", weekstart = 0,language = "en", width = NULL),
                            box(title ="Guide Plate",width = 12,status = "warning",solidHeader = TRUE,collapsible = TRUE,valueBoxOutput("vbox"),valueBoxOutput("vbox1"),valueBoxOutput("vbox2"),infoBoxOutput("ibox"))),
                    tabItem(tabName = "pawl2",textOutput("partsdate1"),dateInput("pawlparts", label=NA, value = NULL, min = NULL, max = NULL,format = "yyyy-mm-dd", startview = "month", weekstart = 0,language = "en", width = NULL),
                            box(title ="PAWL",width = 12,status = "warning",solidHeader = TRUE,collapsible = TRUE,valueBoxOutput("vbox3"),valueBoxOutput("vbox4"),valueBoxOutput("vbox5"),infoBoxOutput("ibox1"))),
                    tabItem(tabName = "guide3",box(title = "Guide Plate",width = 14,status = "info",solidHeader = TRUE,collapsible = TRUE,plotlyOutput("plot",width = "100%",height = 500))),
                    tabItem(tabName = "pawl3",box(title = "PAWL",width = 14,status = "info",solidHeader = TRUE,collapsible = TRUE,plotlyOutput("plot2",width = "100%",height = 500))),
                    tabItem(tabName = "h",actionButton("generate", "Generate PDF"),uiOutput("pdfview"))
                  )
                )
))
                
