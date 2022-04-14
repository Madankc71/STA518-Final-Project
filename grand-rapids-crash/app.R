#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(readr)
setwd("~/STA 518/STA Final Project/STA518-Final-Project")

#Importing datasets
barchart_data <- read_csv("barchart_data.csv")

counties <- read_csv("counties.csv")

piechart_total <- read_csv("piechart_total.csv")

crashes_total <- read_csv("~/STA 518/STA Final Project/STA518-Final-Project/crashes_total.csv")

library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(googlesheets4)
library(tidyverse)
library(plotly)
library(ggmap)
library(maps)
library(mapproj)
library(reshape2)
library(leaflet)
library(sf)
library(tigris)

shape <- tigris::counties(state = "MI", class = "sf")


# Define UI for application that draws a histogram
title<-
  # Define UI
  ui <- fluidPage(theme = shinytheme("cerulean"),
                  navbarPage("Grand Rapids Crash",
                             #theme = "cerulean",  # <--- To use a theme,
                             tabPanel("Total Crash Comparison",
                                      sidebarPanel(
                                        selectInput("Type", "Select Type", choices = unique(barchart_data$Type))
                                      ), # sidebarPanel
                                      mainPanel(
                                        plotOutput("barplot"),
                                      ) # mainPanel
                             ),
                             tabPanel("Crashes - County", #countywise line chart of crash
                                      sidebarPanel(
                                        selectInput("county", "Select County", choices = unique(counties$County))
                                      ), # sidebarPanel
                                      mainPanel(
                                        plotOutput("linechart1"),
                                      ) # mainPanel
                             ), #end - countywise line chart of crash
                             
                             tabPanel("Injuries and death - County", #countywise line chart of injury and death
                                      sidebarPanel(
                                        selectInput("county_injury", "Select One County", choices = unique(counties$County))
                                      ), # sidebarPanel
                                      mainPanel(
                                        plotOutput("linechart2"),
                                      ) # mainPanel
                             ), #end - countywise line chart of injury and death
                             
                             tabPanel("Piechart1", #piechart
                                      sidebarPanel(
                                        selectInput("Year", "Select a Year", choices = unique(piechart_total$year))
                                      ), # sidebarPanel
                                      mainPanel(
                                        plotOutput("piechart1"),
                                      ) # mainPanel
                             ), #end - countywise line chart of injury and death
                            
                             tabPanel("Total Crashes", #histogram
                                      sidebarPanel(
                                        selectInput("county", "Select a county", choices = unique(crashes_total$County))
                                      ), # sidebarPanel
                                      mainPanel(
                                        plotOutput("histogram"),
                                      ) # mainPanel
                             ), #end - histogram of total crashes
                             tabPanel("Total Crashes", #piechart
                                      sidebarPanel(
                                        selectInput("time", "Select a Year", choices = unique(piechart_total$year))
                                      ), # sidebarPanel
                                      mainPanel(
                                        plotOutput("piechart"),
                                      ) # mainPanel
                             )
                          
                  ), #nav bar 
  ) #ui 



server <- function(input, output) {
  
  output$barplot <- renderPlot({
    library(reshape2)
    barchart_data <- barchart_data %>% filter(Type==input$Type)
    barchart_data <- melt(barchart_data[,c('year','Alcohol_total','Drug_total')], id.vars = 1)
    g <- ggplot(barchart_data, aes(x = year, y = value)) +
      geom_bar(aes(fill = variable),stat = "identity",position = "dodge")
    g
  })
  
  output$barr <- renderPlot({
    barchart_data <- barchart_data %>% filter(Type==input$type)
    barchart1 <- data.frame(Year = barchart_data$year,
                            Number1 = c(barchart_data$Alcohol_total, barchart_data$Drug_total),
                            Type = c(rep("Alcohol Total", nrow(barchart_data)),
                                     rep("Drug Total", nrow(barchart_data))))
    g <- ggplot(barchart1, aes(Year, Number1, col = Type)) +
      geom_bar()
    g
  })
  
  #line chart of crash
  output$linechart1 <- renderPlot({
    cc <- counties
    cc <- cc %>% filter(County==input$county)
    county_crash <- data.frame(Year = cc$year,                            # Reshape data frame
                               Number = c(cc$Alcohol_crash, cc$Drug_crash),
                               Type = c(rep("Alcohol Crash", nrow(cc)),
                                        rep("Drug Crash", nrow(cc))))
    
    ggp <- ggplot(county_crash, aes(Year, Number , col = Type)) +             # Create ggplot2 plot
      geom_line()
    ggp 
  }) # end - line chart of crash
  
  #Linechart of injuries and death
  output$linechart2 <- renderPlot({
    cc2 <- counties
    cc2 <- cc2 %>% filter(County==input$county_injury)
    injury_county <- data.frame(Years = cc2$year,                            # Reshape data frame
                                Numbers = c(cc2$Alcohol_injury, cc2$Drug_injury),
                                Types = c(rep("Alcohol Injuries and Death", nrow(cc2)),
                                         rep("Drug Injuries and Death", nrow(cc2))))
    
    ggp <- ggplot(injury_county, aes(Years, Numbers , col = Types)) +             # Create ggplot2 plot
      geom_line()
    ggp 
  }) # end of Linechart of injuries and death
  
  # Start -- piechart of total crashes per year
  output$piechart <- renderPlot({
    piechart_total <- piechart_total %>% filter(year==input$Year)
    df_crash <- data.frame(Types = c("alcohol", "drug", "others"),
                           value=c(piechart_total$total_alcohol_crashes, piechart_total$total_drug_crashes, piechart_total$Other_Crashes)
    )
    ggp <-  ggplot(df_crash, aes(x="", y=value, fill=Types)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) +
      theme_void()
    ggp
  })
  
  output$piechart1 <- renderPlot({
    piechart_total <- piechart_total %>% filter(year==input$Year)
    df_crash <- data.frame(Types = c("alcohol", "drug", "others"),
                           value=c(piechart_total$total_alcohol_crashes, piechart_total$total_drug_crashes, piechart_total$Other_Crashes)
    )
    ggp <-  ggplot(df_crash, aes(x="", y=value, fill=Types)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) +
      theme_void()
    ggp
  })
  
  # Start -- histogram of total crashes per year
  output$histogram <- renderPlot({
    crashes_total <- crashes_total %>% filter(year==input$county)
    crashes <- crashes_total$Total_Crashes
      hist(crashes)
    })
  
  output$piechart <- renderPlot({
    crashes_total <- crashes_total %>% filter(year==input$time)
    df_crash <- data.frame(Types = c("Ionia", "Kent", "Montcalm","Ottawa"),
                           value=c(crashes_total$Ionia_total_crash, crashes_total$Kent_total_crash, crashes_total$Montcalm_total_crash, crashes_total$Ottawa_total_crash)
    )
    ggp <-  ggplot(df_crash, aes(x="", y=value, fill=Types)) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) +
      theme_void()
    ggp
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
