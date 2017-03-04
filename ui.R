#
# Hatteras Region, Feb 28th 2017
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# Install necessary packages
library(shiny)
library(xtractomatic)

# Define UI for application that plots variables
shinyUI(fluidPage(
        # Application title
        titlePanel("Sea Surface Temperature and Chlorophyll-a offshore of Hatteras"),
        h3("This Shiny App downloads and plots satellite data for the time period September 2016 to October 2016", align = "center"),
        h4(textOutput("date")),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
                sidebarPanel(
                        # Select variable box 
                        radioButtons("Variable", label = "Select a variable and click the 'Submit' button",
                                     choices = list("Sea Surface Temperature (SST)", "Chlorophyll-a"),
                                     selected = "Sea Surface Temperature (SST)"),
                        submitButton("Submit", icon = icon("upload")),
                        h4(" "),
                        
                        # Select plot month box 
                        selectInput("Month", label = "Select a month to plot and click the 'Submit' button",
                                    choices = list("2016-09", "2016-10"),
                                    selected = "2016-10"),
                        submitButton("Submit", icon = icon("hand-o-up")),
                        
                        # Showing selections
                        h4(" "),
                        h4("Selections are shown below:"),
                        h4("Selected Variable:"),
                        textOutput("variable"),
                        h4("Selected Month:"),
                        textOutput("selMonth"),
                        h4(" "),
                        h4(" "),
                        actionButton(inputId='Git', label="GitHub Repo with full code", 
                                     icon = icon("github"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                     onclick ="window.open('https://github.com/CandiceGH/SSTApp_chla.git', '_blank')")
                ),
                
                mainPanel(
                        conditionalPanel(condition = "output.setupComplete",
                                         plotOutput("plot1"),
                                         h4(" "),
                                         h4("NOAA NMFSC SWFSC ERD ERDDAP data source:"),
                                         actionButton(inputId='SST', label="SST Link", 
                                                      icon = icon("database"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4", # th, paper-plane
                                                      onclick ="window.open('https://coastwatch.pfeg.noaa.gov/erddap/griddap/jplMURSST41mday.html', '_blank')"),
                                         actionButton(inputId='Chla', label="Chlorophyll-a Link", 
                                                      icon = icon("globe"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                                      onclick ="window.open('https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdMH1chlamday.html', '_blank')")
                        ),
                        conditionalPanel(condition = "!output.setupComplete",
                                         h4("Loading satellite data... ")
                        )
                )
        )
))
