#
# Hatteras Region, Feb 28th 2017
#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# load libraries
library(shiny)
library(httr)
library(ncdf4)
library(sp)
library(xtractomatic)
library(mapdata)
library(ggfortify)
library(xts)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        dateInput <-  base::date()
        
        # setting reative values as F
        rv <- reactiveValues()
        rv$setupComplete <- FALSE
        
        ## Acception user inputs
        # Date Range Input
        selMonth <- reactive({
                monthInput <- input$Month
        })
        # Variable Input
        variable <- reactive({
                varInput <- input$Variable
        })
        
        # EXTRACTION
        
        # This script extracts monthly satellite data for the requested period using the xtracto_3D function. 
        
        # The extract is for the Hatteras coastal region. xtracto_3D extracts data in a 3D bounding box where xpos 
        # has the minimum and maximum longitude, ypos has the minimum and maximum latitude, and tpos has the starting 
        # and ending time, given as YYYY-MM-DD, describing the bounding box.
        
        # First we define the longitude, latitude and temporal boundaries of the box area of interest:
        xpos<-c(-77,-74)
        ypos<-c(33,36)
        tpos<-c('2016-09-16','2016-11-16')
        
        # A. Downloading data from the Multi-scale Ultra-high Resolution (MUR) SST Analysis, Global, 2002-present, Monthly 
        # We determine the min and max time of the dataset by using the getInfo function,
        # require(xtractomatic)
        # getInfo('jplMURSST41mday')
        # The extract will contain data at all of the longitudes, latitudes and times in the requested dataset that are within
        # the given bounds.
        print("downloading jplMURSST41mday")
        require(xtractomatic)
        require(lubridate)
        MURsst<-xtracto_3D(xpos, ypos, tpos,'jplMURSST41mday')
        MURsst$time<-as.Date(MURsst$time)
        
        # B. Downloading data from the Chlorophyll-a, Aqua MODIS, NPP, L3SMI, Global, 4km, Science Quality, 2003-present, Monthly 
        # We determine the min and max time of the dataset by using the getInfo function,
        # require(xtractomatic)
        # getInfo('mhchlamday')
        # The extract will contain data at all of the longitudes, latitudes and times in the requested dataset that are within
        # the given bounds.
        print("downloading mhchlamday")
        require(xtractomatic)
        require(lubridate)
        MODISchl<-xtracto_3D(xpos, ypos, tpos,'mhchlamday')
        MODISchl$time<-as.Date(MODISchl$time)
        print("finished downloading, starting plotting")
        
        ## set my condition to TRUE
        rv$setupComplete <- TRUE
        
        ## PLOTTING FUNCTIONS for later
        
        # The datasets will contain data on a grid defined by longitude, latitude and time.
        # These data from the satellite can be map for a given time period.
        # To do so we define a helper function mapFrame to reshape the data to be used in ggplot2.
        
        # sst helper functions:
        mapFrame_sst<- function(longitude,latitude,sst){
                dims<-dim(sst)
                sst<-array(sst,dims[1]*dims[2])
                #        longitude<-longitude-360
                sstFrame<-expand.grid(x=longitude,y=latitude)
                sstFrame$sst<-sst
                return(sstFrame)
        }
        # chla helper functions:
        mapFrame_chl<- function(longitude,latitude,chla){
                dims<-dim(chla)
                chla<-array(chla,dims[1]*dims[2])
                #        longitude<-longitude-360
                chlaFrame<-expand.grid(x=longitude,y=latitude)
                chlaFrame$chla<-chla
                return(chlaFrame)
        }
        
        # and also define a helper function plotFrame to plot the data:
        plotFrame_sst<-function(sstFrame, xlim, ylim,title,logplot=TRUE){
                require(mapdata)
                require(ggplot2)
                require(RColorBrewer)
                w <- map_data("worldHires", ylim = ylim, xlim = xlim)
                myplot<-ggplot(data = sstFrame, aes(x = x, y = y, fill = sst)) +
                        geom_raster(interpolate = FALSE, na.rm=TRUE) +
                        geom_polygon(data = w, aes(x=long, y = lat, group = group), fill = "grey80") +
                        theme_bw() + ylab("latitude") + xlab("longitude") +
                        coord_fixed(1.3,xlim = xlim, ylim = ylim)
                my.col <- colorRampPalette(rev(brewer.pal(11, "Spectral"))) ((diff(range(sstFrame$sst,na.rm=TRUE))))
                myplot<-myplot + scale_fill_gradientn(colours = my.col, na.value = NA, limits=c(23,30)) +
                        ggtitle(title)
                return(myplot)
        }
        # and also define a helper function plotFrame to plot the data:
        plotFrame_chl<-function(chlaFrame, xlim, ylim,title,logplot=TRUE){
                require(mapdata)
                require(ggplot2)
                require(RColorBrewer)
                w <- map_data("worldHires", ylim = ylim, xlim = xlim)
                myplot<-ggplot(data = chlaFrame, aes(x = x, y = y, fill = chla)) +
                        geom_raster(interpolate = FALSE, na.rm=TRUE) +
                        geom_polygon(data = w, aes(x=long, y = lat, group = group), fill = "grey80") +
                        theme_bw() + ylab("latitude") + xlab("longitude") +
                        coord_fixed(1.3,xlim = xlim, ylim = ylim)
                my.col <- colorRampPalette(rev(brewer.pal(11, "RdBu")))((diff(range(chlaFrame$chla,na.rm=TRUE))))
                myplot<-myplot + scale_fill_gradientn(colours = my.col, trans = "log", na.value = NA,limits=c(0.03,30.0)) +
                        ggtitle(title)
                return(myplot)
        }
        
        varPlot1 <- reactive({
                # Selecting the user input month and year to plot
                monthPeriod <- strsplit(input$Month, "-")[[1]]
                seLmonth <- as.numeric(monthPeriod[[2]])
                seLyear <- as.numeric(monthPeriod[[1]])
                # Plotting figure
                require(lubridate)
                xlim<-c(-76.8,-74.2)
                ylim<-c(33.2,35.8)
                
                if(input$Variable == "Chlorophyll-a"){
                        # We examine chl
                        ttext<-MODISchl$time[month(MODISchl$time) == seLmonth & year(MODISchl$time) == seLyear]
                        chlaFrame<-mapFrame_chl(MODISchl$longitude,MODISchl$latitude,MODISchl$data[,,month(MODISchl$time) == seLmonth & year(MODISchl$time) == seLyear])
                        chlaPlot<-plotFrame_chl(chlaFrame,xlim,ylim,paste("MODIS Mean Chlorophyll a Concentration (mg/m^3)\n",ttext), logplot=FALSE)
                        return(chlaPlot)
                }else{ 
                        # Plotting figure
                        # We examine sst 
                        ttext<-MURsst$time[month(MURsst$time) == seLmonth & year(MURsst$time) == seLyear]
                        sstFrame<-mapFrame_sst(MURsst$longitude,MURsst$latitude,MURsst$data[,,month(MURsst$time) == seLmonth & year(MURsst$time) == seLyear])
                        sstPlot<-plotFrame_sst(sstFrame,xlim,ylim,paste("MUR Sea Surface Temperature Monthly Mean (",intToUtf8(176),"C)\n",ttext), logplot = FALSE)
                        return(sstPlot)
                }
        })
        
        # Plots
        output$plot1 <- renderPlot({
                varPlot1()          # have to use () for reactive function
        })
        
        # Printing text
        output$date <- renderText({
                dateInput
        })
        output$variable <- renderText({
                variable()          # have to use () for reactive function
        })
        output$selMonth <- renderText({
                selMonth()          # have to use () for reactive function
        })
        output$SST <- renderText({
                linkInput()          # have to use () for reactive function
        })
        
        ## the conditional panel reads this output
        output$setupComplete <- reactive({
                return(rv$setupComplete)
        })
        outputOptions(output, 'setupComplete', suspendWhenHidden=FALSE)
        
})
