
if(!require(shiny)){install.packages("shiny")}
if(!require(gridExtra)){install.packages("gridExtra")}
if(!require(openair)){install.packages("openair")}

library(rCharts)
library(shiny)
library(gridExtra)
library(openair)

options(RCHART_WIDTH = 350)
options(RCHART_HEIGHT = 300)

rfc<-read.csv("rfc.csv",colClasses=c("character","numeric","numeric"))

## Define server logic required to summarize and view the selected dataset #
shinyServer(function(input, output,session)

  {
  
##Based on site location chosen by user, either use existing lat-lon information for
##US Steel or utilize user-provided lat-lon for alternate site #
  siteInput<-reactive({

 
     if (input$site=="US Steel")
       {
              Lat <- 41.6035
              Lon <- -87.3326
              tooltip <- paste(input$site,"Xact Monitor",sep=" ")
              site <- data.frame(Lat,Lon,tooltip)
              
     }

                    else{
                      Lat <- as.numeric(input$lat)
                      Lon<- as.numeric(input$long)                      
                      tooltip <- paste(input$site,"Xact Monitor",sep=" ")

                        
                     site <- data.frame(Lat,Lon,tooltip)
                           }                                                    
                                       
                       return(site)
                                    })

 
## This function handles the csv file of Xact monitoring data uploaded by user which can be fed into graphs and tables #
  InputUpload <- reactive( {
    if (input$site=="US Steel") {
    
      inFileUSS <- input$upDataUSS
       if (is.null(inFileUSS)) 
         return (NULL)
      xact_file <- read.csv(inFileUSS$datapath) 
   }


   else {
    
        if(input$site=="Burns Harbor"){
      inFileBH <- input$upDataBH
       if (is.null(inFileBH))
         return (NULL)            
          xact_file <- read.csv(inFileBH$datapath) 

     }
        }

     return (xact_file)
           
   })
  
## This function uses user-uploaded Xact monitoring data or US Steel existing data 
##  and merges with Gary meteorological data for use in openair plots #
  xactInput<-reactive({ 

      xact<-as.data.frame(InputUpload())
        
      colnames(xact) <- c("date","AT.C",  "BP.mmHg",  "ALARM",  "Potassium", "Calcium", "Titanium", 
                          "Chromium", "Manganese", "Iron", "Cobalt", "Nickel", "Copper", "Zinc", 
                          "Arsenic", "Selenium", "Bromine", "Rubidium", "Strontium", "Molybdenm", 
                          "Cadmium", "Antimony", "Barium", "Mercury", "Thallium", "Lead", "Thorium")
      
      
      date2 <- as.POSIXct(strptime(xact[,1], format="%m/%d/%Y %H:%M", tz="UTC"))
      date2 <- format(date2, format="%Y-%m-%d %H:%M:%S", tz="UTC")
      xact <- cbind(date2, xact[,2:27])
      
      gary_met <- as.data.frame(local(get(load("gary_met_wide.rda"))), stringsAsFactors=FALSE)
      colnames(gary_met) <- c("DATE","YEAR","MONTH","DAY","HOUR","ws","wd","std dev hz wd","temp","precip")
      xact_met <- merge(xact, gary_met, by.x="date2", by.y="DATE")
      xact_param <- which(colnames(xact_met)==input$param)
      xact.vec <- xact_met[c(eval(xact_param))]
      xact <- data.frame(xact_met[,1], xact.vec, xact_met[,28:36])
      colnames(xact)<- c("date","param","YEAR","MONTH","DAY","HOUR","ws","wd","std dev hz wd","temp","precip")
      
   
    return (xact)
  })
  
##Function which allows subsetted data to be downloaded by user in .csv format #
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$site,input$param, '.csv', sep='') },
    content = function(file) {
      write.csv(xactInput(), file,row.names=F)
      
    })

  
##This function returns an error message if the user selects a site 
  ##but has not yet uploaded the associated dataset #
  mess<-reactive({
   if (input$site=="US Steel" ) {
       err <- InputUpload()
      errmess <- c("You must first upload US Steel Xact data via the Upload tab")}
   else{
    if(input$site=="Burns Harbor")  {
       err <- InputUpload()
     errmess <- c("You must first upload Burns Harbor Xact data via the Upload tab")
     }
   }
     if (is.null(err)) 
    return (errmess) 
   
   })
    
   
  
  
  output$err_message <- renderText( {
     mess()
   })

    
    
  
  output$xactTable <- renderTable( {
    view_data <- InputUpload()
    return (view_data[1:10,])
  })
  
##Creates UI for user site choice##
  output$site <- renderUI({

              selectInput("site", "Choose a study site:", 
              choices = c("US Steel","Burns Harbor"),
              selected = "US Steel")

 })
  
 
  ##Creates UI for user parameter choice##
  output$param <- renderUI({
          xact_param <- c("Potassium", "Calcium", "Titanium", "Chromium", "Manganese", 
                 "Iron", "Cobalt", "Nickel", "Copper", "Zinc", "Arsenic", "Selenium", 
                 "Bromine", "Rubidium", "Strontium", "Molybdenm", "Cadmium", "Antimony", 
                 "Barium", "Mercury", "Thallium", "Lead", "Thorium") 
          
          selectInput("param", "Choose a parameter:", 
              choices = c(sort(unique(xact_param))),
              selected = "Manganese")
 })
  
  
##Creates text inputs for user-provided lat-long  #
  output$lat<-renderUI( {

    textInput("lat", "Latitude (DD):", value="") 
  
    })
  
 output$long<-renderUI( {
   
   textInput("long", "Longitude (DD):", value="")
 
   })

  
  output$head_text <- renderText(paste(input$site, 'Xact Monitoring Data', sep =" "))
  
  output$map_text <- renderText(paste(input$site, 'Monitoring Site', sep =" "))

##Creates openair plots for display on the main panel #
   output$main_plot <- renderPlot({

      
      xact_gr<-xactInput()
    
   
    

      plotlab1 <- input$site
      plotlab2 <- input$param
       
      rfc2 <- rfc[rfc$Parameter==input$param,2]
      rfc3 <- rfc[rfc$Parameter==input$param,3]
      rfc4 <- log10(rfc2)
      rfc5 <- log10(rfc3)
    
   
      pr<-pollutionRose(xact_gr,ws="ws",wd="wd",pollutant="param",main=paste0(plotlab1," Pollution Rose Plot for 2012-2013 ",plotlab2))
      perR<-percentileRose(xact_gr,plot.transparent=TRUE,pollutant="param", percentile=c(0,5,25,50,75,95,99),key.position="right",main=paste0(plotlab1," Percentile Rose Plot for 2012-2013 ",plotlab2))

    
    if (input$log==FALSE) {
    
      tp <- timePlot(xact_gr, pollutant ="param", key = FALSE, ylab ="Concentration (ng/m3)",
                   main = paste("Hourly", plotlab2, sep =" "), ref.y = c(rfc3), date.format ="%b %Y", log = FALSE)
    
      tp2 <- timePlot(xact_gr, pollutant ="param", key = FALSE, ylab = NULL, statistic ="max", avg.time ="day",
                    main = paste("Daily Max", plotlab2, sep =" "), ref.y = c(rfc2,rfc3), date.format ="%b %Y", log = FALSE)
    
      sp <- scatterPlot(xact_gr, x ="date", y ="param", z ="ws", xlab ="2012-2013", ylab ="Concentration (ng/m3)",
                      key.position ="right", ref.y = c(rfc2,rfc3), log.y = FALSE,
                      main = paste("Hourly", plotlab2,"by Wind Speed",sep =" "))
    
      sp2 <- scatterPlot(xact_gr, x ="date", y ="param", z ="wd", xlab ="2012-2013", ylab = NULL,
                       key.position ="right", ref.y = c(rfc2,rfc3), log.y = FALSE,
                       main = paste("Hourly", plotlab2,"by Wind Direction",sep =" "))
    }
    else {
      
      tp <- timePlot(xact_gr, pollutant = "param", key = FALSE, ylab ="Concentration (ng/m3)",
                     main = paste("Hourly", plotlab2, sep=" "), ref.y = c(rfc4,rfc5), date.format = "%b %Y", log = TRUE)
      
      tp2 <- timePlot(xact_gr, pollutant ="param", key=FALSE, ylab=NULL, statistic ="max", avg.time ="day",
                      main = paste("Daily Max", plotlab2, sep=" "), date.format ="%b %Y", ref.y = c(rfc4,rfc5),log = TRUE)
      
      sp<- scatterPlot(xact_gr, x ="date", y ="param", z ="ws", xlab="2012-2013", ylab ="Concentration (ng/m3)",
                       key.position ="right", ref.y = c(rfc4,rfc5),log.y = TRUE,
                       main = paste("Hourly", plotlab2," by Wind Speed",sep =" "))
      
      sp2 <- scatterPlot(xact_gr, x = "date", y = "param", z = "wd", xlab = "2012-2013", ylab = NULL,
                         key.position = "right", ref.y = c(rfc4,rfc5), log.y = TRUE, date.format = "%b-%Y",
                         main = paste("Hourly", plotlab2," by Wind Direction", sep =" "))
      
    }
    
      
        print(perR, position = c(0,0.66,0.5,1), more = TRUE)
        print(pr, position = c(0.5,0.66,1,1), more = TRUE)
        print(tp, position = c(0,0.33,0.5,0.65), more = TRUE)
        print(tp2, position = c(0.5,0.33,1,0.65), more = TRUE)
        print(sp, position = c(0,0,0.5,0.33), more = TRUE)
        print(sp2, position = c(0.5,0,1,0.33))
    
        print(grid.text(x = 0.11, y = 0.53, label ="--- Rfc", just="left"))
        print(grid.text(x = 0.11, y = 0.55, label ="--- SAT Screening Level", just="left")) 
        print(grid.text(x = 0.60, y = 0.53, label ="--- Rfc", just="left"))
        print(grid.text(x = 0.60, y = 0.55, label ="--- SAT Screening Level", just="left")) 
        print(grid.text(x = 0.11, y = 0.21, label ="--- Rfc", just ="left"))
        print(grid.text(x = 0.11, y = 0.23, label ="--- SAT Screening Level", just="left")) 
        print(grid.text(x = 0.60, y = 0.21, label ="--- Rfc", just="left"))
        print(grid.text(x = 0.60, y = 0.23, label ="--- SAT Screening Level", just="left")) 
     
    
   },   height=725)
   
 
    
##Creates leaflet map to display site location on the side panel #    
  output$mymap <- renderMap( {

    site <- siteInput()
    lat <- site$Lat
    lon <- site$Lon
    tooltip <- site$tooltip

      map3 <- Leaflet$new()
      map3$setView (c(lat,lon),zoom = 13)
      map3$tileLayer (provider = "Stamen.Terrain", maxZoom=18)
      map3$marker (c(lat,lon), bindPopup = tooltip)
      map3

  })
  
  
  })
