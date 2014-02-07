options(RCHART_WIDTH = 700)
options(RCHART_HEIGHT = 600)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output,session)
  {
  
  
  datasetInput <- reactive({
    

    if(input$contab==1){
      
    
    if(input$County==("All Counties")){
       
            dataset = data[data$AQS.Parameter.Desc==input$DESCRIPTION,]  
            dataset = dataset[dataset$Year.GMT %in% seq(input$year[1], input$year[2]), ]}
    
    else{       dataset = data[data$AQS.Parameter.Desc==input$DESCRIPTION,]
                dataset = dataset[dataset$NAME==input$County,]
                dataset = dataset[dataset$Year.GMT %in% seq(input$year[1], input$year[2]), ]} 

    return(dataset) 
   
    }
  })
   
  
  parameterInput<-reactive({
  
  
    
    
    if(input$contab==1){
     
      
      dataset = data[data$Year.GMT %in% seq(input$year[1], input$year[2]), ]
      param = dataset[[11]]
      
      return(param)} else{return()}
    
    
    
  })
  
  
  countyInput<- reactive({
    
    if(input$contab==1){
      
      dataset = data[data$Year.GMT %in% seq(input$year[1], input$year[2]), ]
      dataset = data[data$AQS.Parameter.Desc==input$DESCRIPTION,]
      county = dataset[[27]]
    return(county)} else{return()}
  })
  
  
  siteInput<-reactive({

    
    if (input$contab==1){
      dataset = data[data$Year.GMT %in% seq(input$year[1], input$year[2]), ]
      site = dataset[dataset$AQS.Parameter.Desc==input$DESCRIPTION,]
      site = site[site$DESCRIPTION==input$DESCRIPTION,]
      site = site[site$NAME==input$County,]
           
    }
      
      else{
        if(input$contab==2){      
                  
                  site = wswd[wswd$NAME==input$county,]
                                 
                                  
        }   
                                  else{
                                    if (input$contab==3){
                                      
#                                       lat<- c(41.6035,41.6121,41.5995)
#                                       lon<- c(-87.3326,-87.3263,-87.3443)
#                                       tip<-c("Gary Xact Monitor","US Steel Garyworks","Jefferson Elementary School")
#                                       site<-data.frame(as.numeric(lat),as.numeric(lon),tip)
                                      
                                      LatLon<-c("41.6035:-87.3326", "41.6121:-87.3263", "41.5995:-87.3443")
                                      Tip<-c("Gary Xact Monitor","US Steel Garyworks","Jefferson Elementary School")
                                      site <- data.frame(LatLon,Tip)
                                  
                                    }
                                      
                                      else{
                                        if(input$contab==4){ 
                                                                                  
                                          site = emissions[emissions$YEAR ==input$NEIyear, ]
                                          site = site[site$Pollutant_Code_Desc==input$pollutant,]
                                          site = site[site$name==input$CountyName,]
                                          
                                          #emissions = emissions[emissions$PLANT_DESCRIPTION==input$DESCRIPTION,]
                                          }
                                        else{
                                          if(input$contab==5){
                                            site = emissions[emissions$YEAR =="2008", ]
                                            site = site[site$Pollutant_Code_Desc=="Lead",]
                                            site = site[site$name=="Marion",]
                                          }
                                        }
                                       
                                      }
                                  }
                                        
                                      }
                                    
                                  
       return(site)
     
  })
  

  
  wswdInput<-reactive({ 

    
    
    if(input$contab==2){
      wswd = wswd[wswd$NAME==input$county,]
      wswd = wswd[wswd$YEAR %in% seq(input$Year[1], input$Year[2]), ]
      wswd = wswd[wswd$MONTH %in% seq(input$Month[1], input$Month[2]), ]
        return(wswd)} else{return()}
                       })
  
  xactInput<-reactive({ 
    
 
   if(input$contab==3){
      
      test<-which(colnames(xact)==input$param)
      xact.vec<-xact[c(eval(test))]
      
      xact = data.frame(xact[,1],xact.vec,xact[,28:37])
      colnames(xact)<-c("date","param","YEAR","MONTH","DAY","HOUR","ws","wd","peak wind gust","std dev hz wd","vert wd","temp")
#       xact = xact[xact$date %in% seq(input$Date[1], input$Date[2]), ]
      
      return(xact)} else{return()}
    
        
  })

    emissionsInput<-reactive({ 
   
      
      if(input$contab==4){
   
        
        emissions = emissions[emissions$Pollutant_Code_Desc==input$pollutant,]
        emissions = emissions[emissions$name==input$CountyName,]
        emissions = emissions[emissions$YEAR==input$NEIyear, ]
       
        
    return(emissions)} else{return()}
      #emissions = emissions[emissions$PLANT_DESCRIPTION==input$DESCRIPTION,]
      #return(emissions)} 
                          
  
    })
  
  emissionsInput2<-reactive({ 
    
    
    if(input$contab==4){
      
      
      emissions = emissions[emissions$Pollutant_Code_Desc==input$pollutant,]
      emissions = emissions[emissions$YEAR==input$NEIyear, ]
      
      
      return(emissions)} else{return()}
    #emissions = emissions[emissions$PLANT_DESCRIPTION==input$DESCRIPTION,]
    #return(emissions)} 
    
    
  })
  
 
  pollInput<-reactive({ 

    
    
    if(input$contab==4){
      
      emissions = emissions[emissions$YEAR ==input$NEIyear, ]
      #emissions = emissions[emissions$COUNTY_NAME==input$CountyName,]
      emissionspollutant = emissions[[27]]
      
      #emissions = emissions[emissions$PLANT_DESCRIPTION==input$DESCRIPTION,]
      return(emissionspollutant)}  else{return()}
  })  
  
  
  ecountyInput<-reactive({ 
   
 
    
    if(input$contab==4){
      
      #emissions = emissions[emissions$YEAR ==input$NEIyear, ]
     # emissions = emissions[emissions$Pollutant_Code_Desc==input$pollutant,]
      
      emissionscounty = emissions[[83]]
      
      #emissions = emissions[emissions$PLANT_DESCRIPTION==input$DESCRIPTION,]
      return(emissionscounty)} else {return()}
  })
  

  output$availparam<-renderUI({
    

      out<-parameterInput()
       params<-out
 
   
        selectInput("DESCRIPTION", "Choose a parameter:", 
                choices = sort(unique(params))
       ,
                          selected="Lead (TSP) LC")
      })   
  
 
  
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$DESCRIPTION, '.csv', sep='') },
    content = function(file) {
      write.csv(datasetInput(), file,row.names=F)
  
    })
      
      output$availcounty<-renderUI({
 

    out2<-countyInput()
    
    counties<-out2
    
    
    
      
    selectInput(inputId ="County",
                label = "Choose a county:",
                choices = c("All Counties",as.character(sort(unique(counties))))
                ,
                
               selected="All Counties")
   
})
  
  output$availemisscounty<-renderUI({
    

    
    s<-emissionsInput()
    #s2<-unique(s[83])
    s2<-s[83]
    out<-ecountyInput()
    
    counties<-out
    
    
    selectInput(inputId ="CountyName",
                label = "Choose a county:",
                choices = as.character(sort(unique(counties)))
    ,
    
   selected="Porter")
    
    
  })

  output$availcomp<-renderUI({
    

    
    out<-pollInput()
    params<-out
  
  
  selectInput("pollutant", "Choose the emissions category of metals:", 
              choices = sort(unique(params))
  ,
  selected="Lead")
  
})  
  
  
  output$inRadio<-renderUI({
    
    radioButtons("NEIyear",
                 "Select an NEI data year to view:",
                 choices=levels(as.factor(emissions$YEAR)),
                 selected="2008")
    
  })

  
  
  output$sideplot1<-renderPlot(function(){
    
  
    
    if(input$contab==1){
      
      
    
    site<-datasetInput()
    all<-map_data("county","indiana")
    
    p <- ggplot()
    p <- p + geom_polygon( data=all, aes(x=long, y=lat, group = group),color="gray") + coord_equal() + theme(axis.title.x=element_blank(),axis.title.y=element_blank())
    p <- p + geom_point(data=site, aes(x=LONG, y=LAT), color="coral1",size=4) + coord_equal() + theme(axis.title.x=element_blank(),axis.title.y=element_blank())
    #p <- p + geom_text( data=site, hjust=0.5, vjust=-0.5, aes(x=LONG, y=LAT, label=CITY), colour="gold2", size=3 )
    print(p)  
  }
})
   
    output$sideplot2<-renderPlot({
    
 
      
      if(input$contab==2){
        

        
    site2<-wswdInput()
    all<-map_data("county","indiana")
    p <- ggplot()
    p <- p + geom_polygon( data=all, aes(x=long, y=lat, group = group),colour="gray")+  coord_equal()+ theme(axis.title.x=element_blank(),axis.title.y=element_blank())
    p <- p + geom_point(data=site2, aes(x=lon, y=lat), color="coral1",size=4) 
    print(p)
    }
    })
    
    output$sideplot3<-renderGvis({
    
      
      
      if(input$contab==3)  {
     
  
        
    site3<-siteInput()
    
    gvisMap(site3,"LatLon","Tip",
               options=list(displayMode = "Markers",  
                            useMapTypeControl=TRUE, showTip=TRUE,
                            enableScrollWheel=TRUE, width="85%"))
    }
      
    })
#   output$myChart3 <- renderMap({
#     if(input$contab==3)  {  
#       site<-as.data.frame(siteInput())
#       site_lat<-mean(as.numeric(site[,1]))
#       site_lon<-mean(as.numeric(site[,2]))      
#       #hap<-unique(site$Pollutant_Code_Desc)
#       #hap2<-first.word(hap)
#       #chart_title<-paste(unique(site$name),"Co.\n",hap2, "Emissions",sep=" ")
#       #   Tip<-c("Gary Xact Monitor","US Steel Garyworks","Jefferson Elementary School")
#       
#       
#       map4 <- Leaflet$new()
#           
#       map4$setView(c(site_lat,site_lon),zoom = 9)
#       map4$tileLayer(provider="Stamen.Terrain",maxZoom=18)
#       map4$set(width = 250, height = 250)
#       dat_list <- toJSONArray2(site, json = F)
#       map4$geoJson(toGeoJSON(dat_list, lat = 'x', lon = 'y'),
#                    onEachFeature = '#! function(feature, layer){
#     layer.bindPopup(feature.properties.tip)
#  } !#',
#                    pointToLayer =  "#! function(feature, latlng){
#     return L.circleMarker(latlng, {
#       radius: 6,
#       fillColor: 'blue',
#       color: '#000',
#       weight: 1,
#       fillOpacity: 0.8
#     })
#  } !#"         
#       )
# 
#       #map4$marker(site_lat,site_lon)
#       map4$set(dom = 'myChart3')
#       map4
# #      lapply(1:3,function(x){
# #         lat<-as.numeric(site[x,1])
# #         lon<-as.numeric(site[x,2])
# #         tip<-site[x,3]
#        
#       
# #         return(mark)   }
# #              )
#       
#       
#     
#       
#     }
#       })
   
#     
#       output$sideplot4<-renderPlot({ 
#      
#       
#        
#         if(input$contab==4){
#           
#         
#     site4<-siteInput()
#     site5<-data.frame(site4$FacilityName,site4$name,site4$LAT,site4$LONG,site4$EmissionsTPY)
#     site5<-site5[complete.cases(site5),]
#     colnames(site5)<-c("facility","countyname","x","y","emissions")
#     emiss_merc <- as.data.frame(projectMercator(site5$x, site5$y, drop=TRUE))
#     emiss_merc2<- cbind(emiss_merc,site5$facility,site5$emissions)
#     names(emiss_merc2)<-c("x","y","facility","TPY")
#     
#     countyname<-tolower(unique(site4$name))
#     all<-map_data("county","indiana")
#     county<-subset(all,subregion %in% countyname)
#     county_merc<- as.data.frame(projectMercator(county$lat,county$long,drop=FALSE))
#         
#     lat<-min(county[[2]])
#     lat2<-max(county[[2]])
#     lon<-min(county[[1]])
#     lon2<-max(county[[1]])
# 
#     hap<-unique(site4$Pollutant_Code_Desc)
#     hap2<-first.word(hap)
#     chart_title<-paste(unique(site4$name),"Co.\n",hap2, "Emissions",sep=" ")
#     map <- openmap(c(lat2,lon), c(lat,lon2),zoom=10,type="mapquest")  #'stamen-toner'
#     
# #       if(input$CountyName=="All Counties"){
#            q <- ggplot(site4,aes(LONG,LAT))
#           q <- q + geom_polygon( data=all, aes(x=long, y=lat, group = group),colour="gray") + coord_equal()+ labs(x=NULL,y=NULL) + theme(plot.background = element_rect(fill = "transparent",colour = NA) ,
#                                                                                                                                           panel.background = element_rect(fill = "transparent",colour = NA), # or theme_blank()
#                                                                                                                                           panel.grid.minor = element_blank(), 
#                                                                                                                                           panel.grid.major = element_blank())
#          q <- q + geom_point(data=site4, color="coral1",size=2) + coord_equal() + theme(axis.title.x=element_blank(),plot.background=element_rect(fill="transparent",colour=NA), axis.title.y=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank())   #,plot.margin=unit(x=c(0,0,0,0),units="mm"),plot.background = element_rect(fill='white', colour='white')) #+ scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0))
#            
# # p<-autoplot(map,type="osm") 
# #       p <- p + geom_polygon( data=county_merc, aes(x=x, y=y),colour="black",alpha=0) + coord_equal()+ labs(x="Longitude",y="Latitude") + theme(axis.text.x=element_blank(), axis.text.y=element_blank())
# #       p <- p + geom_point(data=emiss_merc2, aes(size=TPY),color="coral1") + coord_equal() + labs(title= chart_title)  + theme(axis.title.y=element_blank(),axis.title.x=element_blank(),plot.title=element_text(face="bold"),legend.position="bottom" ) +
# #                   scale_size(guide=guide_legend(title="Emissions (TPY)",title.position="top",title.hjust = 0.5, direction="horizontal"))
# 
# ##legend.text=element_text(size=10),legend.title=element_text("Emissions",size=10),legend.text.align=0.1, legend.title.align=0.1, legend.position="bottom",legend.direction="horizontal")                                             
#   
#    # guides(fill=guide_legend(title="Emissions TPY",title.position="bottom,direction="horizontal,label.position="top))
#     # legend.text=element_text(size=10),legend.title=element_text(size=10),legend.position="bottom",legend.direction="horizontal"                                                                                      
#    
#       
# #      p <- ggplot(site4,aes(LONG,LAT))
# #      p <- p + geom_polygon( data=county, aes(x=long, y=lat, group = group),colour="black") + coord_equal()+ labs(x="Longitude",y="Latitude")
# #      p <- p + geom_point(data=site4, aes(size=EmissionsTPY),color="coral1") + labs(title= chart_title) + coord_equal()  
#      
#      grid.newpage()
#    # vpb_ <- viewport(width = 0.95, height = 0.95, x = 0.4, y = 0.5)  # the larger map
#     vpa_ <- viewport(width = 0.35, height = 0.35, x = 0.87, y = 0.85)  # the inset in upper right
#    # print(p, vp = vpb_)
#     print(q, vp = vpa_)
# 
# 
#     #}  
#       }
#     
#     })
    
#   output$sideplot5<-renderChart({
#     h1 <- hPlot(x = "Wr.Hnd", y = "NW.Hnd", data = MASS::survey, type = c("line", 
#                                                                           "bubble", "scatter"), group = "Clap", size = "Age")
#     h1$print("chart5")
#     return(h1)
#   })
  
  output$main_plot <- renderPlot({
    
# 
#     withProgress(session, min=1, max=5, expr={
#       for(i in 1:5) {
#         setProgress(message = 'Calculation in progress',
#                     
#                     value=i)
#         print(i)
#         Sys.sleep(0.5)
#       }
#     })
    
    dataset<-datasetInput()
    plotlab<-input$DESCRIPTION
    plotlab2<-input$chart_type
    chart_title<-paste(plotlab,plotlab2,"by County and Site",sep=" ")
    
    
    
    if(input$chart_type=="Boxplot"){p<-ggplot(dataset,aes(factor(Year.GMT),Sample.Measurement))
                                    q<-p + geom_boxplot() + facet_wrap( ~ NAME+Site.Num,scales="free") + xlab("") + ylab("Concentration (ug/m3)") + labs(title=chart_title) + theme(axis.text.x=element_text(angle = 90, hjust = 1.0,colour="black"),axis.text.y=element_text(colour="black",vjust=1.0),axis.title.x = element_text(colour="black",size=14,face="bold"),axis.title.y = element_text(colour="grey20",size=14,face="bold",vjust=0.2))  #+ scale_y_log10()
                                    
        
  } else {  
     if(input$chart_type=="Trend")  {p<-ggplot(dataset,aes(x=as.Date(Date.GMT),y=Sample.Measurement))
                                    q<-p+ geom_point()+ stat_smooth(method="lm",se=TRUE) + facet_wrap( ~ NAME+Site.Num,scales="free") + scale_x_date()+ xlab("") + ylab("Concentration (ug/m3)") + labs(title=chart_title) + theme(strip.text.x=element_text(colour="black")) + theme(axis.text.x=element_text(angle = 90, hjust = 1.0,colour="black"),axis.text.y=element_text(colour="black",vjust=1.0),axis.title.x = element_text(colour="grey20",size=14,face="bold"),axis.title.y = element_text(colour="grey20",size=14,face="bold",vjust=0.2))
                                                                           
                                 
    }  else {
        if (input$chart_type=="Barplot"){p<-ggplot(dataset,aes(factor(Year.GMT))) 
                                       q<-p +geom_bar() + facet_wrap( ~ NAME+Site.Num,scales="free") + ylab("# of Samples Collected") + xlab("") + labs(title=chart_title) + theme(axis.text.x=element_text(angle = 90, hjust = 1.0,colour="black"),axis.text.y=element_text(colour="black",vjust=1.0),axis.title.x = element_text(colour="black",size=14,face="bold"),axis.title.y = element_text(colour="grey20",size=14,face="bold",vjust=0.2))
                                                                        
               
     }   else {
         if (input$chart_type=="Histogram")
                                  
                                  {p<-ggplot(dataset,aes(x=Sample.Measurement))
                                  q<-p + geom_histogram() + facet_wrap( ~ NAME+Site.Num,scales="free") + xlab("Concentration") + ylab("Count") + labs(title=chart_title) + theme(axis.text.x=element_text(angle = 90, hjust = 1.0,colour="black"),axis.text.y=element_text(colour="black",vjust=1.0),axis.title.x = element_text(colour="black",size=14,face="bold"),axis.title.y = element_text(colour="grey20",size=14,face="bold",vjust=0.2))
    
                    }}}}
  
      print(q)
    },height=675)
  
 
  output$main_plot2 <- renderPlot({
    
   wswd<-wswdInput()
   plotlab2<-input$county
   pr<- pollutionRose(wswd,ws="ws",wd="wd",pollutant="pm25_lc",main=paste("PM 2.5 Pollution Rose for",plotlab2,"County"))
   perRo<-percentileRose(wswd,ws="ws",wd="wd",pollutant="pm25_lc",percentile=c(75,95,99),main=paste0("PM 2.5 Percentile Rose Plot for ",plotlab2," County"))
   print(perRo,position=c(0,0,0.5,1),more=TRUE)
   print(pr,position=c(0.5,0,1,1))
   
    }, height=600)

  output$main_plot3 <- renderPlot({
    
    xact<-xactInput()
    plotlab2<-input$param
    
    rfc2<-rfc[rfc$Parameter==input$param,2]
    rfc3<-rfc[rfc$Parameter==input$param,3]
    
   
    pr2<-pollutionRose(xact,ws="ws",wd="wd",pollutant="param",main=paste0("Pollution Rose Plot for 2012-2013 ",plotlab2," Data"))
    perR<-percentileRose(xact,pollutant="param", percentile=c(75,95,99),main=paste0("Percentile Rose Plot for 2012-2013 ",plotlab2," Data"))
    tp<-timePlot(xact,pollutant="param",key=FALSE,ylab="Concentration (ng/m3)",main=paste0("2012-2013 Hourly ",plotlab2," Data"),ref.y=c(rfc2,rfc3),log=FALSE)
    tp2<-timePlot(xact,pollutant="param",key=FALSE,ylab="Concentration (ng/m3)",avg.time="day",main=paste0("2012-2013 Daily ",plotlab2," Data"),ref.y=c(rfc2,rfc3),log=FALSE)
    
    
    print(perR,position=c(0,0.6,0.5,1),more=TRUE)
    print(pr2,position=c(0.5,0.6,1,1),more=TRUE)
    print(tp, position=c(0,0.3,1,0.6),more=TRUE)
    print(tp2, position=c(0,0,1,0.3))
   # print(grid.text(x=0.10,y=0.3,label="--- Rfc"))
    print(grid.text(x=0.15,y=0.28,label=" --- SAT Screening Level")) 
    
    
  },   height=800)
  
#   output$mychart_ts<-renderChart({
#     
#     
#     xact2<-xactInput()
#     
#     
#     #colnames(xact2)<-c("date",input$parameter)
#     
#     
#     n1<-nPlot(param~date, data=xact2,type="lineWithFocusChart")
#     
#     
#   
# #     key<-paste(input$param,"Concentration",sep=" ")
# #     y<-paste(xact2[,2],"(ug/m3)",sep=" ")
# #     e<-xact2[,1]
# #     graph<-cbind(key,y,e)
#     
#     n1$xAxis(
#       tickFormat=
#         "#!function(d) {return d3.time.format('%b %Y')(new Date( d ));}!#"
#     )
#     lab_ts<-paste(input$param,"Concentration (ng/m3)",sep=" ")
#     n1$yAxis(axisLabel = lab_ts)
#     n1$x2Axis(
#       tickFormat=
#         "#!function(d) {return d3.time.format('%b %Y')(new Date( d ));}!#"
#     )
# #     tooltipContent = "#!function(key, y, e, graph) {
# #                 return '<h4>' + key + '</h4>' +
# #                   '<p><strong>Risk: </strong>' + y + '<br><strong>Return: </strong>' + e + '</p>';
# #               }!#"
#     
#     
#     n1$chart(margin=list(left=100),showControls=FALSE)
#     n1$set(dom="mychart_ts")
#     return(n1)
#     
#     
#   })
  
 
# output$main_plot4 <- renderPlot({
#   
#     
# #   withProgress(session, min=1, max=10, expr={
# #     for(i in 1:10) {
# #       setProgress(message = 'Calculation in progress',
# #                   
# #                   value=i)
# #       print(i)
# #       Sys.sleep(0.1)
# #     }
# #   })
#   
#   
#   emissions<-emissionsInput()
#   plotlab<-input$pollutant
#   plotlab2<-input$CountyName
#           
#   
#   chart_title<-paste("Boxplot of Emissions of",plotlab, "in",plotlab2, "County",sep=" ")
# 
#     p<-ggplot(emissions,aes(as.factor(PLANT_DESC), EmissionsTPY)) 
#     q<-p +geom_boxplot() + facet_wrap( ~ YEAR,scales="free") + ylab("Log Emissions (TPY)") + xlab("Source Description") + labs(title= chart_title) + theme(axis.text.x=element_text(angle = 90, hjust = 1.0,colour="black",face="bold"),axis.text.y=element_text(colour="black",vjust=1.0),axis.title.x = element_text(colour="grey20",size=14,face="bold"),axis.title.y = element_text(colour="grey20",size=14,face="bold",vjust=0.2)) +scale_y_log10()
#   #facet_wrap( ~ YEAR),scales="free")                                    
#  # facet_wrap( ~ NAME+Site.Num,scales="free")                                  
#                                      
#   print(q)
# },height=650)
  

  
# 
#   output$myChart <- renderChart({
#      
# 
#       emissions2<-emissionsInput()
#      # emissions<-emissions[emissions$Pollutant_Code_Desc=="Lead",]
#       #emissions2<-emissions[emissions$name=="Adams",]
#       emissions3<-data.frame(emissions2[,71],as.numeric(emissions2[,29]),emissions2[,64],emissions2[,59])
#       colnames(emissions3)<-c("LAT","EmissionsTPY","PLANT_NAME","year") 
#       emissions3<-aggregate(.~PLANT_NAME*year*LAT,data=emissions3,sum)
#               
#        #emissions3<-emissions3[complete.cases(emissions3[,2]),]
#        
#        
#       # if(length(emissions3$EmissionsTPY)))
#        
# #        plotlab<-input$pollutant
# #        plotlab2<-input$CountyName
# #        y_axis<-paste(plotlab,"Emissions (TPY)",sep=" ")
# #        chart_title<-paste("Emissions of",plotlab, "in",plotlab2, "County by Facility Type",sep=" ")
# #        
# #          
# #     p2 <- rPlot(EmissionsTPY~PLANT_NAME,data = emissions3, type = c('point'), size="EmissionsTPY", color= "year")
# #     p2$guides(color=list(numticks=length(levels(emissions3$year)),labels=as.character(levels(emissions3$year))),            
# #     y = list(min=-0.9*(min(emissions3$EmissionsTPY)),max=1.1*max(emissions3$EmissionsTPY), title = y_axis),
# #     x=list(title="Facility"))
# #         p2$addParams(height = 500, dom = 'myChart',title=chart_title)
# # #        #p2$xAxis(rotateLabels=-90)
# # #        
# # #      
# #     return(p2)
#        
# #        
# #        n9 = nPlot(EmissionsTPY ~ PLANT_DESC, data = emissions3, group = "year", size="EmissionsTPY",type = "scatterChart")
# #          n9$set(dom = 'myChart')
# #                return(n9) 
# #        d1<-dplot(EmissionsTPY~PLANT_DESC,data = emissions3, groups="YEAR", type = 'bubble',id="myChart")
# #       # d1$xAxis(rotateLabels=-90)
# #               # d1$title(text = "Emissions")
# #                d1$xAxis(type="addCAtegoryAxis")
# #                d1$yAxis(type="addMeasureAxis")
# #        d1
#   
# #        p3 <- rPlot(x = 'PLANT_NAME', y = 'box(EmissionsTPY)', group="year",data = emissions3, type = 'box')
# #        p3$addParams(height = 500, dom = 'myChart',title=chart_title)
# #        p3$guides(y = list(min=-0.9*(min(emissions3$EmissionsTPY)),max=1.1*max(emissions3$EmissionsTPY), title = y_axis),
# #        x=list(title="Facility"))
# #        p3
#        
#        # emissions3$PLANT_DESC = "tmp"
# #        
# #         bwstats = setNames(as.data.frame(boxplot(EmissionsTPY ~ PLANT_NAME|year, data = emissions3, plot = F)$stats),nm=NULL) 
# #               
# #        h2 = Highcharts$new()
# #        h2$set(series = list(list(
# #          name = 'Observations',data = bwstats)),dom="myChart",width=3500)
# #        #h2$xAxis(categories = emissions3$PLANT_DESC, title = list(text="Plants"))
# #        h2$xAxis(categories=as.character(list(emissions3$PLANT_NAME))
# #                 ,labels = list(rotation = -90))
# #        
# #        
# #        h2$yAxis(title = list(text = "Emissions"))
# #        h2$chart(type = "boxplot")
# #        #h2$set(dom = 'myChart')
# #      
# #         return(h2)
#        
# #        h9 = hPlot(x = "PLANT_DESC", y = "EmissionsTPY", size = "EmissionsTPY", color="year",data = emissions3, type = "bubble")
# #        h9$set(dom="myChart")
# #        h9$xAxis(rotateLabels=-90,categories=unique(emissions3$PLANT_DESC))
# #        h9
#        
# #        h8 = hPlot(x = "PLANT_DESC", y = "EmissionsTPY", size="EmissionsTPY",data = emissions3, 
# #                   type = "bubble")
# #        #h8$tooltip( formatter = "#! function() { return 'Facility: '     + this.point.x + 
# #                                                # 'Emissions: '    + this.point.y  + 
# #                                                # 'Year: '  + this.point.group; } !#")
# #        h8$set(dom="myChart")
# #        h8
#        
#        
# #        p2 <- nPlot(EmissionsTPY ~ PLANT_DESC, data = emissions3, type = "scatterChart", group = "YEAR")
# #        p2$xAxis(axisLabel = "Plant Description", rotateLabels = -45)
# #        p2$yAxis(axisLabel = "Emissions (TPY)")
# #      #  p2$chart(size = '#! function(d){return d.EmissionsTPY} !#',
# #       p2$chart(tooltipContent = "#! function(key, x, y){ 
# #   return 'PLANT DESCRIPTION: ' + x + '  Emissions: ' + y 
# # } !#"
# #                ,size = '#! function(d){return d.EmissionsTPY} !#')
# #        #p2$chart(reduceXTicks = FALSE)
# # #        p2$chart(tooltipContent = "#! function(key, x, y){ 
# # #   return 'x: ' + x + '  y: ' + y 
# # # } !#")
# #        p2$print("myChart", include_assets = TRUE)
# #        return(p2)
#        
#       # output$myChart <- renderChart({
#        #  emissions<-emissionsInput()
# #          emissions2<-data.frame(emissions$YEAR,emissions$EmissionsTPY,emissions$PLANT_DESC,)
# #          scatter<-gviScatterChart(emissions2)
# #          plot(scatter)
# #       
#         # emissions<-emissionsInput()
# #        # plotlab<-input$pollutant
# #         #plotlab2<-input$CountyName
# #   
# #   #emissions2<-data.frame(emissions$EmissionsTPY,emissions$PLANT_DESC,emissions$YEAR)
# # #       names(emissions2)<-c("emiss","plant","year")
# # #       #bwstats = setNames(as.data.frame(boxplot(emissions2$emiss ~ emissions2$plant, data = emissions, plot = F)$stats), 
# # #                          #nm = NULL)
# #        emissions2<-data.frame(unique(emissions$LONG,emissions$LAT,emissions$EmissionsTPY))
#          
#      h1 <- hPlot(x = "PLANT_NAME", y = "LAT", data = emissions3, type = "bubble", group = "year", size = "EmissionsTPY")
#     #h1$print("myChart")
#       h1$xAxis(labels = list(rotation = -45,align="right",y=4),categories=emissions3$PLANT_NAME,title=list(text="Facility"))
#       h1$addParams(height=600,dom = 'myChart')
#       h1$tooltip( formatter = "#! function() { return 'Year: '     + this.series.name + 
#                                               '<br/>' + 'Sum of Emissions: '    + this.point.y  + 
#                                               '<br/>' + 'Facility: '  + this.point.x; } !#")
#      return(h1)
# #    h2$xAxis(categories=as.character(list(emissions3$PLANT_NAME))
# #        #EMISSION_RELEASE_POINT_ID
# #     h1 <- rPlot(EmissionsTPY~PLANT_DESC, data = emissions, type = c("point"), color = "YEAR", size = "EmissionsTPY")
# #    h1$addParams(height = 500, dom = 'myChart')
# #     h1$xAxis(rotateLabels=-90)
# #        h1$title(text = "Emissions")
# #        h1$xAxis(categories = unique(emissions$PLANT_DESC))
# #        h1$yAxis(title = list(text = y_axis))
# #        h1$guides(y = list(title = y_axis))
# #       return(h1)
#     
# #     
# #     h1 <- Highcharts$new()
# #          h1$chart(type = "scatter")
# #          h1$series(data = c(1, 3, 2, 4, 5), dashStyle = "longdash")
# #          h1$series(data = c(NA, 4, 1, 3, 4), dashStyle = "shortdot")
# #          h1$legend(symbolWidth = 80)
# #          h1$set(dom = "myChart")
# #          return(h1)
#       
#      
#      })   
  
  output$myChart2 <- renderMap({
    
    
     site<-emissionsInput()
     all<-emissionsInput2()
    
#      all<-emissions
    all2<-data.frame(all$FacilityName,as.numeric(all$LAT),as.numeric(all$LONG),all$EmissionsTPY)
    colnames(all2)<-c("facility","x","y","emissions")
     yr<-as.factor(all[1,59])
    
     site2<-data.frame(site$FacilityName,as.numeric(site$LAT),as.numeric(site$LONG),site$EmissionsTPY)
    
     colnames(site2)<-c("facility","x","y","emissions")
     em<-aggregate(.~facility*x*y,data=site2,sum)
     em2<-as.numeric(em$emissions)
    
    em_all<-aggregate(.~facility*x*y,data=all2,sum)
    em_all2<-as.numeric(em_all$emissions)
    
#     emissions2<-as.numeric(em$emissions)
    # fillColor<-cut(emissions2,breaks=5,labels=brewer.pal(5,"Blues"))
    #fillOpacity<-cut(emissions2,breaks=5,labels=c(0.1,0.3,0.5,0.7,0.9))
    
    sum_rad_all<-sum(em_all2)
    labels_rad_all<-(em_all2/sum_rad_all)*100
     
    radius_all<-cut(labels_rad_all,breaks=5,labels=c(8,11,13,16,19))
    lab<-cut(em_all2,breaks=5,dig.lab=1)
 
    leg<-cut(em_all2,breaks=5,labels=brewer.pal(5,"Blues"))
    
#     sum_rad<-sum(emissions2)
#     labels_rad<-(emissions2/sum_rad)*100
#     radius<-cut(labels_rad,breaks=5,labels=c(8,12,16,20,24))
    
    
#     year<-paste("Year:",yr,"<br>")
#     facility<-paste("Facility:",em$facility,"<br>")
#     emissions3<-paste("Emissions:",em$emissions, "TPY")
#     popup<-paste(year,facility,emissions3)
    
    year_all<-paste("Year:",yr,"<br>")
    facility_all<-paste("Facility:",em_all$facility,"<br>")
    emissions3_all<-paste(input$pollutant,"Emissions:",em_all$emissions, "TPY")
    popup_all<-paste(year_all,facility_all,emissions3_all)
    
    em2_all<-cbind(em_all,leg,radius_all,popup_all)
    em_lat<-mean(em$x)
    em_lon<-mean(em$y)
        
    #hap<-unique(site$Pollutant_Code_Desc)
    #hap2<-first.word(hap)
    #chart_title<-paste(unique(site$name),"Co.\n",hap2, "Emissions",sep=" ")
        
    map3 <- Leaflet$new()
    map3$setView(c(em_lat,em_lon),zoom = 10)
    map3$tileLayer(provider="Stamen.Terrain",maxZoom=18)
    
                
    dat_list_all <- toJSONArray2(em2_all, json = F)
    
    
    map3$geoJson(toGeoJSON(dat_list_all, lat = 'x', lon = 'y'),
                 onEachFeature = '#! function(feature, layer){
    layer.bindPopup(feature.properties.popup_all)
 } !#',
                 pointToLayer =  "#! function(feature, latlng){
    return L.circleMarker(latlng, {
      radius: feature.properties.radius_all,
      fillColor: feature.properties.leg,
      color: '#000',
      weight: 1,
      fillOpacity: 0.8
    })
 } !#"         
    )
    
#     leg2<- c("red"="high","yellow"="medium","green"="low")
  # leg3<- c("0-94] (0.894,1.8] (1.8,2.7] (2.7,3.6] (3.6,4.5] (4.5,5.4] (5.4,6.3] (6.3,7.21]
    leg1<-levels(lab)
    leg2<- cbind(lower = signif(as.numeric( sub("\\((.+),.*", "\\1", leg1) ),1),
          upper = signif(as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", leg1) )),1)
   # leg3<-as.character(leg2[,2])
    
    leg4<-paste(as.numeric(leg2[,1]),as.numeric(leg2[,2]),sep="-")
    leg5<-paste(leg4, " (TPY)")
    
    map3$legend(position="bottomright", colors=brewer.pal(5,"Blues"),labels=leg5)
    
    map3
   # feature.properties.radius_all,
    
  })
  
  
})


