 shinyUI(pageWithSidebar(
#   
   headerPanel(h3("Xact Data"),windowTitle="Xact Data Explorer"),
   sidebarPanel(
  #  progressInit(),

#     conditionalPanel(condition="input.contab==1",
#     
#                      uiOutput("availparam") ,    
#          
#                      selectInput(inputId = "chart_type",
#                                  label = "Chart type",
#                                  choices = c("Boxplot",
#                                              "Trend",
#                                              "Barplot",
#                                              "Histogram"),selected="Trend"),
#                       
# 
#                       #   uiOutput ("choosedata"),
#                      uiOutput("availcounty"),
#                      
#                     
#                      sliderInput(inputId="year",
#                                label= "Year", min=2000, max=2013,
#                               value=c(2000,2013), format="####",ticks=TRUE),
#                      
#                      downloadButton('downloadData', 'Download'),
#                      
#                     # plotOutput("side_plot",width="85%")
#                      plotOutput("sideplot1",width="100%")
#                      ),
#     
#   
#       conditionalPanel(condition="input.contab==2",
#                   selectInput(inputId ="county",
#                              label = "Choose a county:",
#                                choices = sort(unique(wswd_names$x)), selected="Lake"),
#                        
#                        sliderInput(inputId="Year",
#                                    label= "Year", min=2006, max=2013,
#                                    value=c(2006,2013), format="####",ticks=TRUE),
#                        
#                        sliderInput(inputId="Month",
#                                    label= "Month", min=1, max=12,
#                                    value=c(1,12), format="##",ticks=TRUE)
#           
#                        
#                       , plotOutput("sideplot2",width="100%")
#                   ),
    
      conditionalPanel(condition="input.contab==1"
                       ,
                       
                       selectInput("param", "Choose a parameter:", 
                                  choices=c(sort(unique(xact_param))),
                                  selected="Manganese"),
#                        dateInput("Date", label="Choose a date:", value = 2012-10-25, min = 2012-10-25,
#                                  max = 2013-04-07, format = "yyyy-mm-dd", startview = "month",
#                                  weekstart = 0, language = "en"),
                       
                       h6("Map of Gary Xact Monitor and Surrounding Area"),
                       
#                        showOutput('myChart3','leaflet')
                       htmlOutput("sideplot3")
                     ),
#     
#     conditionalPanel(condition="input.contab==4"
#                       ,
#                     
#                     
#                      uiOutput("availcomp"),
#                      uiOutput("availemisscounty"),
#                      uiOutput("inRadio")
#                      
#                      
#                      
#                     # plotOutput("sideplot4",width="100%")
#   )

    ,
    conditionalPanel(condition="input.contab==2"
                     ,
          h6("this is info about the tool")

                     
    )
    )
  ,              

    ),
mainPanel(
    
 
    tabsetPanel(
    
    tabPanel("Xact Monitoring",value=1,
             
#              showOutput("mychart_ts","nvd3")),
             plotOutput("main_plot3",height="auto")),
    

    tabPanel("About",value=2,
             h6("this is info about the tool"))
    ,
             
             
                id="contab")
  )
  )
        )



