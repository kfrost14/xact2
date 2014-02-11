shinyUI(pageWithSidebar(

   headerPanel(h3("Xact Data Explorer"),windowTitle="Xact Data Explorer")

   ,

   sidebarPanel(
    
      conditionalPanel(condition="input.contab==1"
                       ,
                       
                       
                       uiOutput("site"),
                       uiOutput("param"),
                                         
                       p("If using Burns Harbor data, please enter the lat-long to generate a site map"),
                       uiOutput("lat"),
                       uiOutput("long"),
                       p(textOutput("map_text")),
                       showOutput('mymap','leaflet'),
#                        htmlOutput("sideplot"),
                       br(),
                      checkboxInput("log","Check to view log-scale on y-axis",value=FALSE)
#                        ,
#                      downloadButton('downloadData', 'Download')

                      ),
                       
      conditionalPanel(condition="input.contab==2"
                       ,
                       
                       
                       fileInput('upDataUSS', "Upload US Steel Xact CSV Files", multiple = FALSE,
                                 accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                       fileInput('upDataBH', "Upload Burns Harbor Xact CSV Files", multiple = FALSE,
                                 accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
                       
              

      
           )
      )
   ,
              
  mainPanel(
    tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                    ".shiny-output-error:before { visibility: hidden; }",
                    "h4 { color: red; }")
   ,
    
    tabsetPanel(
    
    tabPanel("Xact Monitoring",value=1,
             
#                   h5(textOutput("head_text")),
              
           
             h4(textOutput("err_message")),
                  plotOutput("main_plot",height="auto")
                  ),
    

    tabPanel("Upload & View Raw Xact Data",value=2,
             
             tableOutput('xactTable')
             
             )
        ,        
     
          id="contab")
        )
  )
     
)

  
