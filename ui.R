shinyUI(pageWithSidebar(

   headerPanel(h3("Xact Data Explorer"),windowTitle="Xact Data Explorer")

   ,

   sidebarPanel(
     selectInput("site", "Choose a data file:", c("")),
      conditionalPanel(condition="input.contab==1"
                      
                       ,

                       
                       fileInput('upData', "Upload Xact CSV File", multiple = FALSE,
                                 accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
                       
                     
      )     
                   ,    

                       
      conditionalPanel(condition="input.contab==2",
                       
                       uiOutput("param"),
                       
                       p("Please enter the lat-long for your site to generate a site map"),
                       
                       uiOutput("lat"),
                       
                       uiOutput("long"),
                       
                       p(textOutput("map_text")),
                       
                       showOutput('mymap','leaflet'),
                       
                       br(),
                       
                       checkboxInput("log","Check to view log-scale on y-axis",value=FALSE)
                                               ,
                       br(),
                       
                       downloadButton('downloadData', 'Download Data')
                            
       
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
    
    tabPanel("Upload & View Raw Xact Data",value=1,
             
             dataTableOutput('xactTable')
             
    )
    , 
    

    tabPanel("Xact Monitoring",value=2,
             
              h5(textOutput("head_text")),
             
             plotOutput("main_plot",height="auto")
    ),
             
       
     
          id="contab")
        )
  )
     
)

  
