
library(shiny)
library(shinydashboard)
library(shinythemes)
library(plotly)
library(DT)
library(shinyWidgets)
library(readr)
library(ggplot2)
library(scales)
library(dplyr)



source("./gbm_train1.R")


data1<-read.csv("./labelled_train_features_data_wpct_forgbm1.csv",sep=',')


data2<-data1[c(1,3,4,5,6,21)]

head(data2)

#b64 <- base64enc::dataURI(file = "https://s3.console.aws.amazon.com/s3/buckets/forcogimage?region=ap-south-1/cogent_monogram.png", mime = "image/png")

shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(width=6,
                 
                 
                 
                 
                 
                 #tags$h5(tags$img(src="cogent_monogram.png", alt = "This message should not appear", width = "50px", height = "50px"),
                 #           tags$span(style="color:black","ARIA [Sandbox]")),
                 
                 tags$div(`data-value` = "test",tags$img(src="cogent_monogram.png",width="43px" , height= "43px", align="left"),
                          tags$span(style="color:black",HTML('&nbsp;&nbsp;')),
                          tags$span(style="color:gray","ARIA [Sandbox]"),tags$br(),
                          tags$span(style="color:black",HTML('&nbsp;&nbsp;')),
                          tags$span(style="color:black;font",HTML("<b>Diversity Finder [version 2.3] </b>"))
                 ),
                 #tags$span(style="color:black",HTML('&nbsp;&nbsp;&nbsp;&nbsp;')),
                 #tags$span(style="color:black","(c) Cogent Infotech")), 
                 
                 br(),
                 br(),
                 multiInput(inputId = "Ethnic", label = "Filter Records by Ethnicity",
                            #choices = c("ASIAN", "BLACK", "HISPANIC","MULTI-ETHNIC","WHITE","NATIVE AMERICAN"),
                            choices = c("ASIAN", "BLACK", "HISPANIC","WHITE","NATIVE AMERICAN"),
                            selected = "ASIAN"),
                 
                 
                 # downloadButton(outputId = "download_data", label = "Download Results"),
                 
                 br(),
                 br(),
                 
                 
                 
                 #upload download begins
                 dashboardPage(skin="black",
                               
                               
                               
                               dashboardHeader(title=tags$strong("Ethnicity Prediction App - Upload New Data", style="text-align:left;color:#000000;font-size:90%"),titleWidth = 650),
                               
                               dashboardSidebar(width = 150,
                                                
                                                sidebarMenu(
                                                  br(),
                                                  menuItem(tags$em("Upload Data",style="font-size:100%"),icon=icon("upload"),tabName="data"),
                                                  menuItem(tags$em("Analyse Data",style="font-size:100%"),icon=icon("download"),tabName="download")
                                                  
                                                  
                                                )
                               ),
                               
                               dashboardBody(tags$head(tags$style(HTML("body{min-height: 200px !important; }")) ),
                                             tabItems(
                                               tabItem(tabName="data",
                                                       
                                                       
                                                       
                                                       #br(),
                                                       #tags$h4("Ethnicity Prediction App"),
                                                       
                                                       
                                                       
                                                       tags$h4("Upload test data in csv format", style="font-size:100%"),
                                                       
                                                       
                                                       column(width = 12,
                                                              fileInput('file1',
                                                                        em(''),
                                                                        multiple = FALSE,
                                                                        accept=c('.csv'))
                                                              
                                                       ),
                                                       
                                                       
                                               ),
                                               
                                               
                                               tabItem(tabName="download",
                                                       
                                                       fluidRow(
                                                         
                                                         column(width = 12,
                                                                downloadButton("downloadData", em('Download Predictions',style="text-align:center;color:blue;font-size:150%")),
                                                                br(),
                                                                br(),
                                                                br(),
                                                                
                                                                plotOutput('plot_predictions')
                                                         ),
                                                         
                                                         
                                                       ))
                                             ))
                 ),
                 #upload download ends
                 
                 
                 
                 
    ),
    mainPanel(width=6,
              
              br(),
              tags$h5(tags$span(style="color:grey","Showing a sample of 769 records from a database of 1.3 Million candidates")),
              plotOutput('plot_population'),
              br(), 
              downloadButton(outputId = "download_data", label = "Download Results"),
              br(),
              br(),
              br(),
              DT::dataTableOutput(outputId = "table")
    )
  )
)
)




