
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
library(sqldf)



source("./gbm_train1.R")


data1<-read.csv("./labelled_train_features_data_wpct_forgbm1.csv",sep=',')


data2<-data1[c(1,3,4,5,6,21)]

head(data2)

#b64 <- base64enc::dataURI(file = "https://s3.console.aws.amazon.com/s3/buckets/forcogimage?region=ap-south-1/cogent_monogram.png", mime = "image/png")

shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(width=6,
                 
                 
                 
                 
                 
                 #tags$h5(tags$img(src="cogent_monogram.png", alt = "This message should not appear", width = "50px", height = "50px"),
                 #           tgs$span(style="color:black","ARIA [Sandbox]")),
                 
                 tags$div(`data-value` = "test",tags$img(src="https://forcogimage.s3.ap-south-1.amazonaws.com/cogent_monogram.png?response-content-disposition=inline&X-Amz-Security-Token=IQoJb3JpZ2luX2VjEKT%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEaCmFwLXNvdXRoLTEiRjBEAiAkvNEWAoyQHwiSAARSJ63XvXtg%2FqmWKrgZRJsbM3kHMQIgMuBY2eGFKasE0Tm0%2BRuFO3H7wII2BTQHcrCJtNVly78q5AIIHRAAGgw2ODI2NDkwMjIyMTkiDOc0QWnPiWmHLSwUjyrBAtv2nJU28uOBupVkWyyl9IdjyiciM0BdTNBHOdsmPh9JO3sFJnFc2niDSqmRjQuVpguyYmWwWlx8raGuF4xUvnt9fOqeSWx091oM2prAY9oRiKapksBhRHDLscQc8p%2BZ56%2BWQLgydM4FJV93d3baCxdDx3hyNRJO8S7dC2N6bJvKaN0QXn%2Bb6v8ay3V4EZpFWm95%2FdhHOIg094kLeQUvc7N%2F97xJTsP88FGNtjK5zEGuSbpMZab6yZ%2BbECnBBlHMa2l1CgibItTzwr92lj8uFZjUY36crD3V9YQBBWsR0thIGj32WBvNwMjWykrgnayUbojrvkPeLFiF5XKNNiDhVl8%2Bfq%2BT%2FBr64ZZ8hiSoWZd64BB0KHWOGUcH%2BOz9oSq%2B6LyN53jmiam9d1WdruhxsGMdY45rfXvrPLMY6kP1MK760zCctvqXBjq0AliZBpxhJk5%2FRdOXSmfb85yKQQ5mgGPXfHynTzGqj4csvFKdYKS2h7PU7ZaGwzRUFlX93CFUkh7ixpf1qiEvDmyfLoFItX43Hfn18yWASsRhDcl2ef0M63ppvG7ZneodVjhYWaex5Dq5IrqmVdq7JuUD7Pj4BvBQFwhveqcwnuC8fBLmMh1mNYeKvDodfIvjCDcPmQtoMHrHepnc0jWOKFspM6QnBPUuFDnJZgBZcWtYdV44if66E78Ca%2B0xxEhU5J8F5U4nqXRWhG1CndO7bB%2B1qHlT03tICpLKJYQQfPDHqrT9FJpG1DH2ztBjFolGxKkz5Av0ohi7BeA8RG87h6kcEw01WSJEpUupXTYVJPk8u5%2B6Dv3j2u26Mq1b5pu3jL98Ces6iX42UzQeoodcPfi%2BAZyb&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20220819T052953Z&X-Amz-SignedHeaders=host&X-Amz-Expires=43200&X-Amz-Credential=ASIAZ54INN4FWKGJPWPX%2F20220819%2Fap-south-1%2Fs3%2Faws4_request&X-Amz-Signature=0e7270ee3275c57e8eeea9e2c30e5914e5b99a3ff03aea2a4a9f8c20aaf1b8e4",width="43px" , height= "43px", align="left"),
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
              #plotOutput('plot_population'),
              splitLayout(cellWidths = c("65%", "35%"), plotOutput("plot_population"), plotOutput("gender_breakup")),
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




