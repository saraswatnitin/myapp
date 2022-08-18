
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



#setwd("D:/Downloads_D/mobility_data/v2/demo")
source("./gbm_train1.R")


#data1<-read.csv("D:/Downloads_D/mobility_data/v2/demo/labelled_train_features_data_wpct_forgbm.csv",sep=',')

data1<-read.csv("./labelled_train_features_data_wpct_forgbm1.csv",sep=',')


data2<-data1[c(1,3,4,5,6,21)]

head(data2)

ui <-  fluidPage(
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









server <- function(input, output) {
  filtered_data <- reactive({
    subset(data2,
           Ethnic %in% input$Ethnic)})
  
  
  output$plot_population = renderPlot({   # bar graph of counts of selection done
    fd = filtered_data()
    a<- fd %>% count(Ethnic)  
    #a
    b<- a %>% summarize(percent(n/769))
    #b
    c<-cbind(a,b)
    #c
    colnames(c)<-c("Ethnicity","Count","Pct")
    
    #ggplot(data=c,mapping = aes(x=Ethnicity,y=Pct))+ylab("Percentage")+geom_bar(stat="identity",aes(fill = Pct)) +labs(fill = "Percentage")+theme_classic()
    
    ggplot(data=c,mapping = aes(x=Ethnicity,y=Pct))+ylab("Percentage")+geom_bar(stat="identity",aes(fill = Pct)) +
      geom_text(stat='identity',aes(label=Pct),vjust=-0.7)+theme_classic()+theme(legend.position="none")
 
    
  })
  
  
  output$table <- DT::renderDataTable({
    filtered_data()
  })
  
  output$download_data <- downloadHandler(
    filename = "D:/Downloads_D/mobility_data/v2/demo/to_download.csv",
    content = function(file) {
      data3 <- filtered_data()
      write.csv(data3, file, row.names = FALSE)
    }
  )
  
  #upload download begins
  
  options(shiny.maxRequestSize = 800*1024^2)   # This is a number which specifies the maximum web request size, 
  # which serves as a size limit for file uploads. 
  # If unset, the maximum request size defaults to 5MB.
  # The value I have put here is 80MB
  
  
  output$sample_input_data_heading = renderUI({   # show only if data has been uploaded
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      tags$h4('Sample data')
    }
  })
  
  output$sample_input_data = renderTable({    # show sample of uploaded data
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      input_data1 =  readr::read_csv(input$file1$datapath, col_names = TRUE)
      input_data<-as.h2o(input_data1)
      input_data2<-input_data1[,1:6]
      
      head(input_data2)
    }
  })
  
  predictions<-reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      withProgress(message = 'Predictions in progress. Please wait ...', {
        input_data1 =  readr::read_csv(input$file1$datapath, col_names = TRUE)
        input_data6<-as.h2o(input_data1)  #5:18
        input_data<-input_data6[,5:18]
        input_data_name<-input_data6[,1:4]
        
        
        
        prediction1 = predict(race_gbm, newdata=input_data)
        #concatenate original data and predicted data 
        prediction2<-h2o.cbind(input_data_name,prediction1$predict)
        
        prediction<-as.data.frame(prediction2)
        
      })
    }
  })
  
  output$sample_prediction_heading = renderUI({  # show only if data has been uploaded
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }else{
      tags$h4('Sample predictions')
    }
  })
  
  output$sample_predictions = renderTable({   # the last 6 rows to show
    pred1 = predictions()
    pred<-as.h2o(pred1)
    head(pred)
    
  })
  
  
  output$plot_predictions = renderPlot({   # the last 6 rows to show
    pred = predictions()
    
    ggplot(data=pred, aes(x = pred$predict,fill=pred$predict)) + xlab("Predicted Ethnicity")+geom_bar()+ 
      geom_text(stat='count', aes(label=..count..),vjust=-0.7)+labs(fill = "Ethnicity Legend")+theme_classic()+
      theme(legend.position="none")
    
  })
  
  
  # Downloadable csv of predictions ----
  
  output$downloadData <- downloadHandler(
    
    filename = "./pred_download.csv",
    content = function(file) {
      data8 <- predictions()
      #prediction2<-h2o.cbind(input_data_name,prediction1$predict)
      write.csv(data8, file, row.names = FALSE)
    }
  )
  
  
  #upload download ends
}







shinyApp(ui = ui, server = server)
