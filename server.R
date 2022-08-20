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
library(h2o)

h2o.init()

data1<-read.csv("./labelled_train_features_data_wpct_forgbm1.csv",sep=',')


data2<-data1[c(1,3,4,5,6,21)]

shinyServer(function(input, output) {
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
  

   output$gender_breakup = renderPlot({   
    
    #donut chart 
    fd = filtered_data()
    data_gen<- sqldf('select Gender,count(*) as CNT from fd group by Gender')
    data_gen$fraction = data_gen$CNT / sum(data_gen$CNT)
    data_gen$pct<- data_gen$fraction * 100
    
    hsize <- 3
    
    data_gen <- data_gen %>% 
      mutate(x = hsize)
    
    data_gen$fraction<-round(data_gen$fraction, 2)
    
    data_gen$fraction<-round(data_gen$fraction, 2)
    data_gen$pct<-round(data_gen$pct, 2)
    
    
    ggplot(data_gen, aes(x = hsize, y = fraction, fill = Gender)) +
      geom_col(color = "black") +
      geom_text(aes(label = paste0(pct,'%')),
                position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y") +
      #scale_fill_brewer(palette = "BuPu") +
      scale_fill_manual(values = c("orange", "steelblue"))+
      xlim(c(0.2, hsize + 0.5)) +
      theme(panel.background = element_rect(fill = "white"),
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank())+
      labs(title = "Gender Breakup")+
      theme(plot.title = element_text(hjust = 0.5))
    
  })
 
  
  output$table <- DT::renderDataTable({
    filtered_data()
  })
  
  output$download_data <- downloadHandler(
    filename = "./to_download.csv",
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
      
      write.csv(data8, file, row.names = FALSE)
    }
  )
  
  
  #upload download ends
})


