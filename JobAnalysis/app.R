library(shiny)
library(tidyverse)
library(magrittr)
library(shinycssloaders)
library(shinythemes)
library(wordcloud2)
library(tm)

cloudgenerator<-function(input){
  text<-input$contents
  docs<-Corpus(VectorSource(text))
  
  docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
  docs <- tm_map(docs, removeWords, stopwords("english"))
  dtm <- TermDocumentMatrix(docs) 
  matrix <- as.matrix(dtm) 
  words <- sort(rowSums(matrix),decreasing=TRUE) 
  df <- data.frame(word = names(words),freq=words)
  return(df)
}


museset<-readRDS("default.rds")
museset%<>%select("name","contents","locations","company.name")
button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;

/* Change the text size to 15 pixels. */
font-size: 15px;
}"

ui<-fluidPage(
  titlePanel("Jobs Analysis Project"),
  hr(),
  tags$h5("Created by Yinfeng(Steven) Zhou from Boston University"),
  hr(),
  navbarPage("Quick Stats for Jobs",theme=shinytheme("lumen"),
             tabPanel("The Muse Company WordCloud Comparison",fluid=TRUE,icon=icon("table"),
                      
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Companies Comparison"),
                          tags$h5("This app runs slow on shinyapps.io. If the plot does not show, simply select another company and switch back will solve the problem"),
                          hr(),
                          tags$h5("This datset contains the first 1000 job searching results from The Muse"),
                          fluidRow(column(6,
                                          
                                          hr(),
                                          selectInput(inputId="CompanyFinder1",
                                                      label = "Company 1",
                                                      choices = unique(museset$company.name),
                                                      selected = head(unique(museset$company.name),1)),
                                          selectInput(inputId="CompanyFinder2",
                                                      label = "Company 2",
                                                      choices = unique(museset$company.name),
                                                      selected = head(unique(museset$company.name),2))
                                          ))
                        ),
                        mainPanel(
                          h3("Company1 WordCloud"),
                          hr(),
                          fluidRow(column(6,withSpinner(wordcloud2Output('wordcloud1'))),
                                   column(6,withSpinner(plotOutput("hist1")))),
                          hr(),
                          h3("Company2 WordCloud"),
                          fluidRow(column(6,withSpinner(wordcloud2Output('wordcloud2'))),
                                   column(6,withSpinner(plotOutput("hist2")))),
                          hr(),
                          # h3("Company Word Histogram"),
                          # fluidRow(withSpinner(plotOutput(outputId = "sethist"))),
                          # fluidRow(withSpinner(plotOutput(outputId = "companyhist"))
                          # )

                        )
                      )
             ),
             tabPanel("More",icon=icon("info-circle"),
                      fluidRow(
                        hr(),
                        h3("About this Project"),
                        h4("This project began as final project for MA615(Data Science in R) of MSSP program in Boston University. More function might be developed in the future."),
                        h4("The project is intended to make some quick stats on dataset downloaded from jobs API."),
                        h4(p("These data were collected through API from",a("The Muse",href="https://www.themuse.com/developers"),".")),
                        h4(p("Source code can be accessed from ",a("github",href="https://github.com/SteveM3ister/615Final.git"),".")),
                        hr(),
                        h5("Built with",
                           img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                           "by",
                           img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                           "."))
             )
             
  )
)


server<-function(input,output,session){
  datasetInput1 <- reactive({
    input<-museset%>%filter(company.name == input$CompanyFinder1)%>%cloudgenerator()

  })
  
  datasetInput2 <- reactive({
    input<-museset%>%filter(company.name == input$CompanyFinder2)%>%cloudgenerator()
    
  })

  
  output$hist1<-renderPlot({
    datasetInput1()%>%head(n=10)%>%
      mutate(word = reorder(word, freq)) %>%
      ggplot(aes(x=word, y=freq)) +
      geom_bar(stat="identity", fill="#f68060", alpha=.8, width=.4) +
      theme(axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
            axis.text.y = element_text(colour="grey20",size=16,angle=0,hjust=1,vjust=0,face="plain"),
            axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
            axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"),
            plot.title = element_text(colour="#619CFF",size=20))+
      coord_flip() +
      xlab("Word")+
      geom_text(aes(label=freq))+
      ggtitle("Dataset Histogram")
  })
  
  output$hist2<-renderPlot({
    datasetInput2()%>%head(n=10)%>%
      mutate(word = reorder(word, freq)) %>%
      ggplot(aes(x=word, y=freq)) +
      geom_bar(stat="identity", fill="#f68060", alpha=.8, width=.4) +
      theme(axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
            axis.text.y = element_text(colour="grey20",size=16,angle=0,hjust=1,vjust=0,face="plain"),
            axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
            axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"),
            plot.title = element_text(colour="#619CFF",size=20))+
      coord_flip() +
      xlab("Word")+
      geom_text(aes(label=freq))+
      ggtitle("Dataset Histogram")
  })
  output$wordcloud1<-renderWordcloud2({
    wordcloud2(data=head(datasetInput1(),100), size=0.5, color='random-dark')})
  
  output$wordcloud2<-renderWordcloud2({
    wordcloud2(data=head(datasetInput2(),100), size=0.5, color='random-dark')})

  # 
  # observe({
  # output$companyhist<-renderPlot({
  #     datasetInput()%>%head(n=10)%>%
  #       mutate(word = reorder(word, freq)) %>%
  #       ggplot(aes(x=word, y=freq)) +
  #       geom_bar(stat="identity", fill="#f68060", alpha=.8, width=.4) +
  #       theme(axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
  #             axis.text.y = element_text(colour="grey20",size=16,angle=0,hjust=1,vjust=0,face="plain"),  
  #             axis.title.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=0,face="plain"),
  #             axis.title.y = element_text(colour="grey20",size=12,angle=90,hjust=.5,vjust=.5,face="plain"),
  #             plot.title = element_text(colour="#619CFF",size=20))+
  #       coord_flip() +
  #       xlab("Word")+
  #       geom_text(aes(label=freq))+
  #       ggtitle("Company Histogram")
  #   
  # 
  # })
  # })

  
  
  

  

}



shinyApp(ui=ui,server=server)


