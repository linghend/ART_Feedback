#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("CAR Feedback Study"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         # sliderInput("bins",
         #             "Number of bins:",
         #             min = 1,
         #             max = 50,
         #             value = 30),
         selectInput("ARTRelease", label = h3("ART Releases"), 
                     #choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                     choices=as.list(unique(as.Date(Uti$TimeStamp))),
                     selected = 1)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         #plotOutput("distPlot")
         #fluidRow(column(3, verbatimTextOutput("value")))
        #fluidRow(column(3, textOutput("value")))
        textOutput("text0"),
        textOutput("text1"),
        textOutput("text2"),
        textOutput("text3"),
        dataTableOutput("CARTeam")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   # output$distPlot <- renderPlot({
   #    # generate bins based on input$bins from ui.R
   #    x    <- faithful[, 2] 
   #    bins <- seq(min(x), max(x), length.out = input$bins + 1)
   #    
   #    # draw the histogram with the specified number of bins
   #    hist(x, breaks = bins, col = 'darkgray', border = 'white')
   # })
  ART_FB %>% group_by(TimeStamp) %>% summarize(avg_RiskProb = mean(RiskProb))
  #print(input$ARTRelease)
  CAR_FB = ART_FB %>% filter(TimeStamp=='2018-12-31')
  #CAR_FB = ART_FB %>% filter(TimeStamp==as.character(input$ARTRelease))
  Rec_Cnt<-CAR_FB %>% summarize(RecCnt = length(unique(Cnno)))
  Int_Cnt<-CAR_FB %>% filter((!is.na(CAR_Interaction_ID))) %>% summarize(IntCnt = length(unique(Cnno)))
   #output$value <- renderText({ sprintf("# of Rec.: %d;  # of Cnnos with Interactions: %d", as.integer(Rec_Cnt), as.integer(Int_Cnt)) })
  #output$text1 <- renderText({sprintf("# of Rec.: %d",as.integer(Rec_Cnt))})
  output$text0 <- renderText({paste0("ART Release: ",input$ARTRelease)})
  output$text1 <- renderText({sprintf("# of Rec.: %d",as.integer(
    ART_FB %>% filter(TimeStamp==input$ARTRelease) %>% summarize(RecCnt = length(unique(Cnno)))))})
  output$text2 <- renderText({sprintf("# of Cnno with Interactions: %d",as.integer(
    ART_FB %>% filter(TimeStamp==input$ARTRelease) %>% filter((!is.na(CAR_Interaction_ID))) %>% summarize(IntCnt = length(unique(Cnno)))))})
  output$text3 <- renderText({sprintf("Utilization Ratio: %.1f%%",
      (ART_FB %>% filter(TimeStamp==input$ARTRelease) %>% filter((!is.na(CAR_Interaction_ID))) %>% summarize(IntCnt = length(unique(Cnno))))*1000/(ART_FB %>% filter(TimeStamp==input$ARTRelease) %>% summarize(RecCnt = length(unique(Cnno))))/10.0)})
  #Rec_TM_Cnt= ART_FB %>% filter(TimeStamp=='2018-12-31') %>% group_by(CAR_TEAM_CODE) %>% summarize(RecCnt = length(unique(Cnno)))
  #Int_TM_Cnt= ART_FB %>% filter(TimeStamp=='2018-12-31') %>% filter((!is.na(CAR_Interaction_ID))) %>% group_by(CAR_TEAM_CODE) %>% summarize(Int_Cnt = length(unique(Cnno)))
  #Uti<-merge(Rec_TM_Cnt,Int_TM_Cnt, by="CAR_TEAM_CODE",all=T)
  #Uti[is.na(Uti$Int_Cnt),"Int_Cnt"]=0
  #Uti$Ratio=as.numeric(Uti$Int_Cnt/Uti$RecCnt)
  output$CARTeam=renderDataTable((
    merge(
      ART_FB %>% filter(TimeStamp==input$ARTRelease) %>% group_by(CAR_TEAM_CODE) %>% summarize(Rec_Cnt = length(unique(Cnno))),
      ART_FB %>% filter(TimeStamp==input$ARTRelease) %>% filter((!is.na(CAR_Interaction_ID))) %>% group_by(CAR_TEAM_CODE) %>% summarize(Int_Cnt = length(unique(Cnno))),
      by="CAR_TEAM_CODE",all=T 
    ) 
  )
  )
  #table(as.vector((ART_FB %>% filter(TimeStamp=='2018-12-31'))$CAR_TEAM_CODE))
}

# Run the application 
shinyApp(ui = ui, server = server)

