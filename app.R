library(shinydashboard)
library(datasets)

ui<- dashboardPage(
  dashboardHeader(title = "Time Series"),
  dashboardSidebar(
    sidebarMenu(
    selectInput("dataset", "Choose Dataset", choices = c("Air Passengers","Australian total wine sales","Australian monthly gas production")),
    numericInput("freq","Choose period to forecast",10),
    div(style="padding-left:60px;",submitButton("Submit")),
    br(),
    menuItem("ETS", tabName = "ETS"),
    menuItem("ARIMA", tabName = "ARIMA"),
    menuItem("Decomposition", tabName = "Decomposition")
  )
  ),
  dashboardBody(
    tabItems(
      tabItem("ETS", plotOutput("ets")),
      tabItem("ARIMA", plotOutput("arima")),
      tabItem("Decomposition", plotOutput("decompose"))
    )
  )
)

server<- function(input,output){
  dataset<- reactive({
    switch(input$dataset,
           "Air Passengers"=AirPassengers,
           "Australian total wine sales"=wineind,
           "Australian monthly gas production"=gas
           )
  })
  library(forecast)
  output$decompose<- renderPlot({
    p<- ts(dataset(), frequency = 10)
    q<- decompose(p)
    plot(q)
  })
  
  output$arima<- renderPlot({
    fit<- auto.arima(dataset())
    plot(forecast(fit, h=input$freq))
  })
  
  output$ets<- renderPlot({
    fit<- ets(dataset())
    plot(forecast(fit, h=input$freq))
  })
}

shinyApp(ui, server)
