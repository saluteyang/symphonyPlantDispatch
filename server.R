
# options(shiny.error = browser)

shinyServer(function(input, output){
  simOut <- reactive({
    if(input$goButton1 ==0)
      return()
    getSeries(s0 = input$initPrice, sigma = input$annVol, nsims = input$simNum)
  })
  
  selectPaths <- reactive({
    if(input$goButton1 == 0)
      return()
    selectPaths <- as.data.frame(simOut()[, (1:5)])
    selectPaths <- cbind.data.frame(Time = Sys.Date() + 1:dim(selectPaths)[1],
                                    selectPaths)
    colnames(selectPaths)[2:6] <- c('data1', 'data2', 'data3', 'data4', 'data5')
  })
  
  # output$table <- renderTable({
  #   if(input$goButton1 == 0)
  #     return()
  #   selectPaths <- as.data.frame(simOut()[, (1:5)])
  #   selectPaths <- cbind.data.frame(Time = Sys.Date() + 1:dim(selectPaths)[1],
  #                                   selectPaths)
  #   colnames(selectPaths)[2:6] <- c('data1', 'data2', 'data3', 'data4', 'data5')
  #   head(selectPaths, n = 10)
  #   })
  
  output$chart <- renderC3LineBarChart({
    C3LineBarChart(dataset = selectPaths(),
                   colors = list(data1 = "blue", data2 = "green", data3 = "gray", data4 = "red", data5 = "purple")
                   # types = list(data1 = "line", data2 = "line", data3 = "line", data4 = "line", data5 = "line")
                   # groups = c("data1", "data2", "data3", "data4", "data5")
                   )
  })
  
  # output$chart <- renderPlot({
  #   if(input$goButton1 == 0)
  #     return()
  #   matplot(selectPaths(), type = "l")
  #   })
})
  

