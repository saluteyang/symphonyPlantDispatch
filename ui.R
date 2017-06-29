
shinyUI(navbarPage(theme = shinytheme("cerulean"),
                   "Simplified Plant Dispatch",
                   tabPanel("Price simulations",
                            sidebarPanel(
                              helpText("Generate the hourly power curve for use
                                       with dispatch optimization"),
                              sliderInput("simNum", label = h4("Number of Sims"),
                                          min = 100, max = 1000, step = 100, value = 100),
                              numericInput("initPrice", label = h4("Initial price"),
                                           min = 20, max =80, step = 1, value = 30),
                              numericInput("annVol", label = h4("Annualized volatility"),
                                           min = 0.1, max = 0.8, step = 0.05, value = 0.3),
                              actionButton("goButton1", "Simulate price")
                            ),
                           mainPanel(
                             C3LineBarChartOutput("chart")
                             # tableOutput("table")
                             # plotOutput("chart")
                           ))
                   )
        )