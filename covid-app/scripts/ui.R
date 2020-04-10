# User interface ----
ui <- fluidPage(titlePanel("Covid-19 Dashboard"),
                
                sidebarLayout(
                  sidebarPanel(
                    helpText("Basic covid-19 status visualisation
                              based on CSSE data(live link) from
                              Johns Hopkins University"),
                    
                    selectInput(
                      "graph_type",
                      label = "Choose a graph to display",
                      choices = c(
                        "original_scale",
                        "log_scale",
                        "daily_change"
                      ),
                      selected = "daily_change"
                    ),
                    numericInput("num",
                                 label = "Select top n countries",
                                 value = 20)
                    ,textInput("text", 
                               label = "Add additional countries
                             separated by comma", 
                               value = "Singapore,India")
                  ),
                  
                  mainPanel(plotOutput("graph"))
                ))
######