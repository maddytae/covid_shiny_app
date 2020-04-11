# User interface ----
ui <- fluidPage(titlePanel("Covid-19 Dashboard"),
                
                sidebarLayout(
                  sidebarPanel(
                    helpText("Basic covid-19 status visualisation
                              based on CSSE data(live link) from
                              Johns Hopkins University"),
                    
                    selectInput(
                      "graph_type",
                      label = "Choose an item to display",
                      choices = c(
                        "graph: linear_scale",
                        "graph: log10_scale",
                        "graph: daily_change",
                        "graph: case_distribution",
                        "data: current status"
                      ),
                      selected = "graph: daily_change"
                    ),
                    numericInput("num",
                                 label = "Select top n countries",
                                 value = 10)
                    ,textInput("text", 
                               label = "Add additional countries
                             separated by comma", 
                               value = "Canada,Singapore,India")
                  ),
                  
              #    mainPanel(plotOutput("graph"))
                  mainPanel(
                    conditionalPanel(
                      condition = "input.graph_type != 'data: current status'", 
                      plotOutput("graph")),
                    
                    conditionalPanel(
                      condition = "input.graph_type == 'data: current status'",
                      dataTableOutput("tableView")))
                  
                  
                  
                  
                  
                  
                ))
######