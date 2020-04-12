

ui <- fluidPage(
  titlePanel("Covid-19 Dashboard"),
  h6(
    "Basic covid-19 status visualisation
                              based on CSSE data(live link) from
                              Johns Hopkins University."
  ),
  
  
  sidebarPanel(
    ## conditionalPanel() functions for selected tab
    conditionalPanel(
      condition = "input.tabselected==1",
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
    )
    ,
    conditionalPanel(condition = "input.tabselected==2",
                     uiOutput("varx"))
    
  ),
  mainPanel(
   
    tabsetPanel(
      tabPanel("Status_Report", value = 1,
               conditionalPanel(
                 condition = "input.graph_type != 'data: current status'", 
                 plotOutput("graph")),
               
               conditionalPanel(
                 condition = "input.graph_type == 'data: current status'",
                 dataTableOutput("tableView"))),
      tabPanel("Deep_Dive", value = 2,
               
               
               fluidRow(
                 splitLayout(cellWidths = c("33%", "33%","34%"), 
                             plotOutput("graphdeep1"), 
                             plotOutput("graphdeep2"), 
                             plotOutput("graphdeep3"))
               ),
               plotOutput("graph_deep")
               
               ),
      id = "tabselected"
    )
  )
)