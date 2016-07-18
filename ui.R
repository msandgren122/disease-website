library(markdown)
dis <- read.csv("data/diseases_reduced.csv",
                check.names = FALSE)

shinyUI(
  navbarPage(
    theme="orange_bootstrap.css", 
    "Singapore Diseases",
    
    
    
    
    
    tabPanel(
      "Time Series",
      sidebarLayout(
        sidebarPanel(
          h2("Time Series Plotter"),
          p("This application makes simple time-series plots of various diseases in",
            "Singapore, for the years 2006 through 2016. Use the dropdown below to",
            "select up to 10 diseases to plot.",
            "To remove a selection, use backspace, or click on the disease and hit",
            "backspace."),
          selectInput("disease_ts", 
                      "Disease(s):",
                      c(colnames(dis)),
                      selected = colnames(dis)[7],
                      multiple = TRUE),
          sliderInput("range_ts", 
                      "Adjust Time Range:",
                      min = 2006, 
                      max = 2016,
                      value = c(2006, 2016), 
                      sep = "", 
                      dragRange = TRUE),
          tags$hr(),
          
          selectInput("disease_season_ts", 
                      "Disease for seasonal plot:",
                      c(colnames(dis)),
                      selected = colnames(dis)[7],
                      multiple = FALSE),
          helpText("Note: Some diseases do not have data for all 10 years")
        ),
        
        mainPanel(
          plotOutput("plot_ts"),
          plotOutput("plot_seasonplot")
        ) #close mainPanel
      )   #close sidebarPanel
    ),    #close tabPanel
      
      
    
    
    
    
    
    tabPanel(
        "ARIMA",
        sidebarLayout(
          sidebarPanel(
            h2("ARIMA Forecasts"),
            p("This application plots an auto ARIMA fit over the orginal",
              "time series. For the sake of computation time, a stepwise",
              "search for the best model is used."),
            helpText("Note: ARIMA Plots will take a moment",
                     "to render, as the model must be calculated."),
            selectInput("disease_2", 
                        "Disease:",
                        c(colnames(dis)),
                        selected = colnames(dis)[7],
                        multiple = FALSE),
            
            conditionalPanel(
              condition = "input.inc_arima_xreg == false",
              sliderInput("periods", 
                          "Number of weeks to forecast:",
                          min = 1, 
                          max = 100,
                          value = 16)
            ),
            
            tags$hr(), 
            
            sliderInput("max_d",
                        "Max number of non-seasonal differences",
                        min = 0,
                        max = 3,
                        value = 0),
            
            sliderInput("max_D",
                        "Max number of seasonal differences",
                        min = 0,
                        max = 3,
                        value = 0),
            
            checkboxInput("allowdrift",
                          "Allow models with drift?",
                          value = FALSE),
            
            checkboxInput("allowmean",
                          "Allow models with a non-zero mean?",
                          value = FALSE),
            
            checkboxInput("rest_seas",
                          "Restrict search to non-seasonal models?",
                          value = TRUE),
            
            checkboxInput("rest_stat",
                          "Restrict search to stationary models?",
                          value = TRUE),
            
            checkboxInput("inc_arima_xreg",
                          "Include Regressor?",
                          value = FALSE),
            
            conditionalPanel(
              condition = "input.inc_arima_xreg == true",
              selectInput("arima_xreg", 
                          "Choose Exogenous Variable:",
                          c(colnames(dis)),
                          selected = FALSE,
                          multiple = FALSE)
            )
          ),
          
          mainPanel(
            plotOutput("arima_plot"),
            
            h4("Model Summary"),
            verbatimTextOutput("arima_summary")
          ) #close mainPanel
        )   #close sidebarLayour
      ),    #close tabPanel
    
    
    
    
    
    
    
   # tabPanel("ARIMAX",
   #          sidebarLayout(
   #            sidebarPanel(
   #              h2("ARIMAX Forecasts"),
   #              p("This application plots an auto ARIMAX fit over the orginal",
   #                "time series. For the sake of computation time, a stepwise",
   #                "search for the best model is used."),
   #              helpText("Note: ARIMAX Plots will take a few moments",
   #                       "to render, as the model must be calculated."),
   #              selectInput("disease_x", "Disease:",
   #                          c(colnames(dis)),
   #                          selected=colnames(dis)[7],
   #                          multiple = FALSE),
   #              selectInput("regressor", "Choose Exogenous Variable:",
   #                          c(colnames(dis)),
   #                          selected=colnames(dis)[8],
   #                          multiple = FALSE),
   #              tags$hr(),
   #              p("Model Fit: Blue"),
   #              p("Original Series: Red"),
   #              p("Dark Gray: 95% Prediction Interval"),
   #              p("Light Gray: 80% Prediction Interval")
   #              
   #            ),
   #            
   #            mainPanel(
   #              plotOutput("arimax_plot"),
   #              
   #              h4("Model Summary"),
   #              verbatimTextOutput("arimax_summary")
   #            )
   #          )
   # ),
   
   
     
   
   
   
   
      tabPanel(
        "NNAR",
        sidebarLayout(
          sidebarPanel(
            h2("NNAR Forecasts"),
            p("This application plots a Feed-Forward Neural Network, along",
              "with a forecast, alongside the orginal time-series."),
            helpText("Note: NNAR Plots will take a few moments",
                     "to render, as the model must be calculated."),
            selectInput("disease_nn", 
                        "Disease:",
                        c(colnames(dis)),
                        selected = colnames(dis)[7],
                        multiple = FALSE),
            conditionalPanel(
              condition = "input.inc_nnar_xreg == false",
              sliderInput("periods_nn", 
                          "Number of weeks to forecast:",
                          min = 1, 
                          max = 200,
                          value = 16)
            ),
            
            tags$hr(), 
            checkboxInput("inc_nnar_xreg",
                          "Include Regressor?",
                          value = FALSE),
            
            conditionalPanel(
              condition = "input.inc_nnar_xreg == true",
              selectInput("nnar_xreg", 
                          "Choose Exogenous Variable:",
                          c(colnames(dis)),
                          selected = FALSE,
                          multiple = FALSE)
            )
          ),
                
          mainPanel(
            plotOutput("nnar_plot"),
            h4("Model Summary"),
            verbatimTextOutput("nnar_summary"),
            verbatimTextOutput("nnar_accuracy")
          )
        )
      ),
   
   
   
   
   
     tabPanel(
       "Correllogram",
       sidebarLayout(
         sidebarPanel(
           h2("Correllogram"),
           p("This application plots a correllogram of selected diseases."),
           selectInput("corr_diseases", 
                       "Diseases:",
                       c(colnames(dis)),
                       selected = colnames(dis)[7:10],
                       multiple = TRUE),
           
           tags$hr()
           
         ),
         
         mainPanel(
           plotOutput("corr_plot")
         ) #close mainPanel
       )   #close sidebarLayour
     ),
                   
    
    
      navbarMenu(
        "Tables",
        tabPanel(
          "Model Diagnostics",
          DT::dataTableOutput("diagnostic_table")
        ),
          
          tabPanel("6 Week Forecasts Only",
          DT::dataTableOutput("forecasts_table")
        ),
          
          tabPanel("Forecast Widths Only",
          DT::dataTableOutput("widths_only")
        ),
          
          tabPanel("Forecast Widths Full",
          DT::dataTableOutput("widths_full")
        ),
          
          tabPanel("Forecast Widths W1-3",
          DT::dataTableOutput("widths_13")
        ),
          
          tabPanel("Forecast Widths W4-6",
          DT::dataTableOutput("widths_46")
        )
      ) #close navbarMenu
                   
    
    
  ) #close navbarPage
) #close shinyUI