library(tseries); library(forecast); library(caret); library(shiny)
library(ggthemr); library(corrplot)

options(shiny.maxRequestSize = 9*1024^2)
#ggthemr("flat dark", type = "outer")

dis <- read.csv("data/diseases_reduced.csv",
                check.names = FALSE)
model_diagnostics <- read.csv("data/model_diagnostics.csv",
                              check.names = FALSE)
model_forecasts <- read.csv("data/6_forecasts.csv",
                              check.names = FALSE)
forecast_widths_only <- read.csv("data/forecast_widths_only.csv",
                              check.names = FALSE)
forecast_widths_full <- read.csv("data/forecast_widths_full.csv",
                              check.names = FALSE)
forecast_widths_13 <- read.csv("data/forecast_widths_13.csv",
                              check.names = FALSE)
forecast_widths_46 <- read.csv("data/forecast_widths_46.csv",
                              check.names = FALSE)



shinyServer(function(input, output, session) {
 
  
  output$diagnostic_table <- DT::renderDataTable({
    DT::datatable(model_diagnostics, options = list(paging=FALSE))
  })
  
  output$forecasts_table <- DT::renderDataTable({
    DT::datatable(model_forecasts, options = list(paging=FALSE))
  })
  
  output$widths_only <- DT::renderDataTable({
    DT::datatable(forecast_widths_only, options = list(paging=FALSE))
  })
  
  output$widths_full <- DT::renderDataTable({
    DT::datatable(forecast_widths_full, options = list(paging=FALSE))
  })
  
  output$widths_13 <- DT::renderDataTable({
    DT::datatable(forecast_widths_13, options = list(paging=FALSE))
  })
  
  output$widths_46 <- DT::renderDataTable({
    DT::datatable(forecast_widths_46, options = list(paging=FALSE))
  })
  
  
#----Test---------------------
  observeEvent(input$go, 
               print(input$a))
  
  
  
#----Time Series Plot---------
  
  
  timeSeries <- reactive({
    d <- dis
    tab_1 <- ts(d[input$disease_ts], 
                start = c(2006, 1), 
                end = c(2016, 1),
                frequency = 52)
    
    
    tab_2 <- window(tab_1, 
                    start = c(input$range_ts[1], 1),
                    end = c(input$range_ts[2], 1),
                    frequency = 52)
    tab_2
  })
  
  timeSeries2 <- reactive({
    d <- dis
    tab_1 <- ts(d[input$disease_season_ts], 
                start = c(2006, 1), 
                end = c(2016, 1),
                frequency = 52)
    tab_1
  })
  
#  output$plot_ts <- renderPlot({
#    plot(timeSeries(), col="red", lwd=3,
#         ylab="Cases per Week",
#         cex.axis=1)
#    grid(col="gray")
  
  output$plot_ts <- renderPlot({
    if (is.null(input$disease_ts))
      return(NULL)
    
    autoplot(timeSeries(),
             ylab = "Cases/Week",
             main = "",
             size = 1) +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 14))
  })
  
  
  
  output$plot_seasonplot <- renderPlot({
    if (is.null(input$disease_season_ts))
      return(NULL)
    
    ggseasonplot(timeSeries2(),
                 s = 12,
                 ylab = "Cases/Week",
                 main = paste("Seasonal plot of", 
                              input$disease_season_ts)) +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 14))
  })

#-------------------------------
  
  
#******ARIMA Plot*************
  arima_series_full <- reactive({
    d <- dis
    foo_ts <- ts(d[input$disease_2],
                 start = c(2006, 1), end = c(2016, 17),
                 frequency = 52)
    foo_ts
  })
  
  
  arima_series <- reactive({
    d <- dis
    foox_ts <- window(arima_series_full(), 
                 start = c(2006, 1), 
                 end = c(2016, 1),
                 frequency = 52)
    foox_ts
  })

  xreg_series2 <- reactive({
    d <- dis
    xreg_ts2 <- ts(d[input$arima_xreg], 
                   start = c(2006, 1), end=c(2016, 1),
                   frequency = 52)
    xreg_ts2
  })
  
  arima_act_series <- reactive({
    d <- dis
    ts <- window(arima_series_full(),
                 start = c(2016, 2), end = c(2016, 17),
                 frequency = 52)
    ts
  })
  

  
  arima_fit <- reactive({
    if (input$inc_arima_xreg == FALSE) {
      fit_a <- auto.arima(arima_series(),
                          approximation = TRUE,
                          stepwise = TRUE,
                          max.d = input$max_d,
                          max.D = input$max_D,
                          stationary = input$rest_stat,
                          seasonal = input$rest_seas,
                          allowdrift = input$allowdrift,
                          allowmean = input$allowmean,
                          trace = TRUE)
      fit_a
    } else {
      fit_a <- auto.arima(arima_series(),
                          xreg = xreg_series2(),
                          approximation = TRUE,
                          max.d = input$max_d,
                          max.D = input$max_D,
                          stationary = input$rest_stat,
                          seasonal = input$rest_seas,
                          allowdrift = input$allowdrift,
                          allowmean = input$allowmean,
                          stepwise = TRUE,
                          trace = FALSE)
      fit_a
    }
    #fit_a
  })
  
  # arima_fit <- reactive({
  #   fit_a <- auto.arima(arima_series(), 
  #                       stepwise=TRUE, 
  #                       trace=TRUE)
  #   fit_a
  # })
  
  #Looks like you can put the output$ outside of observe as well
  # observeEvent(input$go,
  #              output$arima_summary <- renderPrint({
  #                summary(arima_fit())
  #              })
  #              )
  
  output$arima_summary <- renderPrint({
    summary(arima_fit())
  })

  
  output$arima_plot <- renderPlot({

    if (is.null(dis))
      return(NULL)
    
    if (input$inc_arima_xreg == FALSE) {
      afit1 <- arima_fit()
      afit <- forecast(afit1, h = input$periods)
      afcast <- data.frame(x = time(afit$mean), y = afit$mean)
      aactual <- data.frame(x = time(arima_act_series()), y = arima_act_series())
      afits <- data.frame(x = time(arima_series()), y = fitted.values(afit))
      areal <- data.frame(x = time(arima_series()), y = afit$x)
      ribbon <- data.frame(low_80 = afit$lower[,1],
                           hi_80 = afit$upper[,1],
                           low_95 = afit$lower[,2],
                           hi_95 = afit$upper[,2],
                           x = time(afit$mean))
      ggplot() +
        geom_line(aes(x, y, color = "Actual 2016 Values"),
                  data = aactual,
                  size = 1) +
        geom_line(aes(x, y, color = "Model Fit"),
                  data = afits,
                  size = 1) +
        geom_line(aes(x, y, color = "Actual Series"),
                  data = areal,
                  size = 1) +
        geom_line(aes(x, y, color = "2016 Forecasts"),
                  data = afcast,
                  size = 1) +
        geom_ribbon(aes(ymin = low_80, ymax = hi_80, x), 
                    alpha = 0.27,
                    data = ribbon) +
        geom_ribbon(aes(ymin = low_95, ymax = hi_95, x), 
                    alpha = 0.2,
                    data = ribbon,
                    show.legend = TRUE) +
        labs(x = "Time", y = "Cases/Week") +
        theme(axis.text = element_text(size = 14),
              axis.title = element_text(size = 14),
              legend.text = element_text(size = 14),
              legend.title = element_text(size = 14))
    
    } else {
      afit1 <- arima_fit()
      afit <- forecast(afit1,
                       h = input$periods,
                       xreg = xreg_series2())
      afcast <- data.frame(x = time(afit$mean), y = afit$mean)
      aactual <- data.frame(x = time(arima_act_series()), y = arima_act_series())
      afits <- data.frame(x = time(arima_series()), y = fitted.values(afit))
      areal <- data.frame(x = time(arima_series()), y = afit$x)
      ribbon <- data.frame(low_80 = afit$lower[,1],
                           hi_80 = afit$upper[,1],
                           low_95 = afit$lower[,2],
                           hi_95 = afit$upper[,2],
                           x = time(afit$mean))
      ggplot() +
        geom_line(aes(x, y, color = "Actual Series"),
                  data = areal,
                  size = 1) +
        geom_line(aes(x, y, color = "Model Fit"),
                  data = afits,
                  size = 1) +
        geom_line(aes(x, y, color = "2016 Forecasts"),
                  data = afcast,
                  size = 1) +
        geom_line(aes(x, y, color = "Actual 2016 Values"),
                  data = aactual,
                  size = 1) +
        geom_ribbon(aes(ymin = low_80, ymax = hi_80, x), 
                    alpha = 0.3,
                    data = ribbon) +
        geom_ribbon(aes(ymin = low_95, ymax = hi_95, x), 
                    alpha = 0.2,
                    data = ribbon,
                    show.legend = TRUE) +
        labs(x = "Time", y = "Cases/Week") +
        theme(axis.text = element_text(size = 14),
              axis.title = element_text(size = 14),
              legend.text = element_text(size = 14),
              legend.title = element_text(size = 14))
    }
  })
  
  
  
  
  
#%%%%%%%%%%%%  ARIMAX Plot   %%%%%%%%%%%%%% 
 # arimax_series <- reactive({
 #    d <- dis
 #    ts_x <- ts(d[input$disease_x], 
 #                 start = c(2006, 1), end = c(2016, 1),
 #                 frequency = 52)
 #    ts_x
 #  })
 #  
 #  xreg_series <- reactive({
 #    d <- dis
 #    xreg_ts <- ts(d[input$regressor], 
 #                  start = c(2006, 1), end = c(2016, 1),
 #                  frequency = 52)
 #    xreg_ts
 #  })
 #  
 #  arimax_fit <- reactive({
 #    fit_x <- auto.arima(arimax_series(), xreg = xreg_series(), trace = TRUE, stepwise=TRUE)
 #    fit_x
 #  })
 #  
 #  output$arimax_plot <- renderPlot({
 #    if (is.null(dis))
 #      return(NULL)
 #    plot(forecast(arimax_fit(), 
 #                  xreg = xreg_series(),
 #                  h = input$periods,
 #                  robust = TRUE), 
 #         col="red", 
 #         ylab = "Cases per Week", 
 #         main = paste("Plot of", input$disease_x),
 #         lwd = 2.2)
 #    lines(fitted(arimax_fit()), col = "blue")
 #    grid(col = "gray")
 #  })
  # 
  # output$arimax_summary <- renderPrint({
  #   summary(arimax_fit())
  # })
#----------------------------------------------------- 
  
  
  
  #******NNAR Plot*************
  nnar_series_full <- reactive({
    d <- dis
    nnar_ts <- ts(d[input$disease_nn], 
                  start = c(2006, 1), end = c(2016, 17),
                  frequency = 52)
    nnar_ts
  })
  
  nnar_series <- reactive({
    d <- dis
    nnar_ts <- window(nnar_series_full(), 
                  start = c(2006, 1), end = c(2016, 1),
                  frequency = 52)
    nnar_ts
  })
  
  nnar_xreg_series <- reactive({
    d <- dis
    nnar_xs <- ts(d[input$nnar_xreg], 
                   start = c(2006, 1), end = c(2016, 1),
                   frequency = 52)
    nnar_xs
  })
  
  nnar_act_series <- reactive({
    d <- dis
    ts <- window(nnar_series_full(),
             start = c(2016, 2), end = c(2016, 17),
             frequency = 52)
    ts
  })
  
  
  
  nnar_fit <- reactive({
    if (input$inc_nnar_xreg == FALSE) {
      fit_a <- nnetar(nnar_series())
      fit_a
    } else {
      fit_a <- nnetar(nnar_series(),
                      xreg = nnar_xreg_series())
      fit_a
    }
  })
  
  output$nnar_summary <- renderPrint({
    nnar_fit()
  })
  
  output$nnar_accuracy <- renderPrint({
    accuracy(nnar_fit())
  })
  
  
  output$nnar_plot <- renderPlot({
    if (is.null(dis))
      return(NULL)
    
    if (input$inc_nnar_xreg == FALSE) {
      fit <- forecast(nnar_fit(), h = input$periods_nn)  
      fcast <- data.frame(x = time(fit$mean), y = fit$mean)
      actual <- data.frame(x = time(nnar_act_series()), y = nnar_act_series())
      fits <- data.frame(x = time(nnar_series()), y = fitted.values(fit))
      real <- data.frame(x = time(nnar_series()), y = fit$x)
      
      ggplot() +
        geom_line(aes(x, y, color = "Actual 2016 Values"),
                  data = actual,
                  size = 1) + 
        geom_line(aes(x, y, color = "Model Fit"),
                  data = fits,
                  size = 1) + 
        geom_line(aes(x, y, color = "Actual Series"),
                  data = real,
                  size = 1) +
        geom_line(aes(x, y, color = "2016 Forecasts"),
                  data = fcast,
                  size = 1) +
        labs(x = "Time", y = "Cases/Week") +
        theme(axis.text = element_text(size = 14),
              axis.title = element_text(size = 14),
              legend.text = element_text(size = 14),
              legend.title = element_text(size = 14))
      
    } else {
      fit <- forecast(nnar_fit(), 
                      h = input$periods_nn,
                      xreg = nnar_xreg_series())  
      fcast <- data.frame(x = time(fit$mean), y = fit$mean)
      actual <- data.frame(x = time(nnar_act_series()), y = nnar_act_series())
      fits <- data.frame(x = time(nnar_series()), y = fitted.values(fit))
      real <- data.frame(x = time(nnar_series()), y = fit$x)
      
      ggplot() +
        geom_line(aes(x, y, color = "Actual 2016 Values"),
                  data = actual,
                  size = 1) + 
        geom_line(aes(x, y, color = "Model Fit"),
                  data = fits,
                  size = 1) + 
        geom_line(aes(x, y, color = "Actual Series"),
                  data = real,
                  size = 1) +
        geom_line(aes(x, y, color = "2016 Forecasts"),
                  data = fcast,
                  size = 1) +
        labs(x = "Time", y = "Cases/Week") +
        theme(axis.text = element_text(size = 14),
              axis.title = element_text(size = 14),
              legend.text = element_text(size = 14),
              legend.title = element_text(size = 14))
      # fits <- data.frame(x = time(nnar_series()), 
      #                    y = fitted.values(nnar_fit()))
      # fits2 <- data.frame(x = time(nnar_act_series()), 
      #                     y = nnar_act_series())
      # 
      # autoplot(forecast(nnar_fit(), 
      #                   xreg = nnar_xreg_series(),
      #                   h = input$periods_nn),
      #          ylab = "Cases/Week") +
      #   geom_line(aes(x, y, color = "Model Fit"), 
      #             data=fits) +
      #   geom_line(aes(x, y, color = "Actual Values"), 
      #             data=fits2) +
      #   theme(axis.text = element_text(size = 14),
      #         axis.title = element_text(size = 14),
      #         legend.text = element_text(size = 14),
      #         legend.title = element_text(size = 14))
    }
  })
  

  # output$nnar_plot <- renderPlot({
  #   if (is.null(dis))
  #     return(NULL)
  #   
  #   
  #   plot(forecast(nnar_fit(), 
  #                 h = input$periods_nn), 
  #        col = "red", 
  #        ylab = "Cases per Week", 
  #        main = paste("Plot of", input$disease_nn),
  #        lwd = 2.2)
  #   lines(fitted(nnar_fit()), col = "blue")
  #   grid(col = "gray")
  # })
  
  ##############################################
  # Corrgram 
  ##############################################
  
  output$corr_plot <- renderPlot({
    if (is.null(dis))
      return(NULL)
    
    d <- dis
    mcor <- cor(d[input$corr_diseases], use="complete.obs")
    corrplot(mcor, 
             type = "lower", 
             method = "circle",
             tl.col = "black", 
             tl.srt = 25)
        
  })
  
  
  
  
  
})
