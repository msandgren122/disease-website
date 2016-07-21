library(tseries); library(forecast); library(shiny)
library(ggthemr); library(corrplot); library(plotly)
library(xts); library(lubridate)

options(shiny.maxRequestSize = 9*1024^2)
#ggthemr("flat dark", type = "outer")

dis <- read.csv("data/diseases_reduced.csv",
                check.names = FALSE)
dis <- dis[-469, ]
dis$"acute upper respiratory tract infections" <- NULL
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
                end = c(2015, 52),
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
                end = c(2015, 52),
                frequency = 52)
    tab_1
  })
  
#  output$plot_ts <- renderPlot({
#    plot(timeSeries(), col="red", lwd=3,
#         ylab="Cases per Week",
#         cex.axis=1)
#    grid(col="gray")
  
  output$plot_ts <- renderPlotly({
    if (is.null(input$disease_ts))
      return(NULL)
    
    p <- autoplot(timeSeries(),
             ylab = "Cases/Week",
             main = "",
             size = 1) +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 14))
    ggplotly(p)
  })
  
  
  
  output$plot_seasonplot <- renderPlot({
    if (is.null(input$disease_season_ts))
      return(NULL)

    p <- ggseasonplot(timeSeries2(),
                      s = 52,
                      ylab = "Cases/Week",
                      main = paste("Seasonal plot of", 
                                   input$disease_season_ts)) +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 14))
    #ggplotly(p)
    p
  })

#-------------------------------
  
  
#******ARIMA Plot*************
  
  dt_a <- reactive({
    d <- dis
    dts <- as.Date(paste(d$year, 
                         d$week, 
                         1, 
                         sep = "-"),
                   "%Y-%U-%u")
    dts <- as.POSIXlt(dts)
    dts
  })
  
  #create df_xts
  pretty_time_a <- reactive({
    d <- dis
    d_xts <- xts(x = d[input$disease_2],
                 order.by = dt_a(),
                 frequency = 52)
    d_xts
  })
  
  #create train xts time
  train_a <- reactive({
    trane <- pretty_time_a()[1:520] #hardcoded:bad
    trane
  })
  
  fcast_time_a <- reactive({
    trane <- pretty_time_a()[521:(520 + input$disease_2)] #hardcoded:bad
    trane
  })
  
  #create test xts time
  test_a <- reactive({
    tast <- pretty_time_a()[521:537] #hardcoded:bad
    tast
  })
  
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
                 end = c(2015, 52),
                 frequency = 52)
    foox_ts
  })

  xreg_series2 <- reactive({
    d <- dis
    xreg_ts2 <- ts(d[input$arima_xreg], 
                   start = c(2006, 1), end=c(2015, 52),
                   frequency = 52)
    xreg_ts2
  })
  
  arima_act_series <- reactive({
    d <- dis
    ts <- window(arima_series_full(),
                 start = c(2016, 1), end = c(2016, 17),
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
                          trace = TRUE)
      fit_a
    }
  })
  

  
  output$arima_summary <- renderPrint({
    summary(arima_fit())
  })

  
  output$arima_plot <- renderPlotly({

    if (is.null(dis))
      return(NULL)
    
    if (input$inc_arima_xreg == FALSE) {
      afit1 <- arima_fit()
      afit <- forecast(afit1, h = input$periods)
      
      fc_ts <- ts(afit$mean, start = start(afit$mean), end = end(afit$mean), frequency = 52)
      foo <- format(date_decimal(time(fc_ts)[1:length(time(fc_ts))] + 3/365), "%Y-%m-%d")
      dt2 <- as.POSIXlt(foo, tz = "UTC")
      dt2 <- xts(x = afit$mean, order.by = dt2, frequency = 52)
      
      afcast <- data.frame(x = time(dt2), 
                           y = afit$mean)
      aactual <- data.frame(x = time(test_a()), 
                            y = arima_act_series())
      afits <- data.frame(x = time(train_a()), 
                          y = fitted.values(afit))
      areal <- data.frame(x = time(train_a()), 
                          y = afit$x)
      ribbon <- data.frame(low_80 = afit$lower[,1],
                           hi_80 = afit$upper[,1],
                           low_95 = afit$lower[,2],
                           hi_95 = afit$upper[,2],
                           x = time(dt2))
      plot_ly() %>%
        
        add_trace(data = afits, x = x, y = y, name = "Model Fit") %>%
        add_trace(data = areal, x = x, y = y, name = "Actual Series") %>%
        add_trace(data = ribbon, 
                  x = time(dt2), 
                  y = low_95, 
                  fill = "",
                  marker = list(size = 0),
                  line = list(color = "rgb(160, 160, 160)",
                              width = 0),
                  showlegend = FALSE) %>%
        add_trace(data = ribbon,
                  x = time(dt2), 
                  y = hi_95, 
                  fill = "tonexty",
                  marker = list(size = 0),
                  line = list(color = "rgb(160, 160, 160)",
                              width = 0),
                  name = "95% Pred. Int.") %>%
        add_trace(data = ribbon, 
                  x = time(dt2), 
                  y = low_80,
                  line = list(color = "rgb(140, 140, 140)",
                              width = 0),
                  fill = "",
                  showlegend = FALSE) %>%
        add_trace(data = ribbon, 
                  x = time(dt2), 
                  y = hi_80, 
                  fill = "tonexty",
                  line = list(color = "rgb(140, 140, 140)",
                              width = 0),
                  name = "80% Pred. Int") %>%
        add_trace(data = afcast, 
                  x = x, y = y, 
                  line = list(color = "rgb(226, 51, 232)"),
                  name = "Forecasts") %>%
        add_trace(data = aactual,
                  x = x, y = y, 
                  line = list(color = "rgb(255, 157, 0)"),
                  name = "Actual 2016 Values") %>%
        
        
        layout(xaxis = list(title = "Time"),
               yaxis = list(title = "Cases/Week"))  
    
    } else {
      afit1 <- arima_fit()
      afit <- forecast(afit1, h = input$periods, xreg = xreg_series2())
      
      fc_ts <- ts(afit$mean, start = start(afit$mean), end = end(afit$mean), frequency = 52)
      foo <- format(date_decimal(time(fc_ts)[1:length(time(fc_ts))] + 3/365), "%Y-%m-%d")
      dt2 <- as.POSIXlt(foo, tz = "UTC")
      dt2 <- xts(x = afit$mean, order.by = dt2, frequency = 52)
      
      afcast <- data.frame(x = time(dt2), 
                           y = afit$mean)
      aactual <- data.frame(x = time(test_a()), 
                            y = arima_act_series())
      afits <- data.frame(x = time(train_a()), 
                          y = fitted.values(afit))
      areal <- data.frame(x = time(train_a()), 
                          y = afit$x)
      ribbon <- data.frame(low_80 = afit$lower[,1],
                           hi_80 = afit$upper[,1],
                           low_95 = afit$lower[,2],
                           hi_95 = afit$upper[,2],
                           x = time(dt2))
      plot_ly() %>%
        
        add_trace(data = afits, x = x, y = y, name = "Model Fit") %>%
        add_trace(data = areal, x = x, y = y, name = "Actual Series") %>%
        add_trace(data = ribbon, 
                  x = time(dt2), 
                  y = low_95, 
                  fill = "",
                  marker = list(size = 0),
                  line = list(color = "rgb(160, 160, 160)",
                              width = 0),
                  showlegend = FALSE) %>%
        add_trace(data = ribbon,
                  x = time(dt2), 
                  y = hi_95, 
                  fill = "tonexty",
                  marker = list(size = 0),
                  line = list(color = "rgb(160, 160, 160)",
                              width = 0),
                  name = "95% Pred. Int.") %>%
        add_trace(data = ribbon, 
                  x = time(dt2), 
                  y = low_80,
                  line = list(color = "rgb(140, 140, 140)",
                              width = 0),
                  fill = "",
                  showlegend = FALSE) %>%
        add_trace(data = ribbon, 
                  x = time(dt2), 
                  y = hi_80, 
                  fill = "tonexty",
                  line = list(color = "rgb(140, 140, 140)",
                              width = 0),
                  name = "80% Pred. Int") %>%
        add_trace(data = afcast, 
                  x = x, y = y, 
                  line = list(color = "rgb(226, 51, 232)"),
                  name = "Forecasts") %>%
        add_trace(data = aactual,
                  x = x, y = y, 
                  line = list(color = "rgb(255, 157, 0)"),
                  name = "Actual 2016 Values") %>%
        
        
        layout(xaxis = list(title = "Time"),
               yaxis = list(title = "Cases/Week"))  
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
  
  #create dt
  dt <- reactive({
    d <- dis
    dts <- as.Date(paste(d$year, 
                         d$week, 
                         1, 
                         sep = "-"),
                   "%Y-%U-%u")
    dts <- as.POSIXlt(dts)
    dts
  })
  
  #create df_xts
  pretty_time <- reactive({
    d <- dis
    d_xts <- xts(x = d[input$disease_nn],
                 order.by = dt(),
                 frequency = 52)
    d_xts
  })
  
  #create train xts time
  train <- reactive({
    trane <- pretty_time()[1:520] #hardcoded:bad
    trane
  })
  
  fcast_time <- reactive({
    trane <- pretty_time()[521:(520 + input$periods_nn)] #hardcoded:bad
    trane
  })
  
  #create test xts time
  test <- reactive({
    tast <- pretty_time()[521:537] #hardcoded:bad
    tast
  })
  
  
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
                  start = c(2006, 1), end = c(2015, 52),
                  frequency = 52)
    nnar_ts
  })
  
  nnar_xreg_series <- reactive({
    d <- dis
    nnar_xs <- ts(d[input$nnar_xreg], 
                   start = c(2006, 1), end = c(2015, 52),
                   frequency = 52)
    nnar_xs
  })
  
  nnar_act_series <- reactive({
    d <- dis
    ts <- window(nnar_series_full(),
             start = c(2016, 1), end = c(2016, 17),
             frequency = 52)
    ts
  })
  
  
  
  nnar_fit <- reactive({
    if (input$inc_nnar_xreg == FALSE) {
      fit_a <- nnetar(nnar_series(),
                      reltol = input$reltol_nn,
                      na.action = na.omit)
      fit_a
    } else {
      fit_a <- nnetar(nnar_series(),
                      xreg = nnar_xreg_series(),
                      reltol = input$reltol_nn,
                      na.action = na.omit)
      fit_a
    }
  })
  
  output$nnar_summary <- renderPrint({
    nnar_fit()
  })
  
  output$nnar_accuracy <- renderPrint({
    accuracy(nnar_fit())
  })
  
  
  output$nnar_plot <- renderPlotly({
    if (is.null(dis))
      return(NULL)
    
    if (input$inc_nnar_xreg == FALSE) {
      fit <- forecast(nnar_fit(), h = input$periods_nn)  
      
      fc_ts <- ts(fit$mean, start = start(fit$mean), end = end(fit$mean), frequency = 52)
      foo <- format(date_decimal(time(fc_ts)[1:length(time(fc_ts))] + 3/365), "%Y-%m-%d")
      dt2 <- as.POSIXlt(foo, tz = "UTC")
      dt2 <- xts(x = fit$mean, order.by = dt2, frequency = 52)
      
      
      fcast <- data.frame(x = time(dt2), y = fit$mean)
      actual <- data.frame(x = time(test()), y = nnar_act_series())
      fits <- data.frame(x = time(train()), y = fitted.values(fit))
      real <- data.frame(x = time(train()), y = fit$x)
      
      plot_ly() %>%
        add_trace(data = actual, x = x, y = y, name = "Actual 2016 Values") %>%
        add_trace(data = fits, x = x, y = y, name = "Model Fit") %>%
        add_trace(data = real, x = x, y = y, name = "Actual Series") %>%
        add_trace(data = fcast, x = x, y = y, name = "Forecasts") %>%
        layout(xaxis = list(title = "Time"),
               yaxis = list(title = "Cases/Week"))  
      
      
    } else {
      fit <- forecast(nnar_fit(), h = input$periods_nn, xreg = nnar_xreg_series())  
      fc_ts <- ts(fit$mean, start = start(fit$mean), end = end(fit$mean), frequency = 52)
      foo <- format(date_decimal(time(fc_ts)[1:length(time(fc_ts))] + 3/365), "%Y-%m-%d")
      dt2 <- as.POSIXlt(foo, tz = "UTC")
      dt2 <- xts(x = fit$mean, order.by = dt2, frequency = 52)
      fcast <- data.frame(x = time(dt2), y = fit$mean)
      actual <- data.frame(x = time(test()), y = nnar_act_series())
      fits <- data.frame(x = time(train()), y = fitted.values(fit))
      real <- data.frame(x = time(train()), y = fit$x)
      
      plot_ly() %>%
        add_trace(data = actual, x = x, y = y, name = "Actual 2016 Values") %>%
        add_trace(data = fits, x = x, y = y, name = "Model Fit") %>%
        add_trace(data = real, x = x, y = y, name = "Actual Series") %>%
        add_trace(data = fcast, x = x, y = y, name = "Forecasts") %>%
        layout(xaxis = list(title = "Time"),
               yaxis = list(title = "Cases/Week"))  
      
      # fit <- forecast(nnar_fit(), 
      #                 h = input$periods_nn,
      #                 xreg = nnar_xreg_series())  
      # fcast <- data.frame(x = time(fit$mean), y = fit$mean)
      # actual <- data.frame(x = time(nnar_act_series()), y = nnar_act_series())
      # fits <- data.frame(x = time(nnar_series()), y = fitted.values(fit))
      # real <- data.frame(x = time(nnar_series()), y = fit$x)
      # 
      # ggplot() +
      #   geom_line(aes(x, y, color = "Actual 2016 Values"),
      #             data = actual,
      #             size = 1) + 
      #   geom_line(aes(x, y, color = "Model Fit"),
      #             data = fits,
      #             size = 1) + 
      #   geom_line(aes(x, y, color = "Actual Series"),
      #             data = real,
      #             size = 1) +
      #   geom_line(aes(x, y, color = "2016 Forecasts"),
      #             data = fcast,
      #             size = 1) +
      #   labs(x = "Time", y = "Cases/Week") +
      #   theme(axis.text = element_text(size = 14),
      #         axis.title = element_text(size = 14),
      #         legend.text = element_text(size = 14),
      #         legend.title = element_text(size = 14))
    }
  })
  
  
  
  

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
