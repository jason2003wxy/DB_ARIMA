#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)


shinyServer(function(input, output, session) {
  
  ################################################BEGIN  Part A: REACTIVE data prepare #############################################
  ###### DATA ASSUMPTION:
  # tibble data frame is fetched from data source with datafield: date and val
  # date is in formate of Date
  # val is numeric
  ###############################
  
  #callModule(get_inputInfo_serv, "debug")
  ### Source data from UI inputs 
  reac_data_raw <- reactive({
    df_info_fetch <- data_frame(nameR="df_data_org", dataSource = input$sel_source_2_use, 
                                dataId = toupperNoSpace(input$txt_dataId_2_source), date_beg = as.Date(input$date_range_train[1]))
    if (toupper(input$sel_source_2_use) == "YAHOO"){
      return(Get_Price_Yahoo(df_info_fetch$dataId, date_beg="1999-01-01", date_end = Sys.Date()))
    }
  })
  reac_data_raw_transf <- callModule(transform_df_timeSeries, "date_raw_transf", reac_data_raw, reactive(input$select_type_data_transf))
  ## training sample in original format 
  reac_data_train_org <- callModule(filter_df_timeSeries, "data_training_org", reac_data_raw, 
                                    reactive(input$date_range_train[1]), reactive(input$date_range_train[2]))
  ## testing sample in original format 
  reac_data_test_org <- callModule(filter_df_timeSeries, "data_testing_org", reac_data_raw, 
                                   reactive(input$date_range_test[1]), reactive(input$date_range_test[2]))
  ## training sample in the chosen transformation
  reac_data_train_transf <- callModule(filter_df_timeSeries, "data_training_org", reac_data_raw_transf, 
                                       reactive(input$date_range_train[1]), reactive(input$date_range_train[2]))
  ## testing sample in the chosen transformation
  reac_data_test_transf <- callModule(filter_df_timeSeries, "data_testing_transf", reac_data_raw_transf, 
                                      reactive(input$date_range_test[1]), reactive(input$date_range_test[2]))
  ##############################################################################################END Part A: REACTIVE data prepare###
  
  ################################################BEGIN Part B: Historical data view panel #############################################
  ### Source original data from UI inputs 
  callModule(tab_DT_searchPage_serv, "tab_data_hist", reac_data_train_org, reactive(names(reac_data_train_org())), tab_with_date=FALSE)
  callModule(fig_ggplot_date_serv, "fig_data_hist", reac_data_train_org, reactive("val"), reactive(toupper(input$txt_dataId_2_source)))
  callModule(fig_histgram_density_serv, "fig_data_org_histgram", reac_data_train_org, "val")
  callModule(fig_acf_pacf_serv, "fig_acf_data_org", reactive(reac_data_train_org()$val))
  callModule(tab_simple_serv, "tab_data_org_stationary", reactive(TestsStationaryPval(reac_data_train_org()$val)))
  ### Source transformated data from UI inputs 
  callModule(tab_DT_searchPage_serv, "tab_transf_data_hist", reac_data_train_transf, reactive(names(reac_data_train_transf())), tab_with_date=FALSE)
  callModule(fig_ggplot_date_serv, "fig_transf_data_hist", reac_data_train_transf, reactive("val"), reactive(toupper(input$txt_dataId_2_source)))
  callModule(fig_histgram_density_serv, "fig_data_transf_histgram", reac_data_train_transf, "val")
  callModule(fig_acf_pacf_serv, "fig_acf_data_transf", reactive(reac_data_train_transf()$val))
  callModule(tab_simple_serv, "tab_data_transf_stationary", reactive(TestsStationaryPval(reac_data_train_transf()$val)))
  ##############################################################################################END Part B: Historical data view panel###
  
  
  ################################################BEGIN Part C: ARIMA model fit #############################################
  ### optimal model by AIC SIC etc
  callModule(txt_auto_arima_fit_serv, "txt_auto_arima_org", reactive(reac_data_train_transf()$val), max.order = 10)
  # callModule(interface_mod_ARIMA_serve, "fit_mod_ARIMA_transf")
  callModule(slider_mod_ARIMA_serve, "fit_mod_ARIMA_transf")
  reac_mod_arima <- callModule(fit_mod_ARIMA_serve, "fit_mod_ARIMA_transf", reactive(reac_data_train_transf()$val))
  callModule(tab_simple_serv, "test", reactive(broom::tidy(reac_mod_arima())), nrDigt=5)
  callModule(tab_simple_serv, "fit_mod_ARIMA_transf", reactive(broom::tidy(reac_mod_arima())), nrDigt=5)
  ##############################################################################################END Part C: ARIMA model fit###
  
  
  ################################################BEGIN Part D: ARIMA Model Diagnostic #############################################
  output$fig_diag_arima <- renderPlot({
    tsdiag(reac_mod_arima())
  })
  callModule(fig_acf_pacf_serv, "fig_acf_arima_resid", reactive(reac_mod_arima()$residuals), "Residuals")
  callModule(fig_qq_plot_serv, "fig_qq_plot", reactive(reac_mod_arima()$residuals), "Residual QQ plot")
  callModule(tab_simple_serv, "tab_stationary_residuals", reactive(TestsStationaryPval(as.numeric(reac_mod_arima()$residuals))))
  callModule(fig_histgram_density_serv, "fig_histogram_residuals", reactive(data_frame(residuals = reac_mod_arima()$residuals)), "residuals", "Residuals")
  callModule(fig_acf_pacf_serv, "fig_acf_arima_resid_sqr", reactive(reac_mod_arima()$residuals^2), "Residuals^2 (considering GARCH?)")
  ##############################################################################################END Part D: Model Diagnostic###
  
  
  ################################################BEGIN Part E: ARIMA Model historical fit & forecast #############################################
  df_mod_fit_transf <- callModule(fig_hist_mod_fit_serv, "fig_hist_mod_fit", reac_data_train_transf, reactive("val"),
                                  reactive(reac_mod_arima()$residuals), reactive(sqrt(reac_mod_arima()$sigma2)))
  # callModule(tab_DT_searchPage_serv, "tab_mod_hist_fit", df_mod_fit_transf, reactive(names(df_mod_fit_transf())), tab_with_date=FALSE)
  ### ARIMA model forecast
  df_arima_fore_transf <- callModule(df_arima_forecast_serve, "df_arima_fore_transf", reac_mod_arima, reactive(nrow(reac_data_test_transf())))
  callModule(fig_mod_forecast_serve, "fig_forecast_arima_transf", df_arima_fore_transf, reac_data_test_transf, "ARIMA forecast vs. transformed testing sample")
  ##############################################################################################END Part E: Model historical fit & forecast ###
})
