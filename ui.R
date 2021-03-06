#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)

################################################# BEGIN: UI body building blocks ##############################################
page_data_hist <- tabItem(tabName = "menu_data_hist", 
                          fluidRow(
                            tabBox(title = "Original data"  ,
                                   tabPanel("Training sample", fig_ggplot_date_UI("fig_data_hist")), 
                                   tabPanel("Histogram", fig_histgram_density_UI("fig_data_org_histgram")),
                                   tabPanel("Underlying data", tab_DT_searchPage_UI("tab_data_hist"))
                            ),
                            tabBox(title = "After chosen transformation"  ,
                                   tabPanel("Training sample", fig_ggplot_date_UI("fig_transf_data_hist")), 
                                   tabPanel("Histogram", fig_histgram_density_UI("fig_data_transf_histgram")),
                                   tabPanel("Underlying data", tab_DT_searchPage_UI("tab_transf_data_hist"))
                            )
                          ),
                          fluidRow(
                            tabBox(title = "Stationarity: original data",
                                   tabPanel("ACF", fig_acf_pacf_UI("fig_acf_data_org")),
                                   tabPanel("Tests", tab_simple_UI("tab_data_org_stationary"))
                            ),
                            tabBox(title = "Stationarity: transformed", 
                                   tabPanel("ACF", fig_acf_pacf_UI("fig_acf_data_transf")),
                                   tabPanel("Tests", tab_simple_UI("tab_data_transf_stationary"))
                            )
                          )
)

page_mod_arima <- tabItem(tabName = "memu_mod_arima",
                          fluidRow(
                            tabBox(title = "Fit transformed data",
                                   tabPanel("ARIMA", fluidRow(
                                     slider_mod_ARIMA_UI("fit_mod_ARIMA_transf"),
                                     column(width=8, tab_simple_UI("fit_mod_ARIMA_transf")))),
                                   tabPanel("Auto fit", txt_auto_arima_fit_UI("txt_auto_arima_org"))
                            ),
                            tabBox(title = "Model Diagnostic",
                                   tabPanel("Diag plot", plotOutput("fig_diag_arima")),
                                   tabPanel("ACF", fig_acf_pacf_UI("fig_acf_arima_resid")),
                                   tabPanel("Residual Stationary", tab_simple_UI("tab_stationary_residuals")),
                                   tabPanel("QQ plot", fig_qq_plot_UI("fig_qq_plot")),
                                   tabPanel("Histogram", fig_histgram_density_UI("fig_histogram_residuals")),
                                   tabPanel("Heteroskedasticity", fig_acf_pacf_UI("fig_acf_arima_resid_sqr"))
                            )
                          ),
                          fluidRow(
                            tabBox(title = "Historical fit",
                                   tabPanel("Plot", fig_hist_mod_fit_UI("fig_hist_mod_fit")) #,
                                   #tabPanel("Data", tab_DT_searchPage_UI("tab_mod_hist_fit"))
                            ),
                            tabBox(title = "Model forecast",
                                   tabPanel("Transformed", fig_mod_forecast_UI("fig_forecast_arima_transf")))
                          )
)

############################################################################################ END: UI body building blocks ###

######################################BEGIN: Dashboard UI ###################################################################
header <- dashboardHeader(
  title = "ARMA model fitting"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    selectInput("sel_source_2_use", label = "Data source:", choices="YAHOO"),
    textInput("txt_dataId_2_source", label = "Data symbol/ID", value = "^GSPC"),
    menuItem("Data overview", tabName = "menu_data_hist", icon = icon("line-chart")), 
    menuItem("ARMA fit", tabName = "memu_mod_arima", icon = icon("dashboard"))# ,
    #menuItem("Scenario Forecast", tabName = "memu_mod_fore", icon = icon("line-chart"))
  ),
  h5(),
  sidebarMenu(
    dateRangeInput("date_range_train", label = h5("Traning sample range"), start = "2015-01-01", end = "2016-09-01"),
    selectInput("select_type_data_transf", label = h5(paste0("Value transformation")), choices = lst_val_transform, selected = "diff_log"),
    dateRangeInput("date_range_test", label = h5("Testing sample range"), start = "2016-09-01", end = Sys.Date())
  )
)

body <- dashboardBody(tabItems(
  page_data_hist,
  page_mod_arima
  # tab_mod_override
)
)

ui <- dashboardPage(header, sidebar, body)
####################################################################################################### END: dashboard UI####


# Define UI 
ui <- dashboardPage(header, sidebar, body)
