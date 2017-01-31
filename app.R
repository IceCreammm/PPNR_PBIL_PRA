library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
############################# Source global file #########################
source("global.R")



###### define variable labels for UI
ui_nam_model <- lst_control$dir_model$nam_mod 
names(ui_nam_model) <- lst_control$dir_model$label_mod
ui_nam_scenario <- lst_control$sideBar_scenarios$namR 
names(ui_nam_scenario) <- lst_control$sideBar_scenarios$labR


################################################# BEGIN: UI body building blocks ##############################################

############################################################################################ END: UI body building blocks ###
tab_mod_stat <- tabItem(tabName = "menu_mod_stat", 
                        fluidRow(
                          tabBox(title = "Proposed statistical model",
                                 tabPanel("Estimates", 
                                          DT::dataTableOutput("tab_mod_stat_est"),
                                          print_UI("print_rsq_orgfit")), #,  height = "150px"
                                 tabPanel("Overall fit", plotlyOutput("fig_mod_hist_fit")), #
                                 tabPanel("Fit data", DT::dataTableOutput("tab_mod_hist_fit"))
                          )  , 
                          tabBox(title = "Variables: original values",
                                 tabPanel("Business var", fig_ggplot_date_UI("var_depen")), #,  height = "150px" 
                                 tabPanel("Eco var 1", fig_ggplot_date_UI("var_driver_1")),
                                 tabPanel("Eco var 2", fig_ggplot_date_UI("var_driver_2")),
                                 tabPanel("Eco var 3", fig_ggplot_date_UI("var_driver_3")),
                                 tabPanel("Underlying data", tab_DT_searchPage_UI("var_data_all"))
                          )
                        ), 
                        fluidRow(
                          tabBox(title = "Variables: difference (log/abs)", #,  height = "150px"  
                                 tabPanel("Eco var 1", fig_scatter_UI("eco1_vs_bus")),
                                 tabPanel("Eco var 2", fig_scatter_UI("eco2_vs_bus")),
                                 tabPanel("Eco var 3", fig_scatter_UI("eco3_vs_bus")),
                                 tabPanel("Underlying data", tab_DT_searchPage_UI("var_delta_all"))
                          ),
                          tabBox(title = "Time series plot (aligned)", #,  height = "150px"
                                 # tabPanel("Eco var 1", fig_scatter_UI("eco1_vs_bus")),
                                 # tabPanel("Eco var 2", fig_scatter_UI("eco2_vs_bus")),
                                 tabPanel("Plot", fig_ggplot_date_UI("fig_mod_var_adj")),
                                 tabPanel("Underlying data", tab_DT_searchPage_UI("tab_mod_var_adj"))
                          ) 
                        ) 
)
tab_mod_override <- tabItem(tabName = "memu_mod_overide",
                            fluidRow(
                              box_mod_override_UI("lm_override"),
                              tabBox(
                                tabPanel("Chosen Model", tab_simple_UI("overr_tab_ovrridenMod")),
                                tabPanel("Candidate drivers", tab_DT_searchPage_UI("overr_tab_all_eco_meta")),
                                tabPanel("Overiden driver fit", print_UI("lm_fit_override"))
                              )
                            ),
                            fluidRow(
                              tabBox(title = "Linear correaltions",
                                     # tabPanel("TEMP_SHOW", verbatimTextOutput("debugPrint")),
                                     tabPanel("1st Eco", fig_scatter_UI("overr_fig_eco1_vs_bus")),
                                     tabPanel("2nd Eco", fig_scatter_UI("overr_fig_eco2_vs_bus")),
                                     tabPanel("3rd Eco", fig_scatter_UI("overr_fig_eco3_vs_bus"))
                              ), 
                              tabBox(title = "Model forecast",
                                     tabPanel("Figure", fig_ggplot_date_UI("ovrr_fig_fore_values")) ,  #height = "150px"
                                     tabPanel("Forecast values", tab_simple_UI("ovrr_tab_fore_values")), #
                                     tabPanel("Forecast growth", tab_simple_UI("ovrr_tab_fore_ratios"))
                              )
                            )
)
tab_mod_diag <- tabItem(tabName = "memu_mod_diag",
                        fluidRow(tabBox(title = "Re-estimated Model",
                                        tabPanel("Model estimates", 
                                                 tab_simple_UI("tab_mod_dig_fit_override"),
                                                 print_UI("print_rsq_refit")),
                                        tabPanel("Overall fit", 
                                                 fig_hist_mod_fit_UI("fig_hist_fit_mod_refit")),
                                        tabPanel("Dependent", fig_ggplot_date_UI("fig_var_depen_refit")), #,  height = "150px" 
                                        tabPanel("Driver 1", fig_ggplot_date_UI("fig_var_driver_1_refit")),
                                        tabPanel("Driver 2", fig_ggplot_date_UI("fig_var_driver_2_refit")),
                                        tabPanel("Driver 3", fig_ggplot_date_UI("fig_var_driver_3_refit"))
                        ),
                        tabBox(title = "Residual: White Noise?",
                               tabPanel("ACF", fig_acf_pacf_UI("fig_mod_diag_unitRoot")),
                               # tabPanel("Unit Root", tab_simple_UI("tab_mod_diag_autocorr")),
                               tabPanel("Stationary", tab_simple_UI("tab_stationary_residuals"), 
                                        tab_simple_UI("tab_mod_diag_autocorr")),
                               tabPanel("Homoscedasticity", tab_simple_UI("tab_mod_diag_homosce"), 
                                        fig_acf_pacf_UI("fig_acf_resid_sqr"))
                        ),
                        tabBox(title = "Residual Normality & Linearity",
                               tabPanel("QQ plot", fig_qq_lm_fit_UI("fig_qq_resid")),
                               # tabPanel("Histogram", fig_histgram_density_UI("fig_hist_refit_resid")),
                               tabPanel("Normality tests", tab_simple_UI("tab_mod_diag_norm")),
                               tabPanel("Linearity", fig_resid_linearity_lm_fit_UI("fig_resid_linear")),
                               tabPanel("Outlier", fig_outlier_lm_fit_UI("fig_outlier_mod_refit"))
                        ),
                        tabBox(title = "Model driver collinearity",
                               tabPanel("Correaltion", fig_corr_scatter_UI("fig_corr_scatter_refit")),
                               tabPanel("Covariate VIF", tab_simple_UI("tab_vif_refit"))
                        )
                        )
)
tab_mod_fore <- tabItem(tabName = "memu_mod_fore",
                        fluidRow(
                          tabBox(title = "Scenario data")
                        ))

######################################BEGIN: Dashboard UI ###################################################################
header <- dashboardHeader(
  title = "PPNR Scenario Projection"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Statistical Model", tabName = "menu_mod_stat", icon = icon("dashboard")), 
    menuItem("Model Override", tabName = "memu_mod_overide", icon = icon("balance-scale")),
    menuItem("Model Diagnostics", tabName = "memu_mod_diag", icon = icon("stethoscope")) # ,
    # menuItem("Scenario Forecast", tabName = "memu_mod_fore", icon = icon("line-chart"))
  ),
  h5(),
  sidebarMenu(
    selectInput("sel_mod_2_use", label = "Choose model:", choices=ui_nam_model),
    dateRangeInput("date_range_train", label = "Traning sample range", start = "1990-01-01", end = as.Date(Sys.time())),
    actionButton(inputId = "btn_estimate", label = "Re-Estimate"),
    actionButton(inputId = "btn_reset", label = "RESET"),
    selectInput("sel_scenario", label = "Stress scenario:", choices=ui_nam_scenario),
    sliderInput("sel_vol_initial", label = "Initial volume", min=100, max=1000, value=100, step = 1),
    sliderInput("sel_nim_initial", label = "Initial rate", min=0.5, max=5, value=1, step = 0.1)
  )
)

body <- dashboardBody(tabItems(
  # tabAuthen, # uiOutput("page"),   
  tab_mod_stat,
  tab_mod_override,
  tab_mod_diag
)
)

ui <- dashboardPage(header, sidebar, body)
####################################################################################################### END: dashboard UI####

######################################BEGIN: Sever ###################################################################
server <- function(input, output, session) { 
  output$debugPrint <-  renderPrint({
    "c(length(typ_var), length(nam_sel))"
  })
  
  ################################################ Part A: REACTIVE data prepare #############################################
  ### A.1. load statistical model
  reac_lst_mod_stat <- reactive({
    stopifnot(input$sel_mod_2_use %in% lst_control$dir_model$nam_mod)
    env_mod_fit <- new.env()
    load(lst_control$dir_model$dir_mod[match(input$sel_mod_2_use, lst_control$dir_model$nam_mod)], envir = env_mod_fit)
    env_mod_fit
  })
  ### A.2. variable names used in the model
  reac_lst_nam_mod_var <- callModule(get_var_names_mod_lm, "nam_var_mod_proposed", 
                                     reactive(reac_lst_mod_stat()$mod_res_col$mod_lm_fit))
  ### A.3. load scenarios
  reac_lst_delta_scenario <- reactive({
    stopifnot(input$sel_scenario %in% names(lst_control$sideBar_scenarios$namR))
    reac_lst_mod_stat()$lst_data2Mod$lst_diff_scenario[[names(lst_control$sideBar_scenarios$namR) == input$sel_scenario]]
  })
  ### A.5. prepare lst data for override fitting
  reac_lst_data_fit_mod <- callModule(prep_lst_data_fit_mod_serve, "prep_lst_fit_mod", 
                                      reactive(reac_lst_mod_stat()$lst_data2Mod$df_diff), 
                                      reactive(reac_lst_mod_stat()$lst_data2Mod$lst_diff_scenario), 
                                      reactive(reac_lst_mod_stat()$lst_data2Mod$meta))
  ### A.6. prepare business driver initial value to forecast
  reac_bus_var_type <- reactive({
    Check.IsScalar(reac_lst_mod_stat()$nam_bus_driver)
    stopifnot(reac_lst_mod_stat()$nam_bus_driver %in% reac_lst_mod_stat()$lst_data2Mod$meta$varR)
    with(reac_lst_mod_stat()$lst_data2Mod$meta, tranf_type[varR == reac_lst_mod_stat()$nam_bus_driver])
  })
  reac_bus_val_initial <- reactive({
    Check.IsScalar(reac_bus_var_type())
    stopifnot(reac_bus_var_type() %in% c("diff_abs", "diff_log", "diff_abs")) ### QUICK-FIX
    if (identical(reac_bus_var_type(), "diff_abs")){
      input$sel_nim_initial
    } else {
      input$sel_vol_initial
    }
  })
  ### selected traning sample for re-estimate linear model
  reac_df_train_2_reEstimate <- callModule(filter_df_timeSeries, "data_diff_reEstimate", reactive(reac_lst_data_fit_mod()$df_override_fit), 
                                           reactive(input$date_range_train[1]), reactive(input$date_range_train[2]))
  ########################################################################################################## END: Part A ###
  
  
  
  ################################################ Part C: Tab Statistical model #############################################
  ### C.1. Top left box
  output$tab_mod_stat_est <- DT::renderDataTable({
    reac_lst_mod_stat()$mod_res_col$mod_est %>% 
      mutate(laged = ifelse(grepl("_lag1", term), TRUE, FALSE)) %>% 
      mutate(nameR = gsub("_lag1", "", term)) %>% 
      left_join(select(reac_lst_mod_stat()$df_meta_info, nameR, desc), by="nameR") %>% 
      select(term, estimate, p.value, desc) %>% 
      mutate_at(c("estimate", "p.value"), function(x) round(x, 3))
  }, extensions = 'FixedColumns', options=list(dom = 't', scrollX = TRUE, fixedColumns = list(leftColumns = 1)), rownames = FALSE)
  ## adj-R-sqare for the proposed model
  callModule(print_serv, "print_rsq_orgfit", reactive(paste0("Proposed model with adjusted R-square of ", 
                                                             summary(reac_lst_mod_stat()$mod_res_col$mod_lm_fit)$adj.r.squared)))
  
  ## C.1.1 data for table and plot
  reac_df_mod_hist_fit <- reactive({
    res <- select(reac_lst_mod_stat()$df_mod_fit, date, one_of(reac_lst_mod_stat()$nam_bus_driver)) %>% 
      cbind(predict(reac_lst_mod_stat()$mod_res_col$mod_lm_fit, interval = "confidence")) 
    names(res) <- c("date", "obs", "pred", "lwr", "upr")
    res
  })
  output$fig_mod_hist_fit <- renderPlotly({ #
    obj_gg <- Plot_Pred_Conf_Inter(reac_df_mod_hist_fit()) + 
      ggtitle(paste0("Historical goodness of fit (Adj. R sqaure) is ", round(summary(reac_lst_mod_stat()$mod_res_col$mod_lm_fit)$adj.r.squared, 2)))  +
      ylab(paste0("Relative changes of ", reac_lst_mod_stat()$nam_bus_driver))
    ggplotly(obj_gg)
  })
  output$tab_mod_hist_fit <- DT::renderDataTable({
    reac_df_mod_hist_fit()
  }, extensions = 'FixedColumns', options=list(pageLength = 5, searchHighlight = TRUE, scrollX = TRUE, fixedColumns = list(leftColumns = 1)), rownames = FALSE)
  ### C.2. Top right panel
  callModule(fig_ggplot_date_serv, "var_depen", reactive(reac_lst_mod_stat()$lst_data2Mod$data), 
             reactive(gsub("_lag1", "", reac_lst_nam_mod_var()$nam_dep)))
  callModule(fig_ggplot_date_serv, "var_driver_1", reactive(reac_lst_mod_stat()$lst_data2Mod$data), 
             reactive(gsub("_lag1", "", reac_lst_nam_mod_var()$nam_driver[1])))
  callModule(fig_ggplot_date_serv, "var_driver_2", reactive(reac_lst_mod_stat()$lst_data2Mod$data), 
             reactive(gsub("_lag1", "", reac_lst_nam_mod_var()$nam_driver[2])))
  callModule(fig_ggplot_date_serv, "var_driver_3", reactive(reac_lst_mod_stat()$lst_data2Mod$data), 
             reactive(gsub("_lag1", "", reac_lst_nam_mod_var()$nam_driver[3])))
  callModule(tab_DT_searchPage_serv, "var_data_all", reactive(reac_lst_mod_stat()$lst_data2Mod$data), 
             reactive(gsub("_lag1", "", c(reac_lst_nam_mod_var()$nam_dep, reac_lst_nam_mod_var()$nam_driver))))
  ### C.3. Bottom left panel
  callModule(fig_scatter_serv, "eco1_vs_bus", reactive(reac_lst_data_fit_mod()$df_override_fit), 
             reactive(reac_lst_nam_mod_var()$nam_driver[1]), reactive(reac_lst_nam_mod_var()$nam_dep))
  callModule(fig_scatter_serv, "eco2_vs_bus", reactive(reac_lst_data_fit_mod()$df_override_fit), 
             reactive(reac_lst_nam_mod_var()$nam_driver[2]), reactive(reac_lst_nam_mod_var()$nam_dep))
  callModule(fig_scatter_serv, "eco3_vs_bus", reactive(reac_lst_data_fit_mod()$df_override_fit), 
             reactive(reac_lst_nam_mod_var()$nam_driver[3]), reactive(reac_lst_nam_mod_var()$nam_dep))
  callModule(tab_DT_searchPage_serv, "var_delta_all", reactive(reac_lst_data_fit_mod()$df_override_fit), 
             reactive(c(reac_lst_nam_mod_var()$nam_dep, reac_lst_nam_mod_var()$nam_driver)))
  ### C.5. Bottom RIGHT
  reac_df_hist_scaled <- reactive({
    df_inp <- reac_lst_data_fit_mod()$df_override_fit
    nam_sel <- as.character(unique(c(reac_lst_nam_mod_var()$nam_dep, reac_lst_nam_mod_var()$nam_driver)))
    typ_var <- with(reac_lst_mod_stat()$lst_data2Mod$meta, tranf_type[varR %in% gsub("_lag1", "", nam_sel)]) ### QUICK-FIX !!!
    ## checks
    stopifnot(all(c("date", nam_sel) %in% names(df_inp)))
    stopifnot(identical(length(typ_var), length(nam_sel)))
    stopifnot(any(c("diff_abs", "diff_log") %in% typ_var)) ### QUICK-FIX: CANNOT handle diff_rel
    df_sel <- select(df_inp, one_of(nam_sel))
    df_res <- df_sel
    df_res[, typ_var == "diff_abs"] <- input$sel_nim_initial + cumsum(df_res[, typ_var == "diff_abs"]) ### QUICK-FIX
    df_res[, typ_var == "diff_log"] <- input$sel_vol_initial * cumprod(1+ df_res[, typ_var == "diff_log"]) ### QUICK-FIX
    df_res$date <- df_inp$date
    select(df_res, one_of(c("date", nam_sel)))
  })
  callModule(tab_DT_searchPage_serv, "tab_mod_var_adj", reac_df_hist_scaled, reactive(names(reac_df_hist_scaled())), tab_with_date=FALSE)
  callModule(fig_ggplot_date_serv, "fig_mod_var_adj", reactive(tidyr::gather(reac_df_hist_scaled(), variable, value, -date)), 
             reactive("value"), group="variable", color = "variable")
  ########################################################################################################## END: Part C ###
  
  
  #################################################### Part D: Tab override model #####################################################
  ## initiate original model estimates
  reacVal_mod_est <- reactiveValues(reac_mod_est = reactive(reac_lst_mod_stat()$mod_res_col$mod_est),
                                    reac_mod_fit = reactive(reac_lst_mod_stat()$mod_res_col$mod_lm_fit),
                                    reac_df_diff_2_refit = reactive(reac_lst_data_fit_mod()$df_override_fit),
                                    reac_df_original_refit = reactive(reac_lst_mod_stat()$lst_data2Mod$data))
  # top left prepare override UI
  callModule(mod_override_serv, "lm_override", reactive(reacVal_mod_est$reac_mod_est()), reactive(reac_lst_data_fit_mod()$nam_candi_mod_driver))
  ## re-estimate LM according to chosen drivers
  observeEvent(input$btn_estimate, {
    reacVal_mod_est$reac_mod_est <- reactive(broom::tidy(fit_lm())) 
    reacVal_mod_est$reac_mod_fit <- reactive(fit_lm())
    reacVal_mod_est$reac_df_diff_2_refit <- reac_df_train_2_reEstimate
    reacVal_mod_est$reac_df_original_refit <- callModule(filter_df_timeSeries, "data_original_reEstimate", reactive(reac_lst_mod_stat()$lst_data2Mod$data), 
                                                         reactive(input$date_range_train[1]), reactive(input$date_range_train[2]))
  })
  ## RESET : fall back to original model estimate
  observeEvent(input$btn_reset, {
    reacVal_mod_est$reac_mod_est <- reactive(reac_lst_mod_stat()$mod_res_col$mod_est)
    reacVal_mod_est$reac_mod_fit <- reactive(reac_lst_mod_stat()$mod_res_col$mod_lm_fit)
    reacVal_mod_est$reac_df_diff_2_refit <- reactive(reac_lst_data_fit_mod()$df_override_fit)
    reacVal_mod_est$reac_df_original_refit <- reactive(reac_lst_mod_stat()$lst_data2Mod$data)
  })
  # top right: all candidate varaibles for override
  callModule(tab_DT_searchPage_serv, "overr_tab_all_eco_meta", reactive(reac_lst_data_fit_mod()$meta_df_override), 
             reactive(names(reac_lst_data_fit_mod()$meta_df_override)), tab_with_date=FALSE)
  fit_lm <- callModule(fit_lm_serv, "lm_fit_override", reactive(reac_lst_nam_mod_var()$nam_dep), 
                       reactive(reac_res_mod_overr()$term), reac_df_train_2_reEstimate)
  callModule(print_serv, "lm_fit_override", reactive(summary(fit_lm())))
  # bottom left: scatter plot between the dependent variable and overriden economic variable
  callModule(fig_scatter_serv, "overr_fig_eco1_vs_bus", reactive(reacVal_mod_est$reac_df_diff_2_refit()),
             reactive(reac_res_mod_overr()$term[2]), reactive(reac_lst_nam_mod_var()$nam_dep))
  callModule(fig_scatter_serv, "overr_fig_eco2_vs_bus", reactive(reacVal_mod_est$reac_df_diff_2_refit()),
             reactive(reac_res_mod_overr()$term[3]), reactive(reac_lst_nam_mod_var()$nam_dep))
  callModule(fig_scatter_serv, "overr_fig_eco3_vs_bus", reactive(reacVal_mod_est$reac_df_diff_2_refit()),
             reactive(reac_res_mod_overr()$term[4]), reactive(reac_lst_nam_mod_var()$nam_dep))
  # callModule(tab_DT_searchPage_serv, "overr_tab_eco_bus_data", reactive(reacVal_mod_est$reac_df_diff_2_refit()),
  #            reactive(c(reac_lst_nam_mod_var()$nam_dep, reac_res_mod_overr()$term[-1])), tab_with_date =TRUE) 
  # bottom right forecast for quick check
  reac_fore_overr <- callModule(fore_overr_serv, "lm_override", reac_res_mod_overr, 
                                reactive(reac_lst_data_fit_mod()$lst_scenario_eco), reac_bus_val_initial, reac_bus_var_type)
  # callModule(tab_fore_overr_serv, "lm_override", reac_fore_overr)
  callModule(tab_simple_serv, "ovrr_tab_fore_ratios", reactive(reac_fore_overr()$tab_fore_ratio))
  callModule(tab_simple_serv, "ovrr_tab_fore_values", reactive(reac_fore_overr()$tab_fore_val))
  reac_df_plot_fore_val <- reactive({
    tidyr::gather(reac_fore_overr()$tab_fore_val, scenario, value, -date)
  })
  callModule(fig_ggplot_date_serv, "ovrr_fig_fore_values", reac_df_plot_fore_val, reactive("value"),  col="scenario", group="scenario")
  # summary of overriden model
  reac_res_mod_overr <- callModule(tab_override_summary_serv, "lm_override", reactive(reac_lst_data_fit_mod()$meta_df_override))
  callModule(tab_simple_serv, "overr_tab_ovrridenMod", reac_res_mod_overr)
  ########################################################################################################## END: Part D override ###
  
  
  #################################################### Part E: Tab model diagnostics #####################################################
  ### top left tab
  # model overall fit
  reac_lst_nam_refit <- callModule(get_var_names_mod_lm, "nam_var_mod_refit", reactive(reacVal_mod_est$reac_mod_fit()))
  callModule(tab_simple_serv, "tab_mod_dig_fit_override", reactive(broom::tidy(reacVal_mod_est$reac_mod_fit())), nrDigt=3)
  callModule(print_serv, "print_rsq_refit", reactive(paste0("Re-estimate model with adjusted R-square of ", 
                                                            summary(reacVal_mod_est$reac_mod_fit())$adj.r.squared)))
  callModule(fig_ggplot_date_serv, "fig_var_depen_refit", reactive(reacVal_mod_est$reac_df_original_refit()), 
             reactive(gsub("_lag1", "", reac_lst_nam_refit()$nam_dep)))
  callModule(fig_ggplot_date_serv, "fig_var_driver_1_refit", reactive(reacVal_mod_est$reac_df_original_refit()), 
             reactive(gsub("_lag1", "", reac_lst_nam_refit()$nam_driver[1])))
  callModule(fig_ggplot_date_serv, "fig_var_driver_2_refit", reactive(reacVal_mod_est$reac_df_original_refit()), 
             reactive(gsub("_lag1", "", reac_lst_nam_refit()$nam_driver[2])))
  callModule(fig_ggplot_date_serv, "fig_var_driver_3_refit", reactive(reacVal_mod_est$reac_df_original_refit()), 
             reactive(gsub("_lag1", "", reac_lst_nam_refit()$nam_driver[3])))
  # plot of model historical fit
  df_hist_fit_refit <- reactive({
    data_frame(val_fitted = as.numeric(reacVal_mod_est$reac_mod_fit()$fitted.values),
               val_resid = as.numeric(reacVal_mod_est$reac_mod_fit()$residuals),
               date = reacVal_mod_est$reac_df_diff_2_refit()$date)
  })
  callModule(fig_hist_mod_fit_serv, "fig_hist_fit_mod_refit", 
             df_hist_fit_refit, reactive("val_fitted"), reactive(df_hist_fit_refit()$val_resid), 
             reactive(summary(reacVal_mod_est$reac_mod_fit())$sigma))
  ### top RIGHT tabBox
  # Residual autocorrelation
  reac_tab_mod_diag <- reactive({
    Test_Mod_Residuals(reacVal_mod_est$reac_mod_fit())
  })
  ## Unit root
  callModule(tab_simple_serv, "tab_mod_diag_autocorr", 
             reactive(filter(reac_tab_mod_diag(), grepl("Box", method) | grepl("Durbin", method))))
  ## Stationary
  callModule(tab_simple_serv, "tab_stationary_residuals", 
             reactive(TestsStationaryPval(as.numeric(reacVal_mod_est$reac_mod_fit()$residuals))))
  # Residual ACF plot
  callModule(fig_acf_pacf_serv, "fig_mod_diag_unitRoot", reactive(reacVal_mod_est$reac_mod_fit()$residuals), "Residuals")
  ## homoscedasticity Breusch
  callModule(tab_simple_serv, "tab_mod_diag_homosce", 
             reactive(filter(reac_tab_mod_diag(), grepl("Breusch", method))))
  callModule(fig_acf_pacf_serv, "fig_acf_resid_sqr", reactive(reacVal_mod_est$reac_mod_fit()$residuals^2), "Residuals^2 (considering GARCH?)")
  
  ### Bottom Left tabBox
  # normality test
  callModule(tab_simple_serv, "tab_mod_diag_norm", reactive(filter(reac_tab_mod_diag(), grepl("normality", method) )))
  # Residual QQ plot
  callModule(fig_qq_lm_fit_serv, "fig_qq_resid", reactive(reacVal_mod_est$reac_mod_fit()))
  # Residual Histgram
  # callModule(fig_histgram_density_serv, "fig_hist_refit_resid", 
  #            reactive(data_frame(resid = reacVal_mod_est$reac_mod_fit()$residuals)), "resid", "Re-estimated model residual histogram")
  # Residual linearity plots
  callModule(fig_resid_linearity_lm_fit_serv, "fig_resid_linear", reactive(reacVal_mod_est$reac_mod_fit()))
  callModule(fig_outlier_lm_fit_serv, "fig_outlier_mod_refit", reactive(reacVal_mod_est$reac_mod_fit()))
  
  ### Bottom Right tabBox
  callModule(fig_corr_scatter_serv, "fig_corr_scatter_refit", 
             reactive(broom::tidy(reacVal_mod_est$reac_mod_fit())$term), reactive(reacVal_mod_est$reac_df_diff_2_refit()))
  callModule(tab_simple_serv, "tab_vif_refit", reactive(data.frame(VIF = car::vif(reacVal_mod_est$reac_mod_fit()))), 
             nrDigt = 3, rownames = TRUE)
  ########################################################################################################## END: Part E mode diagnostics###
  
  
  #######close the R session when Chrome closes
  # session$onSessionEnded(function() {
  #   stopApp()
  #   q("no")
  # })
}

shinyApp(ui, server)
# runApp(shinyApp(ui, server), launch.browser=T)
