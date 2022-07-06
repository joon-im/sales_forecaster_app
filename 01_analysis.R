# NESTED FORECASTING APP ----

# Use the development version of modeltime
# remotes::install_github("business-science/modeltime", dependencies = TRUE)

# LIBRARIES & DATA ----
library(modeltime)
library(tidymodels)
library(tidyverse)
library(timetk)
library(plotly)
library(ggthemes)

sales_raw_tbl <- read_rds("~/R/MIS480_Portfolio/data/walmart_item_sales.rds")

sample_12_tbl <- sales_raw_tbl %>%
    filter(as.numeric(item_id) %in% 1:12)

sample_12_tbl %>%
    group_by(item_id) %>%
    plot_time_series(date, value, .facet_ncol = 3, .smooth = FALSE)

# Create nested time series data
nested_data_tbl <- sales_raw_tbl %>%
    group_by(item_id) %>%
    extend_timeseries(
        .id_var = item_id,
        .date_var = date,
        .length_future = 90
    ) %>%
    nest_timeseries(
        .id_var = item_id,
        .length_future = 90
    ) %>%
    split_nested_timeseries(
        .length_test = 90
    )

# Show last 6 nested time series
nested_data_tbl %>% tail()


# MODELING ----

# XGBoost Recipe
rec_xgb <- recipe(value ~ ., extract_nested_train_split(nested_data_tbl)) %>%
    step_timeseries_signature(date) %>%
    step_rm(date) %>%
    step_zv(all_predictors()) %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE)

bake(prep(rec_xgb), extract_nested_train_split(nested_data_tbl))

# XGBoost Models
wflw_xgb_1 <- workflow() %>%
    add_model(boost_tree("regression", learn_rate = 0.35) %>% set_engine("xgboost")) %>%
    add_recipe(rec_xgb)

wflw_xgb_2 <- workflow() %>%
    add_model(boost_tree("regression", learn_rate = 0.50) %>% set_engine("xgboost")) %>%
    add_recipe(rec_xgb)

# Temporal hierarchical forecasting model
wflw_thief <- workflow() %>%
    add_model(temporal_hierarchy() %>% set_engine("thief")) %>%
    add_recipe(recipe(value ~ ., extract_nested_train_split(nested_data_tbl)))

# 1.0 TRY 1 TIME SERIES ----
#   - Tells us if our models work at least once (before we scale)

try_sample_tbl <- nested_data_tbl %>%
    slice(2) %>%
    modeltime_nested_fit(

        model_list = list(
            wflw_xgb_1,
            wflw_xgb_2,
            wflw_thief
        ),

        control = control_nested_fit(
            verbose   = TRUE,
            allow_par = FALSE
        )
    )

try_sample_tbl

# * Check Errors ----

try_sample_tbl %>% extract_nested_error_report()


# 2.0 SCALE ----
#  - LONG RUNNING SCRIPT (2-4 MIN)
parallel_stop()

# Parallel computing with 2 cores
parallel_start(2)

# Apply models to nested data
nested_modeltime_tbl <- nested_data_tbl %>%
    #slice_tail(n = 6) %>%
    modeltime_nested_fit(
        model_list = list(
            wflw_xgb_1,
            wflw_xgb_2,
            wflw_thief
        ),
        control = control_nested_fit(
            verbose   = TRUE,
            allow_par = TRUE
        )
    )

# Show new dataset containing forecast results
nested_modeltime_tbl

# FILES REMOVED: Too large
# nested_modeltime_tbl %>% write_rds("artifacts/nested_modeltime_tbl.rds")
# nested_modeltime_tbl <- read_rds("artifacts/nested_modeltime_tbl.rds")

# * Review Any Errors ----
nested_modeltime_tbl %>% extract_nested_error_report()

nested_modeltime_tbl %>%
    filter(item_id == "HOUSEHOLD_2_101") %>%
    extract_nested_train_split()

# * Review Test Accuracy ----
nested_modeltime_tbl %>%
    extract_nested_test_accuracy() %>%
    mutate(
        .model_desc = replace(
            .model_desc, .model_desc == "TEMPORAL HIERARCHICAL FORECASTING MODEL", "Temporal Hierarchical"),
        .model_desc = replace(
            .model_desc, .model_desc == "XGBOOST", "XGBoost"),
    ) %>%
    table_modeltime_accuracy(.title = "crap")

# Visualize a test forecast
nested_modeltime_tbl %>%
    extract_nested_test_forecast() %>%
    filter(item_id == "FOODS_3_090") %>%
    group_by(item_id) %>%
    plot_modeltime_forecast(.interactive = FALSE)

# * Capture Results:
#   - Deal with small time series (<=90 days)
ids_small_timeseries <- "HOUSEHOLD_2_101"

nested_modeltime_subset_tbl <- nested_modeltime_tbl %>%
    filter(!item_id %in% ids_small_timeseries)

# 3.0 SELECT BEST ----

nested_best_tbl <- nested_modeltime_subset_tbl %>%
    modeltime_nested_select_best(metric = "rmse")

# * Visualize Best Models ----
nested_best_tbl %>%
    extract_nested_test_forecast() %>%
    filter(as.numeric(item_id) %in% 1:12) %>%
    group_by(item_id) %>%
    plot_modeltime_forecast(.facet_ncol = 3)


# 4.0 REFIT ----
#  - Long Running Script: 25 sec

# Refit best model
nested_best_refit_tbl <- nested_best_tbl %>%
    modeltime_nested_refit(
        control = control_refit(
            verbose   = TRUE,
            allow_par = TRUE
        )
    )

# Visualize future forecast of best-fitting model
nested_best_refit_tbl %>%
    extract_nested_future_forecast() %>%
    filter(item_id == "FOODS_3_586") %>%
    group_by(item_id) %>%
    plot_modeltime_forecast(.interactive = FALSE)

# FILES REMOVED: Too large
# nested_best_refit_tbl %>% write_rds("artifacts/nested_best_refit_tbl.rds")
# nested_best_refit_tbl <- read_rds("artifacts/nested_best_refit_tbl.rds")

# * Review Any Errors ----
nested_best_refit_tbl %>% extract_nested_error_report()

# * Visualize Future Forecast ----
nested_best_refit_tbl %>%
    extract_nested_future_forecast() %>%
    filter(as.numeric(item_id) %in% 1:12) %>%
    group_by(item_id) %>%
    plot_modeltime_forecast(.facet_ncol = 3)


# 5.0 HANDLE ERRORS (SMALL TIME SERIES) ----

# * Nested Time Series ----
nested_data_small_ts_tbl <- sales_raw_tbl %>%
    filter(item_id %in% ids_small_timeseries) %>%
    group_by(item_id) %>%
    extend_timeseries(.id_var = item_id, .date_var = date, .length_future = 90) %>%
    nest_timeseries(.id_var = item_id, .length_future = 90) %>%
    split_nested_timeseries(.length_test = 30)

# * Fit, Select Best, & Refit ----
nested_best_refit_small_ts_tbl <- nested_data_small_ts_tbl %>%
    modeltime_nested_fit(

        model_list = list(
            wflw_xgb_1,
            wflw_xgb_2,
            wflw_thief
        ),

        control = control_nested_fit(
            verbose   = TRUE,
            allow_par = FALSE
        )
    ) %>%
    modeltime_nested_select_best() %>%
    modeltime_nested_refit()

nested_best_refit_small_ts_tbl %>%
    extract_nested_future_forecast() %>%
    group_by(item_id) %>%
    plot_modeltime_forecast(.facet_ncol = 3)

# * Recombine ----

nested_best_refit_all_tbl <- nested_best_refit_tbl %>%
    bind_rows(nested_best_refit_small_ts_tbl)

#nested_best_refit_all_tbl %>% write_rds("~/R/MIS480_Portfolio/artifacts/best_models_tbl_TEST.rds")

nested_best_refit_all_tbl <- read_rds("~/R/sales_forecaster_app/artifacts/best_models_tbl.rds")

# BONUS 2: NEW WORKFLOW ----
#   - New Function: modeltime_nested_forecast()
#   - Used to make changes to your future forecast

parallel_stop()

parallel_start(2)
new_forecast_tbl <- nested_best_refit_all_tbl %>%
    modeltime_nested_forecast(
        h = 365,
        conf_interval = 0.99,
        control = control_nested_forecast(
            verbose   = TRUE,
            allow_par = FALSE
        )
    )

new_forecast_tbl %>%
    filter(as.numeric(item_id) %in% 1:12) %>%
    group_by(item_id) %>%
    plot_modeltime_forecast(.facet_ncol = 3)


new_forecast_tbl %>%
    select(.model_desc, .key) %>%
    distinct()



t <- new_forecast_tbl %>%
    filter(as.numeric(item_id) %in% 1:4) %>%
    mutate(
        .model_desc = replace(
            .model_desc, .model_desc == "TEMPORAL HIERARCHICAL FORECASTING MODEL", "Temporal Hierarchical"),
        .model_desc = replace(
            .model_desc, .model_desc == "XGBOOST", "XGBoost"),
        ) %>%
    group_by(item_id) %>%
    plot_modeltime_forecast(.facet_ncol = 2,
                            .interactive = FALSE,
                            .title = "Sales Forecast") +
    theme_gdocs() +
    scale_color_tableau() +
    theme(plot.title = element_text(color = "#424242", face = "bold"),
          legend.title = element_text(color = "#424242"),
          legend.text = element_text(color = "#616161"))

ggplotly(t)

# BONUS 3: SHINY APP ----
output$plotly_1 <- renderPlotly({

    facet_col <- 1

    if (length(input$products) > 3) facet_col <- 2

    if (length(input$products) > 8) facet_col <- 4

    rv$forecast_tbl %>%
        group_by(item_id) %>%
        plot_modeltime_forecast(
            .facet_ncol = facet_col,
            .legend_max_width = 26,
            .plotly_slider = FALSE
        )
})

plotlyOutput(outputId = "plotly_1")
