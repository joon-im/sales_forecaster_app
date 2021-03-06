# Sales Forecasting App

![Screen_Shot_2022-07-07_at_6_52_05_PM](https://user-images.githubusercontent.com/32493276/178042319-15ed1be5-2d42-452c-aa14-cd07a52c6c36.jpg)


This app is a proof of concept for automating sales forecasts at scale. It uses a local forecasting approach as opposed to a global approach which essentially means that multiple models are fit to each time series instead of just one. The dataset used is a subset of the Walmart item sales dataset used in the M5 forecasting competition. The original dataset has been filtered down to the top 100 highest-selling items. This app was built using the R programming languge and a number of libraries including shiny, flexdashboard, tidyverse, tidymodels, and the timetk and modeltime time series forecasting suite. Using the modeltime workflow, it automatically fits and selects the best fitting model on an out-of-sample test set and then refits it to the original time series data for future prediction. 

Download M5 Dataset Here: https://www.kaggle.com/competitions/m5-forecasting-accuracy
