# Election 2019 Realignment Visualisation
A PCA model of UK Constituencies to visualise the effect of Leave support on the Labour vote 

Based on YouGov MRP data for 2017 and 2019, and demographic information about constituencies from the House of Commons Library website.
Source data not provided here (though the code to transform the data is as create_dataset.R), only the combined dataset.

Variables used in the PCA:

-Population for each age group (10 year bands)
-Percentage of population with at least two A-levels
-Median weekly salary
-Average house prices as a multiple of median weekly salary
-Proportion of managerial or professional occupations
-Propotion of jobs paying less than the local living wage figure (according to Living Wage Foundation)
-Proportion of home owners
-Number of businesses 
-Net change in number businesses over last year

The main code for the analysis is in visualising_2019_realignment.R
Some further linear models to explore other aspects of the data are in supplementary_models.R

