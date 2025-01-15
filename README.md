# Consistency in Hitting: An Analysis of Bat Speed and Swing Length in Major League Baseball

This project is a submission for the CSAS 2025 Data Challege. For more information, visit the official website:
https://statds.org/events/csas2025/challenge.html



## Overview
This study analyzes bat speed and swing length to explore consistency in Major League Baseball batters during the 2024 season. We examine batters with consistent swings and seek to understand the conditions that significantly alter their swing. We define "consistent batters" as those with low interquartile ranges (IQR) for both bat speed and swing length, and “outlier swings” as a swing that is an IQR-defined outlier. Our analysis reveals that outlier swings are significantly influenced by factors such as ball-strike count, inning, and pitch type, with batters more likely to deviate from their typical swing when behind in the count or facing pitches like sliders. We found that a significant amount of outlier swings resulted in a much higher swinging strike rate—and much lower contact rate—than normal. More analysis must be done in the future to further explain the factors leading to outlier swings and find ways to implement these factors in game plans.



## Files Included

This repository includes the following files:
- `research.pdf`   (the research paper regarding constistency in batting, describing the methods and results of the study)
- `file.R` (the R script used to analyze the data and create visualizations)
- `data.csv` (the original data set provided by Baseball Savant)



## Requirements

This project uses R for data analysis and data visualization. R can be downloaded on the official website:

https://cran.r-project.org


The packages used are as follows:

- `readxl`
- `dplyr`
- `tidyr`
- `ggplot2`

To install these packages, run the following command in your R console:

`install.packages(c("readxl", "dplyr", "tidyr", "ggplot2"))`

Set your working directory to your folder containing both `file.R` and `data.csv`.

`setwd("path/to/directory")`

To run the R script, execute the following command:

`source("file.R")`
