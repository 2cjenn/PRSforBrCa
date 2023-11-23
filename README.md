# Assessing the value of incorporating a polygenic risk score with non-genetic factors for predicting breast cancer diagnosis in the UK Biobank

This repository contains the code for the analyses presented in the manuscript _"Assessing the value of incorporating a polygenic risk score with non-genetic factors for predicting breast cancer diagnosis in the UK Biobank"._

The code reads UK Biobank data that has been saved in a [DuckDB](https://duckdb.org/) database, in the format produced by the scripts in https://github.com/2cjenn/UKB_database. To view an automatically generated data dictionary, please see [PRSforBrCa.html](https://htmlpreview.github.io/?https://github.com/2cjenn/PRSforBrCa/blob/main/PRSforBrCa.html).

This research has been conducted using the UK Biobank Resource under Application Number 33952. Requests to access the data should be made via application directly to the UK Biobank, https://www.ukbiobank.ac.uk.

## Overview of repository directories

* **Scripts/Data_Management/Scripts_Cleaning**: Processing scripts used to define variables and extract data from the database
* **Scripts/Data_Management/Scripts_Processing**: Script to prepare UK Biobank data in the correct format to input to the [Tyrer-Cuzick model v8](https://ems-trials.org/riskevaluator/)
* **Scripts/Stats_Analysis/RMarkdown**: Analysis scripts to produce the manuscript tables and figures 
