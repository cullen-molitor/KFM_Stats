# MPA Community Trend Report
Repository used to build the KFM MPA community Trend Report.

## Usage

In order to use this script, it is necessary to download or have the raw text files from the Kelp Forest Monitoring access database. View the Database Management SOP in the Kelp Forest Monitoring Handbook for instructions on how to create an export database with the correct tables. With the export databsae setup, the tables should be exported as text files. Only export raw data. Data exports stored in this respository are current as of 2019.

### Export Data
The files needed are:

1. "KFM_1mQuadrat_RawData_1982-CurrentYear.txt"
2. "KFM_5mQuadrat_RawData_1982-CurrentYear.txt"
3. "KFM_BandTransect_RawData_1982-CurrentYear.txt"
4. "KFM_RandomPointContact_RawData_1982-CurrentYear.txt"
5. "KFM_FishBiomass_2007-CurrentYear.txt"
6. "KFM_InvertebrateBiomass_1985-CurrentYear.txt"
7. "KFM_Macrocystis_RawData_1984-CurrentYear.txt"
8. "KFM_RovingDiverFishCount_RawData_1982-CurrentYear.txt"

These files should all be placed in the folder titled "Raw_DB_Files_SAVE_HERE" in the directory of this project. This report cannot be created without these data files. Everything else is already contained in this repository.

### Change Two Variables
With the raw data files in place, open the "global_markdown.R" script in RStudio. There are two variables that need to be changed in this script and both of them are years. 

1. "Year_to_Filter_Data_by <- *CHANGE_ME*" will need to be changed to the last year of the intended report. If the report goes from 2020-20205 this variable will need to be 2025
2. "Export_END_Year <- *CHANGE_ME*" will need to be changed to reflect the year the data files were downloaded. If the files were exported from the database in 2020, change the year to 2020. All the files you saved need to have this year for the code to work.

### Raw Data to Tidy Data
Open the "Raw_to_Tidy.R" script, select all, and hit CTRL + Enter to run. This will take all the raw .txt files you saved and turn them into "Tidy" .csv files saved to the folder "Tidy_Data_Dont_Touch". 

This script will create the output that the "MPA_Community_Trend_Report.Rmd" file will use to run the analysis.

### Create the Report
Open the "MPA_Community_Trend_Report.Rmd" file and hit "Knit." This will generate the word document with the relevant text, andlysis, results, and discussion. The document will be saved in the folder "Output_Documents."

Open this document and create section breaks beofre and after all the plots that need to be in landscape orientation. Once all the section breaks are made, and all sections are in proper orientation, resize the plots in the landscape orientation. Resize the first one by right clicking, selecting "size and position", select the box "lock aspect ratio", then resize the width to be 9" absolute, and click "ok". Click on the next plot that needs to be resized and hit F4 to automatically apply the previous formatting changes.

