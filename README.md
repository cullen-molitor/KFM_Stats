# Annual_Reports
Repository of Rmarkdown scripts used to build KFM Annual Report appendix graphs.

## Usage

In order to use this script, it is necessary to download or have the raw text files from the Kelp Forest Monitoring access database. View the Database Management SOP in the Kelp Forest Monitoring Handbook SOP folder for instructions on how to create an export database with the correct tables. With the export databsae setup, the tables should be exported as text files. Only export raw data.

### Export Data
The files needed are:

1. "KFM_1mQuadrat_RawData_1982-CurrentYear.txt"
2. "KFM_5mQuadrat_RawData_1982-CurrentYear.txt"
3. "KFM_BandTransect_RawData_1982-CurrentYear.txt"
4. "KFM_RandomPointContact_RawData_1982-CurrentYear.txt"
5. "KFM_VisualFishTransect_RawData_1985-CurrentYear.txt"
6. "Temperature_RawData_1994-CurrentYear.txt"
7. "KFM_NaturalHabitatSizeFrequency_RawData_1985-CurrentYear.txt"
8. "KFM_RovingDiverFishCount_RawData_1982-CurrentYear.txt"

These files should all be placed in the folder titled "Raw_DB_Files_SAVE_HERE" in the directory with this project.

### Change Two Variables
With the raw data files in place, open the "global_markdown.R" script in RStudio. There are two variables that need to be changed iun this script and both of them are years. 

1. "Year_to_Filter_Data_by <- *CHANGE_ME*" will need to be changed to the year that you would like to make graphs for. If making the 2019 reports, change the year to 2019.
2. "Export_END_Year <- *CHANGE_ME*" will need to be changed to reflect the year the data files were downloaded. If the files were exported from the database in 2020, change the year to 2020. All the files you saved need to have this year for the code to work.

### Raw to Tidy Data
Open the "RUN.R" script. Select the first line of uncommented code "source("Raw_to_Tidy.R")" and hit CTRL + Enter to run. This will take all the raw .txt files you saved and turn them into "Tidy" .csv files saved to the folder "Tidy_Data_Dont_Touch". This  will output three ".csv" files for every text file. 

1. "protocol_Raw_Tidy.csv"
2. "protocol_Summary.csv"
3. "protocol_MPA.csv"

The first file is still raw data, though it is tidy and clean. The second is summary data that has mean densities, standard deviations, and standard errors calculated. The third file is summary data explicitly for the 24 MPA reference sites.

### Make the Appendix Documents
With the tidy data frames in place, you will need to run the next lines of uncommented code in the "RUN.R" file. Each line will create a new appendix document and save them to the folder "Output_Documents."