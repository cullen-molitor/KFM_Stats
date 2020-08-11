#
#
#
#
#     Use for Manipulating data frames 
#
#
#
#
#

source("global_markdown.R")

{ # SST Anomaly Index (ONI and PDO)   ----
  
  { # Oceanic Nino Index  ----
    oni <- read.table( # Read in  ONI to be added to all data
      "https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/detrend.nino34.ascii.txt",
      header = T) %>%
      dplyr::mutate(Date = as.Date(ISOdate(YR, MON, 1)),
                    DateStart = as.Date(ISOdate(YR, MON, 1)),
                    DateEnd = ceiling_date(DateStart, "month")) %>%
      dplyr::filter(YR > 2004, Date < glue("{Year_to_Filter_Data_by}-2-1")) %>%
      dplyr::rename(ONI_ANOM = ANOM,
                    Month = MON,
                    SurveyYear = YR) %>% 
      dplyr::group_by(SurveyYear) %>%
      dplyr::mutate(Mean_ONI_ANOM = mean(ONI_ANOM)) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(SurveyYear, Month, Date, DateStart, DateEnd, ONI_ANOM, Mean_ONI_ANOM) 
    
    annual_mean_oni <- oni %>% 
      dplyr::select(SurveyYear, Mean_ONI_ANOM) %>% 
      dplyr::distinct(SurveyYear, .keep_all = TRUE)
  }
  
  { # PDO  ----
    pdo <- read.table(
      "https://www.cpc.ncep.noaa.gov/products/GODAS/PDO/pdo_h300_pac_current.txt",
      header = T)  %>%
      dplyr::mutate(Date = as.Date(ISOdate(Year, Month, 1)),
                    DateStart = as.Date(ISOdate(Year, Month, 1)),
                    DateEnd = ceiling_date(DateStart, "month")) %>%
      dplyr::filter(Year > 2004, Date < glue("{Year_to_Filter_Data_by}-2-1")) %>%
      dplyr::rename(PDO_ANOM = PDO,
                    SurveyYear = Year) %>% 
      dplyr::group_by(SurveyYear) %>%
      dplyr::mutate(Mean_PDO_ANOM = mean(PDO_ANOM)) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(SurveyYear, Month, Date, DateStart, DateEnd, PDO_ANOM, Mean_PDO_ANOM)
    
    annual_mean_pdo <- pdo %>% 
      dplyr::select(SurveyYear, Mean_PDO_ANOM) %>% 
      dplyr::distinct(SurveyYear, .keep_all = TRUE)
  }
  
  { # Full Index  ----
    SST_Anomaly_Index <- dplyr::left_join(pdo, oni) %>% 
      readr::write_csv("Tidy_Data_Dont_Touch/SST_Anomaly_Index.csv")
  }
  
}

{ # Benthic Count Table for Diversity   ----
  
  { # 1 m Density     ----
    oneM_Count_Data <- readr::read_csv(
      glue("Raw_DB_Files_SAVE_HERE/KFM_1mQuadrat_RawData_1982-{Export_END_Year}.txt"),   
      col_types = cols(CountA = col_number(), CountB = col_number())) %>%
      dplyr::filter(IslandCode != "CL",
                    ScientificName != "Lithopoma gibberosa" | SurveyYear > 2002) %>%
      dplyr::left_join(siteInfo1) %>%
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::mutate(Date = as.Date(Date, format='%m/%d/%Y')) %>%
      tidyr::pivot_longer(cols = c(CountA, CountB), values_to = "Count") %>% 
      dplyr::filter(!is.na(Count), !is.na(CommonName),
                    CommonName != "giant kelp stipes > 1m") %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                    CommonName, SurveyYear, Date, QuadratNumber, Count, ReserveStatus, MeanDepth, Reference) %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName,  ScientificName,
                      SurveyYear, Date, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = ifelse(SurveyYear %in% 1985:1994, n() * 2, n()),
                       Total_Count = base::sum(Count),
                       Mean_Density = base::round(Total_Count / Area_Surveyed, 4), 
                       SD = base::round(stats::sd(Count), 4),
                       SE = base::round(SD / base::sqrt(Area_Surveyed), 4),
                       Survey_Type = "One_Meter") %>% 
      dplyr::ungroup() %>%  
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, SurveyYear, Date, Total_Count, 
                    Mean_Density, SD, SE, Area_Surveyed, Survey_Type, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, SurveyYear, 
                      Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE)  %>% 
      dplyr::filter(ScientificName != "Pisaster giganteus" | SurveyYear < 1996,
                    ScientificName != "Macrocystis pyrifera" | SurveyYear < 1996,
                    ScientificName != "Undaria pinnatifida",
                    ScientificName != "Dictyoneuropsis reticulata/Agarum fimbriatum",
                    ScientificName != "Haliotis rufescens",
                    ScientificName != "Crassedoma giganteum",
                    ScientificName != "Kelletia kelletii",
                    ScientificName != "Oxylebius pictus",
                    ScientificName != "Pycnopodia helianthoides",
                    ScientificName != "Lytechinus anamesus",
                    ScientificName != "Sargassum horneri") %>%
      dplyr::arrange(SiteNumber, SurveyYear, ScientificName) %>% 
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName, 
                    SurveyYear, Mean_Density, ReserveStatus, Reference)
  } 
  
  { # 5 m Density    ----
    fiveM_Count_Data <- readr::read_csv(
      glue("Raw_DB_Files_SAVE_HERE/KFM_5mQuadrat_RawData_1996-{Export_END_Year}.txt")) %>%
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::filter(IslandCode != "CL") %>%
      dplyr::left_join(siteInfo1) %>%
      dplyr::mutate(Date = as.Date(Date, format='%m/%d/%Y')) %>%
      dplyr::filter(!is.na(Count), !is.na(CommonName)) %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                    CommonName, SurveyYear, Date, QuadratNumber, Count, ReserveStatus, MeanDepth, Reference) %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      SurveyYear, Date, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = dplyr::n() * 5,
                       Total_Count = base::sum(Count),
                       Mean_Density = base::round(Total_Count / Area_Surveyed, 4), 
                       SD = base::round(stats::sd(Count), 4),
                       SE = base::round(SD / base::sqrt(Area_Surveyed), 4),
                       Survey_Type = "Five_Meter") %>% 
      dplyr::ungroup() %>%  
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, 
                    SurveyYear, Date, Total_Count, Mean_Density, SD, SE, Area_Surveyed, MeanDepth, 
                    Survey_Type, ReserveStatus, Reference) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, 
                      ScientificName, SurveyYear, Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE)  %>% 
      dplyr::filter(ScientificName != "Pisaster giganteus" | SurveyYear < 2014,
                    ScientificName != "Pisaster ochraceus" | SurveyYear < 2014,
                    ScientificName != "Undaria pinnatifida") %>%
      dplyr::arrange(SiteNumber, SurveyYear, ScientificName) %>% 
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName, 
                    SurveyYear, Mean_Density, ReserveStatus, Reference)
    
  }
  
  { # Bands Density    ----
    bands_Count_Data <- read_csv(
      glue("Raw_DB_Files_SAVE_HERE/KFM_BandTransect_RawData_1982-{Export_END_Year}.txt"),   
      col_types = cols(CountA = col_number(), CountB = col_number())) %>%
      dplyr::filter(IslandCode != "CL") %>%
      dplyr::left_join(siteInfo1) %>%
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::mutate(Date = as.Date(Date, format='%m/%d/%Y')) %>%
      tidyr::pivot_longer(cols = c(CountA, CountB), values_to = "Count") %>% 
      dplyr::filter(!is.na(Count), !is.na(CommonName)) %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                    CommonName, SurveyYear, Date, TransectNumber, Count, ReserveStatus, MeanDepth, Reference)  %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      SurveyYear, Date, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = ifelse(SurveyYear %in% 1983:1984, n() * 40, n() *60),
                       Total_Count = base::sum(Count),
                       Mean_Density = base::round(Total_Count / Area_Surveyed, 4), 
                       SD = base::round(stats::sd(Count), 4),
                       SE = base::round(SD/base::sqrt(Area_Surveyed), 4),
                       Survey_Type = "Bands") %>% 
      dplyr::ungroup() %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, SurveyYear, Date,
                    Total_Count, Mean_Density, SD, SE, Area_Surveyed, MeanDepth, Survey_Type, ReserveStatus, Reference) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      SurveyYear, Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE) %>% 
      dplyr::filter(ScientificName != "Sargassum horneri") %>%
      dplyr::arrange(SiteNumber, SurveyYear, ScientificName) %>% 
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName, 
                    SurveyYear, Mean_Density, ReserveStatus, Reference)
    
  }
  
  { # Benthic Count Table   ----
    Benthic_Counts_Wide <- base::rbind(oneM_Count_Data, fiveM_Count_Data, bands_Count_Data) %>% 
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName, 
                    SurveyYear, Mean_Density, ReserveStatus, Reference) %>%
      dplyr::filter(Reference == TRUE, SurveyYear > 2004, SiteCode != "KH") %>% 
      dplyr::mutate(Est_Total_Count = Mean_Density * 2000) %>% 
      dplyr::group_by(IslandCode, IslandName, SiteCode, SiteName, ScientificName, SurveyYear, ReserveStatus) %>%
      dplyr::summarise(Est_Total_Count = base::sum(Est_Total_Count)) %>%
      dplyr::ungroup() %>% 
      tidyr::pivot_wider(names_from = ScientificName, values_from = Est_Total_Count, values_fill = 0) %>%
      readr::write_csv("Tidy_Data_Dont_Touch/Benthic_Counts.csv") 
  }

}

{ # Fish Count Table for Diversity   ----
  Fish_Counts_Wide <- data.table::fread(
    glue("Raw_DB_Files_SAVE_HERE/KFM_RovingDiverFishCount_RawData_1982-{Export_END_Year}.txt")) %>%
    dplyr::mutate(CommonName = gsub('ñ', 'n', CommonName),
                  Abundance = gsub("c", "C", Abundance),
                  Abundance = gsub("f", "F", Abundance),
                  Abundance = gsub("s", "S", Abundance),
                  Abundance = gsub("m", "M", Abundance),
                  Abundance = gsub("^$", NA, Abundance),
                  Abundance = gsub("-", NA, Abundance)) %>%
    dplyr::filter(IslandCode != "CL") %>% 
    tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
    dplyr::mutate(Date = base::as.Date(Date, format = '%m/%d/%Y'),
                  Count = as.double(Count)) %>% 
    dplyr::group_by(SiteCode, SurveyYear) %>% 
    dplyr::filter(Date == base::max(Date), SurveyYear > 1996, ExperienceLevel == "E") %>%
    dplyr::ungroup() %>% 
    dplyr::left_join(siteInfo1) %>%
    dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                  SurveyYear, Count, ReserveStatus, Reference) %>%
    dplyr::filter(Reference == TRUE, SurveyYear > 2004, SiteCode != "KH") %>% 
    dplyr::mutate(Count= replace_na(Count, 0)) %>%
    dplyr::group_by(IslandCode, IslandName, SiteCode, SiteName, ScientificName, SurveyYear, ReserveStatus) %>%
    dplyr::summarise(Count = mean(Count)) %>% 
    dplyr::ungroup() %>% 
    tidyr::pivot_wider(names_from = ScientificName, values_from = Count, values_fill = 0) %>%
    readr::write_csv("Tidy_Data_Dont_Touch/Fish_Counts.csv") 
  
  # `%nin%` = Negate(`%in%`)
  # 
  # fishyyy <- read_csv("Tidy_Data_Dont_Touch/RDFC_Raw_Tidy.csv") %>%
  #   distinct(CommonName, ScientificName) %>% 
  #   filter(CommonName %nin% a) %>% 
  #   arrange(ScientificName) %>% 
  #   write_csv("FishyFishy.csv")
  # 
  # fish_species <- readr::read_csv("Meta_Data/SpeciesComplete2.csv") %>% 
  #   filter(Classification == "Fish")
  # a <- unique(fish_species$CommonName)
  # b <- unique(Fish_Data$CommonName)
  # base::setdiff(
  #   b,
  #   a)
}

{ # RPC % Cover Table for Diversity  ----
  rpcs_Data <- readr::read_csv(
    glue("Raw_DB_Files_SAVE_HERE/KFM_RandomPointContact_RawData_1982-{Export_END_Year}.txt"), 
    col_types = cols(CountA = col_number(), CountB = col_number(), CountC = col_number(), CountD = col_number())) %>%
    dplyr::filter(IslandCode != "CL") %>%
    dplyr::left_join(siteInfo1) %>%
    tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
    dplyr::mutate(Date = as.Date(Date, format='%m/%d/%Y'), Survey_Type = "RPC") %>%
    tidyr::pivot_longer(cols = c(CountA, CountB, CountC, CountD), values_to = "Count") %>% 
    dplyr::filter(!is.na(Count), !is.na(CommonName)) %>% 
    dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                  CommonName, SurveyYear, Date, Quadrat_Number, Count, 
                  ReserveStatus, MeanDepth, Reference, Survey_Type) %>% 
    dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                    CommonName, SurveyYear, Date, ReserveStatus, MeanDepth, Reference, Survey_Type) %>%
    dplyr::summarise(
      Count_To_Reuse = Count,
      Area_Surveyed = 
        ifelse(SurveyYear == 1982, 5, 
               ifelse(SurveyYear == 1983, 4,
                      ifelse(SurveyYear == 1984, 5,
                             ifelse(SurveyYear > 1984 & SurveyYear <= 1995, 10, 6)))),
      Total_Count = sum(Count),
      Percent_Cover = 
        ifelse(SurveyYear == 1982, round((Total_Count / Area_Surveyed), 4), 
               ifelse(SurveyYear == 1983, round((Total_Count / Area_Surveyed), 4),
                      ifelse(SurveyYear == 1984, round((Total_Count / Area_Surveyed), 4),
                             ifelse(SurveyYear > 1984 & SurveyYear <= 1995, round((Total_Count / Area_Surveyed), 4), 
                                    round((Total_Count / Area_Surveyed), 4))))),
      SD = round(sd(Count), 4),
      SE = round(SD / sqrt(Area_Surveyed), 4)) %>% 
    dplyr::ungroup() %>%  
    dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
                  Date, Total_Count, Percent_Cover, SD, SE, Area_Surveyed, MeanDepth, ReserveStatus, Reference, Survey_Type) %>%
    dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
                    Total_Count, Percent_Cover, SD, SE, Area_Surveyed, .keep_all = TRUE)  %>% 
    dplyr::filter(ScientificName != "Macrocystis, Pterygophora, and Eisenia combined",
                  # ScientificName != "Macrocystis pyrifera",
                  # ScientificName != "Eisenia arborea",
                  # ScientificName != "Pterygophora californica",
                  # ScientificName != "Laminaria farlowii",
                  # ScientificName != "Sargassum horneri",
                  ScientificName != "Leucetta losangelensis",
                  ScientificName != "Hydrozoa",
                  # ScientificName != "Bare Substrate",
                  # ScientificName != "Rock",
                  # ScientificName != "Cobble",
                  # ScientificName != "Sand",
                  ScientificName != "Balanus",
                  ScientificName != "Sargassum muticum",
                  ScientificName != "Polymastia pachymastia",
                  ScientificName != "Spirobranchus spinosus") %>%
    dplyr::mutate(ScientificName = dplyr::case_when(
      CommonName == "encrusting coralline algae" ~ "encrusting coralline algae",
      CommonName == "articulated coralline algae" ~ "encrusting coralline algae",
      CommonName != "articulated coralline algae" |
        CommonName != "articulated coralline algae" ~ ScientificName))%>% 
    dplyr::select(SiteNumber, SiteCode, SiteName,  IslandCode, IslandName, ScientificName, SurveyYear, Date,
                  Total_Count, Percent_Cover, SD, SE, Area_Surveyed, MeanDepth, Survey_Type, ReserveStatus, Reference) %>%
    dplyr::filter(Reference == TRUE, SurveyYear > 2004, SiteCode != "KH") %>%
    dplyr::group_by(SiteCode, SiteName, IslandCode, IslandName, ScientificName, SurveyYear, ReserveStatus) %>%
    dplyr::summarise(Percent_Cover = base::sum(Percent_Cover)) %>%
    dplyr::ungroup() %>% 
    tidyr::pivot_wider(names_from = ScientificName, values_from = Percent_Cover, values_fill = 0) %>%
    readr::write_csv("Tidy_Data_Dont_Touch/rpcs_Percent_Cover_Wide.csv")
  
}

{ # Benthic Biomass Tables (Wide for models, Long for plots)   ----
  
  { # 1 m Density     ----
    oneM_Biomass_Data <- readr::read_csv(
      glue("Raw_DB_Files_SAVE_HERE/KFM_1mQuadrat_RawData_1982-{Export_END_Year}.txt"),   
      col_types = cols(CountA = col_number(), CountB = col_number())) %>%
      dplyr::filter(IslandCode != "CL",
                    ScientificName != "Lithopoma gibberosa" | SurveyYear > 2002) %>%
      dplyr::left_join(siteInfo1) %>%
      tidyr::pivot_longer(cols = c(CountA, CountB), values_to = "Count") %>% 
      dplyr::filter(!is.na(Count), !is.na(CommonName),
                    CommonName != "giant kelp, juvenile",
                    CommonName != "giant kelp stipes > 1m") %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                    CommonName, SurveyYear, QuadratNumber, Count, ReserveStatus, MeanDepth, Reference) %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName,  ScientificName,
                      SurveyYear, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = ifelse(SurveyYear %in% 1985:1994, n() * 2, n()),
                       Total_Count = base::sum(Count),
                       Mean_Density = base::round(Total_Count / Area_Surveyed, 4), 
                       SD = base::round(stats::sd(Count), 4),
                       SE = base::round(SD / base::sqrt(Area_Surveyed), 4),
                       Survey_Type = "One_Meter") %>% 
      dplyr::ungroup() %>%  
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, 
                    ScientificName,SurveyYear, 
                    Total_Count, Mean_Density, SD, SE, Area_Surveyed, 
                    Survey_Type, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, SurveyYear, 
                      Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE) 
    
    oneM_Biomass <- oneM_Biomass_Data %>% # separate because Macro Biomass below
      dplyr::filter(ScientificName %in% oneM_Biomass_Species, 
                    ScientificName != "Pisaster giganteus" | SurveyYear < 1996)
  }
  
  { # 5 m Density     ----
    
    fiveM_Biomass_Data <- readr::read_csv(
      glue("Raw_DB_Files_SAVE_HERE/KFM_5mQuadrat_RawData_1996-{Export_END_Year}.txt")) %>%
      dplyr::filter(IslandCode != "CL") %>%
      dplyr::left_join(siteInfo1) %>%
      dplyr::filter(!is.na(Count), !is.na(CommonName)) %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                    CommonName, SurveyYear, QuadratNumber, Count, ReserveStatus, MeanDepth, Reference) %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      SurveyYear, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = dplyr::n() * 5,
                       Total_Count = base::sum(Count),
                       Mean_Density = base::round(Total_Count / Area_Surveyed, 4), 
                       SD = base::round(stats::sd(Count), 4),
                       SE = base::round(SD / base::sqrt(Area_Surveyed), 4),
                       Survey_Type = "Five_Meter") %>% 
      dplyr::ungroup() %>%  
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, 
                    SurveyYear, Total_Count, Mean_Density, SD, SE, Area_Surveyed, MeanDepth, 
                    Survey_Type, ReserveStatus, Reference) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, 
                      ScientificName, SurveyYear, Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE)  
    
    fiveM_Biomass <- fiveM_Biomass_Data %>% # separate because Macro Biomass below
      dplyr::filter(ScientificName == "Pisaster giganteus",
                    ScientificName != "Pisaster giganteus" | SurveyYear < 2014) 
    
  }
  
  { # Bands Density     ----
    
    bands_Biomass_Data <- read_csv(
      glue("Raw_DB_Files_SAVE_HERE/KFM_BandTransect_RawData_1982-{Export_END_Year}.txt",),   
      col_types = cols(CountA = col_number(), CountB = col_number())) %>%
      dplyr::filter(IslandCode != "CL") %>%
      dplyr::left_join(siteInfo1) %>%
      tidyr::pivot_longer(cols = c(CountA, CountB), values_to = "Count") %>% 
      dplyr::filter(!is.na(Count), !is.na(CommonName)) %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                    CommonName, SurveyYear, TransectNumber, Count, ReserveStatus, MeanDepth, Reference)  %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      SurveyYear, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = ifelse(SurveyYear %in% 1983:1984, n() * 40, n() *60),
                       Total_Count = base::sum(Count),
                       Mean_Density = base::round(Total_Count / Area_Surveyed, 4), 
                       SD = base::round(stats::sd(Count), 4),
                       SE = base::round(SD/base::sqrt(Area_Surveyed), 4),
                       Survey_Type = "Bands") %>% 
      dplyr::ungroup() %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, SurveyYear,
                    Total_Count, Mean_Density, SD, SE, Area_Surveyed, MeanDepth, Survey_Type, ReserveStatus, Reference) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      SurveyYear, Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE) %>% 
      dplyr::filter(ScientificName %in% bands_Biomass_Species)
    
  }
  
  { # Macro Stipe Density  ----
    oneM_Macro <- oneM_Biomass_Data %>% 
      filter(ScientificName == "Macrocystis pyrifera",
             SurveyYear %in% 1984:1995)
    
    fiveM_Macro <- fiveM_Biomass_Data %>% 
      dplyr::filter(ScientificName == "Macrocystis pyrifera")
    
    Macro <- rbind(oneM_Macro, fiveM_Macro)
    
    stipedensity_FSC <- read_csv("Meta_Data/stipedensity_FSC_regressioncoefs.csv") %>%
      dplyr::filter(Month %in% season_months) %>%
      dplyr::summarise(slope = mean(slope))
    
    Kelp_Biomass <- read_csv(
      glue::glue("Raw_DB_Files_SAVE_HERE/KFM_Macrocystis_RawData_1984-{Export_END_Year}.txt"),
      col_types = cols(PermanentObserverNumber = col_double())) %>% 
      dplyr::filter(IslandCode != "CL") %>%
      dplyr::select(-SurveyDate, -Species, -PermanentObserverNumber, -Marker, -Diameter_cm) %>% 
      dplyr::full_join(Macro, by = c('SiteNumber', 'IslandCode', 'IslandName', 'SiteCode', 
                              'SiteName', 'ScientificName', 'SurveyYear')) %>% 
      dplyr::mutate(
        Stipe_Count = dplyr::case_when(
          is.na(Stipe_Count) & Mean_Density == 0 ~ 0,
          !is.na(Stipe_Count) ~ Stipe_Count),
        Stipe_Count = tidyr::replace_na(Stipe_Count, 0)) %>% 
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, Mean_Density,
                    ScientificName, SurveyYear, Stipe_Count, ReserveStatus,
                    MeanDepth, Reference, Survey_Type) %>%
      dplyr::filter(Reference == TRUE, SurveyYear > 2004, SiteCode != "KH") %>% 
      dplyr::group_by(IslandCode, IslandName, SiteCode, SiteName, 
                      ScientificName, SurveyYear, ReserveStatus) %>%
      dplyr::summarise(Stipe_Density = sum(Stipe_Count / n() * Mean_Density)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Biomass = (Stipe_Density * stipedensity_FSC$slope)*1000) %>% # converts from kg to g here
      dplyr::arrange(SiteName, SurveyYear)
    
  }
  
  { # Invertebrate Biomass Raw  ----
    Invert_Biomass <- readr::read_csv(
      glue("Raw_DB_Files_SAVE_HERE/KFM_InvertebrateBiomass_1985-{Export_END_Year}.txt")) %>% 
      dplyr::filter(IslandCode != "CL", !is.na(Invertebrate_Biomass),
                    ScientificName != "Lithopoma gibberosa" | SurveyYear > 2002) %>%
      tidyr::uncount(weights = NoOfInd) %>% 
      dplyr::group_by(SiteNumber, SurveyYear, CommonName) %>% 
      dplyr::mutate(Bio_Proportion = 1 / n()) %>% 
      dplyr::ungroup() %>%
      dplyr::left_join(siteInfo1) %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName,  
                    SurveyYear, Size_mm, Invertebrate_Biomass, Bio_Proportion, ReserveStatus, MeanDepth, Reference)
  }
  
  { # Benthic Biomass Long  ----
    Benthic_Biomass_Long <- base::rbind(oneM_Biomass, fiveM_Biomass, bands_Biomass_Data) %>% 
      dplyr::full_join(
        Invert_Biomass,
        by = c('SiteNumber', 'IslandCode', 'IslandName', 'SiteCode', 'SiteName','ScientificName',
               'SurveyYear', 'ReserveStatus', 'MeanDepth', 'Reference')) %>%
      dplyr::filter(Reference == TRUE, SurveyYear > 2004, SiteCode != "KH") %>%
      dplyr::group_by(SiteNumber, ScientificName, SurveyYear) %>% 
      dplyr::mutate(
        Mean_Density = dplyr::case_when(
          is.na(Mean_Density) & !is.na(Invertebrate_Biomass) ~ n()/2000,
          !is.na(Mean_Density) ~ Mean_Density),
        Invertebrate_Biomass = dplyr::case_when(
          is.na(Invertebrate_Biomass) & Mean_Density == 0 ~ 0,
          !is.na(Invertebrate_Biomass) ~ Invertebrate_Biomass),
        Bio_Proportion = dplyr::case_when(
          is.na(Bio_Proportion) & Mean_Density == 0 ~ 0,
          !is.na(Bio_Proportion) ~ Bio_Proportion),
        Biomass = base::sum(Bio_Proportion * Invertebrate_Biomass * Mean_Density)) %>% 
      dplyr::ungroup() %>%
      dplyr::distinct(SiteNumber, SurveyYear, ScientificName, .keep_all = TRUE)  %>% 
      dplyr::group_by(ScientificName, IslandName, SurveyYear) %>% 
      dplyr::mutate(Biomass = dplyr::case_when(
        is.na(Biomass) & Mean_Density > 0 ~ mean(Biomass, na.rm = TRUE),
        !is.na(Biomass) ~ Biomass),
        Date = base::as.Date(base::ISOdate(SurveyYear, 1, 1))) %>% 
      dplyr::ungroup()%>% 
      tidyr::complete(nesting(SiteNumber, SiteCode, SiteName, IslandName,
                              IslandCode, MeanDepth, ReserveStatus, Reference),
                      ScientificName, SurveyYear,
                      fill = list(Mean_Density = 0, Survey_Type = "Bands")) %>%
      dplyr::filter(ScientificName != "Tegula regina" | SurveyYear > 2005) %>% 
      dplyr::arrange(SiteName, SurveyYear) %>% 
      dplyr::group_by(SiteName, IslandName, ScientificName, SurveyYear, ReserveStatus) %>% 
      base::rbind(dplyr::select(Kelp_Biomass, -Stipe_Density))%>%
      dplyr::mutate(Mean_Biomass = round(mean(Biomass), 4)) %>%
      dplyr::ungroup() %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, 
                    SurveyYear, Date, Mean_Biomass, Mean_Density, ReserveStatus, MeanDepth, Reference, Survey_Type) %>% 
      dplyr::distinct(SiteName, IslandCode, ScientificName, SurveyYear, ReserveStatus, .keep_all = TRUE) %>% 
      dplyr::mutate(IslandName = gsub(" Island", "", IslandName)) %>%
      readr::write_csv("Tidy_Data_Dont_Touch/Benthic_Biomass_Long.csv")
  }
  
  { # Benthic Biomass Wide   ----
    Benthic_Biomass_Wide <- Benthic_Biomass_Long %>% 
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName, 
                    SurveyYear, Date, Mean_Biomass, ReserveStatus, Reference) %>% 
      dplyr::group_by(SiteCode, SiteName, IslandCode, IslandName, ScientificName, SurveyYear, ReserveStatus) %>%
      dplyr::summarise(Mean_Biomass = base::sum(Mean_Biomass)) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from = ScientificName, values_from = Mean_Biomass, values_fill = 0)  %>%
      dplyr::mutate(IslandName = gsub(" Island", "", IslandName)) %>%
      dplyr::left_join(annual_mean_oni, by = c("SurveyYear"))
    names(Benthic_Biomass_Wide) <- str_replace_all(names(Benthic_Biomass_Wide), c(" " = "_" , "," = "" ))
    readr::write_csv(Benthic_Biomass_Wide, "Tidy_Data_Dont_Touch/Benthic_Biomass_Wide.csv") 
  }
 
}

{ # Fish Biomass Tables (Wide for models, Long for plots)   ----
  
  { # RDFC RAW and NASTY  ----
    RDFC_Biomass_Data <- data.table::fread(
      glue("Raw_DB_Files_SAVE_HERE/KFM_RovingDiverFishCount_RawData_1982-{Export_END_Year}.txt")) %>%
      dplyr::mutate(CommonName = gsub('ñ', 'n', CommonName),
                    Abundance = gsub("c", "C", Abundance),
                    Abundance = gsub("f", "F", Abundance),
                    Abundance = gsub("s", "S", Abundance),
                    Abundance = gsub("m", "M", Abundance),
                    Abundance = gsub("^$", NA, Abundance),
                    Abundance = gsub("-", NA, Abundance)) %>%
      dplyr::filter(IslandCode != "CL", SurveyYear > 2006, ExperienceLevel == "E") %>% 
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::mutate(Date = base::as.Date(Date, format = '%m/%d/%Y')) %>% 
      dplyr::group_by(SurveyYear, SiteNumber) %>% 
      dplyr::filter(Date == base::max(Date)) %>% 
      dplyr::ungroup() %>% 
      dplyr::left_join(siteInfo1) %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName, CommonName, 
                    SurveyYear, Date, Score, Abundance, Count, ExperienceLevel, PermanentObserverNumber,
                    ReserveStatus, MeanDepth, Reference)
    
    RDFC_Biomass_Data2 <- RDFC_Biomass_Data  %>% 
      dplyr::filter(ScientificName != "Semicossyphus pulcher",
                    ScientificName != "Halichoeres semicinctus") %>% 
      dplyr::group_by(SiteNumber, SurveyYear, ScientificName, PermanentObserverNumber) %>% 
      dplyr::mutate(Count = sum(Count)) %>% 
      dplyr::ungroup() %>% 
      dplyr::distinct(SiteNumber, SurveyYear, ScientificName, PermanentObserverNumber, .keep_all = TRUE) %>% 
      dplyr::mutate(CommonName = gsub("juvenile", "all", CommonName),
                    CommonName = gsub("subadult", "all", CommonName),
                    CommonName = gsub("adult", "all", CommonName, fixed = TRUE))
    
    RDFC_Biomass_Data3 <- RDFC_Biomass_Data  %>% 
      dplyr::filter(ScientificName == "Semicossyphus pulcher" |
                      ScientificName == "Halichoeres semicinctus")
    
    RDFC_Biomass_Data4 <- base::rbind(RDFC_Biomass_Data2, RDFC_Biomass_Data3) %>%
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      CommonName, SurveyYear, Date, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Area_Surveyed = 2000,
                       Mean_Count = mean(Count),
                       Mean_Density = round(Mean_Count / Area_Surveyed, 4)) %>%
      dplyr::ungroup() %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, CommonName, ScientificName, SurveyYear,
                    Mean_Count, Mean_Density, Area_Surveyed, MeanDepth, Reference) %>%
      dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName,  CommonName, SurveyYear,
                      Mean_Count, Mean_Density, Area_Surveyed, .keep_all = TRUE) %>% 
      dplyr::mutate(CommonName = gsub(", all", "", CommonName))
    
    Fish_Biomass_Raw <- data.table::fread(
      glue("Raw_DB_Files_SAVE_HERE/KFM_FishBiomass_2007-{Export_END_Year}.txt")) %>%
      dplyr::mutate(CommonName = gsub('ñ', 'n', CommonName),
                    CommonName = gsub(", all", "", CommonName),
                    ScientificName = gsub(", all", "", ScientificName),
                    ScientificName = gsub(", male", "", ScientificName),
                    ScientificName = gsub(", female", "", ScientificName),
                    ScientificName = gsub(", juvenile", "", ScientificName),
                    Species_Count = as.double(Species_Count)) %>% 
      dplyr::filter(!is.na(Fish_Biomass)) %>% 
      dplyr::mutate(
        Fish_Biomass = dplyr::case_when(
          ScientificName == "Semicossyphus pulcher" ~ Fish_Biomass * 1000,
          ScientificName == "Scorpaenichthys marmoratus" ~ Fish_Biomass * 1000,
          ScientificName == "Sebastes mystinus" ~ Fish_Biomass * 1000,
          ScientificName != "Scorpaenichthys marmoratus" &
            ScientificName != "Scorpaenichthys marmoratus" &
            ScientificName != "Sebastes mystinus"~ Fish_Biomass))
    
    RDFC_Biomass_Data4 <- RDFC_Biomass_Data4 %>% 
      dplyr::filter(ScientificName %in% unique(Fish_Biomass_Raw$ScientificName))
    
    Fish_Trophic_Levels <- readr::read_csv("Meta_Data/KFM_Fish_Trophic_Levels.csv") 
  }
  
  { # Fish Biomass Long ----
    
    #Backfilling Fish Biomass for 05/06 from regressing 07-19 count v biomass data
    
    Fish_Biomass_Long <- Fish_Biomass_Raw %>%
      dplyr::full_join(RDFC_Biomass_Data4, by = c(
        'SiteNumber', 'IslandCode', 'IslandName', 'SiteCode', 'SiteName', 'CommonName', 'ScientificName', 'SurveyYear')) %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, ScientificName, CommonName,
                    Species_Count, Total_Length_cm, Fish_Biomass, Mean_Count, Mean_Density) %>%
      dplyr::mutate(
        Mean_Count = dplyr::case_when(
          is.na(Mean_Count) & !is.na(Species_Count) ~ Species_Count,
          Mean_Count == 0 & Species_Count > 0 ~ Species_Count,
          is.na(Mean_Count) & is.na(Species_Count) ~ 0,
          !is.na(Mean_Count) ~ Mean_Count),
        Species_Count = dplyr::case_when(
          !is.na(Mean_Count) & is.na(Species_Count) ~ round(Mean_Count, 0),
          is.na(Mean_Count) & is.na(Species_Count) ~ 0,
          !is.na(Species_Count) ~ Species_Count)) %>%
      dplyr::group_by(CommonName, IslandName) %>%
      dplyr::mutate(Mean_Isl_Bio = base::mean(Fish_Biomass, na.rm = TRUE)) %>%
      dplyr::ungroup()%>%
      dplyr::mutate(
        Fish_Biomass  = dplyr::case_when(
          Mean_Count == 0 & is.na(Fish_Biomass) ~ 0,
          Mean_Count > 0 & is.na(Fish_Biomass) ~ Mean_Isl_Bio,
          !is.na(Fish_Biomass) ~ Fish_Biomass)) %>%
      dplyr::filter(!is.na(Fish_Biomass))  %>%
      tidyr::uncount(weights = Species_Count) %>%
      dplyr::group_by(SiteNumber, SurveyYear, CommonName) %>%
      dplyr::mutate(Biomass = base::sum(1/n() * Fish_Biomass * Mean_Count)) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(SiteNumber, SurveyYear, CommonName, Biomass, .keep_all = TRUE) %>%
      dplyr::left_join(siteInfo1) %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear, ScientificName,
                    CommonName, Biomass, Mean_Count, Mean_Density, MeanDepth, Reference, ReserveStatus) %>%
      dplyr::left_join(Fish_Trophic_Levels)  %>%
      dplyr::filter(Reference == TRUE, SurveyYear > 2004, SiteCode != "KH") %>%
      tidyr::complete(nesting(SiteNumber, SiteCode, SiteName, IslandName,
                              IslandCode, MeanDepth, ReserveStatus, Reference),
                      nesting(ScientificName, CommonName, Broad_Trophic_Level, Targeted), SurveyYear,
                      fill = list(Biomass = 0, Mean_Density = 0)) %>%
      dplyr::group_by(SiteCode, SiteName, IslandName, ScientificName, CommonName, SurveyYear) %>%
      dplyr::mutate(Mean_Biomass = round(mean(Biomass), 4)) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(SiteCode, SiteName, IslandCode, ScientificName, CommonName,
                      SurveyYear, ReserveStatus, .keep_all = TRUE) %>%
      dplyr::select(SiteCode, SiteName, IslandCode, IslandName, CommonName, ScientificName, SurveyYear,
                    Mean_Biomass, ReserveStatus,Reference, Broad_Trophic_Level, Targeted) %>%
      dplyr::mutate(Date = base::as.Date(base::ISOdate(SurveyYear, 1, 1)),
                    IslandName = gsub(" Island", "", IslandName)) 
    
    #combine count and biomass data for 07-19 in long form
    
    Fish_Biomass_and_Counts_Long <- data.table::fread(
      glue("Raw_DB_Files_SAVE_HERE/KFM_RovingDiverFishCount_RawData_1982-{Export_END_Year}.txt")) %>%
      dplyr::mutate(CommonName = gsub('ñ', 'n', CommonName),
                    Abundance = gsub("c", "C", Abundance),
                    Abundance = gsub("f", "F", Abundance),
                    Abundance = gsub("s", "S", Abundance),
                    Abundance = gsub("m", "M", Abundance),
                    Abundance = gsub("^$", NA, Abundance),
                    Abundance = gsub("-", NA, Abundance),
                    CommonName = case_when(
                      CommonName == "rock wrasse, juvenile" ~ "rock wrasse, juvenile",
                      CommonName == "rock wrasse, male" ~ "rock wrasse, male",
                      CommonName == "rock wrasse, female" ~ "rock wrasse, female",
                      CommonName == "California sheephead, juvenile" ~ "California sheephead, juvenile",
                      CommonName == "California sheephead, male" ~ "California sheephead, male",
                      CommonName == "California sheephead, female" ~ "California sheephead, female",
                      ScientificName != "Semicossyphus pulcher" |
                        ScientificName != "Halichoeres semicinctus" ~ gsub(", juvenile", "", CommonName)),
                    CommonName = gsub(", all", "", CommonName),
                    CommonName = gsub(", adult", "", CommonName),
                    CommonName = gsub(", subadult", "", CommonName)) %>%
      dplyr::filter(IslandCode != "CL") %>%
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::mutate(Date = base::as.Date(Date, format = '%m/%d/%Y'),
                    Count = as.double(Count)) %>%
      dplyr::group_by(SiteCode, SurveyYear) %>%
      dplyr::filter(Date == base::max(Date), SurveyYear > 1996, ExperienceLevel == "E") %>%
      dplyr::ungroup() %>%
      dplyr::left_join(siteInfo1) %>%
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName, CommonName,
                    SurveyYear, Count, ReserveStatus, Reference) %>%
      dplyr::filter(Reference == TRUE, SurveyYear > 2004, SiteCode != "KH",
                    ScientificName %in% unique(Fish_Biomass_Long$ScientificName)) %>%
      dplyr::mutate(Count= replace_na(Count, 0)) %>%
      dplyr::group_by(IslandCode, IslandName, SiteCode, SiteName,
                      ScientificName, CommonName, SurveyYear, ReserveStatus) %>%
      dplyr::summarise(Count = sum(Count)) %>%
      dplyr::ungroup() %>%
      tidyr::complete(nesting(IslandCode, IslandName, SiteCode, SiteName, ReserveStatus),
                      nesting(ScientificName, CommonName), SurveyYear,
                      fill = list(Count = 0)) %>%
      dplyr::left_join(dplyr::select(Fish_Biomass_Long, SiteCode, SurveyYear, CommonName, Mean_Biomass))
    
    bc_07_19 <- Fish_Biomass_and_Counts_Long %>%
      dplyr::filter(SurveyYear %in% 2007:2019)
    
    # for each species and site in nested loops, 
    # biomass ~ count, extract values by assignment and compile using rbind
    
    Biomass_SiteSpecific_Regression_Results <- tibble(
      IslandCode = character(), IslandName = character(), SiteCode = character(), 
      SiteName = character(), ReserveStatus = character(), ScientificName = character(), 
      CommonName = character(), coef = double(), intercept = double(), adj_r_squared = double(), p_val = double())
    
    for(s in unique(bc_07_19$SiteCode)){
      for(n in unique(bc_07_19$CommonName)){
        
        df <- bc_07_19 %>%
          dplyr::filter(CommonName == n, SiteCode == s)
        regress.sum <- summary(lm(Mean_Biomass ~ 0 + Count, data = df))
        
        holder_table <- df[1, 1:7]
        holder_table <- holder_table %>%
          dplyr::mutate(intercept = as.double(regress.sum$coefficients[1]),
                        coef = as.double(regress.sum$coefficients[2]),
                        p_val = as.double(regress.sum$coefficients[8]),
                        adj_r_squared = as.double(regress.sum$adj.r.squared))
        
        Biomass_SiteSpecific_Regression_Results <- rbind(Biomass_SiteSpecific_Regression_Results, holder_table)
        
      }
    }
    
    #left join coefficients into 05-06 counts by site and commonname and mutate counts into biomass for 05/06
    
    Fish_Biomass_0506_ests <- Fish_Biomass_and_Counts_Long %>%
      dplyr::filter(SurveyYear %in% 2005:2006) %>%
      dplyr::left_join(dplyr::select(
        Biomass_SiteSpecific_Regression_Results,
        SiteName, CommonName, intercept, coef, p_val, adj_r_squared),
        by = c("SiteName", "CommonName")) %>%
      dplyr::mutate(Mean_Biomass = (Count*coef + intercept))
    
    FB_0506_cleaned <- Fish_Biomass_0506_ests %>%
      dplyr::select(-Count, -intercept, -coef, -p_val, -adj_r_squared) %>%
      dplyr::left_join(dplyr::select(Fish_Biomass_Long[Fish_Biomass_Long$SurveyYear == 2007,], 
                                     SiteName, CommonName, Reference, Broad_Trophic_Level, Targeted, Date),
                       by = c("SiteName", "CommonName"))
    
    #Combine new 05/06 estimates into the rest of the data and rearrange on island, site, species, year
    Fish_Biomass_Long_New_0506_Ests <- Fish_Biomass_Long %>%
      dplyr::add_row(FB_0506_cleaned) %>%
      dplyr::mutate(Mean_Biomass = replace_na(Mean_Biomass, 0)) %>%
      dplyr::arrange(IslandCode, SiteCode, CommonName, SurveyYear) %>% 
      dplyr::mutate(IslandName = gsub(" Island", "", IslandName)) %>%
    readr::write_csv("Tidy_Data_Dont_Touch/Fish_Biomass_Long.csv")
    
  }
  
  { # Fish Biomass Wide   -----
    Fish_Biomass_Wide <- Fish_Biomass_Long_New_0506_Ests %>% 
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName, 
                    SurveyYear, Date, Mean_Biomass, ReserveStatus, Reference) %>% 
      dplyr::filter(Reference == TRUE, SurveyYear > 2004, SiteCode != "KH") %>%
      dplyr::group_by(SiteCode, SiteName, IslandCode, IslandName, ScientificName, SurveyYear, ReserveStatus) %>%
      dplyr::summarise(Mean_Biomass = base::sum(Mean_Biomass)) %>%
      dplyr::ungroup() %>% 
      tidyr::pivot_wider(names_from = ScientificName, values_from = Mean_Biomass, values_fill = 0) %>%
      dplyr::left_join(annual_mean_oni, by = c("SurveyYear")) %>% 
      dplyr::rename_with(~ base::gsub(",", "", .)) %>% 
      dplyr::rename_with(~ base::gsub(" ", "_", .)) %>%
      readr::write_csv("Tidy_Data_Dont_Touch/Fish_Biomass_Wide.csv") 
  }
  
}

{ # Mixed Data (% Cover, Count, Biomass) for Random Forest Model  ----
 
  # Run Both Count Blocks first
  # Run Both Biomass Blocks First
  
  Counts <- base::rbind(oneM_Count_Data, fiveM_Count_Data, bands_Count_Data) %>%
    dplyr::mutate(Mean_Density = Mean_Density * 2000) 
  Biomasses <- dplyr::full_join(Benthic_Biomass_Wide, Fish_Biomass_Wide) %>% 
    dplyr::select(-Mean_ONI_ANOM)  
  remove <- c('SiteCode', 'SiteName', 'IslandCode', 'IslandName', 
              'SurveyYear', 'ReserveStatus', 'Date', 'Mean_ONI_ANOM')
  Biomass_Species <- unique(factor(c(names(Benthic_Biomass_Wide), names(Fish_Biomass_Wide))))
  Biomass_Species <- droplevels(Biomass_Species[! Biomass_Species %in% remove])
  
  All_Mixed_Data_Wide <- readr::read_csv(
    glue("Raw_DB_Files_SAVE_HERE/KFM_RandomPointContact_RawData_1982-{Export_END_Year}.txt"), 
    col_types = cols(CountA = col_number(), CountB = col_number(), CountC = col_number(), CountD = col_number())) %>%
    dplyr::filter(IslandCode != "CL") %>%
    dplyr::left_join(siteInfo1) %>%
    tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
    dplyr::mutate(Date = as.Date(Date, format='%m/%d/%Y'), Survey_Type = "RPC") %>%
    tidyr::pivot_longer(cols = c(CountA, CountB, CountC, CountD), values_to = "Count") %>% 
    dplyr::filter(!is.na(Count), !is.na(CommonName)) %>% 
    dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                  CommonName, SurveyYear, Date, Quadrat_Number, Count, 
                  ReserveStatus, MeanDepth, Reference, Survey_Type) %>% 
    dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName,
                    CommonName, SurveyYear, Date, ReserveStatus, MeanDepth, Reference, Survey_Type) %>%
    dplyr::summarise(
      Count_To_Reuse = Count,
      Area_Surveyed = 
        ifelse(SurveyYear == 1982, 5, 
               ifelse(SurveyYear == 1983, 4,
                      ifelse(SurveyYear == 1984, 5,
                             ifelse(SurveyYear > 1984 & SurveyYear <= 1995, 10, 6)))),
      Total_Count = sum(Count),
      Mean_Density = 
        ifelse(SurveyYear == 1982, round((Total_Count / Area_Surveyed), 4), 
               ifelse(SurveyYear == 1983, round((Total_Count / Area_Surveyed), 4),
                      ifelse(SurveyYear == 1984, round((Total_Count / Area_Surveyed), 4),
                             ifelse(SurveyYear > 1984 & SurveyYear <= 1995, round((Total_Count / Area_Surveyed), 4), 
                                    round((Total_Count / Area_Surveyed), 4))))),
      SD = round(sd(Count), 4),
      SE = round(SD / sqrt(Area_Surveyed), 4)) %>% 
    dplyr::ungroup() %>%  
    dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
                  Date, Total_Count, Mean_Density, SD, SE, Area_Surveyed, MeanDepth, ReserveStatus, Reference, Survey_Type) %>%
    dplyr::distinct(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, Species, ScientificName, CommonName, SurveyYear, 
                    Total_Count, Mean_Density, SD, SE, Area_Surveyed, .keep_all = TRUE)  %>% 
    dplyr::filter(ScientificName != "Macrocystis, Pterygophora, and Eisenia combined",
                  ScientificName != "Macrocystis pyrifera",
                  ScientificName != "Eisenia arborea",
                  ScientificName != "Pterygophora californica",
                  ScientificName != "Laminaria farlowii",
                  ScientificName != "Sargassum horneri",
                  ScientificName != "Leucetta losangelensis",
                  ScientificName != "Hydrozoa",
                  ScientificName != "Bare Substrate",
                  ScientificName != "Rock",
                  ScientificName != "Cobble",
                  ScientificName != "Sand",
                  ScientificName != "Balanus",
                  ScientificName != "Sargassum muticum",
                  ScientificName != "Polymastia pachymastia",
                  ScientificName != "Spirobranchus spinosus") %>%
    dplyr::mutate(ScientificName = dplyr::case_when(
      CommonName == "encrusting coralline algae" ~ "encrusting coralline algae",
      CommonName == "articulated coralline algae" ~ "articulated coralline algae",
      CommonName != "articulated coralline algae" |
        CommonName != "encrusting coralline algae" ~ ScientificName)) %>% 
    dplyr::select(SiteNumber, SiteCode, SiteName,  IslandCode, IslandName, ScientificName, SurveyYear, Date,
                  Total_Count, Mean_Density, SD, SE, Area_Surveyed, MeanDepth, Survey_Type, ReserveStatus, Reference) %>%
    dplyr::arrange(SiteNumber, SurveyYear, ScientificName) %>%
    dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                  SurveyYear, Mean_Density, ReserveStatus, Reference) %>% 
    base::rbind(oneM_Count_Data, fiveM_Count_Data, bands_Count_Data)   %>%
    base::rbind(Counts) %>%
    dplyr::filter(Reference == TRUE, SurveyYear > 2004, SiteCode != "KH") %>% 
    dplyr::group_by(IslandCode, IslandName, SiteCode, SiteName, ScientificName, SurveyYear, ReserveStatus) %>%
    dplyr::summarise(Mean_Density = base::sum(Mean_Density)) %>%
    dplyr::ungroup() %>% 
    tidyr::pivot_wider(names_from = ScientificName, values_from = Mean_Density, values_fill = 0) %>%
    dplyr::full_join(dplyr::select(Fish_Counts_Wide, -'Alloclinus holderi', 
                                   -'Coryphopterus nicholsi', -'Lythrypnus dalli',  -'Sebastes'), 
                     by = c('IslandCode', 'IslandName', 'SiteCode',
                            'SiteName','SurveyYear', 'ReserveStatus')) %>%
    dplyr::mutate(IslandName = gsub(" Island", "", IslandName),
                  IslandName = factor(IslandName, levels = MPA_Levels)) %>%   
    dplyr::rename_with(~ base::gsub(",", "", .)) %>% 
    dplyr::rename_with(~ base::gsub(" ", "_", .)) %>%
    dplyr::select(-tidyselect::all_of(Biomass_Species)) %>% 
    dplyr::full_join(Biomasses) %>% 
    readr::write_csv("Tidy_Data_Dont_Touch/All_Mixed_Data_Wide.csv") 
  
}

{ # Temperature RAW to Tidy   ----
  # temp_Raw <- readr::read_csv( # have not yet used but could...
  #   glue("Raw_DB_Files_SAVE_HERE/Temperature_RawData_1994-{Export_END_Year}.txt")) %>%
  #   dplyr::filter(IslandCode != "CL", Site_Number < 38, !base::is.na(Temp_C)) %>% 
  #   dplyr::select(-Date, -Time) %>%
  #   tidyr::separate(DateTime, c('Date','Time'),' ') %>%
  #   dplyr::group_by(Date, Site_Number) %>% 
  #   dplyr::mutate(Date = base::as.Date(Date, format = '%m/%d/%Y'),
  #                 Month = lubridate::month(Date, label = TRUE),
  #                 Temp_Daily_Min = base::min(Temp_C),
  #                 Temp_Daily_Max = base::max(Temp_C),
  #                 Temp_Daily_Mean = base::mean(Temp_C)) %>%
  #   dplyr::ungroup() %>% 
  #   dplyr::group_by(Site_Number, Year, Month) %>% 
  #   dplyr::mutate(Include = ifelse(is.even(match(Month, month.abb)) & n() < 24, FALSE, 
  #                                  ifelse(is.odd(match(Month, month.abb)) & n() < 25, FALSE, TRUE)))%>% 
  #   dplyr::filter(Include == TRUE) %>%
  #   dplyr::mutate(Temp_Monthly_Mean = base::mean(Temp_C)) %>% 
  #   dplyr::ungroup() %>% 
  #   dplyr::distinct(Site_Number, Date, .keep_all = TRUE) %>% 
  #   dplyr::left_join(siteInfo1) %>%
  #   dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName,
  #                 Year, Month, Date, Temp_Daily_Mean, Temp_Daily_Min, Temp_Daily_Max, Temp_Monthly_Mean, MeanDepth) %>% 
  #   readr::write_csv("Tidy_Data_Dont_Touch/Temp_Raw_Tidy.csv")
  
}
