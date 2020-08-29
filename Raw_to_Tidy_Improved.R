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
      readr::write_csv("Tidy_Data_Dont_Touch2/SST_Anomaly_Index.csv")
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
      readr::write_csv("Tidy_Data_Dont_Touch2/Benthic_Counts.csv") 
  }

}

{ # Benthic Density Table for density plots  ----
 
    Benthic_Densities_Wide <- base::rbind(oneM_Count_Data, fiveM_Count_Data, bands_Count_Data) %>% 
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName, 
                    SurveyYear, Mean_Density, ReserveStatus, Reference) %>%
      dplyr::filter(Reference == TRUE, SurveyYear > 2004, SiteCode != "KH") %>% 
      tidyr::pivot_wider(names_from = ScientificName, values_from = Mean_Density, values_fill = 0) %>% 
      dplyr::left_join(annual_mean_oni, by = c("SurveyYear")) %>%
      dplyr::rename_with(~ base::gsub(" ", "_", .)) %>%
      readr::write_csv("Tidy_Data_Dont_Touch2/Benthic_Densities.csv") 
 
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
    readr::write_csv("Tidy_Data_Dont_Touch2/Fish_Counts.csv") 

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
                  
                  ScientificName != "Leucetta losangelensis",
                  ScientificName != "Hydrozoa",
                  ScientificName != "Balanus",
                  ScientificName != "Sargassum muticum",
                  ScientificName != "Polymastia pachymastia",
                  ScientificName != "Spirobranchus spinosus") %>%
    dplyr::mutate(ScientificName = dplyr::case_when(
      CommonName == "encrusting coralline algae" ~ "encrusting coralline algae",
      CommonName == "articulated coralline algae" ~ "articulated coralline algae",
      TRUE ~ ScientificName))%>% 
    dplyr::select(SiteNumber, SiteCode, SiteName,  IslandCode, IslandName, ScientificName, SurveyYear, Date,
                  Total_Count, Percent_Cover, SD, SE, Area_Surveyed, MeanDepth, Survey_Type, ReserveStatus, Reference) %>%
    dplyr::filter(Reference == TRUE, SurveyYear > 2004, SiteCode != "KH") %>%
    dplyr::group_by(SiteCode, SiteName, IslandCode, IslandName, ScientificName, SurveyYear, ReserveStatus) %>%
    dplyr::summarise(Percent_Cover = base::sum(Percent_Cover)) %>%
    dplyr::ungroup() %>% 
    tidyr::pivot_wider(names_from = ScientificName, values_from = Percent_Cover, values_fill = 0) %>%
    readr::write_csv("Tidy_Data_Dont_Touch2/rpcs_Percent_Cover_Wide.csv")
  
  
  # dplyr::filter(ScientificName %in% c("Rock", "Cobble", "Sand") %>% 
  # readr::write_csv("Meta_Data/RPC_Substrate.csv")
}

{ # Benthic Biomass Tables (Wide for models, Long for plots)   ----
  
  { # Benthic Biomass Conversion Tables  ----
    
    { # Correct Equations   ----
      
      Benthic_Biomass_Coversions <- readr::read_csv(
        "Meta_Data/Benthic_Biomass_Equations.txt") %>% 
        dplyr::mutate(Source = "Reed 2016") %>% 
        tibble::add_row(ScientificName = "Megastraea undosa",
                        Independent_variable = "shell length",
                        Range = NA, a = .00065, b = 2.823,
                        r2 = NA, p = NA, N = NA, RMSE = NA,
                        Smearing_estimate = NA,
                        Source = "See L. Gibberosa") %>% 
        tibble::add_row(ScientificName = "Tegula regina",
                        Independent_variable = "shell length",
                        Range = NA, a = .00065, b = 2.823,
                        r2 = NA, p = NA, N = NA, RMSE = NA,
                        Smearing_estimate = NA,
                        Source = "See L. Gibberosa") %>%  
        dplyr::left_join(dplyr::select(Species_Info, ScientificName, CommonName)) %>% 
        tibble::add_row(ScientificName = "Macrocystis pyrifera",
                        CommonName = "giant kelp",
                        Independent_variable = "stipe density",
                        Range = NA, a = NA, b = .008477333,
                        r2 = NA, p = NA, N = NA, RMSE = NA,
                        Smearing_estimate = NA,
                        Source = "Rassweiler 2018") %>% 
        dplyr::mutate(
          LW_Equation = case_when(
            Independent_variable == "percent cover" ~ paste("W(g)=", "PC*", b, sep = ""),
            Independent_variable == "stipe density" ~ paste("W(g)=", "SD*", b, "*1000", sep = ""),
            Independent_variable != "body diameter" ~ paste("W(g)=", a, "*L^", b, sep = ""),
            TRUE ~ paste("W(g)=", a, "*(L*2)^", b, sep = ""))) %>%  
        readr::write_csv("Meta_Data/Benthic_Biomass_Equations.csv") 
    }
    
    { # Correct Equations with LaTex formatting  ----
      Benthic_Biomass_Coversions_Latex <- readr::read_csv(
        "Meta_Data/Benthic_Biomass_Equations.txt") %>% 
        dplyr::mutate(Source = "Reed 2016") %>% 
        tibble::add_row(ScientificName = "Megastraea undosa",
                        Independent_variable = "shell length",
                        Range = NA, a = .00065, b = 2.823,
                        r2 = NA, p = NA, N = NA, RMSE = NA,
                        Smearing_estimate = NA,
                        Source = "See L. Gibberosa") %>% 
        tibble::add_row(ScientificName = "Tegula regina",
                        Independent_variable = "shell length",
                        Range = NA, a = .00065, b = 2.823,
                        r2 = NA, p = NA, N = NA, RMSE = NA,
                        Smearing_estimate = NA,
                        Source = "See L. Gibberosa") %>%  
        dplyr::left_join(dplyr::select(Species_Info, ScientificName, CommonName)) %>% 
        tibble::add_row(ScientificName = "Macrocystis pyrifera",
                        CommonName = "giant kelp",
                        Independent_variable = "stipe density",
                        Range = NA, a = NA, b = .008477333,
                        r2 = NA, p = NA, N = NA, RMSE = NA,
                        Smearing_estimate = NA,
                        Source = "Rassweiler 2018") %>%
        dplyr::mutate(
          LW_Equation = case_when(
            Independent_variable == "percent cover" ~ paste("$W_{g}=", "C*", b, "$", sep = ""),
            Independent_variable == "stipe density" ~ paste("$W_{g}=", "SD*", b, "*1000", "$", sep = ""),
            Independent_variable != "body diameter" ~ paste("$W_{g}=", a, "*L^", b, "$", sep = ""),
            TRUE ~ paste("$W_{g}=", a, "*(L*2)^", b, "$", sep = "")),
          ScientificName = base::gsub(" ", "\\\\text{ }", ScientificName),
          ScientificName = paste("$", ScientificName, "$", sep = ""),
          Range = paste("$", Range, "$", sep = ""),
          a = paste("$", a, "$", sep = ""),
          b = paste("$", b, "$", sep = ""),
          r2 = paste("$", r2, "$", sep = ""),
          p = paste("$", p, "$", sep = ""),
          N = paste("$", N, "$", sep = ""),
          RMSE = paste("$", RMSE, "$", sep = ""),
          Smearing_estimate = paste("$", Smearing_estimate, "$", sep = "")) %>%  
        dplyr::select(ScientificName, CommonName, Independent_variable, Range, a, b, LW_Equation,
                      r2, p, N, RMSE, Smearing_estimate, Source) %>% 
        readr::write_csv("Meta_Data/Benthic_Biomass_Equations_Latex.csv") 
      
    }
    
  }
  
  { # Gorgonian Biomass  ----
    
    bands_Gorgo_Data <- read_csv(
      glue("Raw_DB_Files_SAVE_HERE/KFM_BandTransect_RawData_1982-{Export_END_Year}.txt",),   
      col_types = cols(CountA = col_number(), CountB = col_number())) %>%
      dplyr::left_join(siteInfo1) %>%
      dplyr::filter(ScientificName %in% c("Lophogorgia chilensis", "Muricea californica", "Muricea fruticosa"),
                    Reference == TRUE, SiteCode != "KH", SurveyYear > 2004) %>%
      tidyr::pivot_longer(cols = c(CountA, CountB), values_to = "Count") %>% 
      dplyr::group_by(IslandCode, IslandName, SiteCode, SiteName, ScientificName, 
                      CommonName, SurveyYear, ReserveStatus, Reference) %>%
      dplyr::summarise(Count_To_Reuse = Count,
                       Area_Surveyed = ifelse(SurveyYear %in% 1983:1984, n() * 40, n() * 60),
                       Total_Count = base::sum(Count),
                       Mean_Density = base::round(Total_Count / Area_Surveyed, 4)) %>% 
      dplyr::ungroup() %>% 
      dplyr::distinct(IslandCode, IslandName, SiteCode, SiteName, ScientificName, CommonName,
                      SurveyYear, Mean_Density, ReserveStatus, Reference)
    
    Gorgo <- readr::read_csv(
      glue("Raw_DB_Files_SAVE_HERE/KFM_Gorgonians_RawData_1984-2014_ NHSF_ 2015-{Export_END_Year}.txt")) %>% 
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::mutate(Date = base::as.Date(Date, format = '%m/%d/%Y')) %>% 
      dplyr::left_join(siteInfo1) %>%
      tidyr::uncount(weights = Count) %>% 
      dplyr::mutate(
        Biomass = case_when(
          ScientificName ==  "Lophogorgia chilensis" ~ (.018 * 10 ^ 1.529) * Width_cm ^ 1.529,
          ScientificName ==  "Muricea californica" ~ (.002 * 10 ^ 1.529) * Width_cm ^ 2.001,
          ScientificName ==  "Muricea fruticosa" ~ (.002 * 10 ^ 1.529) * Width_cm ^ 2.001)) %>% 
      dplyr::filter(Reference == TRUE, SiteCode != "KH", SurveyYear > 2004) %>% 
      dplyr::full_join(bands_Gorgo_Data) %>%
      dplyr::group_by(SiteCode, ScientificName, SurveyYear) %>% 
      dplyr::mutate(
        Biomass = case_when(
         is.na(Biomass) & Mean_Density == 0 ~ 0,
         TRUE ~ Biomass),
        Mean_Density = case_when(
          Biomass > 0 & Mean_Density == 0 ~ n()/2000,
          TRUE ~ Mean_Density)) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName, CommonName,
                    SurveyYear, Biomass, Mean_Density, ReserveStatus, Reference)
    
    Gorgo_Regression_Tidy <- Gorgo %>%
      group_by(CommonName, IslandName) %>%
      do(broom::tidy(lm(Biomass ~ 0 + Mean_Density, ., na.action = na.exclude)))
    
    Gorgo_Regression <- Gorgo %>%
      group_by(CommonName, IslandName) %>%
      do(broom::glance(lm(Biomass ~ 0 + Mean_Density, ., na.action = na.exclude)))  %>% 
      dplyr::select(-statistic, -p.value) %>% 
      dplyr::full_join(Gorgo_Regression_Tidy)
    
    Gorgo_Biomass_Long <- Gorgo %>% 
      left_join(Gorgo_Regression) %>% 
      dplyr::mutate(
        Biomass = dplyr::case_when(
          is.na(Biomass) ~ Mean_Density * estimate,
          TRUE ~ Biomass)) %>% 
      dplyr::select(IslandCode, SiteCode,  IslandName, SiteName, SurveyYear, 
                    ScientificName, CommonName, Biomass, Mean_Density, 
                    ReserveStatus, Reference) %>% 
      dplyr::group_by(IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      CommonName, SurveyYear, ReserveStatus, Reference) %>%
      dplyr::summarise(Mean_Biomass = sum(1/n() * Biomass * Mean_Density),
                       Mean_Density = Mean_Density) %>% 
      dplyr::distinct(IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      CommonName, SurveyYear, Mean_Biomass, Mean_Density, ReserveStatus, Reference)
  
  }
  
  { # 1 m Density     ----
    oneM_Biomass_Data <- readr::read_csv(
      glue("Raw_DB_Files_SAVE_HERE/KFM_1mQuadrat_RawData_1982-{Export_END_Year}.txt"),   
      col_types = cols(CountA = col_number(), CountB = col_number())) %>%
      dplyr::filter(IslandCode != "CL",
                    CommonName != "giant kelp, juvenile",
                    CommonName != "giant kelp stipes > 1m",
                    CommonName != "giant kelp, all",
                    ScientificName != "Lithopoma gibberosa" | SurveyYear > 2002) %>%
      dplyr::left_join(siteInfo1) %>%
      tidyr::pivot_longer(cols = c(CountA, CountB), values_to = "Count") %>% 
      dplyr::filter(!is.na(Count), !is.na(CommonName)) %>% 
      dplyr::mutate(CommonName = factor(CommonName),
                    CommonName = forcats::fct_collapse(
                      CommonName, "giant kelp" = c("giant kelp, adult (>1m)", "giant kelp, juvenile (<1m)"))) %>% 
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName,  ScientificName,
                      CommonName, SurveyYear, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Area_Surveyed = ifelse(SurveyYear %in% 1985:1994, n() * 2, n()),
                       Total_Count = base::sum(Count),
                       Mean_Density = base::round(Total_Count / Area_Surveyed, 4)) %>% 
      dplyr::ungroup() %>%  
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                    CommonName, SurveyYear, Mean_Density, ReserveStatus, Reference) %>%
      dplyr::distinct(SiteCode, ScientificName, SurveyYear, Mean_Density, .keep_all = TRUE) 
    
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
      dplyr::mutate(CommonName = factor(CommonName),
                    CommonName = forcats::fct_collapse(
                      CommonName, "giant kelp" = 
                        c("giant kelp, adult (>1m and haptera above the primary dichotomy)", 
                          "giant kelp, subadult (>1m and no haptera above the primary dichotomy)"))) %>% 
      dplyr::group_by(IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      CommonName, SurveyYear, ReserveStatus, Reference) %>%
      dplyr::summarise(Mean_Density = base::round(base::sum(Count) / 200, 4)) %>% 
      dplyr::ungroup() %>%  
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName, 
                    CommonName, SurveyYear, Mean_Density, ReserveStatus, Reference) %>%
      dplyr::distinct(SiteCode, ScientificName, SurveyYear, Mean_Density, .keep_all = TRUE)  
    
    fiveM_Biomass <- fiveM_Biomass_Data %>% # separate because Macro Biomass below
      dplyr::filter(ScientificName == "Pisaster giganteus",
                    ScientificName != "Pisaster giganteus" | SurveyYear < 2014) 
    
  }
  
  { # Bands Density     ----
    
    bands_Biomass_Data <- read_csv(
      glue("Raw_DB_Files_SAVE_HERE/KFM_BandTransect_RawData_1982-{Export_END_Year}.txt",),   
      col_types = cols(CountA = col_number(), CountB = col_number())) %>%
      dplyr::filter(IslandCode != "CL", ScientificName %in% bands_Biomass_Species) %>%
      dplyr::left_join(siteInfo1) %>%
      tidyr::pivot_longer(cols = c(CountA, CountB), values_to = "Count") %>% 
      dplyr::filter(!is.na(Count), !is.na(CommonName)) %>% 
      dplyr::group_by(IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      CommonName,  SurveyYear, ReserveStatus, Reference) %>%
      dplyr::summarise(Area_Surveyed = ifelse(SurveyYear %in% 1983:1984, n() * 40, n() *60),
                       Mean_Density = base::round(base::sum(Count) / Area_Surveyed, 4)) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName, 
                    CommonName, SurveyYear, Mean_Density, ReserveStatus, Reference) %>%
      dplyr::distinct(SiteCode, ScientificName, SurveyYear, Mean_Density, .keep_all = TRUE)
    
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
      dplyr::select(-SurveyDate, -Species, -PermanentObserverNumber, -Marker, -Diameter_cm) %>% 
      dplyr::full_join(Macro) %>% 
      dplyr::filter(Reference == TRUE, SurveyYear > 2004, SiteCode != "KH") %>%
      dplyr::mutate(
        Stipe_Count = dplyr::case_when(
          is.na(Stipe_Count) & Mean_Density == 0 ~ 0,
          TRUE ~ Stipe_Count),
        Stipe_Count = tidyr::replace_na(Stipe_Count, 0)) %>% 
      dplyr::group_by(IslandCode, IslandName, SiteCode, SiteName, SurveyYear, 
                      ScientificName, CommonName, Mean_Density, Reference, ReserveStatus) %>%
      dplyr::summarise(Stipe_Density = sum(Stipe_Count / sum(Stipe_Count) * Mean_Density, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Mean_Biomass = (Stipe_Density * stipedensity_FSC$slope)*1000) %>% # converts from kg to g here
      dplyr::arrange(SiteName, SurveyYear) %>% 
      dplyr::select(-Stipe_Density)
    
  }
  
  { # Invertebrate Biomass Raw  ----
    Invert_Biomass <- readr::read_csv(
      glue("Raw_DB_Files_SAVE_HERE/KFM_InvertebrateBiomass_1985-{Export_END_Year}.txt")) %>% 
      dplyr::left_join(siteInfo1) %>%
      dplyr::filter(Reference == TRUE, SiteCode != "KH", SurveyYear > 2004,
                    ScientificName %in% Benthic_Biomass_Species,
                    ScientificName != "Macrocystis pyrifera") %>%
      dplyr::left_join(
        dplyr::select(Benthic_Biomass_Coversions, ScientificName, CommonName, a, b)) %>% 
      tidyr::uncount(weights = NoOfInd) %>% 
      dplyr::mutate(Biomass = a * Size_mm ^ b) %>% 
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear,  
                    ScientificName, CommonName, Biomass, ReserveStatus, Reference)
  }
  
  { # Benthic Biomass Long  ----
    Benthic_Biomass <- base::rbind(
      oneM_Biomass, fiveM_Biomass, bands_Biomass_Data) %>% 
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, SurveyYear,  
                    ScientificName, CommonName, Mean_Density, ReserveStatus, Reference) %>% 
      dplyr::filter(Reference == TRUE, SiteCode != "KH", SurveyYear > 2004) %>% 
      dplyr::full_join(Invert_Biomass)  %>%
      dplyr::group_by(SiteCode, ScientificName, SurveyYear) %>% 
      dplyr::mutate(
        Biomass = case_when(
          is.na(Biomass) & Mean_Density == 0 ~ 0,
          TRUE ~ Biomass),
        Mean_Density = case_when(
          Biomass > 0 & Mean_Density == 0 ~ n()/2000,
          TRUE ~ Mean_Density)) %>% 
      dplyr::ungroup() %>% 
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName, CommonName,
                    SurveyYear, Biomass, Mean_Density, ReserveStatus, Reference)
      
    Benthic_Regression_Tidy <- Benthic_Biomass %>%
      group_by(CommonName, ReserveStatus) %>%
      do(broom::tidy(lm(Biomass ~ 0 + Mean_Density, ., na.action = na.exclude)))
    
    Benthic_Regression <- Benthic_Biomass %>%
      group_by(CommonName, ReserveStatus) %>%
      do(broom::glance(lm(Biomass ~ 0 + Mean_Density, ., na.action = na.exclude)))  %>% 
      dplyr::select(-statistic, -p.value) %>% 
      dplyr::full_join(Benthic_Regression_Tidy)
    
    Benthic_Biomass_Long <- Benthic_Biomass %>% 
      left_join(Benthic_Regression) %>% 
      dplyr::mutate(
        Biomass = dplyr::case_when(
          is.na(Biomass) ~ Mean_Density * estimate,
          TRUE ~ Biomass)) %>% 
      dplyr::select(IslandCode, SiteCode,  IslandName, SiteName, SurveyYear, 
                    ScientificName, CommonName, Biomass, Mean_Density, 
                    ReserveStatus, Reference) %>% 
      dplyr::group_by(IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      CommonName, SurveyYear, ReserveStatus, Reference) %>%
      dplyr::summarise(Mean_Biomass = sum(1/n() * Biomass * Mean_Density),
                       Mean_Density = Mean_Density) %>% 
      dplyr::distinct(IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      CommonName, SurveyYear, Mean_Biomass, Mean_Density, ReserveStatus, Reference) %>% 
      base::rbind(Kelp_Biomass, Gorgo_Biomass_Long) %>%
      dplyr::mutate(Date = base::as.Date(base::ISOdate(SurveyYear, 1, 1)),
                    IslandName = gsub(" Island", "", IslandName)) %>%
      readr::write_csv("Tidy_Data_Dont_Touch2/Benthic_Biomass_Long.csv")
  }
  
  { # Benthic Biomass Wide   ----
    Benthic_Biomass_Wide <- Benthic_Biomass_Long %>% 
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName, 
                    SurveyYear, Date, Mean_Biomass, ReserveStatus, Reference) %>% 
      dplyr::group_by(SiteCode, SiteName, IslandCode, IslandName, 
                      ScientificName, SurveyYear, Date, ReserveStatus) %>%
      dplyr::summarise(Mean_Biomass = base::sum(Mean_Biomass)) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from = ScientificName, values_from = Mean_Biomass, values_fill = 0)  %>%
      dplyr::mutate(IslandName = gsub(" Island", "", IslandName)) %>%
      dplyr::left_join(annual_mean_oni, by = c("SurveyYear")) %>% 
      dplyr::rename_with(~ base::gsub(",", "", .)) %>% 
      dplyr::rename_with(~ base::gsub(" ", "_", .)) %>%
      readr::write_csv("Tidy_Data_Dont_Touch2/Benthic_Biomass_Wide.csv") 
  }
 
}

{ # Fish Biomass Tables (Wide for models, Long for plots)   ----
  
  { # Fish Biomass Conversion Tables ----
    
    { # Database equations... WRONG!  ----
      Species_Biomass_DB <- readr::read_csv("Fish_Bio/Species_Biomass_LW.txt") %>%
        dplyr::select(2, 10, 20) %>%
        dplyr::rename_with(~ base::gsub("x", "", .)) %>%
        lapply(function(x) gsub("Total_Length_cm", "TL", x)) %>%
        lapply(function(x) gsub("size_mm", "Size_mm", x)) %>%
        lapply(function(x) gsub("\\[|\\]", "", x)) %>%
        as.data.frame() %>%
        dplyr::distinct(ScientificName, .keep_all = TRUE) %>%
        dplyr::mutate(Category = case_when(
          Database_value == "TL" ~ "Fish",
          Database_value == "Size_mm" ~ "Invertebrate")) %>%
        dplyr::arrange(Category, ScientificName) %>%
        readr::write_csv("Fish_Bio/Species_Biomass_LW.csv")
    }
    
    { # Correct Equations  ----
      Fish_Biomass_Coversions <- readr::read_csv("Fish_Bio/Kramp_growth_parameter.csv") %>% 
        dplyr::filter(ScientificName %in% unique(Fish_Biomass_Long$ScientificName))  %>% 
        dplyr::arrange(ScientificName) %>%
        dplyr::select(1, 3:12, -9, -10, -11)  %>% 
        dplyr::left_join(dplyr::select(Species_Info, ScientificName, CommonName)) %>% 
        dplyr::mutate(CommonName = gsub(", juvenile", "", CommonName),
                      CommonName = gsub(", subadult", "", CommonName),
                      CommonName = gsub(", adult", "", CommonName),
                      CommonName = gsub(", male", "", CommonName),
                      CommonName = gsub(", female", "", CommonName)) %>% 
        dplyr::distinct(ScientificName, .keep_all = TRUE) %>%
        dplyr::mutate(
          WL_a_corrected = case_when(
            WL_L_units == "mm" ~ WL_a * 10 ^ WL_b,
            WL_L_units == "cm" ~ WL_a),
          WL_W_units_corrected = "g",
          WL_L_units_corrected = "cm",
          WL_input_length_corrected = "TL",
          WL_a_conversion = case_when(
            WL_L_units == "mm" ~ paste(round(WL_a, 10), "* 10 ^", WL_b, sep = ""),
            WL_L_units == "cm" ~ paste(WL_a)),
          WL_L_units_conversion = case_when(
            ScientificName == "Embiotoca jacksoni" ~ "(0.799 * TL - 0.407)",
            ScientificName == "Girella nigricans" ~ "(0.851 * TL)",
            ScientificName == "Hypsypops rubicundus" ~ "(0.79 * TL + 0.42)",
            ScientificName == "Medialuna californiensis" ~ "(0.92 * TL)",
            TRUE ~ "TL"),
          WL_Equation = case_when(
            WL_W_units =="kg" ~ paste("W(g)=", round(WL_a_corrected, 6), "*", WL_L_units_conversion, 
                                      "^", WL_b, "*1000", sep = ""),
            WL_W_units =="g" ~ paste("W(g)=", round(WL_a_corrected, 6),"*", WL_L_units_conversion,
                                     "^", WL_b, sep = "")),
          WL_W_conversion = case_when(
            WL_W_units =="kg" ~ "g*1000",
            WL_W_units =="g" ~ "g")) %>% 
        dplyr::select(ScientificName, CommonName,
                      WL_a, WL_a_conversion, WL_a_corrected, WL_b, 
                      WL_W_units, WL_W_conversion, WL_W_units_corrected,
                      WL_L_units, WL_L_units_conversion, WL_L_units_corrected, 
                      WL_input_length, WL_input_length_corrected, WL_Equation,
                      WL_Reference, WL_L_units_conversion_reference) %>%
        readr::write_csv("Fish_Bio/Fish_Biomass_Coversions.csv")
    }
    
    { # Correct Equations with LaTex formatting  ----
      Fish_Biomass_Coversions_Latex <- readr::read_csv("Fish_Bio/Kramp_growth_parameter.csv") %>% 
        dplyr::filter(ScientificName %in% unique(Fish_Biomass_Long$ScientificName))  %>% 
        dplyr::arrange(ScientificName) %>%
        dplyr::select(1, 3:12, -9, -10, -11)  %>% 
        dplyr::left_join(dplyr::select(Species_Info, ScientificName, CommonName)) %>% 
        dplyr::mutate(CommonName = gsub(", juvenile", "", CommonName),
                      CommonName = gsub(", subadult", "", CommonName),
                      CommonName = gsub(", adult", "", CommonName),
                      CommonName = gsub(", male", "", CommonName),
                      CommonName = gsub(", female", "", CommonName)) %>% 
        dplyr::distinct(ScientificName, .keep_all = TRUE) %>%
        dplyr::mutate(
          WL_a_corrected = case_when(
            WL_L_units == "mm" ~ WL_a * 10 ^ WL_b,
            WL_L_units == "cm" ~ WL_a),
          WL_W_units_corrected = "$g$",
          WL_L_units_corrected = "$cm$",
          WL_input_length_corrected = "$TL$",
          WL_a_conversion = case_when(
            WL_L_units == "mm" ~ paste("$a(cm)=", round(WL_a, 10), "* 10 ^", WL_b, "$", sep = ""),
            WL_L_units == "cm" ~ paste("$a(cm)=", WL_a, "$", sep = "")),
          WL_L_units = paste("$", WL_L_units, "$", sep = ""),
          WL_L_units_conversion = case_when(
            ScientificName == "Embiotoca jacksoni" ~ "$(0.799*TL-0.407)$",
            ScientificName == "Girella nigricans" ~ "$(0.851 * TL)$",
            ScientificName == "Hypsypops rubicundus" ~ "$(0.79 * TL + 0.42)$",
            ScientificName == "Medialuna californiensis" ~ "$(0.92 * TL)$",
            TRUE ~ "$TL$"),
          WL_Equation = case_when(
            WL_W_units =="kg" ~ paste("$W_g=", round(WL_a_corrected, 6), "*", WL_L_units_conversion,
                                      "^", "{", WL_b, "}", "*1000", "$", sep = ""),
            WL_W_units =="g" ~ paste("$W_g=", round(WL_a_corrected, 6),"*", WL_L_units_conversion,
                                     "^", "{", WL_b, "}", "$", sep = "")),
          WL_W_conversion = case_when(
            WL_W_units =="kg" ~ "$g*1000$",
            WL_W_units =="g" ~ "$g$"),
          WL_W_units = paste("$", WL_W_units, "$", sep = "")) %>% 
        dplyr::select(ScientificName, CommonName,
                      WL_a, WL_a_conversion, WL_a_corrected, WL_b, 
                      WL_W_units, WL_W_conversion, WL_W_units_corrected,
                      WL_L_units, WL_L_units_conversion, WL_L_units_corrected, 
                      WL_input_length, WL_input_length_corrected, WL_Equation,
                      WL_Reference, WL_L_units_conversion_reference) %>%
        dplyr::mutate(ScientificName = base::gsub(" ", "\\\\text{ }", ScientificName),
                      ScientificName = paste("$", ScientificName, "$", sep = "")) %>% 
        readr::write_csv("Fish_Bio/Fish_Biomass_Coversions_Latex.csv")
    }
    
  }
  
  { # RDFC RAW and NASTY  ----
    RDFC_Biomass_Data <- data.table::fread(
      glue("Raw_DB_Files_SAVE_HERE/KFM_RovingDiverFishCount_RawData_1982-{Export_END_Year}.txt")) %>%
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::mutate(CommonName = gsub('ñ', 'n', CommonName),
                    Abundance = gsub("c", "C", Abundance),
                    Abundance = gsub("f", "F", Abundance),
                    Abundance = gsub("s", "S", Abundance),
                    Abundance = gsub("m", "M", Abundance),
                    Abundance = gsub("^$", NA, Abundance),
                    Abundance = gsub("-", NA, Abundance)) %>%
      dplyr::mutate(Date = base::as.Date(Date, format = '%m/%d/%Y'),
                    CommonName = factor(CommonName)) %>% 
      dplyr::group_by(SurveyYear, SiteNumber) %>% 
      dplyr::filter(Date == base::max(Date), 
                    IslandCode != "CL", 
                    SurveyYear > 2004, 
                    ExperienceLevel == "E",
                    ScientificName %in% Fish_Biomass_Species ) %>% 
      dplyr::ungroup() %>% 
      dplyr::left_join(siteInfo1) %>%
      dplyr::filter(CommonName != "black surfperch, adult",
                    CommonName != "black surfperch, juvenile",
                    CommonName != "blacksmith, adult",
                    CommonName != "blacksmith, juvenile",
                    CommonName != "blue rockfish, adult",
                    CommonName != "blue rockfish, juvenile",
                    CommonName != "kelp bass, adult",
                    CommonName != "kelp bass, juvenile",
                    CommonName != "kelp rockfish, adult",
                    CommonName != "kelp rockfish, juvenile",
                    CommonName != "olive rockfish, adult",
                    CommonName != "olive rockfish, juvenile",
                    CommonName != "opaleye, adult",
                    CommonName != "opaleye, juvenile",
                    CommonName != "pile perch, adult",
                    CommonName != "pile perch, juvenile",
                    CommonName != "senorita, adult",
                    CommonName != "senorita, juvenile",
                    CommonName != "striped surfperch, adult",
                    CommonName != "striped surfperch, juvenile") %>% 
      dplyr::mutate(
        CommonName = forcats::fct_collapse(
          CommonName, "black and yellow rockfish" = c("black and yellow rockfish, adult")),
        CommonName = forcats::fct_collapse(
          CommonName, "cabezon" = c("cabezon, adult","cabezon, juvenile")),
        CommonName = forcats::fct_collapse(
          CommonName, "California scorpionfish" = c("California scorpionfish, adult", "California scorpionfish, juvenile")),
        CommonName = forcats::fct_collapse(
          CommonName, "California sheephead, female" = c("California sheephead, female", "California sheephead, juvenile")),
        CommonName = forcats::fct_collapse(
          CommonName, "halfmoon" = c("halfmoon, adult", "halfmoon, juvenile")),
        CommonName = forcats::fct_collapse(
          CommonName, "garibaldi" = c("garibaldi, adult", "garibaldi, subadult", "garibaldi, juvenile")),
        CommonName = forcats::fct_collapse(
          CommonName, "lingcod" = c("lingcod, adult")),
        CommonName = forcats::fct_collapse(
          CommonName, "ocean whitefish" = c("ocean whitefish, adult", "ocean whitefish, juvenile")),
        CommonName = forcats::fct_collapse(
          CommonName, "rock wrasse, female" = c("rock wrasse, female", "rock wrasse, juvenile")),
        CommonName = forcats::fct_collapse(
          CommonName, "treefish" = c("treefish, adult", "treefish, juvenile")),
        CommonName = gsub(", all", "", CommonName),
        CommonName = factor(CommonName)) %>%
      dplyr::group_by(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, ScientificName,
                      CommonName, SurveyYear, Date, ReserveStatus, MeanDepth, Reference) %>%
      dplyr::summarise(Area_Surveyed = 2000,
                       Mean_Count = as.double(mean(Count)),
                       Mean_Density = round(Mean_Count / Area_Surveyed, 4)) %>%
      dplyr::ungroup() %>%
      dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, 
                    CommonName, ScientificName, SurveyYear, Date,
                    Mean_Count, Mean_Density, Area_Surveyed, MeanDepth, ReserveStatus, Reference) 
    
  }
  
  { # FSF to Biomass  ----
    FSF_Raw <- data.table::fread(glue::glue(
      "Raw_DB_Files_SAVE_HERE/KFM_FishSizeFrequency_RawData_2007-{Export_END_Year}.txt")) %>%
      dplyr::left_join(siteInfo1) %>%
      tidyr::separate(SurveyDate, c('Date','Time'),' ') %>%
      dplyr::filter(ScientificName %in% Fish_Biomass_Species) %>%
      dplyr::mutate(
        CommonName = gsub('ñ', 'n', CommonName),
        CommonName = gsub(", all", "", CommonName),
        CommonName = factor(CommonName),
        CommonName = forcats::fct_collapse(
          CommonName, "California sheephead, female" = c("California sheephead, female", "California sheephead, juvenile")),
        CommonName = forcats::fct_collapse(
          CommonName, "rock wrasse, female" = c("rock wrasse, female", "rock wrasse, juvenile")),
        Count = as.double(Count),
        Date = as.Date(Date, format='%m/%d/%Y')) %>% 
      dplyr::rename(Size_cm = TotalLength_cm) %>%
      tidyr::uncount(weights = Count, .remove = TRUE)  %>%
      dplyr::left_join(dplyr::select(
        Fish_Biomass_Coversions, ScientificName, WL_a_corrected, WL_b)) %>% 
      dplyr::mutate(
        Fish_Biomass = case_when(
          ScientificName == "Embiotoca jacksoni" ~ WL_a_corrected * (0.799 * Size_cm - 0.407) ^ WL_b,
          ScientificName == "Girella nigricans" ~ WL_a_corrected * (0.851 * Size_cm) ^ WL_b, 
          ScientificName == "Hypsypops rubicundus" ~ WL_a_corrected * (0.79 * Size_cm + 0.42) ^ WL_b, 
          ScientificName == "Medialuna californiensis" ~ WL_a_corrected * (0.92 * Size_cm) ^ WL_b,
          ScientificName == "Scorpaenichthys marmoratus" ~ WL_a_corrected * Size_cm ^ WL_b * 1000, 
          ScientificName == "Ophiodon elongatus" ~ WL_a_corrected * Size_cm ^ WL_b * 1000,
          TRUE ~ WL_a_corrected * Size_cm ^ WL_b))%>%
      dplyr::full_join(RDFC_Biomass_Data) %>% 
      dplyr::filter(Reference == TRUE, SiteCode != "KH")  %>%  
      dplyr::group_by(SiteNumber, IslandCode, SiteCode,  IslandName, SiteName, SurveyYear,  
                      ScientificName, CommonName, ReserveStatus, Reference, MeanDepth) %>% 
      dplyr::mutate(
        Mean_Count = case_when(
          Fish_Biomass > 0 & is.na(Mean_Count) ~ as.double(length(Size_cm)),
          Fish_Biomass == 0 & is.na(Mean_Count) ~ as.double(0),
          TRUE ~ Mean_Count)) %>% 
      dplyr::summarise(Mean_Biomass = sum(Fish_Biomass),
                       Mean_Count = mean(Mean_Count)) %>%
      dplyr::ungroup() %>% 
      tidyr::complete(nesting(SiteNumber, SiteCode, SiteName, IslandName,
                              IslandCode, MeanDepth, ReserveStatus, Reference),
                      nesting(ScientificName, CommonName), SurveyYear,
                      fill = list(Mean_Biomass = NA, Mean_Count = NA)) %>%
      dplyr::group_by(SiteNumber, IslandCode, SiteCode,  IslandName, SiteName, SurveyYear, 
                      ScientificName, CommonName, ReserveStatus, Reference, MeanDepth) %>% 
      dplyr::mutate(
        Mean_Biomass = case_when(
          Mean_Count == 0 & is.na(Mean_Biomass) ~ 0,
          is.na(Mean_Count) & is.na(Mean_Biomass) ~ 0,
          TRUE ~ Mean_Biomass)) %>%
      dplyr::mutate(
        Mean_Count = case_when(
          Mean_Biomass == 0 & is.na(Mean_Count) ~ 0,
          TRUE ~ Mean_Count)) %>%
      dplyr::ungroup() 
    
    
  }
  
  { # Fish Biomass Long ----- 
    
    Fish_Regression_Tidy <- FSF_Raw %>%
      group_by(CommonName, IslandName) %>%
      do(broom::tidy(lm(Mean_Biomass ~ 0 + Mean_Count, ., na.action = na.exclude)))
    
    Fish_Regression <- FSF_Raw %>%
      group_by(CommonName, IslandName) %>%
      do(broom::glance(lm(Mean_Biomass ~ 0 + Mean_Count, ., na.action = na.exclude)))  %>% 
      dplyr::select(-statistic, -p.value) %>% 
      dplyr::full_join(Fish_Regression_Tidy)
    
    Fish_Biomass_Long <- FSF_Raw %>% 
      left_join(Fish_Regression) %>% 
      dplyr::mutate(
        Mean_Biomass = dplyr::case_when(
          is.na(Mean_Biomass) ~ Mean_Count * estimate,
          TRUE ~ Mean_Biomass)) %>%
      dplyr::left_join(Fish_Trophic_Levels) %>% 
      dplyr::select(SiteNumber, IslandCode, SiteCode,  IslandName, SiteName, SurveyYear, 
                    ScientificName, CommonName, Mean_Biomass, Mean_Count, Targeted, Trophic_Full,
                    ReserveStatus, Reference, MeanDepth) %>%
      dplyr::mutate(Date = base::as.Date(base::ISOdate(SurveyYear, 1, 1)),
                    IslandName = gsub(" Island", "", IslandName)) %>% 
      readr::write_csv("Tidy_Data_Dont_Touch2/Fish_Biomass_Long.csv")
  }
  
  { # Fish Biomass Wide   -----
    Fish_Biomass_Wide <- Fish_Biomass_Long %>% 
      dplyr::select(IslandCode, IslandName, SiteCode, SiteName, ScientificName, 
                    SurveyYear,  Mean_Biomass, ReserveStatus, Reference) %>% 
      dplyr::filter(Reference == TRUE, SurveyYear > 2004, SiteCode != "KH") %>%
      dplyr::group_by(SiteCode, SiteName, IslandCode, IslandName, 
                      ScientificName, SurveyYear, ReserveStatus) %>%
      dplyr::summarise(Mean_Biomass = base::sum(Mean_Biomass)) %>%
      dplyr::ungroup() %>% 
      tidyr::pivot_wider(names_from = ScientificName, values_from = Mean_Biomass, values_fill = 0) %>%
      dplyr::left_join(annual_mean_oni, by = c("SurveyYear")) %>% 
      dplyr::rename_with(~ base::gsub(",", "", .)) %>% 
      dplyr::rename_with(~ base::gsub(" ", "_", .)) %>%
      readr::write_csv("Tidy_Data_Dont_Touch2/Fish_Biomass_Wide.csv") 
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
      TRUE ~ ScientificName)) %>% 
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
    readr::write_csv("Tidy_Data_Dont_Touch2/All_Mixed_Data_Wide.csv") 
  
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
  #   readr::write_csv("Tidy_Data_Dont_Touch2/Temp_Raw_Tidy.csv")
  
}







