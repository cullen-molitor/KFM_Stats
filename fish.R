{ # Fish Biomass Tables (Wide for models, Long for plots)   ----
  
  { # Fish Biomass Species and Trophic Levels ----
    
    Fish_Trophic_Levels <- readr::read_csv("Meta_Data/KFM_Fish_Trophic_Levels.csv")
    
    Fish_Biomass_Species <- c(
      "Caulolatilus princeps", "Chromis punctipinnis", "Embiotoca jacksoni", 
      "Embiotoca lateralis", "Girella nigricans", "Halichoeres semicinctus",   
      "Hypsypops rubicundus", "Medialuna californiensis", "Ophiodon elongatus", 
      "Oxyjulis californica", "Paralabrax clathratus", "Rhacochilus toxotes",
      "Rhacochilus vacca", "Scorpaena guttata", "Scorpaenichthys marmoratus",
      "Sebastes atrovirens", "Sebastes chrysomelas", "Sebastes mystinus",         
      "Sebastes serranoides", "Sebastes serriceps", "Semicossyphus pulcher")
  }
  
  { # Fish Biomass Conversion Tables ----
    
    { # Database equations... WRONG!  ----
      # Species_Biomass_DB <- readr::read_csv("Fish_Bio/Species_Biomass_LW.txt") %>%
      #   dplyr::select(2, 10, 20) %>%
      #   dplyr::rename_with(~ base::gsub("x", "", .)) %>% 
      #   lapply(function(x) gsub("Size_cm", "TL", x)) %>% 
      #   lapply(function(x) gsub("\\[|\\]", "", x)) %>% 
      #   as.data.frame() %>% 
      #   dplyr::distinct(ScientificName, .keep_all = TRUE) %>% 
      #   dplyr::mutate(Category = case_when(
      #     Database_value == "TL" ~ "Fish",
      #     Database_value == "size_mm" ~ "Invertebrate")) %>% 
      #   dplyr::arrange(Category, ScientificName) %>% 
      #   readr::write_csv("Fish_Bio/Species_Biomass_LW.csv")
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
  
  { # FSF to Biomass ----
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
    library(broom)
    library(purrr)
    
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
      readr::write_csv("Tidy_Data_Dont_Touch/Fish_Biomass_Long.csv")
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
      readr::write_csv("Tidy_Data_Dont_Touch/Fish_Biomass_Wide.csv") 
  }
  
}