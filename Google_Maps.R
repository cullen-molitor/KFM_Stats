


source("global_markdown.R")
library(ggmap)
library(sf)
library(gganimate)
library(ggnewscale)
library(gifski)
library(magick)

register_google("-------------------------------")

mpa <-  sf::st_as_sf(sf::st_read("GIS_Files/California_Marine_Protected_Areas.shp"))%>%
  dplyr::filter(Type == "SMR" | Type == "FMCA" | Type == "SMCA" | Type == "FMR")

Transect_Endpoints <- read_csv('GIS_Files/transect_0_100.csv')

transects <- sf::st_as_sf(sf::st_read("GIS_Files/KFM_Transects_SmoothLine5.shp"))  %>%
  dplyr::mutate(geometry = sf::st_transform(geometry, "+proj=longlat +ellps=WGS84 +datum=WGS84"))

NPS_boundary <- sf::st_as_sf(sf::st_read("GIS_Files/nps_boundary.shp")) 

CINMS_boundary <- sf::st_as_sf(sf::st_read("GIS_Files/cinms_py.shp"))

MPA_Colors <- c('SMR' = "green", 'FMR' = "deepskyblue2", 'SMCA' = "blue2", 'FMCA' = "red2",
                'Inside' = "green", 'Outside' = "red2")
CHIS <- get_googlemap(c(-119.8, 33.90037),
                      zoom = 9, maptype = "satellite",
                      style = c(labels = "off"))

SouthPointSMR <- get_googlemap(c(-120.141, 33.90037),
                               zoom = 13, maptype = "satellite",
                               style = c(labels = "off"))

ScorpionSMR <- get_googlemap(c(-119.5669, 34.05428),
                             zoom = 13, maptype = "satellite",
                             style = c(labels = "off"))

AnacapaSMR <- get_googlemap(c(-119.40, 34.01260),
                            zoom = 13, maptype = "satellite",
                            style = c(labels = "off"))


SantaBarbaraSMR <- get_googlemap(c(-119.0392, 33.475),
                                 zoom = 14, maptype = "satellite",
                                 style = c(labels = "off"))
theme_set(theme_classic() + 
            theme(legend.position = "none",
                  legend.title = element_text(),
                  plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
                  plot.subtitle = element_text(hjust = 0.5, size = 18),
                  axis.text.y = element_text(size = 12,),
                  axis.text.x = element_text(size = 12,)))

{ # CHIS MPA   ----
  CHIS_Full <- ggmap(CHIS, extent = "device") +
    scale_fill_manual(values = MPA_Colors) +
    scale_color_manual(values = MPA_Colors) +
    labs(title = NULL, x = NULL, y = NULL) +
    theme_classic() +
    theme(legend.position = "none",
          legend.title = element_text(),
          axis.text = element_blank())
  
  # png(filename = "Report_Maps/CHIS.png", width = 1000, height = 1000, type = "cairo")
  # CHIS_Full
  # dev.off()
}

{ # South Point SMR   ----
  SP_SMR <- ggmap(SouthPointSMR, extent = "device") +
    geom_sf(data = mpa, aes(fill = Type, color = Type),
            inherit.aes = FALSE, alpha = .1) +
    geom_sf(data = transects, aes(color = Site_Code),
            inherit.aes = FALSE, alpha = .6, color = "lightblue", size = 1) +
    geom_sf(data = NPS_boundary, inherit.aes = FALSE, color = "green", size = 1, alpha = 0) +
    geom_point(data = siteInfo1,
               aes(x = Longitude, y = Latitude, color = ReserveStatus),
               size = 2, inherit.aes = FALSE) +
    geom_label(data = siteInfo1, hjust = 0, vjust = 1,
               aes(x = Longitude + .003, y = Latitude + .001, label = SiteCode)) +
    geom_sf_label(data = mpa, inherit.aes = FALSE, hjust = .5, vjust = .5, 
                  aes(label = NAME)) +
    scale_fill_manual(values = MPA_Colors) +
    scale_color_manual(values = MPA_Colors) +
    labs(title = "South Point SMR at Santa Rosa Island",
         x = NULL, y = NULL) +
    theme_get()
  
  # png(filename = "Report_Maps/SRI.png", width = 1000, height = 1000, type = "cairo")
  # SP_SMR
  # dev.off()
}

{ # Scorpion SMR   ----
  SC_SMR <- ggmap(ScorpionSMR, extent = "device") +
    geom_sf(data = mpa, aes(fill = Type, color = Type),
            inherit.aes = FALSE, alpha = .1) +
    geom_sf_label(data = mpa, inherit.aes = FALSE, hjust = .5, vjust = .5, 
                  aes(label = NAME)) +
    geom_sf(data = transects, aes(color = Site_Code),
            inherit.aes = FALSE, alpha = .6, color = "lightblue", size = 1) +
    geom_sf(data = NPS_boundary, inherit.aes = FALSE, color = "green", size = 1, alpha = 0) +
    geom_point(data = siteInfo1,
               aes(x = Longitude, y = Latitude, color = ReserveStatus),
               size = 2, inherit.aes = FALSE) +
    geom_label(data = siteInfo1, hjust = .1, vjust = 0,
               aes(x = Longitude + .001, y = Latitude + .001, label = SiteCode)) +
    scale_fill_manual(values = MPA_Colors) +
    scale_color_manual(values = MPA_Colors) +
    labs(title = "Scorpion SMR at Santa Cruz Island",
         x = NULL, y = NULL) +
    theme_get()

  # png(filename = "Report_Maps/SCI.png", width = 1000, height = 1000, type = "cairo")
  # SC_SMR
  # dev.off()
}

{ # Anacapa Island SMR   ----
  ANI_SMR <- ggmap(AnacapaSMR, extent = "device") +
    geom_sf(data = mpa, aes(fill = Type, color = Type),
            inherit.aes = FALSE, alpha = .1) +
    geom_sf_label(data = mpa, inherit.aes = FALSE, hjust = .5, vjust = .5, 
                  aes(label = NAME)) +
    geom_sf(data = transects, aes(color = Site_Code),
            inherit.aes = FALSE, alpha = .6, color = "lightblue", size = 1) +
    geom_sf(data = NPS_boundary, inherit.aes = FALSE, color = "green", size = 1, alpha = 0) +
    geom_point(data = siteInfo1,
               aes(x = Longitude, y = Latitude, color = ReserveStatus),
               size = 2, inherit.aes = FALSE) +
    geom_label(data = siteInfo1, hjust = 1, vjust = 1, 
               aes(x = Longitude - .001, y = Latitude - .001, label = SiteCode)) +
    scale_fill_manual(values = MPA_Colors) +
    scale_color_manual(values = MPA_Colors) +
    labs(title = "Anacapa Island SMR",
         x = NULL, y = NULL) +
    theme_get()
  
  # png(filename = "Report_Maps/ANI.png", width = 1000, height = 1000, type = "cairo")
  # ANI_SMR
  # dev.off()
}

{ # Santa Barbara Island SMR   ----
  SBI_SMR <- ggmap(SantaBarbaraSMR, extent = "device") +
    geom_sf(data = mpa, aes(fill = Type, color = Type),
            inherit.aes = FALSE, alpha = .1) +
    geom_sf(data = transects, aes(color = Site_Code),
            inherit.aes = FALSE, alpha = .6, color = "lightblue", size = 1) +
    geom_sf(data = NPS_boundary, inherit.aes = FALSE, color = "green", size = 1, alpha = 0) +
    geom_label(inherit.aes = FALSE, hjust = .6, vjust = 1,
                  aes(x = -119.02, y = 33.4575), label = "Santa Barbara Island SMR") +
    geom_point(data = siteInfo1,
               aes(x = Longitude, y = Latitude, color = ReserveStatus),
               size = 2, inherit.aes = FALSE) +
    geom_label(data = siteInfo1, hjust = 0, vjust = 1, 
               aes(x = Longitude + .001, y = Latitude + .0001, label = SiteCode)) +
    scale_fill_manual(values = MPA_Colors) +
    scale_color_manual(values = MPA_Colors) +
    labs(title = "Santa Barbara Island SMR",
         x = NULL, y = NULL) +
    theme_get()
  
  # png(filename = "Report_Maps/SBI.png", width = 1000, height = 1000, type = "cairo")
  # SBI_SMR
  # dev.off()
}

{ # MPA Annotated Grid ----
MPA_Grid <- ggarrange(SP_SMR, SC_SMR, ANI_SMR, SBI_SMR, ncol = 2, nrow = 2, align = "hv")
MPA_Grid_Annotated <- ggpubr::annotate_figure(
  MPA_Grid,
  top = text_grob("MPA Reference Sites", color = "black", size = 32, x = .38, hjust = 0),
  bottom = text_grob("Longitude", color = "black", size = 28, x = .47, hjust = 0),
  left = text_grob("Latitdue", color = "black", rot = 90, size = 28, vjust = .6))
png(filename = "Report_Maps/MPA_Grid.png", width = 1000, height = 1000, type = "cairo")
MPA_Grid_Annotated
dev.off()
}
oni <- read.table("https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/detrend.nino34.ascii.txt", header = T) 
oni <- oni %>% 
  dplyr::mutate(Date = as.Date(ISOdate(oni$YR, oni$MON, 1))) %>% 
  dplyr::filter(YR %in% 2005:Year_to_Filter_Data_by)

Benthic_Data <- readr::read_csv("Tidy_Data_Dont_Touch/Benthic_Diversity_Table.csv")

RDFC_Data <- readr::read_csv("Tidy_Data_Dont_Touch/Fish_Diversity_Table.csv")

All_Community_Data <- RDFC_Data %>%
  dplyr::select(-'Alloclinus holderi', -'Coryphopterus nicholsi', -'Lythrypnus dalli', -'Sebastes') %>%
  full_join(Benthic_Data, by = c('IslandCode', 'IslandName', 'SiteCode',
                                 'SiteName','SurveyYear', 'ReserveStatus'))
Shannon_Index <- All_Community_Data %>%
  dplyr::select(-IslandCode, - IslandName, - SiteCode, -SiteName, - SurveyYear, - ReserveStatus) %>%
  vegan::diversity()

Site_Info <- dplyr::select(All_Community_Data, IslandCode, IslandName, 
                           SiteCode, SiteName, SurveyYear, ReserveStatus) 
MPA_Sites <- siteInfo1 %>% 
  dplyr::filter(Reference == TRUE, SiteCode != "KH")

Diversity <- cbind(Site_Info, "ShannonIndex" = Shannon_Index) %>% 
  dplyr::left_join(MPA_Sites) %>% 
  dplyr::select(SiteNumber, IslandCode, IslandName, SiteCode, SiteName, SurveyYear,
                ReserveStatus, ShannonIndex, Latitude, Longitude)  %>% 
  dplyr::mutate(Date = as.Date(ISOdate(SurveyYear, 7, 1)))


# spsmr <- ggmap(SouthPointSMR, extent = "device") +
#   geom_rect(data = oni, aes(xmin = -120.1, xmax = 1.6, ymin = -1.6, ymax = -1.3, fill = ANOM)) +
#   scale_fill_gradient2(high = "darkred", mid = "white", low = "navyblue", midpoint = 0,
#                        guide = guide_colorbar(order = 3)) 


{ # Diversity Animations   ----
SP_SMR <- ggmap(SouthPointSMR, extent = "device") +
  geom_sf(data = mpa, aes(fill = Type, color = Type),
          inherit.aes = FALSE, alpha = .1) +
  geom_sf(data = NPS_boundary, inherit.aes = FALSE, color = "green", size = 1, alpha = 0) +
  geom_point(data = Diversity, inherit.aes = FALSE,
             aes(x = Longitude, y = Latitude, color = ReserveStatus, size = ShannonIndex)) +
  geom_label(data = siteInfo1, hjust = 0, vjust = 1,
             aes(x = Longitude + .003, y = Latitude + .001, label = SiteCode)) +
  geom_sf_label(data = mpa, inherit.aes = FALSE, hjust = .5, vjust = .5, 
                aes(label = NAME)) +
  scale_color_manual(values = MPA_Colors) +
  scale_fill_manual(values = MPA_Colors) +
  scale_size_continuous(range = c(1, 50)) +
  labs(title = "South Point SMR at Santa Rosa Island",
       subtitle = "{frame_time}",
       x = NULL, y = NULL) +
  transition_time(Date) +
  theme_get()

animate(SP_SMR, width = 1000, height = 800, nframes = 300, fps = 15)
anim_save(filename = "Animations/Diversity_SP_SMR_SRI.gif", animation = last_animation())

SC_SMR <- ggmap(ScorpionSMR, extent = "device") +
  geom_sf(data = mpa, aes(fill = Type, color = Type),
          inherit.aes = FALSE, alpha = .1) +
  geom_sf_label(data = mpa, inherit.aes = FALSE, hjust = .5, vjust = .5, 
                aes(label = NAME)) +
  geom_sf(data = NPS_boundary, inherit.aes = FALSE, color = "green", size = 1, alpha = 0) +
  geom_point(data = Diversity, inherit.aes = FALSE,
             aes(x = Longitude, y = Latitude, color = ReserveStatus, size = ShannonIndex)) +
  geom_label(data = siteInfo1, hjust = .1, vjust = 1,
             aes(x = Longitude + .0001, y = Latitude - .001, label = SiteCode)) +
  scale_color_manual(values = MPA_Colors) +
  scale_fill_manual(values = MPA_Colors) +
  scale_size_continuous(range = c(1, 50)) +
  labs(title = "Scorpion SMR at Santa Cruz Island",
       subtitle = "Year: {frame_time}",
       x = NULL, y = NULL) +
  transition_time(Date) +
  theme_get()

animate(SC_SMR, width = 1000, height = 800, nframes = 300, fps = 10)
anim_save(filename = "Animations/Diversity_S_SMR_SCI.gif", animation = last_animation())

ANI_SMR <- ggmap(AnacapaSMR, extent = "device") +
  geom_sf(data = mpa, aes(fill = Type, color = Type),
          inherit.aes = FALSE, alpha = .1) +
  geom_sf_label(data = mpa, inherit.aes = FALSE, hjust = .5, vjust = .5, 
                aes(label = NAME)) +
  geom_sf(data = NPS_boundary, inherit.aes = FALSE, color = "green", size = 1, alpha = 0) +
  geom_point(data = Diversity, inherit.aes = FALSE,
             aes(x = Longitude, y = Latitude, color = ReserveStatus, size = ShannonIndex)) +
  geom_label(data = dplyr::filter(siteInfo1, SiteCode != "KH"), hjust = 1, vjust = 1, 
             aes(x = Longitude - .001, y = Latitude - .001, label = SiteCode)) +
  scale_fill_manual(values = MPA_Colors) +
  scale_color_manual(values = MPA_Colors) +
  scale_size_continuous(range = c(1, 50)) +
  labs(title = "Anacapa Island SMR",
       subtitle = "Year: {frame_time}",
       x = NULL, y = NULL) +
  transition_time(Date) +
  theme_get()

animate(ANI_SMR, width = 1000, height = 800, nframes = 300, fps = 10)
anim_save(filename = "Animations/Diversity_ANI_SMR.gif", animation = last_animation())

SBI_SMR <- ggmap(SantaBarbaraSMR, extent = "device") +
  geom_sf(data = mpa, aes(fill = Type, color = Type),
          inherit.aes = FALSE, alpha = .1) +
  geom_sf(data = NPS_boundary, inherit.aes = FALSE, color = "green", size = 1, alpha = 0) +
  geom_label(inherit.aes = FALSE, hjust = .6, vjust = 1,
             aes(x = -119.02, y = 33.4575), label = "Santa Barbara Island SMR") +
  geom_point(data = Diversity, inherit.aes = FALSE,
             aes(x = Longitude, y = Latitude, color = ReserveStatus, size = ShannonIndex)) +
  geom_label(data = siteInfo1, hjust = 0, vjust = 1, 
             aes(x = Longitude + .001, y = Latitude + .0001, label = SiteCode)) +
  scale_fill_manual(values = MPA_Colors) +
  scale_color_manual(values = MPA_Colors) +
  scale_size_continuous(range = c(1, 50)) +
  labs(title = "Santa Barbara Island SMR",
       subtitle = "Year: {frame_time}",
       x = NULL, y = NULL) +
  transition_time(Date) +
  theme_get()

animate(SBI_SMR, width = 1000, height = 800, nframes = 300, fps = 20)
anim_save(filename = "Animations/Diversity_SBI_SMR.gif", animation = last_animation())


# MPA_Grid <- ggarrange(SP_SMR, SC_SMR, ANI_SMR, SBI_SMR, ncol = 2, nrow = 2, align = "hv") +
#   transition_time(Date)
# MPA_Grid_Annotated <- ggpubr::annotate_figure(
#   MPA_Grid,
#   bottom = text_grob("Longitude", color = "black", size = 26, x = .47, hjust = 0),
#   left = text_grob("Latitdue", color = "black", rot = 90, size = 26, vjust = .6)
# ) 
# animate(MPA_Grid, width = 1000, height = 800, nframes = 300, fps = 10)
# anim_save(filename = "Animations/Diversity_Grid.gif", animation = last_animation())
}


{
  indic_spp_table <- data_frame(Species = character(), s.AN = double(), 
                               s.SB = double(), s.SC = double(), s.SR = double(), 
                               index = integer(), stat= double(), p.value= double(), Year= integer())
  for (k in unique(All_Community_Data$SurveyYear)) {
    Indic_Spp <- All_Community_Data %>%
      filter(SurveyYear %in% k)  %>%
      arrange(IslandName) %>%
      dplyr::select(-IslandCode, - IslandName, -SiteCode, -SiteName, - SurveyYear, - ReserveStatus) %>%
      indicspecies::multipatt(nMDS_Table$IslandCode, func = "r.g", control = how(nperm=1000)) %>%
      .$sign %>%
      rownames_to_column("Species") %>%
      mutate(Year = k) %>%
      filter(p.value < 0.05) 
    indic_spp_table <- rbind(indic_spp_table, Indic_Spp)
  }
  
  
  
  # Turn Species into Y, year into X, strengths of association as fill
  # somehow control fill so anything p.value > 0.05 as red, and increasing shades of green as stat approaches 1
  # mutate new column - if P > 0.05 Significant = NO, else = YES
  # color - red if Significant = No, color Red, else color Green with gradient = p.value
  
  
  
  indic_sites <- c("s.AN", "s.SB", "s.SC", "s.SR")
  for (i in indic_sites) {
    Spp_List <- indic_spp_table %>%
      filter(!!as.symbol(i) == 1) %>%
      .$Species %>%
      unique()
    
    unique_spplist <- paste(i, "_Spp_List", sep = "")
    assign(unique_spplist, Spp_List)
  }
  #These are at least the lists that will be on our y axis when we finally get this right
  
  
  
  heat_map_data <- data.frame(species =character(), stat=double(), p.value=double(), year=integer(), site= character())
  for (z in indic_sites) {
    Spp_Scores <- indic_spp_table %>%
      filter(!!as.symbol(z) == 1) %>%
      mutate(site = z)
    heat_map_data <- heat_map_data %>%
      rbind(Spp_Scores)
  }
  heat_map_data <- dplyr::select(heat_map_data, Species, stat, p.value, Year, site)
  # really this should be called significant heatmap scores or something... 
  
  
  
  Heat_Species <- rep(c(s.AN_Spp_List, s.SB_Spp_List, s.SC_Spp_List, s.SR_Spp_List), times=length(unique(heat_map_data$Year)))
  
  
  
  Heat_Sites <- rep(c((rep("s.AN", each=length(s.AN_Spp_List))), 
                      (rep("s.SB", each=length(s.SB_Spp_List))), 
                      (rep("s.SC", each=length(s.SC_Spp_List))), 
                      (rep("s.SR", each=length(s.SR_Spp_List)))),
                    times=length(unique(heat_map_data$Year)))
  
  
  
  Heat_Year <- rep((unique(heat_map_data$Year)), each = 
                     ((length(s.AN_Spp_List))+
                        (length(s.SB_Spp_List))+
                        (length(s.SC_Spp_List))+
                        (length(s.SR_Spp_List))))
  
  
  
  full_heat_table <- data.frame(site= Heat_Sites, Year= Heat_Year, Species= Heat_Species)
  
  
  
  heat_map <- left_join(full_heat_table, heat_map_data, by=c("site", "Year", "Species"))
}
