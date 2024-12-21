library(dplyr)

#calling the data
data <- readxl::read_excel("EMU430-DATA.xls")

#creating 'REGIONS' column
{
  regions <- data.frame(
    Province_code = 1:81
  )
  
  regions <- regions %>% 
    mutate(regions = case_when(
      Province_code %in% c(1,7,15,31,32,33,46,80) ~ "Mediterranean",
      Province_code %in% c(2,21,27,47,56,63,72,73,79) ~ "Southeast Anatolia",
      Province_code %in% c(3,9,20,35,43,45,48,64) ~ "Aegean",
      Province_code %in% c(4,12,13,23,24,25,30,36,44,49,62,65,75,76) ~ "Eastern Anatolia",
      Province_code %in% c(5,8,14,19,28,29,37,52,53,55,57,60,61,67,69,74,78,81) ~ "Black Sea",
      Province_code %in% c(6,18,26,38,40,42,50,51,58,66,68,70,71) ~ "Central Anatolia",
      Province_code %in% c(10,11,16,17,22,34,39,41,54,59,77) ~ "Marmara",
    ))
  
  data <- data %>%
    rename(Province_code = `Province code`)
  
  data <- data %>%
    left_join(regions)
}

