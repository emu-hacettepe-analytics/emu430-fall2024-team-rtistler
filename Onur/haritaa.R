library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(gganimate)
library(writexl)

#Graph 1.1.1 : Illiterates Change Over the Years by Provinces -MAP
{
  #Calling the map datas
  turkey_map <- st_read("gadm41_TUR_1.json")
  data <- readxl::read_excel("EMU430-DATA.xls")
  
  data <- data |> 
    mutate(
      Master_Doctorate_Universities = `General Master Graduates` + `General Doctorate Graduates` + `General Universities and Other Higher Educational Institutions Graduates`,
      Male_Master_Doctorate_Universities = `Male Master Graduates` + `Male Doctorate Graduates`+ `Male Universities and Other Higher Educational Institutions Graduates`,
      Female_Master_Doctorate_Universities = `Female Master Graduates`+ `Female Doctorate Graduates`+`Female Universities and Other Higher Educational Institutions Graduates`,
      
      Primary_Education_School = `General Primary School Graduates` + `General Primary Education Graduates`,
      Male_Primary_Education_School = `Male Primary Education Graduates...18` + `Male Primary School Graduates`,
      Female_Primary_Education_School = `Female Primary School Graduates` + `Male Primary Education Graduates...19`,
      
      Secondary_School = `General Lower Secondary School Graduates` + `General Upper Secondary School Graduates`,
      Male_Secondary_School = `Male Lower Secondary School Graduates`+`Male Upper Secondary School Graduates`,
      Female_Secondary_School = `Female Lower Secondary School Graduates` + `Female Upper Secondary School Graduates`
  )
  
  #Adjusting the Map data
  turkey_map[3,11] <- "TR-03"
  turkey_map[13,11] <- "TR-74"
  turkey_map[17,11] <- "TR-12"
  turkey_map[22,11] <- "TR-17"
  turkey_map[23,11] <- "TR-18"
  turkey_map[24,11] <- "TR-19"
  turkey_map[27,11] <- "TR-81"
  turkey_map[29,11] <- "TR-23"
  turkey_map[35,11] <- "TR-29"
  turkey_map[38,11] <- "TR-76"
  turkey_map[42,11] <- "TR-46"
  turkey_map[43,11] <- "TR-78"
  turkey_map[49,11] <- "TR-71"
  turkey_map[54,11] <- "TR-43"
  turkey_map[81,11] <- "TR-67"
  
  turkey_map$ISO_1 <-  as.numeric(gsub("TR-", "", turkey_map$ISO_1))
  names(turkey_map)[11] <- "Province Code"
  names(data)[2] <- "Province Code"
  
  data_with_map <- left_join(turkey_map,data,by = "Province Code")
  

########################TR??YK??E
  Percentage_of_Literacy_Relative_to_the_General_Population.gif <- ggplot(data = data_with_map) +
    geom_sf(aes(fill = (data_with_map$`General Population`- data_with_map$`General Illiterate Population`)/`General Population`)) +
    labs(title = "Percentage of Literacy Relative to the General Population in Year :{frame_time}", x = "Longitude", y = "Latitude") +
    theme_minimal() +
    scale_fill_gradientn(
      name = "Percentage of Literacy",
      colors = c("white","red","yellow", "green","darkgreen"),
    ) +
    theme(legend.position = "bottom") +
    transition_time(as.integer(data_with_map$Year))
  anim_save("Percentage_of_Literacy_Relative_to_the_General_Population.gif", animation = Percentage_of_Literacy_Relative_to_the_General_Population.gif)
  
  ggplot(data = data_with_map |> filter(Year == 2008)) +
    geom_sf(aes(fill = (`General Population` - `General Illiterate Population`) / `General Population`)) +
    labs(title = "Percentage of Literacy Relative to the General Population in Year :2008", 
         x = "Longitude", 
         y = "Latitude") +
    theme_minimal() +
    scale_fill_gradientn(
      name = "Percentage of Literacy",
      colors = c("white", "red", "yellow", "green", "darkgreen")
    ) +
    theme(legend.position = "bottom")
  
  ggplot(data = data_with_map |> filter(Year == 2023)) +
    geom_sf(aes(fill = (`General Population` - `General Illiterate Population`) / `General Population`)) +
    labs(title = "Percentage of Literacy Relative to the General Population in Year :2023", 
         x = "Longitude", 
         y = "Latitude") +
    theme_minimal() +
    scale_fill_gradientn(
      name = "Percentage of Literacy",
      colors = c("white", "red", "yellow", "green", "darkgreen")
    ) +
    theme(legend.position = "bottom")
  
#############T??RK??YE UN??V
  ggplot(data = data_with_map) +
    geom_sf(aes(fill = (`General Master Graduates` + `General Doctorate Graduates` + `General Universities and Other Higher Educational Institutions Graduates`)/`General Population`)) +
    labs(title = "Percentage of Illiteracy Relative to the General Population in Year :{frame_time}", x = "Longitude", y = "Latitude") +
    theme_minimal() +
    scale_fill_gradientn(
      name = "Percentage of Illiteracy",
      colors = c("white","blue"),
    ) +
    theme(legend.position = "bottom") +
    transition_time(as.integer(data_with_map$Year))
  anim_save("illiterateanim.gif", animation = illiterateanim)

################################YYAN YANA BOX PLOT
  region_and_data <- data |> 
    filter(!is.na(Region)) |> 
    pivot_longer(
      cols = c(
        Primary_Education_School, 
        Secondary_School,
        Master_Doctorate_Universities
      ),
      names_to = "Population_Type",
      values_to = "Count"
    )
  region_and_data$Population_Type <- factor(region_and_data$Population_Type, 
                                            levels = c("Primary_Education_School", 
                                                       "Secondary_School", 
                                                       "Master_Doctorate_Universities"))
  
  ggplot(region_and_data, aes(x = factor(Region), y = Count / `General Population`, fill = Population_Type)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.6) +
    labs(
      title = "Education Level Rates by Region in Year : {frame_time}",
      x = "Region",
      y = "Rate",
      fill = "Population Type"
    ) +
    scale_fill_discrete(
      labels = c(
        "Primary_Education_School" = "Primary",    
        "Secondary_School" = "Secondary",           
        "Master_Doctorate_Universities" = "Higher" 
      )
    ) +
    transition_time(as.integer(region_and_data$Year)) +
    theme_minimal()
  
  ##2008 Grafi??i
  ggplot(region_and_data |> filter(Year == 2008), aes(x = factor(Region), y = Count / `General Population`, fill = Population_Type)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.6) +
    labs(
      title = "Education Level Rates by Region in Year : 2008",
      x = "Region",
      y = "Rate",
      fill = "Population Type"
    ) +
    scale_fill_discrete(
      labels = c(
        "Primary_Education_School" = "Primary",    
        "Secondary_School" = "Secondary",           
        "Master_Doctorate_Universities" = "Higher" 
      )
    ) +
    theme_minimal()
  ##2023
  ggplot(region_and_data |> filter(Year == 2023), aes(x = factor(Region), y = Count / `General Population`, fill = Population_Type)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.6) +
    labs(
      title = "Education Level Rates by Region in Year : 2023",
      x = "Region",
      y = "Rate",
      fill = "Population Type"
    ) +
    scale_fill_discrete(
      labels = c(
        "Primary_Education_School" = "Primary",    
        "Secondary_School" = "Secondary",           
        "Master_Doctorate_Universities" = "Higher" 
      )
    ) +
    theme_minimal()
  
  ############GENDERGAP
  
  Gender_Education_Gap <- data %>%
    select(Year, Region, `General Male Population`, `General Female Population`,
           `Female Illiterate Population`, `Male Illiterate Population`, `General Illiterate Population`,
           Master_Doctorate_Universities, Female_Master_Doctorate_Universities, Male_Master_Doctorate_Universities,
           Secondary_School, Female_Secondary_School, Male_Secondary_School,
           Primary_Education_School, Female_Primary_Education_School, Male_Primary_Education_School) %>%
    mutate(
      Male_Literacy_Rate = (`General Male Population` - `Male Illiterate Population`) / `General Male Population`,
      Female_Literacy_Rate = (`General Female Population` - `Female Illiterate Population`) / `General Female Population`,
      Primary_Rate_Gap = (`Male_Primary_Education_School` - `Female_Primary_Education_School`) / `Primary_Education_School`,
      Secondary_Rate_Gap = (`Male_Secondary_School` - `Female_Secondary_School`) / `Secondary_School`,
      Higher_Rate_Gap = (`Male_Master_Doctorate_Universities` - `Female_Master_Doctorate_Universities`) / `Master_Doctorate_Universities`
    )
  
  Gender_Education_Gap <- Gender_Education_Gap %>%
    mutate(
      Literacy_Rate_Gap = Male_Literacy_Rate - Female_Literacy_Rate
    )
  
  Gender_Education_Gap_long <- Gender_Education_Gap %>%
    select(Year, Primary_Rate_Gap, Secondary_Rate_Gap, Higher_Rate_Gap, Literacy_Rate_Gap) %>%
    pivot_longer(cols = c(Primary_Rate_Gap, Secondary_Rate_Gap, Higher_Rate_Gap, Literacy_Rate_Gap), 
                 names_to = "Rate_Type", 
                 values_to = "Gap")
  
  ggplot(Gender_Education_Gap_long, aes(x = factor(Year), y = Gap, fill = Rate_Type)) +
    geom_boxplot(color = "black", alpha = 0.6) +
    labs(
      title = "Gender Education Rate Gap Over the Years",
      x = "Year",
      y = "Difference Between Male and Female Education Rates (%)",
      fill = "Rate Type"
    ) +
    theme_minimal() +
    facet_wrap(~ Rate_Type, scales = "free_y") +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1)  # X eksenindeki y??l verilerini dikey yapar
    )
  
  
  
  

  Gender_Education_Gap_avg <- Gender_Education_Gap_long %>%
    group_by(Year, Rate_Type) %>%
    summarise(Average_Gap = mean(Gap, na.rm = TRUE))
  
  ggplot(Gender_Education_Gap_avg, aes(x = factor(Year), y = Average_Gap, color = Rate_Type, group = Rate_Type)) +
    geom_line(size = 1.2) +  # Line plot kullan??yoruz
    geom_point(size = 3) +  # Noktalar ekleyerek ??izgileri daha belirgin yap??yoruz
    labs(
      title = "Average Gender Education Rate Gap Over the Years",
      x = "Year",
      y = "Average Difference Between Male and Female Education Rates (%)",
      color = "Rate Type"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1)  # X eksenindeki y??l verilerini dikey yapar
    )
  
  
  
  
}