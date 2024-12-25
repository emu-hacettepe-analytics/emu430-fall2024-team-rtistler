library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(gganimate)
library(writexl)
library(transformr)

head(data)

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
  
  
  
  ##############
  Gender_Education_Gap <- Gender_Education_Gap |> filter(!is.na(Region))
  data2 <- data |> select(`Province Code`,`General Illiterate Population`)
  data2 <- data2 |> filter(!is.na(`Province Code`))
  names(data2)[2] <- "General_Illiterate_Pop"
  names(Gender_Education_Gap)[7] <- "General_Illiterate_Pop"
  Gender_Education_Gap <- left_join(Gender_Education_Gap,data2,by="General_Illiterate_Pop")
  
  
  names(Gender_Education_Gap)[23] <- "Province Code"
  gap_and_map <- left_join(turkey_map,Gender_Education_Gap,by="Province Code")

  gender_gap_data <- gap_and_map %>% 
    mutate(Gender_Gap = Literacy_Rate_Gap)  # Gender gap'i do??ru s??tuna ba??lay??n
  
  # Animasyonlu harita
  Gender_Gap_Turkey_Anim <- ggplot(gender_gap_data) +
    geom_sf(aes(fill = Gender_Gap), color = "black") +  # Harita s??n??rlar??n?? ??izin
    labs(
      title = "Gender Literacy Gap Over the Years: {frame_time}",
      subtitle = "Difference in Literacy Rates Between Genders",
      x = "Longitude", y = "Latitude"
    ) +
    scale_fill_gradientn(
      name = "Gender Gap",
      colors = c("red", "yellow", "green"),  # ??stedi??iniz renk skalas??
      limits = c(-0.5, 0.5),  # Gender gap aral??????n?? kontrol edin
      na.value = "gray80"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 14),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    transition_time(Year) +  # Y??l s??tununu kullanarak ge??i?? yap
    ease_aes("linear")  # Daha yumu??ak ge??i??ler i??in
  
  # Animasyonu kaydedin
  anim_save(
    "Gender_Gap_Turkey_Anim.gif", 
    animation = animate(
      Gender_Gap_Turkey_Anim, 
      nframes = 200,  # Animasyon i??in kare say??s??
      fps = 10,       # Saniyedeki kare say??s??
      width = 800, 
      height = 600
    )
  )
  
  
  
  
  ##########EN HIZLI DEGISEN 10 IL
  ten_provinces <- data %>%
    select(Year, `General Illiterate Population`, `General Population`, Region, `Province Code`) %>%
    mutate(Illiteracy_Rate = `General Illiterate Population` / `General Population`)
  
  # 2008 ve 2023 y??l?? i??in illiteracy oran??n?? al
  illiteracy_2008 <- ten_provinces %>%
    filter(Year == 2008) %>%
    select(`Province Code`, Illiteracy_Rate) %>%
    rename(Illiteracy_Rate_2008 = Illiteracy_Rate)
  
  illiteracy_2023 <- ten_provinces %>%
    filter(Year == 2023) %>%
    select(`Province Code`, Illiteracy_Rate) %>%
    rename(Illiteracy_Rate_2023 = Illiteracy_Rate)
  
  # En ??ok de??i??im g??steren iller (mutlak de??i??im en b??y??k olanlar)
  change_rate_desc <- illiteracy_2023 %>%
    left_join(illiteracy_2008, by = "Province Code") %>%
    mutate(Change = Illiteracy_Rate_2023 - Illiteracy_Rate_2008) %>%
    arrange(desc(abs(Change))) %>%  # Mutlak de??eri b??y??k olanlar?? s??ral??yoruz
    slice_head(n = 10)
  
  # En az de??i??im g??steren iller (de??i??im de??eri 0'a yak??n olanlar)
  change_rate_inc <- illiteracy_2023 %>%
    left_join(illiteracy_2008, by = "Province Code") %>%
    mutate(Change = Illiteracy_Rate_2023 - Illiteracy_Rate_2008) %>%
    arrange(abs(Change)) %>%  # De??i??imi en k??????k olanlar?? s??ral??yoruz (yani 0'a yak??n)
    slice_head(n = 10)
  
  # En ??ok de??i??im g??steren iller i??in grafik
  ggplot(change_rate_desc, aes(x = reorder(`Province Code`, Change), y = Change, fill = Change > 0)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    labs(
      title = "Top 10 Provinces with the Greatest Change in Illiteracy Rate (2008-2023)",
      x = "Province Code",
      y = "Change in Illiteracy Rate"
    ) +
    coord_flip() +  # Y eksenindeki province isimlerini yatay g??stermek i??in
    theme_minimal()
  
  # En az de??i??im g??steren iller i??in grafik
  ggplot(change_rate_inc, aes(x = reorder(`Province Code`, Change), y = Change, fill = Change > 0)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    labs(
      title = "Top 10 Provinces with the Least Change in Illiteracy Rate (2008-2023)",
      x = "Province Code",
      y = "Change in Illiteracy Rate"
    ) +
    coord_flip() +  # Y eksenindeki province isimlerini yatay g??stermek i??in
    theme_minimal()
  
  
  ##########
  # K??t??phaneler
  library(ggplot2)
  library(gganimate)
  library(dplyr)
  
  # Veriyi d??zenleme
  illiteracy_data <- literacy_rate_by_region %>%
    group_by(Year) %>%
    mutate(rank = rank(-Illiteracy_Rate, ties.method = "first")) %>%  # Azalan s??ralama
    filter(rank <= 10) %>%  # ??lk 10 ??ehir
    ungroup()
  
  # Animasyonlu run chart
  illiteracy_chart <- ggplot(illiteracy_data, aes(
    x = rank, y = Illiteracy_Rate, group = `Province Code`,
    fill = Region, label = `Province Code`
  )) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_text(aes(y = Illiteracy_Rate + 0.01), size = 4, hjust = 0) +
    scale_x_reverse() +
    coord_flip() +
    labs(
      title = "Illiteracy Rate Rankings Over Time",
      subtitle = "Year: {closest_state}",
      x = "Rank",
      y = "Illiteracy Rate"
    ) +
    scale_fill_manual(values = c("#a90432", "#fdb912")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 16),
      axis.text = element_text(size = 12),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) +
    transition_states(
      Year,
      transition_length = 2,  # Ge??i?? s??resi (saniye olarak de??il, ??er??eve uzunlu??u)
      state_length = 12       # Her y??l??n ekranda kalma s??resi (fps'e ba??l?? olarak)
    ) +
    ease_aes("cubic-in-out")
  
  # Animasyonu ??al????t??r
  animate(
    illiteracy_chart,
    nframes = 300,  # Toplam kare say??s?? (daha fazla kare, daha yava?? animasyon)
    fps = 15,       # 15 kare/saniye -> Y??l ba????na 30 ??er??eve = 2 saniye
    width = 800,
    height = 600,
    renderer = gifski_renderer("illiteracy_run_chart_slow_colored.gif")
  )
  
  
  #################OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
  
  # Veriyi d??zenleme (Region bazl?? ortalama)
  region_illiteracy_data <- literacy_rate_by_region %>%
    group_by(Year, Region) %>%
    summarise(Average_Illiteracy_Rate = mean(Illiteracy_Rate, na.rm = TRUE)) %>%  # B??lge baz??nda ortalama
    ungroup() %>%
    group_by(Year) %>%
    mutate(rank = rank(-Average_Illiteracy_Rate, ties.method = "first")) %>%  # Azalan s??rada
    ungroup()
  
  # Animasyonlu run chart (Region bazl??)
  region_illiteracy_chart <- ggplot(region_illiteracy_data, aes(
    x = rank, y = Average_Illiteracy_Rate, group = Region,
    fill = Region, label = Region
  )) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_text(aes(y = Average_Illiteracy_Rate + 0.01), size = 4, hjust = 0) +
    scale_x_reverse() +
    coord_flip() +
    labs(
      title = "Average Illiteracy Rate by Region Over Time",
      subtitle = "Year: {closest_state}",
      x = "Rank",
      y = "Average Illiteracy Rate"
    ) +
    scale_fill_manual(values = c("#a90432", "#fdb912", "#6a5acd", "#32cd32", "#ff4500", "#1e90ff", "#ff1493")) +  # 7 renk
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 16),
      axis.text = element_text(size = 12),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) +
    transition_states(
      Year,
      transition_length = 2,  # Ge??i?? s??resi
      state_length = 12       # Her y??l??n ekranda kalma s??resi
    ) +
    ease_aes("cubic-in-out")
  
  # Animasyonu ??al????t??r
  animate(
    region_illiteracy_chart,
    nframes = 300,  # Toplam kare say??s??
    fps = 15,       # 15 kare/saniye -> Y??l ba????na 30 ??er??eve = 2 saniye
    width = 800,
    height = 600,
    renderer = gifski_renderer("region_illiteracy_run_chart.gif")
  )
  
  
  #########literrate
  # Veriyi d??zenleme (Region bazl?? okuma-yazma oran??)
  region_literacy_data <- literacy_rate_by_region %>%
    group_by(Year, Region) %>%
    summarise(Average_Literacy_Rate = mean(1 - Illiteracy_Rate, na.rm = TRUE)) %>%  # Literacy rate hesaplama
    ungroup() %>%
    group_by(Year) %>%
    mutate(rank = rank(-Average_Literacy_Rate, ties.method = "first")) %>%  # Azalan s??rada s??ralama
    ungroup()
  
  # Animasyonlu run chart (Literacy rate)
  region_literacy_chart <- ggplot(region_literacy_data, aes(
    x = rank, y = Average_Literacy_Rate, group = Region,
    fill = Region, label = Region
  )) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_text(aes(y = Average_Literacy_Rate + 0.01, label = scales::percent(Average_Literacy_Rate, accuracy = 0.1)), 
              size = 4, hjust = 0) +  # Y??zde olarak metin ekleme
    scale_x_reverse() +
    coord_flip() +
    labs(
      title = "Average Literacy Rate by Region Over Time",
      subtitle = "Year: {closest_state}",
      x = "Rank",
      y = "Average Literacy Rate"
    ) +
    scale_fill_manual(values = c("#a90432", "#fdb912", "#6a5acd", "#32cd32", "#ff4500", "#1e90ff", "#ff1493")) +  # 7 renk
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 16),
      axis.text = element_text(size = 12),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) +
    transition_states(
      Year,
      transition_length = 2,  # Ge??i?? s??resi
      state_length = 12       # Her y??l??n ekranda kalma s??resi
    ) +
    ease_aes("cubic-in-out")
  
  # Animasyonu ??al????t??r
  animate(
    region_literacy_chart,
    nframes = 300,  # Toplam kare say??s??
    fps = 15,       # 15 kare/saniye -> Y??l ba????na 30 ??er??eve = 2 saniye
    width = 800,
    height = 600,
    renderer = gifski_renderer("region_literacy_run_chart.gif")
  )
  
  
}