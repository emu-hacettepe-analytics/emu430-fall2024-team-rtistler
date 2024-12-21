library(readxl)

# Dosya yolunu belirleyin
file_path <- "C:/Users/emirm/Desktop/R/stats.xls"

# Veriyi okuma
data <- read_excel(file_path)

#bölgeleri ekleme
# Türkiye'nin illeri ve bağlı oldukları bölgeler
il_ve_bolgeler <- data.frame(
  Il = c("Adana", "Adıyaman", "Afyonkarahisar", "Ağrı", "Aksaray", "Amasya", "Ankara", "Antalya", 
         "Artvin", "Aydın", "Balıkesir", "Bilecik", "Bingöl", "Bitlis", "Bolu", "Burdur", 
         "Bursa", "Çanakkale", "Çorum", "Denizli", "Diyarbakır", "Düzce", "Edirne", "Elazığ", 
         "Erzincan", "Erzurum", "Eskişehir", "Gaziantep", "Giresun", "Gümüşhane", "Hakkari", 
         "Hatay", "Iğdır", "Isparta", "İstanbul", "İzmir", "Kahramanmaraş", "Karabük", "Karaman", 
         "Kastamonu", "Kayseri", "Kilis", "Kocaeli", "Konya", "Kütahya", "Malatya", "Manisa", 
         "Mardin", "Mersin", "Muğla", "Muş", "Nevşehir", "Niğde", "Ordu", "Osmaniye", "Rize", 
         "Sakarya", "Samsun", "Siirt", "Sinop", "Sivas", "Şanlıurfa", "Şırnak", "Tekirdağ", 
         "Tokat", "Trabzon", "Tunceli", "Uşak", "Van", "Yalova", "Yozgat", "Zonguldak"),
  
  Bolge = c("Akdeniz", "Güneydoğu Anadolu", "Ege", "Doğu Anadolu", "İç Anadolu", "Karadeniz", "İç Anadolu", 
            "Akdeniz", "Karadeniz", "Ege", "Marmara", "Marmara", "Doğu Anadolu", "Doğu Anadolu", "Karadeniz", 
            "Akdeniz", "Marmara", "Marmara", "Karadeniz", "Ege", "Güneydoğu Anadolu", "Karadeniz", "Marmara", 
            "Doğu Anadolu", "Doğu Anadolu", "Ege", "Güneydoğu Anadolu", "Karadeniz", "Karadeniz", "Güneydoğu Anadolu", 
            "Akdeniz", "İç Anadolu", "Akdeniz", "İç Anadolu", "İç Anadolu", "Marmara", "Marmara", "Akdeniz", 
            "İç Anadolu", "İç Anadolu", "İç Anadolu", "Ege", "Karadeniz", "Marmara", "Karadeniz", "Ege", 
            "Marmara", "Akdeniz", "Karadeniz", "Karadeniz", "İç Anadolu", "Karadeniz", "Marmara", "Marmara", 
            "Akdeniz", "Karadeniz", "Ege", "İç Anadolu", "Güneydoğu Anadolu", "İç Anadolu", "Güneydoğu Anadolu")
)
data <- merge(data, il_ve_bolgeler, by = "City")

# Bölgelere göre toplam mezun sayısı
region_data <- data %>%
  group_by(Region, Year) %>%
  summarise(TotalGraduates = sum(GraduateCount))


# Verinin ilk birkaç satırını görüntüleme
head(data)
# Veriyi kontrol edin
print(data)

# Her yıl için bir pie chart çizmek
years <- unique(region_data$Year)

for (year in years) {
  year_data <- region_data %>% filter(Year == year)
  
  pie(
    year_data$TotalGraduates, 
    labels = year_data$Region, 
    main = paste("University Graduates by Region -", year)
  )
}

