#install.packages(c("leaflet", "RColorBrewer", "readxl", "tidyverse", "sf","shiny"))
#install.packages("shiny")
library(readxl)
library(geosphere)  
library(tidyverse)
library(sf)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(stringr)


# --- 1. Baca data utama ---
data <- read_excel("/Users/macbookpro/Documents/rProjects/source/delivery_sla_summary_ta.xlsx", 
                   sheet = "delivery_sla_summary") %>%
  mutate(
    latitude = latitude / 1000000,
    longitude = longitude / 1000000
  ) %>%
  filter(
    !is.na(longitude),
    !is.na(latitude),
    between(longitude, -97, 140), #filter untuk longlat yang valid saja
    between(latitude, -20, 5), #filter untuk longlat yang valid saja
    str_detect(position_name, regex("SATRIA", ignore_case = TRUE)),
    str_detect(staging_penempatan, regex("CLBT0", ignore_case = TRUE))
  )

#INISIASI JUMLAH CLUSTER SESUAI KURIR
# ----- PARAMETER ITERASI -----
iter <- 3
k <- 36
n <- nrow(data)
best_assignment <- NULL
lowest_score <- Inf

coords <- data[, c("latitude", "longitude")]

for (it in 1:iter) {
  set.seed(100 + it)
  init_kmeans <- kmeans(coords, centers = k)
  centroids <- init_kmeans$centers
  
  # Hitung jarak haversine matrix
  haversine_matrix <- matrix(NA, nrow = n, ncol = k)
  for (i in 1:n) {
    for (j in 1:k) {
      haversine_matrix[i, j] <- distHaversine(
        c(coords$longitude[i], coords$latitude[i]),
        c(centroids[j, "longitude"], centroids[j, "latitude"])
      )
    }
  }
  haversine_matrix <- haversine_matrix / 1000
  
  # Alokasi round-robin berdasarkan jarak
  distance_df <- as.data.frame(haversine_matrix)
  distance_df$id <- 1:n
  long_df <- pivot_longer(distance_df, cols = -id, names_to = "cluster", values_to = "dist")
  long_df$cluster <- as.integer(gsub("V", "", long_df$cluster))
  long_df <- long_df %>% arrange(dist)
  
  assignment <- rep(NA_integer_, n)
  cluster_sizes <- rep(0, k)
  max_per_cluster <- rep(floor(n / k), k)
  if (n %% k > 0) max_per_cluster[1:(n %% k)] <- max_per_cluster[1:(n %% k)] + 1
  
  for (i in 1:nrow(long_df)) {
    row <- long_df[i, ]
    if (is.na(assignment[row$id]) && cluster_sizes[row$cluster] < max_per_cluster[row$cluster]) {
      assignment[row$id] <- row$cluster
      cluster_sizes[row$cluster] <- cluster_sizes[row$cluster] + 1
    }
  }
  
  # Evaluasi: hitung jarak antar titik per cluster
  data$cluster_id <- assignment
  summary_cluster <- data %>%
    group_by(cluster_id) %>%
    summarise(
      total_awb = n(),
      centroid_lat = mean(latitude),
      centroid_lon = mean(longitude)
    )
  
  # Hitung total jarak antar titik ke centroid per cluster
  data_eval <- data %>%
    left_join(summary_cluster, by = "cluster_id") %>%
    rowwise() %>%
    mutate(
      dist_to_centroid = distHaversine(c(longitude, latitude), c(centroid_lon, centroid_lat)) / 1000
    )
  
  cluster_distance_summary <- data_eval %>%
    group_by(cluster_id) %>%
    summarise(
      total_awb = n(),
      total_dist_km = sum(dist_to_centroid)
    )
  
  # Hitung skor total deviasi AWB dan jarak
  score <- sd(cluster_distance_summary$total_awb) + sd(cluster_distance_summary$total_dist_km)
  
  if (score < lowest_score) {
    lowest_score <- score
    best_assignment <- assignment
  }
}

# Gunakan best_assignment
data$cluster_id <- best_assignment


# --- 7. Gabungkan nama cluster dari sheet courier_name ---
cluster_lookup <- read_excel("/Users/macbookpro/Documents/rProjects/source/delivery_sla_summary_ta.xlsx", 
                             sheet = "courier_name") %>%
  mutate(cluster_id = as.integer(cluster_id))

data <- data %>%
  left_join(cluster_lookup, by = "cluster_id")

# --- 8. Warna dan visualisasi Leaflet ---
pal <- colorFactor(
  palette = colorRampPalette(brewer.pal(9, "Set1"))(k), 
  domain = data$satria
)

leaflet(data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    ~longitude, ~latitude,
    label = ~paste(awb, ":", satria),
    color = ~pal(satria),
    radius = 8,
    stroke = FALSE, fillOpacity = 0.8
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~satria,
    title = "Cluster"
  )

# --- 9. Tampilkan jumlah AWB per cluster ---
awb_count_per_cluster <- data %>%
  count(satria, name = "awb_count") %>%
  arrange(desc(awb_count))

#======10 evaluassi================

# 1. Ambil titik awal berdasarkan AWB pertama per kurir
start_coords <- data %>%
  group_by(`satria`) %>%
  slice(1) %>%
  select(`satria`, start_lat = latitude, start_lon = longitude)

# 2. Gabungkan titik awal ke seluruh data
data <- data %>%
  left_join(start_coords, by = "satria") %>%
  arrange(`satria`, `205_tm`) %>%
  group_by(`satria`) %>%
  mutate(
    lon_prev = lag(longitude),
    lat_prev = lag(latitude),
    lon_prev = ifelse(is.na(lon_prev), start_lon, lon_prev),
    lat_prev = ifelse(is.na(lat_prev), start_lat, lat_prev),
    distance_km = distHaversine(cbind(lon_prev, lat_prev), cbind(longitude, latitude)) / 1000
  )

data <- data %>%
  mutate(
    time_current = as.POSIXct(`205_tm`),
    time_prev = lag(time_current),
    time_prev = ifelse(is.na(time_prev), time_current, time_prev),
    time_prev = as.POSIXct(time_prev, origin = "1970-01-01"),
    time_diff_min = as.numeric(difftime(time_current, time_prev, units = "mins"))
  )

summary_distance <- data %>%
  group_by(`satria`) %>%
  summarise(
    total_awb = n(),
    total_distance_km = sum(distance_km),
    min_distance_km = min(distance_km),
    max_distance_km = max(distance_km),
    mean_distance_km = mean(distance_km)
  )

summary_time <- data %>%
  group_by(`satria`) %>%
  summarise(
    total_time_min = sum(time_diff_min, na.rm = TRUE),
    min_time_min = min(time_diff_min, na.rm = TRUE),
    max_time_min = max(time_diff_min, na.rm = TRUE),
    mean_time_min = mean(time_diff_min, na.rm = TRUE)
  )

summary_efficiency <- summary_distance %>%
  inner_join(summary_time, by = "satria") %>%
  mutate(
    efficiency_km_per_min = total_distance_km / total_time_min
  )

#=================
# Urutkan berdasarkan nama kurir agar urutan sama di kedua metrik
summary_dual <- summary_distance %>%
  arrange(desc(total_distance_km)) %>%
  mutate(Kurir = factor(`satria`, levels = `satria`))

# Buat plot
ggplot(summary_dual, aes(x = Kurir)) +
  geom_line(aes(y = total_awb, group = 1, color = "Total AWB"), size = 1) +
  geom_line(aes(y = total_distance_km * (max(total_awb) / max(total_distance_km)), group = 1, color = "Total Distance (km)"), linetype = "dashed", size = 1) +
  scale_y_continuous(
    name = "Total AWB",
    sec.axis = sec_axis(~ . * (max(summary_dual$total_distance_km) / max(summary_dual$total_awb)), name = "Total Distance (km)")
  ) +
  labs(
    title = "Perbandingan Total Pengiriman (AWB) dan Jarak Tempuh Kurir",
    x = "Nama Kurir",
    color = "Metrik"
  ) +
  scale_color_manual(values = c("Total AWB" = "blue", "Total Distance (km)" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#====HIISTOGRAM===========
# Histogram jumlah Paket per kurir
ggplot(awb_count_per_cluster, aes(x = reorder(satria, -awb_count), y = awb_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Histogram Jumlah AWB per Kurir",
    x = "Nama Kurir",
    y = "Jumlah Paket"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#================================================
#AFTER
#================================================
print(awb_count_per_cluster, n = k)
mean_awb <- mean(awb_count_per_cluster$awb_count)
sd_awb <- sd(awb_count_per_cluster$awb_count)
mean_jarak <- mean(summary_distance$total_distance_km)
sd_jarak <- sd(summary_distance$total_distance_km)

cat("Rata-rata jumlah AWB per cluster:", mean_awb, "\n")
cat("Standar deviasi jumlah AWB per cluster:", sd_awb, "\n")
cat("Rata-rata jarak per kurir:", mean_jarak, "\n")
cat("Standar deviasi jarak per kurir:", sd_jarak, "\n")

#=================================================
#Round-Robin Assignment Berdasarkan Jarak Terdekat
#ADI GUNAWAN DAPAT ASSIGNMENT JAUH, DARI BARAT KE TIMUR, JADI KURANG PERHATIIN ASPEK JARAK
#ALGORITMA SEDIKIT LAMBAT
#DEVIASI BAGUS HANYA 1 AWB
#Intinya:
#Hitung jarak semua titik ke semua centroid.
#Urutkan setiap titik berdasarkan jarak terdekat.
#Alokasikan titik ke klaster dengan rotasi (round robin) sambil mengecek agar ukuran klaster tetap seimbang.
#keuntungan:
#Jumlah data per klaster akan benar-benar seimbang (selisih maksimal 1).
#Tetap mempertimbangkan kedekatan ke centroid, jadi distribusi tidak terlalu random.