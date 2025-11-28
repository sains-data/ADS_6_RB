#Import Data
library(tidyverse)
df_raw <- read_csv2("Dataset_Tugas_Besar_ADS.csv")

#Pembersihan data dan penamaan kembali kolom
df_final <- df_raw %>%
  filter(!is.na(NIM)) %>%
  
  select(
    NIM,
    IPK = `IPK Terakhir :`,
    Pendapatan_Ortu = `Median Pendapatan Orang Tua (Rupiah) :`,
    Jarak = `Median dari Jarak rumah dari kampus ITERA (Km) :`,
    
   
    Wifi_Pribadi = D_Wifi,
    Fasilitas_Kampus = D_Kampus
  ) %>%
  
  mutate(
    IPK = as.numeric(IPK),
    Jarak = as.numeric(Jarak),
    Pendapatan_Ortu = as.numeric(Pendapatan_Ortu),
  )

glimpse(df_final)
summary(df_final$IPK)



#Mengolah data -> Analisis Regresi dan Korelasi



#Model 1 : Pengaruh Pendapatan Orang Tua terhadap IPK 
#Persiapan data
df_analisis <- df_final %>%
  mutate(Pendapatan_Juta = Pendapatan_Ortu / 1000000)

#Uji Korelasi (Pearson)
uji_korelasi <- cor.test(df_analisis$Pendapatan_Juta, df_analisis$IPK)
nilai_r <- uji_korelasi$estimate
print("Hasil uji korelasi :")
print(uji_korelasi)

#Analisis regresi
model_regresi <- lm(IPK ~ Pendapatan_Juta, data = df_analisis)
print("Model regresi :")
summary(model_regresi)
ringkasan_model <- summary(model_regresi)

#Mengambil Koefisien (Intercept dan Slope)
a <- coef(model_regresi)[1] 
b <- coef(model_regresi)[2] 
r_squared <- ringkasan_model$r.squared


#Visualisasi menggunakan scatter plot dan garis regresi
library(ggplot2)

ggplot(df_analisis, aes(x = Pendapatan_Juta, y = IPK)) +
  geom_point(color = "darkblue", alpha = 0.6, size = 3) + 
  geom_smooth(method = "lm", color = "red", se = TRUE) +  
  labs(
    title = "PENGARUH PENDAPATAN ORANG TUA TERHADAP IPK",
    x = "Pendapatan Orang Tua (Juta Rupiah)",
    y = "Indeks Prestasi Kumulatif (IPK) Terakhir"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

#Hasil
cat("Hasil analisis regresi dan korelasi\n")
cat("1. Nilai Korelasi Pearson (r):", round(nilai_r, 4), "\n")
cat("   (Interpretasi: Kekuatan hubungan antara variabel X dan Y)\n\n")

cat("2. Koefisien Determinasi (R^2):", round(r_squared, 4), "\n")
cat("   (Interpretasi: Persentase pengaruh X terhadap Y)\n\n")

cat("3. Persamaan Regresi Linear:\n")
cat("   Y =", round(a, 3), "+", round(b, 3), "X\n")
cat("   (Y = IPK, X = Pendapatan Orang Tua)\n")



#Model 2 : Pengaruh Jarak Tempat Tinggal terhadap IPK 
#Persiapan Data
df_model2 <- df_final 

#Uji Korelasi (Pearson)
uji_korelasi_2 <- cor.test(df_model2$Jarak, df_model2$IPK)
nilai_r_2 <- uji_korelasi_2$estimate
print("Hasil uji korelasi (Jarak) :")
print(uji_korelasi_2)

#Analisis regresi
model_regresi_2 <- lm(IPK ~ Jarak, data = df_model2)
print("Model regresi :")
summary(model_regresi_2)
ringkasan_model_2 <- summary(model_regresi_2)

# Mengambil Koefisien (Intercept dan Slope)
a_2 <- coef(model_regresi_2)[1] 
b_2 <- coef(model_regresi_2)[2] 
r_squared_2 <- ringkasan_model_2$r.squared

#Visualisasi menggunakan scatter plot dan garis regresi
library(ggplot2)

p_bubble <- ggplot(df_model2, aes(x = Jarak, y = IPK)) +
  
  geom_count(aes(color = after_stat(n), size = after_stat(n)), alpha = 0.7) +
  
  geom_smooth(method = "lm", color = "red", se = FALSE, linetype = "solid") +
  
  scale_size_continuous(range = c(3, 10), name = "Jlh Mhs") + 
  scale_color_gradient(low = "#85D8CE", high = "#085078", name = "Jlh Mhs") + 
  
  labs(
    title = "PENGARUH JARAK TEMPAT TINGGAL TERHADAP IPK",
    x = "Jarak Tempat Tinggal (Km)",
    y = "Indeks Prestasi Kumulatif (IPK) Terakhir"
  ) +
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right" 
  )

print(p_bubble)


#Hasil
cat("Hasil analisis regresi dan korelasi\n")
cat("1. Nilai Korelasi Pearson (r):", round(nilai_r_2, 4), "\n")
cat("   (Interpretasi: Kekuatan hubungan antara variabel X dan Y)\n\n")

cat("2. Koefisien Determinasi (R^2):", round(r_squared_2, 4), "\n")
cat("   (Interpretasi: Persentase pengaruh X terhadap Y)\n\n")

cat("3. Persamaan Regresi Linear:\n")
cat("   Y =", round(a_2, 3), "+ (", round(b_2, 3), ") X\n")
cat("   (Y = IPK, X = Jarak dalam Km)\n")



#Model 3 : Pengaruh Akses Internet (Wifi Pribadi) terhadap IPK
#Uji Korelasi (Point-Biserial)
uji_korelasi <- cor.test(df_final$Wifi_Pribadi, df_final$IPK)
nilai_r <- uji_korelasi$estimate
print("Hasil uji korelasi :")
print(uji_korelasi)

#Analisis Regresi
model_regresi <- lm(IPK ~ Wifi_Pribadi, data = df_final)
print("Model regresi :")
summary(model_regresi)
ringkasan_model <- summary(model_regresi)

#Mengambil Koefisien (Intercept dan Slope)
a <- coef(model_regresi)[1] 
b <- coef(model_regresi)[2] 
r_squared <- ringkasan_model$r.squared

#Visualisasi menggunakan boxplot dengan Jitter
library(ggplot2)
library(ggcorrplot)

df_viz <- df_final %>%
  mutate(Wifi_Label = factor(Wifi_Pribadi, labels = c("Tidak (0)", "Ya (1)")))

ggplot(df_viz, aes(x = Wifi_Label, y = IPK)) +
  geom_boxplot(aes(fill = Wifi_Label), alpha = 0.3, outlier.shape = NA) +
  
  geom_jitter(aes(color = Wifi_Label), width = 0.2, size = 2, alpha = 0.6) +
  
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, color = "darkred") +
  
  scale_fill_manual(values = c("grey70", "skyblue")) +
  scale_color_manual(values = c("grey40", "darkblue")) +
  
  labs(
    title = "PENGARUH AKSES INTERNET TERHADAP IPK",
    x = "Kepemilikan Wifi Pribadi",
    y = "Indeks Prestasi Kumulatif (IPK) Terakhir",
    fill = "Status", color = "Status"
  ) +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5))

#Hasil
cat("Hasil analisis regresi dan korelasi\n")
cat("1. Nilai Korelasi (r):", round(nilai_r, 4), "\n")
cat("   (Interpretasi: Kekuatan hubungan antara variabel X dan Y)\n\n")

cat("2. Koefisien Determinasi (R^2):", round(r_squared, 4), "\n")
cat("   (Interpretasi: Persentase pengaruh X terhadap Y)\n\n")

cat("3. Persamaan Regresi Linear:\n")
cat("   Y =", round(a, 3), "+", round(b, 3), "X\n")
cat("   (X = 1 jika punya Wifi Pribadi, X = 0 jika tidak)\n")



#Model 4 : Analisis regresi berganda
model_berganda <- lm(IPK ~ Pendapatan_Juta + Jarak + Wifi_Pribadi, 
                     data = df_final %>% 
                       mutate(Pendapatan_Juta = Pendapatan_Ortu/1000000))

print("Hasil regresi berganda")
summary(model_berganda)

ringkasan_final <- summary(model_berganda)
adj_r_squared <- ringkasan_final$adj.r.squared
f_statistic <- ringkasan_final$fstatistic
p_value_global <- pf(f_statistic[1], f_statistic[2], f_statistic[3], lower.tail = FALSE)
koefisien <- coef(model_berganda)

df_korelasi <- df_final %>%
  transmute( 
    IPK = IPK,
    Pendapatan = Pendapatan_Ortu,
    Jarak = Jarak,
    Wifi = Wifi_Pribadi
  )

#Visualisasi menggunakan heatmap
matriks_kor <- cor(df_korelasi, use = "complete.obs") 
ggcorrplot(matriks_kor, 
           method = "square",      
           type = "lower",         
           lab = TRUE,              
           lab_size = 3,           
           colors = c("red", "white", "blue"), 
           title = "HEATMAP KORELASI ANTAR VARIABEL/FAKTOR",
           ggtheme = ggplot2::theme_minimal) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

cat("\nHasil Analisis Regresi Berganda : \n")

cat("1. Kebaikan Model (Adjusted R-squared):\n")
cat("   Nilai :", round(adj_r_squared, 5), "\n")
cat("   (Interpretasi: Variabel independen menjelaskan", round(adj_r_squared * 100, 2), "% variasi IPK)\n\n")

cat("2. Uji Simultan (F-Test):\n")
cat("   P-value :", format.pval(p_value_global, digits = 4), "\n")
if(p_value_global < 0.05) {
  cat("   (Interpretasi: Model SIGNIFIKAN. Minimal ada satu variabel berpengaruh.)\n\n")
} else {
  cat("   (Interpretasi: Model TIDAK SIGNIFIKAN secara statistik.)\n\n")
}

cat("3. Persamaan Regresi Berganda:\n")
cat("   Y (IPK) =", round(koefisien["(Intercept)"], 3), "\n")
cat("            + (", round(koefisien["Pendapatan_Juta"], 4), ") * Pendapatan_Juta\n")
cat("            + (", round(koefisien["Jarak"], 4), ") * Jarak\n")
cat("            + (", round(koefisien["Wifi_Pribadi"], 4), ") * Wifi_Pribadi\n\n")

cat("4. Rangkuman Variabel Parsial (Yang Signifikan P < 0.05):\n")
coef_table <- as.data.frame(ringkasan_final$coefficients)
sig_vars <- subset(coef_table, `Pr(>|t|)` < 0.05 & rownames(coef_table) != "(Intercept)")

if(nrow(sig_vars) > 0) {
  print(sig_vars)
  cat("\n   -> Variabel di atas berpengaruh signifikan terhadap IPK.\n")
} else {
  cat("   -> Tidak ada variabel independen (X) yang signifikan mempengaruhi IPK.\n")
}

