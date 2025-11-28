# ADS_6_RB - Analisis Regresi Dan Korelasi Pengaruh Faktor Sosial - Ekonomi Terhadap IPK Mahasiswa Sains Data ITERA

## Daftar Isi
- [Struktur Repository](#struktur-repository)
- [Cara Menjalankan Script](#cara-menjalankan-script)
- [Paket R yang Digunakan](#paket-r-yang-digunakan)
- [Penjelasan Dataset](#penjelasan-dataset)


---

## Struktur Repository

```text
ADS_6_RB/
├── Dataset_Tugas_Besar_ADS.xlsx  
├── Output_6_RB.pdf                 
├── Poster_6_RB.pdf                 
├── README.md
└── codeR_6_RB.R
```


---

## Cara Menjalankan Script

```text
1. RStudio sudah terinstal
2. R versi 4.0 atau lebih baru
Langkah-langkah :
1. Buka file codeR_6_RB.R menggunakan RStudio.
2. Instal paket yang dibutuhkan (jika belum terinstal):
3. Pastikan dataset tersedia di folder kerja (Working Directory) dengan nama Dataset_Tugas_Besar_ADS.csv.
4. Jalankan analisis dengan memblok semua kode (Ctrl+A) lalu klik Run (Ctrl+Enter).
```

---

## Paket R yang Digunakan

```text
tidyverse = Paket utama yang memuat readr (baca data), dplyr (manipulasi), dan ggplot2 (visualisasi)
readr = Membaca file CSV (read_csv2) dengan delimiter titik koma
dplyr = Pembersihan data, seleksi kolom (select), pembuatan variabel baru (mutate), dan filtering
ggplot2 = Membuat visualisasi Scatter Plot dan Boxplot
ggcorrplot = Membuat visualisasi Heatmap matriks korelasi
```

## Penjelasan Dataset

```text
Dataset berisi data survei mahasiswa mengenai faktor sosial-ekonomi dan geografis
untuk melihat pengaruhnya terhadap prestasi akademik (IPK).
IPK	= Indeks Prestasi Kumulatif terakhir mahasiswa (Variabel Dependen / Y)
Pendapatan_Ortu	= Median pendapatan orang tua dalam Rupiah (Variabel Independen / X1)
Jarak	= Jarak tempat tinggal ke kampus dalam Km (Variabel Independen / X2)
Wifi_Pribadi	= Status kepemilikan akses internet pribadi (Variabel Independen / X3, Dummy: 0/1)
```

Kelompok 6
1. Bryan Paskah Telaumbanua (124450003)
2. Haikal Seventino Tamba (124450012)
3. Yollanda Agustina (124450024)
4. Daffa Kharisma Adzana (124450061)
```
