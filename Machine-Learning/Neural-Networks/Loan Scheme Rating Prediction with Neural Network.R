---
title: "Tugas Individu TPM"
author: "Fadly Mochammad Taufiq"
date: "2024-02-20"
output:
  html_document: default
  word_document: default
  pdf_document: default
geometry: margin=2cm
---

# Prediksi Rating Skema Pinjaman dengan Neural Network

```{r}
library(tidyverse) 
library(neuralnet) #Untuk membangkitkan fungsi Neural Networks
library(GGally) #Sama seperti GGplot namun memiliki fitur lebih luas
```

## Dataset
Data berisi informasi yang berasal dari Suatu perusahaan perbankan
dengan 75 amatan. Terdiri dari 6 peubah, dimana 1 peubah yaitu rating
akan dijadikan sebagai peubah respon (Y) dan sisanya sebagai peubah
penjelas (X) dengan rincian sebagai berikut : X1 = Besar pinjaman (dalam
juta rupiah) X2 = Lama pembayaran (dalam tahun) X2 = Tambahan bunga yang
ditetapkan (dalam %) X3 = Pembayaran per bulan (dalam 10000) X4 = Banyak
cash back yang diterapkan pada skema tersebut Y = Rating/penilaian dari
Customer

```{r}
df <- read.csv("https://raw.githubusercontent.com/imomenujung/Coba/main/data%20ann.csv")
colnames(df) <- c("X1","X2","X3","X4","X5","Y")
head(df,20)
```

## Eksplorasi Data

### Boxplot
```{r}
boxplot(df, horizontal = FALSE)
summary(df)
```

Dari Boxplot terlihat bahwa peubah memiliki nilai yang bervariasi
maka dari itu akan dilakukan normalisasi data. Normalisasi dilakukan untuk membuat data lebih homogen dan membantu mempercepat proses pelatihan model.

## Normalisasi Data
Metode normalisasi yang digunakan adalah Min-Max Scaling. Cara kerjanya
setiap nilai pada sebuah fitur dikurangi dengan nilai minimum fitur
tersebut, kemudian dibagi dengan rentang nilai atau nilai maksimum
dikurangi nilai minimum dari fitur tersebut. Cara ini akan menghasilkan
nilai baru hasil normalisasi antara 0 sampai 1.

$\displaystyle \frac{x_{i}-x_{min}}{x_{max}-x_{min}}$

```{r}
# Min-Max Scaling
# Fungsi Min-Max
scale01 <- function(x){
  (x - min(x)) / (max(x) - min(x))
}
# Normalisasi Data
df_MM <- df %>%
  mutate_all(scale01)
```

### Eksplorasi Data setelah Normalisasi
```{r}
boxplot(df_MM, horizontal = FALSE)
summary(df_MM)
```
Setelah dilakukan normalisasi data menjadi tidak terlalu bervariasi

## Pemodelan Artificial Neural Network (ANN)
Neural Network (Jaringan saraf) adalah model matematika yang menggunakan algoritma pembelajaran yang terinspirasi dari fungsi otak dalam menyimpan informasi. Karena jaringan saraf digunakan dalam mesin, mereka secara kolektif disebut "artificial neural network (jaringan saraf buatan)”.

### Default (Menggunakan Fungsi Aktivasi Logistik, Hidden Unit = 1 size = 1)
Secara default Package neuralnet memiliki pamareter sebagai berikut:
![neuralnet mode default](/home/faggluyy/Documents/Kuliah/Semester 6/TPM/Tugas/Default nn R.png)

#### Pelatihan Model
```{r}
set.seed(100)
df_NN1 <- neuralnet(Y ~ X1 + X2 + X3 + X4 + X5, data = df_MM)
plot(df_NN1, rep = 'best')
```

#### Hasil Prediksi dengan skala min-max scaling
```{r}
df_NN1$net.result
```

#### Hasil prediksi dengan Skala nilai asli
```{r}
# Your normalized array
normalized_array <- as.numeric(unlist(df_NN1$net.result))

# Original min and max values (replace with your actual values)
min_orig <- min(df$Y)
max_orig <- max(df$Y)

# Calculate the original values
original_array <- (normalized_array * (max_orig - min_orig)) + min_orig

# Print the original array
print(original_array)
```

```{r}
RMSE1 <- sqrt(mean((df$Y - original_array)^2))
RMSE1
```

### Menggunakan Fungsi Aktivasi Tanh
Dilakukan beberapa pengujian parameter pada model Neural Network. Berikut adalah hasil model terbaik yang didapatkan setelah beberapa kali tuning. Dengan nilai perubahan parameternya antara lain Hidden Unit = 3 size(5,2,1) activation = tanh

```{r}
set.seed(77)
df_NN2 <- neuralnet(Y ~ X1 + X2 + X3 + X4 + X5, data = df_MM,hidden = c(5,2,1),act.fct = "tanh")
plot(df_NN2, rep = 'best')
```

#### Hasil prediksi dengan Skala min-max scaling
```{r}
df_NN2$net.result
```

#### Hasil prediksi dengan Skala nilai asli
```{r}
# Your normalized array
normalized_array <- as.numeric(unlist(df_NN2$net.result))

# Original min and max values (replace with your actual values)
min_orig <- min(df$Y)
max_orig <- max(df$Y)

# Calculate the original values
original_array <- (normalized_array * (max_orig - min_orig)) + min_orig

# Print the original array
print(original_array)
```

```{r}
RMSE2 <- sqrt(mean((df$Y - original_array)^2))
RMSE2
```

## Perbandingan Hasil Model ANN
```{r}
perbandingan <- rbind(RMSE1,RMSE2)
rownames(perbandingan) <- c("logistik HU = 1(1)","tanH HU = 3(5,2,1)")
colnames(perbandingan) <- "RMSE"
perbandingan
```

Dari kedua model Neural Network didapatkan bahwa model dengan Aktivasi tanh hidden unit = 3(5,2,1) memiliki nilai RMSE  lebih baik dibandingkan model default neuralnet yang menggunakan aktivasi logistik dengan hidden unit = 1(1). 