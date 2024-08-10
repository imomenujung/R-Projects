---
title: "Tugas Pembuktian MSR"
author: "Fadly Mochammad Taufiq"
date: "2024-03-01"
output:
  word_document: default
  html_document:
    df_print: paged
  pdf_document: default
---

# Penerapan Simulasi untuk Pembuktian Sifat Statistik

![](/home/faggluyy/Documents/Kuliah/Semester 6/MSR/Tugas/SS an/penduga bias.png)

## Simulasi Rataan, ragam dan Standar Deviasi
```{r}
# Menentukan parameter populasi
pop_mean <- 50
pop_sd <- 10
pop_var <- 100
sample_size <- 30
num_samples <- 1000

# Inisialisasi vektor untuk menyimpan hasil simulasi
sample_means <- numeric(num_samples)
sample_variances <- numeric(num_samples)
sample_sds <- numeric(num_samples)

# Melakukan simulasi
set.seed(77) 

for (i in 1:num_samples) {
  # Mengambil sampel dari populasi
  sample_data <- rnorm(sample_size, mean = pop_mean, sd = pop_sd)
  
  # Menghitung rata-rata, ragam, dan standar deviasi sampel
  sample_means[i] <- mean(sample_data)
  sample_variances[i] <- var(sample_data)
  sample_sds[i] <- sd(sample_data)
}
sample_mean = mean(sample_means)
sample_sd = mean(sample_sds)
sample_var = mean(sample_variances)
# Menampilkan hasil simulasi
cat("Penduga Rata-rata Populasi (rataan sampel):", sample_mean, "\n")
cat("Penduga Ragam Populasi (ragam sampel):", sample_var, "\n")
cat("Penduga Standar Deviasi Populasi (standar deviasi sampel):", sample_sd, "\n")
```
# Perbandingan 
```{r}
perbandingan <- data.frame(
  Populasi = c(pop_mean,pop_sd,pop_var),
  Sampel = c(sample_mean,sample_sd,sample_var))
row.names(perbandingan) <- c("Rataan","SD","Ragam")
perbandingan
```
Berdasarkan hasil simulasi diketahui bahwa baik rataan, standar deviasi, maupun ragam merupakan penduga bias dari populasinya. Hal ini terlihat dari nilai2 dari hasil simulasi yang mendekati nilai populasi.


# Pembuktian Dalil Limit Pusat

![](/home/faggluyy/Documents/Kuliah/Semester 6/MSR/Tugas/SS an/Dalil limit pusat.png)
## simulasi contoh acak berukuran n = 10
```{r}
y1<-rnorm(1000000)
k<-1000
n<-10
z11<-matrix(sample(y1,n*k),k)
z11<-apply(z11,1,mean)
hist(z11,xlim = c(-1, 1),main = "n = 10", xlab = "Values", col = "lightblue", border = "black")
```

## simulasi contoh acak berukuran n = 100
```{r}
y2<-rnorm(1000000)
k<-1000
n<-100
z21<-matrix(sample(y2,n*k),k)
z21<-apply(z21,1,mean)
hist(z21,xlim = c(-1, 1),main = "n = 100", xlab = "Values", col = "lightblue", border = "black")
```

## simulasi contoh acak berukuran n = 1000
```{r}
y3<-rnorm(1000000)
k<-1000
n<-1000
z31<-matrix(sample(y3,n*k),k)
z31<-apply(z31,1,mean)
hist(z31,xlim = c(-1, 1),main = "n = 1000", xlab = "Values", col = "lightblue", border = "black")
```
## Perbandingan Hasil Simulasi
```{r}
par(mfrow = c(2, 2))
hist(z11,xlim = c(-1, 1),main = "n = 10", xlab = "Values", col = "lightblue", border = "black")
hist(z21,xlim = c(-1, 1),main = "n = 100", xlab = "Values", col = "lightblue", border = "black")
hist(z31,xlim = c(-1, 1),main = "n = 1000", xlab = "Values", col = "lightblue", border = "black")
```

Dari hasil simulasi terbukti bahwa semakin berukuran besar ukuran n maka rata-rata contoh
akan memiliki sebaran yang mendekati normal dilihat dari histogram yang semakin menyempit ketika n nya semakin besar