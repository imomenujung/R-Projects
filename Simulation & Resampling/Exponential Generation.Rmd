---
title: "MSR tugas prak 8"
author: "Fadly"
date: "2024-04-08"
output:
  word_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Eksponensial

## Pembangkitan Sebaran Eksponensial 

Membangkitkan sebaran Eksponensial dengan ukuran contoh 2, 5, dan 25
```{r}
set.seed(77)
# Menghasilkan sampel sebaran eksponensial dengan ukuran contoh 2
sample_size_2 <- rexp(2)

# Menghasilkan sampel sebaran eksponensial dengan ukuran contoh 5
sample_size_5 <- rexp(5)

# Menghasilkan sampel sebaran eksponensial dengan ukuran contoh 25
sample_size_25 <- rexp(25)

# Tampilkan hasil
print(sample_size_2)
print(sample_size_5)
print(sample_size_25)

```

## Histogram rataan contoh sebaran eksponensial

```{r}
# Menghitung rata-rata dari setiap sampel
mean_sample_2 <- mean(sample_size_2)
mean_sample_5 <- mean(sample_size_5)
mean_sample_25 <- mean(sample_size_25)

# Membuat vektor rata-rata
means <- c(mean_sample_2, mean_sample_5, mean_sample_25)

# Membuat histogram dari rata-rata contoh
hist(means, main = "Histogram Rataan Contoh", xlab = "Mean", ylab = "Frequency")
```

## Normal Q-Q plot
Membuat normal QQ plot untuk masing-masing sampel
```{r}
par(mfrow=c(1,3)) # Mengatur layout untuk tiga plot secara horizontal
qqnorm(sample_size_2, main = "QQ Plot - n=2")
qqline(sample_size_2)

qqnorm(sample_size_5, main = "QQ Plot - n=5")
qqline(sample_size_5)

qqnorm(sample_size_25, main = "QQ Plot - n=25")
qqline(sample_size_25)
```

# Pembangkitan dari dua gugus data

Dilakukan pembangkitan dari dua gugus data. Data pertama adalah data yang menyebar normal. Sementara data kedua adalah data campuran yang terdiri dari 50% data menyebar normal dan 50% data menyebar chisquare dengan nilai df = 3 (di r nilai df harus dideskripsikan disini kita mengambil acak angka 3).

## ukuran contoh = 4

### Data Sebaran Normal saja
```{r}
# Load library
library(dplyr)

# Seed untuk reproducibility
set.seed(77)

# Ukuran sampel
n <- 4

# Membuat gugus data dari distribusi normal
normal_data_4 <- rnorm(n)
normal_data_4
```

### Data Campuran 
```{r}
set.seed(77)
# Membuat gugus data dari distribusi campuran
mixed_data_4 <- c(rnorm(n/2), rchisq(n/2, df = 3))
mixed_data_4
```

## ukuran contoh = 12

### Data Sebaran Normal saja
```{r}
# Load library
library(dplyr)

# Seed untuk reproducibility
set.seed(77)

# Ukuran sampel
n <- 12

# Membuat gugus data dari distribusi normal
normal_data_12 <- rnorm(n)
normal_data_12
```

### Data Campuran 
```{r}
set.seed(77)
# Membuat gugus data dari distribusi campuran
mixed_data_12 <- c(rnorm(n/2), rchisq(n/2, df = 3))
mixed_data_12
```

## ukuran contoh = 20

### Data Sebaran Normal saja
```{r}
# Load library
library(dplyr)

# Seed untuk reproducibility
set.seed(77)

# Ukuran sampel
n <- 20

# Membuat gugus data dari distribusi normal
normal_data_20 <- rnorm(n)
normal_data_20
```

### Data Campuran 
```{r}
set.seed(77)
# Membuat gugus data dari distribusi campuran
mixed_data_20 <- c(rnorm(n/2), rchisq(n/2, df = 3))
mixed_data_20
```

## ukuran contoh = 60

### Data Sebaran Normal saja
```{r}
# Load library
library(dplyr)

# Seed untuk reproducibility
set.seed(77)

# Ukuran sampel
n <- 60

# Membuat gugus data dari distribusi normal
normal_data_60 <- rnorm(n)
normal_data_60
```

### Data Campuran 
```{r}
set.seed(77)
# Membuat gugus data dari distribusi campuran
mixed_data_60 <- c(rnorm(n/2), rchisq(n/2, df = 3))
mixed_data_60
```

## ukuran contoh = 100

### Data Sebaran Normal saja
```{r}
# Load library
library(dplyr)

# Seed untuk reproducibility
set.seed(77)

# Ukuran sampel
n <- 100

# Membuat gugus data dari distribusi normal
normal_data_100 <- rnorm(n)
normal_data_100
```

### Data Campuran 

```{r}
set.seed(77)
# Membuat gugus data dari distribusi campuran
mixed_data_100 <- c(rnorm(n/2), rchisq(n/2, df = 3))
mixed_data_100
```

## Eksplorasi Data dengan Sebaran Normal

### Histogram

```{r}
par(mfrow=c(2,3)) # Mengatur layout untuk tiga plot secara horizontal
hist(normal_data_4, main = "n = 4", xlab = "Mean", ylab = "Frequency")
hist(normal_data_12, main = "n = 12", xlab = "Mean", ylab = "Frequency")
hist(normal_data_20, main = "n = 20", xlab = "Mean", ylab = "Frequency")
hist(normal_data_60, main = "n = 60", xlab = "Mean", ylab = "Frequency")
hist(normal_data_100, main = "n = 100", xlab = "Mean", ylab = "Frequency")
```

### QQ-Plot

```{r}
par(mfrow=c(2,3)) # Mengatur layout untuk tiga plot secara horizontal
qqnorm(normal_data_4, main = "QQ Plot - n=4")
qqline(normal_data_4)

qqnorm(normal_data_12, main = "QQ Plot - n=12")
qqline(normal_data_12)

qqnorm(normal_data_20, main = "QQ Plot - n=20")
qqline(normal_data_20)

qqnorm(normal_data_60, main = "QQ Plot - n=60")
qqline(normal_data_60)

qqnorm(normal_data_100, main = "QQ Plot - n=100")
qqline(normal_data_100)
```
### Interpretasi

Berdasarkan Hasil dari visualisasi melalui Histogram dan QQ Plot diketahui bahwa data dengan sebaran normal terlihat mulai simetris (mendekati sebaran normal) pada saat nilai n = 60.

## Eksplorasi Data dengan Sebaran Campuran
### Histogram

```{r}
par(mfrow=c(2,3)) # Mengatur layout untuk tiga plot secara horizontal
hist(mixed_data_4, main = "n = 4", xlab = "Mean", ylab = "Frequency")
hist(mixed_data_12, main = "n = 12", xlab = "Mean", ylab = "Frequency")
hist(mixed_data_20, main = "n = 20", xlab = "Mean", ylab = "Frequency")
hist(mixed_data_60, main = "n = 60", xlab = "Mean", ylab = "Frequency")
hist(mixed_data_100, main = "n = 100", xlab = "Mean", ylab = "Frequency")
```

### QQ-Plot 

```{r}
par(mfrow=c(2,3)) # Mengatur layout untuk tiga plot secara horizontal
qqnorm(mixed_data_4, main = "QQ Plot - n=4")
qqline(mixed_data_4)

qqnorm(mixed_data_12, main = "QQ Plot - n=12")
qqline(mixed_data_12)

qqnorm(mixed_data_20, main = "QQ Plot - n=20")
qqline(mixed_data_20)

qqnorm(mixed_data_60, main = "QQ Plot - n=60")
qqline(mixed_data_60)

qqnorm(mixed_data_100, main = "QQ Plot - n=100")
qqline(mixed_data_100)
```

### Interpretasi

Berdasarkan Hasil dari visualisasi melalui Histogram dan QQ Plot diketahui bahwa data dengan sebaran Campuran (50% data menyebar normal dan 50% data menyebar chisquare df = 3) belum bisa dikatakan simetris (mendekati sebaran normal) pada rentang nilai n = 4-100.
