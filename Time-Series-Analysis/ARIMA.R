---
title: "ARIMA"
author: "Fadly Mochammad Taufiq"
date: "2023-09-21"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# MA(2)

## Membangkitkan Model MA(2)

dengan $\theta = 0.4$ dan $\theta = 0.6$ sebanyak 300 observasi dan $c=0$. Karena diperlukan satu nilai awal untuk $e_{t-1}$, masukkan nilai pertama white noise sebagai nilai awal tersebut.

### White Noise

```{r}
wn <- rnorm(300)
ts.plot(wn)
```

```{r}
set.seed(1024)
ma <- wn[c(1,2)]

```

### Manual

```{r}
for(i in 3:300){
   ma[i] <- wn[i] + 0.4 * wn[i - 1] + 0.6 * wn[i - 2]
}
ma
```

### arima.sim

```{r}
set.seed(1024)
ma2 <- arima.sim(list(order=c(0,0,2), ma=c(0.4,0.6)), n=300)
ma2
```

## Membuat Plot

### Plot Time Series

```{r}
ts.plot(ma)
```

Berdasarkan plot time series stasioner pada nilai rataan

### Plot ACF

```{r}
acf(ma,lag.max = 20)
```

Berdasarkan plot AFC tersebut, terlihat bahwa plot ACF *cuts off* di lag kedua

### Plot PACF

```{r}
pacf(ma)
```

Berdasarkan plot PACF tersebut, terlihat bahwa plot PACF cenderung *tails off* dan membentuk gelombang sinus

### Plot EACF

```{r}
TSA::eacf(ma)
```

Berdasarkan pola segitiga nol pada plot EACF, terlihat bahwa segitiga nol berada pada ordo AR(0) dan ordo MA(2)

## Membuat Scatterplot

#### Korelasi antara $Y_t$ dengan $Y_{t-1}$

```{r}
set.seed(1024)
#Yt
yt_ma <- ma[-1]
yt_ma
#Yt-1
yt_1_ma <- ma[-300]
yt_1_ma

```

### Scatterplot $Y_t$ dengan $Y_{t-1}$

```{r}
plot(y=yt_ma,x=yt_1_ma)
```

Berdasarkan scatterplot tersebut, terlihat bahwa terdapat hubungan positif antara $Y_t$ dengan $Y_{t-1}$. Hal ini sesuai dengan teori yang ada

```{r}
cor(yt_ma,yt_1_ma)
```

Korelasi antara $Y_t$ dengan $Y_{t-1}$ dari hasil simulasi yaitu 0.395

#### Korelasi antara $Y_t$ dengan $Y_{t-2}$

```{r}
set.seed(1024)
#Yt
yt_ma2 <- ma[-c(1,2)]
yt_ma2
#Yt-2
yt_2_ma <- ma[-c(299,300)]
yt_2_ma
```

### Scatterplot $Y_t$ dengan $Y_{t-2}$

```{r}
plot(y=yt_ma2,x=yt_2_ma)
```

Berdasarkan scatterplot tersebut, terlihat bahwa terdapat hubungan positif antara $Y_t$ dengan $Y_{t-2}$.

```{r}
cor(yt_ma2,yt_2_ma)
```

Korelasi antara $Y_t$ dengan $Y_{t-1}$ dari hasil simulasi yaitu 0.397

#### Korelasi antara $Y_t$ dengan $Y_{t-3}$

```{r}
set.seed(1024)
#Yt
yt_ma3 <- ma[-c(1,2,3)]
yt_ma3
#Yt-2
yt_3_ma <- ma[-c(298,299,300)]
yt_3_ma
```

### Scatterplot $Y_t$ dengan $Y_{t-3}$

```{r}
plot(y=yt_ma3,x=yt_3_ma)
```

Berdasarkan scatterplot tersebut, terlihat bahwa cenderung tidak terdapat hubungan antara $Y_t$ dengan $Y_{t-3}$.

```{r}
cor(yt_ma3,yt_3_ma)
```

Korelasi antara $Y_t$ dengan $Y_{t-3}$ hasil simulasi yaitu 0.023

```{r}

```

# AR(2)

Proses AR

Proses AR dapat dituliskan sebagai berikut:

$$ y_{t} = c + e_t + \phi_{1}Y_{t-1} + \phi_{2}Y_{t-2} + \dots + \phi_{q}Y_{t-q} = c+{e_t+\sum_{i=1}^p \phi_iY_{t-i}} $$ Terlihat bahwa $Y_t$ berperan penting dalam pembangkitan proses AR.

## Membangkitan Proses AR(2)

Akan dicoba membangkitkan proses AR paling sederhana, yaitu AR(1) dengan $\phi_1 = 0.5$ dan $\phi_1 = 0.2$ sebanyak 300 observasi dan $c=0$.

```{r}
set.seed(1024)

```

Nilai-nilai selanjutnya dapat dicari melalui *loop*. Bentuk loop dapat dilihat dari rumus AR(1) yang hendak dibangkitkan:

$$ Y_t = e_t+0.5Y_{t-1}++0.2Y_{t-2} $$

```{r}
n<-length(wn)
n
ar <- c(1:n) 
for (i in 3:n) {ar[i]<-wn[i]+0.5*ar[i-1]+0.2*ar[i-2]}
ar
```

Selain menggunakan cara di atas, pembangkitan proses AR dapat dilakukan dengan fungsi `arima.sim()` sebagai berikut.

```{r}
ar2 <- arima.sim(list(order=c(2,0,0), ar=c(0.5,0.2)), n=300)
ar2
```

Karakteristik AR(1)

### Plot Time Series

```{r}
ts.plot(ar)
```

Berdasarkan plot time series tersebut terlihat bahwa data cenderung stasioner pada rataan

### Plot ACF

```{r}
acf(ar)
```

Berdasarkan plot ACF tersebut terlihat bahwa plot ACF cenderung *tails off* dan cenderung membentuk pola grafik sinus

### Plot PACF

```{r}
pacf(ar)
```

Berdasarkan plot PACF tersebut, terlihat bahwa plot PACF *cuts off* pada lag pertama, sejalan dengan teori yang ada

### Plot EACF

```{r}
TSA::eacf(ar)
```

Berdasarkan pola segitiga nol pada plot EACF, terlihat bahwa segitiga nol berada pada ordo AR(2) dan ordo MA(0)

## Membuat Scatterplot

#### Korelasi antara $Y_t$ dengan $Y_{t-1}$

```{r}
set.seed(1024)
#Yt
yt_ar <- ar[-1]
yt_ar
#Yt-1
yt_1_ar <- ar[-300]
yt_1_ar
```

### Scatterplot $Y_t$ dengan $Y_{t-1}$

```{r}
plot(y=yt_ar,x=yt_1_ar)
```

Berdasarkan scatterplot tersebut, terlihat bahwa terdapat hubungan positif antara $Y_t$ dengan $Y_{t-1}$. Hal ini sesuai dengan teori yang ada

```{r}
cor(yt_ar,yt_1_ar)
```

Korelasi antara $Y_t$ dengan $Y_{t-1}$ dari hasil simulasi yaitu 0.6202

#### Korelasi antara $Y_t$ dengan $Y_{t-2}$

```{r}
set.seed(1024)
#Yt
yt_ar2 <- ar[-c(1,2)]
yt_ar2
#Yt-2
yt_2_ar <- ar[-c(299,300)]
yt_2_ar
```

### Scatterplot $Y_t$ dengan $Y_{t-2}$

```{r}
plot(y=yt_ar2,x=yt_2_ar)
```

Berdasarkan scatterplot tersebut, terlihat bahwa terdapat hubungan positif antara $Y_t$ dengan $Y_{t-2}$. Hal ini sesuai dengan teori yang ada

```{r}
cor(yt_ar2,yt_2_ar)
```

Korelasi antara $Y_t$ dengan $Y_{t-2}$ dari hasil simulasi yaitu 0.532

#### Korelasi antara $Y_t$ dengan $Y_{t-3}$

```{r}
set.seed(1024)
#Yt
yt_ar3 <- ar[-c(1,2,3)]
yt_ar3
#Yt-2
yt_3_ar <- ar[-c(298,299,300)]
yt_3_ar
```

### Scatterplot $Y_t$ dengan $Y_{t-3}$

```{r}
plot(y=yt_ar3,x=yt_3_ar)
```

Berdasarkan scatterplot tersebut, terlihat bahwa cenderung tidak terdapat hubungan antara $Y_t$ dengan $Y_{t-3}$.

```{r}
cor(yt_ar3,yt_3_ar)
```

Korelasi antara $Y_t$ dengan $Y_{t-3}$ hasil simulasi yaitu 0.425

# ARMA(2,2)

## Membangkitkan ARMA(2,2)

Setelah mengetahui cara membangkitkan data berpola AR, MA, dan ARMA sederhana, bagaimana cara melakukan pembangkitan data berpola tersebut yang lebih kompleks? Apakah dapat dibuat suatu fungsi yang fleksibel yang memungkinan pembangkitan dengan berapapun jumlah koefisien?

Pertama, lihat kembali bentuk umum data berpola ARMA.

$$
y_{t} = c + \sum_{i=1}^p \phi_{i}y_{t-i} + \sum_{j=1}^q e_{t-j}+ e_{t}
$$ Jika koefisien dan *white noise*/nilai deret waktu sebelumnya dapat diekstrak dalam bentuk vektor, dapat dilakukan perkalian matriks untuk mencari nilai bagian AR dan MA:

```{r}
set.seed(1024)
coefs <- c(0.4, 0.6, 0.5,0.2)
e <- c(1, 2, 3, 4)

coefs %*% e
```

Atau, dapat dilakukan perkalian *elementwise* yang dijumlahkan:

```{r}
coefs * e
sum(coefs * e)
```

Dari prinsip ini, dapat dibuat fungsi umum untuk membangkitkan data ARMA. Input dari fungsi adalah jumlah data yang hendak dibangkitkan, koefisien MA, dan koefisien AR

```{r}
arma.sim <- function(n, macoef, arcoef){
  manum <- length(macoef)
  arnum <- length(arcoef)
  stopifnot(manum < n & arnum < n)
  
  wn <- rnorm(n, sd = 0.5)
  init <- max(manum, arnum)

  arma <- wn[1:init]
  for(i in {init+1}:n){
   mastart <- i - manum
   maend <- i-1
   arstart <- i - arnum
   arend <- i-1
   arma[i] <- sum(arcoef * arma[arstart:arend]) + sum(macoef * wn[mastart:maend])  + wn[i]
   }
  return(arma)
}
```

Terlihat bahwa komponen $\sum_{i=1}^q y_{t-1}$ disimulasikan melalui `sum(arcoef * arma[arstart:arend])`. Jadi, koefisien dikalikan dengan data $y$ dari $t-q$ di mana q adalah jumlah koefisien AR, sampai data $t-1$. Lalu komponen $\sum_{j=1}^q e_{t-j}$ disimulasikan melalui `sum(macoef * wn[mastart:maend])`. Koefisien dikalikan dengan *white noise* $e$ dari $t-p$, p jumlah koefisien MA, sampai $t-1$.

```{r}
# beberapa contoh pembangkitan melalui fungsi

set.seed(1024)
ma2 <- arma.sim(300, c(0.4, 0.6), 0)
ar2 <- arma.sim(300, 0, c(0.5, 0.2))

par(mfrow = c(2, 2))
acf(ma2)
pacf(ma2)
acf(ar2)
pacf(ar2)
```

```{r}
#contoh untuk ARMA
set.seed(1024)
arma22 <- arma.sim(300, c(0.4, 0.6), c(0.5,0.2))

arma22 |> arima(c(2,0,2))
```

```{r}
set.seed(1024)
n = length(wn)
phi1 = 0.5
phi2 = 0.2
theta1 = 0.4
theta2 = 0.6

y.arma=c(2:n)
for (i in 3:n){y.arma[i] = phi1*y.arma[i-1] + phi2*y.arma[i-2] + theta1*wn[i-1]+theta2*wn[i-2]+wn[i]}
```

Pembangkitan ARMA(p,q) juga dapat dilakukan dengan fungsi `arima.sim` sebagai berikut.

```{r}
set.seed(1024)
arma22 <- arima.sim(list(order=c(2,0,2), ar = c(0.5,0.2), ma = c(0.4,0.6)), n=300)
arma22

```

## Karakteristik ARMA(2,2)

### Plot Time Series

```{r}
par(mfrow = c(1, 2))
ts.plot(y.arma)
ts.plot(arma22)
par(mfrow = c(1, 1))
```

Berdasarkan plot time series tersebut, terlihat bahwa model ARMA(2,2) cenderung stasioner dalam rataan

### Plot ACF

```{r}
par(mfrow = c(1, 2))
acf(y.arma)
acf(arma22)
par(mfrow = c(1, 1))
```

Berdasarkan plot ACF tersebut, terlihat bahwa model ARMA(2,2) hasil simulasi memiliki plot ACF yang *tails off*, sesuai dengan teori yang ada

### Plot PACF

```{r}
par(mfrow = c(1, 2))
pacf(y.arma)
pacf(arma22)
par(mfrow = c(1, 1))
```

Berdasarkan plot PACF tersebut, terlihat bahwa model ARMA(2,2) hasil simulasi memiliki plot PACF yang *tails off*, sesuai dengan teori

### Plot EACF

```{r}
TSA::eacf(y.arma)
TSA::eacf(arma22)
```

Berdasarkan pola segitiga nol pada plot EACF, terlihat bahwa segitiga nol berada pada ordo AR(1) dan ordo MA(2)

### Scatterplot Antar Lag

#### Korelasi antara $Y_t$ dengan $Y_{t-1}$

```{r}
set.seed(1024)
#Yt
yt_arma <- arma22[-1]
yt_arma
#Yt-1
yt_1_arma <- arma22[-300]
yt_1_arma
```

### Scatterplot $Y_t$ dengan $Y_{t-1}$

```{r}
plot(y=yt_arma,x=yt_1_arma)
```

Berdasarkan scatterplot tersebut, terlihat bahwa terdapat hubungan positif antara $Y_t$ dengan $Y_{t-1}$. Hal ini sesuai dengan teori yang ada

```{r}
cor(yt_arma,yt_1_arma)
```

Korelasi antara $Y_t$ dengan $Y_{t-1}$ dari hasil simulasi yaitu 0.864

#### Korelasi antara $Y_t$ dengan $Y_{t-2}$

```{r}
set.seed(1024)
#Yt
yt_arma <- arma22[-c(1,2)]
yt_arma
#Yt-1
yt_2_arma <- arma22[-c(299,300)]
yt_2_arma
```

### Scatterplot $Y_t$ dengan $Y_{t-2}$

```{r}
plot(y=yt_arma,x=yt_2_arma)
```

Berdasarkan scatterplot tersebut, terlihat bahwa terdapat hubungan positif antara $Y_t$ dengan $Y_{t-2}$. Hal ini sesuai dengan teori yang ada

```{r}
cor(yt_arma,yt_2_arma)
```

Korelasi antara $Y_t$ dengan $Y_{t-2}$ dari hasil simulasi yaitu 0.7644

#### Korelasi antara $Y_t$ dengan $Y_{t-3}$

```{r}
set.seed(1024)
#Yt
yt_arma <- arma22[-c(1,2,3)]
yt_arma
#Yt-1
yt_3_arma <- arma22[-c(298,299,300)]
yt_3_arma
```

### Scatterplot $Y_t$ dengan $Y_{t-3}$

```{r}
plot(y=yt_arma,x=yt_3_arma)
```

Berdasarkan scatterplot tersebut, terlihat bahwa terdapat hubungan positif antara $Y_t$ dengan $Y_{t-3}$. Hal ini sesuai dengan teori yang ada

```{r}
cor(yt_arma,yt_3_arma)
```

Korelasi antara $Y_t$ dengan $Y_{t-3}$ dari hasil simulasi yaitu 0.569

```{r}

```
