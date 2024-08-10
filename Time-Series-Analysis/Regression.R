---
title: "Regresi"
author: "Fadly Mochammad Taufiq"
date: "2023-09-05"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# **Persiapan Libraries**

```{r}
library("rio")
library(dplyr)
library(TTR)
library(forecast)
library(lmtest) #digunakan untuk uji formal pendeteksian autokorelasi
library(orcutt) #untuk membuat model regresi Cochrane-Orcutt
library(HoRM) #untuk membuat model regresi Hildreth-Lu
```

# Persiapan Data

```{r}
input <- import("https://raw.githubusercontent.com/davinarachmadyanti/mpdw/main/Pertemuan%202/Wine%20Data.csv")
input

data = input[input$County=="Kings",]
data = cbind(data$Year,data$Production,data$HarvestedAcres)
data = as.data.frame(data)
colnames(data) = c("Year","Production","HarvestedAcres")
data

```

# **Eksplorasi Data**

Akan diperlihatkan plot time-series dari area panen (dalam acre) dan total produksi (dalam ton) Wine County Kings Periode 1980-2020

```{r}
#Membentuk objek time series
data_production.ts<-ts(data$Production)
data_production.ts

data_harvested.ts<-ts(data$HarvestedAcres)
data_harvested.ts

#Membuat plot time series
ts.plot(data_production.ts, xlab="Time Period ", ylab="Total Production", main= "Time Series Plot of Total Production")
points(data_production.ts)

ts.plot(data_harvested.ts, xlab="Time Period ", ylab="HarvestedAcres", main= "Time Series Plot of Harvested Acres")
points(data_harvested.ts)

#Eksplorasi Data
#Pembuatan Scatter Plot
plot(data$Year,data$Production, pch = 20, col = "blue",
     main = "Scatter Plot Tahun vs Total Produksi (ton)",
     xlab = "Tahun",
     ylab = "Total Produksi")

plot(data$Year,data$HarvestedAcres, pch = 20, col = "red",
     main = "Scatter Plot Tahun vs Total Area Panen (acre)",
     xlab = "Tahun",
     ylab = "Total Area Panen")

#Menampilkan Nilai Korelasi
cor(data$Year,data$Production)
cor(data$Year,data$HarvestedAcres)

```

# **Regresi**

```{r}
#Pembuatan Model Regresi
#model regresi
model<- lm(Production~Year+HarvestedAcres, data = data)
summary(model)

```

Model yang dihasilkan adalah

$$y_i=-(2.14e+06)+1082x_1+3.825x_2$$

Berdasarkan ringkasan model dapat diketahui bahwa hasil uji F memiliki *p-value* \< $\alpha$ (5%).

Artinya, minimal terdapat satu variabel yang berpengaruh nyata terhadap model. Begitu juga dengan hasil uji-t parsial kedua parameter regresi, yaitu intersep dan koefisien regresi 1 dan 2 yang menunjukkan hal yang sama, yaitu memiliki p-value \< $\alpha$ (5%) sehingga baik peubah Year (tahun) maupun HarvestedAcres (area panen) memiliki pengaruh yang nyata terhadap peubah Production (produksi) dalam taraf 5%.

Selanjutnya dapat dilihat juga nilai $R^2=0.9105$. Artinya, sebesar 91.05% keragaman Production (produksi) dapat dijelaskan oleh peubah Year (tahun) dan HarvestedAcres (luas area panen).

Selanjutnya kita perlu melakukan uji terhadap sisaannya untuk melihat kondisi normalitas dan indikasi autokorelasi

```{r}

#sisaan dan fitted value
sisaan<- residuals(model)
fitValue<- predict(model)

#Diagnostik dengan eksploratif
par(mfrow = c(2,2))
qqnorm(sisaan)
qqline(sisaan, col = "steelblue", lwd = 2)
plot(fitValue, sisaan, col = "steelblue", pch = 20, xlab = "Sisaan", ylab = "Fitted Values", main = "Sisaan vs Fitted Values")
abline(a = 0, b = 0, lwd = 2)
hist(sisaan, col = "steelblue")
plot(seq(1,41,1), sisaan, col = "steelblue", pch = 20, xlab = "Sisaan", ylab = "Order", main = "Sisaan vs Order")
lines(seq(1,41,1), sisaan, col = "red")
abline(a = 0, b = 0, lwd = 2)

```

Dua plot di samping kiri digunakan untuk melihat apakah sisaan menyebar normal. Normal Q-Q Plot di atas menunjukkan bahwa sisaan cenderung tidak mengikuti sebaran normal, begitu juga dengan histogram dari sisaan.

Selanjutnya, dua plot di samping kanan digunakan untuk melihat autokorelasi. Plot Sisaan vs Fitted Value dan Plot Sisaan vs Order menunjukkan adanya pola pada sisaan, hal ini mengindikasikan adanya autokorelasi.

Untuk lebih lanjut akan digunakan uji formal melihat normalitas sisaan dan plot ACF dan PACF untuk melihat apakah ada autokorelasi atau tidak.

```{r}

```

## Uji Normalitas

Uji ini dilakukan untuk melihat apakah sisaan menyebar normal atau tidak. Kali ini uji yang digunakan adalah *Shapiro Test* dan *Kolmogorov-Smirnov Test*

```{r}
#Melihat Sisaan Menyebar Normal/Tidak
#H0: sisaan mengikuti sebaran normal
#H1: sisaan tidak mengikuti sebaran normal
shapiro.test(sisaan)
ks.test(sisaan, "pnorm", mean=mean(sisaan), sd=sd(sisaan))
```

Berdasarkan uji formal Saphiro-Wilk dan Kolmogorov-Smirnov didapatkan nilai *p-value* \> $\alpha$ (5%), sehingga keputusannya adalah terima H0. Artinya, cukup bukti untuk menyatakan sisaan berdistribusi normal.

## Uji Autokorelasi

```{r}
#ACF dan PACF identifikasi autokorelasi
par(mfrow = c(1,2))
acf(sisaan)
pacf(sisaan)
```

Berdasarkan plot ACF dan PACF, terlihat terdapat garis vertikal yang melebihi rentang batas (garis biru), hal ini mengindikasikan adanya autokorelasi. Namun, untuk lebih memastikan akan dilakukan uji formal dengan uji *Durbin Watson.*

```{r}
#Deteksi autokorelasi dengan uji-Durbin Watson
#H0: tidak ada autokorelasi
#H1: ada autokorelasi
dwtest(model)
```

Berdasarkan hasil DW Test, didapatkan nilai $DW = 1.3137$ dan p-value = $0.004819$. Berdasarkan tabel Durbin-Watson diperoleh nilai $DL = 1.3992$ dan $DU = 1.6031$. Nilai DW yang lebih kecil dari DL mengindikasikan terjadinya autokorelasi.

Hal ini didukung juga oleh nilai *p-value* \< 0.05 dapat disimpulkan bahwa tolak H0, artinya cukup bukti mengatakan adanya autokorelasi. Oleh karena itu, diperlukan penangan autokorelasi. Penanganan yang akan digunakan menggunakan dua metode, yaitu *Cochrane-Orcutt* dan *Hildret-Lu*.

### Penanganan Autokorelasi

#### Metode Cochrance-Orcutt 

Penanganan metode *Cochrane-Orcutt* dapat dilakukan dengan bantuan packages Orcutt pada aplikasi R maupun secara manual. Berikut ini ditampilkan cara menggunakan bantuan library packages Orcutt.

```{r}
#Penanganan Autokorelasi Cochrane-Orcutt
modelCO<-cochrane.orcutt(model)
modelCO
```

Hasil keluaran model setelah dilakukan penanganan adalah sebagai berikut.

$$y_i=-(2.12e+06)+1067.31x_1+4.38x_2$$

Hasil juga menunjukkan bahwa nilai DW dan *p-value* meningkat menjadi $1.9019$ dan $0.2985$. Nilai DW sudah berada pada rentang DU \< DW \< 4-DU atau $1.3992 < DW < 2.3969$. Hal tersebut juga didukung oleh nilai *p-value* \> 0.05, artinya belum cukup bukti menyatakan bahwa sisaan terdapat autokorelasi pada taraf nyata 5%.

Untuk nilai $ρ ̂$ optimum yang digunakan adalah $0.339488$. Nilai tersebut dapat diketahui dengan syntax berikut

```{r}
#Rho optimum
rho<- modelCO$rho
rho
```

Selanjutnya akan dilakukan transformasi secara manual dengan syntax berikut ini.

```{r}
#Transformasi Manual
Production.trans<- data$Production[-1]-data$Production[-41]*rho
Year.trans<- data$Year[-1]-data$Year[-41]*rho
HarvestedAcres.trans<- data$HarvestedAcres[-1]-data$HarvestedAcres[-41]*rho
modelCOmanual<- lm(Production.trans~Year.trans+HarvestedAcres.trans)
summary(modelCOmanual)
```

Hasil model transformasi bukan merupakan model sesungguhnya. Koefisien regresi masih perlu dicari kembali mengikuti $β_0^*=β_0+ρ ̂β_0$, $β_1^*=β_1$, dan $β_2^*=β_2$.

```{r}
#Mencari Penduga Koefisien Regresi setelah Transformasi ke Persamaan Awal
modelCOmanual$coefficients
b0bintang <- modelCOmanual$coefficients[1]
b0 <- b0bintang/(1-rho)
b1 <- modelCOmanual$coefficients[2]
b2 <- modelCOmanual$coefficients[3]
b0
b1
b2
```

Hasil perhitungan koefisien regresi tersebut akan menghasilkan hasil yang sama dengan model yang dihasilkan menggunakan packages.

#### Metode Hildreth-Lu

Penanganan kedua adalah menggunakan metode Hildreth-Lu. Metode ini akan mencari nilai SSE terkecil dan dapat dicari secara manual maupun menggunakan packages. Jika menggunakan packages, gunakan library packages HORM.

```{r}
#Penanganan Autokorelasi Hildreth lu
# Hildreth-Lu
hildreth.lu.func<- function(r, model){
  x <- model.matrix(model)[,-1]
  y <- model.response(model.frame(model))
  n <- length(y)
  t <- 2:n
  y <- y[t]-r*y[t-1]
  x <- x[t]-r*x[t-1]
  
  return(lm(y~x))
}

#Pencariab rho yang meminimumkan SSE
r <- c(seq(0.1,0.9, by= 0.1))
tab <- data.frame("rho" = r, "SSE" = sapply(r, function(i){deviance(hildreth.lu.func(i, model))}))
round(tab, 4)
```

Pertama-tama akan dicari di mana kira-kira $ρ$ yang menghasilkan SSE minimum. Pada hasil di atas terlihat $ρ$ minimum ketika 0.3. Namun, hasil tersebut masih kurang teliti sehingga akan dicari kembali $ρ$ yang lebih optimum dengan ketelitian yang lebih. Jika sebelumnya jarak antar $ρ$ yang dicari adalah 0.1, kali ini jarak antar $ρ$ adalah 0.001 dan dilakukan pada selang 0.2 sampai dengan 0.5.

```{r}
#Rho optimal di sekitar 0.4
rOpt <- seq(0.2,0.5, by= 0.001)
tabOpt <- data.frame("rho" = rOpt, "SSE" = sapply(rOpt, function(i){deviance(hildreth.lu.func(i, model))}))
head(tabOpt[order(tabOpt$SSE),])

#Grafik SSE optimum
par(mfrow = c(1,1))
plot(tab$SSE ~ tab$rho , type = "l", xlab = "Rho", ylab = "SSE")
abline(v = tabOpt[tabOpt$SSE==min(tabOpt$SSE),"rho"], lty = 2, col="red",lwd=2)
text(x=0.339488, y=1136026607, labels = "rho=0.339488", cex = 0.8)
```

Perhitungan yang dilakukan aplikasi R menunjukkan bahwa nilai $ρ$ optimum, yaitu saat SSE terkecil terdapat pada nilai $ρ=0.339488$. Hal tersebut juga ditunjukkan pada plot. Selanjutnya, model dapat didapatkan dengan mengevaluasi nilai $ρ$ ke dalam fungsi *hildreth.lu.func*, serta dilanjutkan dengan pengujian autokorelasi dengan uji Durbin-Watson. Namun, setelah pengecekan tersebut tidak lupa koefisien regresi tersebut digunakan untuk transformasi balik. Persamaan hasil transformasi itulah yang menjadi persamaan sesungguhnya.

```{r}
#Model terbaik
modelHL <- hildreth.lu.func(0.339488, model)
summary(modelHL)

#Transformasi Balik
cat("y = ", coef(modelHL)[1]/(1-0.339488), "+", coef(modelHL)[2],"x", sep = "")
```

\` Setelah dilakukan tranformasi balik, didapatkan model dengan metode Hildreth-Lu sebagai berikut.

$$y_i=-2919151+1474.819x_t$$

```{r}
#Deteksi autokorelasi
#H0: tidak ada autokorelasi
#H1: ada autokorelasi
dwtest(modelHL)
```

Hasil uji Durbin-Watson juga menunjukkan bawah nilai DW sebesar $1.9768$ berada pada selang daerah tidak ada autokorelasi, yaitu pada rentang DU \< DW \< 4-DU atau $1.6031 < DW < 2.3969$.

Hal tersebut juga didukung oleh *p-value* sebesar $0.4036$, di mana *p-value* \> $\alpha$=5%. Artinya tak tolak $H_0$ atau belum cukup bukti menyatakan bahwa ada autokorelasi dalam data nilai Produksi dengan metode Hildreth-Lu pada taraf nyata 5%.

Terakhir, akan dibandingkan nilai SSE dari ketiga metode (metode awal, metode Cochrane-Orcutt, dan Hildreth-Lu).

```{r}
#Perbandingan
sseModelawal <- anova(model)$`Sum Sq`[1]
sseModelCO <- anova(modelCOmanual)$`Sum Sq`[1]
sseModelHL <- anova(modelHL)$`Sum Sq`[1]
mseModelawal <- sseModelawal/length(data$Production)
mseModelCO <- sseModelCO/length(data$Production)
mseModelHL <- sseModelHL/length(data$Production)
akurasi <- matrix(c(sseModelawal,sseModelCO,sseModelHL,
                    mseModelawal,mseModelCO,mseModelHL),nrow=2,ncol=3,byrow = T)
colnames(akurasi) <- c("Model Awal", "Model Cochrane-Orcutt", "Model Hildreth-Lu")
row.names(akurasi) <- c("SSE","MSE")
akurasi
```

Berdasarkan hasil tersebut dapat diketahui bahwa hasil penanganan autokorelasi dengan metode *Cochrane-Orcutt* dan *Hildreth-Lu* memiliki SSE berturut-turut, sebesar $  5057849550$ dan $507848894$ yang mana lebih kecil sehingga lebih baik dibandingkan model awal ketika autokorelasi masih terjadi, yaitu sebesar $11837263935$.

# Simpulan

Autokorelasi yang terdapat pada data Production terjadi akibat adanya korelasi di antara unsur penyusunnya. Peubah Production yang erat hubungannya dengan perkebunan sangat rawan menjadi penyebab adanya autokorelasi. Adanya autokorelasi menyebabkan model regresi kurang baik karena akan meingkatkan galatnya. Autokorelasi dapat dideteksi secara eksploratif melalui plot sisaan, ACF, dan PACF, serta dengan uji formal Durbin-Watson. Namun, autokorelasi tersebut dapat ditangani dengan metode Cochrane-Orcutt dan Hildreth-Lu. Kedua metode menghasilkan nilai SSE yang sama, artinya keduanya baik untuk digunakan.

```{r}

```
