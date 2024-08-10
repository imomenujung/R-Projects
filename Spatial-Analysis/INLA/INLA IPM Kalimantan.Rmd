---
title: "INLA"
author: "Farik Firsteadi Haristiyanto"
date: "`r Sys.Date()`"
output:
  word_document: default
  html_document: default
---

# Library

```{r, warning=FALSE,message=FALSE}
library(raster)
library(spdep)
library(sp)
library(tidyverse)
library(ggplot2)
library(sf)
library(ggnewscale)
library(ggspatial)
library(rgdal)
library(maptools)
library(corrplot)
library(DescTools)
library(nortest)
library(car)
library(spatialreg)
library(corrplot)
library(phylin)
library(rio)
```

# Input Data

Berikut ini adalah data dengan peubah-peubah sebagai berikut:

Y = Indeks Pembangunan Manusia (%); X1 = Indeks Ketahanan Pangan (%); X2 = Infrastruktur Internet; X3 = Laju Pertumbuhan Penduduk; X4 = Produk Domestik Regional Bruto Atas Dasar Harga Berlaku (Rp 1.000.000,-); X5 = Produksi Tanaman Pangan (100 ton); serta diberikan juga peta Kalimantan.

```{r}
data <- import("https://raw.githubusercontent.com/Fhar1st/DataTugas/main/PSB/IPM%20Kalimantan.csv")
data <- data[, -c(3,9)]
colnames(data) <- c("Provinsi","y","x1","x2","x3","x4","x5")

data$x4 <- data$x4/1000000
data$x5 <- data$x5/1000

str(data)
head(data)
```

# Eksplorasi Data

## Distribusi Data Pengamatan

### Cek Missing Value

```{r}
data[!complete.cases(data),]
```

Pada data dengan 56 baris yang digunakan tidak terdapat data yang hilang.

### Summary Data

```{r}
summary(data[,-1])
```

### Boxplot

```{r}
par(mfrow=c(1,3))
boxplot(data$y,main = "Boxplot IPM")
boxplot(data$x1,main = "Boxplot IKP")
boxplot(data$x2,main = "Boxplot II")
boxplot(data$x3,main = "Boxplot LPP")
boxplot(data$x4,main = "Boxplot PDRB ADHB")
boxplot(data$x5,main = "Boxplot PTP")
```

Dari kedelapan boxplot dapat diamati bahwa setiap kategori peubah terdapat pencilan.

### Scatter Plot

```{r, include=FALSE}
panel.cor <- function(x, y, cex.cor = 0.8, method = "pearson", ...) {
options(warn = -1)                   # Turn of warnings (e.g. tied ranks)
usr <- par("usr"); on.exit(par(usr)) # Saves current "usr" and resets on exit
par(usr = c(0, 1, 0, 1))             # Set plot size to 1 x 1
r <- cor(x, y, method = method, use = "pair")               # correlation coef
p <- cor.test(x, y, method = method)$p.val                  # p-value
n <- sum(complete.cases(x, y))                              # How many data pairs
txt <- format(r, digits = 3)                                # Format r-value
txt1 <- format(p, digits = 3)                                 # Format p-value
txt2 <- paste0("r= ", txt, '\n', "p= ", txt1, '\n', 'n= ', n) # Make panel text
text(0.5, 0.5, txt2, cex = cex.cor, ...)                      # Place panel text
options(warn = 0)                                             # Reset warning
}
```

```{r, warning=FALSE}
pairs(data[,2:7], pch = 19, lower.panel=panel.smooth, upper.panel=panel.cor)
corrplot.mixed(cor(data[,2:7]), upper = "number", lower = "ellips")
corrplot(cor(data[,2:7]), addCoef.col = 'black', tl.pos = 'd',
         cl.pos = 'n', col = COL2('PRGn'))
```

Pada scatter plot, dapat diperhatikan bahwa terdapat korelasi negatif paling signifikan dapat ditemukan di antara peubah penjelas x1 dengan peubah penjelas x3, dan di antara peubah x2 dan x4, serta peubah x3 dan x7. Sementara korelasi positif yang signifikan dapat ditemukan di antara peubah respon y dengan peubah penjelas x7, x5, dan x1.

## Sebaran Spasial Data Pengamatan

### Kalimantan Barat

```{r warning=FALSE}
petaKB <- readOGR(dsn ="/home/faggluyy/Documents/Documents/Kuliah/Semester 6/Bayes/Tugas Akhir/SHP/SHP Kalimantan Barat")
petaKB$Kabupaten

dataKB <- data[c(1:14),]
```

```{r}
k=101
colfunc <- colorRampPalette(c("blue", "green", "yellow","red"))
color <- colfunc(k)
petaKB$y <- dataKB$y
spplot(petaKB, "y", col.regions=color)
```

### Kalimantan Tengah

```{r warning=FALSE}
petaKTg <- readOGR(dsn ="/home/faggluyy/Documents/Documents/Kuliah/Semester 6/Bayes/Tugas Akhir/SHP/SHP Kalimantan Tengah")
petaKTg$Kabupaten

dataKTg <- data[c(15:28),]
```

```{r}
k=101
colfunc <- colorRampPalette(c("blue", "green", "yellow","red"))
color <- colfunc(k)
petaKTg$y <- dataKTg$y
spplot(petaKTg, "y", col.regions=color)
```

### Kalimantan Selatan

```{r warning=FALSE}
petaKS <- readOGR(dsn ="/home/faggluyy/Documents/Documents/Kuliah/Semester 6/Bayes/Tugas Akhir/SHP/SHP Kalimantan Selatan")
petaKS$Kabupaten

dataKS <- data[c(29:41),]
```

```{r}
k=101
colfunc <- colorRampPalette(c("blue", "green", "yellow","red"))
color <- colfunc(k)
petaKS$y <- dataKS$y
spplot(petaKS, "y", col.regions=color)
```

### Kalimantan Timur

```{r warning=FALSE}
petaKT <- readOGR(dsn ="/home/faggluyy/Documents/Documents/Kuliah/Semester 6/Bayes/Tugas Akhir/SHP/SHP Kalimantan Timur")
petaKT$Kabupaten

dataKT <- data[c(42:51),]
```

```{r}
k=101
colfunc <- colorRampPalette(c("blue", "green", "yellow","red"))
color <- colfunc(k)
petaKT$y <- dataKT$y
spplot(petaKT, "y", col.regions=color)
```

### Kalimantan Utara

```{r warning=FALSE}
petaKU <- readOGR(dsn ="/home/faggluyy/Documents/Documents/Kuliah/Semester 6/Bayes/Tugas Akhir/SHP/SHP Kalimantan Utara")
petaKU$Kabupaten

dataKU <- data[c(52:56),]
```

```{r}
k=101
colfunc <- colorRampPalette(c("blue", "green", "yellow","red"))
color <- colfunc(k)
petaKU$y <- dataKU$y
spplot(petaKU, "y", col.regions=color)
```

### Kalimantan

```{r}
Kalimantan <- rbind(petaKB,petaKTg,petaKS,petaKT,petaKU)

k=101
colfunc <- colorRampPalette(c("blue", "green", "yellow","red"))
color <- colfunc(k)
Kalimantan$y <- data$y
spplot(Kalimantan, "y", col.regions=color)
```

# Matriks Bobot Spasial

Pemilihan matriks bobot didasarkan pada matriks bobot dengan nilai indeks moran yang tertinggi.

## Matriks Ketetanggaan

### 1 Queen Contiguity

```{r}
sp.peta <- SpatialPolygons(Kalimantan@polygons)
qc <- poly2nb(sp.peta, queen = TRUE)
qc
```

```{r, echo=TRUE, warning=FALSE}
W.qc <- nb2listw(qc, style='W',zero.policy=TRUE)
ols <- lm(y~x1+x2+x3+x4+x5, data=data)
qct = lm.morantest(ols, W.qc, alternative="greater")
qct
```

### 2 Rook Contiguity

```{r}
sp.peta <- SpatialPolygons(Kalimantan@polygons)
rc <- poly2nb(sp.peta, queen = FALSE)
rc
```

```{r, echo=TRUE, warning=FALSE}
W.rc <- nb2listw(rc, style='W',zero.policy=TRUE)
rct = lm.morantest(ols, W.rc, alternative="greater")
rct
```

## Matriks Jarak

```{r}
longlat <- coordinates(Kalimantan)
head(longlat)
```

```{r}
Kalimantan$long <- longlat[,1]
Kalimantan$lat <- longlat[,2]
coords <- Kalimantan[c("long","lat")]
#class(coords)
koord <- as.data.frame(coords)
djarak<-dist(longlat)
m.djarak<-as.matrix(djarak)
```

### 1 KNN

```{r}
# k = 5
W.knn<-knn2nb(knearneigh(longlat,k=5,longlat=TRUE))
W.knn.s <- nb2listw(W.knn,style='W')
mt1 = lm.morantest(ols,W.knn.s,alternative = "greater")
mt1
```

### 2 Radial Distance Weigth

```{r}
W.dmax<-dnearneigh(longlat,0,677,longlat=TRUE)
W.dmax.s <- nb2listw(W.dmax,style='W')
mt2 = lm.morantest(ols,W.dmax.s,alternative = "greater")
mt2
```

### 3 Exponential Distance Weigth

```{r}
alpha=1
W.e<-exp((-alpha)*m.djarak)
diag(W.e)<-0
rtot<-rowSums(W.e,na.rm=TRUE)
W.e.sd<-W.e/rtot #row-normalized
W.e.s = mat2listw(W.e.sd,style='W')
mt5 = lm.morantest(ols,W.e.s,alternative ="greater")
mt5
```

```{r}
alpha2=2
W.e2<-exp((-alpha2)*m.djarak)
diag(W.e2)<-0
rtot2<-rowSums(W.e2,na.rm=TRUE)
W.e2.sd<-W.e2/rtot #row-normalized
W.e2.s = mat2listw(W.e2.sd,style='W')
mt6 = lm.morantest(ols,W.e2.s,alternative = "greater")
mt6
```

### 4 Inverse Distance Weight

```{r, echo=TRUE, results='hide'}
coords <- cbind(Kalimantan$long,Kalimantan$lat)

W.inv <- knn2nb(knearneigh(coords))
critical.threshold <- max(unlist(nbdists(W.inv,coords)))
critical.threshold

nb.dist.band <- dnearneigh(coords, 0, critical.threshold)
distances <- nbdists(nb.dist.band,coords)
distances[1]

invd1 <- lapply(distances, function(x) (1/x))
length(invd1)
invd1[1]

invd1a <- lapply(distances, function(x) (1/(x/100)))
invd1a[1]

W.inv.s <- nb2listw(nb.dist.band,glist = invd1a,style = "B")

mt4 = lm.morantest(ols,W.inv.s,alternative = "greater")
```

```{r}
mt4
```

## Matriks Bobot Terpilih

```{r}
MatrikBobot <- c("KNN","dmax","inv","Exp1","Exp2","Rook","Queen")
IndeksMoran <- c(mt1$estimate[1],mt2$estimate[1],mt4$estimate[1],mt5$estimate[1],mt6$estimate[1],rct$estimate[1],qct$estimate[1])
pv = c(mt1$p.value,mt2$p.value,mt4$p.value,mt5$p.value,mt6$p.value,rct$p.value,qct$p.value)
M = cbind.data.frame(MatrikBobot,IndeksMoran,"p-value"=pv)
M
```

Matriks bobot yang memiliki nilai Indeks Moran yang tertinggi adalah K-Nearest Neighbor (KNN). Selanjutnya, matriks bobot ini yang akan digunakan dalam analisis.

# Model OLS

```{r}
ols <- lm(y~x1+x2+x3+x4+x5, data=data)
summary(ols)
AIC(ols)
```

## Pemeriksaan multikolinieritas

Pada regresi OLS perlu diperiksa persyaratan tidak adanya kolinearitas antar peubah penjelas agar koefisien regresi yang diperoleh dapat dikatakan valid. Pemeriksaan multikolinearitas dapat dilakukan dengan menggunakan nilai variance inflation factor (VIF). Multikolinearitas terjadi jika nilai VIF $\geq$ 5.

```{r}
car::vif(ols)
```

Berdasarkan perhitungan, tidak diperoleh nilai VIF $\geq$ 5 menandakan tidak terjadinya multikolinearitas antar peubah penjelas

## Uji Asumsi

### 1. Uji Normalitas Sisaan

H0 : galat model menyebar normal H1 : galat model tidak menyebar normal

```{r, message=FALSE}
library(lmtest)
err.ols <- residuals(ols)
library(nortest)
ad.test(err.ols)
```

Karena p-value $\geq$ 5%, maka TERIMA H0. Artinya sisaan menyebar NORMAL.

### 2. Uji Autokorelasi Spasial

Pengujian autokorelasi spasial menggunakan Indeks Moran yang memerlukan matriks pembobot spasial sehingga dibutuhkan matriks bobot terbaik yaitu Exponential Distance Weight dengan $\alpha=2$.

H0 : Tidak Ada Autokorelasi H1 : Ada Autokorelasi

```{r}
ww = W.knn.s
lm.morantest(ols, listw=ww, alternative="greater")
```

Karena p-value $\leq$ 5%, maka TOLAK H0. Artinya terdapat AUTOKORELASI SPASIAL pada data.

### 3. Uji Kehomogenan Ragam

H0 : Ragam galat homogen H1 : Ragam galat tidak homogen

```{r}
lmtest::bptest(ols)
```

Karena p-value $\geq$ 5%, maka TERIMA H0. Artinya ragam galat homogen.

## Uji Efek Spasial

```{r}
#Peubah Respon Y
qcty = moran.test(data$y, W.knn.s, alternative="greater")

#Peubah Penjelas Xi
qctx1 = moran.test(data$x1, W.knn.s, alternative="greater")
qctx2 = moran.test(data$x2, W.knn.s, alternative="greater")
qctx3 = moran.test(data$x3, W.knn.s, alternative="greater")
qctx4 = moran.test(data$x4, W.knn.s, alternative="greater")
qctx5 = moran.test(data$x5, W.knn.s, alternative="greater")
```

```{r}
Variable <- c("Y","X1","X2","X3","X4","X5")
InMor <- c(qcty$estimate[1],qctx1$estimate[1],qctx2$estimate[1],qctx3$estimate[1],qctx4$estimate[1],qctx5$estimate[1])
pval <- c(qcty$p.value,qctx1$p.value,qctx2$p.value,qctx3$p.value,qctx4$p.value,qctx5$p.value)
N <- cbind.data.frame(Variable, "Indeks Moran"=InMor,"p-value"=pval)
N
```

Pada uji Morans'I variabel yang terdapat autokorelasi atau keterkaitan antar provinsi adalah variabel Indeks Ketahanan Pangan (X1), Infrastruktur Internet (X2), Laju Pertumbuhan Penduduk (X3), dan Produk Domestik Regional Bruto Atas Dasar Harga Berlaku (X4), yang dilihat berdasarkan nilai p-value $\leq (\alpha = 0.05)$. Peubah Produksi Tanaman Pangan (X5) tidak akan digunakan dalam tahapan lanjut penelitian karena tidak memenuhi asumsi signifikansi pada taraf $(\alpha = 0.05)$.

# Model Dependensi Spasial

## Uji LM

Uji LM untuk mengetahui model dependensi mana yang mungkin dipilih.

```{r}
ols <- lm(y~x1+x2+x3+x4, data=data)
ww = W.knn.s
model <- lm.LMtests(ols,listw=ww,zero.policy = TRUE, test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
summary(model)
```

Pada Uji LM hingga taraf 10%, belum dapat diketahui secara nyata pada setiap pengujian, sehingga diduga modelnya belu mdapat ditentukan. Akan tetapi seluruh model akan diuji kembali untuk memastikan kecurigaan.

### Spatial Lag X (SLX)

```{r, warning=FALSE}
SLX <- lmSLX(y~x1+x2+x3+x4, data=data, listw=ww, zero.policy=TRUE)
summary(SLX)
```

Output di atas menunjukkan bahwa koefisien signifikan pada taraf nyata 5% (p-value \< 4.67e-06). AIC model SLX adalah sebesar 300.2665

### Spatial Autoregressive Model (SAR)

```{r}
SAR <- lagsarlm(y~x1+x2+x3+x4, data = data, listw = ww)
summary(SAR, Nagelkerke = T)
```

Output di atas menunjukkan bahwa koefisien Rho tidak signifikan pada taraf nyata 5% (p-value = 0.14682). AIC model SAR adalah sebesar 300.9304

### Spatial Error Model (SEM)

```{r}
SEM<-errorsarlm(y~x1+x2+x3+x4, data = data, listw = ww)
summary(SEM)
```

Output di atas menunjukkan bahwa koefisien lambda signifikan pada taraf nyata 5% (p-value = 0.027781). AIC model SEM adalah sebesar 299.8284

### SARMA

```{r}
SARMA <- sacsarlm(y~x1+x2+x3+x4,data=data,ww, zero.policy = TRUE)
summary(SARMA)
```

Output di atas menunjukkan bahwa koefisien lambda tidak signifikan pada taraf nyata 5% (p-value = 0.20996). AIC model SARMA adalah sebesar 301.8179

## Model Terbaik

```{r}
cbind.data.frame(AIC(ols),AIC(SLX),AIC(SAR),AIC(SEM),AIC(SARMA))
```

Dari seluruh model dependensi spasial yang diujicobakan, model SEM memiliki nilai AIC yang paling kecil dengan koefisien yang signifikan, sehingga model OLS tetapt dipilih menjadi model terbaik.
