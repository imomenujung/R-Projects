---
title: "PSB"
output:
  word_document: default
  html_document: default
date: "2024-06-02"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
require(INLA)
require(spdep)
```





```{r}
library(sf)
Barat <- read_sf('/home/faggluyy/Documents/Documents/Kuliah/Semester 6/Bayes/Tugas Akhir/SHP/SHP Kalimantan Barat/Kalimantan_Barat_ADMIN_BPS.shp')
Selatan <- read_sf('/home/faggluyy/Documents/Documents/Kuliah/Semester 6/Bayes/Tugas Akhir/SHP/SHP Kalimantan Selatan/Kalimantan_Selatan_ADMIN_BPS.shp')
Tengah <- read_sf('/home/faggluyy/Documents/Documents/Kuliah/Semester 6/Bayes/Tugas Akhir/SHP/SHP Kalimantan Tengah/Kalimantan_Tengah_ADMIN_BPS.shp')
Timur <- read_sf('/home/faggluyy/Documents/Documents/Kuliah/Semester 6/Bayes/Tugas Akhir/SHP/SHP Kalimantan Timur/Kalimantan_Timur_ADMIN_BPS.shp')
Utara <- read_sf('/home/faggluyy/Documents/Documents/Kuliah/Semester 6/Bayes/Tugas Akhir/SHP/SHP Kalimantan Utara/Kalimantan_Utara_ADMIN_BPS.shp')

Kalimantan <- rbind(Barat, Selatan, Tengah, Timur, Utara)
head(Kalimantan)
```
```{r}
library(spData)
library(tidyr)
```

```{r}
library(sp)

datakor <- st_coordinates(st_centroid(Kalimantan$geometry))
datakor <- as.data.frame(datakor)
```


```{r}
data <- read.csv('Data Kalimantan.csv', sep = ';')
colnames(data)[1] <- 'Kabupaten'
head(data)
```

```{r}
ols <- lm(IPM ~  IKP + II + LPP  + PDRB_ADHB + PTP , data=data)
```


```{r}
W.knn<-knn2nb(knearneigh(datakor,k=5,longlat=TRUE))
KNN_mat <- nb2mat(W.knn, style = 'B', zero.policy = TRUE)
W.knn.s <- nb2listw(W.knn,style='W')
mt1 = lm.morantest(ols,W.knn.s,alternative = "greater")
mt1
```



```{r}
djarak <- dist(datakor)
m.djarak <- as.matrix(djarak)
head(m.djarak)
```

```{r}
alpha2=2
W.e2<-exp((-alpha2)*m.djarak)
diag(W.e2)<-0
rtot2<-rowSums(W.e2,na.rm=TRUE)
W.e2.sd<-W.e2/rtot2 #row-normalized
W = mat2listw(W.e2.sd,style='W')
W
```
```{r}
library(spdep)
```








```{r}
colnames(data)
```


```{r}
library(spdep)
# Kalimantan_nb <- poly2nb(Kalimantan)
# 
# nb2INLA("Kalimantan_inla.graph", Kalimantan_nb)
# Kalimantan_adj <- "Kalimantan_inla.graph"
```

```{r}
library(INLA)
# H <- inla.read.graph(filename = Kalimantan_adj)
# image(inla.graph2matrix(H), xlab = "", ylab = "")
```





```{r}
library(INLA)
```



```{r}
formula_inla <- IPM ~ 1  + IKP + II + LPP  + PDRB_ADHB + PTP +  f(Kabupaten, model="iid",graph=KNN_mat)
    ## spefiying the priors for the unstri and str 
                     # hyper=list(prec.unstruct=list(prior="loggamma",param=c(1,0.001)), 
                     #            prec.spatial=list(prior="loggamma",param=c(1,0.001))))
```











```{r}
model_inla <- inla(formula_inla,family="gaussian",
                     data=data,
                   control.predictor = list(compute = TRUE),
                     control.compute=list(dic=TRUE))
```


```{r}
summary(model_inla)
```


```{r}
head(model_inla$summary.random$Kabupaten)
```
```{r}
model_inla$summary.random
```

```{r}
spatial_random_effects <- model_inla$summary.random$Kabupaten
spatial_random_effects
```

```{r}
data_gabung <- merge(data, spatial_random_effects, by.x = "Kabupaten", by.y = "ID", all.x = TRUE)
head(data_gabung)
```

```{r}
library(INLA)
library(spdep)
library(sf)
library(tmap)
library(ggplot2)

```
```{r}

data_gabung$longitude <- datakor$X
data_gabung$latitude <- datakor$Y

library(sf)
if (!inherits(data_gabung, "sf")) {
  data_gabung <- st_as_sf(data_gabung, coords = c("longitude", "latitude"), crs = 4326)
  cat('haho')
}
```

```{r}
data_gabung$geometry <- Kalimantan$geometry
```


```{r}
plot(data_gabung[c('mean')])
```


```{r}
data_gabung$IPM_prediksi <- model_inla$summary.fitted.values$mean
```


```{r}
plot(data_gabung[c('IPM_prediksi')])
```

