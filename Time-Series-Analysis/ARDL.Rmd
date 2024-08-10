---
title: "ARDL"
author: "Fadly"
date: "2023-09-18"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
#install.packages("dLagM") #install jika belum ada
#install.packages("dynlm") #install jika belum ada
#install.packages("MLmetrics") #install jika belum ada
#install.packages("lmtest")
#install.packages("car")
library(dLagM)
library(dynlm)
library(MLmetrics)
library(lmtest)
library(car)
```


## Import data tentang total CO2 emissions
```{r}
data <- read.csv("https://raw.githubusercontent.com/MNafiz/MPDW/main/Pertemuan%202/dataPertemuan2.csv")
str(data)
data
```

## Splitting data uji dan data latih
```{r}
train<-data[1:100,]
test<-data[101:126,]
```

## pemodelan koyck
## peubah xt dan peubah y(t-1) berpengaruh signifikan
```{r}
model.koyck <- koyckDlm(x = train$Solid.Fuel, y = train$Total)
summary(model.koyck)
AIC(model.koyck)
BIC(model.koyck)
```

```{r}
train.ts<-ts(train)
test.ts<-ts(test)
data.ts<-ts(data)
```

## MAPE yang dihasilkan dari model koyck cukup bagus yaitu sekitar 1,5 %
```{r}
fore.koyck <- forecast(model = model.koyck, x=test$Solid.Fuel, h=26)
fore.koyck
mape.koyck <- MAPE(fore.koyck$forecasts, test$Total)
#akurasi data training
GoF(model.koyck)
```

## Pada model dlm dengan parameter q = 2, rsquared yang dihasilkan tidak baik yaitu 10 %
```{r}
model.dlm <- dlm(x = train$Solid.Fuel,y = train$Total , q = 2)
summary(model.dlm)
AIC(model.dlm)
BIC(model.dlm)
```
## MAPE yang dihasilkan 14 % dimana model koyck masih lebih baik
```{r}
fore.dlm <- forecast(model = model.dlm, x=test$Solid.Fuel, h=26)
fore.dlm
mape.dlm <- MAPE(fore.dlm$forecasts, test$Total)
#akurasi data training
GoF(model.dlm)
```

## Melakukan pencarian nilai parameter q optimum dan didapatkan q optimum adalah 48 dan nilai AIC serta BIC yang dihasilkan berkurang drastis
```{r}
finiteDLMauto(formula = Total ~ Solid.Fuel,
              data = data.frame(train), q.min = 1, q.max = 48,
              model.type = "dlm", error.type = "AIC", trace = FALSE)
```
## Rsquared yang dihasikan cukup tinggi yaitu sekitar 99,9 %
```{r}
model.dlm2 <- dlm(x = train$Solid.Fuel,y = train$Total , q = 48)
summary(model.dlm2)
AIC(model.dlm2)
BIC(model.dlm2)
```

## MAPE yang dihasilkan cukup baik yaitu 0,1 %
```{r}
#peramalan dan akurasi
fore.dlm2 <- forecast(model = model.dlm2, x=test$Solid.Fuel, h=26)
mape.dlm2<- MAPE(fore.dlm2$forecasts, test$Total)
#akurasi data training
GoF(model.dlm2)
```

## pemodelan ARDL dengan parameter p dan q = 1
## rsquared yang dihasilkan sekitar 98,5 %
```{r}
model.ardl <- ardlDlm(x = train$Solid.Fuel, y = train$Total, p = 1 , q = 1)
summary(model.ardl)
```
## Peramalan ardl
```{r}

fore.ardl <- forecast(model = model.ardl, x=test$Solid.Fuel, h=26)
fore.ardl

```

## MAPE yang dihasilkan sekitar 1,2 %
```{r}
mape.ardl <- MAPE(fore.ardl$forecasts, test$Total)
mape.ardl
#akurasi data training
GoF(model.ardl)
```
## Penentuan lag optimum
# didapatkan nilai q optimum adalah 4 dan nilai p optimum adalah 15
```{r}
#penentuan lag optimum
model.ardl.opt <- ardlBoundOrders(data = data.frame(data), ic = "AIC", 
                                  formula = Total ~ Solid.Fuel )
min_p=c()
for(i in 1:6){
  min_p[i]=min(model.ardl.opt$Stat.table[[i]])
}
q_opt=which(min_p==min(min_p, na.rm = TRUE))
p_opt=which(model.ardl.opt$Stat.table[[q_opt]] == 
              min(model.ardl.opt$Stat.table[[q_opt]], na.rm = TRUE))
data.frame("q_optimum" = q_opt, "p_optimum" = p_opt, 
           "AIC"=model.ardl.opt$min.Stat)
```

```{r}
#sama dengan model dlm q=1
cons_lm1 <- dynlm(Total ~ Solid.Fuel+L(Solid.Fuel),data = train.ts)
#sama dengan model ardl p=1 q=0
cons_lm2 <- dynlm(Total ~ Solid.Fuel+L(Total),data = train.ts)
#sama dengan ardl p=1 q=1
cons_lm3 <- dynlm(Total ~ Solid.Fuel+L(Solid.Fuel)+L(Total),data = train.ts)
#sama dengan dlm p=2
cons_lm4 <- dynlm(Total ~ Solid.Fuel+L(Solid.Fuel)+L(Solid.Fuel,2),data = train.ts)
```

## Terlihat model bagus ketika mempunyai peubah lag pada target yaitu L(Total)
```{r}
summary(cons_lm1)
summary(cons_lm2)
summary(cons_lm3)
summary(cons_lm4)
```

## SSE yang dihasilkan dengan peubah lag target lebih kecil dibandingkan dengan yang tidak
```{r}
deviance(cons_lm1)
deviance(cons_lm2)
deviance(cons_lm3)
deviance(cons_lm4)
```
```{r}
if(require("lmtest")) encomptest(cons_lm1, cons_lm2)
```
## Model dengan menggunakan peubah lag target tidak terindikasi terjadinya autokorelasi (tak tolak h0)
```{r}
dwtest(cons_lm1)
dwtest(cons_lm2)
dwtest(cons_lm3)
dwtest(cons_lm4)
```
## ragam sisaan untuk keempat model bersifat homogen
```{r}
bptest(cons_lm1)
bptest(cons_lm2)
bptest(cons_lm3)
bptest(cons_lm4)
```

## ragam sisaan yang dihasilkan pada seluruh model tidak menyebar normal
```{r}
shapiro.test(residuals(cons_lm1))
shapiro.test(residuals(cons_lm2))
shapiro.test(residuals(cons_lm3))
shapiro.test(residuals(cons_lm4))
```

## Model ardl merupakan model yang paling baik karena memiliki nilai MAPE terendah
```{r}
akurasi <- matrix(c(mape.koyck, mape.dlm, mape.dlm2, mape.ardl))
row.names(akurasi)<- c("Koyck","DLM 1","DLM 2","Autoregressive")
colnames(akurasi) <- c("MAPE")
akurasi
```
```{r}
par(mfrow=c(1,1))
plot(test$Solid.Fuel, test$Total, type="b", col="black", ylim=c(120,200000))
points(test$Solid.Fuel, fore.koyck$forecasts,col="red")
lines(test$Solid.Fuel, fore.koyck$forecasts,col="red")
points(test$Solid.Fuel, fore.dlm$forecasts,col="blue")
lines(test$Solid.Fuel, fore.dlm$forecasts,col="blue")
points(test$Solid.Fuel, fore.dlm2$forecasts,col="orange")
lines(test$Solid.Fuel, fore.dlm2$forecasts,col="orange")
points(test$Solid.Fuel, fore.ardl$forecasts,col="green")
lines(test$Solid.Fuel, fore.ardl$forecasts,col="green")
legend("topleft",c("aktual", "koyck","DLM 1","DLM 2", "autoregressive"), lty=1, col=c("black","red","blue","orange","green"), cex=0.8)
```

## pengayaan

```{r}
data(M1Germany)
data1 = M1Germany[1:144,]
```

```{r}
finiteDLMauto(formula = logprice ~ interest+logm1,
              data = data.frame(data1), q.min = 1, q.max = 5,
              model.type = "dlm", error.type = "AIC", trace = FALSE)
```

```{r}
model.dlmberganda = dlm(formula = logprice ~ interest + logm1,
                data = data.frame(data1) , q = 5)
summary(model.dlmberganda)

model.dlmberganda2 = dlm(formula = logprice ~ interest + logm1,
                        data = data.frame(data1) , q = 1)
summary(model.dlmberganda2)
```

```{r}
#Mencari orde lag optimum model ARDL
ardlBoundOrders(data = data1 , formula = logprice ~ interest + logm1,
                ic="AIC")

model.ardlDlmberganda = ardlDlm(formula = logprice ~ interest + logm1,
                        data = data.frame(data1) , p = 4 , q = 4)
summary(model.ardlDlmberganda)
```

```{r}
rem.p = list(interest = c(1,2,3,4))
remove = list(p = rem.p)
model.ardlDlmberganda2 = ardlDlm(formula = logprice ~ interest + logm1,
                        data = data.frame(data1) , p = 4 , q = 4 ,
                        remove = remove)
summary(model.ardlDlmberganda2)
```

