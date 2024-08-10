# Library
library(readr)  #Membaca data
library(dplyr)  #Data processing
library(DT)     #Menampilkan tabel agar mudah dilihat di browser
library(factoextra) #Visualisasi Cluster

# Lihat Data
df <- umkm_data
df

# Pilih Data
df_feature <- df[,3:12]
df_feature

# Tentukan Cluster
jumlah_klaster <- c(1:9)  #Vektor yang berisikan jumlah klaster yang ingin dilihat nilai dari total within-cluster sum of squares
within_ss <- c()  #Vektor kosong yang akan diisi nilai total within-cluster sum of squares
for (i in jumlah_klaster) {
  within_ss <- c(within_ss, kmeans(x = df_feature, centers = i, nstart = 25)$tot.withinss)
}

plot(x = jumlah_klaster, y = within_ss, type = "b", xlab = "Number of Cluster",
     ylab = "Total Within Sum of Squares", main = "Elbow Plot")
abline(v = 3, col = 'red')

# K-means
set.seed(123)
kmeans_clustering <- kmeans(x = df_feature, centers = 3, nstart = 25)  #parameter nstart digunakan untuk memberitahu fungsi berapa kali inisiasi centroid awal (secara acak) yang akan dibentuk dan centers digunakan untuk memberitahu fungsi berapa jumlah klaster yang akan dibentuk.
kmeans_clustering

# Visualisasi Cluster
fviz_cluster(kmeans_clustering, data = df_feature, 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

