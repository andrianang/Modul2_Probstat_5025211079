# Modul2_Probstat_5025211079

1. Seorang peneliti melakukan penelitian mengenai pengaruh aktivitas 𝐴 terhadap kadar saturasi oksigen pada manusia. Peneliti tersebut mengambil sampel sebanyak 9 responden. Pertama, sebelum melakukan aktivitas 𝐴, peneliti mencatat kadar saturasi oksigen dari 9 responden tersebut. Kemudian, 9 responden tersebut diminta melakukan aktivitas 𝐴. Setelah 15 menit, peneliti tersebut mencatat kembali kadar saturasi oksigen dari 9 responden tersebut. Berikut data dari 9 responden mengenai kadar saturasi oksigen sebelum dan sesudah melakukan aktivitas 𝐴 

Berdasarkan data pada tabel diatas, diketahui kadar saturasi oksigen  dari responden ke-3 ketika belum melakukan aktivitas 𝐴 sebanyak 67, dan setelah melakukan aktivitas 𝐴 sebanyak 70.

a. Carilah Standar Deviasi dari data selisih pasangan pengamatan tabel diatas

```R
Mengelompokkan before dan after
bef = c(78, 75, 67, 77, 70, 72, 78, 74, 77)
aft = c(100, 95, 70, 90, 90, 90, 89, 90, 100)

Mencari perbedaan bef dan aft
dif = bef - aft
```
```R
Membentuk tabel
data <- data.frame( 
  before = rep(c(before)),
  after = c(after),
  dif = c(dif)
)
print(data)
```
![image](https://user-images.githubusercontent.com/91018876/206917739-ed9ce49d-f92d-4766-937f-3c0ffb8e998e.png)

```R
Mencari nilai Standard Deviasi
std = sd(dif)
print(std)
```
![image](https://user-images.githubusercontent.com/91018876/206917780-9c018884-9692-47e7-92b9-d01909d3bc99.png)

b. carilah nilai t (p-value)
```R
std = sd(dif)
zbar = mean(dif)
n = length(dif)

tvalue <- zbar/(std/sqrt(n))
print(tvalue)
```
![image](https://user-images.githubusercontent.com/91018876/206917833-67efd8d1-fd17-40b7-b07e-03b9d47d8427.png)

c. tentukanlah apakah terdapat pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan aktivitas 𝐴 jika diketahui tingkat signifikansi 𝛼 = 5% serta H0 : “tidak ada pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan aktivitas 𝐴”
![image](https://user-images.githubusercontent.com/91018876/206917961-ffc458f3-0061-4e2b-bd07-1db359f5b68c.png)

```R
tvalue (kritis) dari tabel bernilai 2.306. 
pvalue <- 2*pt(tvalue, df=n-1)
print(pvalue)
```
![image](https://user-images.githubusercontent.com/91018876/206918276-6c8056f3-3e25-4d86-8f96-114230863d78.png)

```
tvalue > tvalue(kritikal) dan pvalue < nilai signifikan, sehingga tolak H0.
Kesimpulan H1 : “Ada pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan aktivitas 𝐴”
```

2. Diketahui bahwa mobil dikemudikan rata-rata lebih dari 20.000 kilometer per tahun. Untuk menguji klaim ini, 100 pemilik mobil yang dipilih secara acak diminta untuk mencatat jarak yang mereka tempuh. Jika sampel acak menunjukkan rata-rata 23.500 kilometer dan standar deviasi 3900 kilometer. (Kerjakan menggunakan library seperti referensi pada modul). 

```
H0 : mu <= 20 000km
H1 : mu > 20 000km
```
```R
library(BSDA)
xbar = 23500
n = 100
std = 3900
mu = 20000
zsum.test(xbar, std, n, alternative = "greater",
          mu=20000, conf.level=0.95)
```
![image](https://user-images.githubusercontent.com/91018876/206918346-bf11982e-c3a4-4d45-a7dd-cede6a71b82b.png)

a. Apakah Anda setuju dengan klaim tersebut?
```
Setuju, karena pvalue < nilai signifikan
```

b. Jelaskan maksud dari output yang dihasilkan! 
```
Dengan menggunakan fungsi zsum.test, akan menghasilkan pvalue dan nilai z.
Terlihat bahwa pvalue < nilai signifikan, yang berarti tolak H0, yaitu mu <= 20 000km.
```

c. Buatlah kesimpulan berdasarkan P-Value yang dihasilkan!
```
Terlihat bahwa pvalue < nilai signifikan, yang berarti tolak H0, yaitu mu <= 20 000km.
Terbukti bahwa mobil dikemudikan rata-rata lebih dari 20 000km per tahun.
```

3. (Hipotesa 2 sampel) Diketahui perusahaan memiliki seorang data analyst ingin memecahkan permasalahan pengambilan keputusan dalam perusahaan tersebut. Selanjutnya didapatkanlah data berikut dari perusahaan saham tersebut.
![image](https://user-images.githubusercontent.com/91018876/206918636-268bbee6-b9d6-4216-a10d-0a847827409a.png)

Dari data diatas berilah keputusan serta kesimpulan yang didapatkan dari hasil diatas. Asumsikan nilai variancenya sama, apakah ada perbedaan pada rata-ratanya (α= 0.05)? Buatlah :
```R
#Diketahui
xbar1 = 3.64
xbar2 = 2.79

n1 = 19
n2 = 27

std1 = 1.67
std2 = 1.332
```
a. H0 dan H1
```
H0 : mu1 = mu2
H1 : mu1 <> mu2
```

b. Hitung Sampel Statistik
```R
#Asumsikan variance sama
df = (n1+n2-2)
sp =sqrt(((n1-1)*(std1**2) + (n2-1)*(std2**2))/df)

tvalue = (xbar1-xbar2)/(sp*sqrt((1/n1) + (1/n2)))

print(tvalue)
```
![image](https://user-images.githubusercontent.com/91018876/206918716-cfa6fc0d-832a-44b9-940a-5085735e8110.png)

c. Lakukan Uji Statistik (df =2)
```R
Kita dapat melakukan uji statistik dengan fungsi berikut.
plotDist(dist = 't', df = 2, col = "blue")
```
![image](https://user-images.githubusercontent.com/91018876/206918797-ef997758-050f-460c-b38e-299d77d0f9a5.png)

d. Nilai Kritikal
```R
alpha = 0.05
crivalue1 = qnorm(alpha/2)
crivalue2 = qnorm(1-alpha/2)

print(crivalue1)
print(crivalue2)
```
![image](https://user-images.githubusercontent.com/91018876/206918865-3e0292e9-78ab-45b7-91f2-655509b0ce44.png) ![image](https://user-images.githubusercontent.com/91018876/206918871-9248190c-5bdf-4c72-9959-abbcdaddfe04.png)

e. Keputusan
```
Terlihat bahwa nilai tvalue berada pada interval crivalue1-crivalue2, maka terima H0.
```
f. Kesimpulan
```
H0 terbukti : mu1 = mu2
```

4. (Anova satu arah) Seorang Peneliti sedang meneliti spesies dari kucing di ITS . Dalam penelitiannya ia mengumpulkan data  tiga spesies kucing yaitu kucing oren, kucing hitam dan kucing putih dengan panjangnya masing-masing. 
Jika : 

diketahui dataset  https://intip.in/datasetprobstat1 
H0 : Tidak ada perbedaan panjang antara ketiga spesies atau rata-rata panjangnya sama    

Maka Kerjakan atau Carilah:

a. Buatlah masing masing jenis spesies menjadi  3 subjek "Grup" (grup 1,grup 2,grup 3). Lalu Gambarkan plot kuantil normal untuk setiap kelompok dan lihat apakah ada outlier utama dalam homogenitas varians.
```R
Membaca tabel dari sebuah txt file
oneway_anova = read.table("D:\\Kuliah\\SEM 3\\Probabilitas dan Statistika\\Praktikum\\2\\data_soal4.txt", h=T)

Menjadikan tabel sebagai factor
oneway_anova$Group <- as.factor(oneway_anova$Group)

Memberikan label pada tabel
oneway_anova$Group = factor(oneway_anova$Group,labels = c("kucing oren", "kucing hitam", "kucing putih"))

Membagi tabel ke beberapa grup.
group1 <- subset(oneway_anova, Group == "kucing oren")
group2 <- subset(oneway_anova, Group == "kucing hitam")
group3 <- subset(oneway_anova, Group == "kucing putih")
```
```
Menampilkan plot kuantil normal
```
```R
qqnorm(group1$Length)
qqline(group1$Length)
```
![image](https://user-images.githubusercontent.com/91018876/206919363-5d1fdfe0-d6ae-4fbb-8d14-ab36cc38c03b.png)

```R
qqnorm(group2$Length)
qqline(group2$Length)
```
![image](https://user-images.githubusercontent.com/91018876/206919391-f357e476-74a3-40ec-b469-5d8197862515.png)

```R
qqnorm(group3$Length)
qqline(group3$Length)
```
![image](https://user-images.githubusercontent.com/91018876/206919410-c99f11bd-5993-445c-9c90-6c065f7bd796.png)

b. Carilah atau periksalah Homogeneity of variances nya , Berapa nilai p yang didapatkan? , Apa hipotesis dan kesimpulan yang dapat diambil ?

```R
Untuk memeriksa homogenitas varians dilakukan seperti berikut

bartlett.test(oneway_anova$Length, oneway_anova$Group)
```
![image](https://user-images.githubusercontent.com/91018876/206919509-3ea9edf6-c884-4e31-83ab-19a4a1babac7.png)

```
P-value sebesar 0.8054 > 0.05 ,sehingga varians dari ketiga kelompok sama.
Oleh karena itu, terdapat homogenitas varians untuk melakukan anova satu arah (one way).
```
c. Untuk uji ANOVA, buatlah model linier dengan Panjang versus Grup dan beri nama model tersebut model 1.

```R
model1 = lm(Length ~ Group, data = oneway_anova)
anova(model1)
```
![image](https://user-images.githubusercontent.com/91018876/206919614-5c435bcd-03cf-48d1-9b56-6d1d2555c17a.png)

d. Dari Hasil Poin C , Berapakah nilai-p ? ,  Apa yang dapat Anda simpulkan dari H0?
```
F-value = 7.0982, yang menunjukkan nilai P-value < 0.05. 
Kesimpulannya tolak H0, sehingga terbukti bahwa adanya perbedaan panjang antara ketiga spesies atau terdapat rata-rata panjangnya yang berbeda dengan yang lainnya.
```

e. Verifikasilah jawaban model 1 dengan Post-hooc test TukeyHSD ,  dari nilai p yang didapatkan apakah satu jenis kucing lebih panjang dari yang lain? Jelaskan.
```R
TukeyHSD(aov(model1))
```
![image](https://user-images.githubusercontent.com/91018876/206919852-25c67cf0-d61b-4a2a-a6c4-f1821f32de68.png)
```
Jika perbandingan antar spesies kucing memiliki nilai p > 0.05, maka kedua spesies memiliki panjang yang sama dan juga sebaliknya. 
Contoh: hasil kucing putih dan kucing oren memiliki panjang sama p = 0.8726158. Sedangkan kucing hitam dengan kucing oren memiliki panjang yang berbeda p = 0.0020955. 
```

f. Visualisasikan data dengan ggplot2
```R

ggplot(oneway_anova, aes(x = Group, y = Length)) +
  geom_boxplot(fill= "grey", color = c("#FFA500", "#000000","#FFFFFF")) +
  scale_x_discrete() + xlab("Group") + ylab("Length (cm)")
```
![image](https://user-images.githubusercontent.com/91018876/206920681-0668d1b5-e32b-491d-b36b-c2e6be248320.png)

5.(Anova dua arah) 
Data yang digunakan merupakan hasil eksperimen yang dilakukan untuk mengetahui pengaruh suhu operasi (100˚C, 125˚C dan 150˚C) dan tiga jenis kaca pelat muka (A, B dan C) pada keluaran cahaya tabung osiloskop. Percobaan dilakukan sebanyak 27 kali dan didapat data sebagai berikut: Data Hasil Eksperimen. Dengan data tersebut: 

a. Buatlah plot sederhana untuk visualisasi data 
```R
Membaca dari csv file
tabel = read.csv("D:\\Kuliah\\SEM 3\\Probabilitas dan Statistika\\Praktikum\\2\\GTL.csv")

head(tabel)
str(tabel)
```
![image](https://user-images.githubusercontent.com/91018876/206921400-65b3bced-80ea-40fd-8630-d8c64b46fbfc.png) ![image](https://user-images.githubusercontent.com/91018876/206921416-1ab737a6-7faf-4a80-9ca1-90b2aba1b15e.png)

```R
Plot sederhana

qplot(x = Temp, y = Light, geom = "point", data = tabel) + facet_grid(.~Glass, labeller = label_both)
```
![image](https://user-images.githubusercontent.com/91018876/206921434-04c6be8f-197c-4b1c-9a54-8c7b63a2a986.png)

b. Lakukan uji ANOVA dua arah untuk 2 faktor
```R
Set tabel menjadi factor
tabel$Glass <- as.factor(tabel$Glass)
tabel$Temp <- as.factor(tabel$Temp)
```
```R
Uji ANOVA 2 arah
anova <- aov(Light ~ Glass*Temp, data=tabel)
summary(anova)
```
![image](https://user-images.githubusercontent.com/91018876/206921566-9d5e902d-6bdd-4d68-94a9-f38d3ae184e5.png)

c. Tampilkan tabel dengan mean dan standar deviasi keluaran cahaya untuk setiap perlakuan (kombinasi kaca pelat muka dan suhu operasi)

```R
library(magrittr)
library(dplyr)
data_sum <- group_by(tabel, Glass, Temp) %>%
  summarise(mean=mean(Light), sd=sd(Light)) %>%
  arrange(desc(mean))
print(data_sum)
```
![image](https://user-images.githubusercontent.com/91018876/206921841-ae129b43-14e7-403c-bfd5-7f142379c514.png)

d. Lakukan uji Tukey
```R
tukey <- TukeyHSD(anova)
print(tukey)
```
![image](https://user-images.githubusercontent.com/91018876/206990417-8c33e7ea-d9e4-4fcc-a83d-3c8fca42f098.png)

e. Gunakan compact letter display untuk menunjukkan perbedaan signifikan antara uji Anova dan uji Tukey

```R
Membuat compact letter display
install.packages("multcompView")
library(multcompView)

tukey.cld <- multcompLetters4(anova, tukey)
print(tukey.cld)
```
![image](https://user-images.githubusercontent.com/91018876/206992148-35ea1c66-17ad-4bd7-bdc9-119034f4fdaa.png)

```R
Menambahkan means dan sd pada tabel compact letter display
cld <- as.data.frame.list(tukey.cld$`Glass:Temp_Factor`)
data_sum$Tukey <- cld$Letters
print(data_sum)
```
![image](https://user-images.githubusercontent.com/91018876/206992390-46200bf3-5fc1-40ac-b4a9-977bc09a285a.png)
