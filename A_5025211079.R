#1
bef = c(78, 75, 67, 77, 70, 72, 78, 74, 77)
aft = c(100, 95, 70, 90, 90, 90, 89, 90, 100)

dif = bef - aft

data <- data.frame( 
  before = rep(c(before)),
  after = c(after),
  dif = c(dif)
)
#a
print(data)
std = sd(dif)
zbar = mean(dif)
n = length(dif)

print(std)

#b
tvalue <- zbar/(std/sqrt(n))
print(tvalue)

pvalue <- 2*pt(tvalue, df=n-1)
print(pvalue)

res <- t.test(after, before, paired = TRUE)
print(res)

#2
library(BSDA)
xbar = 23500
n = 100
std = 3900
mu = 20000
zsum.test(xbar, std, n, alternative = "greater",
          mu=20000, conf.level=0.95)

#3

library(mosaic)
library(BSDA)

#Diketahui
xbar1 = 3.64
xbar2 = 2.79

n1 = 19
n2 = 27

std1 = 1.67
std2 = 1.332

#Asumsikan variance sama
df = (n1+n2-2)
sp =sqrt(((n1-1)*(std1**2) + (n2-1)*(std2**2))/df)

#b
tvalue = (xbar1-xbar2)/(sp*sqrt((1/n1) + (1/n2)))

print(tvalue)

#c
plotDist(dist = 't', df = 2, col = "blue")

#d 
alpha = 0.05
crivalue1 = qnorm(alpha/2)
crivalue2 = qnorm(1-alpha/2)

print(crivalue1)
print(crivalue2)

#4
#a
oneway_anova = read.table("D:\\Kuliah\\SEM 3\\Probabilitas dan Statistika\\Praktikum\\2\\data_soal4.txt", h=T)

oneway_anova$Group <- as.factor(oneway_anova$Group)


oneway_anova$Group = factor(oneway_anova$Group, labels = c("kucing oren", "kucing hitam", "kucing putih"))

group1 <- subset(oneway_anova, Group == "kucing oren")
group2 <- subset(oneway_anova, Group == "kucing hitam")
group3 <- subset(oneway_anova, Group == "kucing putih")


qqnorm(group1$Length)
qqline(group1$Length)

qqnorm(group2$Length)
qqline(group2$Length)

qqnorm(group3$Length)
qqline(group3$Length)

#b
bartlett.test(oneway_anova$Length, oneway_anova$Group)

#c
model1 = lm(Length ~ Group, data = oneway_anova)
anova(model1)

#e
TukeyHSD(aov(model1))

#f
install.packages("ggplot2")
library(ggplot2)

ggplot(oneway_anova, aes(x = Group, y = Length)) +
  geom_boxplot(fill= "grey", color = c("#FFA500", "#000000","#FFFFFF")) +
  scale_x_discrete() + xlab("Group") + ylab("Length (cm)")

#5
#a
tabel = read.csv("D:\\Kuliah\\SEM 3\\Probabilitas dan Statistika\\Praktikum\\2\\GTL.csv")

print(tabel)
head(tabel)
str(tabel)

library(ggplot2)
qplot(x = Temp, y = Light, geom = "point", data = tabel) + facet_grid(.~Glass, labeller = label_both)

#b
tabel$Glass <- as.factor(tabel$Glass)
tabel$Temp <- as.factor(tabel$Temp)

#c
anova <- aov(Light ~ Glass*Temp, data=tabel)
summary(anova)

#d
library(magrittr)
library(dplyr)
data_sum <- group_by(tabel, Glass, Temp) %>%
  summarise(mean=mean(Light), sd=sd(Light)) %>%
  arrange(desc(mean))
print(data_sum)

#e
tukey <- TukeyHSD(anova)
print(tukey)

#f
install.packages("multcompView")
library(multcompView)

tukey.cld <- multcompLetters4(anova, tukey)
print(tukey.cld)

cld <- as.data.frame.list(tukey.cld$`Glass:Temp_Factor`)
data_sum$Tukey <- cld$Letters
print(data_sum)


