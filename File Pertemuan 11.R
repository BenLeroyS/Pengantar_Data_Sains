#Memuat data library di R
library(ggplot2)
library(reshape2)
library(lmtest)
library(car)

#Sisipkan data mtcars di R
data(mtcars)

#Melakukan preview data mtcars
head(mtcars)
summary(mtcars)
str(mtcars)

#Membuat heatmap korelasi data
cor_matrix <- cor(mtcars)  
melted_cor <- melt(cor_matrix) 
ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low="blue", high="red", mid="green", midpoint=0)

#Melakukan uji asumsi normalitas 
model <- lm(mpg ~ wt +hp, data=mtcars) 
shapiro.test(residuals(model))
qqnorm(residuals(model))
qqline(residuals(model))

#Melakukan uji asumsi Heteroskedastisitas
bptest(model)
plot(fitted(model), residuals(model))
abline(h=0, col="orange")

#Melakukan uji asumsi Multikolinieritas 
model2 <- lm(mpg ~ wt + hp, data=mtcars)
vif(model2) # Jika memiliki multiple predictors

#Melakukan uji asumsi autokorelasi
dwtest(model)

#Melakukan pemodelan regresi
model <- lm(mpg ~ wt, data=mtcars)
summary(model) # Melakukan ringkasan model

#Melakukan R-Square
summary(model)$r.squared

#Melakukan inteprestasi model
summary(model)
# Interprestasi:
# 1. Model regresi berganda: 30.29 (mpg) - 5.3445 
# 2. Setiap penambahan 1 unit berat (wt), mpg berkurang 5.3445
# 3. R-square: 0.8269
# 4. Nilai p < 0.05 berarti model signifikan
