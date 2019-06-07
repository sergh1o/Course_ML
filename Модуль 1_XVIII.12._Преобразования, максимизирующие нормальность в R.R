
# задаем рабочий каталог
setwd("/Users/artemgruzdev/Documents/Курс/Course_ML/Data")

# загружаем данные
development <- read.csv2("Normality.csv", 
                         sep = ";", 
                         dec=".",
                         na.strings = "")
# выводим первые 5 наблюдений
head(development, 5)

# смотрим типы переменных
str(development)

# устанавливаем пакет rcompanion
# install.packages("rcompanion")

# устанавливаем пакет e1071
# install.packages("e1071")

# загружаем пакет rcompanion
library(rcompanion)
# загружаем пакет e1071
library(e1071)

# строим гистограмму распределения
# переменной PERSONAL_INCOME
plotNormalHistogram(development$PERSONAL_INCOME)

# строим график квантиль-квантиль
# для переменной PERSONAL_INCOME
qqnorm(development$PERSONAL_INCOME,
       xlab = "Квантили теоретического распределения",
       ylab = "Квантили наблюдаемого распределения")
qqline(development$PERSONAL_INCOME, col = "red")

# вычисляем скос и эксцесс
skew <- skewness(development$PERSONAL_INCOME)
kurt <- kurtosis(development$PERSONAL_INCOME)
results <- c(skew, kurt)
names(results) <- c("скос", "эксцесс")
results

# приравняем возможные отрицательные значения к нулю
development$PERSONAL_INCOME <- ifelse(development$PERSONAL_INCOME < 0, 
                                      0, 
                                      development$PERSONAL_INCOME)

# выполняем обратное преобразование переменной PERSONAL_INCOME, 
# используем константу, чтобы не брать нулевые значения
a <- 0.001
var_reciprocal <- 1 / (development$PERSONAL_INCOME + a)

# выводим гистограмму распределения для переменной var_reciprocal
plotNormalHistogram(var_reciprocal)

# выводим график квантиль-квантиль для переменной var_reciprocal
qqnorm(var_reciprocal, 
       xlab = "Квантили теоретического распределения",
       ylab = "Квантили наблюдаемого распределения")
qqline(var_reciprocal, col = "red")

# вычисляем скос и эксцесс
skew_rec <- skewness(var_reciprocal)
kurt_rec <- kurtosis(var_reciprocal)
results <- c(skew_rec, kurt_rec)
names(results) <- c("скос", "эксцесс")
results

# выполняем логарифмическое преобразование переменной PERSONAL_INCOME, 
# используем константу, чтобы не брать нулевые значения
var_log <- log(development$PERSONAL_INCOME + a)

# выводим гистограмму распределения для переменной var_log
plotNormalHistogram(var_log)

# выводим график квантиль-квантиль для переменной var_log
qqnorm(var_log, 
       xlab = "Квантили теоретического распределения",
       ylab = "Квантили наблюдаемого распределения")
qqline(var_log, col = "red")

# вычисляем скос и эксцесс
skew_log <- skewness(var_log)
kurt_log <- kurtosis(var_log)
results <- c(skew_log, kurt_log)
names(results) <- c("скос", "эксцесс")
results

# выполняем логарифмическое преобразование переменной PERSONAL_INCOME, 
# где k - небольшое значение, близкое к 0,
# чтобы сильнее смещать распределение влево
k <- 0.001
var_log_min_k <- log(((development$PERSONAL_INCOME + a) / mean(development$PERSONAL_INCOME)) + k)
# выводим гистограмму распределения для переменной var_log_min_k
plotNormalHistogram(var_log_min_k)
                     
# выводим график квантиль-квантиль для переменной var_log_min_k
qqnorm(var_log_min_k, 
       xlab = "Квантили теоретического распределения",
       ylab = "Квантили наблюдаемого распределения")
qqline(var_log_min_k, col = "red")

# вычисляем скос и эксцесс
skew_log_min_k <- skewness(var_log_min_k)
kurt_log_min_k <- kurtosis(var_log_min_k)
results <- c(skew_log_min_k, kurt_log_min_k)
names(results) <- c("скос", "эксцесс")
results

# выполняем логарифмическое преобразование переменной PERSONAL_INCOME, 
# где k - небольшое значение, близкое к 1,
# чтобы слабее смещать распределение влево
k <- 0.6
var_log_max_k <- log(((development$PERSONAL_INCOME + a) / mean(development$PERSONAL_INCOME)) + k)

# выводим гистограмму распределения для переменной var_log_max_k
plotNormalHistogram(var_log_max_k)

# выводим график квантиль-квантиль для переменной var_log_max_k
qqnorm(var_log_max_k, 
       xlab = "Квантили теоретического распределения",
       ylab = "Квантили наблюдаемого распределения")
qqline(var_log_max_k, col = "red")

# вычисляем скос и эксцесс
skew_log_max_k <- skewness(var_log_max_k)
kurt_log_max_k <- kurtosis(var_log_max_k)
results <- c(skew_log_max_k, kurt_log_max_k)
names(results) <- c("скос", "эксцесс")
results

# выполняем преобразование корнем четвертой степени,
# используем модуль, чтобы не вычислять корни
# отрицательных чисел, и затем учитываем знак числа
var_frthsq <- sign(development$PERSONAL_INCOME) * abs(
  development$PERSONAL_INCOME)^(1/4)

# выводим гистограмму распределения переменной var_frthsq
plotNormalHistogram(var_frthsq)

# выводим график квантиль-квантиль для переменной var_frthsq
qqnorm(var_frthsq, 
       xlab = "Квантили теоретического распределения",
       ylab = "Квантили наблюдаемого распределения")
qqline(var_frthsq, col = "red")

# вычисляем скос и эксцесс
skew_frthsq <- skewness(var_frthsq)
kurt_frthsq <- kurtosis(var_frthsq)
results <- c(skew_frthsq, kurt_frthsq)
names(results) <- c("скос", "эксцесс")
results

# выполняем преобразование кубическим корнем,
# используем модуль, чтобы не вычислять корни
# отрицательных чисел, и затем учитываем знак числа
var_сubsq <- sign(development$PERSONAL_INCOME) * abs(
  development$PERSONAL_INCOME)^(1/3)

# выводим гистограмму распределения переменной var_сubsq
plotNormalHistogram(var_сubsq)

# выводим график квантиль-квантиль для переменной var_сubsq
qqnorm(var_сubsq, 
       xlab = "Квантили теоретического распределения",
       ylab = "Квантили наблюдаемого распределения")
qqline(var_сubsq, col = "red")

# вычисляем скос и эксцесс
skew_сubsq <- skewness(var_сubsq)
kurt_сubsq <- kurtosis(var_сubsq)
results <- c(skew_сubsq, kurt_сubsq)
names(results) <- c("скос", "эксцесс")
results

# выполняем преобразование квадратным корнем,
# используем модуль, чтобы не вычислять корни
# отрицательных чисел, и затем учитываем знак числа
var_sq <- sign(development$PERSONAL_INCOME) * abs(
  development$PERSONAL_INCOME)^(1/2)

# выводим гистограмму распределения переменной var_sq
plotNormalHistogram(var_sq)

# выводим график квантиль-квантиль для переменной var_sq
qqnorm(var_sq, 
       xlab = "Квантили теоретического распределения",
       ylab = "Квантили наблюдаемого распределения")
qqline(var_sq, col = "red")

# вычисляем скос и эксцесс
skew_sq <- skewness(var_sq)
kurt_sq <- kurtosis(var_sq)
results <- c(skew_sq, kurt_sq)
names(results) <- c("скос", "эксцесс")
results

# устанавливаем пакет car
# install.packages("car")

# вычисляем лямбду преобразования Бокса-Кокса
# с помощью функции powerTransform пакета car
library(car)
powerTransform(development$PERSONAL_INCOME)

# выполняем преобразование с помощью вычисленной лямбды,
# используя функцию bcPower() пакета car
var_boxcox <-bcPower(development$PERSONAL_INCOME, -0.04318456)

# выводим гистограмму распределения переменной var_boxcox
plotNormalHistogram(var_boxcox)
# выводим график квантиль-квантиль для переменной var_boxcox
qqnorm(var_boxcox, 
       xlab = "Квантили теоретического распределения",
       ylab = "Квантили наблюдаемого распределения")
qqline(var_boxcox, col = "red")

# вычисляем скос и эксцесс
skew_boxcox <- skewness(var_boxcox)
kurt_boxcox <- kurtosis(var_boxcox)
results <- c(skew_boxcox, kurt_boxcox)
names(results) <- c("скос", "эксцесс")
results

# загружаем пакет caret
library(caret)

# строим модель – вычисляем значения лямбда, необходимые для преобразования
trans <- preProcess(development], method = "BoxCox")

# применяем модель к обучающему набору
development <- predict(trans, development)