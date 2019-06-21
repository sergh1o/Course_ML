# задаем рабочий каталог
setwd("/Users/artemgruzdev/Documents/Курс/Course_ML/Модуль_1/Data")

# загружаем пакет e1071
library(e1071)

# загружаем и смотрим данные
data <- read.csv2("glucose.csv", dec=".")
data

# проверяем данные на нормальность
skew_before <- skewness(data$before)
kurt_before <- kurtosis(data$before)
before_results <- c(skew_before, kurt_before)
names(before_results) <- c("скос признака before", 
                           "эксцесс признака before")
before_results

skew_after <- skewness(data$after)
kurt_after <- kurtosis(data$after)
after_results <- c(skew_after, kurt_after)
names(after_results) <- c("скос признака after", 
                          "эксцесс признака after")
after_results

summary(data)

# вычисляем разность каждой пары значений
diff = data$before - data$after

# вычисляем среднее разностей
mean_diff = sum(diff) / nrow(data)
mean_diff

# вычисляем стандартное отклонение разностей от среднего
std_diff = sqrt((sum((mean_diff - diff) ^ 2)) / (nrow(data) - 1))
std_diff

# еще можно так
# sd(diff)

# вычисляем t-критерий Стьюдента для зависимых выборок
t = mean_diff / (std_diff / sqrt(nrow(data)))
t

# можно воспользоваться функцией t.test()
t.test(data$before, data$after, paired=TRUE)