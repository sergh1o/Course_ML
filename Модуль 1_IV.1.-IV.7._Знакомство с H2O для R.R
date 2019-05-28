# загружаем пакет h2o
library(h2o)

# запускаем кластер H2O
h2o.init(nthreads = -1, max_mem_size = "8G")

# задаем рабочий каталог
setwd("/Users/artemgruzdev/Documents/Курс/Course_ML/Data")

# загружаем данные
data <- read.csv2("StateFarm_for_H2O.csv", 
                  sep = ";", 
                  na.strings = "")

# разбиваем данные на обучающие и тестовые
set.seed(100)
ind <- sample(2, nrow(data), 
              replace = TRUE, 
              prob = c(0.7, 0.3))
development <- data[ind == 1, ]
holdout <- data[ind == 2, ]

# преобразовываем датафреймы во фреймы H2O
tr <- as.h2o(development)
tst <- as.h2o(holdout)

# отключаем экспоненциальную запись
options(scipen=999)

# смотрим содержимое фрейма
h2o.describe(tr)

# преобразовываем в категориальную переменную
tr$Response <- h2o.asfactor(tr$Response)
tst$Response <- h2o.asfactor(tst$Response)

# смотрим содержимое фрейма
h2o.describe(tr)

# задаем имя зависимой переменной и
# имена предикторов
dependent <- "Response"
predictors <- setdiff(names(tr), "Response")

# обучаем модель
forest_model <- h2o.randomForest(x = predictors, 
                                 y = dependent, 
                                 training_frame = tr, 
                                 validation_frame = tst, 
                                 seed = 42) 

# смотрим модель
forest_model

# получаем спрогнозированные значения и 
# спрогнозированные вероятности классов 
# зависимой переменной
predictions <- h2o.predict(forest_model, newdata = tst)
predictions

# завершаем работу с H2O
h2o.shutdown(prompt = TRUE)
