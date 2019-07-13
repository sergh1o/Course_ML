# задаем рабочий каталог
setwd("/Users/artemgruzdev/Documents/Курс/Course_ML/Модуль_2/Data")

# считываем CSV-файл в датафрейм data
data <- read.csv2("bankloan.csv", dec = ",")

# выводим первые 5 наблюдений датафрейма
head(data, 5)

# смотрим типы переменных
str(data)

# преобразовываем переменную default в фактор,
# при этом значения 0 и 1 запишем как Good и Bad
data$default <- factor(data$default, levels = c(0, 1), 
                       labels = c("Good", "Bad"))

# задаем стартовое значение генератора случайных
# чисел для воспроизводимости результатов разбиения
set.seed(42)

# создаем переменную random_number, которая случайным
# образом разбивает набор данных на обучающую
# и контрольную выборки в соотношении 70% на 30%
random_number <- runif(nrow(data), 0, 1)
development <- data[random_number > 0.3, ]
holdout <- data[random_number <= 0.3, ]

# загружаем пакет rpart
library(rpart)

# строим модель дерева классификации
set.seed(42)
model <- rpart(default ~ ., method = "class", data = development)

# вычисляем правильность на тестовой выборке
predvalues_hold <- predict(model, holdout, type = "class")
cm <- as.matrix(table(holdout$default, predvalues_hold))
accuracy_hold <- sum(diag(cm)) / sum(cm) 
accuracy_hold

# загружаем необходимые пакеты
library(caret)
library(randomForest)
library(e1071)
library(pROC)

# создаем массив признаков и массив меток
features <- setdiff(names(data), "default")
x <- data[, features]
y <- data$default

# задаем стратегию проверки
set.seed(42)
train_control <- trainControl(method = "cv", number = 10)
model <- train(x = x, y = y, 
               method = "rf",
               trControl = train_control, 
               tuneGrid=data.frame(mtry = floor(sqrt(ncol(x)))))
print(model)

# задаем стратегию проверки
set.seed(42)
train_control <- trainControl(method = "cv", number = 10, 
                              classProbs = TRUE, 
                              summaryFunction = twoClassSummary)
model <- train(x = x, 
               y = y, 
               method = "rf",
               trControl = train_control,
               tuneGrid=data.frame(mtry = floor(sqrt(ncol(x)))),
               metric = "ROC")
print(model)

# задаем стратегию проверки
set.seed(42)
train_control <- trainControl(method = "repeatedcv",    
                              number = 10, repeats=5)
model <- train(x = x, 
               y = y, 
               method = "rf",
               trControl = train_control,
               tuneGrid=data.frame(mtry = floor(sqrt(ncol(x)))))
print(model)

# задаем стратегию проверки
set.seed(42)
train_control <- trainControl(method = "LGOCV", p=0.7)
model <- train(x = x, 
               y = y, 
               method = "rf",
               trControl = train_control,
               tuneGrid=data.frame(mtry = floor(sqrt(ncol(x)))))
print(model)


# еще вариант

# сюда будем записывать значения правильности
acc <- numeric(10)

# выполняем 10-кратное случайное разбиение
# на обучающую  и тестовую выборки
set.seed(42)
for (i in 1:10) {
  random_number <- runif(nrow(data), 0, 1)
  development <- data[random_number > 0.3, ]
  holdout <- data[random_number <= 0.3, ]
  model <- rpart(default ~ ., method = "class", data = development)
  actual <- holdout$default
  predicted <- predict(model, holdout, type = "class")
  cm <- as.matrix(table(actual, predicted))
  n <- sum(cm)
  diag <- diag(cm)
  accuracy <- sum(diag) / n 
  acc[i] <- accuracy
}
# печатаем значения правильности
acc
# печатаем среднее значение правильности
mean_acc <- c("Среднее значение правильности" = mean(acc))
mean_acc

# задаем стратегию проверки, для уменьшения
# времени вычислений сократим количество
# деревьев в ансамбле до 10 и увеличив
# минимальный размер терминального узла
train_control <- trainControl(method = "LOOCV")
num_of_cols = floor(sqrt(ncol(x)))
model <- train(x = x, 
               y = y, 
               method = "rf", ntree=10, nodesize=200,
               trControl = train_control,
               tuneGrid=data.frame(mtry = num_of_cols))
print(model)

# задаем сетку гиперпараметров для решетчатого поиска
tunegrid <- expand.grid(mtry = c(1:7))

# задаем набор условий для оптимизации по правильности
control_acc <- trainControl(method = "cv", 
                            number = 5, 
                            search = "grid")

# строим модели случайного леса и выбираем 
# оптимальную с т.з. правильности
set.seed(152)
rf_gridsearch_acc <- train(default ~ ., 
                           data = development, 
                           method = "rf", 
                           tuneGrid = tunegrid, 
                           trControl = control_acc)

# выводим результаты решетчатого поиска 
print(rf_gridsearch_acc)

# визуализируем результаты решетчатого поиска 
plot(rf_gridsearch_acc)

# вычисляем правильность оптимальной модели
# на тестовой выборке, не участвовшей
# в обучении и настройке гиперпараметров
predval_hold <- predict(rf_gridsearch_acc, holdout)
cm <- as.matrix(table(holdout$default, predval_hold))
accuracy_hold <- sum(diag(cm)) / sum(cm) 
accuracy_hold

# задаем набор условий для оптимизации по AUC
control_auc <- trainControl(method = "cv", 
                            number = 5, 
                            search = "grid",
                            classProbs = TRUE, 
                            summaryFunction = twoClassSummary)

# строим модели случайного леса и выбираем оптимальную с т.з. AUC
set.seed(152)
rf_gridsearch_auc <- train(default ~ ., 
                           data = development, 
                           method = "rf", 
                           metric = "ROC",
                           tuneGrid = tunegrid, 
                           trControl = control_auc)

# выводим результаты решетчатого поиска 
print(rf_gridsearch_auc)

# визуализируем результаты решетчатого поиска 
plot(rf_gridsearch_auc)

# вычисляем AUC оптимальной модели
# на контрольной выборке, не участвовшей
# в обучении и настройке гиперпараметров
prob <- predict(rf_gridsearch_auc, holdout, type = "prob")
roc(holdout$default, prob[, 2], ci = TRUE)

# строим ROC-кривую оптимальной модели
# на контрольной выборке, не участвовшей
# в обучении и настройке гиперпараметров
plot(roc(holdout$default, prob[, 2], ci = TRUE))

# считываем CSV-файл в датафрейм data
timeseries_data <- read.csv2("Flats.csv")
# выводим первые 5 наблюдений датафрейма
head(timeseries_data, 5)

# преобразовываем в формат даты
timeseries_data$Date_Create <- as.POSIXct(timeseries_data$Date_Create, 
                                          format="%d.%m.%Y")
# сортируем данные по дате сделки
# (от самой ранней к самой поздней)
timeseries_data = timeseries_data[order(timeseries_data$Date_Create), ]
timeseries_data

# разбиваем данные на обучающую и тестовую выборки
sampleSizeTrain <- floor(0.70 * nrow(timeseries_data))
timeseries_train <- timeseries_data[1:sampleSizeTrain, ]
timeseries_test <- timeseries_data[(sampleSizeTrain+1):nrow(timeseries_data), ]

# смотрим обучающую выборку
timeseries_train

# смотрим тестовую выборку
timeseries_test

# считываем CSV-файл в датафрейм
wellsfargo_data <- read.csv2("wellsfargo.csv")
# выводим первые 5 наблюдений датафрейма
head(wellsfargo_data, 5)

# преобразовываем в формат даты
wellsfargo_data$date <- as.Date(wellsfargo_data$date)
# преобразовываем в фактор
wellsfargo_data$response <- as.factor(wellsfargo_data$response)

# сортируем данные по дате
# (от самой ранней к самой поздней)
wellsfargo_data <- wellsfargo_data[order(wellsfargo_data$date), ]
wellsfargo_data

# удаляем date
wellsfargo_data <- wellsfargo_data[, -1]

# выполняем разбиение на обучающую и тестовую выборки, 
# учитывающее временную структуру
sampleSizeTrain <- floor(0.70 * nrow(wellsfargo_data))
tr <- wellsfargo_data[1:sampleSizeTrain, ]
tst <- wellsfargo_data[(sampleSizeTrain+1):nrow(wellsfargo_data), ]

# строим модель дерева классификации
set.seed(42)
model <- rpart(response ~ ., method = "class", data = tr)

# загружаем пакет ROCR
library(ROCR)

# вычисляем AUC на обучающей и тестовой выборках
prob_tr <- predict(model, tr, type = "prob")
pred_tr <- prediction(prob_tr[, 2], tr$response)
prob_tst <- predict(model, tst, type = "prob")
pred_tst <- prediction(prob_tst[, 2], tst$response)
auc_tr <- performance(pred_tr,"auc")@y.values[[1]]
auc_tst <- performance(pred_tst,"auc")@y.values[[1]]
output <- c("AUC на обучающей выборке" = auc_tr,
            "AUC на тестовой выборке" = auc_tst)
output

# пишем функцию, вычисляющую правильность
accuracy <- function(model){
  actual <- holdout$response
  predicted <- predict(model, holdout, type = "class")
  cm <- as.matrix(table(actual, predicted))
  n <- sum(cm)
  diag <- diag(cm)
  accuracy <- sum(diag) / n 
  return(accuracy)
}

# задаем количество бутстреп-выборок
R <- 100
# задаем количество элементов для обучающих бутстреп-выборок
k <- nrow(development)
# задаем количество элементов для контрольных бутстреп-выборок
n <- nrow(holdout)

# сюда будем записывать значения правильности и AUC 
tree.acc_boot <- numeric(100)
tree.auc_boot <- numeric(100)

# выполняем бутстреп, на каждой итерации формируем на основе исходной обучающей выборки 
# бутстреп-выборку, формируем на основе исходной тестовой выборки бутстреп-выборку, строим 
# модель по бутстреп-выборке, сгенерированной на основе исходной обучающей выборки, проверяем ее 
# на бутстреп-выборке, сгенерированной на основе исходной тестовой выборки
set.seed(701)
for(i in 1:R){
  obs_dev.boot <- sample(x = 1:k, size = k, replace = TRUE)
  development.boot <- development[obs_dev.boot, ]
  obs_hold.boot <- sample(x = 1:n, size = n, replace = TRUE)
  holdout.boot <- holdout[obs_hold.boot, ]
  tree <- rpart(default ~ . , data = development.boot)
  tree.score_boot <- predict(tree, holdout.boot, type = "prob")
  tree.roc_boot <- roc(holdout.boot$default, tree.score_boot[, 2])
  tree.auc_boot[i] <- tree.roc_boot$auc
  tree.acc_boot[i] <- accuracy(tree)
}

# вычисляем среднее значение правильности
mean_acc <- mean(tree.acc_boot)
# вычисляем среднее значение AUC
mean_auc <- mean(tree.auc_boot)

# печатаем средние значения правильности и AUC
output <- c("среднее значение правильности" = mean_acc, 
            "среднее значение AUC" = mean_auc)
output

# задаем стартовое значение генератора
# случайных чисел
set.seed(42)
# генеририруем 50 случайных чисел
# в диапазоне от 45 до 100
income <- runif(50, min = 45, max = 100)

# вычисляем среднее значение дохода
mean_income <- mean(income)
mean_income

# записываем информацию о размере выборки
N <- 50

# вычисляем предел погрешности 
err <- 1.645 * (sd(income) / sqrt(N))

# вычисляем нижнюю границу 90%-ного
# доверительного интервала 
mean_income - err

# вычисляем верхнюю границу 90%-ного
# доверительного интервала 
mean_income + err

# создаем 1000 бутстреп-выборок и вычисляем
# 1000 средних значений дохода
boot <- replicate(1000, 
                  mean(sample(income, 
                              replace = TRUE)))

# вычисляем нижнюю границу 95%-ного
# доверительного интервала
quantile(boot, 0.025)

# вычисляем верхнюю границу 95%-ного
# доверительного интервала
quantile(boot, 0.975)

# вычисляем бутстрепированный 95%-ный 
# доверительный интервал правильности
tree_acc.ci = quantile(tree.acc_boot, c(.025,.975))
# вычисляем бутстрепированный 95%-ный 
# доверительный интервал AUC
tree_auc.ci = quantile(tree.auc_boot, c(.025,.975))
# печатаем бутстрепированные 95%-ные доверительные
# интервалы правильности и AUC
results <- list(tree_acc.ci, tree_auc.ci)
acc_string = "бутстрепированный 95%-ный дов. интервал правильности"
auc_string = "бутстрепированный 95%-ный дов. интервал AUC"
names(results) <- c(acc_string, auc_string)
print(results)
