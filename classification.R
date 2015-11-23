library(e1071)

index <- sample(1:20000, 15000)
train <- TOPICS[index, ]
cate <- sapply(row.names(TOPICS), FUN = function(x) strsplit(x, "_")[[1]][2])
cate_train <- as.factor(cate[index])

N <- length(cate_train)
SVM_model <- tune('svm',  train, cate_train, ranges = list(class.weights = list(N/table(cate_train)), gamma = 10^(-6:-1), cost = 10^(-1:5)), kernel = 'radial', type = 'C-classification', tunecontrol = tune.control(sampling = 'cross', cross = 5))
