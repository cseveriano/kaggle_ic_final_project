library('readr')
library('R.utils')
dirData = "C:\\Users\\Carlos\\Dropbox\\Doutorado\\Disciplinas\\IntroducaoIC\\Trabalho Final\\kaggle_ic_final_project"

setwd(dirData)
### Read as Data Frames
data.tr <- readr::read_csv('train.csv.zip')
data.te <- readr::read_csv('test.csv.zip')
### Tranform target variable to factor
data.tr$Target <- factor(data.tr$Target, levels = paste0('class', 1:7))
### Convert Data Frames to matrices
x <- as.matrix( subset(data.tr, select = -c(Id, Target)) )
x.new <- as.matrix( subset(data.te, select = -c(Id)) )
y <- data.tr$Target



### Fit GLM using one-against-all
glm.model <- lapply(levels(y), function(y.class) {
  y.num <- as.numeric(y == y.class)
  glm(y ~., data.frame(x, y = y.num), family = binomial(link = "logit"))
})
### Save model
saveRDS(glm.model, file = 'glm_model.rds.gzip', compress = TRUE)
# glm.model <- readRDS('glm_model.rds.gzip')
### Predict probabilities
glm.pred <- lapply(glm.model, function(model) {
  predict(model, as.data.frame(x.new), type = 'response')
})
glm.pred <- do.call('cbind', glm.pred)



### Normalize probabilities (rowSums = 1)
glm.pred <- sweep(glm.pred, 1, rowSums(glm.pred), '/')
### Save prediction
glm.pred <- round(glm.pred, 8)
glm.pred <- as.data.frame(glm.pred)
names(glm.pred) <- levels(y)
glm.pred <- cbind.data.frame(Id = as.integer(data.te$Id), glm.pred)
write.csv(glm.pred, file = gzfile('glm_pred.csv.gz'), row.names = FALSE)