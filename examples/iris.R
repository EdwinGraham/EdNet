library(EdNet)

data("iris")

plot(iris$Sepal.Length, iris$Sepal.Width, col=iris$Species, pch=16)

X <- sapply(iris[, 1:4], normalise)
Y <- onehotEncode(iris$Species)

set.seed(1984)
holdoutRows <- sample(1:150, 20)

modelFit <- EdNetTrain(X, Y,
                       family="multiclass",
                       learning_rate=0.01,
                       num_epochs=40,
                       hidden_layer_dims=c(128, 64, 32),
                       hidden_layer_activations="relu",
                       optimiser="Adam",
                       lambda = 0.001,
                       keep_prob=0.5,
                       mini_batch_size=16,
                       print_every_n=1L,
                       seed=1984,
                       plot=TRUE,
                       dev_set=holdoutRows)

classProbs <- predict(modelFit, X)
iris$predictions <- predictedClass(classProbs, levels(iris$Species))

par(mfrow=c(1,2))
plot(iris$Sepal.Length[dev_set], iris$Sepal.Width[dev_set], col=iris$Species[dev_set], pch=16)
plot(iris$Sepal.Length[dev_set], iris$Sepal.Width[dev_set], col=iris$predictions[dev_set], pch=16)

### For Debugging
funcs <- list.files("./R")
for(f in funcs){
  source(paste0("./R/", f))
}

family <- "multiclass"
learning_rate <- 0.05
num_epochs <- 10
hidden_layer_dims <- c(128, 64, 32)
hidden_layer_activations <- "relu"
optimiser <- "Adam"
keep_prob <- 0.5
input_keep_prob <- NULL
alpha <- 0
lambda <- 0
mini_batch_size <- NULL
dev_set <- NULL
beta1 <- 0.9
beta2 <- 0.999
epsilon <- 1E-8
print_every_n <- 1L
seed <- 1984L
plot <- TRUE
checkpoint <- NULL
keep <- FALSE



newdata <- X
object <- modelFit
