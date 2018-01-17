library(EdNet)

funcs <- list.files("./R")
for(f in funcs){
  source(paste0("./R/", f))
}


data("iris")

plot(iris$Sepal.Length, iris$Sepal.Width, col=iris$Species, pch=16)

X <- sapply(iris[, 1:4], normalise)
Y <- onehotEncode(iris$Species)

modelFit <- EdNetTrain(X,
                       Y,
                       family="multiclass",
                       learning_rate=0.05,
                       num_epochs=10,
                       hidden_layer_dims=c(128, 64, 32),
                       hidden_layer_activations="relu",
                       optimiser="Adam",
                       keep_prob=0.5,
                       mini_batch_size=8,
                       print_every_n=1L,
                       seed=1984L,
                       plot=TRUE)



### For Debugging
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
beta1 <- 0.9
beta2 <- 0.999
epsilon <- 1E-8
print_every_n <- 1L
seed <- 1984L
plot <- TRUE
checkpoint <- NULL
keep <- FALSE



