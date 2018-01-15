source("./R/functions.R")
source("./R/classes.R")
source("./R/families.R")
source("./R/buildModel.R")

set.seed(1984)
#development_set <- sample(seq(1, ncol(X)), 250)

## Build some dummy data
n <- 25600

set.seed(1984)
X <- sapply(1:n, function(i) rnorm(5))

W1 <- matrix(c(1, 2, 0, -2, 3, 0.5, 0, 1, 1, -2, 0, -2, -3, 4, 1), nrow=3)
b1 <- c(-0.5, 0.5, -1)

A1 <- relu(W1 %*% X + b1)

W2_binary <- matrix(c(0.3, 0.1, -0.6), nrow=1)
Y_binary <- sigmoid(W2_binary %*% relu(W1 %*% X + b1))
Y_binary <- 1*(Y_binary > 0.5)

W2_poisson <- matrix(runif(3, min=-1), nrow=1)
Y_poisson <- exp(W2_poisson %*% relu(W1 %*% X + b1))

W2_gaussian <- matrix(runif(3, min=-1), nrow=1)
Y_gaussian <- W2_gaussian %*% relu(W1 %*% X + b1)

W2_gamma <- matrix(runif(3, min=-1), nrow=1)
Y_gamma <- exp(W2_gamma %*% relu(W1 %*% X + b1))

W2_multiclass <- matrix(runif(9, min=-1), nrow=3)
Y_multiclass <- softmax(W2_multiclass %*% relu(W1 %*% X))
Y_multiclass <- apply(Y_multiclass, 2, cumsum)
Y_multiclass <- 1*(t(Y_multiclass) > runif(n))
Y_multiclass <- 1*(apply(Y_multiclass, 1, cumsum)==1)



# Build a neural network
NNet <- buildModel(X,
                   Y=Y_gamma,
                   family="gamma",
                   learning_rate=0.001,
                   num_epochs=30L,
                   hidden_layer_dims=3L,
                   hidden_layer_activations="relu",
                   optimiser="Adam",
                   mini_batch_size=128,
                   print_every=5L,
                   seed=1984L,
                   plot=TRUE)

NNet@model$Params$l1$W <- W1
NNet@model$Params$l1$b <- matrix(b1, nrow=3)
NNet@model$Params$l2$W <- W2_multiclass
NNet@model$Params$l2$b <- matrix(c(0, 0, 0), nrow=3)

NNet2 <- buildModel(X,
                    Y=Y_multiclass,
                    learning_rate=0.001,
                    num_epochs=30L,
                    optimiser="Adam",
                    mini_batch_size=128,
                    print_every=5L,
                    seed=1984L,
                    plot=TRUE,
                    checkpoint = NNet)

### For debugging

X <- X
Y <- Y_binary
family <- "binary"
learning_rate <- 0.002
num_epochs <- 500L
hidden_layer_dims <- 5L
hidden_layer_activations <- "relu"
optimiser <- "GradientDescent"
keep_prob <- NULL
input_keep_prob <- NULL
alpha <- 0
lambda <- 0
mini_batch_size <- 128
dev_set <- NULL
beta1 <- NULL
beta2 <- NULL
epsilon <- NULL
print_every <- 5L
seed <- 1984L
plot <- TRUE
checkpoint <- NULL
keep <- FALSE


# Debug softmax and gamma regression
# Cost functions to deviance for gamma/possion (maybe align with XGBoost??)
# Check interaction between keep and checkpoint
# Cross-validation
# Seeds
# Implement Gradient Checking
# Support for Tweedie
# Learning-rate decay
# Early-stopping
# Batch normalisation
# Weights
# Offsets
# Multi-task learning
# Transfer learning

# Package with examples