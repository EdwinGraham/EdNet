# EdNet
Train deep neural networks with fully connected layers for classification and regression ([Vignette](https://edwingraham.github.io/EdNet/).)

## Installation

```
devtools::install_github("EdwinGraham/EdNet")
```


## Features
* Train models for classification tasks
  * Binary
  * Multi-class
* Regression supported for following distribtuion
  * Normal
  * Poisson
  * Gamma
  * Tweedie
* Supports weighted regression
* Supports training with an offset model
* L1 and L2 regularisation
* Drop-out
* Mini-batch learning
* Train with the following optimisation algorithms
  * Gradient descent
  * Momentum
  * RMSProp
  * Adam
* Initialise with random weights or continue learning from existing checkpoint model.