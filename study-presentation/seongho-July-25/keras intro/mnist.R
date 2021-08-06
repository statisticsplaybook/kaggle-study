# installation of tensorflow covered at:
# https://tensorflow.rstudio.com/installation/
# Windows users must have Rtools installed:
#     https://cran.r-project.org/bin/windows/Rtools/
# install.packages("tensorflow")

# The following code is from:
# https://tensorflow.rstudio.com/tutorials/beginners/

# Full Reference for Keras
# https://keras.rstudio.com/reference/index.html

library(tensorflow)
library(keras)

# load the MNIST dataset
mnist <- dataset_mnist()
mnist$train$x[20, , ]
mnist$train$x <- mnist$train$x/255
mnist$test$x <- mnist$test$x/255

# Define the model:
# keras_model_sequential defines a neural network with a sequence of layers
model <- keras_model_sequential() 

# With this model created, we provide a sequence of layers to define the
# structure of the neural network.
# More information based on the keras sequential model can be found at:
#    https://keras.io/guides/sequential_model/
#    https://keras.rstudio.com/reference/keras_model_sequential.html

28*28
model %>% 
  layer_flatten(input_shape = c(28, 28)) %>% 
  layer_dense(units = 128, activation = "relu") %>% 
  layer_dropout(0.2) %>% 
  layer_dense(10, activation = "softmax")
model # 데이터 수는 6만개인데 parameter 수가 10만을 넘는다
#parameter 수는 연결된 선의 갯수라고 생각해도 될것 같다. 



# The first layer is layer_flatten().
# The training data $mnist$train$x is an array with dimensions:
#     60000 x 28 x 28 - images x pixels wide x pixels height
# layer_flatten() turns the 2D structure into a 1D vector of length 784
# The argument is the input shape which is 28 by 28.

# The second layer is layer_dense().
# This creates a densely connected Neural network layer, which means all 
# neurons from the previous layer will be connected to every neuron in this one.
# A synonym for dense layer is fully-connected layer.
# You specify how many neurons or units, in this case 128 (arbitrary).
# You also specify the activation function. In this case the ReLu.
# function which will be applied to all of the values.
# There are more options for layer_dense() which can be found at:
#     https://keras.rstudio.com/reference/layer_dense.html
#     https://tensorflow.rstudio.com/reference/keras/layer_dense/

# The next layer is a dropout layer. 
# The dropout layer randomly sets some (in this case 20%) of the values to 0.
# This is a technique to help avoid over-fitting the data.
#    https://keras.rstudio.com/reference/layer_dropout.html

# Finally, we have another densely connected layer.
# The activation function, "softmax" is chosen for classification problems.
# The Softmax takes a vector of values and outputs another vector of the same
# length where all values are between 0 and 1 and the sum is 1. 
# The values in the resulting vector are found via: exp(z_i) / sum(exp(z_i))
# More info: https://en.wikipedia.org/wiki/Softmax_function
# A description of the available activation functions can be found at the 
# Keras documentation: https://keras.io/api/layers/activations/

summary(model)
784*128+128 # 편향
# The summary function provides a brief overview of the model structure
# For each layer, it shows the shape of the output and the number of parameters
# that need to be estimated.


# Keras requires you to compile and fit the models with two separate steps.
# These calls are done as follows.
# See the Keras documentation: https://keras.io/api/models/model_training_apis/

# Available choices for loss functions: https://keras.io/api/losses/
# sparse_categorical_crossentropy
# Computes the crossentropy loss between the labels and predictions.
# Use this crossentropy loss function when there are two or more label classes

# Available optimizers: https://keras.io/api/optimizers/#available-optimizers
# You can click the links for more details
# SGD - gradient descent with momentum
# adam - stochastic gradient descent with adaptive estimation of moments

# Available metrics: https://keras.io/api/metrics/

model %>% 
  compile(
    loss = "sparse_categorical_crossentropy",
    optimizer = "adam",
    metrics = "accuracy"
  )


# Fitting the model
# https://keras.io/api/models/model_training_apis/#fit-method
# We specify the training data: input array x and outputs y.
# The number of epochs is a way of specifying how many passes over the entire
# dataset should be performed. 5 epochs tells keras to go over the training
# data 5 times.
# The validation split will split the training data into training and 
# validation sets. The validation set is used to estimate the model performance 
# by measuring the loss at the end of each epoch.


model %>% 
  fit(
    x = mnist$train$x, y = mnist$train$y,
    epochs = 5, # 반복
    validation_split = 0.3, 
    verbose = T # 진행과정 보여주기
  )

# once the model is fit, you can use the predict() function to make predictions
predictions <- predict(model, mnist$test$x[1,,])
mnist$test$x[2,,]
which.max(head(predictions, 10)[2,])



#
model %>% 
  evaluate(mnist$test$x, mnist$test$y, verbose = T)
