library(tidyverse)
library(lubridate)

library(tsibble) # Tidy Temporal Data Frames and Tools
library(feasts) # Feature Extraction and Statistics for Time Series
library(tsibbledata) # Diverse Datasets for 'tsibble'

vic_elec %>% glimpse()
vic_elec %>% head()

# Temperature and electricity demand (normalized). Victoria, Australia, 07/2014.
vic_elec_2014 <-  vic_elec %>%
    filter(year(Date) == 2014) %>%
    select(-c(Date, Holiday)) %>%
    mutate(Demand = scale(Demand), Temperature = scale(Temperature)) %>%
    pivot_longer(-Time, names_to = "variable") %>%
    update_tsibble(key = variable)

vic_elec_2014 %>% filter(month(Time) == 1) %>% 
    autoplot() + 
    scale_colour_manual(values = c("#08c5d1", "#00353f")) +
    theme_minimal()


vic_elec_2014 <-  vic_elec %>%
    filter(year(Date) == 2014) %>%
    select(-c(Date, Holiday))

cmp <- vic_elec_2014 %>% 
    filter(month(Time) == 7) %>%
    model(STL(Demand)) %>% 
    components()

cmp %>% autoplot()

# data set define
elec_dataset <- dataset(
    name = "elec_dataset",
    
    initialize = function(x, n_timesteps, sample_frac = 1) {
        
        self$n_timesteps <- n_timesteps
        self$x <- torch_tensor((x - train_mean) / train_sd)
        
        n <- length(self$x) - self$n_timesteps 
        
        self$starts <- sort(sample.int(
            n = n,
            size = n * sample_frac
        ))
        
    },

    .getitem = function(i) {
        
        start <- self$starts[i]
        end <- start + self$n_timesteps - 1
        
        list(
            x = self$x[start:end],
            y = self$x[end + 1]
        )
        
    },
    
    .length = function() {
        length(self$starts) 
    }
)

2 * 24 * 7

vic_elec_get_year <- function(year, month = NULL) {
    vic_elec %>%
        filter(year(Date) == year, 
               month(Date) == if (is.null(month)) month(Date) else month) %>%
        as_tibble() %>%
        select(Demand)
}

elec_train <- vic_elec_get_year(2012) %>% as.matrix()
elec_valid <- vic_elec_get_year(2013) %>% as.matrix()
elec_test <- vic_elec_get_year(2014, 1) %>% as.matrix() # or 2014, 7, alternatively

train_mean <- mean(elec_train)
train_sd <- sd(elec_train)

n_timesteps <- 7 * 24 * 2 # days * hours * half-hours

train_ds <- elec_dataset(elec_train, n_timesteps, 
                         sample_frac = 0.5)
length(train_ds)

batch_size <- 32
train_dl <- train_ds %>% 
    dataloader(batch_size = batch_size, 
               shuffle = TRUE)
length(train_dl)

b <- train_dl %>% 
    dataloader_make_iter() %>% 
    dataloader_next()
b

valid_ds <- elec_dataset(elec_valid, n_timesteps, sample_frac = 0.5)
valid_dl <- valid_ds %>% dataloader(batch_size = batch_size)

test_ds <- elec_dataset(elec_test, n_timesteps)
test_dl <- test_ds %>% dataloader(batch_size = 1)

model <- nn_module(
    
    initialize = function(type, input_size, hidden_size,
                          num_layers = 1, dropout = 0) {
        self$type <- type
        self$num_layers <- num_layers
        
        self$rnn <- if (self$type == "gru") {
            nn_gru(
                input_size = input_size,
                hidden_size = hidden_size,
                num_layers = num_layers,
                dropout = dropout,
                batch_first = TRUE
            )
        } else {
            nn_lstm(
                input_size = input_size,
                hidden_size = hidden_size,
                num_layers = num_layers,
                dropout = dropout,
                batch_first = TRUE
            )
        }
        
        self$output <- nn_linear(hidden_size, 1)
        
    },
    
    forward = function(x) {
        
        # list of [output, hidden]
        # we use the output, which is of size (batch_size, n_timesteps, hidden_size)
        x <- self$rnn(x)[[1]]
        
        # from the output, we only want the final timestep
        # shape now is (batch_size, hidden_size)
        x <- x[ , dim(x)[2], ]
        
        # feed this to a single output neuron
        # final shape then is (batch_size, 1)
        x %>% self$output() 
    }
    
)
