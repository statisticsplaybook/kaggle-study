library(rlang)

wmae_vec <- function(truth, estimate, weights, na_rm = TRUE, ...) {
    
    wmae_impl <- function(truth, estimate, weights) {
        sum(weights * abs(truth - estimate)) / sum(weights)
    }
    
    metric_vec_template(
        metric_impl = wmae_impl,
        truth = truth, 
        estimate = estimate,
        weights = weights,
        na_rm = na_rm,
        cls = "numeric",
        ...
    )
    
}

wmae <- function(data, ...) {
    UseMethod("wmae")
}

wmae <- new_numeric_metric(wmae, direction = "minimize")

wmae.data.frame <- function(data, truth, estimate, weights, na_rm = TRUE, ...) {
    
    metric_summarizer(
        metric_nm = "wmae",
        metric_fn = wmae_vec,
        data = data,
        truth = !! enquo(truth),
        estimate = !! enquo(estimate), 
        metric_fn_options = list(weights = weights),
        na_rm = na_rm,
        ...
    )
    
}

# data("solubility_test")
# wmae_vec(
#   truth = solubility_test$solubility, 
#   estimate = solubility_test$prediction,
#   weights = 1
# )
# solubility_test$weights <- 2
# wmae(solubility_test, 
#      truth = solubility, 
#      estimate = prediction, weights = solubility_test$weights)


# tune_result$.predictions %>% head()
# pred_df <- tune_result$.predictions[[1]]
# pred_df %>% filter(.config == "Preprocessor1_Model001",
#                    penalty == 1.000000e-10, mixture == 0.00) %>% 
#     select(.row) %>% unlist()
# pred_df$penalty %>% unique()
