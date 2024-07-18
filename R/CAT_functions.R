#' Generate Item Bank
#'
#' This function generates an item bank with specified parameters for different IRT models.
#'
#' @param n_items Integer. The number of items to generate.
#' @param model Character. The IRT model to use. Options are "Rasch", "1pl", "2pl", and "3pl". Default is "3pl".
#' @param a_params Numeric vector. The parameters for generating the item discrimination (a) values. Default is \code{c(0.1, 0.2)}.
#' @param b_params Numeric vector. The parameters for generating the item difficulty (b) values. Default is \code{c(0, 1.5)}.
#' @param c_params Numeric vector. The parameters for generating the item guessing (c) values. Default is \code{c(0, 0.3)}.
#'
#' @return A data frame containing the generated item bank with columns: `item_id`, `a`, `b`, `c`.
#'
#' @details The function generates an item bank based on the specified IRT model. The item parameters are generated using specified distributions:
#' \describe{
#'   \item{Rasch}{a = 1, b ~ N(mean, sd), c = 0}
#'   \item{1pl}{a = log-normal(meanlog, sdlog), b ~ N(mean, sd), c = 0}
#'   \item{2pl}{a ~ log-normal(meanlog, sdlog), b ~ N(mean, sd), c = 0}
#'   \item{3pl}{a ~ log-normal(meanlog, sdlog), b ~ N(mean, sd), c ~ uniform(min, max)}
#' }
#'
#' @examples
#' # Example usage
#' item_bank <- generate_item_bank(n_items = 10, model = "3pl")
#' print(item_bank)
#'
#' @export
generate_item_bank <- function(n_items, model = "3pl",
                               a_params = c(0.1, 0.2),
                               b_params = c(0, 1.5),
                               c_params = c(0, 0.3)) {
  
  if(round(n_items, 0) != n_items) {
    stop("n_items must be an integer")
  }
  
  if (model == "Rasch") {
    a <- rep(1, n_items)
    b <- rnorm(n_items, mean = b_params[1], sd = b_params[2])
    c <- rep(0, n_items)
  }
  
  if (model == "1pl") {
    a <- rep(rlnorm(1, meanlog = a_params[1], sdlog = a_params[2]), n_items)
    b <- rnorm(n_items, mean = b_params[1], sd = b_params[2])
    c <- rep(0, n_items)
  }
  
  if (model == "2pl") {
    a <- rlnorm(n_items, meanlog = a_params[1], sdlog = a_params[2])
    b <- rnorm(n_items, mean = b_params[1], sd = b_params[2])
    c <- rep(0, n_items)
  }
  
  if (model == "3pl") {
    a <- rlnorm(n_items, meanlog = a_params[1], sdlog = a_params[2])
    b <- rnorm(n_items, mean = b_params[1], sd = b_params[2])
    c <- runif(n_items, min = c_params[1], max = c_params[2])
  }
  
  bank <- data.frame(item_id = 1:n_items, a = a, b = b, c = c)
  
  return(bank)
}


#' Probability Function
#'
#' This function calculates the probability of a correct response in an IRT model.
#'
#' @param theta Numeric. The ability level of the examinee.
#' @param a Numeric. The discrimination parameter of the item.
#' @param b Numeric. The difficulty parameter of the item.
#' @param c Numeric. The guessing parameter of the item.
#'
#' @return A numeric value representing the probability of a correct response.
#'
#' @export
prob <- function(theta, a, b, c) {
  return(c + (1 - c) / (1 + exp(-a * (theta - b))))
}

#' Item Information Function
#'
#' This function calculates the item information for a given ability level (\eqn{\theta}) 
#' based on the parameters of an item response theory (IRT) model.
#'
#' @param theta Numeric. The ability level of the examinee.
#' @param a Numeric. The discrimination parameter of the item.
#' @param b Numeric. The difficulty parameter of the item.
#' @param c Numeric. The guessing parameter of the item.
#'
#' @return A numeric value representing the information provided by the item at the given ability level (\eqn{\theta}).
#'
#' @details The item information function is based on the probability of a correct response given by the IRT model. 
#' The probability function (\code{prob}) is assumed to be defined elsewhere in the code. 
#' The information is calculated using the following formula:
#' \deqn{I(\theta) = a^2 \frac{Q}{P} \left( \frac{P - c}{1 - c} \right)^2}
#' where \eqn{P} is the probability of a correct response, \eqn{Q} is \eqn{1 - P}, \eqn{a} is the discrimination parameter, 
#' \eqn{b} is the difficulty parameter, and \eqn{c} is the guessing parameter.
#'
#' @examples
#' # Example usage
#' theta <- 1.5
#' a <- 1.2
#' b <- 0.5
#' c <- 0.2
#' item_info(theta, a, b, c)
#'
#' @seealso \code{prob} for the probability function used in the calculation.
#'
#' @export
item_info <- function(theta, a, b, c) {
  P <- prob(theta, a, b, c)  # Probability function defined elsewhere
  Q <- 1 - P
  info <- (a^2) * (Q / P) * ((P - c)^2 / (1 - c)^2)
  return(info)
}

#' Item Information for Item Bank
#'
#' This function calculates the item information for each item in an item bank across a specified range of ability levels (\eqn{\theta}).
#'
#' @param item_bank Data frame. The item bank containing the parameters of the items. It should have columns named `a`, `b`, and `c` for the item parameters.
#' @param range Numeric vector of length 2. The range of ability levels (\eqn{\theta}) over which to calculate the item information. Default is \code{c(-3, 3)}.
#' @param n_thetas Integer. The number of ability levels (\eqn{\theta}) to evaluate within the specified range. Default is 100.
#'
#' @return A data frame containing the ability levels (\eqn{\theta}), item identifiers, and the corresponding item information values.
#'
#' @details The function calculates the item information for each item in the item bank across a sequence of ability levels specified by the `range` and `n_thetas` parameters.
#' The item information is calculated using the \code{item_info} function.
#'
#' @examples
#' # Example usage
#' item_bank <- data.frame(
#'   a = rlnorm(10, meanlog = 0.1, sdlog = 0.2),
#'   b = rnorm(10, mean = 0, sd = 1.5),
#'   c = runif(10, min = 0, max = 0.35)
#' )
#' result <- item_info_item_bank(item_bank)
#' print(result)
#'
#' @seealso \code{item_info} for the function used to calculate item information.
#'
#' @export
item_info_bank <- function(item_bank, range = c(-3, 3), n_thetas = 100) {
  
  if (range[1] >= range[2] || length(range) != 2) {
    stop("Range must be length 2, with lower and upper bound. Example: c(-3, 3)")
  }
  
  # List out the range for theta values
  theta_values <- seq(range[1], range[2], length.out = n_thetas)
  
  # Pre-allocate a data frame to store results
  n_items <- nrow(item_bank)
  item_bank_item_info <- data.frame(
    thetas = rep(theta_values, n_items),
    item = rep(1:n_items, each = n_thetas),
    info = NA
  )
  
  # Calculate item information for all items in the bank
  for (i in 1:n_items) {
    item_bank_item_info$info[(1:n_thetas) + (i - 1) * n_thetas] <- item_info(
      theta_values,
      item_bank$a[i],
      item_bank$b[i],
      item_bank$c[i]
    )
  }
  
  return(item_bank_item_info)
}




#' Item Characteristic Curves for Item Bank
#'
#' This function calculates the item characteristic curves (ICCs) for each item in an item bank across a specified range of ability levels (\eqn{\theta}).
#'
#' @param item_bank Data frame. The item bank containing the parameters of the items. It should have columns named `a`, `b`, and `c` for the item parameters.
#' @param range Numeric vector of length 2. The range of ability levels (\eqn{\theta}) over which to calculate the ICCs. Default is \code{c(-3, 3)}.
#' @param n_thetas Integer. The number of ability levels (\eqn{\theta}) to evaluate within the specified range. Default is 100.
#'
#' @return A data frame containing the ability levels (\eqn{\theta}), item identifiers, and the corresponding ICC values.
#'
#' @details The function calculates the ICCs for each item in the item bank across a sequence of ability levels specified by the `range` and `n_thetas` parameters.
#' The ICCs are calculated using the \code{prob} function.
#'
#' @examples
#' # Example usage
#' item_bank <- data.frame(
#'   a = rlnorm(10, meanlog = 0.1, sdlog = 0.2),
#'   b = rnorm(10, mean = 0, sd = 1.5),
#'   c = runif(10, min = 0, max = 0.35)
#' )
#' result <- item_bank_ICC(item_bank)
#' print(result)
#'
#' @seealso \code{prob} for the function used to calculate the ICCs.
#'
#' @export
item_ICC_bank <- function(item_bank, range = c(-3, 3), n_thetas = 100) {
  
  if (range[1] >= range[2] || length(range) != 2) {
    stop("Range must be length 2, with lower and upper bound. Example: c(-3, 3)")
  }
  
  # List out the range for theta values
  theta_values <- seq(range[1], range[2], length.out = n_thetas)
  
  # Pre-allocate a data frame to store results
  n_items <- nrow(item_bank)
  item_bank_ICC_data <- data.frame(
    thetas = rep(theta_values, n_items),
    item = rep(1:n_items, each = n_thetas),
    icc = NA
  )
  
  # Calculate ICCs for all items in the bank
  for (i in 1:n_items) {
    item_bank_ICC_data$icc[(1:n_thetas) + (i - 1) * n_thetas] <- prob(
      theta_values,
      item_bank$a[i],
      item_bank$b[i],
      item_bank$c[i]
    )
  }
  
  return(item_bank_ICC_data)
}

#' Check Lengths of Elements
#'
#' This function checks whether all provided elements have the same length.
#'
#' @param ... Elements to check. Each element should be a vector or list.
#'
#' @return A logical value. Returns \code{TRUE} if all elements have the same length, \code{FALSE} otherwise.
#'
#' @details The function takes a variable number of arguments and checks whether all provided elements (vectors or lists) have the same length.
#' The lengths of all elements are compared, and if they are equal, the function returns \code{TRUE}; otherwise, it returns \code{FALSE}.
#'
#' @examples
#' # Example usage
#' vec1 <- c(1, 2, 3)
#' vec2 <- c("a", "b", "c")
#' vec3 <- c(TRUE, FALSE, TRUE)
#' 
#' check_lengths(vec1, vec2, vec3) # Returns TRUE
#'
#' vec4 <- c(1, 2)
#' check_lengths(vec1, vec2, vec4) # Returns FALSE
#'
#' @export
check_lengths <- function(...) {
  objs <- list(...)
  lengths <- sapply(objs, length)
  all(lengths == lengths[1])
}

#' Maximum Likelihood Ability Estimation Function
#'
#' This function estimates the most likely ability level of an individual given item parameters and observed responses.
#' It makes available the use of an adjustment factor aka "kludge" to adjust for response strings with zero variance to prevent extreme ability estimates early on in a Computerized Adaptive Test (CAT).
#'
#' @param responses Numeric vector. The observed item responses (0 or 1).
#' @param as Numeric vector. The item discrimination parameters.
#' @param bs Numeric vector. The item difficulty parameters.
#' @param cs Numeric vector. The item guessing parameters.
#' @param optim_method Character. The optimization method to be used in the \code{optim} function. Default is \code{"CG"}.
#' @param kludge Logical. Whether to apply a kludge to the MLE estimation to prevent extreme ability estimates. Default is TRUE.
#'
#' @return A list containing:
#' \describe{
#'   \item{ability_est}{The estimated ability level.}
#'   \item{ability_est_se}{The standard error of the ability estimate.}
#' }
#'
#' @details The function calculates the ability estimate by maximizing the log-likelihood function using the \code{optim} function.
#' An adjustment factor \eqn{\left(\frac{1}{3 \sqrt{n}}\right)} is applied to response strings with zero variance to prevent wild swings in ability estimates,
#' where \eqn{n} is the number of responses. 
#' The log-likelihood function is defined based on the probability of a correct response given the ability level and item parameters.
#'
#' @examples
#' # Example usage
#' responses <- c(1, 0, 1, 1, 0)
#' as <- c(1.2, 1.0, 0.8, 1.1, 0.9)
#' bs <- c(-1, 0, 1, 2, -0.5)
#' cs <- c(0.2, 0.25, 0.15, 0.3, 0.1)
#' result <- est_ability_mle(responses, as, bs, cs)
#' print(result)
#'
#' @seealso \code{check_lengths} for checking the lengths of input vectors, \code{prob} for calculating response probabilities.
#'
#' @export
est_ability_mle <- function(responses, as, bs, cs, optim_method = "CG", kludge = TRUE) {
  
  # Confirm equal lengths
  if (!check_lengths(responses, as, bs, cs)) {
    stop("Error: Input vectors must have the same length.")
  }
  
  # Remove item params and responses where response == NA
  valid_indices <- !is.na(responses)
  as <- as[valid_indices]
  bs <- bs[valid_indices]
  cs <- cs[valid_indices]
  responses <- responses[valid_indices]
  
  if(kludge == TRUE) {
  
    # Create an adjustment factor (kludge) for response strings with zero variance
    adjustment_factor <- (1 / (3 * sqrt(length(responses))))
  
      # Adjust responses for perfect or zero scores
      if (mean(responses) == 1) {
        responses <- responses - adjustment_factor
      } else if (mean(responses) == 0) {
        responses <- responses + adjustment_factor
      }
    }
  
  # The log likelihood function
  logLik <- function(theta) {
    p <- prob(theta, as, bs, cs)
    sum(responses * log(p) + (1 - responses) * log(1 - p))
  }
  
  # Run an optimization to find max likelihood
  optim_result <- optim(par = 0, fn = logLik, method = optim_method, control = list(fnscale = -1))
  
  # Get the theta value with the max likelihood 
  theta_hat <- optim_result$par
  
  # Function to calculate the information matrix for standard error
  information_matrix <- function(theta) {
    p <- prob(theta, as, bs, cs)
    q <- 1 - p
    sum(as^2 * p * q)
  }
  
  info_value <- information_matrix(theta_hat)
  se <- sqrt(1 / info_value)
  
  list(ability_est = theta_hat, ability_est_se = se)
}


#' Select the Initial Item for CAT
#'
#' This function selects the initial item for a Computerized Adaptive Test (CAT) based on the initial ability estimate and weighted distance of item b parameters.
#'
#' @param item_bank_df Data frame. The item bank containing the parameters of the items. It should have columns: `item_id`, `a`, `b`, `c`.
#' @param initial_ability Numeric. The initial ability estimate. Default is 0.
#'
#' @return A data frame containing the selected initial item and additional information for the test event.
#'
#' @details The function calculates weights for each item based on the distance from the initial ability estimate. Items within 0.5 logits of the initial ability estimate receive weights inversely proportional to their distance from the initial ability. The weights are normalized to sum to 1, and an item is randomly selected based on these weights. The returned data frame includes placeholders for response data and the current ability estimate.
#'
#' @examples
#' # Example usage
#' item_bank_df <- data.frame(
#'   item_id = 1:10,
#'   a = rlnorm(10, meanlog = 0.1, sdlog = 0.2),
#'   b = rnorm(10, mean = 0, sd = 1.5),
#'   c = runif(10, min = 0, max = 0.35)
#' )
#' result <- initial_item(item_bank_df, initial_ability = 0)
#' print(result)
#'
#' @export
initial_item <- function(item_bank_df, initial_ability = 0) {
  
  # Ensure item_id is integer
  item_bank_df$item_id <- as.integer(item_bank_df$item_id)
  
  # Pull out the b-parameters
  b <- item_bank_df$b
  
  # Calculate weights based on distance to the initial ability estimate, but only for items within 0.5 logits
  weights <- ifelse(abs(b - initial_ability) <= 0.5, 1 / abs(b - initial_ability), 0)
  
  # Normalize weights to sum to 1
  weights <- weights / sum(weights)
  
  # Randomly select an item based on our weights
  selected_item <- sample(1:length(b), size = 1, prob = weights)
  
  # Create an initial test_event_df
  test_event_df <- item_bank_df[selected_item, ]
  test_event_df$order <- as.integer(1)
  test_event_df$item_selection_ts <- Sys.time() # Timestamp for when the item was selected for the CAT
  test_event_df$response <- NA                        # Placeholder for the response
  test_event_df$response_score <- NA                  # Placeholder for how the item was scored
  test_event_df$response_ts <- as.POSIXct(NA)         # Placeholder for the response timestamp
  test_event_df$current_ability <- NA                 # Placeholder for ability estimate
  test_event_df$current_ability_se <- NA              # Placeholder for SE of ability estimate
  
  # Rearrange the columns to the desired order
  test_event_df <- test_event_df[c("order", "item_id", "a", "b", "c", 
                                         "response_score", "current_ability", "current_ability_se", 
                                         "item_selection_ts", "response", "response_ts")]
  
  return(test_event_df)
}

#' Select the Next Item for CAT
#'
#' This function selects the next item for a Computerized Adaptive Test (CAT) based on the current ability estimate.
#'
#' @param eligible_items_df Data frame. The item bank containing the parameters of the eligible items. It should have columns: `item_id`, `a`, `b`, `c`.
#' @param test_event_df Data frame. The test event table containing the selected items and their corresponding information.
#'
#' @return A data frame containing the updated test event table with the next selected item.
#'
#' @details The function selects the next item based on the distance between the current ability estimate and the item difficulty parameters (`b`). The item with the smallest distance is selected. The returned data frame includes placeholders for response data.
#'
#' @examples
#' # Example usage
#' eligible_items_df <- data.frame(
#'   item_id = 1:10,
#'   a = rlnorm(10, meanlog = 0.1, sdlog = 0.2),
#'   b = rnorm(10, mean = 0, sd = 1.5),
#'   c = runif(10, min = 0, max = 0.35)
#' )
#' test_event_df <- data.frame(
#'   order = integer(),
#'   item_id = integer(),
#'   a = numeric(),
#'   b = numeric(),
#'   c = numeric(),
#'   response_score = numeric(),
#'   current_ability = numeric(),
#'   current_ability_se = numeric(),
#'   item_selection_ts = as.POSIXct(character()),
#'   response = numeric(),
#'   response_ts = as.POSIXct(character())
#' )
#' test_event_df$current_ability <- 0
#' result <- next_item(eligible_items_df, test_event_df)
#' print(result)
#'
#' @export
next_item <- function(eligible_items_df, test_event_df) {
  
  ## bOpt criterion // Urry's Rule
  # Pull out the b-parameters
  b <- eligible_items_df$b
  
  # Identify the current ability
  current_ability_est <- test_event_df$current_ability[nrow(test_event_df)]
  
  # Calculate distance between b and current ability estimate
  distance <- abs(current_ability_est - b)
  
  # Pick the item with the smallest distance
  selected_item <- which.min(distance)
  
  # Output the selected item
  selected_item_table <- eligible_items_df[selected_item,]
  selected_item_table$order <- as.integer(nrow(test_event_df) + 1)
  # selected_item_table$item_id <- as.integer(selected_item_table$item_id)
  selected_item_table$item_selection_ts <- Sys.time() # Timestamp for when the item was selected for the CAT
  selected_item_table$response <- NA                        # Placeholder for the response
  selected_item_table$response_score <- NA                  # Placeholder for how the item was scored
  selected_item_table$response_ts <- as.POSIXct(NA)         # Placeholder for the response timestamp
  selected_item_table$current_ability <- NA                 # Placeholder for the current ability estimate
  selected_item_table$current_ability_se <- NA              # Placeholder for the current ability standard error
  
  
  # Combine the new item with the existing test_event_df
  test_event_df <- rbind(test_event_df, selected_item_table)
  
  return(test_event_df)
}

#' Score Responses and Estimate Ability
#'
#' This function scores the response to a given item and updates the ability estimate for a Computerized Adaptive Test (CAT).
#'
#' @param test_event_df Data frame. The test event table containing the selected items and their corresponding information.
#' @param item_id Integer. The identifier of the item to be scored.
#' @param response Integer. The response provided by the test taker.
#' @param key Integer. The correct response for the item. Default is 1.
#' @param item_type Character. The type of item. Default is "dich" for dichotomous scoring (either 1 or 0, no partial credit).
#' @param optim_method Character. The optimization method to be used in the \code{optim} function. Default is \code{"CG"}.
#' @param kludge Logical. Whether to apply a kludge to the MLE estimation to prevent extreme ability estimates. Default is TRUE.
#' 
#' @return A data frame containing the updated test event table with the scored response and updated ability estimate.
#'
#' @details The function scores the response based on the item type. For dichotomous items, the response is scored as 1 if it matches the key, and 0 otherwise. The function then updates the test event table with the scored response and the timestamp. It also estimates the ability and updates the test event table with the current ability estimate and its standard error.
#'
#' @examples
#' # Example usage
#' test_event_df <- data.frame(
#'   order = integer(),
#'   item_id = integer(),
#'   a = numeric(),
#'   b = numeric(),
#'   c = numeric(),
#'   response_score = numeric(),
#'   current_ability = numeric(),
#'   current_ability_se = numeric(),
#'   item_selection_ts = as.POSIXct(character()),
#'   response = numeric(),
#'   response_ts = as.POSIXct(character())
#' )
#' test_event_df <- rbind(test_event_df, data.frame(
#'   order = 1,
#'   item_id = 1,
#'   a = 1.2,
#'   b = -0.5,
#'   c = 0.2,
#'   response_score = NA,
#'   current_ability = NA,
#'   current_ability_se = NA,
#'   item_selection_ts = Sys.time(),
#'   response = NA,
#'   response_ts = as.POSIXct(NA)
#' ))
#' test_event_df$current_ability <- 0
#' result <- score_response(test_event_df, item_id = 1, response = 1)
#' print(result)
#'
#' @seealso \code{est_ability_mle} for estimating ability.
#'
#' @export
score_response <- function(test_event_df, item_id, response, key = 1, item_type = "dich", kludge = TRUE, optim_method = "CG") {
  
  # Score the response based on item type
  score <- if (item_type == "dich") {
    if (response == key) 1 else 0
  } else {
    NA  # Placeholder for other item types
  }
  
  # Update test event table
  idx <- which(test_event_df$item_id == item_id)
  test_event_df$response[idx] <- response
  test_event_df$response_score[idx] <- score
  test_event_df$response_ts[idx] <- Sys.time()
  
  # Estimate ability
  current_ability_ls <- est_ability_mle(test_event_df$response_score, test_event_df$a, test_event_df$b, test_event_df$c, optim_method = optim_method, kludge = kludge)
  
  # Update the test event table with the current ability estimate and its SE
  test_event_df$current_ability[idx] <- current_ability_ls$ability_est
  test_event_df$current_ability_se[idx] <- current_ability_ls$ability_est_se
  
  return(test_event_df)
}

#' Determine Whether to Stop the CAT
#'
#' This function determines whether to stop the Computerized Adaptive Test (CAT) based on the maximum number of items, minimum standard error (SE), and optional minimum number of items.
#'
#' @param test_event_df Data frame. The test event table containing the selected items and their corresponding information.
#' @param max_items Integer. The maximum number of items to be administered.
#' @param min_se Numeric. The minimum standard error stopping criterion.
#' @param min_items Integer. The minimum number of items to be administered before considering other stopping criteria. Default is NULL.
#'
#' @return A logical value indicating whether the CAT should be stopped (\code{TRUE}) or not (\code{FALSE}).
#'
#' @details The function checks whether the test should be stopped based on the specified stopping criteria. If the current standard error is less than or equal to the minimum SE and the number of administered items is greater than or equal to the minimum items, or if the maximum number of items has been reached, the test will be stopped.
#'
#' @examples
#' # Example usage
#' test_event_df <- data.frame(
#'   order = integer(),
#'   item_id = integer(),
#'   a = numeric(),
#'   b = numeric(),
#'   c = numeric(),
#'   response_score = numeric(),
#'   current_ability = numeric(),
#'   current_ability_se = numeric(),
#'   item_selection_ts = as.POSIXct(character()),
#'   response = numeric(),
#'   response_ts = as.POSIXct(character())
#' )
#' test_event_df <- rbind(test_event_df, data.frame(
#'   order = 1,
#'   item_id = 1,
#'   a = 1.2,
#'   b = -0.5,
#'   c = 0.2,
#'   response_score = 1,
#'   current_ability = 0,
#'   current_ability_se = 0.3,
#'   item_selection_ts = Sys.time(),
#'   response = 1,
#'   response_ts = Sys.time()
#' ))
#' result <- stop_test(test_event_df, max_items = 10, min_se = 0.3, min_items = 1)
#' print(result)
#'
#' @export
stop_test <- function(test_event_df, max_items, min_se, min_items = 0) {
  
  if (max_items < min_items) {
    warning("max_items cannot be less than min_items")
  }
  stop_test <- FALSE
  
  items_tested <- sum(!is.na(test_event_df$response_score))
  current_se <- test_event_df$current_ability_se[nrow(test_event_df)]
  
  min_se_stop <- current_se <= min_se
  max_items_stop <- items_tested == max_items
  min_items_met <- items_tested >= min_items
  
  # Check SE criteria
  if (min_items_met & (min_se_stop | max_items_stop)) {
    stop_test <- TRUE
  }
  
  return(stop_test)
}

#' Update List of Eligible Items for CAT
#'
#' This function updates the list of eligible items for a Computerized Adaptive Test (CAT) by removing items that have already been administered.
#'
#' @param eligible_items_df Data frame. An item bank containing the parameters of all eligible items. It should have a column `item_id`.
#' @param test_event_df Data frame. The test event table containing the selected items and their corresponding information. It should have a column `item_id`.
#'
#' @return A data frame containing the updated list of eligible items.
#'
#' @details The function filters out items from the item bank that have already been administered, based on the `item_id` column.
#'
#' @examples
#' # Example usage
#' item_bank_df <- data.frame(
#'   item_id = 1:10,
#'   a = rlnorm(10, meanlog = 0.1, sdlog = 0.2),
#'   b = rnorm(10, mean = 0, sd = 1.5),
#'   c = runif(10, min = 0, max = 0.35)
#' )
#' test_event_df <- data.frame(
#'   item_id = c(1, 3, 5),
#'   order = c(1, 2, 3),
#'   a = c(1.2, 0.8, 1.1),
#'   b = c(-1, 0, 1),
#'   c = c(0.2, 0.25, 0.15),
#'   response_score = c(1, 0, 1),
#'   current_ability = c(NA, NA, NA),
#'   current_ability_se = c(NA, NA, NA),
#'   item_selection_ts = Sys.time(),
#'   response = c(1, 0, 1),
#'   response_ts = Sys.time()
#' )
#' result <- update_eligible_items(item_bank_df, test_event_df)
#' print(result)
#'
#' @export
update_eligible_items <- function(eligible_items_df, test_event_df) {
  updated_eligible_items <- eligible_items_df[!eligible_items_df$item_id %in% test_event_df$item_id, ]
  return(updated_eligible_items)
}

#' Test Information Function
#'
#' This function calculates the total test information and standard error (SE) for a given set of ability levels (\eqn{\theta}) based on item parameters.
#'
#' @param thetas Numeric vector. A vector of ability levels (\eqn{\theta}) to be used to calculate information.
#' @param as Numeric vector. The item discrimination parameters.
#' @param bs Numeric vector. The item difficulty parameters.
#' @param cs Numeric vector. The item guessing parameters.
#'
#' @return A data frame containing the ability levels (\eqn{\theta}), total test information, and standard error (SE).
#'
#' @details The function calculates the total test information by summing the item information for each item across all provided ability levels (\eqn{\theta}). The standard error (SE) is then calculated as the inverse square root of the total information.
#'
#' @examples
#' # Example usage
#' thetas <- seq(-3, 3, length.out = 100)
#' as <- rlnorm(10, meanlog = 0.1, sdlog = 0.2)
#' bs <- rnorm(10, mean = 0, sd = 1.5)
#' cs <- runif(10, min = 0, max = 0.35)
#' result <- test_info(thetas, as, bs, cs)
#' print(result)
#'
#' @seealso \code{item_info} for calculating item information.
#'
#' @export
test_info <- function(thetas, as, bs, cs) {
  n_items <- length(as)
  I_total <- rep(0, length(thetas))
  
  for (i in 1:n_items) {
    a <- as[i]
    b <- bs[i]
    c <- cs[i]
    
    I_total <- I_total + item_info(thetas, a, b, c)
  }
  
  SE_total <- 1 / sqrt(I_total)
  
  return(data.frame(
    theta = thetas,
    info = I_total,
    se = SE_total
  ))
}

#' Simulate a Computerized Adaptive Test (CAT)
#'
#' This function simulates a Computerized Adaptive Test (CAT) for a given set of abilities using the specified item bank and stopping criteria.
#'
#' @param item_bank Data frame. The item bank containing the parameters of the items. It should have columns: `item_id`, `a`, `b`, `c`.
#' @param abilities Numeric vector. The true ability levels of the test takers.
#' @param seed Integer. The random seed for reproducibility. Default is 123.
#' @param max_items Integer. The maximum number of items to be administered. Default is 20.
#' @param min_se Numeric. The minimum standard error stopping criterion. Default is 0.5.
#' @param min_items Integer. The minimum number of items to be administered before considering other stopping criteria. Default is NULL.
#' @param response_consistency Numeric. A multiplier for the item discrimination parameter to adjust response consistency. Default is 1.
#' @param kludge Logical. Whether to apply a kludge to the MLE estimation to prevent extreme ability estimates. Default is TRUE.
#' @param silent Logical. Whether to print the case # after completing each simulated CAT. Default is FALSE.
#' @param optim_method Character. The optimization method to be used in the \code{optim} function. Default is \code{"CG"}.
#'
#' @return A list containing:
#' \describe{
#'   \item{summary}{A data frame summarizing the results for each test taker.}
#'   \item{event}{A data frame containing the detailed test event information for all test takers.}
#' }
#'
#' @details The function simulates the CAT process by selecting items based on the current ability estimate, scoring responses, and updating the ability estimate until the stopping criteria are met. The stopping criteria include a maximum number of items, a minimum standard error, and an optional minimum number of items.
#'
#' @examples
#' # Example usage
#' item_bank <- data.frame(
#'   item_id = 1:10,
#'   a = rlnorm(10, meanlog = 0.1, sdlog = 0.2),
#'   b = rnorm(10, mean = 0, sd = 1.5),
#'   c = runif(10, min = 0, max = 0.35)
#' )
#' abilities <- rnorm(5, mean = 0, sd = 1)
#' result <- simulate_cat(item_bank, abilities)
#' print(result)
#'
#' @export
simulate_cat <- function(item_bank, abilities, seed = 123, max_items = 20, min_se = 0.5, min_items = 0, response_consistency = 1, kludge = TRUE, silent = FALSE, optim_method = "CG") {
  set.seed(seed)
  
  if (response_consistency < 0) {
    warning("response_consistency must be greater than 0")
  }
  if (max_items < min_items) {
    warning("max_items must be greater than or equal to min_items")
  }
  
  master_test_event <- NULL
  master_test_summary <- NULL
  
  for (i in 1:length(abilities)) {
    # Start the CAT and select the first item
    test_event <- initial_item(item_bank)
    a <- test_event$a[nrow(test_event)]
    b <- test_event$b[nrow(test_event)]
    c <- test_event$c[nrow(test_event)]
    
    # Select the answer from a distribution of probabilities based on item parameters.
    answer <- rbinom(1, 1, prob(abilities[i], a * response_consistency, b, c))
    
    # Score response
    test_event <- score_response(test_event, test_event$item_id[nrow(test_event)], answer, kludge = kludge, optim_method = optim_method)
    
    while (!stop_test(test_event_df = test_event, max_items = max_items, min_se = min_se, min_items = min_items)) {
      # Update Eligible items
      eligible_items <- update_eligible_items(item_bank, test_event)
      
      # Select the next item
      test_event <- next_item(eligible_items, test_event)
      
      # Get item parameters
      a <- test_event$a[nrow(test_event)]
      b <- test_event$b[nrow(test_event)]
      c <- test_event$c[nrow(test_event)]
      
      # Select the answer from a probability distribution of probabilities based on item parameters.
      item_prob <- prob(abilities[i], a * response_consistency, b, c)
      answer <- rbinom(1, 1, item_prob)
      
      # Score response
      test_event <- score_response(test_event, test_event$item_id[nrow(test_event)], answer, kludge = kludge, optim_method = optim_method)
    }
    
    # Once the loop is finished
    if (is.null(master_test_event)) {
      master_test_event <- cbind(data.frame(case = i), test_event)
      master_test_summary <- data.frame(
        case = i,
        ability = abilities[i],
        n_items = nrow(test_event),
        final_ability = test_event$current_ability[nrow(test_event)],
        final_ability_se = test_event$current_ability_se[nrow(test_event)],
        test_info_at_final_ability = test_info(test_event$current_ability[nrow(test_event)], test_event$a, test_event$b, test_event$c)$info
      )
    } else {
      master_test_event <- rbind(master_test_event, cbind(data.frame(case = i), test_event))
      master_test_summary <- rbind(master_test_summary, data.frame(
        case = i,
        ability = abilities[i],
        n_items = nrow(test_event),
        final_ability = test_event$current_ability[nrow(test_event)],
        final_ability_se = test_event$current_ability_se[nrow(test_event)],
        test_info_at_final_ability = test_info(test_event$current_ability[nrow(test_event)], test_event$a, test_event$b, test_event$c)$info
      ))
    }
    if(silent == FALSE) {
      print(paste0("Case ", i))
      }
  }
  
  return(
    list(
      "summary" = master_test_summary,
      "event" = master_test_event
    )
  )
}
