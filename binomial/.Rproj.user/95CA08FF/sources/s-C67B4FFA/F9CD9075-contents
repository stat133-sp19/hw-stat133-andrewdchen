# Checks if input prob is a valid probability
check_prob <- function(p) {
  if (p >= 0 & p <= 1) {
    return(TRUE)
  }
  stop("invalid prob value")
}

# Checks if the number of trials is non-negative
check_trials <- function(trials) {
  if (trials >= 0) {
    return(TRUE)
  }
  stop("invalid trials value")
}

# Checks if the number of successes is valid
check_success <- function(success, trials) {
  if (success >= 0 & success <= trials) {
    return(TRUE)
  }
  stop('invalid success value')
}

aux_mean <- function(trials, prob) {
  return(trials*prob)
}
aux_variance <- function(trials, prob) {
  return(trials*prob*(1 - prob))
}
aux_mode <- function(trials, prob) {
  return(floor(trials*prob + prob))
}
aux_skewness <- function(trials, prob) {
  return((1-2*prob)/sqrt(trials*prob*(1-prob)))
}
aux_kurtosis <- function(trials, prob) {
  return((1-6*prob*(1 - prob))/(trials*prob*(1-prob)))
}

#' @title Binomial Choose
#' @description Calculates n choose k
#' @param n, number of trials
#' @param k, number of succeses
#' @export
#' @return n choose k
bin_choose <- function(n, k) {
  if(k > n) {
    stop("k cannot be greater than n")
  }
  return(factorial(n)/(factorial(k)*factorial(n - k)))
}

#doesn't really work with sequences bin_probability(success = 0:6, trials = 5, prob = 0.5)

#' @title Binomial Probability
#' @description Calculates binomail probabilities given success, trials, prob
#' @param success, number of heads
#' @param trials, number of flips
#' @param prob, probability of observing heads
#' @export
#' @return binomial probability
bin_probability <- function(success, trials, prob) {
  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)
  return(bin_choose(trials, success)*(prob^success)*(1-prob)^(trials - success))
}

#' @title Binomial Distribution
#' @description Calculates the distribution of a binomial random variable
#' @param trials, number of flips
#' @param prob, probability of observing heads
#' @export
#' @return dataframe object detailing the distribution
bin_distribution <- function(trials, prob) {
  probs <- bin_probability(0:trials, trials, prob)
  dist <- data.frame("success" = 0:trials, "probability" = probs)
  class(dist) <- c("bindis","data.frame")
  return(dist)
}

#' @export
plot.bindis <- function(dis) {
  return(barplot(dis1$probability, xlab = "Successes", ylab = "probability"))
}

#' @title Binomial CDF
#' @description Calculates the distribution of a binomial random variable and its CDF
#' @param trials, number of flips
#' @param prob, probability of observing heads
#' @export
#' @return dataframe object detailing the distribution and CDF
bin_cumulative <- function(trials, prob) {
  dist <- bin_distribution(trials, prob)
  dist$cumulative <- cumsum(dist$probability)
  class(dist) <- c("bincum", "data.frame")
  return(dist)
}

#' @export
plot.bincum <- function(dis) {
  return(plot(dis$cumulative, type = "o", xlab = "successes", ylab = "probability"))
}

#' @title Binomial Variable
#' @description Creates a list with trials and prob information
#' @param trials, number of flips
#' @param prob, probability of observing heads
#' @export
#' @return binvar object that is a list with trials and prob information
bin_variable <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  params <- list(trials, prob)
  names(params) <- c("trials", "prob")
  class(params) <- c("binvar")
  return(params)
}

#' @export
print.binvar <- function(binvar) {
  cat('"Binomial variable"\n\n')
  cat("Parameters")
  cat(paste0("\n- number of trials: ", binvar[1]))
  cat(paste0("\n- prob of success : ", binvar[2]))
}

#' @title Binomial Variable Summary
#' @description Creates a list with trials, prob, mean, variance, mode, skewness, and kurtosis information
#' @param binvar, a binomial random variable object
#' @export
#' @return a summary.binvar object that is a list with above information
summary.binvar <- function(binvar) {
  n <- binvar[[1]]
  p <- binvar[[2]]
  binvar$mean <- aux_mean(n, p)
  binvar$variance <- aux_variance(n, p)
  binvar$mode <- aux_mode(n, p)
  binvar$skewness <- aux_skewness(n, p)
  binvar$kurtosis <- aux_kurtosis(n, p)
  class(binvar) <- "summary.binvar"
  return(binvar)
}

#' @export
print.summary.binvar <- function(s) {
  cat('"Summary Binomial"\n\n')
  cat("Parameters")
  cat(paste0("\n- number of trials: ", s[1]))
  cat(paste0("\n- prob of success: ", s[2]))
  cat("\n\n Measures\n")
  cat(paste0("- mean    : ", s[3]))
  cat(paste0("\n- variance: ", s[4]))
  cat(paste0("\n- mode    : ", s[5]))
  cat(paste0("\n- skewness: ", s[6]))
  cat(paste0("\n- kurtosis: ", s[7]))
}

#' @title Binomial Mean
#' @description Finds the mean of a binomial random variable
#' @param trials, number of flips
#' @param prob, probability of heads
#' @export
#' @return mean
bin_mean <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mean(trials, prob))
}

#' @title Binomial variance
#' @description Finds the variance of a binomial random variable
#' @param trials, number of flips
#' @param prob, probability of heads
#' @export
#' @return variance
bin_variance <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_variance(trials, prob))
}

#' @title Binomial mode
#' @description Finds the mode of a binomial random variable
#' @param trials, number of flips
#' @param prob, probability of heads
#' @export
#' @return mode
bin_mode <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_mode(trials, prob))
}

#' @title Binomial skewness
#' @description Finds the skewness of a binomial random variable
#' @param trials, number of flips
#' @param prob, probability of heads
#' @export
#' @return skewness
bin_skewness <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_skewness(trials, prob))
}

#' @title Binomial kurtosis
#' @description Finds the kurtosis of a binomial random variable
#' @param trials, number of flips
#' @param prob, probability of heads
#' @export
#' @return kurtosis
bin_kurtosis <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  return(aux_kurtosis(trials, prob))
}

