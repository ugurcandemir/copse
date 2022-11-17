
#' @keywords internal
#' @export
# Here we write a simple function to compute combination.
combination <- function(n , r) {
  factorial(n) /(factorial(n - r) * factorial(r))
}

#' @keywords internal
#' @export
# we write two different functions for large and small sample tests. These functions
# are given n1, n2 and the number of runs and they return the inference back. These
# two functions will later be embedded in the runsTest() functions.
SmallSampleRunsInference <- function(n1 , n2 , runs , alternative , alpha) {

  minimum_number_of_runs <- 2
  maximum_number_of_runs <- 2*n2+1

  k <- function(r) {
    if(r %% 2 == 0 ) {
      k <- r/2
    } else {
      k <- (r-1)/2
    }
    k
  }

  ProbOdd <- function(n1 , n2 , r) {
    k <- k(r = r)
    x <- combination(n = (n1-1) , r = k)  * combination(n = (n2-1) , r = (k - 1))
    y <- combination(n = (n2-1) , r = k)  * combination(n = (n1-1) , r = (k - 1))
    z <- combination(n = (n1 + n2) , r = n1)
    prob <- (x+y)/z
    prob
  }


  ProbEven <- function(n1 , n2 , r) {
    k <- k(r = r)
    x <- combination(n = (n1-1) , r = (k -1))  * combination(n = (n2-1) , r = (k - 1))
    y <- combination(n = (n1 + n2) , r = n1 )
    prob <- (2*x)/y
    prob
  }

  total_prob <- data.frame()

  for (r in minimum_number_of_runs:(maximum_number_of_runs -1) ) {
    if(r %% 2 == 0) {
      total_prob <- rbind(total_prob , c(r ,  ProbEven(n1 = n1 , n2 = n2 , r = r)))
    } else {
      total_prob <- rbind(total_prob , c(r , ProbOdd(n1 = n1 , n2 = n2 , r = r)))
    }
  }

  names(total_prob) <- c("r" , "probs")

  # Finding Critical Values.

  CriticalValueLess <- function(total_prob , alpha) {
    for (i in 1:nrow(total_prob)) {
      if(sum(total_prob$probs[ 1:i ]) >= alpha) {
        c <- total_prob$r[i - 1]
        break
      }
    }
    c
  }

  CriticalValueGreater <- function(total_prob , alpha) {
    for (i in nrow(total_prob):1) {
      if(sum(total_prob$probs[ nrow(total_prob) :i ]) >= alpha) {
        c <- total_prob$r[i +1]
        break
      }
    }
    c
  }

  CriticalValueTwoSided <- function(total_prob , alpha ) {
    alpha_over_two <- alpha/2
    c1 <- CriticalValueLess(total_prob = total_prob , alpha = alpha_over_two)
    c2 <- CriticalValueGreater(total_prob = total_prob , alpha = alpha_over_two)
    c <- c(c1 , c2)
    c
  }

  if(alternative == "two.sided") {
    c <- CriticalValueTwoSided(total_prob = total_prob , alpha = alpha)
  } else if(alternative == "less") {
    c <- CriticalValueLess(total_prob = total_prob , alpha = alpha)
  } else if(alternative == "greater") {
    c <- CriticalValueGreater(total_prob = total_prob , alpha = alpha)
  }
  c
}

#' @keywords internal
#' @export
LargeSampleRunsInference <- function(n1 , n2 , runs , alternative) {
  N <- n1+n2
  mu_r <- 1+((2*n1*n2)/N)
  var_r <- (2*n1*n2*( (2*n1*n2) - N)) / ( (N^2)*(N-1))
  sd_r <- sqrt(var_r)
  z <- (runs - mu_r)/sd_r

  if(alternative == "two.sided") {
    if(z >= 0) {
      p_value <- 2*(pnorm(q = z , lower.tail = F))
    } else {
      p_value <- 2*(pnorm(q = z , lower.tail = T))
    }

  } else if(alternative == "less") {
    p_value <- pnorm(q = z , lower.tail = T)
  } else if(alternative == "greater") {
    p_value <- pnorm(q = z , lower.tail = F)
  }

  p_value
}
