#' @title
#' Wald–Wolfowitz (Runs) Test
#'
#' @description
#' Generic function for Wald–Wolfowitz runs test.
#'
#' @param x A 'numeric' , 'character 'or 'factor' object.
#' @param ... Additional arguments.
#'
#' @examples
#' ch_vec <- sample(x = c("John" , "Jane" ) , size = 30 , replace = TRUE , prob = c(40 , 60))
#' RunsTest(ch_vec)
#'
#' fac_vec <- as.factor(sample(x = c("John" , "Jane" ) , size = 30 , replace = TRUE , prob = c(40 , 60)))
#' RunsTest(fac_vec)
#'
#' num_vec <- round(rnorm(n = 30 , mean = 100 , sd = 10 ))
#' RunsTest(num_vec)
#'
#' @return Wald–Wolfowitz runs test results , the associated p-value or the critical values.
#'
#' @export
RunsTest <- function(x , ...) {
  UseMethod("RunsTest" , x)
}


#' @title
#' Wald–Wolfowitz (Runs) Test
#'
#' @description
#' RunsTest Method for 'numeric' class.
#'
#' @param x A 'numeric' object.
#' @param cutPoint The cut-off point for quantitative runs test. 'median' , 'mean' or a custom value is accepted. Tha values that are same with the cut-off point will be discarded as the convential algorithm suggests.
#' @param alpha The level of significance. It is used only in small sample (exact) test , since for large sample tests (asymptotic) p-value will be provided.
#' @param alternative The tail of the test. "two.sided" , "greater" and "less" are available.
#' @param exact If TRUE exact critical values will be used  otherwise normal approximation will be returned. By default , if both n1 and n2 are smaller than or equal to 20 it is considered a small sample and the exact values will be used.
#'
#' @examples
#' num_vec <- round(rnorm(n = 30 , mean = 100 , sd = 10 ))
#' RunsTest(num_vec)
#'
#' @return Wald–Wolfowitz runs test results , the associated p-value or the critical values.
#'
#' @export
RunsTest.numeric <- function(x  ,
                             cutPoint = "median",
                             alpha = 0.05 ,
                             alternative = c("two.sided" , "greater" , "less" ) ,
                             exact = NULL ){

  # Setting the cut-off point for numerical runs test.
  if(cutPoint %in% c("median" , "mean")) {
    cut_point_method <- match.fun(cutPoint)
    cut_point_value <- cut_point_method(x)
  } else if(is.numeric(cutPoint)) {
    cut_point_value <- cutPoint
  } else {
    stop("You have passed an invalid argument to the cutPoint parameter.\nYou should either give a numeric value or pass one of the \"mean\" or \"median\". ")
  }

  # Tail of the test.
  alternative <- match.arg(alternative, alternative)


  # Finding n1 and n2 sets.
  if(any(x == cut_point_value)) {
    x <- x[ - (x == cut_point_value)]
  }

  below_and_above <- ifelse(test = x < cut_point_value , yes = "below" , no = "above")

  below <- sum(below_and_above == "below")
  above <- sum(below_and_above == "above")

  if (below <= above) {
    n1 <- above ; n2 <- below
  } else {
    n2 <- above ; n1 <- below
  }

  # Here we specify whether the exact or the asymptotic method will be used.
  # This is decided by either the user or the function.
  if(is.null(exact)) {

    if(n1 >20 | n2 > 20) {
      sample_size <- "large"
    } else {
      sample_size <- "small"
    }


  } else if(exact == F) {

    sample_size <- "large"

  } else if(exact == T) {

    sample_size <- "small"

  } else {

    stop("The argument to the exact parameter should be either a boolean or be left NULL which is the default value.")
  }



  # Finding runs.
  runs <- 1

  for (i in 1:(length(below_and_above) -1 ) ) {
    if(below_and_above[i] != below_and_above[i+1]) {
      runs <- runs + 1
    }
  }

  if(runs <2) {
    print(paste("Runs : " , runs))
    stop("The number of runs cannot be less than 2.\nIf you have passed a custom argument to the cutPoint parameter , please check it. ")
  }

  # Here we compute the exact and asymptotic test statistics.
  c <- SmallSampleRunsInference(n1 = n1 ,
                                   n2 = n2 ,
                                   runs = runs ,
                                   alternative = alternative ,
                                   alpha = alpha)

  p_value <- LargeSampleRunsInference(n1 = n1 ,
                                         n2 = n2 ,
                                         runs = runs ,
                                         alternative = alternative)

  # And we return the test output.
  if(sample_size == "small"){
    test_output <- list(cut_point_value = cut_point_value ,
                        alpha = alpha ,
                        sample_size = "Small Sample" ,
                        alternative = tail ,
                        critical_value = c ,
                        n1 = n1 ,
                        n2 = n2 ,
                        runs = runs)

    class(test_output) <- c("SmallSample" , "RunsTest")

  } else if(sample_size == "large") {

    test_output <- list(cut_point_value = cut_point_value ,
                        sample_size = "Large Sample" ,
                        alternative = tail ,
                        p_value = p_value ,
                        n1 = n1 ,
                        n2 = n2 ,
                        runs = runs)

    class(test_output) <- c("LargeSample" , "RunsTest" )
  }

  return(test_output)
}

#' @title
#' Wald–Wolfowitz (Runs) Test
#'
#' @description
#' RunsTest Method for 'character' class.
#'
#' @param x A 'character' object.
#' @param alpha The level of significance. It is used only in small sample (exact) test , since for large sample tests (asymptotic) p-value will be provided.
#' @param alternative The tail of the test. "two.sided" , "greater" and "less" are available.
#' @param exact If TRUE exact critical values will be used  otherwise normal approximation will be returned. By default , if both n1 and n2 are smaller than or equal to 20 it is considered a small sample and the exact values will be used.
#'
#' @examples
#' ch_vec <- sample(x = c("John" , "Jane" ) , size = 30 , replace = TRUE , prob = c(40 , 60))
#' RunsTest(ch_vec)
#'
#' @return Wald–Wolfowitz runs test results , the associated p-value or the critical values.
#'
#' @export
RunsTest.character <- function(x  ,
                               alpha = 0.05 ,
                               alternative = c("two.sided" , "greater" , "less" ) ,
                               exact = NULL) {

  alternative <- match.arg(alternative, alternative)

  # Finding n1 and n2 sets.
  data_vec <- as.vector(table(x))
  if(length(data_vec) != 2) {
    stop("Runs test is only used for sequences consisting two different classes.")
  }


  if(data_vec[1] > data_vec[2]) {
    n1 <- data_vec[1]
    n2 <- data_vec[2]
  } else {
    n2 <- data_vec[1]
    n1 <- data_vec[2]
  }

  # Here we specify whether the exact or the asymptotic method will be used.
  # This is decided by either the user or the function.
  if(is.null(exact)) {

    if(n1 >20 | n2 > 20) {
      sample_size <- "large"
    } else {
      sample_size <- "small"
    }


  } else if(exact == F) {

    sample_size <- "large"

  } else if(exact == T) {

    sample_size <- "small"

  } else {

    stop("The argument to the exact parameter should be either a boolean or be left NULL which is the default value.")
  }

  # Finding runs.
  runs <- 1

  for (i in 1:(length(x) -1 ) ) {
    if(x[i] != x[i+1]) {
      runs <- runs + 1
    }
  }

  if(runs <2) {
    print(paste("Runs : " , runs))
    stop("The number of runs cannot be less than 2.\nIf you have passed a custom argument to the cutPoint parameter , please check it. ")
  }

  # Here we compute the exact and asymptotic test statistics.
  c <- SmallSampleRunsInference(n1 = n1 ,
                                   n2 = n2 ,
                                   runs = runs ,
                                   alternative = alternative ,
                                   alpha = alpha)

  p_value <- LargeSampleRunsInference(n1 = n1 ,
                                         n2 = n2 ,
                                         runs = runs ,
                                         alternative = alternative)

  # And we return the test output.
  if(sample_size == "small"){
    test_output <- list(alpha = alpha ,
                        sample_size = "Small Sample" ,
                        alternative = tail ,
                        critical_value = c ,
                        n1 = n1 ,
                        n2 = n2 ,
                        runs = runs)

    class(test_output) <- c("SmallSample" , "RunsTest")

  } else if(sample_size == "large") {

    test_output <- list(sample_size = "Large Sample" ,
                        alternative = tail ,
                        p_value = p_value ,
                        n1 = n1 ,
                        n2 = n2 ,
                        runs = runs)

    class(test_output) <- c("LargeSample" , "RunsTest" )
  }

  return(test_output)

}

#' @title
#' Wald–Wolfowitz (Runs) Test
#'
#' @description
#' RunsTest Method for 'factor' class.
#'
#' @param x A 'factor' object.
#' @param alpha The level of significance. It is used only in small sample (exact) test , since for large sample tests (asymptotic) p-value will be provided.
#' @param alternative The tail of the test. "two.sided" , "greater" and "less" are available.
#' @param exact If TRUE exact critical values will be used  otherwise normal approximation will be returned. By default , if both n1 and n2 are smaller than or equal to 20 it is considered a small sample and the exact values will be used.
#'
#' @examples
#' fac_vec <- as.factor(sample(x = c("John" , "Jane" ) , size = 30 , replace = TRUE , prob = c(40 , 60)))
#' RunsTest(fac_vec)
#'
#' @return Wald–Wolfowitz runs test results , the associated p-value or the critical values.
#'
#' @export
RunsTest.factor <- function(x  ,
                            alpha = 0.05 ,
                            alternative = c("two.sided" , "greater" , "less" ) ,
                            exact = NULL) {

  alternative <- match.arg(alternative, alternative)

  # Finding n1 and n2 sets.
  data_vec <- as.vector(table(x))
  if(length(data_vec) != 2) {
    stop("Runs test is only used for sequences consisting two different classes.")
  }


  if(data_vec[1] > data_vec[2]) {
    n1 <- data_vec[1]
    n2 <- data_vec[2]
  } else {
    n2 <- data_vec[1]
    n1 <- data_vec[2]
  }

  # Here we specify whether the exact or the asymptotic method will be used.
  # This is decided by either the user or the function.
  if(is.null(exact)) {

    if(n1 >20 | n2 > 20) {
      sample_size <- "large"
    } else {
      sample_size <- "small"
    }


  } else if(exact == F) {

    sample_size <- "large"

  } else if(exact == T) {

    sample_size <- "small"

  } else {

    stop("The argument to the exact parameter should be either a boolean or be left NULL which is the default value.")
  }

  # Finding runs.
  runs <- 1

  for (i in 1:(length(x) -1 ) ) {
    if(x[i] != x[i+1]) {
      runs <- runs + 1
    }
  }

  if(runs <2) {
    print(paste("Runs : " , runs))
    stop("The number of runs cannot be less than 2.\nIf you have passed a custom argument to the cutPoint parameter , please check it. ")
  }

  # Here we compute the exact and asymptotic test statistics.
  c <- SmallSampleRunsInference(n1 = n1 ,
                                   n2 = n2 ,
                                   runs = runs ,
                                   alternative = alternative ,
                                   alpha = alpha)

  p_value <- LargeSampleRunsInference(n1 = n1 ,
                                         n2 = n2 ,
                                         runs = runs ,
                                         alternative = alternative)

  # And we return the test output.
  if(sample_size == "small"){
    test_output <- list(alpha = alpha ,
                        sample_size = "Small Sample" ,
                        alternative = tail ,
                        critical_value = c ,
                        n1 = n1 ,
                        n2 = n2 ,
                        runs = runs)

    class(test_output) <- c("SmallSample" , "RunsTest")

  } else if(sample_size == "large") {

    test_output <- list(sample_size = "Large Sample" ,
                        alternative = tail ,
                        p_value = p_value ,
                        n1 = n1 ,
                        n2 = n2 ,
                        runs = runs)

    class(test_output) <- c("LargeSample" , "RunsTest" )
  }

  return(test_output)

}


#' @keywords internal
#' @export
print.SmallSample <- function(object) {
  cat("Wald-Wolfowitz (Runs) Test - " , object$sample_size , "\n" ,
      "n1 :" , object$n1 , ", n2 :" , object$n2 , ", runs :" , object$runs , "\n" ,
      "Critical Value(s) : " , object$critical_value , "\n" ,
      "H0 : The sequence is random." , "\n")
}

#' @keywords internal
#' @export
print.LargeSample <- function(object) {
  cat("Wald-Wolfowitz (Runs) Test - " , object$sample_size , "\n" ,
      "n1 :" , object$n1 , ", n2 :" , object$n2 , ", runs :" , object$runs , "\n" ,
      "P-Value : " , object$p_value , "\n" ,
      "H0 : The sequence is random." , "\n" )
}

