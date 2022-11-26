#' @title
#' Chow Test for Structural Change
#'
#' @description
#' ChowTest() function applies Chow Structural Change test to 'lm' and 'glm' objects.
#'
#' @param model An 'lm' or 'glm' object.
#' @param n1 The number of rows of the first part of the model until the suspected breakpoint.
#' @param n2 An optional parameter of the number of rows of the second part of the model after the breakpoint.
#'
#' @return The Chow Test statistic and the associated p-value.
#'
#' @importFrom stats lm pf
#'
#' @examples
#' y <- c(5*rnorm(30 , 100 , 5 ) + 60 , 2*rnorm(20 , 100 , 5)+20)
#' x <- 5*rnorm(50 , 100 , 5 ) + 20
#' model1 <- lm(y ~ x)
#' ChowTest(model = model1 , n1 = 30)
#' ChowTest(model = model1 , n1 = 30 , n2 = 20)
#'
#' @references {Chow, G. C. (1960). Tests of equality between sets of coefficients in two linear regressions. Econometrica: Journal of the Econometric Society, 591-605.}
#' @export



ChowTest <- function(model , n1 , n2 = NULL ) {

  # Extract model information
  model_data <- model$model
  model_formula <- model$call$formula
  m <- length(model$coefficients)

  if(!(is.null(n2))) {

    if( !(n1+n2 == nrow(model_data)) ) {
      warning("The sum of of n1 and n2 is not equal to the sample size.\n So , it is automatically corrected.")
    }
  }

  n1 <- n1
  n2 <- nrow(model_data) - n1


  # Compute the residual sum of squares
  rss_total <- sum(model$residuals^2)

  submodel_1 <- lm(data = model_data[1: n1 , ] , formula = model_formula)
  rss_1 <- sum(submodel_1$residuals^2)


  submodel_2 <- lm(data = model_data[(n1 +1): nrow(model_data) , ] , formula = model_formula)
  rss_2 <- sum(submodel_2$residuals^2)


  # Compute the test statistic.
  chow_test_statistic <- ( (rss_total - (rss_1 + rss_2) ) / m ) / ( (rss_1 + rss_2) /(nrow(model_data) - 2*m) )

  # Compute the p-value
  p_value <- pf(chow_test_statistic , m , (nrow(model_data) - 2*m) , lower.tail = F)

  # The output.

  test_output <- list( test_statistic = chow_test_statistic ,
                       p_value = p_value)

  class(test_output) <- 'ChowTest'

  return(test_output)


  }


#' @keywords internal
#' @export
print.ChowTest <- function(x) {

  cat("Chow Test for Structural Break\n" ,
      "Test Statistic =" , x$test_statistic , "\n" ,
      "P-Value =" , x$p_value  , '\n',
      "H0 : There is no structural break." , '\n')
}
