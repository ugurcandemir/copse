#' @title
#' Ramsey Regression Equation Specification Error Test (RESET)
#'
#' @description
#' ResetTest() function applies Ramsey (RESET) Test to 'lm' and 'glm' objects.
#'
#' @param model An 'lm' or 'glm' object.
#' @param order The number of powers to be applied to the fitted values.
#' @param testStatistic In the literature there are two different formulas two compute the test statistic. With regards to that you can chose either "rss" or "r.squared".
#'
#' @return Ramsey (RESET) Test statistic and the associated p-value.
#'
#' @examples
#' y <- rnorm(50 , 100 , 15)
#' x <- runif(50 , 50 , 150 )
#' model1 <- lm(y ~ x)
#' ResetTest(model1 , 2 , "rss")
#' ResetTest(model1 , 2:3 , "rss")
#' ResetTest(model1 , 3:4 , "r.squared")
#'
#' @references {Ramsey, J. B. (1969). Tests for specification errors in classical linear least‐squares regression analysis. Journal of the Royal Statistical Society: Series B (Methodological), 31(2), 350-371.}
#' @export
ResetTest <- function(model , order = 2 , testStatistic = c("rss" , "r.squared")) {

  # Let's extract the information about the restricted model.
  restricted_mod <- model
  rrss <- sum(model$residuals^2)
  rmf <- model.frame(model)
  y <- rmf[ , 1 , drop = F]
  x <- rmf[ , -1 , drop = F]
  n <- nrow(rmf)
  k <- ncol(x)

  # We create the Z matrix whose columns are the respective powers of the y-hat.

  y_hat <- model$fitted.values
  z <- matrix(t(sapply(y_hat , "^" , order)) , ncol = length(order))
  z <- data.frame(z)
  z_names <- paste("yhat" ,  as.character(order) , sep = "")
  names(z) <- z_names

  # Now we can create the unrestricted(some texts call it the extended model) model
  # and extract the relevant information from it.

  xz <- cbind(x , z)
  yxz <- cbind(y , x , z)
  unrestricted_mod_formula <- as.formula( paste( names(y) , paste(names(xz) , collapse = " + ") , sep = " ~ ") )
  unrestricted_mod <- lm(formula = unrestricted_mod_formula  , data = yxz)
  urrss <- sum(unrestricted_mod$residuals^2)
  h <- ncol(z)

  # We find the test statistic and the p-value.
  test_statistic <- match.arg(testStatistic)

  if(test_statistic == "rss") {

    ramsey_test_statistic <- ( (rrss - urrss) / h ) / (urrss / (n - (k+h)) )

  } else if(test_statistic == "r.squared") {
    ramsey_test_statistic <- ( (summary(unrestricted_mod)$r.squared - summary(model)$r.squared) / h) / ((1 - summary(unrestricted_mod)$r.squared) / (n - length(coef((unrestricted_mod)))))
  }

  p_value <- pf(ramsey_test_statistic , h , n - (k+h) , lower.tail = F)

  # The output.
  test_output <- list( test_statistic = ramsey_test_statistic ,
                       p_value = p_value)

  class(test_output) <- 'ResetTest'

  return(test_output)



}

#' @keywords internal
#' @export
print.ResetTest <- function(test) {

  cat("Ramsey RESET Test\n" ,
      "Test Statistic =" , test$test_statistic , "\n" ,
      "P-Value =" , test$p_value  , '\n',
      "H0 : The model is correctly specified." , '\n')
}

