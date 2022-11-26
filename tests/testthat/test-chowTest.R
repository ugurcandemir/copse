
set.seed(100)
y <- c(5*rnorm(30 , 100 , 5 ) + 60 , 2*rnorm(20 , 100 , 5)+20)
x <- 5*rnorm(50 , 100 , 5 ) + 20
model1 <- lm(y ~ x)

test_that("multiplication works", {
  expect_lt(ChowTest(model1 , 30)$p_value , 0.05)
})
