
set.seed(100)
y <- rnorm(50 , 100 , 15)
x <- runif(50 , 50 , 150 )
model1 <- lm(y ~ x)

test_that("Reset Test", {
  expect_gt(ResetTest(model1 , 2 , "rss")$p_value , 0.10)
  expect_lt(ResetTest(model1 , 2:3 , "rss")$p_value , 0.10)
  expect_lt(ResetTest(model1 , 3:4 , "r.squared")$p_value , 0.10)
})


