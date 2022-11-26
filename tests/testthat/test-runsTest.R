
set.seed(100)
ch_vec <- sample(x = c("John" , "Jane" ) , size = 30 , replace = TRUE , prob = c(40 , 60))
fac_vec <- as.factor(sample(x = c("John" , "Jane" ) , size = 30 , replace = TRUE , prob = c(40 , 60)))
num_vec <- round(rnorm(n = 30 , mean = 100 , sd = 10 ))


test_that("Chow Test", {
  expect_equal(RunsTest(ch_vec)$runs , 15)
  expect_equal(RunsTest(fac_vec)$runs , 11)
  expect_equal(RunsTest(num_vec)$runs , 19)
})

