context("greedy_knapsack")

set.seed(42)
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)

test_that("Correct object is returned", {
  expect_silent(gk <- greedy_knapsack(x = knapsack_objects[1:8,], W = 3500))
  expect_named(gk, c("value", "elements"))
})

test_that("functions rejects errounous input.", {
  expect_error(greedy_knapsack("hej", 3500))
  expect_error(greedy_knapsack(x = knapsack_objects[1:8,], W = -3500))
})

test_that("Function return correct results.", {
  bfk <- greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
  expect_equal(round(bfk$value), 195851)
  expect_true(all(round(bfk$elements) %in% c(256,530,701,559,89,75,759,626,219,244,63,672,455,764,329,77,705,320,110,509,762,729,691,283,553,620,341,187,83,707,511,322)))
  
  bfk <- greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)
  expect_equal(round(bfk$value), 185278)
  expect_true(all(round(bfk$elements) %in% c(256,530,1186,951,701,1157,973,559,1019,89,75,759,845,1153,626,897,1106,219,1065,1070,244,1036,63,672,455,826,764,981,329,77,320,110)))
  
  st <- system.time(gk <- greedy_knapsack(x = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[2] <= 0.01)
  
  gk <- greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
  expect_equal(round(gk$value), 195851)
  
  gk <- greedy_knapsack(x = knapsack_objects[1:1200,], W = 3500)
  expect_equal(round(gk$value), 252439)
})

