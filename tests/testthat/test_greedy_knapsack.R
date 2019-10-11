context("greedy_knapsack")

suppressWarnings(RNGversion("3.5.9"))
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
  expect_equal(round(bfk$value), 194924)
  expect_true(all(round(bfk$elements) %in% c(92,574,472,80,110,537,332,117,37,776,577,288,234,255,500,794 ,55,290,436,346,282,764,599,303,345,300,243 ,43,747,35,77,229,719,564,401)))
  
  bfk <- greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)
  expect_equal(round(bfk$value), 213298)
  expect_true(all(round(bfk$elements) %in% c(92,574,472,80,110,840,537,1000,332,117,37,1197,1152,947,904,776,577,288,1147,1131,234,255,1006,833,1176,1092,873,828,1059,500,1090,794,1033,1134)))
  
  st <- system.time(gk <- greedy_knapsack(x = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[2] <= 0.01)
  
  gk <- greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
  expect_equal(round(gk$value), 194924)
  
  gk <- greedy_knapsack(x = knapsack_objects[1:1200,], W = 3500)
  expect_equal(round(gk$value), 276034)
})

