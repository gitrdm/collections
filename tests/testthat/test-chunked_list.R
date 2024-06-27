test_that("initialize works correctly", {
  cl <- ChunkedList$new()
  expect_equal(cl$size(), 0)
  expect_equal(cl$actualSize(), 10) # Default initial size

  cl_with_data <- ChunkedList$new(initial_data = list(1, 2, 3))
  expect_equal(cl_with_data$size(), 3)
  expect_equal(cl_with_data$get_all(), list(1, 2, 3))
})

test_that("add and add_many work correctly", {
  cl <- ChunkedList$new()
  cl$add(1)
  expect_equal(cl$size(), 1)
  expect_equal(cl$get_at_index(1), 1)

  cl$add_many(list(2, 3))
  expect_equal(cl$size(), 3)
  expect_equal(cl$get_all(), list(1, 2, 3))
})

test_that("get_at_index handles out of bounds correctly", {
  cl <- ChunkedList$new()
  cl$add(1)
  expect_error(cl$get_at_index(2), "Index out of bounds")
})

test_that("size and actualSize report correctly", {
  cl <- ChunkedList$new(initial_size = 5)
  expect_equal(cl$actualSize(), 5)
  cl$add_many(list(1, 2, 3))
  expect_equal(cl$size(), 3)
})

test_that("get_all retrieves all non-NULL elements", {
  cl <- ChunkedList$new()
  cl$add_many(list(1, 2, 3))
  expect_equal(cl$get_all(), list(1, 2, 3))
})

test_that("iterator works correctly", {
  cl <- ChunkedList$new()
  cl$add_many(list(1, 2, 3))
  iterator <- cl$iterator()
  expect_equal(iterator$get_next(), 1)
  expect_equal(iterator$get_next(), 2)
  expect_equal(iterator$get_next(), 3)
  expect_equal(iterator$get_next(), NULL) # End of list
  iterator$reset()
  expect_equal(iterator$get_next(), 1) # After reset
})

test_that("equals compares ChunkedList instances correctly", {
  cl1 <- ChunkedList$new()
  cl1$add_many(list(1, 2, 3))

  cl2 <- ChunkedList$new()
  cl2$add_many(list(1, 2, 3))

  cl3 <- ChunkedList$new()
  cl3$add_many(list(4, 5, 6))

  expect_true(cl1$equals(cl2))
  expect_false(cl1$equals(cl3))
})
