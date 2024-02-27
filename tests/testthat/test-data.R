test_that("Test data", {
  # Check that data available and of correct type.
  expect_type(gauteng, 'integer')
  expect_s3_class(gauteng, 'xts')
})
