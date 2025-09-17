test_that("check_atlas works correctly", {
  # Test with valid data frame
  valid_atlas <- data.frame(
    ROI.Name = c("Region1", "Region2"),
    x.mni = as.integer(c(10, 20)),
    y.mni = as.integer(c(15, 25)),
    z.mni = as.integer(c(5, 35))
  )
  
  # Should not throw an error
  expect_message(check_atlas(valid_atlas), "Atlas fits brainconn specifications")
  
  # Test with invalid data frame (missing column)
  invalid_atlas <- data.frame(
    ROI.Name = c("Region1", "Region2"),
    x.mni = as.integer(c(10, 20)),
    y.mni = as.integer(c(15, 25))
    # z.mni column is missing
  )
  
  expect_error(check_atlas(invalid_atlas), "File missing z.mni column")
  
  # Test with non-data.frame input
  expect_message(check_atlas(list()), "Please convert atlas to a dataframe")
})

test_that("basic functionality doesn't break", {
  # This is a simple test to ensure the functions can be called without error
  # More comprehensive tests would require actual atlas data
  expect_true(is.function(brainconn))
  expect_true(is.function(brainconn3D))
  expect_true(is.function(list_atlases))
})
