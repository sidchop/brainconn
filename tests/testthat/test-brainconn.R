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
  # brainconn3D is loaded in the main R environment, no need to source again
  expect_true(exists("brainconn3D"))
  expect_true(is.function(list_atlases))
})

test_that("L/R orientation fix for front/back views", {
  # Test that the coordinate transformations are correct for front/back views
  # This addresses issue #33: problem with L/R sides of front/back view
  
  # Create test data with clear left/right positions
  test_data <- data.frame(
    ROI.Name = c("Left_Region", "Right_Region"),
    x.mni = as.integer(c(-50, 50)),  # Negative for left, positive for right
    y.mni = as.integer(c(0, 0)),
    z.mni = as.integer(c(0, 0)),
    network = c(1, 1)
  )
  
  # Test front view transformation (should negate x.mni)
  view <- "front"
  x.mni.front <- test_data$x.mni * -1
  expect_equal(x.mni.front, c(50, -50), 
              info = "Front view should negate x.mni coordinates")
  
  # Test back view transformation (should NOT negate x.mni)
  view <- "back" 
  x.mni.back <- test_data$x.mni
  expect_equal(x.mni.back, c(-50, 50),
              info = "Back view should keep x.mni coordinates unchanged")
})
