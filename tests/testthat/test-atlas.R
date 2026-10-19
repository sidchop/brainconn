test_that("check_atlas works correctly", {
  valid_atlas <- data.frame(
    ROI.Name = c("Region1", "Region2"),
    x.mni = as.integer(c(10, 20)),
    y.mni = as.integer(c(15, 25)),
    z.mni = as.integer(c(5, 35))
  )
  expect_message(check_atlas(valid_atlas), "Atlas fits brainconn specifications")

  # Non-integer coordinates are fine
  valid_atlas$x.mni <- c(10.5, 20.2)
  expect_message(check_atlas(valid_atlas), "Atlas fits brainconn specifications")

  invalid_atlas <- valid_atlas[, c("ROI.Name", "x.mni", "y.mni")]
  expect_error(check_atlas(invalid_atlas), "File missing z.mni column")

  valid_atlas$x.mni <- c("a", "b")
  expect_error(check_atlas(valid_atlas), "need to be numeric")

  expect_message(check_atlas(list()), "Please convert atlas to a dataframe")
})

test_that("list_atlases lists atlases but not backgrounds or examples", {
  atlases <- suppressMessages(list_atlases())
  expect_true(all(c("schaefer300_n7", "aal90", "dk68") %in% atlases))
  expect_false(any(grepl("ICBM|example", atlases)))
})
