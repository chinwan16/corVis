test_that("calc_assoc function returns object with correct class", {

  df <- iris

  updated_assoc <- update_assoc(num_pair = "tbl_nmi")
  assoc <- calc_assoc(iris,types = updated_assoc)
  expect_s3_class(assoc,c("pairwise","tbl_df","tbl","data.frame"))

  updated_assoc <- update_assoc(mixed_pair = "tbl_uncertainty")
  assoc <- calc_assoc(iris,types = updated_assoc)
  expect_s3_class(assoc,c("pairwise","tbl_df","tbl","data.frame"))

})


test_that("calc_assoc function returns measures value less than 1", {

  df <- iris

  updated_assoc <- update_assoc(num_pair = "tbl_nmi")
  assoc <- calc_assoc(iris,types = updated_assoc)
  expect_lte(sample(assoc$measure,1),1)

  updated_assoc <- update_assoc(mixed_pair = "tbl_uncertainty")
  assoc <- calc_assoc(iris,types = updated_assoc)
  expect_lte(sample(assoc$measure,1),1)


})


test_that("calc_assoc_by function returns object with correct class", {

  df <- iris

  updated_assoc <- update_assoc(num_pair = "tbl_nmi")
  assoc_by <- calc_assoc_by(iris,types = updated_assoc,by="Species")
  expect_s3_class(assoc_by,c("pairwise","tbl_df","tbl","data.frame"))

  updated_assoc <- update_assoc(num_pair_argList = "kendall")
  assoc_by <- calc_assoc_by(iris,types = updated_assoc,by="Species")
  expect_s3_class(assoc_by,c("pairwise","tbl_df","tbl","data.frame"))

})


test_that("calc_assoc_by function returns measures value less than 1", {

  df <- iris

  updated_assoc <- update_assoc(num_pair = "tbl_nmi")
  assoc_by <- calc_assoc_by(iris,types = updated_assoc,by="Species")
  expect_lte(sample(assoc_by$measure,1),1)

  updated_assoc <- update_assoc(mixed_pair = "tbl_uncertainty")
  assoc_by <- calc_assoc_by(iris,types = updated_assoc,by="Species")
  expect_lte(sample(assoc_by$measure,1),1)


})
