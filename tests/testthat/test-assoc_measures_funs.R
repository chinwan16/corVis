test_that("all tbl_* functions work", {

  df <- iris

  #tbl_cor
  pearson <- tbl_cor(df)
  expect_s3_class(pearson,c("pairwise","tbl_df","tbl","data.frame"))

  spearman <- tbl_cor(df, method = "spearman")
  expect_s3_class(spearman,c("pairwise","tbl_df","tbl","data.frame"))

  kendall <- tbl_cor(df, method = "kendall")
  expect_s3_class(kendall,c("pairwise","tbl_df","tbl","data.frame"))

  #tbl_cancor (canonical correlation)
  can_cor <- tbl_cancor(df)
  expect_s3_class(can_cor,c("pairwise","tbl_df","tbl","data.frame"))

  #tbl_dcor (distance correlation)
  distance <- tbl_dcor(df)
  expect_s3_class(can_cor,c("pairwise","tbl_df","tbl","data.frame"))

  #tbl_easy (easystats package measures)
  easystats_per <- tbl_easy(df,method = "percentage")
  expect_s3_class(easystats_per,c("pairwise","tbl_df","tbl","data.frame"))

  easystats_hoeff <- tbl_easy(df,method = "shepherd")
  expect_s3_class(easystats_hoeff,c("pairwise","tbl_df","tbl","data.frame"))

  #tbl_mine (mine measures)
  mine_mic <- tbl_mine(df,method = "mic")
  expect_s3_class(mine_mic,c("pairwise","tbl_df","tbl","data.frame"))

  mine_mas <- tbl_mine(df, method="mas")
  expect_s3_class(mine_mas,c("pairwise","tbl_df","tbl","data.frame"))

  #tbl_nmi (normalized mutual information coefficient)
  nmi <- tbl_nmi(df)
  expect_s3_class(nmi,c("pairwise","tbl_df","tbl","data.frame"))

  #tbl_tau (Kendall Tau coefficients from DescTools package)
  KendallTauA <- tbl_tau(df, method = "A")
  expect_s3_class(KendallTauA,c("pairwise","tbl_df","tbl","data.frame"))

  #tbl_uncertainty (Uncertainty coefficient)
  uncertainty <- tbl_uncertainty(df)
  expect_s3_class(uncertainty,c("pairwise","tbl_df","tbl","data.frame"))


})
