
# set.seed(123)
test_df <- data.frame(id=1:10, cat1=c(rep("red", 4), rep("blue", 6)), cat2=c(rep("front", 6), rep("back", 4)), val1=sqrt(31:40), val2=(13:22)**2)
expected_dt <- data.table::data.table(val1=c(5.70, 6.12), val2=c(211.50, 383.17), group=c('red - mean', 'blue - mean'))

test_that("draft test", {

  expect_equal(get_avg(test_df, grp_col="cat1", val_col=c("val1", "val2")), expected_dt)

})
