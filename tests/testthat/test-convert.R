#test_that("conversion works", {
#  convertDtoMZML(path='..')
#  expect_condition(File.exists('../_mzML/0001_r001.mzML'))
#  file.remove('../_mzML/0001-r001.mzML')
#})
test_that("calcPPM works", {
  expect_equal(calcPPM(100), c(99.9980, 100.0020))
})
