test_that("conversion works", {
  ConvertDtoMZML(path='..')
  expect_condition(File.exists('../_mzML/0001_r001.mzML'))
  file.remove('../_mzML/0001-r001.mzML')
})
