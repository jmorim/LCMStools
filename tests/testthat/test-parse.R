test_that("parsing ms method creates a table", {
  expect_no_error(parseMSMethod(system.file('extdata', '_d/0001.d/AcqData/opt_dmrm.m/192_1.xml', package='LCMStools')))
})
test_that("parsing sample info creates a table", {
  expect_no_error(parseSampleInfo(system.file('extdata', '_d/0001.d/AcqData/sample_info.xml', package='LCMStools')))
})
