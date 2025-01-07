test_that("getCEChroms works", {
  ex.file = system.file('extdata', '_d/0176.d/0176.mzML', package='LCMStools')
  print(ex.file)
  ex.data = readSRMData(ex.file)
  print(ex.data)
  print(featureData(ex.data)$precursorCollisionEnergy)
  expect_no_error(getCEChroms(ex.data))
})
test_that("getCEData works", {
  ex.file = system.file('extdata', '_d/0176.d/0176.mzML', package='LCMStools')
  ex.data = readSRMData(ex.file)
  ex.peaks = findChromPeaks(ex.data, CentWaveParam(peakwidth=c(0.2, 0.25)))
  expect_no_error(getCEData(ex.peaks))
})