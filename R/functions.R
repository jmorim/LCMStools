#' @importFrom stringr str_c
#' @importFrom parallel makeCluster parSapply stopCluster detectCores
#' @export
# TODO
# parameter for TOF vs MRM (SRM) data conversion
convertDtoMZML = function(path, path.out = paste0(path,'/_mzML')) {
  file.paths = list.files(path=path, pattern='\\.d$', full.names=T)
  print(paste('file.paths:', file.paths))
  cmd = stringr::str_c('msconvert ', file.paths, ' --mzML --outdir ', path.out)
  print(paste('cmd: ', cmd))
  n.files = length(file.paths)
  if (n.files > 1) {
    cl = parallel::makeCluster(if (n.files < parallel::detectCores()) {
      n.files
    } else {
      parallel::detectCores() - 1
  })
    out = parallel::parSapply(cl, cmd, function(x) system(x, intern=T, invisible=F))
    print(paste('out: ',out))
    parallel::stopCluster(cl)
    print('cluster stopped')
  } else {
    system(cmd)
  }
}

#' @export
# Calculates the bounds around a mass for a given ppm threshold.
calcPPM = function(mz, ppm = 20) {
  mzr = mz + mz * c(-ppm, ppm) / 10^6
  return(mzr)
}