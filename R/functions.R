#' @importFrom stringr str_c
#' @importFrom parallel makeCluster parSapply stopCluster detectCores
#' @export
# TODO
# parameter for TOF vs MRM (SRM) data conversion
ConvertDtoMZML = function(path, path.out = paste0(path,'/_mzML')) {
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