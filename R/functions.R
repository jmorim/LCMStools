#' @importFrom stringr str_c
#' @importFrom parallel makeCluster parSapply stopCluster detectCores
#' @importFrom xml2 read_xml xml_find_all xml_text xml_name xml_children
#' @importFrom dplyr bind_rows
#' @importFrom purrr pluck map set_names
#' 
#' @export
# TODO
# parameter for TOF vs MRM (SRM) data conversion
# rewrite to take a list of files instead of a path
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

#' @export
# Parses a data file's sample_info.xml to a named vector
# Combine with map_dfr to generate a data table for multiple data files
parseSampleInfo = function(sample.info.xml) {
  sample.info = read_xml(sample.info.xml)
  xml.fields = sample.info |> xml_find_all('//Field')
  sample.info.values = xml.fields |> xml_find_all('//Value') |> xml_text(trim=T)
  sample.info.fields = xml.fields |> xml_find_all('//Name') |> xml_text()
  names(sample.info.values) = sample.info.fields
  return(sample.info.values)
}

# Parses the MRM trigger info in 192_1.xml
# old version took the xml file
#parseMRMTrigger = function(ms.xml) {
#  mrm.info = ms.xml |> read_xml() |> xml_find_all('//triggerMRMInfo')
#  values = mrm.info |> map(~ {
#    xml_children(.) |> xml_text()
#  })
#  names = mrm.info |> pluck(1) |> xml_children() |> xml_name()
#  trigger.table = map(values, ~ set_names(., names)) |> bind_rows()
#  return(trigger.table)
#}
parseMRMTriggerInfo = function(xml) {
  trigger.info = xml |> xml_find_all('//triggerMRMInfo')
  values = trigger.info |> map(~ xml_children(.) |> xml_text())
  names = trigger.info |> pluck(1) |> xml_children() |> xml_name()
  trigger.table = map(values, ~ set_names(., names)) |> bind_rows()
  return(trigger.table)
}

# Parses scan element info in 192_1.xml
parseScanElement = function(xml) {
  scan.info = xml |> xml_find_all('//scanElement')
  values = scan.info |> map(~ xml_children(.) |> xml_text())
  names = scan.info |> pluck(1) |> xml_children() |> xml_name()
  trigger.mrm.info = scan.info |> map(~ parseMRMTriggerInfo(.))

  scan.table = map(values, ~ set_names(., names)) |> bind_rows()
  scan.table$triggerMRMInfo = trigger.mrm.info
  return(scan.table)
}

#' @export
#' Reduces peak list by maximum intensity ions as quantifier and second-most
#' intense as qualifier
#reducePeaklist = function(peaklist) {
#  # For each pseudospectrum group (psg)
#  psgs = 
#}