#' @importFrom stringr str_c
#' @importFrom parallel makeCluster parSapply stopCluster detectCores
#' @importFrom xml2 read_xml xml_find_all xml_text xml_name xml_children
#' @importFrom dplyr bind_rows tibble
#' @importFrom purrr pluck map set_names map_df
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
  trigger.info = xml |> xml_find_all('.//triggerMRMInfo')
  values = trigger.info |> map(~ xml_children(.) |> xml_text())
  names = trigger.info |> pluck(1) |> xml_children() |> xml_name()
  trigger.table = map(values, ~ set_names(., names)) |> bind_rows()
  return(trigger.table)
}

# Parses scan element info in 192_1.xml containing multiple MRM trigger tables
parseScanElement = function(xml) {
  scan.info = xml |> xml_find_all('.//scanElement')
  values = scan.info |> map(~ xml_children(.) |> xml_text())
  names = scan.info |> pluck(1) |> xml_children() |> xml_name()
  trigger.mrm.info = scan.info |> map(~ {
    if (xml_find_all(., './/triggerMRMInfo') |> length() > 0) {
      parseMRMTriggerInfo(.)
    } else {
      NA
    }
  })

  scan.table = map(values, ~ set_names(., names)) |> bind_rows()
  scan.table$triggerMRMInfo = trigger.mrm.info
  return(scan.table)
}

# Parses scan segment, which contains multiple scan elements
parseScanSegment = function(xml) {
  scan.segment = xml |> xml_find_all('.//scanSegment')
  values = scan.segment |> map(~ xml_children(.) |> xml_text())
  names = scan.segment |> pluck(1) |> xml_children() |> xml_name()
  scan.info = scan.segment |> map(parseScanElement)

  scan.segment.table = map(values, ~ set_names(., names)) |> bind_rows()
  scan.segment.table$scanElements = scan.info
  return(scan.segment.table)
}

# Parses the source parameters from 192_1.xml
parseSourceParams = function(xml) {
  source.params = xml |> xml_find_all('.//sourceParameter')
  params = source.params |> map_df(~ {
    id = xml_find_first(., 'id') |> xml_text()
    pos = xml_find_first(., 'posPolarityValue') |> xml_text() |> as.numeric()
    neg = xml_find_first(., 'negPolarityValue') |> xml_text() |> as.numeric()
    tibble(
      id = c(id, id),
      polarity = c('positive', 'negative'),
      value = c(pos, neg)
    )
  })
  return(params)
}

# Parses ion funnel parameters: positive/negative, high rf/low rf, voltage
parseIonFunnel = function(xml) {
  ion.funnel.info = xml |> xml_find_all('.//IonFunnel') |> xml_children()
  ion.funnel.table = ion.funnel.info |> map_df(~ {
    name = xml_name(.)
    value = xml_text(.) |> as.numeric()
    tibble(
      polarity = if_else(str_detect(name, '^Pos'), 'positive', 'negative'),
      pressure = str_extract(name, 'HP|LP'),
      voltage = value
    )
  })
  return(ion.funnel.table)
}

# Parses time segment containing startTime, diverterValveState, sourceParameters,
# isDataSaved, scanSegments, and IonFunnel parameters
parseTimeSegment = function(xml) {
  time.info = xml |> xml_find_all('.//timeSegment')
  values = time.info |> map(~ xml_children(.) |> xml_text())
  names = time.info |> pluck(1) |> xml_children() |> xml_name()
  scan.segments = time.info |> map(~ parseScanSegment(.))
  source.params = time.info |> map(~ parseSourceParams(.))
  ion.funnel = time.info |> map(~ parseIonFunnel(.))

  time.table = map(values, ~ set_names(., names)) |> bind_rows()
  time.table$scanSegments = scan.segments
  time.table$sourceParameters = source.params
  time.table$IonFunnel = ion.funnel
  return(time.table)
}

# Parses chromatogram parameters used for plotting in acqusition
parseChromatograms = function(xml) {
  chromatogram.info = xml |> xml_find_all('.//chromatogram')
  names = chromatogram.info |> pluck(1) |> xml_children() |> xml_name()
  values = chromatogram.info |> map(~ xml_children(.) |> xml_text())
  chromatogram.table = map(values, ~ set_names(., names)) |> bind_rows()
  return(chromatogram.table)
}

#' @export
# Parses whole MS method using above parsers. Combine with map_dfr to build a
# table for multiple method files
parseMSMethod = function(xml.file) {
  ms.info = xml.file |> read_xml() |> xml_children()
  names = ms.info |> xml_name()
  values = ms.info |> xml_text(trim=T)
  method.table = tibble(!!!set_names(values, names))
  method.table$timeSegments = parseTimeSegment(ms.info)
  method.table$chromatograms = parseChromatograms(ms.info)
  return(method.table)
}

#' @export
#' Reduces peak list by maximum intensity ions as quantifier and second-most
#' intense as qualifier
#reducePeaklist = function(peaklist) {
#  # For each pseudospectrum group (psg)
#  psgs = 
#}