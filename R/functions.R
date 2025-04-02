#' @importFrom stringr str_c str_detect str_extract
#' @importFrom parallel makeCluster parSapply stopCluster detectCores
#' @importFrom xml2 read_xml xml_find_all xml_text xml_name xml_children xml_find_first
#' @importFrom dplyr bind_rows tibble if_else
#' @importFrom tibble add_column
#' @importFrom purrr pluck map set_names map_df
#' @importFrom Biobase featureData
#' @importFrom MSnbase readSRMData rtime intensity fromFile isEmpty
#' @importFrom xcms chromPeaks findChromPeaks CentWaveParam

#' @title
#' convertDtoMZML()
#' @description
#' Convert Agilent .d file to .mzML. Can only be MS1 scan data.
#' 
#' @param  file .d data file to convert
#' @param path.out Output directory for converted files. Defaults to 'path/_mzmL'
#' @returns NULL
#' @examples
#' convertDtoMZML(system.file('extdata', '_d/0001.d', package='LCMStools'))
#' @export
# TODO
# parameter for TOF vs MRM (SRM) data conversion
# rewrite to take a list of files instead of a path
convertDtoMZML = function(file, path.out = paste0(file,'/_mzML'), MRM=FALSE) {
#  exp.type = file |> dir(pattern='192_1.xml') |> read_xml()# |> xml_find_all()
#  file.paths = list.files(path=path, pattern='\\.d$', full.names=T, include.dirs=T)
#  print(paste('file.paths:', file.paths))
  cmd = stringr::str_c('msconvert ', file, 
    if(MRM==TRUE){
      ' --srmAsSpectra'
    }, ' --mzML --outdir ', path.out)
#  print(paste('cmd: ', cmd))
#  n.files = length(file.paths)
#  if (n.files > 1) {
#    cl = parallel::makeCluster(if (n.files < parallel::detectCores()) {
#      n.files
#    } else {
#      parallel::detectCores() - 1
#  })
#    out = parallel::parSapply(cl, cmd, function(x) system(x, intern=T, invisible=F))
#    print(paste('out: ',out))
#    parallel::stopCluster(cl)
#    print('cluster stopped')
#  } else {
#    system(cmd)
#  }
  system(cmd)
}

#' @title
#' calcPPM()
#' @description
#' Calculates the bounds around a mass for a given ppm threshold.
#' @param mz Target *m/z*
#' @param ppm Tolerance parameter
#' @returns numeric list
#' @examples
#' calcPPM(198.1000, 20)
#' @export
calcPPM = function(mz, ppm = 20) {
  mzr = mz + mz * c(-ppm, ppm) / 10^6
  return(mzr)
}

#' @title
#' parseSampleInfo()
#' @description
#' Parses a data file's sample_info.xml to a named vector
#' Combine with map_dfr to generate a data table for multiple data files
#' @param sample.info.xml Path of the sample_info.xml file in the data file directory
#' @returns named list
#' @examples
#' parseSampleInfo(
#'  system.file('extdata', '_d/0001.d/AcqData/sample_info.xml', package='LCMStools')
#' )
#' @export
parseSampleInfo = function(sample.info.xml) {
  sample.info = read_xml(sample.info.xml)
  xml.fields = sample.info |> xml_find_all('//Field')
  sample.info.values = xml.fields |> xml_find_all('//Value') |> xml_text(trim=T)
  sample.info.fields = xml.fields |> xml_find_all('//Name') |> xml_text()
  names(sample.info.values) = sample.info.fields
  return(sample.info.values)
}

#' @title
#' parseMSMethod()
#' @description
#' Parses whole MS method using above parsers. Combine with map_dfr to build a
#' table for multiple method files
#' 
#' @param xml.file The 192_1.xml MS method file in the data file's method folder
#' @returns Nested tibble of method parameters, including scan segments, source
#' parameters, and chromatograms
#' @examples
#' parseMSMethod(
#'  system.file('extdata', '_d/0001.d/AcqData/opt_dmrm.m/192_1.xml', package='LCMStools')
#' )
#' library(purrr)
#' system.file('extdata', '_d', package='LCMStools') |> 
#'  dir(pattern='192_1.xml', recursive=TRUE, full.names=TRUE) |> 
#'  map_df(parseMSMethod)
#' @export
parseMSMethod <- function(xml.file) {
  # Parses the MRM trigger info in 192_1.xml
  parseMRMTriggerInfo <- function(xml) {
    trigger.info <- xml |> xml_find_all(".//triggerMRMInfo")
    values <- trigger.info |> map(~ xml_children(.) |> xml_text())
    names <- trigger.info |>
      pluck(1) |>
      xml_children() |>
      xml_name()
    trigger.table <- map(values, ~ set_names(., names)) |> bind_rows()
    return(trigger.table)
  }

  # Parses scan element info in 192_1.xml containing multiple MRM trigger tables
  parseScanElement <- function(xml) {
    scan.info <- xml |> xml_find_all(".//scanElement")
    values <- scan.info |> map(~ xml_children(.) |> xml_text())
    names <- scan.info |>
      pluck(1) |>
      xml_children() |>
      xml_name()
    trigger.mrm.info <- scan.info |> map(~ {
      if (xml_find_all(., ".//triggerMRMInfo") |> length() > 0) {
        parseMRMTriggerInfo(.)
      } else {
        NA
      }
    })

    scan.table <- map(values, ~ set_names(., names)) |> bind_rows()
    scan.table$triggerMRMInfo <- trigger.mrm.info
    return(scan.table)
  }

  # Parses scan segment, which contains multiple scan elements
  parseScanSegment <- function(xml) {
    scan.segment <- xml |> xml_find_all(".//scanSegment")
    values <- scan.segment |> map(~ xml_children(.) |> xml_text())
    names <- scan.segment |>
      pluck(1) |>
      xml_children() |>
      xml_name()
    scan.info <- scan.segment |> map(parseScanElement)

    scan.segment.table <- map(values, ~ set_names(., names)) |> bind_rows()
    scan.segment.table$scanElements <- scan.info
    return(scan.segment.table)
  }

  # Parses the source parameters from 192_1.xml
  parseSourceParams <- function(xml) {
    source.params <- xml |> xml_find_all(".//sourceParameter")
    params <- source.params |> map_df(~ {
      id <- xml_find_first(., "id") |> xml_text()
      pos <- xml_find_first(., "posPolarityValue") |>
        xml_text() |>
        as.numeric()
      neg <- xml_find_first(., "negPolarityValue") |>
        xml_text() |>
        as.numeric()
      tibble(
        id = c(id, id),
        polarity = c("positive", "negative"),
        value = c(pos, neg)
      )
    })
    return(params)
  }

  # Parses ion funnel parameters: positive/negative, high rf/low rf, voltage
  parseIonFunnel <- function(xml) {
    ion.funnel.info <- xml |>
      xml_find_all(".//IonFunnel") |>
      xml_children()
    ion.funnel.table <- ion.funnel.info |> map_df(~ {
      name <- xml_name(.)
      value <- xml_text(.) |> as.numeric()
      tibble(
        polarity = if_else(str_detect(name, "^Pos"), "positive", "negative"),
        pressure = str_extract(name, "HP|LP"),
        voltage = value
      )
    })
    return(ion.funnel.table)
  }

  # Parses time segment containing startTime, diverterValveState, sourceParameters,
  # isDataSaved, scanSegments, and IonFunnel parameters
  parseTimeSegment <- function(xml) {
    time.info <- xml |> xml_find_all(".//timeSegment")
    values <- time.info |> map(~ xml_children(.) |> xml_text())
    names <- time.info |>
      pluck(1) |>
      xml_children() |>
      xml_name()
    scan.segments <- time.info |> map(~ parseScanSegment(.))
    source.params <- time.info |> map(~ parseSourceParams(.))
    ion.funnel <- time.info |> map(~ parseIonFunnel(.))

    time.table <- map(values, ~ set_names(., names)) |> bind_rows()
    time.table$scanSegments <- scan.segments
    time.table$sourceParameters <- source.params
    time.table$IonFunnel <- ion.funnel
    return(time.table)
  }

  # Parses chromatogram parameters used for plotting in acqusition
  parseChromatograms <- function(xml) {
    chromatogram.info <- xml |> xml_find_all(".//chromatogram")
    names <- chromatogram.info |>
      pluck(1) |>
      xml_children() |>
      xml_name()
    values <- chromatogram.info |> map(~ xml_children(.) |> xml_text())
    chromatogram.table <- map(values, ~ set_names(., names)) |> bind_rows()
    return(chromatogram.table)
  }

  ms.info <- xml.file |>
    read_xml() |>
    xml_children()
  names <- ms.info |> xml_name()
  values <- ms.info |> xml_text(trim = T)
  method.table <- tibble(!!!set_names(values, names))

  sample.info = parseSampleInfo(
    file.path(dirname(dirname(xml.file)), 'sample_info.xml')
  )
  method.table = add_column(
    method.table,
    sampleName = sample.info |> pluck('Sample Name'),
    dataFile = sample.info |> pluck('Data File'),
    .after='msInstrument'
  )
#  method.table = add_column(method.table, dataFile = sample.info |> pluck('Data File'), .after='sampleName')

  method.table$timeSegments <- list(parseTimeSegment(ms.info))
  method.table$chromatograms <- list(parseChromatograms(ms.info))
  
  return(method.table)
}

#' @title
#' Get chromatograms for each collision energy in experiment
#' @description
#' Returns a data frame of retention times and intensities (chromatograms) along
#' with the associated collision energy of the MS experiment
#' 
#' @param chromatograms A list of chromatogram objects
#' @returns A long data frame of retention times, intensities, collision
#' energies, and data file
#' @examples
#' library(MSnbase)
#' ex.file = system.file('extdata', '_d/0176.d/0176.mzML', package='LCMStools')
#' ex.data = readSRMData(ex.file)
#' getCEChroms(ex.data)
#' @export
getCEChroms <- function(chromatograms) {
  ce.data = data.frame()
  for(i in seq_along(chromatograms)){
    ce.data = bind_rows(ce.data, data.frame(
      rtime = rtime(chromatograms[i]), 
      intensity = intensity(chromatograms[i]),
      precursor = featureData(chromatograms)$precursorIsolationWindowTargetMZ[i],
      product = featureData(chromatograms)$productIsolationWindowTargetMZ[i],
      ce = featureData(chromatograms)$precursorCollisionEnergy[i],
      file = fromFile(chromatograms[i])))
  }
  return(ce.data)
}

#' @title
#' Get peak intensities for each collision energy in an experiment
#' @description
#' Returns a data frame of collision energies, peak intensities, and data files.
#' Assumes that one peak is detected per file.
#' 
#' @param peaks An XChromatogram object that contains detected peaks
#' @returns A data frame of collision energy, peak intensities, and data files.
#' @examples
#' library(xcms)
#' ex.file = system.file('extdata', '_d/0176.d/0176.mzML', package='LCMStools')
#' ex.data = readSRMData(ex.file)
#' ex.peaks = findChromPeaks(ex.data, CentWaveParam(peakwidth=c(0.2,0.25)))
#' getCEData(ex.peaks)
#' @export
getCEData <- function(peaks, metric = 'into') {
  ce.data = data.frame()
  for(i in seq_len(nrow(peaks))){
    ce.data = ce.data |> bind_rows(
      data.frame(
        precursor = featureData(peaks)$precursorIsolationWindowTargetMZ[i],
        product = featureData(peaks)$productIsolationWindowTargetMZ[i],
        ce = featureData(peaks)$precursorCollisionEnergy[i],
        file = pData(peaks)$file[i],
        into = if(!isEmpty(chromPeaks(peaks[i]))){
          chromPeaks(peaks[i])[metric]
        } else {
          0
        }
      )
    )
  }
  return(ce.data)
}

#'
#' Reduces peak list by maximum intensity ions as quantifier and second-most
#' intense as qualifier
#reducePeaklist = function(peaklist) {
#  # For each pseudospectrum group (psg)
#  psgs = 
#}