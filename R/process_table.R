# .compile function calls vdat utility to open each vrl, convert each file to an interleaved .csv files containing all the data stored on the receiver. Converts vrl to csv using Innovasea vdat command line utility.

.compile <- function(vdata_file, vdat_pth){
    
    ## Convert function arguments
    vdat_args <- c(
      "convert",
      "--format=csv.fathom",
      vdata_file,
     # paste0("--output=", out_dir),
      "--timec=default"
    )
    
  #time_shell_out <- format(Sys.time(), "[%s]")
  shell_out <- sys::exec_internal(cmd = vdat_pth, args = vdat_args, error = FALSE)  
  return(vdata_file)
}


# Internal function for extracting tables from vrl files

 read_dta_lst <- function(src, record_types = c("DET", "DATA_SOURCE_FILE", "CFG_CHANNEL", "CLOCK_REF", "EVENT_INIT", "EVENT_OFFLOAD", "CFG_TRANSMITTER")
                           ){
    vdat_txt <- data.table::fread(file = src, skip = 2, header = FALSE, sep = NULL, col.names = "txt")

    vdat_txt[, `:=`(record_type, data.table::fread(file = src, skip = 2, header = FALSE, sep = ",", select = 1, fill = TRUE))]
    vdat_txt[, `:=`(record_type, gsub("_DESC$", "", record_type))]
    vdat_txt <- vdat_txt[record_type %in% record_types,]
    vdat_list <- split(vdat_txt, by = "record_type", keep.by = FALSE)
    vdat_list <- lapply(vdat_list, function(x) {fread(text = paste0(c(x$txt,""), collapse = "\n"), sep = ",", na.strings = "", colClasses = "character", header = TRUE, drop = 1)})

    return(vdat_list)
  }


##' Compiles output data from all receiver log files, creates table used in output report
##' 
##' @param fls data.frame of produced from shiny::fileInput that contains name, size, type, datapath for each receiver selected by user
##' @param action User selected radio button for "download" and "initialize" included in output table
##' @param vdat_pth Path to vdat executable installed on your computer
##'
##' @returns returns table of diagnostic  metrics extracted from each receiver

##' @examples
##' \dontrun{
##' # datapath below  must direct to actual files vrl/vdat files.  When data are uploaded by shiny, original file name is changed internally (i.e., 0.vrl, 1.vrl) and renamed file is in "name" column.
# fls <- data.frame(name = c("0.vrl", "1.vrl"), size = c(10,10), type = c(NA, NA), datapath = c("C:\\Users\\Admin\\AppData\\Local\\Temp\\RtmpoBzcDm/d84e0d8bdfc118382467b5ad/0.vrl", "C:\\Users\\Admin\\AppData\\Local\\Temp\\RtmpoBzcDm/d84e0d8bdfc118382467b5ad/1.vrl"))
##' # process_table(fls = fls, action = "download", vdat_pth = glatosQAQC::check_vdat())
##' }
##'
##' @export
process_table <- function(fls, action, vdat_pth = glatosQAQC::check_vdat()){
  
 # extract vdat version used to extract data
  # this is displayed in output
  shell_out <- sys::exec_internal(cmd = vdat_pth, args = "--version")
  vdat_ver <- rawToChar(shell_out$stdout)
  vdat_ver <- unlist(strsplit(vdat_ver, "\r?\n"))
  
  # create key
  setDT(fls)  
  fls[, hash := digest::digest(datapath, serialize = FALSE), by = 1:nrow(fls)]
  fls[, pathname := basename(datapath)]
  fls[, vdat_version := vdat_ver]

  #fwrite(fls, "~/Desktop/check.csv")
  # fls <- fread("~/Desktop/check.csv")
  #vdat_pth = glatosQAQC::check_vdat()
  
  dta <- lapply(as.list(fls$datapath), .compile, vdat_pth = vdat_pth )
  foo <- lapply(dta, function(x){(fls[fls$datapath %in% x, ]$hash)})
  names(dta) <- unlist(foo)

 # browser()
  dta <- lapply(dta, function(x) {gsub(pattern = "\\.(vrl|vdat)$", x = x, replacement = ".csv")})
  
  dta <- lapply(dta, read_dta_lst)

#dta <- lapply(dta, glatos::read_vdat_csv)
  
  for(i in 1:length(dta)){
    dta[[i]]$DET <- as.det(dta[[i]]$DET)
    dta[[i]]$CFG_CHANNEL <- as.cfg_channel(dta[[i]]$CFG_CHANNEL)
    dta[[i]]$CLOCK_REF <- as.clock_ref(dta[[i]]$CLOCK_REF)
    dta[[i]]$EVENT_INIT <- as.event_init(dta[[i]]$EVENT_INIT)
    dta[[i]]$EVENT_OFFLOAD <- as.event_offload(dta[[i]]$EVENT_OFFLOAD)
    dta[[i]]$CFG_TRANSMITTER <- as.cfg_transmitter(dta[[i]]$CFG_TRANSMITTER)
    dta[[i]]$DATA_SOURCE_FILE <- as.data_source_file(dta[[i]]$DATA_SOURCE_FILE)
    }

#  saveRDS(dta, "~/Desktop/check.rds")
  #dta <- readRDS("~/Desktop/check.rds")
 
  # Process detection records to summarize first and last detections, and tags associated with detections
  dtc <- glatosQAQC::process_detections(rbindlist(lapply(dta, "[[", "DET"), idcol = "file"))
  
  # pull out receiver map data
  rec_map <- rbindlist(lapply(dta, "[[", "CFG_CHANNEL"), idcol = "file")

##   # this extracts receiver init/download times and computer time at download
  ##   # creates a wide table and renames columns
  clock_ref <- rbindlist(lapply(dta, "[[", "CLOCK_REF"), idcol = "file")
  clock_ref <- data.table::dcast(clock_ref, file ~ Source, value.var = c("Time", "External Time (UTC)"))
  setnames(clock_ref, c('Time_INITIALIZATION', 'Time_OFFLOAD', 'External Time (UTC)_INITIALIZATION', 'External Time (UTC)_OFFLOAD'), c("rec init", "rec download", "comp init", "comp download"))

 ## # extract firmware version, memory remaining%, total detections, model, firmware = event_init
  event_init <- rbindlist(lapply(dta, "[[", "EVENT_INIT"), idcol = "file")

  # event offload
  event_offload <- rbindlist(lapply(dta, "[[", "EVENT_OFFLOAD"), idcol = "file")
  ##   # round memory available column to 1 digit
  event_offload[, `mem avail` := round(event_offload$`Memory Remaining (%)`, 1)]

  # i_tag stuff...
  i_tag <- rbindlist(lapply(dta, "[[", "CFG_TRANSMITTER"), idcol = "file")


  # combine detection records and file records for everything but the "i_tag" data
  out <- dtc[fls, on = .(file = hash)][rec_map, on = .(file)][clock_ref, on = .(file)][event_init, on = .(file)][event_offload, on = .(file)]


##   # works to here.  Need to figure out how to summarize i_tag data.
##   # records for integrated tag are not restricted to initialization and download with the exception of all i_tag changes must occur after initialization and before the next future initialization.
##   # for the QAQC, only look at status between init and download and report the setting with the longest duration during this time period.
##   # for HBBS protocol, this duration is the only event possible (i.e., should be no partial downloads
##   # changes in i_tag power after download but before the next initialization are not considered.
  
## ###################################

  
  if(nrow(i_tag) > 0){
    i_tag <- i_tag[!is.na(file),]
    setkey(i_tag, file, Time)    
    i_tag[, end := data.table::shift(Time, fill = NA, type = "lead"), by = "file"] 

    #write.fst(i_tag, "~/Desktop/i_tag.fst")
    #write.fst(out, "~/Desktop/out.fst")
    #i_tag <- read.fst("~/Desktop/i_tag.fst", as.data.table = TRUE)
    #out <- read.fst("~/Desktop/out.fst", as.data.table = TRUE)

    # fill in missing i_tag events with download timestamp.  This occurs when i_tag is not adjusted prior to download. 
    i_tag[is.na(end), end := out[.SD, on = .(file), `comp download`]]
    
#    browser()
    event <- out[!is.na(`comp init`), c("file", "comp init", "comp download")] 
    i_tag  <- event[i_tag, .(file,
                           end,
                           Time,
                           `Power Level`,
                           `Min Delay (s)`,
                           `Max Delay (s)`,
                           `Full ID`
                           ),
                    on = .(file = file,
                         `comp init` <= Time,
                         `comp download` >= end)
                  ]

    i_tag <- i_tag[, duration := difftime(end, Time, units = "secs")]
    i_tag <- i_tag[i_tag[, .I[duration == max(duration)], by = "file"]$V1, c("file", "Time", "Power Level", "Full ID", "duration", "Min Delay (s)", "Max Delay (s)")]
    
    i_tag[, `Delay (s)` := paste(`Min Delay (s)`, `Max Delay (s)`, sep = "-")]
    i_tag[`Power Level` == "DISABLED", `Delay (s)` := NA]

  } else {
    i_tag <- data.table(file = NA_character_,
                        Time = as.POSIXct(NA, tz = "UTC"),
                        `Power Level` = NA_character_,
                        `Delay` =  NA_character_,
                        duration = NA_character_
                        )
  }


  out <- i_tag[out, on = .(file)] 

   #write.fst(i_tag, "~/Desktop/i_tag.fst")
    #fst::write.fst(out, "~/Desktop/out.fst")
 # fwrite(out, "~/Desktop/check.csv")
 
##   # fix names and formatting
  data.table::setnames(out, c("Time_first",
                              "Time_last",
                              "Full ID_first",
                              "Full ID_last",
                              "Time",
                              "Full ID",
                              "Power Level",
                              "Map ID",
                              "vdat_version",
                              "Serial Number",
                              "Model",
                              "Firmware Version",
                              "PPM Total Accepted Detections",
                              "Battery Remaining (%)",
                              "Memory Remaining (%)",
                              "Original File",
                              "Delay (s)"
                              ),
                       c("first det",
                         "last det",
                         "first tag",
                         "last tag",
                         "int tag init",
                         "int tag ID",
                         "int tag power",
                         "rec map",
                         "vdat_ver",
                         "rec num",
                         "rec mod",
                         "rec firmware",
                         "num det",
                         "battery (%)",
                         "mem avail",
                         "name",
                         "int delay rng (s)"
                         ), skip_absent = TRUE
                       )
  
  ## # prepare data for export
  out <- out[, c("name",
                 "rec num",
                 "rec mod",
                 "rec firmware",
                 "rec map",
                 "mem avail",
                 "battery (%)",              
                 "rec init", 
                 "rec download", 
                 "comp download",
                 "first det",
                 "last det",
                 "num det",
                 "int tag ID",
                 "int tag power",
                 "int delay rng (s)",
                 "vdat_ver"
                 )
             ]   
  
  ## # enforce data output types
  ## out[, `:=` (`file` = as.character(`file`),
  ##             `OG` = as.character(`OG`),
  ##             `rec num` = as.integer(`rec num`),
  ##             `rec mod` = as.character(`rec mod`),
  ##             `rec firmware` = as.character(`rec firmware`),
  ##             `rec map` = as.character(`rec map`),
  ##             `mem avail` = as.integer(`mem avail`),
  ##             `rec init` = as.POSIXct(`rec init`, tz = "UTC"),
  ##             `rec download` = as.POSIXct(`rec download`, tz = "UTC"),
  ##             `comp download` = as.POSIXct(`comp download`, tz = "UTC"),
  ##             `first det` = as.POSIXct(`first det`, tz = "UTC"),
  ##             `last det` = as.POSIXct(`last det`, tz = "UTC"),
  ##             `num det` = as.integer(`num det`),
  ##             `int tag init` = as.POSIXct(`int tag init`, tz = "UTC"),
  ##             `int tag ID` = as.character(`int tag ID`),
  ##             `int tag power` = as.character(`int tag power`),
  ##             `int tag min delay` = as.integer(`int tag min delay`),
  ##             `int tag max delay` = as.integer(`int tag max delay`),
  ##             `vdat_ver` = as.character(`vdat_ver`),
  ##             `battery (%)` = as.integer(`battery (%)`),
  ##             `OGfile` = as.character(`OGfile`)
  ##             )
  ##     ]

  

  # add in action type
  if (action == "down"){
    out[, action := "download"]}
  if (action == "init"){
      out[, action := "initialize"]}

  return(out)
}


