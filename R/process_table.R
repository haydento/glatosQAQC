##' Compiles data from all receivers, generates table of data included in report.
##' 
##' @param fls a character vector of files to include in report
##' @param params rmarkdown parameters needed to process data
##' @param work_dir working directory where processing takes place


# if don't want to interactively choose files
#' fls <- c("~/Documents/VRL tests/VR2AR_546310_20190607_1.vrl", "~/Documents/VRL tests/VR2AR_546310_20220624_1.vrl", "~/Documents/VRL tests/VR2W_134214_20230626_1.vrl")
#' mrk_params = "C:\\PROGRA~1\\vdat\\vdat.exe"
#' work_dir = "~/Documents/"
#' nme <- c("VR2AR_546310_20190607_1.vrl", "VR2AR_546310_20220624_1.vrl", "VR2W_134214_20230626_1.vrl")
#' action <- "down"
#' datapath = file.path(tempdir(), c("0.vrl", "1.vrl"))

#' dtc <- glatosQAQC::compile_vdats(vdat_files = fls, v_path = mrk_params)
#'
#' 
#' foo <-  process_table(fls = fls, mrk_params = mrk_params, nme = nme, action = action, datapath = datapath, work_dir = tempdir())
# https://stackoverflow.com/questions/52385295/get-the-name-of-an-uploaded-file-in-shiny

#' @export

process_table <- function(fls, mrk_params, nme, action, work_dir = work_dir, datapath = datapath){

  # convert vrls to csv format
  dtc <- glatosQAQC::compile_vdats(vdat_files = fls, v_path = mrk_params, temp_dir = work_dir)
  
  # extract vdat version used to extract data
  y <- function(x){attributes(x)$vdat_version}
  vdat_ver <- lapply(dtc, y)
  f_name <- sub(".csv", ".vrl", names(vdat_ver))
  vdat_ver <- data.table(file = f_name, vdat = vdat_ver)

  # extract detection records for each file
  det <- glatosQAQC::extract_records(vdat = dtc, type = "DET")

  # validate and assign vdat_dtc class
  det <- glatosQAQC::vdat_dtc(det)

  # Process detection records to summarize first and last detections, and tags associated with detections
  det <- glatosQAQC::process_detections(det)
  
  # extract file record from "DATA_SOURCE_FILE"
  file_id <- glatosQAQC::extract_records(vdat = dtc, type = "DATA_SOURCE_FILE")

  # these next three lines are a work-around that re-establishes
  # a link between original file name and internal file name that is automatically assigned when the files are uploaded to shiny server.
  # this is a bug.  Not sure why or how file names within file are changed to automatically assigned shiny names
  # behaviour is obvious when step through these lines starting browser from here...
  #browser()
  
  nme_mod <- as.data.table(nme)
  nme_mod[, int_name := basename(datapath)]
  file_id[nme_mod, `File Name` := nme_mod$name, on = .(file = int_name)]
  
  # combine detection records and file records
  out <- merge(det, file_id, by = "file", all.x = TRUE)

  # combine detection records and vdat version info
  out <- merge(out, vdat_ver, by = "file", all.x = TRUE)

  ## # extract receiver battery info
#   bat <- glatosQAQC::extract_records(vdat = dtc, type = "BATTERY")
  
  ## # validate and assign vdat_bat class
 #  bat <- glatosQAQC::vdat_bat(bat)

  
  ## # Process battery records to summarize first and last voltages
  # bat <- glatosQAQC::process_detections_battery(bat)

    
  ## # combine
   #out <- merge(out, bat, by = "file", all.x = TRUE)

  # extract receiver map info for the receiver (in CFG_CHANNEL) 
  rec_map <- glatosQAQC::extract_records(vdat = dtc, type = "CFG_CHANNEL")

  # extract only needed map info (rec map)
  rec_map <- rec_map[, c("file", "Map ID")]
  #data.table::setnames(rec_map, "Map ID", "rec map")

  # combine
  out <- merge(out, rec_map, by = "file", all.x = TRUE)

  # extract clock info (offload time, initilization time) from clock ref table
  new_event_offload <- glatosQAQC::extract_records(vdat = dtc, type = "CLOCK_REF")
  new_rec_init_down <- data.table::dcast(new_event_offload, file ~ Source, value.var = c("Device Time (UTC)"))

  # nothing is written to vdat file if initialization or offload is messed up.  This next code block makes sure that table contains both "offload" and "initialization" columns.
  req_cols <- c("file", "OFFLOAD", "INITIALIZATION")
  col_difs <- setdiff(req_cols, names(new_rec_init_down))
  if(length(col_difs) > 0){
    # problem here...should not be integer type...
    new_rec_init_down[, (col_difs) := as.POSIXct(NA, tz = "UTC")]
  }

  new_comp_time <- data.table::dcast(new_event_offload[Source == "OFFLOAD",], file ~ Source, value.var = c("External Time (UTC)"))
  data.table::setnames(new_comp_time, "OFFLOAD", "comp download")
  event <- merge(new_rec_init_down, new_comp_time, by = "file", all = TRUE)

# combine
  out <- merge(out, event, by = "file", all.x = TRUE)
  
  
# extract firmware version, memory remaining%, total detections, model
  
  # firmware = event_init
  #browser()
  new_stats <- glatosQAQC::extract_records(vdat = dtc, type = "EVENT_OFFLOAD")
  new_stats <- new_stats[, c("file", "Model", "PPM Total Accepted Detections", "Memory Remaining (%)", "Battery Remaining (%)")]
  data.table::setnames(new_stats, c("Model", "PPM Total Accepted Detections", "Memory Remaining (%)", "Battery Remaining (%)"), c("rec mod", "num det", "mem avail", "battery (%)"))

  # extract event offload for the receivers
  #event_offload <- glatosQAQC::extract_records(vdat = dtc, type = "EVENT_OFFLOAD")

  # extract event info needed for report (rec num, computer download time, mem avail, num det, receiver download time)
  #event_offload <- event_offload[, c("file", "Serial Number", "External Time (UTC)",
   #                                  "Memory Remaining (%)",
    #                                 "PPM Total Accepted Detections", "Device Time (UTC)")]

  # extract firmware info.  Had to customize code because of missing column data in some later receivers

  firmware <- lapply(dtc, "[[", "EVENT_INIT")
  firmware <-  lapply(firmware, function(x){x[, c("Firmware Version", "Serial Number", "Time")]})
  firmware <- data.table::rbindlist(firmware, idcol = "file")
  firmware[, file := gsub(".csv", ".vrl", file, fixed = TRUE)]
  alt_init <- firmware[,c("file", "Time")]
  setnames(alt_init, "Time", "alt_init")
  firmware[, Time := NULL]
  data.table::setnames(firmware, c("Firmware Version", "Serial Number"), c("rec firmware", "rec num"))
  new_stats <- merge(new_stats, firmware, by = "file", all = TRUE)

  # combine
  out <- merge(out, new_stats, by = "file", all.x = TRUE)
  
  # extract integrated tag info
  i_tag <- glatosQAQC::extract_records(vdat = dtc, type = "CFG_TRANSMITTER")
  i_tag <- glatosQAQC::vdat_i_tag(i_tag)
  i_tag <- i_tag[!is.na(Time),]


  # create null table if no recs have integrated tags
  null_i_tag <- data.table( file = NA_character_, Time = as.POSIXct(NA, tz = "UTC"), `Power Level` = NA_character_, `Min Delay (s)` = NA_integer_, `Max Delay (s)` = NA_integer_, `Full ID` = NA_character_)

  
  if(nrow(i_tag) > 0){
    
    setkey(i_tag, Time, file)
    i_tag$start <- i_tag$Time  
    i_tag[, end := data.table::shift(Time, fill = NA, type = "lead"), by = "file"]
    i_tag[, n_row := .N, by = "file"]
    i_tag[n_row == 1, end := Sys.time()]
    i_tag <- i_tag[ !is.na(end),]
    
    # simplify events for dev (no NA init)
    #event <- event[1,]
    #  ark <- event
    #  ark_i_tag <- i_tag
    #  ark_event <- event
  
    event <- alt_init[event, on = "file",]
    event[,`:=`(start = `INITIALIZATION`, end = `comp download`)]
    event[is.na(start), start := alt_init]
    
    # event <- event[!is.na(start),]
    setkey(event, file, start, end)
    setkey(i_tag, file, start, end)
    tst <- foverlaps(event, i_tag)
    
    tst[, duration := as.numeric(lubridate::as.duration(lubridate::intersect(lubridate::interval(start, end), lubridate::interval(i.start, i.end))))]
    
    # extract receivers that don't have integrated tag or NA in duration column
    tst <- tst[!is.na(duration),]

    # find maximum for each
    tst <- tst[tst[!is.na(duration), .I[duration == max(duration)], by = "file"]$V1, c("file", "Time", "Power Level", "Min Delay (s)", "Max Delay (s)", "Full ID")]
    
    if(nrow(tst) == 0){
      tst <- null_i_tag
    }
    
  } else {

    tst <- null_i_tag
  }
  
  # combine objects
  out <- merge(out, tst, by = "file", all.x = TRUE)

  #browser()
  # fix names and formatting
  data.table::setnames(out, c("Time_first",
                              "Time_last",
                              "Full ID_first",
                              "Full ID_last",
                              "Time",
                              "Full ID",
                              "Power Level",
                              "Min Delay (s)",
                              "Max Delay (s)",
                              "Map ID",
                              "INITIALIZATION",
                              "OFFLOAD",
                              "vdat"),
                       c("first det",
                         "last det",
                         "first tag",
                         "last tag",
                         "int tag init",
                         "int tag ID",
                         "int tag power",
                         "int tag min delay",
                         "int tag max delay",
                         "rec map",
                         "rec init",
                         "rec download",
                         "vdat_ver"
                         ))

  # round memory available column to 1 digit
  out[, "mem avail" := round(out$'mem avail', 1)]


  # prepare data for export
  out <- out[, c("File Name",
                 "rec num",
                 "rec mod",
                 "rec firmware",
                 "rec map",
                 "mem avail", 
                 "rec init", 
                 "rec download", 
                 "comp download",
                 "first det",
                 "last det",
                 "num det",
                 "int tag init",
                 "int tag ID",
                 "int tag power",
                 "int tag min delay",
                 "int tag max delay",
                 "vdat_ver",
                 "battery (%)"
                 )
             ]   

  # enforce data output types
  out[, `:=` (`rec num` = as.numeric(`rec num`),
              `rec mod` = as.character(`rec mod`),
              `rec firmware` = as.character(`rec firmware`),
              `rec map` = as.character(`rec map`),
              `mem avail` = as.numeric(`mem avail`),
              `rec init` = as.POSIXct(`rec init`, tz = "UTC"),
              `rec download` = as.POSIXct(`rec download`, tz = "UTC"),
              `comp download` = as.POSIXct(`comp download`, tz = "UTC"),
              `first det` = as.POSIXct(`first det`, tz = "UTC"),
              `last det` = as.POSIXct(`last det`, tz = "UTC"),
              `num det` = as.numeric(`num det`),
              `int tag init` = as.POSIXct(`int tag init`, tz = "UTC"),
              `int tag ID` = as.character(`int tag ID`),
              `int tag power` = as.character(`int tag power`),
              `int tag min delay` = as.character(`int tag min delay`),
              `int tag max delay` = as.character(`int tag max delay`),
              `vdat_ver` = as.character(`vdat_ver`),
              `battery (%)` = as.numeric(`battery (%)`)
              )
              ]

  # add in file name info to output table
  # order of records are not same.  THis causes a mismatch of rows with receivers.

  ## tst <- data.frame(name = c("VR2AR_546908_20190610_1.vrl", "VR2AR_546310_20190607_1.vrl", "VR2AR_547547_20210528_1.vrl"), size = c(772460, 1486380, 12314), type = c("","",""), datapath = c("C:\\Users\\thayden\\AppData\\Local\\Temp\\2\\RtmpawIRf0/5ac847036f9463dc36e6b01c/0.vrl", "C:\\Users\\thayden\\AppData\\Local\\Temp\\2\\RtmpawIRf0/5ac847036f9463dc36e6b01c/1.vrl", "C:\\Users\\thayden\\AppData\\Local\\Temp\\2\\RtmpawIRf0/5ac847036f9463dc36e6b01c/2.vrl"))

  ## browser()
  
  ## # match(out[["tst_file"]], tst$name)
  ## nme <- nme$name[match(basename(nme$datapath), out[["tst_file"]])]

  ## nme$id <- basename(nme$datapath)


  
  ## out[[tst_file

  ## print(nme)
                                                                                                    
  ## out[, file := as.character(nme)]
  
  # add in action type
  if (action == "down"){
    out[, action := "download"]}
  if (action == "init"){
      out[, action := "initialize"]}

  return(out)
}


