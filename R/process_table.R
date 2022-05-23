##' Compiles data from all receivers, generates table of data included in report.
##' 
##' @param fls a character vector of files to include in report
##' @param params rmarkdown parameters needed to process data
##' @param work_dir working directory where processing takes place
##' @export

# if don't want to interactively choose files (for dev)
  ## fls <- c("C:/Users/thayden/Documents/VR2AR_546310_20190607_1.vrl", "C:/Users/thayden/Documents/VR2AR_546908_20190610_1.vrl", "C:/Users/thayden/Documents/VR2AR_547547_20210528_1.vrl")

## fls <- c("C:/Users/thayden/Documents/VR2AR_547547_20210528_1.vrl", "C:/Users/thayden/Documents/VR2AR_546310_20190607_1.vrl", "C:/Users/thayden/Documents/VR2AR_547608_20190820_1.vrl")

## fls <- c("C:/Users/thayden/Documents/VR2AR_546310_20190607_1.vrl", "C:/Users/thayden/Documents/VR2AR_546908_20190610_1.vrl", "C:/Users/thayden/Documents/VR2W_109420_20190619_1.vrl", "C:/Users/thayden/Documents/VR2W_109412_20190619_1.vrl")

##  mrk_params = "C:/Program Files/Innovasea/Fathom/vdat.exe"
## work_dir = "C:/Users/thayden/Desktop"
## nme <- "VR2AR_547537_20210528_1.vrl" 

## ## ## #  dtc <- glatosQAQC::compile_vdats(vdat_files = fls, v_path = pth, temp_dir = "C:/Users/thayden/Desktop")
## #foo <-  process_table(fls = fls, mrk_params = mrk_params, work_dir = "C:/Users/thayden/Desktop")
# https://stackoverflow.com/questions/52385295/get-the-name-of-an-uploaded-file-in-shiny

process_table <- function(fls, mrk_params, nme, work_dir = work_dir){
  
  # convert vrls to csv format
  dtc <- glatosQAQC::compile_vdats(vdat_files = fls, v_path = mrk_params, temp_dir = work_dir)

  # extract detection records for each file
  det <- glatosQAQC::extract_records(vdat = dtc, type = "DET")

  # Process detection records to summarize first and last detections, and tags associated with detections
  det <- glatosQAQC::process_detections(det, battery = FALSE)

  # extract file record from "DATA_SOURCE_FILE"
  file_id <- glatosQAQC::extract_records(vdat = dtc, type = "DATA_SOURCE_FILE")
  file_id <- file_id[, c("file", "File Name")]
  file_id[, `File Name` := shQuote(`File Name`)]

  # combine detection records and file records
  out <- merge(det, file_id, by = "file", all.x = TRUE)
  
  # extract receiver battery info
  bat <- glatosQAQC::extract_records(vdat = dtc, type = "BATTERY")

  # Process battery records to summarize first and last voltages
  bat <- glatosQAQC::process_detections(bat, battery = TRUE)
  bat <- data.table::dcast(bat, file ~ `Battery Position`, value.var = "Battery Voltage (V)_last")

    req_cols <- c("file", "PRIMARY", "MOTOR")
  col_difs <- setdiff(req_cols, names(bat))
  if(length(col_difs) > 0){
    bat[, (col_difs) := NA_integer_]
  }
  
  # combine
  out <- merge(out, bat, by = "file", all.x = TRUE)

  # extract map info for the receiver (in CFG_CHANNEL) 
  rec_map <- glatosQAQC::extract_records(vdat = dtc, type = "CFG_CHANNEL")

  # extract only needed map info (rec map)
  rec_map <- rec_map[, c("file", "Map ID")]
  #data.table::setnames(rec_map, "Map ID", "rec map")

  # combine
  out <- merge(out, rec_map, by = "file", all.x = TRUE)

  ###### clock ref table
  new_event_offload <- glatosQAQC::extract_records(vdat = dtc, type = "CLOCK_REF")

  new_rec_init_down <- data.table::dcast(new_event_offload, file ~ Source, value.var = c("Device Time (UTC)"))

  # nothing is written to vdat file if initialization or offload is messed up.  This makes sure that table contains both "offload" and "initialization" columns.
  req_cols <- c("file", "OFFLOAD", "INITIALIZATION")
  col_difs <- setdiff(req_cols, names(new_rec_init_down))
  if(length(col_difs) > 0){
    new_rec_init_down[, (col_difs) := NA_integer_]
  }

  data.table::setnames(new_rec_init_down, c("INITIALIZATION", "OFFLOAD"), c("rec init", "rec download"))

  new_comp_time <- data.table::dcast(new_event_offload[Source == "OFFLOAD",], file ~ Source, value.var = c("External Time (UTC)"))
  data.table::setnames(new_comp_time, "OFFLOAD", "comp download")
  event <- merge(new_rec_init_down, new_comp_time, by = "file", all = TRUE)

# combine
  out <- merge(out, event, by = "file", all.x = TRUE)
  
  
# need firmware version, memory remaining%, total detections, model
  
  # firmware = event_init

  new_stats <- glatosQAQC::extract_records(vdat = dtc, type = "EVENT_OFFLOAD")
  new_stats <- new_stats[, c("file", "Model", "PPM Total Accepted Detections", "Memory Remaining (%)")]
data.table::setnames(new_stats, c("Model", "PPM Total Accepted Detections", "Memory Remaining (%)"), c("rec mod", "num det", "mem avail"))

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
  setkey(i_tag, Time, file)

  i_tag[, start := Time]
  i_tag[, end := data.table::shift(Time, fill = NA, type = "lead"), by = "file"]
  i_tag <- i_tag[ !is.na(end),]
  

  # simplify events for dev (no NA init)
  #event <- event[1,]
#  ark <- event

  event <- alt_init[event, on = "file",]
  event[,`:=`(start = `rec init`, end = `comp download`)]
  event[is.na(start), start := alt_init]
    
#######3#######
  
  # event <- event[!is.na(start),]
  setkey(event, file, start, end)
  setkey(i_tag, file, start, end)
  tst <- foverlaps(event, i_tag)
  tst[, duration := as.numeric(lubridate::as.duration(lubridate::intersect(lubridate::interval(start, end), lubridate::interval(i.start, i.end))))]
  
  # find maximum for each 
  tst <- tst[tst[, .I[duration == max(duration)], by = "file"]$V1, c("file", "Time", "Power Level", "Min Delay (s)", "Max Delay (s)", "Full ID")]

#  i_tag[`Power Level` == "DISABLED", `:=` (`Min Delay (s)` = NA, `Max Delay (s)` = NA)]
  
  # combine objects
  out <- merge(out, tst, by = "file", all.x = TRUE)
  
  # fix names and formatting
  data.table::setnames(out, c("Time_first",
                              "Time_last",
                              "Full ID_first",
                              "Full ID_last",
                              "PRIMARY",
                              "MOTOR",
                              "Time",
                              "Full ID",
                              "Power Level",
                              "Min Delay (s)",
                              "Max Delay (s)",
                              "Map ID"
                              ),
                       c("first det",
                         "last det",
                         "first tag",
                         "last tag",
                         "rec bat_V",
                         "rel bat_V",
                         "int tag init",
                         "int tag ID",
                         "int tag power",
                         "int tag min delay",
                         "int tag max delay",
                         "rec map"))

  # round memory available column to 1 digit
  out[, "mem avail" := round(out$'mem avail', 1)]

  # round battery voltage
  out[, "rel bat_V" := round(out$'rel bat_V', 1)]
  out[, "rec bat_V" := round(out$'rec bat_V', 1)]

  # prepare data for export
  out <- out[, c("rec num",
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
                 "rel bat_V",
                 "rec bat_V",
                 "int tag init",
                 "int tag ID",
                 "int tag power",
                 "int tag min delay",
                 "int tag max delay"
                 )
             ]   

  # enforce data output types
  ## out[, `:=` (`rec num` = as.numeric(`rec num`),
  ##             `rec mod` = as.character(`rec mod`),
  ##             `rec firmware` = as.character(`rec firmware`),
  ##             `rec map` = as.character(`rec map`),
  ##             `mem avail` = as.numeric(`mem avail`),
  ##             `rec init` = as.POSIXct(`rec init`, tz = "UTC"),
  ##             `rec download` = as.POSIXct(`rec download`, tz = "UTC"),
  ##             `comp download` = as.POSIXct(`comp download`, tz = "UTC"),
  ##             `first det` = as.POSIXct(`first det`, tz = "UTC"),
  ##             `last det` = as.POSIXct(`last det`, tz = "UTC"),
  ##             `num det` = as.numeric(`num det`),
  ##             `rel bat_V` = as.numeric(`rel bat_V`),
  ##             `rec bat_V` = as.numeric(`rec bat_V`),
  ##             `int tag init` = as.POSIXct(`int tag init`, tz = "UTC"),
  ##             `int tag ID` = as.character(`int tag ID`),
  ##             `int tag power` = as.character(`int tag power`),
  ##             `int tag min delay` = as.character(`int tag min delay`),
  ##             `int tag max delay` = as.character(`int tag max delay`)      
  ##             )
  ##             ]

    # add in file name info to output table
  out[, file := as.character(nme)]

  return(out)
}


