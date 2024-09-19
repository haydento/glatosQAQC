##' Compiles data from all receivers, generates table of data included in report.
##' 
##' @param fls a character vector of files to include in report
##' @param params rmarkdown parameters needed to process data
##' @param work_dir working directory where processing takes place


# if don't want to interactively choose files
#' fls <- list.files("~/Desktop", "*.vrl", full.names = TRUE, recursive = TRUE)[-1] 


#'fls <- c("C:/Users/Admin/Documents/VRL_tests/VR2AR_546310_20190607_1.vrl", "C:/Users/Admin/Documents/VRL_tests/VR2AR_546310_20190607_1.vrl", "C:/Users/Admin/Documents/VRL_tests/VR2AR_546310_20220624_1.vrl", "C:/Users/Admin/Documents/VRL_tests/VR2W_134214_20230626_1.vrl")
#' vdat_pth <- "C:/Program Files/Innovasea/Fathom Connect/vdat.exe" 
#'
#' #OffInitOn
#' fls <- c("C:/Users/Admin/Desktop/SyncTest/OffInitOn/2024-08-21/NexTrak-R1 800305 2024-08-21 112645.vdat", "C:/Users/Admin/Desktop/SyncTest/OffInitOn/2024-08-21/VR2AR-69 548785 2024-08-21 112612.vdat", "C:/Users/Admin/Desktop/SyncTest/OffInitOn/2024-08-21/VR2AR-69 548785 2024-08-21 112612.vdat")
#'
#' #OffInitOnOff
#' fls <- c("C:/Users/Admin/Desktop/SyncTest/OffInitOnOff/2024-08-21/NexTrak-R1 800305 2024-08-21 113003.vdat", "C:/Users/Admin/Desktop/SyncTest/OffInitOnOff/2024-08-21/VR2AR-69 548785 2024-08-21 112915.vdat", "C:/Users/Admin/Desktop/SyncTest/OffInitOnOff/2024-08-21/VR2Tx-69 483804 2024-08-21 113022.vdat")
#'
#'fls <- c("C:/Users/Admin/Desktop/SyncTest/OnInitOff/2024-08-21/NexTrak-R1 800305 2024-08-21 113630.vdat", "C:/Users/Admin/Desktop/SyncTest/OnInitOff/2024-08-21/VR2AR-69 548785 2024-08-21 113643.vdat", "C:/Users/Admin/Desktop/SyncTest/OnInitOff/2024-08-21/VR2Tx-69 483804 2024-08-21 113604.vdat")
#'
#'
#' fls <- c("C:\\Users\\Admin\\Desktop\\SyncTest\\OnInitOffOn\\2024-08-21\\NexTrak-R1 800305 2024-08-21 113851.vdat", "C:\\Users\\Admin\\Desktop\\SyncTest\\OnInitOffOn\\2024-08-21\\VR2AR-69 548785 2024-08-21 113813.vdat", "C:\\Users\\Admin\\Desktop\\SyncTest\\OnInitOffOn\\2024-08-21\\VR2Tx-69 483804 2024-08-21 113934.vdat") 
#'
#' fls <- c("C:\\Users\\Admin\\Desktop\\SyncTest\\OnInitOffOnChange\\2024-08-21/NexTrak-R1 800305 2024-08-21 114305.vdat", "C:\\Users\\Admin\\Desktop\\SyncTest\\OnInitOffOnChange\\2024-08-21\\VR2AR-69 548785 2024-08-21 114233.vdat" , "C:\\Users\\Admin\\Desktop\\SyncTest\\OnInitOffOnChange\\2024-08-21\\VR2Tx-69 483804 2024-08-21 114324.vdat" )
#'
#' 
#' vdat_pth <- "C:/Program Files/Innovasea/Fathom Connect/vdat.exe" 
#' 
#' mrk_params = "C:\\Program Files\\vdat\\vdat.exe"
#' work_dir = "~/Documents/"
#' nme <- basename(fls)
#' action <- "down"
#' datapath = file.path(tempdir(), paste0(seq(0,length(fls)-1,1),".vrl"))

#' dtc <- glatosQAQC::compile_vdats(vdat_files = fls, v_path = mrk_params)
#'
#' 
#' foo <-  process_table(fls = fls, mrk_params = mrk_params, nme = nme, action = action, datapath = datapath, work_dir = tempdir())
# https://stackoverflow.com/questions/52385295/get-the-name-of-an-uploaded-file-in-shiny

#' @export

#process_table <- function(fls, vdat_pth, nme, action, work_dir = work_dir, datapath = datapath, schema = scheme()){

process_table <- function(fls, action, work_dir = work_dir, vdat_pth = vdat_call, nme){

  #vdat_pth = glatosQAQC::check_vdat()
  
  # extract vdat version used to extract data
  # this is displayed in output
  shell_out <- sys::exec_internal(cmd = vdat_pth, args = "--version")
  vdat_ver <- rawToChar(shell_out$stdout)
  vdat_ver <- unlist(strsplit(vdat_ver, "\r?\n"))
  
  # extracts all vdat data into a directory of multiple csv files.  Returns a vector of local paths pointing to directories containing extracted data (csv files).  
  dta <- lapply(fls, .vdat_compile, vdat_pth = vdat_pth )

  # read in data needed to make summary

  # get file information from within file
  #file_id <- read_select_cols(dta_fls = dta, tbl = "DATA_SOURCE_FILE.csv", col_select = vdat_cols[["DATA_SOURCE_FILE"]])
  file_id <- read_tbl(dta_fls = dta, tbl = "DATA_SOURCE_FILE.csv")
  file_id <- lapply(file_id, as.data_source_file)
  file_id <- rbindlist(file_id, idcol = "file")

  # column classes have been definied and only columns needed for summary are extracted.
  dtc <- read_tbl(dta_fls = dta, tbl = "DET.CSV")
  dtc <- lapply(dtc, as.det)
  dtc <- rbindlist(dtc, idcol = "file")

  # Process detection records to summarize first and last detections, and tags associated with detections
  dtc <- glatosQAQC::process_detections(dtc)
  
################
  # these next three lines are a work-around that re-establishes
  # a link between original file name and internal file name that is automatically assigned when the files are uploaded to shiny server.
  # this is a bug.  Not sure why or how file names within file are changed to automatically assigned shiny names
  # behaviour is obvious when step through these lines starting browser from here...
  #browser()
  
  nme_mod <- as.data.table(nme)
  nme_mod[, int_name := basename(datapath)]
  file_id[nme_mod, `File Name` := nme_mod$name, on = .(`File Name` = int_name)]
#################
  
  # combine detection records and file records
  out <- merge(dtc, file_id, by = "file", all.x = TRUE)

  # combine detection records and vdat version info
  out[, vdat_version := vdat_ver]

  rec_map <- read_tbl(dta_fls = dta, tbl = "CFG_CHANNEL.csv")
  rec_map <- lapply(rec_map, as.cfg_channel)
  rec_map <- rbindlist(rec_map, idcol = "file")

  # combine
  out <- merge(out, rec_map, by = "file", all.x = TRUE)

  # extract clock info (offload time, initilization time) from clock ref table
  clock_ref <- read_tbl(dta_fls = dta, tbl = "CLOCK_REF.csv")
  clock_ref <- lapply(clock_ref, as.clock_ref)
  clock_ref <- rbindlist(clock_ref, idcol = "file")

  # this extracts receiver init/download times and computer time at download
  # creates a wide table and renames columns
  clock_ref <- data.table::dcast(clock_ref, file ~ Source, value.var = c("Time", "External Time (UTC)"))
  setnames(clock_ref, c('Time_INITIALIZATION', 'Time_OFFLOAD', 'External Time (UTC)_INITIALIZATION', 'External Time (UTC)_OFFLOAD'), c("rec init", "rec download", "comp init", "comp download"))

# combine receiver initalization/download info
  out <- merge(out, clock_ref, by = "file", all.x = TRUE)
  
# extract firmware version, memory remaining%, total detections, model  
  # firmware = event_init
  event_init <- read_tbl(dta_fls = dta, tbl = "EVENT_INIT.csv" )
  event_init <- lapply(event_init, as.event_init)
  event_init <- rbindlist(event_init, idcol = "file")
  out <- merge(out, event_init, by = "file", all.x = TRUE)

  event_offload <- read_tbl(dta_fls = dta, tbl = "EVENT_OFFLOAD.csv")
  event_offload <- lapply(event_offload, as.event_offload)
  event_offload <- rbindlist(event_offload, idcol = "file")

  out <- merge(out, event_offload, by = "file", all.x = TRUE)

  # extract integrated tag info
  i_tag <- read_tbl(dta_fls = dta, tbl = "CFG_TRANSMITTER.csv")
  i_tag <- lapply(i_tag, as.cfg_transmitter)
  i_tag <- rbindlist(i_tag, idcol = "file")

  # works to here.  Need to figure out how to summarize i_tag data.
  # records for integrated tag are not restricted to initialization and download with the exception of all i_tag changes must occur after initialization and before the next future initialization.
  # for the QAQC, only look at status between init and download and report the setting with the longest duration during this time period.
  # for HBBS protocol, this duration is the only event possible (i.e., should be no partial downloads
  # changes in i_tag power after download but before the next initialization are not considered.
  
###################################
  
  if(nrow(i_tag) > 0){

    i_tag <- i_tag[!is.na(CFG_TRANSMITTER_DESC),]    
    setkey(i_tag, file, Time)
    i_tag$start <- i_tag$Time
    i_tag[, end := data.table::shift(Time, fill = NA, type = "lead"), by = "file"] #11

    event <- out[!is.na(`comp init`), c("file", "comp init", "comp download")] #23
    
    i_tag <- event[i_tag, on = "file"]
    i_tag[is.na(end), end := `comp download`]
    i_tag <- i_tag[, duration := difftime(end, start, units = "secs")]
    i_tag <- i_tag[i_tag[, .I[duration == max(duration)], by = "file"]$V1, c("file", "Time", "Power Level", "Min Delay (s)", "Max Delay (s)", "Full ID")]

  } else {
    i_tag <- data.table(file = NA_character_,
                        Time = as.POSIXct(NA, tz = "UTC"),
                        `Power Level` = NA_character_,
                        `Min Delay (s)` = NA_integer_,
                        `Max Delay (s)` = NA_integer_,
                        `Full ID` = NA_character_)
  }
  
  
    
  # combine objects
  out <- merge(out, i_tag, by = "file", all.x = TRUE)

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
                              "vdat_version",
                              "Serial Number",
                              "Model",
                              "Firmware Version",
                              "PPM Total Accepted Detections",
                              "Battery Remaining (%)",
                              "File Name"
                              ),
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
                         "vdat_ver",
                         "rec num",
                         "rec mod",
                         "rec firmware",
                         "num det",
                         "battery (%)",
                         "OG source"
                        ))

  # round memory available column to 1 digit
  out[, `mem avail` := round(out$`Memory Remaining (%)`, 1)]


  # prepare data for export
  out <- out[, c("OG source",
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
  out[, `:=` (`OG source` = as.character(`OG source`),
              `rec num` = as.integer(`rec num`),
              `rec mod` = as.character(`rec mod`),
              `rec firmware` = as.character(`rec firmware`),
              `rec map` = as.character(`rec map`),
              `mem avail` = as.integer(`mem avail`),
              `rec init` = as.POSIXct(`rec init`, tz = "UTC"),
              `rec download` = as.POSIXct(`rec download`, tz = "UTC"),
              `comp download` = as.POSIXct(`comp download`, tz = "UTC"),
              `first det` = as.POSIXct(`first det`, tz = "UTC"),
              `last det` = as.POSIXct(`last det`, tz = "UTC"),
              `num det` = as.integer(`num det`),
              `int tag init` = as.POSIXct(`int tag init`, tz = "UTC"),
              `int tag ID` = as.character(`int tag ID`),
              `int tag power` = as.character(`int tag power`),
              `int tag min delay` = as.integer(`int tag min delay`),
              `int tag max delay` = as.integer(`int tag max delay`),
              `vdat_ver` = as.character(`vdat_ver`),
              `battery (%)` = as.integer(`battery (%)`)
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


