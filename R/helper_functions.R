# helper functions for internal use
# create empty nested list to capture output
make_nested <- function(dimensions){
  if(length(dimensions) == 1){
    return(vector("list", dimensions))} else{
      return(lapply(1:dimensions[1], function(...) make_nested(dimensions[-1])))
    }
}

# make empty table if no data is present
# this creates one row with "NA" so it joins well down the way...
empty_table <- function(x){
  dt <- setDT(lapply(x, function(f) f(NA)[1]))
  time_cols <- names(dt)[sapply(dt, is.POSIXct)]
  for(y in time_cols){
    attributes(dt[[y]])$tzone <- "UTC"
  }
  return(dt)
}

# base R does not have a is.POSIXct function.
# this creates a is.POSIXct function to test (TRUE, FALSE) whether object is a POSIXt type.
is.POSIXct <- function(x){inherits(x, c("POSIXct", "POSIXlt"))}

# helper functions that takes a character string from the schema and converts it to a function that constructs the type
as.constructor <- function(type_str){
  if(!is.character(type_str)) return(type_str)
  val_func_name <- paste0("as.", type_str)
  
  tryCatch({
    match.fun(val_func_name)}, error = function(e){
      stop(paste("Construction function not found for:", type_str))
    })
}

# submits the command line to system to extract receiver info from vdat
.compile <- function(vdata_file, vdat_pth){

  if(Sys.info()[["sysname"]] == "Windows"){
    ## Convert function arguments
    vdat_args <- c(
      "convert",
      vdata_file,
      "--format=csv.fathom",
      #paste0("--output=", out_dir),
      "--timec=default", "--progress"
    )
    shell_out <- processx::run(command = vdat_pth, args = vdat_args, echo_cmd = TRUE, echo = TRUE)
}

if(Sys.info()[["sysname"]] == "Linux"){
  
  vdat_linux(exe_path = vdat_pth, args_paths = vdata_file)
}
  
  return(vdata_file)
}


# Internal function for extracting tables from vrl files
# this creates a named list for each table extracted for each receiver input

 read_dta_lst <- function(src, record_types = c("DET", "DATA_SOURCE_FILE", "CFG_CHANNEL", "CLOCK_REF", "EVENT_INIT", "EVENT_OFFLOAD", "CFG_TRANSMITTER")){

   record_type = NULL #NSE fix for R CMD check
   
    vdat_txt <- data.table::fread(file = src, skip = 2, header = FALSE, sep = NULL, col.names = "txt")

    vdat_txt[, `:=`(record_type, data.table::fread(file = src, skip = 2, header = FALSE, sep = ",", select = 1, fill = TRUE))]
    vdat_txt[, `:=`(record_type, gsub("_DESC$", "", record_type))]
    vdat_txt <- vdat_txt[record_type %in% record_types,]
    vdat_list <- split(vdat_txt, by = "record_type", keep.by = FALSE)
    vdat_list <- lapply(vdat_list, function(x) {fread(text = paste0(c(x$txt,""), collapse = "\n"), sep = ",", na.strings = "", colClasses = "character", header = TRUE, drop = 1)})

    return(vdat_list)
  }


# not sure this is needed anymore...
## extract_table <- function(fls, vdat_pth = vdat_check(), action, battery ){

##   hash = datapath = pathname = NULL # due to NSE notes in R CMD check

##   # create keyl
##   setDT(fls)
##   fls[, hash := digest::digest(datapath, serialize = FALSE), by = 1:nrow(fls)]
##   fls[, pathname := basename(datapath)]
##   dta <- lapply(as.list(fls$datapath), .compile, vdat_pth = vdat_pth )
##   foo <- lapply(dta, function(x){(fls[fls$datapath %in% x, ]$hash)})
##   names(dta) <- unlist(foo)
##   dta <- lapply(dta, function(x) {gsub(pattern = "\\.(vrl|vdat)$", x = x, replacement = ".csv")})

##   # create list of tables for each receiver
##   dta <- lapply(dta, read_dta_lst)
##   #saveRDS(dta, "~/Desktop/fix.rds")

##   # new S7 workflow
##   # create vdat schema that defines column names and data types
##   # vdat class constructor handles missing columns and tables, etc
##   struct <- vdat_schema()

##   # create a vdat object based on receiver file inputs
##   # this creates a validated vdat object (list containing multiple data.tables)
##   # of vdat class.
##   out_vdat <- vdat(data = dta, schema = struct, fls = fls$hash, action = action, battery = battery)
    
##   return(out_vdat)
## }

