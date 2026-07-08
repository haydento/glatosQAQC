#' @include classes.R
# Summarize vdat output table and produce table shown in shiny app.

# Create "tableizer" generic.  Input must be a data.table, cannot be empty.
tableize <- S7::new_generic("tableize", dispatch_args = "x")

# create and validate a list of vdats produced by shiny
# @name tableize method for vdat collection object (vdat_c)
# @title tableize method for vdat_c
# @param x input vdat_c object
# @export

# formats data for output table.
  S7::method(tableize, vdat_c) <- function(x){
    x <- x@items
    y <- rbindlist(lapply(x, function(x){.tableize(x@rec_data, nme = out_names(), battery = x@battery, action = x@action, cols = table_cols())}))
    
    return(tableize_class(y))
  }

 
.tableize <- function(x, nme, battery = x@battery, action = x@action, cols = table_cols()){

  `int tag delay` = `int tag delay min` = `int tag delay max` = `variable` = NULL # due to NSE notes in R CMD check
  
    output <- lapply(x, function(x){rbindlist(x, fill = TRUE, idcol = "file")})

  # extract first and last dtc records for each file and cast as wide
  # this seems to handle NA values in the RHS ok
  output[["DET"]] <- dcast(output[["DET"]], file ~ ., fun.aggregate = list(min, max), value.var = "Time")

  # convert from long to wide format for both "Time" and "External Time" columns
  # Because there is an NA in the RHS, you end up with a column of NAs for both "time" and "external time"
  # subset these out.
  output[["CLOCK_REF"]] <- dcast(output[["CLOCK_REF"]], file ~ Source, value.var = c("Time", "External Time (UTC)"))

  # schema that maps new column names for old names
  # this makes output easier to understand
  # format: list(new name = old name)
  # if name is not included in schema, nothing happens to it (i.e, not deleted)

  # extract tables
  fix_names <- names(nme)
  fix_names <- fix_names[fix_names != "cols"]

  # assign new names
  for(i in 1:length(fix_names)){
    names.i <- nme[[fix_names[i]]]
    data.table::setnames(output[[fix_names[i]]], old = unlist(names.i) , new = names(unlist(names.i)), skip_absent = TRUE)
  }
  
  # odds and ends- combine tag power info
  output[["CFG_TRANSMITTER"]] <- output[["CFG_TRANSMITTER"]][, `int tag delay` := paste(`int tag delay min`, `int tag delay max`, sep = "-")]

  # create object that holds info for clock and integrated tag
  #int_trans <- dta[c("CFG_TRANSMITTER", "CLOCK_REF", "EVENT_OFFLOAD")]
  #int_trans <- lapply(int_trans, data.table::copy)

  # convert all timestamps to character with format.  This allows setting the output format.
  output <- lapply(output, function(x){
    posix_cols <- names(x)[sapply(x, inherits, "POSIXct")]
    for(col in posix_cols){
      data.table::set(x, j = col, value = format(x[[col]], format = "%Y-%m-%d %H:%M"))
    }
    return(x)
  }
  )

  # convert all integers to character.
  output <- lapply(output, function(x){
    int_cols <- names(x)[sapply(x, is.numeric)]
    for(col in int_cols){
      data.table::set(x, j = col, value = as.character(x[[col]]))
    }
    return(x)
  }
  )

  # create one long format data.table with all values character
#  output <- rbindlist(lapply(output, function(x) melt(x, id.vars = c("file", "grp"), value.name = "value", value.factor = FALSE, variable.factor = FALSE)), idcol = "source")
  output <- rbindlist(lapply(output, function(x) melt(x, id.vars = c("file"), value.name = "value", value.factor = FALSE, variable.factor = FALSE)), idcol = "source")

  # create output
  #cols = c("name", "rec num", "rec mod", "rec firmware", "rec map", "mem avail", "battery (%)", "rec init", "rec download", "comp download", "first det", "last det", "num det")
    
  dta <- dcast(output, file ~ variable, value.var = "value", subset = .(variable %in% cols))
  
  # exclude file column
    dta <- dta[, !"file"]

    # set output order
    #browser()
    setcolorder(dta, cols)
    if (action == "download"){
  dta[, action := "download"]}
    if (action == "init"){
      dta[, action := "initialize"]}

  # add in battery replacement (new or not)
  if (battery == "yes"){
    dta[, battery := "new"]}
  if (battery == "no"){
    dta[, battery := "old"]}
  
  return(dta)
}

# @name tableize method 
# @title print table of clock metadata and user provided data
# @param x input object, clk class

S7::method(tableize, clk) <- function(x){
  y <- data.table(field = c(
    "Data recorder",
    "Study code",
    "Time zone",
    "Full test tag ID",
    "Vue version",
    "NIST time (lcl)",
    "computer time (lcl)",
    "time difference (s)",
    "vdat version"), 
    value = c(
      x@recorder,
      x@code,
      x@tz,
      x@tag,
      x@vue,
      format(x@tsync$`NIST time (lcl)`),
      format(x@tsync$`computer time (lcl)`),
      x@tsync$`time difference (s)`,
      x@vdat_ver)
    )
  return(tableize_class(y)) 
}



