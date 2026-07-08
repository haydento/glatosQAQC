
# class defs

# create new S7 vdat class for validated vdat object
# @title class of all data exported from vdat file object
# @name vdat
# @param meta shiny server upload data property
# @param fls receiver file names property
# @param battery character vector of battery status spec'd by user
# @param action character vector user spec'd download or upload
# @param vdat_pth path to vdat executable on user machine.
# @param rec_data list of extracted receiver data
# @param shiny_in input from shiny data selection
# @param schema schema of column names and data types

vdat <- S7::new_class(
  name = "vdat",
  package = "glatosQAQC",
  properties = list(
  meta = S7::class_any, # table of input data from shiny upload
  fls = S7::class_character, # actual receiver log list
  battery = S7::class_character,
  action = S7::class_character,
  vdat_pth = S7::class_character,
  rec_data = S7::class_list
  ),
  constructor = function(shiny_in, schema, battery, action, vdat_pth){

  `..req_cols` = Source = new_object = S7_object = hash = datapath = NULL
  
  # If nothing provided then stop!
    stopifnot("No input!" = length(shiny_in) > 0 || is.null(shiny_in))
    stopifnot("data property must be a data.frame!" = is.data.frame(shiny_in))

    setDT(shiny_in)
    shiny_in[, hash := digest::digest(datapath, serialize = FALSE), by = 1:nrow(shiny_in)]
    fls <- shiny_in$hash
    set(shiny_in, j = "pathname", value = basename(shiny_in$datapath))
    set(shiny_in, j = "og_datapath", value = shiny_in$datapath)
    set(shiny_in, j = "datapath", value = gsub(pattern = "\\.(vrl|vdat)$", x = shiny_in$datapath, replacement = ".csv"))
    
    dta <- lapply(as.list(shiny_in$og_datapath), .compile, vdat_pth = vdat_pth)
    dta <- as.list(stats::setNames(shiny_in$datapath, shiny_in$hash))

    # create list of tables for each receiver
    dta <- lapply(dta, read_dta_lst)
  
  # convert schema to vdat object
  schema <- rapply(schema, f = as.constructor, how = "replace")

  # extract names of tables from schema
  tbl <- names(schema)

  # make nested list output for results
  # transpose list such that output is files nested within tables
  output <- make_nested(c(length(tbl), length(fls)))

  # set names for each table type
  names(output) <- tbl

  # set names for nested files within tables
  output <- lapply(output, function(x) stats::setNames(x, fls))

  # extract data in schema, check column names
  # ark <- obs 
  # obs <- ark
  # fls <- unique(names(data))
  for(i in 1:length(fls)){
    for(j in 1:length(tbl)){
      dt_ij <- dta[[fls[i]]][[tbl[j]]]
      schema_ij <- schema[[tbl[j]]]
      if(is.null(dt_ij)){dt_ij <- empty_table(schema_ij)}
      req_cols <- names(schema_ij)
      # extract only required columns
      dt_ij <- dt_ij[, req_cols, with = FALSE]
      for(k in 1:length(req_cols)){      
        set(dt_ij, j = names(schema_ij[k]),
            value = do.call(schema_ij[[k]],
                            list(x = dt_ij[[names(schema_ij[k])]], tz = "UTC")))
      }

      # this line transposes the output such that each file is nested within tables.
      output[[tbl[j]]][[fls[i]]] <- dt_ij
    }
  }
    
  #saveRDS(output, "~/Desktop/check.rds")
  #output <- readRDS("~/Desktop/check.rds")

  # if `Original File` is missing, (this data was not included in early files ~ 2010)
  # then recreate file name from model, serial number, and initialization date
  # Decided to do this rather than use the original file name
  missing <- lapply(output[["EVENT_OFFLOAD"]], function(y){is.na(y$`Original File`)})
  if(any(missing == TRUE)){
    lost <- names(missing[missing == TRUE])
    
    for(i in 1:length(lost)){
      mod <- output[["CFG_CHANNEL"]][[lost[i]]]$`Model`
      serial <- output[["CFG_CHANNEL"]][[lost[i]]]$`Serial Number`
      date <- format(output[["CLOCK_REF"]][[lost[i]]][Source == "INITIALIZATION",][["Time"]], "%Y%m%d")
      output[["EVENT_OFFLOAD"]][[lost[i]]]$`Original File` <- sprintf("%s_%i_%s", mod, serial, date)
    }
  }

  # Initialize the S7 vdat class
  S7::new_object(
    S7::S7_object(),
    meta = shiny_in,
    rec_data = output,
    fls = fls,
    battery = battery,
    action = action,
    vdat_pth = vdat_pth
  )
},

validator = function(self){
  
  # create a vector to hold errors
  errors <- character()

  # Length constraint for fls input
  if(length(self@fls) == 0){
    errors <- c(errors, "@fls must include a receiver file")
  }

  if(!inherits(self@meta, c("data.table", "data.frame"))){
    errors <- c(errors, "@meta must be a data.frame or data.table")}

  if(!inherits(self@rec_data, c("list"))){
    errors <- c(errors, "@rec_data is not a list")
  }

  all_dt <- lapply(self@rec_data, function(sub){
    all(vapply(sub, function(x) {all(inherits(x, c("data.table", "data.frame")))}, FUN.VALUE = logical(1)))})

if(!all(unlist(all_dt))){
  errors <- c(errors, "all nested tables in @rec_data are not data.table or data.frame")
}

  if(length(errors) == 0){
    NULL
  } else {
    errors
  }
}
)

# create and validate a list of vdats produced by shiny
# @name list containing vdat objects for each load batch (i.e., old and new)
# @param items list object containing object for each load batch (i.e., old info and new info uploaded)
# @param in1 first vdat object
# @param in2 second vdat object

vdat_c <- S7::new_class(
  name = "vdat_c",
  package = "glatosQAQC",
  properties = list(
  items = S7::class_list,
  meta_all = S7::class_any),
  
  constructor = function(in1 = NULL, in2 = NULL){
    
    if(!inherits(in1, "glatosQAQC::vdat") & !inherits(in2, "glatosQAQC::vdat")){
      stop("one input must be vdat class")
    }
    if(is.null(in1) & inherits(in2, "glatosQAQC::vdat")){
      combo <- list(in2)
      meta = in2@meta
    }
    
    if(inherits(in1, "glatosQAQC::vdat_c") & inherits(in2, c("glatosQAQC::vdat"))){
      combo <- c(in1@items, in2)
      meta <- rbind(in1@meta_all, in2@meta)
      
    }
    S7::new_object(S7::S7_object(),
                   items = combo,
                   meta_all = meta)
  }
)


# @name clk
# @title class for validated output produced for shiny app clock tab.
# @param recorder recorder info entered by user
# @param code project code entered by user
# @param tz tz code
# @param tag test tag ID code
# @param vue vue version number

clk <- S7::new_class(
  name = "clk",
  package = "glatosQAQC",
  properties = list(
    `recorder` = S7::class_character,
    `code` = S7::class_character,
    `tz` = S7::class_character,
    `tag` = S7::class_character,
    `vue` = S7::class_character,
    `tsync` = S7::new_property(getter = function(self) {
      tnist <- try(httr::GET("http://www.google.com/")$date, silent = TRUE)
      if(inherits(tnist, "try-error")){tnist <- NA}
      attr(tnist, "tzone") <- "America/Detroit"
      tlocal <- Sys.time()
      attr(tlocal, "tzone") <- "America/Detroit"
      list(`NIST time (lcl)` = tnist,
        `computer time (lcl)` = tlocal,
        `time difference (s)` = abs(round(as.numeric(tnist) - as.numeric(tlocal), 2)))
    }),
    `vdat_ver` = S7::new_property(getter = function(self){
      vdat_version(vdat_exe_path = NULL)$version
    }
    )
  )
)

# Create "tableizer" class.  Input must be a data.table, cannot be empty.
# create and validate a list of vdats produced by shiny
# @name list containing vdat objects for each load batch (i.e., old and new)
# @param table output table object

tableize_class <- S7::new_class(
  name = "tableize_class",
  package = "glatosQAQC",
  properties = list(
    table = S7::class_any),
 
  validator = function(self){
  
    # create a vector to hold errors
    errors <- character()

    if(!is.data.table(self@table)){
      errors <- c(errors, "@table must be a data.table")
    }
    
    # Length constraint for fls input
    if(length(self@table) == 0){
      errors <- c(errors, "@input must include something")
    }

    if(length(errors) == 0){
      NULL
    } else {
      errors
    }
  }
)


# Create "excel" class.  Input must be a list of tableize classes that are then written into the excel file

# create and validate a list of vdats produced by shiny
# @name list containing vdat objects for each load batch (i.e., old and new)
# @param lst list object containing object for each load batch (i.e., old info and new info uploaded)

excel_class <- S7::new_class(
  name = "excel_class",
  package = "glatosQAQC",
  properties = list(
    lst = S7::class_list),
  
  validator = function(self){
    # create a vector to hold errors
    errors <- character()
    
    if(!all(sapply(self@lst, function(x){inherits(x, c("glatosQAQC::tableize_class"))}))){
      errors <- c(errors, "all objects in @lst must be glatosQAQC::tableize_class data.table")
    }
    
    if(length(errors) == 0){
      NULL
    } else {
      errors
    }
  }
)

