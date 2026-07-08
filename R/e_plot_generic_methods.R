#' @include classes.R

# Plot integrated tag object timeline

# create new generic that makes output echarts plot
# @name e_plot
# @title interactive plot for integrated tag operation timing
# @param x input vdat_c object

e_plot <- S7::new_generic("e_plot", dispatch_args = "x")

# formats data for output table.
S7::method(e_plot, vdat_c) <- function(x){
  
    x <- x@items

  i.Model = `i.Serial Number` = `i.Full ID` = `i.Original File` = Time = `hover_label` = `Full ID` = `Source` =
    `y_lab1` = `Original File` = line_col = y_lab2 = NULL # due to NSE notes in R CMD check

  x <- lapply(x, function(i) i@rec_data[c("CFG_TRANSMITTER", "CLOCK_REF", "EVENT_OFFLOAD")])

  
  # combine into list of three necessary tables
  x <- list(
    CFG_TRANSMITTER = rbindlist(
      lapply(x, function(x) rbindlist(x$CFG_TRANSMITTER, fill = TRUE, idcol = "file")), fill = TRUE),
    CLOCK_REF = rbindlist(
      lapply(x, function(x) rbindlist(x$CLOCK_REF, fill = TRUE, idcol = "file")), fill = TRUE),
    EVENT_OFFLOAD = rbindlist(
      lapply(x, function(x) rbindlist(x$EVENT_OFFLOAD, fill = TRUE, idcol = "file")), fill = TRUE)
  )

   #browser()
  if(nrow(x$CFG_TRANSMITTER) == 0){return(NULL)}

  recs <- unique(x[["CFG_TRANSMITTER"]]$file)
  x$CLOCK_REF <- x$CLOCK_REF[file %in% recs,]
  x$EVENT_OFFLOAD <- x$EVENT_OFFLOAD[file %in% recs,]
  
  # add Source column = Power level for I tag
  set(x[["CFG_TRANSMITTER"]], j = "Source", value = x[["CFG_TRANSMITTER"]]$`Power Level`)

  x[["CLOCK_REF"]][x[["CFG_TRANSMITTER"]], `:=` (Model = i.Model, `Serial Number` = `i.Serial Number`,
                                                 "Full ID" = `i.Full ID`), on = .(file)]
  x[["CLOCK_REF"]][x[["EVENT_OFFLOAD"]], `:=` (`Original File` = `i.Original File`), on = .(file)]
  x[["CFG_TRANSMITTER"]][x[["EVENT_OFFLOAD"]], `:=` (`Original File` = `i.Original File`), on = .(file)]
  x <- rbindlist(x[c("CFG_TRANSMITTER", "CLOCK_REF")], fill = TRUE, idcol = "tbl_nme")
  x <- x[, c("tbl_nme", "file", "Time", "Model", "Serial Number", "Full ID", "Source", "Original File")] 
  setkey(x, file, Time)

  x[, `:=`(start = Time, end = data.table::nafill(data.table::shift(Time, type = "lead"), type = "locf")), by = "file"]
  x[, hover_label := paste0(`Full ID`, "|", `Source`)]
  x[, y_lab1 := `Original File`]


  tag_intervals <- x[!(Source %in% c("INITIALIZATION", "OFFLOAD")),]
  tag_intervals[, line_col := fcase(Source == "DISABLED", "black", default = "red")]
  tag_intervals[, y_lab1 := `Original File`]
  tag_intervals[, y_lab2 := `Full ID`]

  #tag_intervals[, line_col := c("red", "blue", "red", "blue", "red", "blue")]
  chart <- echarts4r::e_charts() 

  arrow_right_path <- "path://M0,2 L8,5 L0,8 L2,5 Z"

  # 2. Initialization (Blue Rings)
  chart <- echarts4r::e_data(chart, x[Source == "INITIALIZATION", .(Time, y_lab1, `Full ID`, hover_label)], `Time`)
  chart <-  echarts4r::e_scatter(chart, y_lab1, name = "Initialization time", bind = hover_label, symbol_size = 20, symbol = "circle", itemStyle = list(color = "transparent", borderColor = "blue", borderWidth = 2))

  # 3. Offload (Red Rings)
  chart <- echarts4r::e_data(chart, x[Source == "OFFLOAD", .(Time, y_lab1, `Full ID`, hover_label)], `Time`)
  chart <- echarts4r::e_scatter(chart, y_lab1, name = "Offload time", bind = hover_label, symbol_size = 20, symbol = "circle",
    itemStyle = list(color = "transparent", borderColor = "red", borderWidth = 2)
  )


  # plot intervals
  for(i in 1:nrow(tag_intervals)) {
    #for(i in 1:100){
  
    segment_data <- data.table::data.table(
      Time = c(tag_intervals$start[i], tag_intervals$end[i]),
      y_lab1 = c(tag_intervals$y_lab1[i], tag_intervals$y_lab1[i]),
      hover_label = c(tag_intervals$hover_label[i], tag_intervals$hover_label[i]),
      line_col = c(tag_intervals$line_col[i])
    )
    
    chart <- echarts4r::e_data(chart, segment_data, `Time`) 
    chart <- echarts4r::e_line(chart, y_lab1, name = "interval vector", bind = hover_label,
      symbol = c("none"),
      showSymbol = FALSE,
      #lineStyle = list(width = 3, color = segment_data$line_col),
      lineStyle = list(width = 3, color = tag_intervals$line_col[i]),
      legend = FALSE)

    # create Rt end arrows for line segments
    chart <- echarts4r::e_data(chart, segment_data[2,], `Time`)
    chart <- echarts4r::e_scatter(chart, y_lab1, name = "interval vector", bind = hover_label,
      symbol = arrow_right_path,
      symbol_size = c(20, 20),
      #symbol_rotate = 90,
      itemStyle = list(color = tag_intervals$line_col[i]),
      legend = FALSE)

    # create start line markers
    chart <- echarts4r::e_data(chart, segment_data[1,], `Time`)
    chart <- echarts4r::e_scatter(chart, y_lab1, name = "interval vector", bind = hover_label,
      symbol = "rect",
      symbol_size = c(8, 20),
      #symbol_rotate = 90,
      itemStyle = list(color = tag_intervals$line_col[i]),
      legend = FALSE)
  }


  # 4. Custom Tooltip Formatter Fix
  # ECharts time-axis passes an array of values where params.value[0] is the timestamp 
  # and params.value[1] is the categorical Y value. Bound data or extra series parameters
  # are usually tucked into params.value or params.name depending on series type.
  chart <- echarts4r::e_tooltip(chart, trigger = "item",
    formatter = htmlwidgets::JS("function(params) {
      // 1. Format the Date
      var date = new Date(params.value[0]);
      var formattedTime = date.toISOString().replace('T', ' ').substring(0, 19);
      
      // 2. Extract and split the bound information
      var fullID = 'N/A';
      var source = 'N/A';
      
      if (params.name) {
        var parts = params.name.split('|');
        fullID = parts[0] ? parts[0] : 'N/A';
        source = parts[1] ? parts[1] : 'N/A';
      }
      
      return '<strong>Full ID:</strong> ' + fullID + '<br />' +
             '<strong>Time:</strong> ' + formattedTime + '<br />' +
             '<strong>Status:</strong> ' + source;
    }"))

  # 5. Axes Configurations
  chart <- chart |> 
    echarts4r::e_y_axis(
      type = "category", 
      name = "Receiver File",
      axisLabel = list(interval = 0, fontSize = 10, overflow = "truncate")
    ) |> 
    echarts4r::e_x_axis(
      type = "time", 
      name = "Date-Time"
    )

  # 6. Interactivity & Sliders
  chart <- chart |> 
    echarts4r::e_datazoom(y_index = 0, orient = "vertical", filterMode = "none", type = "slider", realtime = FALSE) |> 
    echarts4r::e_datazoom(x_index = 0, orient = "horizontal", filterMode = "none", type = "slider", realtime = FALSE)

  # 7. Layout Adjustments
  chart <- chart |> 
    echarts4r::e_legend(left = "center", top = "top") |> 
    echarts4r::e_grid(top = 0, bottom = 20, left = 50, right = 0) # Expanded margins slightly so axis labels don't get cut off

  # Display Chart
  return(chart)
}
