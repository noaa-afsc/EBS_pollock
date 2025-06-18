read_data <- function(file) {
  lines <- readLines(file)
  result <- list()
  current_name <- NULL
  buffer <- list()
  
  for (line in lines) {
    line <- trimws(line)
    if (line == "") next
    if (startsWith(line, "#")) {
      if (!is.null(current_name)) {
        result[[current_name]] <- if (length(buffer) == 1) buffer[[1]] else do.call(rbind, buffer)
      }
      current_name <- sub("^#", "", line)
      buffer <- list()
    } else {
      nums <- as.numeric(unlist(strsplit(line, "\\s+")))
      buffer[[length(buffer) + 1]] <- nums
    }
  }
  
  if (!is.null(current_name)) {
    result[[current_name]] <- if (length(buffer) == 1) buffer[[1]] else do.call(rbind, buffer)
  }
  
  return(result)
}
