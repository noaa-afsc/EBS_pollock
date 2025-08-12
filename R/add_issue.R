# add_issue.R
# Append a new ADMB → RTMB bridge issue to inst/issues.csv (no owner column)
#
# Usage:
#   source("scripts/add_issue.R")
#   add_issue(
#     component      = "Catch likelihood (robust)",
#     admb_behavior  = "Huberized residuals with c=1.345 on log-scale",
#     rtmb_behavior  = "Replicate Huber loss using vectorized penalty",
#     status         = "Pending",
#     priority       = "Medium",
#     test_ref       = "test-catch-robust"
#   )
#
add_issue <- function(component,
                      admb_behavior,
                      rtmb_behavior,
                      status    = c("Pending","In progress","Blocked","Done")[1],
                      priority  = c("High","Medium","Low")[1],
                      test_ref  = "",
                      id        = NULL,
                      issues_path = file.path("inst","issues.csv")) {

  if (!dir.exists(dirname(issues_path))) dir.create(dirname(issues_path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(issues_path)) {
    df <- utils::read.csv(issues_path, stringsAsFactors = FALSE)
  } else {
    df <- data.frame(id=character(), component=character(), admb_behavior=character(),
                     rtmb_behavior=character(), status=character(), priority=character(),
                     test_ref=character(), stringsAsFactors = FALSE)
  }

  # Generate next ID if missing
  if (is.null(id) || is.na(id) || !nzchar(id)) {
    existing <- suppressWarnings(as.integer(sub("^ISS-","", df$id)))
    next_n <- if (length(existing) && any(!is.na(existing))) max(existing, na.rm=TRUE) + 1L else 1L
    id <- sprintf("ISS-%03d", next_n)
  } else if (id %in% df$id) {
    stop("ID already exists: ", id)
  }

  # Validate fields
  status <- match.arg(status, c("Pending","In progress","Blocked","Done"))
  priority <- match.arg(priority, c("High","Medium","Low"))

  new <- data.frame(id=id, component=component, admb_behavior=admb_behavior,
                    rtmb_behavior=rtmb_behavior, status=status, priority=priority,
                    test_ref=test_ref, stringsAsFactors = FALSE)

  df2 <- rbind(df, new)
  utils::write.csv(df2, issues_path, row.names = FALSE)
  message("Added: ", id, " → ", issues_path)
  invisible(df2)
}
