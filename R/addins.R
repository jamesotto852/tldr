tldr_selection_addin <- function() {
  selection <- rstudioapi::selectionGet()
  if (selection == "") {
    return(invisible(NULL))
  }
  rstudioapi::sendToConsole(paste0("tldr::tldr(", selection, ")"), execute = TRUE, focus = FALSE)
}

tldr_roxygenize_addin <- function() {
  library("tldr")
  rstudioapi::sendToConsole('roxygen2::roxygenize(roclets = c("rd", "namespace", "collate", "tldr_roclet"))', execute = TRUE, focus = FALSE)
}

# Run examples of corresponding to previous tldr() call
tldr_prev_examples_addin <- function() {
  tempfile <- tempfile(pattern="rhistory_", fileext=".txt")
  utils::savehistory(tempfile)

  hist <- readLines(tempfile)
  # Need to adjust for call to tldr_prev_examples_addin() if interactive is true
  # hist <- utils::tail(hist, 2)[1]
  hist <- utils::tail(hist, 1)[1]

  if (!grepl("^tldr(\\(|::tldr\\()", hist)) {
    # Error in extension is very messy
    # stop("Previous command wasn't of form tldr(fun)")
    return(invisible(NULL))
  }

  # Get tldr output as text string
  # TODO - better error handling
  # TODO - currently, capture.output wraps lines based on width of console pane,
  #        leads to issues w/ long bullet lines
  tldr_output <- tryCatch(
    utils::capture.output(eval(parse(text=hist)), type = "message"),
    error = function(e) ""
  )

  # Get examples section (if it exists)
  if (!any(grepl("\u2022 Common Tasks:", tldr_output))) {
    # stop("No tasks in previous tldr() output")
    return(invisible(NULL))
  }
  tasks_begin <- grep("\u2022 Common Tasks:", tldr_output) + 1
  tldr_output <- tldr_output[tasks_begin:length(tldr_output)]

  # cli attempt -- runs into issues as rstudioapi::sendToConsole consolidates  calls
  if (FALSE) {

    # trim leading 2x spaces
    tldr_output <- gsub("^  ", "", tldr_output)

    lapply(tldr_output, tldr_prev_examples_item)

  }

  # basic method using comments
  if (TRUE) {

    # replace bullet points with "#"
    tldr_output <- gsub("\u2022", "#", tldr_output)

    # trim leading whitespace
    tldr_output <- trimws(tldr_output)

    # send commands to console
    rstudioapi::sendToConsole(tldr_output, execute = TRUE, focus = FALSE)

  }

}

tldr_prev_examples_item <- function(line) {
  if (grepl("\u2022", line)) {
    line <- substring(line, 3) # Remove bullet point and space
    cli_h3(line)
    # line <- paste0('cli::cli_h3("', line, '")')
    # rstudioapi::sendToConsole(line, echo = FALSE, execute = TRUE, focus = FALSE)
  } else {
    cli_div()
    line <- substring(line, 3) # Remove leading 2x spaces
    rstudioapi::sendToConsole(line, execute = TRUE, focus = FALSE)
  }
}

tldr_input_addin <- function() {

  topic <- rstudioapi::showPrompt(
    title = "Render tldr Documentation",
    message = "Input topic:",
    default = ""
  )

  rstudioapi::sendToConsole(paste0("tldr::tldr(", topic, ")"), execute = TRUE, focus = FALSE)

}

