tldr_selection_addin <- function() {
  selection <- rstudioapi::selectionGet()
  if (selection == "") {
    return(invisible(NULL))
  }
  rstudioapi::sendToConsole(paste0("tldr::tldr(", selection, ")"), execute = TRUE, focus = FALSE)
}

tldr_roxygenize_addin <- function() {
  rstudioapi::sendToConsole('roxygen2::roxygenize(roclets = c("rd", "namespace", "collate", "tldr_roclet"))', execute = TRUE, focus = FALSE)
}

# Very bare-bones miniUI shiny app
# To-Do: Theming, change size
tldr_input_addin <- function() {

  # check_installed(c("shiny", "miniUI"), "in order to use the shiny addin")

  # js for:
  #   setting focus on load
  #   hitting done on enter key press
  js <- '
$(document).on("shiny:connected", function(){
  document.getElementById("topic").focus();
});
$(document).keyup(function(event) {
    if ((event.key == "Enter")) {
        document.getElementById("done").click();
    }
});
'

  ui <- miniUI::miniPage(
    tags$head(tags$script(shiny::HTML(js))),
    miniUI::gadgetTitleBar("tldr"),
    miniUI::miniContentPanel(
      shiny::textInput("topic", "Topic", value = "", width = NULL, placeholder = NULL)
    )
  )

  server <- function(input, output, session) {
    shiny::observeEvent(input$done, {
      rstudioapi::sendToConsole(paste0("tldr::tldr(", input$topic, ")"), execute = TRUE, focus = FALSE)
      shiny::stopApp()
    })
  }

  app <- shiny::shinyApp(ui, server, options = list(quiet = TRUE))
  shiny::runGadget(app, viewer = shiny::dialogViewer("tldr"))
}

