"%||%" <- function(a, b) {
  if (length(a) > 0) a else b
}



# Unexported functions from roxygen2 --------------------------------------

## pulling these in the friendliest way to avoid build-time dependencies
## TODO: Figure out better solution, according to:
##   https://www.tidyverse.org/blog/2022/09/playing-on-the-same-team-as-your-dependecy/

made_by_roxygen <- function(...) {
  asNamespace("roxygen2")$made_by_roxygen(...)
}

nice_name <- function(...) {
  asNamespace("roxygen2")$nice_name(...)
}

topic_add_describe_in <- function(...) {
  asNamespace("roxygen2")$topic_add_describe_in(...)
}

topic_add_name_aliases <- function(...) {
  asNamespace("roxygen2")$topic_add_name_aliases(...)
}

write_if_different <- function(...) {
  asNamespace("roxygen2")$write_if_different(...)
}

process_templates <- function(...) {
  asNamespace("roxygen2")$process_templates(...)
}

needs_doc <- function(...) {
  asNamespace("roxygen2")$needs_doc(...)
}

escapeExamples <- function(...) {
  asNamespace("roxygen2")$escapeExamples(...)
}

RoxyTopic <- function() {
  asNamespace("roxygen2")$RoxyTopic
}

RoxyTopics <- function() {
  asNamespace("roxygen2")$RoxyTopics
}
