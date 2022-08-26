# unexported functions from roxygen2
RoxyTopic <- get("RoxyTopic", asNamespace("roxygen2"))
RoxyTopics <- get("RoxyTopics", asNamespace("roxygen2"))
made_by_roxygen <- get("made_by_roxygen", asNamespace("roxygen2"))
nice_name <- get("nice_name", asNamespace("roxygen2"))
topic_add_describe_in <- get("topic_add_describe_in", asNamespace("roxygen2"))
topic_add_name_aliases <- get("topic_add_name_aliases", asNamespace("roxygen2"))
write_if_different <- get("write_if_different", asNamespace("roxygen2"))
process_templates <- get("process_templates", asNamespace("roxygen2"))
needs_doc <- get("needs_doc", asNamespace("roxygen2"))

"%||%" <- function(a, b) {
  if (length(a) > 0) a else b
}
