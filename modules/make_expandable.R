make_expandable <- function(text, threshold = 80) {
  if (length(text) == 0 || is.na(text)) return("")

  text <- as.character(text)
  escaped_text <- htmlEscape(text)

  if (nchar(text) <= threshold) return(escaped_text)

  short <- substr(text, 1, threshold)
  paste0(
    '<div class="expandable-text">',
    '<span class="short-text">', htmlEscape(short), '...</span>',
    '<span class="full-text" style="display:none;">', escaped_text, '</span> ',
    '<a href="#" class="toggle-link">Show more</a>',
    '</div>'
  )
}
