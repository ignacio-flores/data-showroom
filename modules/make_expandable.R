make_expandable <- function(text, threshold = 50) {
  if (nchar(text) <= threshold) return(text)

  short <- substr(text, 1, threshold)
  paste0(
    '<div class="expandable-text">',
    '<span class="short-text">', htmlEscape(short), '...</span>',
    '<span class="full-text" style="display:none;">', htmlEscape(text), '</span> ',
    '<a href="#" class="toggle-link">Show more</a>',
    '</div>'
  )
}
