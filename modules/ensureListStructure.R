ensureListStructure <- function(input) {
  if (is.vector(input) && !is.list(input)) {
    # Convert vector to a list where each entry is a list with a 'label'
    setNames(lapply(input, function(x) list(label = x)), input)
  } else {
    input
  }
}