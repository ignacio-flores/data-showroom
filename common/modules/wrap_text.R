# Function to insert line breaks at specified intervals
wrap_text <- function(text, width = 20) {
  sapply(text, function(x) {
    # Split the string into words
    words <- unlist(strsplit(x, " "))
    wrapped <- ""
    line_length <- 0
    
    for (word in words) {
      # If adding the next word exceeds the width, add a line break
      if (nchar(word) + line_length > width) {
        wrapped <- paste0(wrapped, "<br>", word)
        line_length <- nchar(word)
      } else {
        # If it's the first word, don't prepend a space
        if (wrapped == "") {
          wrapped <- word
        } else {
          wrapped <- paste0(wrapped, " ", word)
        }
        line_length <- line_length + nchar(word) + 1
      }
    }
    return(wrapped)
  })
}