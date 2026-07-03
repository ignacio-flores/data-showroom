terminal_menu <- function(prompt,
                          choices,
                          default = 1L,
                          stdin_interactive = isatty(stdin()),
                          read_key = NULL) {
  if (!length(choices)) {
    stop("terminal_menu() requires at least one choice.")
  }
  if (!isTRUE(stdin_interactive)) {
    return(NA_integer_)
  }

  if (is.null(read_key)) {
    key_reader <- terminal_key_reader()
    on.exit(key_reader$close(), add = TRUE)
    read_key <- key_reader$read
  }

  current <- max(1L, min(as.integer(default %||% 1L), length(choices)))
  rendered <- FALSE
  rendered_lines <- length(choices) + 1L

  render <- function() {
    if (isTRUE(rendered)) {
      cat(sprintf("\033[%sA", rendered_lines))
    }
    cat("\r\033[J")
    cat(prompt, "\n", sep = "")
    for (idx in seq_along(choices)) {
      marker <- if (idx == current) "> " else "  "
      cat(sprintf("%s%s\n", marker, choices[[idx]]))
    }
    rendered <<- TRUE
    flush.console()
  }

  move <- function(delta) {
    current <<- ((current - 1L + delta) %% length(choices)) + 1L
  }

  render()
  repeat {
    key <- read_key()
    if (identical(key, "up")) {
      move(-1L)
      render()
    } else if (identical(key, "down")) {
      move(1L)
      render()
    } else if (identical(key, "enter")) {
      cat("\n")
      return(current)
    } else if (identical(key, "escape")) {
      cat("\n")
      return(NA_integer_)
    }
  }
}

terminal_key_reader <- function() {
  old <- system("stty -g", intern = TRUE)
  system("stty -echo -icanon min 1 time 0")

  con <- file("stdin", open = "rb")

  list(
    read = function() {
      system("stty -echo -icanon min 1 time 0")
      key <- readBin(con, what = "raw", n = 1L)
      if (!length(key)) {
        return("other")
      }

      if (identical(key, as.raw(0x1b))) {
        system("stty -echo -icanon min 0 time 1")
        next_bytes <- readBin(con, what = "raw", n = 2L)
        sequence <- rawToChar(c(key, next_bytes), multiple = FALSE)
        if (identical(sequence, "\033[A")) return("up")
        if (identical(sequence, "\033[B")) return("down")
        return("escape")
      }
      if (identical(key, charToRaw("\r")) || identical(key, charToRaw("\n"))) return("enter")
      "other"
    },
    close = function() {
      if (isOpen(con)) {
        close(con)
      }
      system(sprintf("stty %s", shQuote(old)))
    }
  )
}

read_terminal_key <- function() {
  reader <- terminal_key_reader()
  on.exit(reader$close(), add = TRUE)
  reader$read()
}

choose_menu_value <- function(prompt,
                              labels,
                              values = labels,
                              cancel_value = NULL,
                              menu_fn = terminal_menu) {
  choice <- menu_fn(prompt, labels)
  if (is.na(choice)) {
    return(cancel_value)
  }
  values[[choice]]
}
