Stack <- R6::R6Class(
  "Stack",
  private = list(.elements = list(), .size = 0),
  public = list(
    push = function(element) {
      private$.elements[[length(private$.elements) + 1]] <- element
      private$.size <- private$.size + 1
    },
    pop = function() {
      if (private$.size == 0) {
        stop("Stack is empty")
      }
      element <- private$.elements[[length(private$.elements)]]
      private$.elements[[length(private$.elements)]] <- NULL
      private$.size <- private$.size - 1
      return(element)
    },
    peek = function() {
      if (private$.size == 0) {
        stop("Stack is empty")
      }
      return(private$.elements[[length(private$.elements)]])
    },
    is_empty = function() {
      return(private$.size == 0)
    },
    size = function() {
      return(private$.size)
    },
    contents = function() {
      return(private$.elements)
    },
    as_list = function() {
      return(as.list(private$.elements))
    }
  )
)
