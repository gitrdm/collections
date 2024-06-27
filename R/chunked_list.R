#' ChunkedList R6 Class
#'
#' A flexible, chunked list implementation using R6 class system, allowing for efficient
#' dynamic list resizing and element access.
#'
#' @field data A list to store the data elements.
#' @field max_chunk_size The maximum size each chunk can grow to.
#'
#' @param initial_data Initial data to populate the ChunkedList with. Can be NULL, a single element, or a list of elements.
#' @param initial_size The initial size of the data list. Default is 10.
#' @param chunk_size The size of each chunk when the list is resized. Default is 10.
#' @param max_chunk_size The maximum size to which a chunk can grow. Default is 1000.
#' @return A new ChunkedList object.
#' @export
ChunkedList <- R6::R6Class(
  "ChunkedList",
  private = list(
    chunk_size = 10,
    # Default chunk size
    next_index = 1,
    number_of_times_chunked = 0
  ),
  public = list(
    data = NULL,
    max_chunk_size = NULL,

    #' @description Initializes a new ChunkedList object with optional initial data and size parameters.
    #' @method initialize ChunkedList
    #' @param initial_data Initial data to populate the ChunkedList.
    #' @param initial_size Initial size of the ChunkedList.
    #' @param chunk_size Size of the chunks for dynamic resizing.
    #' @param max_chunk_size Maximum size of any chunk in the list.
    #' @return Invisible self for method chaining.
    initialize = function(initial_data = NULL,
                          initial_size = 10,
                          chunk_size = 10,
                          max_chunk_size = 1000) {
      self$data <- vector("list", initial_size)
      private$chunk_size <- chunk_size
      self$max_chunk_size <- max_chunk_size
      if (!is.null(initial_data)) {
        if (is.list(initial_data)) {
          self$add_many(initial_data)
        } else {
          self$add(initial_data)
        }
      }
      invisible(self)
    },

    #' @description Adds a single element to the ChunkedList.
    #' @method add ChunkedList
    #' @param element The element to add to the list.
    #' @return Invisible self for method chaining.
    add = function(element) {
      if (private$next_index > length(self$data)) {
        # Increase the list size by the chunk size when needed
        private$number_of_times_chunked <- private$number_of_times_chunked + 1
        private$chunk_size <- min(self$max_chunk_size,
                                  private$chunk_size * private$number_of_times_chunked)
        self$data <- c(self$data, vector("list", private$chunk_size))
      }
      self$data[[private$next_index]] <- element
      private$next_index <- private$next_index + 1
      invisible(self)
    },

    #' @description Adds multiple elements to the ChunkedList.
    #' @method add_many ChunkedList
    #' @param list_of_elements A list of elements to add to the ChunkedList.
    #' @return ChunkedList with the added elements.
    #' @examples
    #' myChunkedList <- ChunkedList$new()
    #' myChunkedList$add_many(list(12, 13, 14))
    add_many = function(list_of_elements) {
      needed_space <- length(list_of_elements)
      while ((private$next_index + needed_space - 1) > length(self$data)) {
        # Increase data size more efficiently
        self$data <- c(self$data, vector("list", private$chunk_size))
      }
      self$data[private$next_index:(private$next_index + needed_space - 1)] <- list_of_elements
      private$next_index <- private$next_index + needed_space
    },

    #' @description Retrieves an element at a specified index.
    #' @method get_at_index ChunkedList
    #' @param index The index of the element to retrieve.
    #' @return The element at the specified index.
    #' @examples
    #' myChunkedList <- ChunkedList$new()
    #' myChunkedList$add(12)
    #' myChunkedList$get_at_index(1)
    get_at_index = function(index) {
      if (index >= private$next_index || index < 1) {
        stop("Index out of bounds")
      }
      return(self$data[[index]])
    },

    #' @description Returns the number of elements in the ChunkedList.
    #' @method size ChunkedList
    #' @return The number of elements in the ChunkedList.
    #' @examples
    #' myChunkedList <- ChunkedList$new()
    #' myChunkedList$size()
    size = function() {
      return(private$next_index - 1)
    },

    #' @description Returns the actual allocated size of the ChunkedList.
    #' @method actualSize ChunkedList
    #' @return The actual allocated size of the ChunkedList.
    #' @examples
    #' myChunkedList$actualSize()
    actualSize = function() {
      return(length(self$data))
    },

    #' @description Retrieves all non-NULL elements from the ChunkedList.
    #' @method get_all ChunkedList
    #' @return A list of all non-NULL elements in the ChunkedList.
    #' @examples
    #' myChunkedList <- ChunkedList$new()
    #' myChunkedList$get_all()
    get_all = function() {
      # Filter out NULL elements and return the list of non-NULL elements
      return(self$data[1:(private$next_index - 1)])
    },

    #' @description Prints the contents of the ChunkedList.
    #' @method print ChunkedList
    #' @return invisible(self)
    #' @examples
    #' myChunkedList <- ChunkedList$new()
    #' myChunkedList$print()
    print = function() {
      cat(
        "ChunkedList containing",
        self$size(),
        "items (actual allocated size:",
        self$actualSize(),
        "):\n"
      )
      print(self$get_all())
      invisible(self)
    },

    #' @description Provides an iterator for the ChunkedList to traverse elements.
    #' @method iterator ChunkedList
    #' @return An iterator object with `next` and `reset` functions.
    #' @examples
    #' myChunkedList <- ChunkedList$new()
    #' iterator <- myChunkedList$iterator()
    #' element <- iterator$get_next()
    #' while (!is.null(element)) {
    #'   print(element)
    #'   element <- iterator$get_next()
    #' }
    #' iterator$reset()
    iterator = function() {
      i <- 0
      #list(
        #' @description Retrieves the next element in the ChunkedList.
        #' @method next Iterator
        #' @examples
        #' element <- iterator$get_next()
        get_next = function() {
          i <<- i + 1
          if (i < private$next_index) {
            return(self$data[[i]])
          } else {
            return(NULL)  # Indicates end of the list
          }
        }

        #' @description Resets the iterator to the beginning of the ChunkedList.
        #' @method reset Iterator
        #' @examples
        #' iterator$reset()
        reset = function() {
          i <<- 0
          invisible(self)
        }
     return(list(get_next = get_next, reset = reset))
    },

    #' @description Compares the ChunkedList with another ChunkedList for equality.
    #' @method equals ChunkedList
    #' @param other Another ChunkedList object to compare against.
    #' @return TRUE if the two ChunkedLists are equal, FALSE otherwise.
    #' @examples
    #' myChunkedList <- ChunkedList$new()
    #' myChunkedList$add("Hello")
    #' otherChunkedList <- ChunkedList$new()
    #' otherChunkedList$add("Hello")
    #' myChunkedList$equals(otherChunkedList)
    equals = function(other) {
      if (!inherits(other, "ChunkedList")) {
        return(FALSE)
      }
      if (self$size() != other$size()) {
        return(FALSE)
      }

      for (i in 1:self$size()) {
        # Direct comparison for simple types
        if (is.atomic(self$data[[i]]) && is.atomic(other$data[[i]])) {
          if (self$data[[i]] != other$data[[i]]) {
            return(FALSE)
          }
        } else {
          # Use hashing for complex types or if direct comparison is not feasible
          digest_self <- digest::digest(self$data[[i]], algo = "xxhash32")
          digest_other <- digest::digest(other$data[[i]], algo = "xxhash32")
          if (digest_self != digest_other) {
            return(FALSE)
          }
        }
      }

      return(TRUE)
    }
  )
)





# Example usage
# myList <- ChunkedList$new(initial_size = 5, chunk_size = 5)
# myList$add("Hello")
# myList$add("World")
# iter <- myList$iterator()
#
# while(TRUE) {
#   element <- iter()
#   if (is.null(element)) break  # Exit loop if the iterator returns NULL (end of list)
#   print(element)
# }

# Example usage
# myList <- ChunkedList$new(initial_size = 5, chunk_size = 5)
# myList$add("Hello")
# myList$add("World")
# nonNullElements <- myList$getNonNullElements()
# print(nonNullElements)  # Should print a list with "Hello" and "World"
