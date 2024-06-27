#Ordered Set Stack Collection####
OrderedSetStackCollection <- R6::R6Class(
  "SetStackCollection",
  private = list(
    .elements = list(), # List to store the elements in LIFO order
    .hashes = new.env(hash = TRUE) # Environment to maintain uniqueness
  ),
  public = list(
    add = function(object) {
      hash <- digest::digest(object, algo = "md5")
      if (!exists(hash, envir = private$.hashes)) {
        private$.elements[[length(private$.elements) + 1]] <- object
        assign(hash, TRUE, envir = private$.hashes)
      }
    },

    push = function(object) {
      self$add(object)
    },

    remove = function(object) {
      hash <- digest::digest(object, algo = "md5")
      if (exists(hash, envir = private$.hashes)) {
        idx <- which(sapply(private$.elements, function(x) digest::digest(x, algo = "md5")) == hash)
        if (length(idx) > 0) {
          private$.elements <- private$.elements[-idx]
          if (exists(hash, envir = private$.hashes)) { # Check if the hash exists before removing
            rm(list = hash, envir = private$.hashes)
          }
        }
      }
    },

    contains = function(object) {
      hash <- digest::digest(object, algo = "md5")
      return(exists(hash, envir = private$.hashes))
    },

    size = function() {
      return(length(private$.elements))
    },

    elements = function() {
      return(private$.elements)
    },

    add_many = function(elements) {
      # for(element in elements) {
      #   theElement = element
      #   self$add(element)
      # }
      vapply(elements, self$add, FUN.VALUE = logical(1))
    },

    pop = function() {
      if (self$size() == 0) {
        return(NULL)
      } else {
        element <- private$.elements[[length(private$.elements)]]
        hash <- digest::digest(element, algo = "md5")
        private$.elements <- private$.elements[-length(private$.elements)]
        if (exists(hash, envir = private$.hashes)) { # Check if the hash exists before removing
          rm(list = hash, envir = private$.hashes)
        }
        return(element)
      }
    },

    peek = function() {
      if (self$size() == 0) {
        return(NULL)
      } else {
        return(private$.elements[[length(private$.elements)]])
      }
    },

    is_empty = function() {
      return(self$size() == 0)
    }
  )
)
