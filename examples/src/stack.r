#' Create new stack object
#'
#' Create a new stack object with reference semantics.
#'
#' This is dependency-free class based on function closures with reference semantics.
#' Internally, the stack is represented as a pre-allocated `list` that is extended as required.
#' The size of the `list` is set to the `init`, and is dynamically extended by the `init` value.
#'
#' ## Methods
#'
#' * `push(...)` - element or elements to stack, elements are converted to list
#' * `pop(n)` - removes and returns `n` latest elements from stack, if `n` is missing, remove all elements
#' * peek(n) - returns `n` latest elements from stack, if `n` is missing, return all elements
#'
#' Modifying internal slots could lead to inconsistent behaviour.
#'
#' @param init an initial size of the stack
#' @return an environment containing callable methods, see details
#'
#' @examples
#' s = new_stack()
#' s$push("foo", "bar", "baz")
#'
#' identical(s$peek(), list("foo", "bar", "baz"))
#' identical(s$peek(1), list("baz")
#' identical(s$pop(1), list("baz"))
#' identical(s$pop(), list("foo", "bar"))
#' identical(s$pop(), list())
#'
#' @export
new_stack = function(init = 20L){
    items = vector("list", init)
    size = 0L

    push = function(...){
        new = list(...)
        new_size = length(new) + size
        while(new_size > length(items))
            items[[length(items) + init]] = list(NULL)

        items[size + seq_along(new)] <<- new
        size <<- new_size

        invisible(NULL)
        }

    pop = function(n = NULL){
        if(is.null(n)) n = size
        if(n > size) n = size

        size <<- size - n # no need to clean list
        items[size + seq_len(n)]
        }


    peek = function(n = NULL){
        if(is.null(n)) n = size
        if(n > size) n = size

        items[size - n + seq_len(n)]
        }

    structure(environment(), "class" = "stack")
    }
