#' Create new stack object
#'
#' @description
#' Create a new stack object with reference semantics and two methods,
#' `push` adds objects to the stack and `pop` removes them.
#'
#' @details
#' This is dependency-free class based on function closures with reference semantics.
#' Internally, the stack is represented as a pre-allocated `list` that is extended as required.
#' The size of the `list` is set to the `init`, and is dynamically extended by the `init` value.
#'
#' Following methods are available:
#'
#' * `push(...)` - element or elements to stack, elements are converted to list
#' * `pop(n)` - removes and returns `n` latest elements from stack, if `n` is missing, remove all elements
#'
#' Following slots are available:
#'
#' * items - stack memory, a pre-allocated
#' * size - current size of the stack
#'
#' Modifying these slots could lead to inconsistent behaviour.
#'
#' @param init an initial size of the stack
#' @return an environment containing callable methods, see details
#'
#' @examples
#' s = new_stack()
#' s$push("foo", "bar", "baz")
#'
#' identical(s$pop(1), list("foo"))
#' identical(s$pop(), list("bar", "baz"))
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
        size <<- size - n # no need to clean list
        items[size + seq_len(n)]
        }

    structure(environment(), "class" = "stack")
    }
