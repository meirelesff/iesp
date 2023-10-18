#' RDD extensions for tidy
#' 
#' These functions provide extensions to the \code{modelsummary()} function for
#' \code{rdd} objects created by the \code{rdrobust} package.
#' 
#' @param model An \code{rdd} object created by the \code{rdrobust} package.
#' @param ... Additional arguments passed to \code{modelsummary()}.
#' 
#' @export 

# Function to tidy the output of rdrobust
tidy.rdrobust <- function(model, ...) {
  ret <- data.frame(
    term = row.names(model$coef)[3],
    estimate = model$coef[3],
    std.error = model$se[3],
    p.value = model$pv[3]
  )
  row.names(ret) <- NULL
  ret
}


#' RDD extensions for glance
#' 
#' These functions provide extensions to the \code{modelsummary()} function for
#' \code{rdd} objects created by the \code{rdrobust} package.
#' 
#' @param model An \code{rdd} object created by the \code{rdrobust} package.
#' @param ... Additional arguments passed to \code{modelsummary()}.
#' 
#' @export 

glance.rdrobust <- function(model, ...) {
  ret <- data.frame(
    Janela = round(model$bws[1], 1),
    N = sum(model$N_h)
  )
  ret
}