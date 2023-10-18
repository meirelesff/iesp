#' RDD extensions for tidy
#' 
#' Tidy methods for \code{rdd} objects created by the \code{rdrobust} package.
#' 
#' @param model An \code{rdd} object created by the \code{rdrobust} package.
#' @param ... Additional arguments passed to \code{modelsummary()}.
#' 
#' @examples
#' \dontrun{
#' x <- runif(100, -10, 10)
#' y <- 10 + 2*x + 2*(x>=0) + rnorm(100)
#' mod <- rdrobust(y,x)
#' 
#' tidy(mod)
#' glance(mod)
#' }
#' 
#' @export 

tidy.rdrobust <- function(model, ...) {
  res <- data.frame(
    term = row.names(model$coef)[3],
    estimate = model$coef[3],
    std.error = model$se[3],
    p.value = model$pv[3]
  )
  row.names(res) <- NULL
  res
}


#' RDD extensions for glance
#' 
#' Glance methods for \code{rdd} objects created by the \code{rdrobust} package.
#' 
#' @inheritParams tidy.rdrobust
#' 
#' @export 

glance.rdrobust <- function(model, ...) {
  res <- data.frame(
    Janela = round(model$bws[1], 1),
    N = sum(model$N_h)
  )
  res
}