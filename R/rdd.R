#' RDD extensions for tidy
#' 
#' Tidy methods for \code{rdd} objects created by the \code{rdrobust} package.
#' 
#' @param x An \code{rdd} object created by the \code{rdrobust} package.
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
#' @importFrom rdrobust rdrobust
#' @importFrom broom tidy
#' @export 

tidy.rdrobust <- function(x, ...) {
  res <- data.frame(
    term = row.names(x$coef)[3],
    estimate = x$coef[3],
    std.error = x$se[3],
    p.value = x$pv[3],
    conf.low  = x$ci[3, 1],
    conf.high = x$ci[3, 2]
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
#' @importFrom rdrobust rdrobust
#' @importFrom broom glance
#' @export 

glance.rdrobust <- function(x, ...) {
  res <- data.frame(
    Janela = round(x$bws[1], 1),
    N = sum(x$N_h),
    `N (controle)` = x$N_h[1],
    `N (tratamento)` = x$N_h[2],
    Polinômio = x$p
  )
  res
}