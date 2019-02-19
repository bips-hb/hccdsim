#' @export
test <- function(x, exposure) {
  matrix(c(x,
           sapply(1:length(x), function(i)
             exposure(x[1:i]))),
         ncol = 2)
}
