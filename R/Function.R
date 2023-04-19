#' Pearson correlation coefficient
#'
#' @param x is vector of variables
#' @param y is vector of variables
#'
#' @return a numerical value of length 1
#' @export
#'
#' @examples
#' Pear_corr(x,y)

Pear_corr <-   function(x, y) {

  Xm   = mean(x)
  Ym   = mean(y)
  Dxy  = sum((x-Xm)*(y-Ym))
  sqdx = sqrt(sum((x-Xm)^2) * sum((y-Ym)^2))
  r = Dxy/sqdx
  if(r > 0.7){
    print("strongly correlated")
  } else {
    print("Not strongly correlated")
  }
  return(r)
}

