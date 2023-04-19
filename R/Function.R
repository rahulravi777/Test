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

