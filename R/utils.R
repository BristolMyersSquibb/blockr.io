zchr_to_null <- function(x) {

  if (nzchar(x)) {
    return(x)
  }

  NULL
}
