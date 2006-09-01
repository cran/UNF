.onLoad <- function(lib, pkg) {
  #if (!unfTest()) {
  #	cat("Warning unf: failed self-test\n")
  #}
  print(utils::citation("UNF"))
  return(TRUE)
}
