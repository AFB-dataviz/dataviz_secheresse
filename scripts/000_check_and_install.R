check_and_install <- function(pkgs) {
  pkgs2install <- pkgs[!sapply(pkgs, require, character.only = TRUE)]
  
  if(length(pkgs2install) > 0)
    install.packages(pkgs = pkgs2install)
}
