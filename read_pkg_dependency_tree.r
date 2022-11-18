#' @title Read Package Dependency Trees
#' @description Checks if two dataframes are equal.
#' @param df1 A dataframe
#' @param df2 The second dataframe
#' @return TRUE if the dataframes are equal else FALSE
#' @export

read_pkg_dependency_tree <- \(
  pack,
  dep_level = c("Depends", "Imports", "LinkingTo"), 
  available_packages = available.packages()
) {
  
  # source; https://gist.github.com/johnrc/faaa796e4b1ac53b7848
  # ex: read_pkg_dependency_tree("dplyr")
  
  packages = pack %>% 
    package_dependencies(available_packages, which = dep_level) %>% 
    unlist() %>% 
    unname()
  
  for(pkg in packages) {
    packages = c(packages, read_pkg_dependency_tree(pkg, dep_level, available_packages))
  }
  
  packages
  
}