install_packages <- \(pkg_vec){
  
  install.packages(
    pkgs, 
    dependencies = TRUE,
    repos = "https://packagemanager.rstudio.com/all/__linux__/focal/2022-10-06+Y3JhbiwyOjQ1MjYyMTU7MjdGNzBGNTE")
  )

  TRUE
  
}