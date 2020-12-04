install.packages(c('repr', 'IRdisplay', 'evaluate', 'crayon', 'pbdZMQ', 'devtools', 'uuid', 'digest'))
devtools::install_github('IRkernel/IRkernel')

name = "ir"
displayname = "R"
rprofile = NULL
srcdir <- system.file("kernelspec", package = "IRkernel")
tmp_name <- tempfile()
dir.create(tmp_name)
file.copy(srcdir, tmp_name, recursive = TRUE)
spec_path <- file.path(tmp_name, "kernelspec", "kernel.json")
spec <- jsonlite::fromJSON(spec_path)
spec$argv[[1]] <- file.path(R.home("bin"), "R")
spec$display_name <- displayname
write(jsonlite::toJSON(spec, pretty = TRUE, auto_unbox = TRUE), file = spec_path)
user_flag <- "--user"
prefix_flag <-  character(0)

args <- c("kernelspec", "install", "--replace", "--name",
          name, user_flag, prefix_flag, file.path(tmp_name, "kernelspec"))

paste("jupyter", paste(args, collapse = " "))
