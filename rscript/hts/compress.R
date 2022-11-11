# compress all rds files
# Find all files
files <- fs::dir_ls(storage_folder, glob = "*.rds")
for (i in seq_along(files)) {
  tmp <- readr::read_rds(files[i])
  readr::write_rds(tmp, files[i], compress = "bz2")
}
