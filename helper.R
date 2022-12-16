# Downloads a url if it doesn't already exist locally
get_if_needed <- function(
    # Url to be downloaded, necessary
  url, 
  
  # destination filename (optional)
  filename = NULL, 
  
  # destination directory (optional)
  destdir = "data"
) {
  
  # If `filename` parameter is not set, it defaults to online file name.
  if(is.null(filename)){
    the_filename <- url |> str_extract("[a-z A-Z 0-9 \\- ]+[.]{1,1}+[a-zA-Z]{1,4}$")
  } else {
    the_filename <- filename
  }
  
  # The `destdir` directory will be created if necessary
  if(!dir.exists(destdir)){
    dir.create(destdir)
  }
  
  filepath <- file.path(destdir, the_filename)
  
  if(!file.exists(filepath)) {
    download.file(url, destfile = filepath)
  }
}
