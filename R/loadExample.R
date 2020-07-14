# Detect OS
get_os <- function() {
	if (.Platform$OS.type == "windows") {
		"win"
	} else if (Sys.info()["sysname"] == "Darwin") {
		"mac"
	} else if (.Platform$OS.type == "unix") {
		"unix"
	} else {
		stop("Unknown OS")
	}
}

loadExample <- function(){
	if( get_os() == "win" ) {
		path <- system.file("extdata/haddock-win", package = "gadgetr")
	} else {
		path <- system.file("extdata/haddock", package = "gadgetr")
	}
	print(path)
	setwd(path)
}
