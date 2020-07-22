#' @title Get the example haddock model directory 
#' @rdname loadExample
#' @export
loadExample <- function(){
 	path <- system.file("extdata/haddock", package = "gadgetr") 
	return(path)
}
