# Properly unload gadget shared lib
.onUnload <- function (libpath) {
  library.dynam.unload("gadgetr", libpath)
}
