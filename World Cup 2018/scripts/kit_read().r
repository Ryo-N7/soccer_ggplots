kit_read <- function(path) {
  
  japan_kits <- list.files(path = path, pattern = "*.gif", full.names = TRUE) %>% 
    map(image_read) %>% 
    image_join()  
  
  return(japan_kits)
  
}