library(stringr)
handles2query <- function(lhandles,hashtag=FALSE,plain_text=FALSE){

  handle_at <- str_replace_all(str_trim(lhandles,side="both"),"\n",",")
  handle_hash <- str_replace_all(handle_at,"@","#")
  handle_plain <- str_replace_all(handle_at,"@","")
  
  if(hashtag & plain_text){
    rslt <- gsub(",,",",",gsub("[[:space:]]", "", paste0("(",handle_at,",",handle_hash,",",handle_plain,")",collapse = ""))) 
  }else if(hashtag & !plain_text){
    rslt <- gsub(",,",",",gsub("[[:space:]]", "", paste0("(",handle_at,",",handle_hash,")",collapse = "")))
  }else if(!hashtag & plain_text){
    rslt <- gsub(",,",",",gsub("[[:space:]]", "", paste0("(",handle_at,",",handle_plain,")",collapse = ""))) 
  }else{
    rslt <- gsub(",,",",",gsub("[[:space:]]", "", paste0("(",handle_at,")",collapse = "")))     
  }
  return(rslt)
}