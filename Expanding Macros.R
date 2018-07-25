require(RCurl)
require(RJSONIO)
require(magrittr)
require(stringr)
require(dplyr)
options(expressions = 50000)

############################
# Import macro definitions #
############################
macros <- read.csv("/Users/yekun/qanalysis/Users/Yekun/expanding_macros/Macros.csv",header = TRUE)

#############
# Functions #
#############
# Parsing Macros in queries
macroQueryParser <- function(query){
  query <- str_trim(str_replace_all(query,"[(\")]"," "),"both")
  
  if(query==""){
    # Length of 0
    cleanQuery <- ""
    
  }else if(str_detect(query,",")){
    # Length greater than 1
    first <- substr(query,1,str_locate(query,",")-1)
    rest <- substr(query,str_locate(query,",")+1,str_length(query))
    cleanQuery <- paste0(macroQueryParser(first),",",macroQueryParser(rest))
    
  }else{
    # Length of 1
    if(str_detect(query," ")){
      first <- macroQueryParser(substr(query,1,str_locate(query," ")-1))
      first <- unlist(str_split(first,","))
      rest <- macroQueryParser(substr(query,str_locate(query," ")+1,str_length(query)))
      rest <- unlist(str_split(rest,","))
      cleanQuery <- str_replace_all(paste0(as.vector(sapply(first,function(x)sapply(rest,function(y)ifelse(str_detect(x,"\"")&!str_detect(y,"\""),paste(x,paste0(y,"\"")),paste(x,y))))),collapse=","),", ",",")
    }else{
      first <- query
      if(str_detect(first,"\\qf")){
        # First term contains qf macros
        
        if(str_detect(first,"\\qfnoun")){
          macroName <- substr(first,1,str_locate(first,"\\qf")-2)
          first <- paste0(macroName,",",paste0(macroName,"s",collapse = ""))
        }else if(str_detect(first,"\\qfverb")){
          macroName <- substr(first,1,str_locate(first,"\\qf")-2)
          first <- paste0(macroName,",",
                          paste0(macroName,"s",collapse = ""),",",
                          paste0(macroName,"ed",collapse = ""),",",
                          paste0(macroName,"ing",collapse = ""))
        }else if(str_detect(first,"\\qfand")){
          one <- substr(first,1,str_locate(first,"\\qf")-2)
          two <- substr(first,str_locate(first,"\\qf")+5,str_length(first))
          first <- paste0(paste0(one,"and",two),",",
                          paste0(one,"&",two),",",
                          paste0(one,"&amp;",two),",",
                          paste0(one,"n",two))
        }else{
          # Not the common "linguistic" macros
          macroName <- substr(first,str_locate(first,"\\qf")+2,str_length(first))
          macroDef <- str_replace_all(str_trim(str_replace_all(filter(macros,Macro==macroName)$Terms,"[(),]"," "),"both")," ",",")
          if(length(macroDef)==0){print(paste(macroName,"needs to be fixed"))}
          macroPrefix <- unlist(strsplit(first,paste0("\\\\qf",macroName,collapse = "")))
          first <- paste0(paste0(macroPrefix," ",unlist(strsplit(macroDef,","))),collapse = ",")
        }
      }
      cleanQuery <- first
    } 
  } 
  dupes <- unlist(str_split(str_replace_all(cleanQuery,"[(\")]",""),","))
  deduped <- paste(dupes[!duplicated(dupes)],collapse = ",")
  return(paste0("(",str_replace_all(deduped,"[()]",""),")"))
}

# Clean up macros in iterations
macroCleanUp <- function(query){
  
  cleanQuery <- macroQueryParser(query)
  cleanQuery <- str_replace_all(cleanQuery,"  "," ")
  cleanQuery <- macroQueryParser(cleanQuery)
  cleanQuery <- str_replace_all(cleanQuery,"  "," ")
  cleanQuery <- macroQueryParser(cleanQuery)
  cleanQuery <- str_replace_all(cleanQuery,"  "," ")
  # if(str_detect(cleanQuery,"\\qf")){
  #   cleanQuery <- macroQueryParser(macroQueryParser(cleanQuery))
  # }
  # if(str_detect(cleanQuery,"\\qf")){
  #   cleanQuery <- macroQueryParser(macroQueryParser(cleanQuery))
  # }
  # 
  
  return(cleanQuery)
  
}


##############
# Test Cases #
##############
# The function, macroCleanUp, takes in a list of keywords, separated by commas.
# Note that " and \ are special characters in R, so you need to first format 
# your query to escape these characters.

# Example 1
query = "\"hello\\qfnoun\""
macroCleanUp(query=query)

# Example 2
query="pininterest,pinterested,pinterest\\qfnoun,pinteresting,pintrest\\qfnoun"
macroCleanUp(query=query)
