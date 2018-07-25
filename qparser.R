qparser <- function(query){
  
  require(stringr)
  
  boolparse <- function(query){
    
    if(!str_detect(query,"[:alpha:]")){
      return(NULL)
      break
    }
    
    splitted <- unlist(strsplit(query,""))
    
    
    if(splitted[1]=="("){
      # OR statement
      
      if(splitted[length(splitted)]==")"){
        inner <- paste0(splitted[2:(length(splitted)-1)],collapse ="")
        inner_splitted <- unlist(strsplit(inner,""))
        
        # if(inner_splitted[1]=="!"){
        #   iter <- paste0("(",inner_splitted[2:length(inner_splitted)],")",collapse="")
        #   rslt <- unlist(sapply(boolparse(iter),function(x)paste0("!{",x,"}",collapse="")))
        #   
        # }else 
          
        if(inner_splitted[1]=="("){
          iter <- paste0(inner_splitted[1:min(which(inner_splitted==")"))],collapse="")
          left <- paste0("(",paste0(na.omit(inner_splitted[(min(which(inner_splitted==")"))+2):length(inner_splitted)]),collapse=""),")")
          rslt <- c(boolparse(iter),boolparse(left))
          
        }else if(inner_splitted[1]=="["){
          iter <- paste0(inner_splitted[1:min(which(inner_splitted=="]"))],collapse="")
          left <- paste0("(",paste0(na.omit(inner_splitted[(min(which(inner_splitted=="]"))+2):length(inner_splitted)]),collapse=""),")")
          rslt <- c(boolparse(iter),boolparse(left))
          
        }else if(!("(" %in% inner_splitted) && !("[" %in% inner_splitted) ){
          rslt <- unlist(strsplit(inner,","))
          
        }else{
          if(inner_splitted[min(which(inner_splitted=="("|inner_splitted=="["))]=="("){
            iter <- paste0("(",paste0(inner_splitted[1:(min(which(inner_splitted=="("))-2)],collapse=""),")")
            left <- paste0("(",paste0(na.omit(inner_splitted[min(which(inner_splitted=="(")):length(inner_splitted)]),collapse=""),")")
            rslt <- c(boolparse(iter),boolparse(left))
          }else if(inner_splitted[min(which(inner_splitted=="("|inner_splitted=="["))]=="["){
            iter <- paste0("(",paste0(inner_splitted[1:(min(which(inner_splitted=="["))-2)],collapse=""),")")
            left <- paste0("(",paste0(na.omit(inner_splitted[min(which(inner_splitted=="[")):length(inner_splitted)]),collapse=""),")")
            rslt <- c(boolparse(iter),boolparse(left))
          }
        }
        
      }else{
        stop("missing closing parenthesis )")
      }
    }else if(splitted[1]=="["){
      # AND statement
      if(splitted[length(splitted)]=="]"){
        
        inner <- paste0(splitted[2:(length(splitted)-1)],collapse ="")
        inner_splitted <- unlist(strsplit(inner,""))
        
        # if(inner_splitted[1]=="!"){
        #   iter <- paste0("[",inner_splitted[2:length(inner_splitted)],"]",collapse="")
        #   rslt <- unlist(sapply(boolparse(iter),function(x)paste0("!{",x,"}",collapse="")))
        #   
        # }else 
        
        if(inner_splitted[1]=="("){
          iter <- paste0(inner_splitted[1:min(which(inner_splitted==")"))],collapse="")
          left <- paste0("[",paste0(na.omit(inner_splitted[(min(which(inner_splitted==")"))+2):(length(inner_splitted)+1)]),collapse=""),"]")
          piter <- boolparse(iter)
          pleft <- boolparse(left)
          rslt <- unlist(ifelse(is.null(pleft),list(piter),list(as.vector(sapply(piter,function(y)sapply(pleft,function(x)paste(y,x)))))))
          
        }else if(inner_splitted[1]=="["){
          iter <- paste0(inner_splitted[1:min(which(inner_splitted=="]"))],collapse="")
          left <- paste0("[",paste0(na.omit(inner_splitted[(min(which(inner_splitted=="]"))+2):(length(inner_splitted)+1)]),collapse=""),"]")
          rslt <- paste(boolparse(iter),boolparse(left),collapse=" ") 
          
        }else if(!("(" %in% inner_splitted) && !("[" %in% inner_splitted) ){
          rslt <- paste(unlist(strsplit(inner,",")),collapse=" ")
          
        }else{
          if(inner_splitted[min(which(inner_splitted=="("|inner_splitted=="["))]=="("){
            iter <- paste0("[",paste0(inner_splitted[1:(min(which(inner_splitted=="("))-2)],collapse=""),"]")
            left <- paste0("[",paste0(na.omit(inner_splitted[(min(which(inner_splitted=="("))):(length(inner_splitted)+1)]),collapse=""),"]")
            piter <- boolparse(iter)
            pleft <- boolparse(left)
            rslt <- unlist(lapply(boolparse(left),function(x)paste(boolparse(iter),x,sep=" ",collapse=" ")))
          }else if(inner_splitted[min(which(inner_splitted=="("|inner_splitted=="["))]=="["){
            iter <- paste0("[",paste0(inner_splitted[1:(min(which(inner_splitted=="["))-2)],collapse=""),"]")
            left <- paste0("[",paste0(na.omit(inner_splitted[(min(which(inner_splitted=="["))):(length(inner_splitted)+1)]),collapse=""),"]")
            rslt <- paste(boolparse(iter),boolparse(left),collapse=" ") 
          }
        }
        
      }else{
        stop("missing closing bracket ]")
      }
    }else{
      rslt <- paste0(splitted,collapse="")
    }
    return(rslt)
  }
  
  rslt_df <- as.data.frame(boolparse(query))
  colnames(rslt_df) <- ""
  return(rslt_df)
}

# Test cases
qparser("(a,b)")
qparser("(a,[b,c])")
qparser("(a,(b,c))")
qparser("(a,(b,c),[d,n,(e,l,m,n)],f,[g,(h,i)])")
qparser("([a,b],c)")
qparser("((a,b),c)")
qparser("[a,b]")
qparser("[a,(b,c)]")
qparser("[a,[b,c]]")
qparser("[[a,b],c]")
qparser("[(a,b),c]")
qparser("[[(a,b),c],(d.e)]")

# Print pretty outputs
query="[[(a,b),c],(d.e)]"
cat(paste0(as.character(qparser(query)[,1]),collapse=","))

