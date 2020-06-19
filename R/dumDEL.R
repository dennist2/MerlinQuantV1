#' @title dumDEL
#' @description takes Historical() output and groups by Trues/Falses
#' @param x The df as input (can use lapply because it's just x)
#' @return transformed data.frame
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  dumDEL(LGP6122)
#'  }
#' }
#' @seealso 
#'  \code{\link[stringr]{str_replace}},\code{\link[stringr]{str_remove}}
#' @rdname dumDEL
#' @export 
#' @importFrom stringr str_replace_all str_remove
dumDEL <- function(x){
  data <- x
  print("Data Date:")
  print(data$Date[1])
  data$Date <- format(data$Date,"%m/%d")
  allF = data[c(which(data$Truth=="False")),]
  netF = sum(abs(allF$delta))
  
  allT = data[c(which(data$Truth=="True")),]
  netT = -1*(sum(abs(allT$delta)))
  
  NetProfit = netF+netT
  
  colnames(data) -> cols
  
  c(which(cols=="Go.to.")) -> pre_first
  
  cols[c(seq(pre_first+1,pre_first+3))] -> nar1
  
  stringr::str_replace_all(nar1,"\\.","-") -> nar
  stringr::str_remove(nar,"X") -> nar
  
  c(which(nar==as.character(data$Date[1]))) -> first_ind
  
  nar1[first_ind] -> first
  as.Date(nar[first_ind]) -> cfirst
  
  ## Delete above
  if(length(cols) <= 20){
    last <- cols[length(cols)-3]
  } else {
    cols[18] -> last
  }
  
  clast <- stringr::str_replace_all(last,"\\.","-")
  clast <- stringr::str_remove(clast,"X")
  clast <- as.Date(clast)
  clast <- format(clast,"%m/%d")
  
  # clast <- format(clast,format="%m/%d")
  
  paste("allF$'",first,"'",sep = "") -> d1_F
  d1F <- parse(text=d1_F)
  
  paste("allF$'",last,"'",sep = "") -> d2_F
  d2F <- parse(text=d2_F)
  
  paste("allT$'",first,"'",sep = "") -> d1_T
  d1T <- parse(text=d1_T)
  
  paste("allT$'",last,"'",sep = "") -> d2_T
  d2T <- parse(text=d2_T)
  
  # print(cfirst)
  # print(clast)
  
  print("Net Profit")
  print(NetProfit)
  print("                                                     ")
  
  
  output <- list(False=data.frame(Date=allF$Date,
                                  Symbol=allF$Symbol,
                                  Prior=round(allF$Pre,1),
                                  Price=round(allF$Price,1),
                                  Change=round(allF$Change,1),
                                  End=round(eval(d2F),1),
                                  Delta=round(allF$delta,2),
                                  Net=round(netF,1),
                                  Truth=allF$Truth,
                                  Dir=allF$Direction,
                                  EndDate=clast),
                 True=data.frame(Date=allT$Date,
                                 Symbol=allT$Symbol,
                                 Prior=round(allT$Pre,1),
                                 Price=round(allT$Price,1),
                                 Change=round(allT$Change,0),
                                 End=round(eval(d2T),1),
                                 Delta=round(allT$delta,2),
                                 Net=round(netT,1),
                                 Truth=allT$Truth,
                                 Dir=allT$Direction,
                                 EndDate=clast))
  output$False <- data.frame(output$False,type=ifelse(output$False$Change>0,"put","call"))
  output$True <- data.frame(output$True,type=ifelse(output$True$Change>0,"put","call"))
  out2 <- rbind(output$False,output$True)
  return(out2)
}
