#' @title historicalVI
#' @description take csv file path and gets all needed data for analysis
#' @param file csv filepath for data to be pulled
#' @return results for a given date of csv data
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[here]{here}}
#'  \code{\link[stringr]{str_sub}},\code{\link[stringr]{str_split}},\code{\link[stringr]{str_remove}},\code{\link[stringr]{str_locate}},\code{\link[stringr]{str_replace}}
#'  \code{\link[zoo]{yearmon}}
#'  \code{\link[utils]{read.table}}
#'  \code{\link[quantmod]{getSymbols}},\code{\link[quantmod]{OHLC.Transformations}}
#'  \code{\link[purrr]{map}},\code{\link[purrr]{reduce}},\code{\link[purrr]{reexports}}
#'  \code{\link[data.table]{transpose}}
#' @rdname historicalVI
#' @export 
#' @importFrom here here
#' @importFrom stringr str_sub str_split str_remove str_locate str_replace
#' @importFrom zoo as.Date
#' @importFrom utils read.csv
#' @importFrom quantmod getSymbols Ad
#' @importFrom purrr map reduce is_empty
#' @importFrom data.table transpose
#' @importFrom magrittr %>%
historicalVI <- function(file){
  
    checkSymbols <- function(syms){
      validSymbols <- read.csv(here::here("data-raw","ValidQuantModSymbols.csv"))
      vSymbols <- validSymbols[,2]
      validSymbols <- data.frame(vSymbols)
      
      testing <- data.frame(syms)
      
      validSym <- subset(validSymbols, vSymbols %in% testing$syms)
      colnames(validSym) <- "symbols"
      return(validSym)
    }
    
    epath <- stringr::str_sub(file,start=34)
    
    eps <- stringr::str_split(epath,"-",simplify = T)
    
    type <- eps[,3]
    dtype <- stringr::str_remove(type,".csv")
    
    
    loc <- stringr::str_locate(epath,"-")
    sloc <- loc[1]
    
    day <- stringr::str_sub(epath,end=sloc-1)
    
    day <- stringr::str_replace(day,"_","/")
    
    
    date <- zoo::as.Date(day,format="%m/%d")
    
    data <- utils::read.csv(file)
    
    data <- data.frame(Date=date,Chart=dtype,data)
    
    
    sym <- data$Symbol
    
    sym2 <- c(as.character(sym))
    
    bad_s <- stringr::str_locate(sym2,"\\.")
    
    bad <- bad_s[,2]
    good <- c(which(is.na(bad)))
    
    
    data[good,] -> original
    if(exists("checkSymbols")==TRUE){
      Symbols <- checkSymbols(sym2[good])
      symbols <- as.character(Symbols$symbols)  
      
    } else {
      symbols <- sym2[good]
    }
    
    
    f <- weekdays(data$Date[1]+7)
    if(f=="Saturday"){
      too <- data$Date[1]+7
    } else if(f=="Sunday"){
      too <- data$Date[1]+8
    } else {
      too <- data$Date[1]+7
    }
    
    prices <- quantmod::getSymbols(Symbols = symbols, src = 'yahoo',from = data$Date[1]-1,to=too, auto.assign = TRUE) %>%
      purrr::map(~quantmod::Ad(get(.))) %>%
      purrr::reduce(merge) %>%
      'colnames<-'(symbols)
    
    data.frame(prices) -> priced
    
    colnames(priced) -> rows
    rownames(priced) -> dates
    
    tr <- data.table::transpose(priced)
    
    prices2 <- do.call(cbind,tr)
    
    ## Move symbols from row names to own array
    rownames(prices2) -> symbol_col
    
    p3 <- matrix(as.numeric(unlist(prices2)), nrow=nrow(prices2))
    
    pricesd <- data.frame(Symbol=rows,p3)
    
    ## Determine number of days returned (will never be 7, always a weekend after 5)
    ndays <- ncol(pricesd)-1
    nday <- ndays+1
    
    
    
    colnames(pricesd)[c(2:nday)] <- dates
    
    
    ## Now, pricesd is V1 of final dataset
    
    
    
    original[order(original$Symbol),-3] -> orig_sorted
    pricesd[order(pricesd$Symbol),] -> pricesd_sorted
    
    
    merge(orig_sorted,pricesd_sorted,by = "Symbol") -> bothd
    bothd[order(bothd$Change),-8] -> compare
    
    stringr::str_remove(compare$Price,",") -> compare$Price
    
    as.numeric(as.character(compare$Change)) -> compare$Change
    as.numeric(as.character(compare$Price)) -> compare$Price
    
    colnames(compare)-> d2
    nd <- length(d2)
    
    e2 <- paste0("compare$'",d2[nd],"'","-compare$Price")
    e3 <- parse(text = e2)
    
    pre <- ifelse(compare$Change>0,compare$Price-compare$Change,compare$Price+abs(compare$Change))
    
    
    
    transform(compare,delta=eval(e3),Truth="",Direction="") -> compare
    data.frame(Pre=pre,compare) -> compare
    
    compare$Truth <- as.character(compare$Truth)
    compare$Direction <- as.character(compare$Direction)
    
    bad <- c(which(is.na(compare$delta)))
    if(purrr::is_empty(bad)==TRUE){
      print("No NA rows")
    } else {
      print("BAD")
      print(compare[bad,]$Symbol)
      compare <- compare[-c(bad),]
    }
    
    for(i in 1:length(compare$Symbol)){
      cur <- compare[i,]
      print(cur$Symbol)
      
      if(cur$Change<0 & cur$delta<0){
        compare$Truth[i] <- "True"
        compare$Direction[i] <- "Decrease"
      } else if(cur$Change>0 & cur$delta>0){
        compare$Truth[i] <- "True"
        compare$Direction[i] <- "Increase"
      } else if(cur$Change>0 & cur$delta<0){
        compare$Truth[i] <- "False"
        compare$Direction[i] <- "Increase"
      } else if(cur$Change<0 & cur$delta>0){
        compare$Truth[i] <- "False"
        compare$Direction[i] <- "Decrease"
      }
      
    }
 
  return(compare)
}
