#' @importFrom rstudioapi getActiveDocumentContext modifyRange
#' @importFrom utils capture.output
#' @import tidylog
internal <- function(x){
  if(!isNamespaceLoaded("tidylog")){
    attachNamespace('tidylog')  
  }
  sc <- rstudioapi::getActiveDocumentContext()
  full_sc <- sc$contents
  nr <- sc$selection[[1]]$range$end[[1]]
  flag <- TRUE
  i <- 0
  find_txt <- '(%>%)(|>)$'
  while(flag){
    i <- i + 1
    flag = grepl(find_txt,full_sc[nr-i])
  }
  subtext_rng <- (nr-i+1):nr
  subtext <- full_sc[subtext_rng]
  
  ret <- paste0(subtext, collapse = '')
  
  msgs <- utils::capture.output(eval(parse(text = gsub(find_txt, '', ret))), type = 'message')
  
  new_rng <- subtext_rng[grep('\\(',subtext)]
  new_txt  <- vector('character',length = length(new_rng)*2)
  j1 <- j2 <- 1
  for(j in seq(length(new_txt))){
    if(j%%2==1){
      new_txt[j] <- full_sc[new_rng[j1]]
      j1 <- j1 + 1
    }else{
      new_txt[j] <- paste0('# tidylog: ',msgs[j2])
      j2 <- j2 + 1
    }
  }
  
  rpl_txt <- paste0(new_txt,collapse = '\n')
  
  sel <- sc$selection[[1]]$range
  sel$start[[1]] <- subtext_rng[1] + 1
  sel$start[[2]] <- 1
  sel$end[[2]] <- Inf
  
  rstudioapi::modifyRange(location = sel, text = rpl_txt, id = sc$id)
}
