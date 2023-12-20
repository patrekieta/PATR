require(httr)
require(jsonlite)
require(dplyr)
require(tidyr)
require(lubridate)
require(reticulate)
require(rstudioapi)


get_QB_report <- function(QID = NULL, tableId = "", userToken = "", large_query = FALSE, hostname = ""){
  ### Warnings for blank parameters
  if(is.null(QID)){
    stop("QID can't be empty")
  }
  if(tableId==""){
    stop("tableID can't be empty")
  }
  if(userToken==""){
    stop("userToken can't be empty")
  }

  ### Wake-up ping
  timer=1
  statusCode <- 0
  while(statusCode!=200){
  report <- httr::POST(url = paste0("https://api.quickbase.com/v1/reports/",QID,"/run"),
                 httr::add_headers("Authorization" = paste("QB-USER-TOKEN", userToken),
                             "QB-Realm-Hostname" = hostname,
                             "Content-Type" = "Application/json") ,
                 query = list("tableId" = tableId
                              ,"skip" = "0", "top" = "1"
                              ))
  statusCode <- report$status_code
  timer=timer+1
  if(timer==6){
    report_text <- jsonlite::fromJSON(rawToChar(report$content))
    stop(paste0("API Timeout: Aborting Request - Status Code: ",statusCode, "\n", "Error: ", report_text$description))
  }
  }

  if(large_query==FALSE){
    report <- httr::POST(url = paste0("https://api.quickbase.com/v1/reports/",QID,"/run"),
                   httr::add_headers("Authorization" = paste("QB-USER-TOKEN", userToken),
                               "QB-Realm-Hostname" = hostname,
                               "Content-Type" = "Application/json") ,
                   query = list("tableId" = tableId))
    report_text <- jsonlite::fromJSON(rawToChar(report$content))
    final_report <- as.data.frame(report_text$data, stringsAsFactors = FALSE)
  } else{
          ### loop for large_report
          k=1
          while(k>0){
            if(k==1){
              i=0
            }
            report <- httr::POST(url = paste0("https://api.quickbase.com/v1/reports/",QID,"/run"),
                                 httr::add_headers("Authorization" = paste("QB-USER-TOKEN", userToken),
                                       "QB-Realm-Hostname" = hostname,
                                       "Content-Type" = "Application/json") ,
                           query = list("tableId" = tableId
                                        ,"skip" = i, "top" = "5000"
                           ))
            report_text <- jsonlite::fromJSON(rawToChar(report$content))
            records <- as.data.frame(report_text$data, stringsAsFactors = FALSE)
            if(i==0){
              final_report <- records
            } else {
              if(nrow(records)==0){
                # print("No More Records")
              }else{
                final_report <- suppressMessages(dplyr::full_join(records,final_report))
              }}
            i <- nrow(records) + i
            k <- nrow(records)
          }
        }

  ### IF NO DATA IS RETURNED
  if(nrow(final_report)==0){
    warning("No Records Returned")
    return(final_report)
    stop()
  }

  ###Convert columns to vectors
  for(i in 1:ncol(final_report)){
    final_report[,i] <- as.vector(final_report[,i])
  }

  ### Map Field Names
  for(i in 1:ncol(final_report)){
    colnames(final_report)[i] <- report_text$fields[which(report_text$fields$id == colnames(final_report)[i]),2]
  }


  ### Nested Column Check
  nested_cols <- NULL
  for(i in 1:ncol(final_report)){
    if(length(final_report[,i])<nrow(final_report)){
      col_name <- colnames(final_report)[i]
      nested_cols <- append(nested_cols,col_name)
    }
  }

  ### Fix nested columns
  for(i in 1:length(nested_cols)){
    final_report <- tidyr::unnest_wider(final_report, col = nested_cols[i],names_sep = ".")
  }


  return(final_report)
}

get_QB_query <- function(from = "", select = "",  where = "", userToken = "", large_query = FALSE, hostname = ""){
  ### Warnings for blank parameters
  if(from==""){
    stop("from can't be empty. Please provide the Table ID.")
  }
  if(select==""){
    stop("select can't be empty. Must provide at least one Field ID.")
  }
  if(userToken==""){
    stop("userToken can't be empty")
  }

  ### Create API Query JSON Body
  wakeup_body = paste('{"from": "',from,'","select": ',select,',"where": "',where,'","options": {"skip":0,"top":1}}'
                      , sep = "")


  ### Wake-up ping
  timer=1
  statusCode <- 0
  while(statusCode!=200){
    report <- httr::POST(url = paste0("https://api.quickbase.com/v1/records/query"),
                         httr::add_headers("Authorization" = paste("QB-USER-TOKEN", userToken),
                               "QB-Realm-Hostname" = hostname,
                               "Content-Type" = "Application/json") ,
                   body = wakeup_body
    )
    statusCode <- report$status_code
    timer=timer+1
    if(timer==6){
      report_text <- jsonlite::fromJSON(rawToChar(report$content))
      stop(paste0("API Timeout: Aborting Request - Status Code: ",statusCode, "\n", "Error: ", report_text$description))
    }
  }

  if(large_query==FALSE){
    body = paste('{"from":"',from,'","select":',select,',"where": "',where,'","options": {"skip":0}}'
                 , sep = "")

    report <- httr::POST(url = paste0("https://api.quickbase.com/v1/records/query"),
                         httr::add_headers("Authorization" = paste("QB-USER-TOKEN", userToken),
                               "QB-Realm-Hostname" = hostname,
                               "Content-Type" = "Application/json") ,
                   body = body
    )
    report_text <- jsonlite::fromJSON(rawToChar(report$content))
    final_report <- as.data.frame(report_text$data, stringsAsFactors = FALSE)
  } else{
    ### loop for large_report
    k=1
    while(k>0){
      if(k==1){
        i=0
      }
      body = paste('{ "from":"',from,'","select":',select,',"where":"',where,'","options": {"skip":',i,',"top":5000}}'
                   , sep = "")

      report <- httr::POST(url = paste0("https://api.quickbase.com/v1/records/query"),
                           httr::add_headers("Authorization" = paste("QB-USER-TOKEN", userToken),
                                 "QB-Realm-Hostname" = hostname,
                                 "Content-Type" = "Application/json") ,
                     body = body
      )
      report_text <- jsonlite::fromJSON(rawToChar(report$content))
      records <- as.data.frame(report_text$data, stringsAsFactors = FALSE)
      if(i==0){
        final_report <- records
      } else {
        if(nrow(records)==0){
          # print("No More Records")
        }else{
          final_report <- suppressMessages(dplyr::full_join(records,final_report))
        }}
      i <- nrow(records) + i
      k <- nrow(records)
    }
  }

if(nrow(final_report)==0){
  warning("No Records Returned")
  return(final_report)
  stop()
}

  ###Convert columns to vectors
  for(i in 1:ncol(final_report)){
    final_report[,i] <- as.vector(final_report[,i])
  }

  ### Map Field Names
  for(i in 1:ncol(final_report)){
    colnames(final_report)[i] <- report_text$fields[which(report_text$fields$id == colnames(final_report)[i]),2]
  }


  ### Nested Column Check
  nested_cols <- NULL
  for(i in 1:ncol(final_report)){
    if(length(final_report[,i])<nrow(final_report)){
        col_name <- colnames(final_report)[i]
        nested_cols <- append(nested_cols,col_name)
    }
  }

  ### Fix nested columns
  for(i in 1:length(nested_cols)){
  final_report <- tidyr::unnest_wider(final_report, col = nested_cols[i],names_sep = ".")
  }

  return(final_report)
}






calc_age <- function(date, age.day = today(), units = "years", floor = TRUE) {
  if(!base::is.Date(date)&!lubridate::is.POSIXct(date)&!lubridate::is.POSIXlt(date)){
    stop("date argument must be of class 'Date', 'POSIXct', or 'POSIXlt'")
  }

  calc.age = lubridate::interval(date, age.day) / lubridate::duration(num = 1, units = units)
  if (floor) return(base::as.integer(base::floor(calc.age)))
  return(calc.age)
}




remove_na <- function(x, na_replace = "", nan_replace = 0){
  if(!base::is.data.frame(x)){
    stop("x must be a data.frame")
  }

  numeric_col <- base::which(base::sapply(x, is.numeric))
  char_col <- base::which(base::sapply(x, is.character))
  y = x[,base::sapply(x, is.numeric)]
  z = x[,base::sapply(x,is.character)]

  y[base::is.na(y)] <- nan_replace
  z[base::is.na(z)] <- na_replace

  a <- base::cbind(y,z)

  a <- a[base::names(x)]

  return(a)
}




remove_col <- function(x, cols = c()){
  x <- x[,!base::colnames(x) %in% cols]
  return(base::as.data.frame(x))
}






write_sysenv <- function(){
if(! base::file.exists(base::file.path(base::normalizePath("~/"), ".Renviron"))){
  base::file.create(base::file.path(base::normalizePath("~/"), ".Renviron"))
}
utils::file.edit(base::file.path(base::normalizePath("~/"), ".Renviron") )
}



paste2 <- function(..., sep = " ", collapse = NULL, na.rm = F) {
  if (na.rm == F)
    paste(..., sep = sep, collapse = collapse)
  else
    if (na.rm == T) {
      paste.na <- function(x, sep) {
        x <- gsub("^\\s+|\\s+$", "", x)
        ret <- paste(na.omit(x), collapse = sep)
        is.na(ret) <- ret == ""
        return(ret)
      }
      df <- data.frame(..., stringsAsFactors = F)
      ret <- apply(df, 1, FUN = function(x) paste.na(x, sep))

      if (is.null(collapse))
        ret
      else {
        paste.na(ret, sep = collapse)
      }
    }
}



milli_time <- function(expr){
  start <- base::as.numeric(Sys.time())*1000
  expr
  end <- base::as.numeric(Sys.time())*1000
  return(end - start)
}




drive_letter <- function(name = ""){
  if(name == ""){
    base::stop("Drive Name can not be empty")
  }

drives <- base::system("wmic logicaldisk get DeviceID, VolumeName, ProviderName", intern = TRUE)
drive_ndx <- base::which(base::grepl(name, drives, ignore.case = T))
if(base::identical(drive_ndx,integer(0))){
  base::stop("No drive found with provided name")
}
drive_letter <- base::strsplit(drives[drive_ndx], " ")[[1]][1]

return(drive_letter)
}




get_QB_Fields <- function(tableId = "", includeFieldPerms = FALSE, userToken = "", hostname = ""){
  if(tableId==""){
    base::stop("tableId can not be empty!")
  }
  if(hostname==""){
    base::stop("hostname can not be empty!")
  }
  if(userToken==""){
    base::stop("userToken can't be empty")
  }

  timer=1
  statusCode <- 0
  while(statusCode!=200){
    report <- httr::GET(url = base::paste0("https://api.quickbase.com/v1/fields?tableId=",tableId,"&includeFieldPerms=",as.character(tolower(includeFieldPerms))),
                         httr::add_headers("Authorization" = paste("QB-USER-TOKEN", userToken),
                                           "QB-Realm-Hostname" = hostname,
                                           "Content-Type" = "Application/json")
                         )
    statusCode <- report$status_code
    timer=timer+1
    if(timer==6){
      report_text <- jsonlite::fromJSON(base::rawToChar(report$content))
      stop(base::paste0("API Timeout: Aborting Request - Status Code: ",statusCode, "\n", "Error: ", report_text$description))
    }
  }
  report_text <- jsonlite::fromJSON(base::rawToChar(report$content))
  if(base::class(report_text)!="data.frame"){
    base::stop("Data not returned as Data.Frame")
  }
  return(report_text)
}



POST_QB_Data <- function(x, tableId = "", userToken = "", hostname = ""){

  if(base::class(x)!="data.frame"){
    base::stop("parameter 'x' must be a dataframe")
  }
  if(tableId==""){
    base::stop("tableId can not be empty")
  }
  if(userToken == ""){
    base::stop("Must provide a userToken")
  }
  if(hostname == ""){
    base::stop("Must provide a hostname")
  }

  field_ids <- DSTR::get_QB_Fields(tableId = tableId, userToken = userToken, hostname = hostname)
  for(i in 1:ncol(x)){
    col_ndx <- base::which(colnames(x)[i]==field_ids$label)
    base::colnames(x)[i] <- field_ids$id[col_ndx]
  }

  final_json <- DSTR::QB_toJSON(x = x, tableId = tableId)

  report <- httr::POST(url = base::paste0("https://api.quickbase.com/v1/records"),
                      httr::add_headers("Authorization" = paste("QB-USER-TOKEN", userToken),
                                        "QB-Realm-Hostname" = hostname),
                      body = final_json
  )
  report_text <- jsonlite::fromJSON(base::rawToChar(report$content))

  if(httr::status_code(report)==200) {
    return(base::cat(base::paste0("Success! \n Total Number of Records Processed: ", report_text$metadata$totalNumberOfRecordsProcessed)))
  } else {
    return(base::cat(base::paste0("FAILURE! \n STATUS CODE: ",report$status_code, "\n ERROR: ", paste(report_text$metadata$lineErrors, collapse = ","))))
  }

}




QB_toJSON <- function(x, tableId=""){

  x[base::is.na(x)] <- ""
  start_json <- base::paste('{"to":"',tableId,'","data": [',sep = "")


  for(i in 1:ncol(x)){
    x[,i] <- paste('"',base::colnames(x)[i],'"',': {"value": '
                   ,if(all(base::class(x[,i])=="numeric")){x[,i]} else { if(all(base::class(x[,i])=="logical")) {base::tolower(base::as.character(x[,i]))} else { if(any(base::class(x[,i])=="POSIXt")){paste('"',gsub(" ", "T",base::round(x[,i])),'"',sep = "")} else {base::paste('"',x[,i],'"',sep = "")}}}
                   ,'}'
                   ,sep = "")
  }


  json_data <- x |> tidyr::unite( col = "comb", sep = ",")

  json_data$comb <- base::paste('{',json_data$comb,'}',sep = "")

  json_data <- base::paste(json_data$comb, collapse = ",")

  all_data <- base::paste(start_json,json_data,']}',sep = "")
  if(grepl("\t|\n|\r|\a|\b|\f|\v", all_data)){
    all_data <- gsub("\r|\a|\b|\f|\v","",all_data)
    all_data <- gsub("\n","<br>",all_data)
    all_data <- gsub("\t","    ",all_data)
  }

  return(all_data)
}



QB_DELETE <- function(x, tableId="", userToken = "",hostname = "", recordID = TRUE, where = ""){
  if(!exists("x")&recordID == TRUE){
    stop("Must supply a value for x when recordID is TRUE")
  }
  if(class(x)!="numeric"&recordID==TRUE){
    stop("x must be a single numeric or numeric vector")
  }
  if(recordID==FALSE & where==""){
    stop("where can not be empty when recordID is FALSE")
  }
  if(hostname == ""){
    stop("hostname can not be empty!")
  }
  if(tableId == ""){
    stop("tableId can not be empty!")
  }
  if(userToken == ""){
    stop("tableId can not be empty!")
  }

  doubleCheck <- utils::menu(c("Yes","No"), title = "Are you sure you wish to proceed with deletion?")
if(doubleCheck == 1L){
  if(recordID==TRUE){
    log <- data.frame("RecordIDs" = x)
    log$Deleted <- NA
  for(i in 1:length(x)) {
    body = paste("{\"from\":\"",tableId,"\",\"where\": \"{3.EX.",x[i],"}\"}", sep = "")
    report <- httr::DELETE(url = paste0("https://api.quickbase.com/v1/records"),
                           httr::add_headers(Authorization = paste("QB-USER-TOKEN",
                                                                   userToken), "QB-Realm-Hostname" = hostname), body = body)
    report_text <- jsonlite::fromJSON(rawToChar(report$content))
    if(report$status_code!=200L){
      stop(paste0("Error Code: ",report$status_code, "\n Error Statement: ", paste(report_text, collapse = ";")))
    }
    log$Deleted[i] <- ifelse(report_text$numberDeleted==1, yes = TRUE, no = FALSE)
  }
    return(log)
  } else {
    body = paste("{\"from\":\"",tableId,"\",\"where\": \"",where,"\"}", sep = "")
    report <- httr::DELETE(url = paste0("https://api.quickbase.com/v1/records"),
                           httr::add_headers(Authorization = paste("QB-USER-TOKEN",
                                                                   userToken), "QB-Realm-Hostname" = hostname), body = body)
    report_text <- jsonlite::fromJSON(rawToChar(report$content))
    report_text$status_code <- report$status_code
    if(report$status_code!=200L){
      stop(paste0("Error Code: ",report$status_code, "\n Error Statement: ", paste(report_text, collapse = ";")))
    }
    return(report_text)
  }
} else {
  stop("User aborted operation")
}
}

