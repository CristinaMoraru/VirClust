#set to "continue" mode
#rval_cont$data <- "yes"

#show status message for the project
log_name <- paste0(rval_proj_name$data, "/log.txt")
if(file.exists(log_name) == TRUE)
{
  logul <- read.csv(log_name, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
  status_mes <- logul[1,1]
  rm(logul)
}else
{status_mes <- "inexistent"}

rval_status_mes$data <- status_mes
rm(log_name, status_mes)


##set continue parameter
if(rval_status_mes$data == "created")
{
  rval_cont$data <- "no"
}

if(rval_status_mes$data == "finished")
{
  rval_cont$data <- "yes"
}

###load status_df from disk
status_p <- paste0(rval_proj_name$data, "/status.txt")
if(file.exists(status_p) == TRUE)
{
  status_df <- read.csv(file = status_p, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  rval_status_DF$data <- status_df
  rm(status_df)
}
rm(status_p)