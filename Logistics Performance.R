# The data is from shopee code league 2020

# Create a empty dataframe
late_delivery <- data.frame()

# Define the holidays and convert the strings to time format
holidays <- c('2020-03-08', '2020-03-25','2020-03-30','2020-03-31')
holidays <- as.POSIXct(as.character(holidays),origin="1970-01-01")

# Calculate how many delivery days of each order 
day_diff <- function(x, y) {
  # If there is no second attempt delivery, just return NA.
  if(!is.na(y)){
    # List out each date between pickup date and 1st delivery date
    day <- seq(x,y,by='days')
    # Convert the format of the date to the same format of holidays
    date <- format(day,"%Y-%m-%d")
    # If the days aren't Sunday and not holidays, count the day as a working day
    return(sum(!format(day, "%u") %in% "7" & !date %in% holidays)) 
  }else return(NA)
}

# The volume of the dataset is big, divide the dataset into multiple baches to manipulate easily.
for (i in seq(0,3176313,500000)){
  
  # Read the data
  data <- read.csv('/Users/chenlyn/Desktop/Logistics/delivery_orders_march.csv', skip = i, nrows=500000)
  # Set the names of columns
  colnames(data) <-c("orderid", "pick", "X1st_deliver_attempt", "X2nd_deliver_attempt", "buyeraddress", "selleraddress")
  
  # Convert the time format to readable time format 
  data[,'pick'] <- as.POSIXct(as.numeric(as.character(data[, 'pick'])),origin="1970-01-01")
  data[,'X1st_deliver_attempt'] <- as.POSIXct(as.numeric(as.character(data[, 'X1st_deliver_attempt'])),origin="1970-01-01")
  data[,'X2nd_deliver_attempt'] <- as.POSIXct(as.numeric(as.character(data[, 'X2nd_deliver_attempt'])),origin="1970-01-01")
  
  # Add a SLA column depending on the reference
  data[which(grepl('Visayas|Mindanao',data$buyeraddress,ignore.case=TRUE)),'SLA'] <- 7
  data[which(grepl('Visayas|Mindanao',data$selleraddress,ignore.case=TRUE)),'SLA'] <- 7
  data[which(grepl('Luzon',data$buyeraddress,ignore.case=TRUE) & (grepl('Metro Manila|Luzon',data$selleraddress,ignore.case=TRUE))),'SLA'] <- 5
  data[which(grepl('Metro Manila',data$buyeraddress,ignore.case=TRUE) & (grepl('Luzon',data$selleraddress,ignore.case=TRUE))),'SLA'] <- 5
  data[which(grepl('Metro Manila',data$buyeraddress,ignore.case=TRUE) & (grepl('Metro Manila',data$selleraddress,ignore.case=TRUE))),'SLA'] <- 3
  
  # Remove address information to manipulate the date more efficiently 
  data <- data[,c("orderid", "pick", "X1st_deliver_attempt", "X2nd_deliver_attempt", 'SLA')]
  
  # Calculate how many delivery days of each order
  data[,'Timediff'] <- mapply(day_diff,x= data$pick, y = data$X1st_deliver_attempt)
  data[,'2nd-Timediff'] <- mapply(day_diff,x= data$X1st_deliver_attempt, y = data$X2nd_deliver_attempt)
  
  # Decide if the order is late depending on SLA and less than 3 working days between 1st and 2nd attempt delivery dates
  data[,'is_late'] <- ifelse(data$Timediff<= data$SLA & (data$`2nd-Timediff`<= 3 | is.na(data$`2nd-Timediff`)),0,1)
  
  # Remain the desired column
  data <- data[,c('orderid','is_late')]
  
  # Combine the batches of data together
  late_delivery <- rbind(late_delivery, data)
}

# Export the data to csv format
write.csv(format(late_delivery,digits=20), '/Users/chenlyn/Desktop/Logistics/submission.csv', row.names = FALSE)
