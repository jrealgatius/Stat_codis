

# Funcio que converteix UTC data a date ymd
data_convert_UTC<-function(x){
  x<-format(as.POSIXct(x, origin='1970-01-01'), format='%Y/%m/%d')
  x<-lubridate::ymd(x)}