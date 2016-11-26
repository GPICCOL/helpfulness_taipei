df <- read.csv("TripAdvisor.csv", stringsAsFactors = FALSE)
dim(df)
names(df)
df[ , c("mobile", "stay_type", "hotel")] <- lapply(df[ , c("mobile", "stay_type", "hotel")], as.factor)
df[ , c("stay_date", "review_date", "response_date")] <- lapply(df[ , c("stay_date", "review_date", "response_date")], as.Date, format = "%Y-%m-%d")
summary(df)
sapply(df[, c("value", "rooms", "sleep_quality", "location",  "cleanliness", "service_r")], table)
tr <- function(x) {
  is.na(x[which(x == 0)]) <- TRUE
  return(x)
}

lapply(df[ , c("value", "rooms", "sleep_quality", "location",  "cleanliness", "service_r")], tr)
       
       
is.na(review_2012$ratings.overall[review_2012$ratings.overall == 0]) <- TRUE
nreviews <- xtabs(~date + offering_id, data = usedata)
nreviews <- apply(nreviews, 2, function(x) rev(cumsum(rev(x))))
review_2012$nreviews <- nreviews[cbind(as.character(review_2012$date), review_2012$offering_id)]
O <- with(usedata, order(offering_id, date, id))
odata <- usedata[O, c("offering_id", "date", "id")]
endday <- as.Date("2012-12-21") 
odata$firstpage <- unlist(with(odata, tapply(date, offering_id, 
                                             function(x) if (length(x) > 10) c(x[-c(1:10)], rep(endday, 10)) - x else endday - x)))
odata$untilend <- unlist(with(odata, tapply(date, offering_id, 
                                            function(x) if (length(x) > 10) rep(0:1, c(length(x) - 10, 10)) else rep(1, length(x)))))				     
review_2012 <- merge(review_2012, odata[, c("id", "firstpage", "untilend")], by = "id", all.x = TRUE)