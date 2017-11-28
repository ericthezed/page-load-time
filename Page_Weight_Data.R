# Load data
page_weight_dataset <- read.csv("page_weight_data.csv")
page_weight_data <- data.frame(page_weight_dataset)
page_weight_data_head <- head(page_weight_data, n = 10000L)

# Assign names to variables
# names(page_weight_data) <- c("nugget_id", "visitor_id", "session_id", "session_timestamp", "nugget_page_loaded_ms", "upworthy_loaded_ms", "facebook_loaded_ms", "youtube_loaded_ms", "new_visitor", "device_type", "bounced", "quick_exit_status")

# Remove unneeded columns
# page_weight_data <- subset(page_weight_data, select = -c(visitor_id, session_id, session_timestamp))

# Set variables to factors

# page_weight_data$new_visitor <- revalue(page_weight_data$new_visitor, c("t"="New", "f"="Returning"))
# new_visitor <- factor(page_weight_data$new_visitor, levels = c("New","Returning"), labels = c("New","Returning"))
# device_type <- factor(page_weight_data$device_type, levels = c("Mobile","Desktop", "Tablet"), labels = c("Mobile","Desktop", "Tablet"))
# quick_exit_status <- factor(page_weight_data$quick_exit_status, levels = c("exited_lt_10","exited_lt_30", "stayed_at_least_30"), labels = c("Exited_10","Exited_30", "Stayed_30"))

library("plyr")

# page_weight_data$bounced <- revalue(page_weight_data$bounced, c("no"=0, "yes"=1))
# page_weight_data$bounced <- factor(page_weight_data$bounced, levels = c(0,1), labels = c("No","Yes"))

# Create Binary Factors for Quick Exit at 10s and 30s

# page_weight_data$quick_exit_10 <- ifelse(page_weight_data$quick_exit_status == "exited_lt_10", c("exited_lt_10"), c("stayed_at_least_10"))
# page_weight_data$quick_exit_30 <- ifelse(page_weight_data$quick_exit_status == "stayed_at_least_30", c("stayed_at_least_30"), c("exited_lt_30"))
# page_weight_data$quick_exit_10 <- revalue(page_weight_data$quick_exit_10, c("stayed_at_least_10"=0, "exited_lt_10"=1))
# page_weight_data$quick_exit_30 <- revalue(page_weight_data$quick_exit_30, c("stayed_at_least_30"=0, "exited_lt_30"=1))
# page_weight_data$quick_exit_10 <- factor(page_weight_data$quick_exit_10, levels = c(0,1), labels = c("Stayed","Exited"))
# page_weight_data$quick_exit_30 <- factor(page_weight_data$quick_exit_30, levels = c(0,1), labels = c("Stayed","Exited"))


# Create factor labeling nuggets containing YT videos

# page_weight_data$yt_video <- mapvalues(page_weight_data$nugget_id, from = c("53ebaac46a2eb5afce000031", "550a0d356235630024880000", "5525c0b5646532002c380000", "5526b8eb646532002ce70000", "552bfff9626562002c840000"), to = c("y", "y", "y", "y", "n"))
# page_weight_data$yt_video <- factor(page_weight_data$yt_video, levels = c("y", "n"), labels = c("Yes", "No"))

# Create binary factor labeling nuggets where all applicable JS loads

# page_weight_data$load_all[!is.na(page_weight_data$nugget_page_loaded_ms) & !is.na(page_weight_data$upworthy_loaded_ms) & !is.na(page_weight_data$facebook_loaded_ms) & (!is.na(page_weight_data$youtube_loaded_ms) | page_weight_data$yt_video == "No")] <- "Load"
# page_weight_data$load_all[is.na(page_weight_data$load_all) ] <- "No_Load"
# page_weight_data$load_all <- factor(page_weight_data$load_all, levels = c("Load", "No_Load"), labels = c("Load", "No_Load"))

# Factor of load time for all applicable JS (or NA if not all relevant components load)

# page_weight_data$max_load_time[page_weight_data$load_all == "Load" & page_weight_data$yt_video == "Yes"] <- pmax(page_weight_data$nugget_page_loaded_ms, page_weight_data$upworthy_loaded_ms, page_weight_data$facebook_loaded_ms, page_weight_data$youtube_loaded_ms)[page_weight_data$load_all == "Load" & page_weight_data$yt_video == "Yes"]
# page_weight_data$max_load_time[page_weight_data$load_all == "Load" & page_weight_data$yt_video == "No"] <- pmax(page_weight_data$nugget_page_loaded_ms, page_weight_data$upworthy_loaded_ms, page_weight_data$facebook_loaded_ms)[page_weight_data$load_all == "Load" & page_weight_data$yt_video == "No"]
# page_weight_data$log_max_load_time <- log(page_weight_data$max_load_time)
# max_load_time_95quantile <- quantile(page_weight_data, c(0.95))
# page_weight_data$max_load_time_capped <- ifelse(page_weight_data$max_load_time > max_load_time_95quantile, c(max_load_time_95quantile), c(page_weight_data$max_load_time))

# Histograms of page load times
hist(page_weight_data$max_load_time)
hist(page_weight_data$log_max_load_time)
hist(page_weight_data$max_load_time_capped)

# Logit models predicting bounce rate
logit_bounce_int <- glm(bounced ~ log_max_load_time + device_type + new_visitor + log_max_load_time:device_type + log_max_load_time:new_visitor, family = "binomial", data = page_weight_data)
summary(logit_bounce_int)
logit_bounce <- glm(bounced ~ log_max_load_time + device_type + new_visitor, family = "binomial", data = page_weight_data)
summary(logit_bounce)
logit_bounce_load <- glm(bounced ~ load_all + device_type + new_visitor, family = "binomial", data = page_weight_data)
summary(logit_bounce_load)

# Logit models predicting bounce rate by device type 
logit_bounce_desktop <- glm(bounced ~ log_max_load_time + new_visitor, family = "binomial", data = subset(page_weight_data, device_type == "Desktop"))
summary(logit_bounce_desktop)
logit_bounce_mobile <- glm(bounced ~ log_max_load_time + new_visitor, family = "binomial", data = subset(page_weight_data, device_type == "Mobile"))
summary(logit_bounce_mobile)
logit_bounce_load_desktop <- glm(bounced ~ load_all + new_visitor, family = "binomial", data = subset(page_weight_data, device_type == "Desktop"))
summary(logit_bounce_load_desktop)
logit_bounce_load_mobile <- glm(bounced ~ load_all + new_visitor, family = "binomial", data = subset(page_weight_data, device_type == "Mobile"))
summary(logit_bounce_load_mobile)

# Logit models predicting quick exit rate (10s)
logit_quick10_int <- glm(quick_exit_10 ~ log_max_load_time + device_type + new_visitor + log_max_load_time:device_type + log_max_load_time:new_visitor, family = "binomial", data = page_weight_data)
summary(logit_quick10_int)
logit_quick10 <- glm(quick_exit_10 ~ log_max_load_time + device_type + new_visitor, family = "binomial", data = page_weight_data)
summary(logit_quick10)
logit_quick10_load <- glm(quick_exit_10 ~ load_all + device_type + new_visitor, family = "binomial", data = page_weight_data)
summary(logit_quick10_load)

# Logit models predicting quick exit rate (10s) by device type 
logit_quick10_desktop <- glm(quick_exit_10 ~ log_max_load_time + new_visitor, family = "binomial", data = subset(page_weight_data, device_type == "Desktop"))
summary(logit_quick10_desktop)
logit_quick10_mobile <- glm(quick_exit_10 ~ log_max_load_time + new_visitor, family = "binomial", data = subset(page_weight_data, device_type == "Mobile"))
summary(logit_quick10_mobile)
logit_quick10_load_desktop <- glm(quick_exit_10 ~ load_all + new_visitor, family = "binomial", data = subset(page_weight_data, device_type == "Desktop"))
summary(logit_quick10_load_desktop)
logit_quick10_load_mobile <- glm(quick_exit_10 ~ load_all + new_visitor, family = "binomial", data = subset(page_weight_data, device_type == "Mobile"))
summary(logit_quick10_load_mobile)

# Logit models predicting quick exit rate (30s)
logit_quick30_int <- glm(quick_exit_30 ~ log_max_load_time + device_type + new_visitor + log_max_load_time:device_type + log_max_load_time:new_visitor, family = "binomial", data = page_weight_data)
summary(logit_quick30_int)
logit_quick30 <- glm(quick_exit_30 ~ log_max_load_time + device_type + new_visitor, family = "binomial", data = page_weight_data)
summary(logit_quick30)
logit_quick30_load <- glm(quick_exit_30 ~ load_all + device_type + new_visitor, family = "binomial", data = page_weight_data)
summary(logit_quick30_load)

# Logit models predicting quick exit rate (30s) by device type 
logit_quick30_desktop <- glm(quick_exit_30 ~ log_max_load_time + new_visitor, family = "binomial", data = subset(page_weight_data, device_type == "Desktop"))
summary(logit_quick30_desktop)
logit_quick30_mobile <- glm(quick_exit_30 ~ log_max_load_time + new_visitor, family = "binomial", data = subset(page_weight_data, device_type == "Mobile"))
summary(logit_quick30_mobile)
logit_quick30_load_desktop <- glm(quick_exit_30 ~ load_all + new_visitor, family = "binomial", data = subset(page_weight_data, device_type == "Desktop"))
summary(logit_quick30_load_desktop)
logit_quick30_load_mobile <- glm(quick_exit_30 ~ load_all + new_visitor, family = "binomial", data = subset(page_weight_data, device_type == "Mobile"))
summary(logit_quick30_load_mobile)

# Logit models predicting bounce rates and quick exit rates from successful page loads (based on whether all applicable JS components load)
logit_bounce_load <- glm(bounced ~ load_all, family = "binomial", data = page_weight_data)
summary(logit_bounce_load)
logit_quick10_load <- glm(quick_exit_10 ~ load_all, family = "binomial", data = page_weight_data)
summary(logit_quick10_load)
logit_quick30_load <- glm(quick_exit_30 ~ load_all, family = "binomial", data = page_weight_data)
summary(logit_quick30_load)


library("gmodels")

# Crosstabs for Bounce Rate
CrossTable(page_weight_data$bounced[page_weight_data$load_all == "Load"], page_weight_data$device_type[page_weight_data$load_all == "Load"], chisq = TRUE)
CrossTable(page_weight_data$bounced[page_weight_data$load_all == "Load"], page_weight_data$new_visitor[page_weight_data$load_all == "Load"], chisq = TRUE)
CrossTable(page_weight_data$bounced, page_weight_data$load_all, chisq = TRUE)

# Crosstabs for Quick Exit (10s)
CrossTable(page_weight_data$quick_exit_10[page_weight_data$load_all == "Load"], page_weight_data$device_type[page_weight_data$load_all == "Load"], chisq = TRUE)
CrossTable(page_weight_data$quick_exit_10[page_weight_data$load_all == "Load"], page_weight_data$new_visitor[page_weight_data$load_all == "Load"], chisq = TRUE)
CrossTable(page_weight_data$quick_exit_10, page_weight_data$load_all, chisq = TRUE)

# Crosstabs for Quick Exit (30s)
CrossTable(page_weight_data$quick_exit_30[page_weight_data$load_all == "Load"], page_weight_data$device_type[page_weight_data$load_all == "Load"], chisq = TRUE)
CrossTable(page_weight_data$quick_exit_30[page_weight_data$load_all == "Load"], page_weight_data$new_visitor[page_weight_data$load_all == "Load"], chisq = TRUE)
CrossTable(page_weight_data$quick_exit_30, page_weight_data$load_all, chisq = TRUE)


# Plot logit models
library("ggplot2")
page_weight_data$bounced_num <- as.numeric(page_weight_data$bounced=="Yes")
ggplot(page_weight_data, aes(x=log_max_load_time, y=bounced_num)) + geom_point() + stat_smooth(method="glm", family="binomial", se=F, aes(y=bounced_num)) + facet_grid(new_visitor ~ device_type) + xlab("Page Load Time (log)") + ylab("Bounce Probability") + ggtitle("Page Load Time vs. Bounce Probability")

page_weight_data$quick_exit_10_num <- as.numeric(page_weight_data$quick_exit_10=="Exited")
ggplot(page_weight_data, aes(x=log_max_load_time, y=quick_exit_10_num)) + geom_point() + stat_smooth(method="glm", family="binomial", se=F, aes(y=quick_exit_10_num)) + facet_grid(new_visitor ~ device_type) + xlab("Page Load Time (log)") + ylab("Quick Exit (<10s) Probability") + ggtitle("Page Load Time vs. Quick Exit (<10s) Probability")

page_weight_data$quick_exit_30_num <- as.numeric(page_weight_data$quick_exit_30=="Exited")
ggplot(page_weight_data, aes(x=log_max_load_time, y=quick_exit_30_num)) + geom_point() + stat_smooth(method="glm", family="binomial", se=F, aes(y=quick_exit_30_num)) + facet_grid(new_visitor ~ device_type) + xlab("Page Load Time (log)") + ylab("Quick Exit (<30s) Probability") + ggtitle("Page Load Time vs. Quick Exit (<30s) Probability")

# ggplot(page_weight_data, aes(x=log_max_load_time, y=bounced)) + geom_point() + stat_smooth(geom = "smooth", method="glm", family="binomial", formula = page_weight_data$bounced ~ page_weight_data$log_max_load_time, se=FALSE, na.rm = TRUE) + facet_grid(new_visitor ~ device_type)
# ggplot(page_weight_data, aes(x=log_max_load_time, y=quick_exit_10)) + geom_point() + stat_smooth(geom = "smooth", method="glm", family="binomial", formula = page_weight_data$bounced ~ page_weight_data$log_max_load_time, se=FALSE, na.rm = TRUE) + facet_grid(new_visitor ~ device_type)
# ggplot(page_weight_data, aes(x=log_max_load_time, y=quick_exit_30)) + geom_point() + stat_smooth(geom = "smooth", method="glm", family="binomial", formula = page_weight_data$bounced ~ page_weight_data$log_max_load_time, se=FALSE, na.rm = TRUE) + facet_grid(new_visitor ~ device_type)
