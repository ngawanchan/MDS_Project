#loading packages
library(readxl)
library(activity)
library(tidyverse)
install.packages("lubridate")
library(lubridate)
install.packages("circular")
library(circular)
library(dplyr)
install.packages("openxlsx")
library(openxlsx)
install.packages("overlap")
library(overlap)
install.packages("gridExtra")  
library(gridExtra)

# Read data
dat <- read.csv("Downloads/Animal (8).csv", stringsAsFactors = F)
sum(is.na(dat))
nrow(dat)
dat$taken <- as.POSIXct(dat$taken_timestamp, format = "%Y-%m-%d %H:%M:%S")
summary(dat)
head(dat)

# Generate summary data and save it as Excel
sum_data <- summary(dat)
# Convert to a data frame
sum_data_df <- as.data.frame(as.table(sum_data))
# Write to Excel
write.xlsx(sum_data_df, "summary_data.xlsx")
system("open summary_data.xlsx")

# Filter out qualified data
dat1 <- dat %>% 
  filter(
    taken_timestamp != 0,
    year(taken) >= 2013, 
    latitude >= 49 & latitude <= 59, # British latitude range
    longitude >= -7 & longitude <= 2, # British longitude range
    Species != "0",
    !is.na(taken) 
  )
nrow(dat1)

# Filter sequences based on the time difference > 30 mins
dat2 <- dat1 %>% 
  mutate(diff = as.numeric(difftime(taken, lag(taken, default = first(taken)), units = "mins"))) %>% 
  filter(is.na(diff) | diff >= 30)
nrow(dat2)
summary(dat2)

# Generate summary data and save it as Excel
sum_data1 <- summary(dat2)
# Convert to a data frame
sum_data_df1 <- as.data.frame(as.table(sum_data1))
# Write to Excel
write.xlsx(sum_data_df1, "summary_data1.xlsx")
system("open summary_data1.xlsx")

# Total camera sites 
# Convert timestamp to date
dat2$date_only <- as.Date(dat2$taken)
total_camera_sites_all <- dat2 %>% 
  distinct(latitude, longitude) %>% 
  nrow()
print(total_camera_sites_all)
# Calculate cumulative camera days
cumulative_camera_days <- dat2 %>%
  distinct(latitude, longitude, date_only) %>%
  nrow()
print(cumulative_camera_days)

#total camera sites for otter
otters <- dat2 %>% filter(Species == "Otter")
nrow(otters) # otters' total sequences
total_camera_sites_otter <- otters %>% 
  distinct(latitude, longitude) %>% 
  nrow()
print(total_camera_sites_otter)
# cumulative camera days for otter
cumulative_camera_days <- otters %>%
  distinct(latitude, longitude, date_only) %>%
  nrow()
print(cumulative_camera_days)



#### 1 
###
# Convert the photos taken time
dat2$taken <- as.POSIXct(dat2$taken_timestamp, format = "%Y-%m-%d %H:%M:%S")
dat2$time <- format(dat2$taken, "%H:%M:%S")
dat2$radtime <- gettime(dat2$taken, scale="radian")
dat2$solartime <- solartime(dat2$taken, lat=dat2$latitude, lon=dat2$longitude, tz=0)$solar
head(dat2)

# otters' daily activity rhythms based on solartime
set.seed(12345)
mod_otter <- fitact(otters$solartime, sample = "model", reps = 5)
#visulation
otdat <- as.data.frame(mod_otter@pdf)
f0 <- ggplot(otdat) + geom_ribbon(aes(x=x,ymin=lcl,ymax=ucl), fill = "red", alpha = 0.3) +
  theme_classic() +geom_vline(xintercept = seq(0,2*pi-0.01,pi/2), col = "grey70") +
  scale_x_continuous(breaks = seq(0,2*pi-0.01,pi/2), labels = seq(0,23,6)) +
  geom_hline(yintercept = seq(0,0.4,0.1), col = "grey70") +
  scale_y_continuous(breaks = seq(0,0.4,0.1)) +
  xlab("Hour of day") + ylab("Relative activity")
  
f0
f0 <- f0 + coord_polar()

# compare daily activity rhythms based on solartime vs standard time
mod_otter2 <- fitact(otters$radtime, sample = "model", reps = 5)
otdat2 <- as.data.frame(mod_otter2@pdf)
otdat$type <- "solar"
otdat2$type <- "standard"
otdat <- rbind(otdat,otdat2)
#visulation
f <- ggplot(otdat) + geom_ribbon(aes(x=x,ymin=lcl,ymax=ucl, fill = type), alpha = 0.3) +
  theme_classic() +geom_vline(xintercept = seq(0,2*pi-0.01,pi/2), col = "grey70") +
  scale_x_continuous(breaks = seq(0,2*pi-0.01,pi/2), labels = seq(0,23,6)) +
  geom_hline(yintercept = seq(0,0.4,0.1), col = "grey70") +
  scale_y_continuous(breaks = seq(0,0.4,0.1)) +
  xlab("Hour of day") + ylab("Relative activity")

f
f <- f + coord_polar()
f
# p-value test
comp<-compareCkern(mod_otter,mod_otter2, reps = 1000)
comp

# Day of year activity rhythms based on solartime
dat2$solartime_day <- (dat2$solartime - min(dat2$solartime)) * (2 * pi / (max(dat2$solartime) - min(dat2$solartime)))
otters_day <- dat2 %>% filter(Species == "Otter")
mod_otter_day <- fitact(otters_day$solartime_day, sample = "model", reps = 5)
#visulation
f_day <- as.data.frame(mod_otter_day@pdf)
f_day <- ggplot(f_day) + 
  geom_ribbon(aes(x=x, ymin=lcl, ymax=ucl), fill = "red", alpha = 0.3) +
  theme_classic() +
  geom_vline(xintercept = seq(0, 2*pi, pi/2), col = "grey70") +
  scale_x_continuous(breaks = seq(0, 2*pi, pi/2), labels = seq(0, 365, 91.25)) +
  geom_hline(yintercept = seq(0, 0.4, 0.1), col = "grey70") +
  scale_y_continuous(breaks = seq(0, 0.4, 0.1)) +
  xlab("Day of Year") + 
  ylab("Relative activity")+
  annotate("text", x = 1, y = 0.5, label = "Otter", size=4)

f_day <- f_day + coord_polar()
grid.arrange(f0, f_day, nrow=2)


# seasonal activity rhythms 
dat2$month <- month(dat2$taken)
dat2$season <- case_when(
  dat2$month %in% c(3,4,5) ~ "Spring",
  dat2$month %in% c(6,7,8) ~ "Summer",
  dat2$month %in% c(9,10,11) ~ "Autumn",
  TRUE ~ "Winter"
)
head(dat2)
# standard vs solar
set.seed(12345)
results <- list()
for(season in unique(dat2$season)){
  sub_data <- otters %>% filter(season == !!season)
  mod_standard <- fitact(sub_data$radtime, sample = "model", reps = 5)
  mod_solar <- fitact(sub_data$solartime, sample = "model", reps = 5)
  
  otdat_standard <- as.data.frame(mod_standard@pdf)
  otdat_standard$type <- "standard"
  otdat_standard$season <- season
  otdat_solar <- as.data.frame(mod_solar@pdf)
  otdat_solar$type <- "solar"
  otdat_solar$season <- season
  
  results[[season]] <- rbind(otdat_standard, otdat_solar)
}
# combine the results of 4 seasons 
final_data <- bind_rows(results)
#visulation
f <- ggplot(final_data, aes(x=x, ymin=lcl, ymax=ucl, fill=type)) +
  geom_ribbon(alpha = 0.3) +
  facet_wrap(~ season) +  
  theme_classic() +
  geom_vline(xintercept = seq(0,2*pi-0.01,pi/2), col = "grey70") +
  scale_x_continuous(breaks = seq(0,2*pi-0.01,pi/2), labels = seq(0,23,6)) +
  geom_hline(yintercept = seq(0,0.4,0.1), col = "grey70") +
  scale_y_continuous(breaks = seq(0,0.4,0.1)) +
  xlab("Hour of day") + ylab("Relative activity")
f
f <- f + coord_polar()
f

# total observations in each season
par(mfrow=c(1,1))
otters <- dat2 %>% filter(Species == "Otter")
season_counts <- table(otters$season)
head(season_counts)
sum(is.na(season_counts))
barplot(season_counts, main="Number of Observations per Season", horiz=TRUE, las=1, 
        ylab="Season", xlab="Number of Observations", 
        col=c("blue", "green", "yellow", "orange"), cex.names=0.7)

# Use the chi-square test to assess whether the difference in the number of observations between seasons is significant
seasons <- unique(otters$season)
head(seasons)
comparisons <- combn(seasons, 2)  
for(i in 1:ncol(comparisons)) {
  cat("Comparing", comparisons[1, i], "and", comparisons[2, i], "\n")
  
  observed <- season_counts[comparisons[, i]]
  chisq_test <- chisq.test(observed)
  
  print(chisq_test)
  cat("\n")
}



#### 2
###
#interspecies comparsion
#first, choose top 10 sympatric Species
# Create an empty data frame to store unique information about same camera traps
same_camera_traps <- data.frame()

# A list to store checked camera trap locations
checked_locations <- data.frame(latitude = numeric(), longitude = numeric())

# Iterate through each observation of the otter
for(i in 1:nrow(dat2)) {
  if (dat2$Species[i] == "Otter") {
    
    # Extract the latitude and longitude of the current otter observation
    otter_latitude <- dat2$latitude[i]
    otter_longitude <- dat2$longitude[i]
    
    # Filter camera traps with the exact same latitude and longitude
    same_traps <- dat2 %>%
      filter(longitude == otter_longitude, 
             latitude == otter_latitude) %>%
      anti_join(checked_locations, by = c("latitude", "longitude")) 
    
    # Append the same camera traps information to the data frame
    same_camera_traps <- rbind(same_camera_traps, same_traps)
    
    # Add these trap locations to the checked list
    checked_locations <- rbind(checked_locations, distinct(same_traps, latitude, longitude))
  }
}

# Calculate the total counts of each species in the entire dataset
total_counts <- dat2 %>%
  group_by(Species) %>%
  summarise(total_n = n())

# Calculate the counts of each species in same camera traps
same_counts <- same_camera_traps %>%
  group_by(Species) %>%
  summarise(same_n = n()) %>%
  filter(same_n >= 50)  # Only include species observed in the same camera traps as otters more than 50 times

# Join the two datasets to compute the proportions
proportions <- inner_join(total_counts, same_counts, by = "Species") %>%
  mutate(proportion = (same_n / total_n) * 100) %>%
  arrange(-proportion)  
proportions

# Create a new column that combines species name, counts, and proportions
proportions <- proportions %>%
  mutate(label = paste(Species, " (", same_n, " times, ", round(proportion, 2), "%)"))

# Choose top 10
top_ten <- head(proportions, 10)

# visualization
par(mfrow=c(2,1))
barplot(top_ten$proportion, 
        names.arg = top_ten$label,
        main="Top 10 Animals Coexisting with Otters by Proportion", 
        ylab="Proportion (%)", 
        las=2,
        cex.axis=0.7  
)


# Comparison of activity patterns between otters and other sympatric species
dat2$takentime <- as.POSIXct(dat2$taken, format="%Y-%m-%d %H:%M:%S")
dat2$hour <- as.numeric(format(dat2$taken, "%H")) + 
  as.numeric(format(dat2$taken, "%M")) / 60
dat2$Hour <- dat2$hour * (2 * pi) / 24
#otter activity
par(mfrow=c(1,1))
otter_act <- dat2$solartime[dat2$Species == "Otter"]
densityPlot(otter_act, rug=T, adjust=1, xlab="Time",
            ylab="Density", lwd=1, main="", xlim=c(0, 24), ylim=c(0,0.12))
legend("topright", c("otter"), col="black", lwd=1, bty="n")

# Moorhen (Common) activity
Moorhen_act <- dat2$solartime[dat2$Species == "Moorhen (Common)"]
densityPlot(Moorhen_act, rug=T, adjust=1, xlab="Time",
            ylab="Density", lwd=1, main="", xlim=c(0, 24), ylim=c(0,0.12))
legend("topright", c("Moorhen (Common)"), col="black", lwd=1, bty="n")

# American mink activity
mink_act <- dat2$solartime[dat2$Species == "American mink"]
densityPlot(mink_act, rug=T, adjust=1, xlab="Time",
            ylab="Density", lwd=1, main="", xlim=c(0, 24), ylim=c(0,0.12))
legend("topright", c("American mink"), col="black", lwd=1, bty="n")

# Brown Rat activity
brownrat_act <- dat2$solartime[dat2$Species == "Brown rat"]
densityPlot(brownrat_act, rug=T, adjust=1, xlab="Time",
            ylab="Density", lwd=1, main="", xlim=c(0, 24), ylim=c(0,0.12))
legend("topright", c("Brown rat"), col="black", lwd=1, bty="n")

##Comparison of activity patterns between two species
par(mfrow=c(3,1))

# otter vs Moorhen
overlap_result1 <-overlapEst(otter_act,Moorhen_act,type="Dhat4")
delta_value1 <- overlap_result1["Dhat4"]
overlapPlot(otter_act,Mallard_act, rug=T,main="",xlab="Time",
            ylab="Density",ylim=c(0,0.12))
legend(5,0.13, c("otter","Moorhen"), lty=c(1,2), col=c(1,4), bty='n')
text(20, 0.11, paste("Δ =", round(delta_value1, 3)))

#otter vs american mink
overlap_result2 <-overlapEst(otter_act,mink_act,type="Dhat4")
delta_value2 <- overlap_result2["Dhat4"]
delta_value2
overlapPlot(otter_act,mink_act, rug=T,main="",xlab="Time",
            ylab="Density",ylim=c(0,0.12))
legend(5,0.13, c("otter","American_mink"), lty=c(1,2), col=c(1,4), bty='n')
text(20, 0.11, paste("Δ =", round(delta_value2, 3)))

#otter vs brown rat
overlap_result3 <-overlapEst(otter_act,brownrat_act,type="Dhat4")
delta_value3 <- overlap_result3["Dhat4"]
delta_value3
overlapPlot(otter_act,brownrat_act, rug=T,main="",xlab="Time",
            ylab="Density",ylim=c(0,0.12))
legend(5,0.13, c("otter","Brown rat"), lty=c(1,2), col=c(1,4), bty='n')
text(20, 0.11, paste("Δ =", round(delta_value3, 3)))

# p-value test
otter_act_mod <- fitact(dat2$solartime[dat2$Species == "Otter"], sample = "model", reps = 5)
brat_act_mod <- fitact(dat2$solartime[dat2$Species == "Brown rat"], sample = "model", reps = 5)
mink_act_mod <- fitact(dat2$solartime[dat2$Species == "American mink"], sample = "model", reps = 5)
moorhen_act_mod <- fitact(dat2$solartime[dat2$Species == "Moorhen (Common)"], sample = "model", reps = 5)

omocomp<-compareCkern(otter_act_mod,moorhen_act_mod, reps = 1000)
omocomp

omicomp<-compareCkern(otter_act_mod,mink_act_mod, reps = 1000)
omicomp

obrcomp<-compareCkern(otter_act_mod,brat_act_mod, reps = 1000)
obrcomp

# calculate CI
# Self-help overlap estimation function
bootstrap_overlap <- function(species1, species2, n_reps=1000) {
  results <- numeric(n_reps)
  
  for(i in 1:n_reps) {
    boot_sample1 <- sample(species1, size=length(species1), replace=TRUE)
    boot_sample2 <- sample(species2, size=length(species2), replace=TRUE)
    
    overlap_result <- overlapEst(boot_sample1, boot_sample2, type="Dhat4")
    results[i] <- overlap_result["Dhat4"]
  }
  
  return(results)
}

# put the data into the model
otter_moorhen_bootstrap <- bootstrap_overlap(otter_act, Moorhen_act)
otter_mink_bootstrap <- bootstrap_overlap(otter_act, mink_act)
otter_brat_bootstrap <- bootstrap_overlap(otter_act, brownrat_act)

# Calculate 99% confidence interval
CI_lower_om1 <- quantile(otter_moorhen_bootstrap, 0.005)
CI_upper_om1 <- quantile(otter_moorhen_bootstrap, 0.995)
#
CI_lower_om2 <- quantile(otter_mink_bootstrap, 0.005)
CI_upper_om2 <- quantile(otter_mink_bootstrap, 0.995)
#
CI_lower_om3 <- quantile(otter_brat_bootstrap, 0.005)
CI_upper_om3 <- quantile(otter_brat_bootstrap, 0.995)

# print the result
cat("[", CI_lower_om1, ",", CI_upper_om1, "]\n")
cat("[", CI_lower_om2, ",", CI_upper_om2, "]\n")
cat(" [", CI_lower_om3, ",", CI_upper_om3, "]\n")


# brown rats'day of year relative activity based on solartime
set.seed(12345)
dat2$solartime_day <- (dat2$solartime - min(dat2$solartime)) * (2 * pi / (max(dat2$solartime) - min(dat2$solartime)))
brats_day <- dat2 %>% filter(Species == "Brown rat")
mod_brats_day <- fitact(brats_day$solartime_day, sample = "model", reps = 5)
#visulation
f_brat_day <- as.data.frame(mod_brats_day@pdf)
f_brat_day <- ggplot(f_brat_day) + 
  geom_ribbon(aes(x=x, ymin=lcl, ymax=ucl), fill = "red", alpha = 0.3) +
  theme_classic() +
  geom_vline(xintercept = seq(0, 2*pi, pi/2), col = "grey70") +
  scale_x_continuous(breaks = seq(0, 2*pi, pi/2), labels = seq(0, 365, 91.25)) +
  geom_hline(yintercept = seq(0, 0.4, 0.1), col = "grey70") +
  scale_y_continuous(breaks = seq(0, 0.4, 0.1)) +
  xlab("Day of Year") + 
  ylab("Relative activity")+
  annotate("text", x = 1, y = 0.5, label = "Brown rat", size=4)
f_brat_day <- f_brat_day + coord_polar()
grid.arrange(f_brat_day, f_day, ncol=2)

###calculate the overlap index for otter and Brown rat in Non-Summer 
#divide seasons into Non-Summer vs Summer
dat2$season[dat2$season %in% c("Spring", "Winter", "Fall")] <- "Non-Summer"
seasons <- c("Non-Summer", "Summer")
compare_results <- data.frame(season=character(), test_statistic=numeric(), p_value=numeric())

# Calculate and plot activity rhythm overlap index for "Non-Summer"
par(mfrow=c(1,1))
otter_act_non_summer <- dat2$solartime[dat2$Species == "Otter" & dat2$season == "Non-Summer"]
rat_act_non_summer <- dat2$solartime[dat2$Species == "Brown rat" & dat2$season == "Non-Summer"]

overlap_non_summer <- overlapEst(otter_act_non_summer, rat_act_non_summer, type="Dhat4")
delta_non_summer <- overlap_non_summer["Dhat4"]

overlapPlot(otter_act_non_summer, rat_act_non_summer, rug=T, main="Non-Summer", xlab="Time", ylab="Density", ylim=c(0,0.12))
legend(0,0.13, c("otter","Brown rat"), lty=c(1,2), col=c(1,4), bty='n')
text(15, 0.11, paste("Δ =", round(delta_non_summer, 4)))

#p-value
otter_model_test <- fitact(otter_data_test, sample = "model", reps = 5)
brat_model_test <- fitact(brat_data_test, sample = "model", reps = 5)
comparison_test01 <- compareCkern(otter_model_test, brat_model_test, reps = 10000)
comparison_test01 


########
dat2$location <- paste(dat2$latitude, dat2$longitude, sep="-")
locations_with_otter <- unique(dat2$location[dat2$Species == "Otter"])

brat_with_otter <- dat2[dat2$Species == "Brown rat" & dat2$location %in% locations_with_otter,]
brat_without_otter <- dat2[dat2$Species == "Brown rat" & !dat2$location %in% locations_with_otter,]

set.seed(12345)
# Fitting the activity rhythm for Brown rat in the position with otter.
mod_brat_with_otter <- fitact(brat_with_otter$solartime, sample = "model", reps = 5)

# Fitting the activity rhythm for Brown rat in the position without otter.
mod_brat_without_otter <- fitact(brat_without_otter$solartime, sample = "model", reps = 5)

# Activity rhythm graph of Brown rat in the position with otter
brat_with <- as.data.frame(mod_brat_with_otter@pdf)
f_rw <- ggplot(brat_with) + 
  geom_ribbon(aes(x=x, ymin=lcl, ymax=ucl), fill = "red", alpha = 0.3) +
  theme_classic() +
  geom_vline(xintercept = seq(0,2*pi-0.01,pi/2), col = "grey70") +
  scale_x_continuous(breaks = seq(0,2*pi-0.01,pi/2), labels = seq(0,23,6)) +
  geom_hline(yintercept = seq(0,0.4,0.1), col = "grey70") +
  scale_y_continuous(breaks = seq(0,0.4,0.1)) +
  xlab("Hour of day") + ylab("Relative activity") + ggtitle("Brown rat in Otter territories")
f_rw <- f_rw + coord_polar()

# Activity rhythm graph of Brown rat in the position without otter
brat_without <- as.data.frame(mod_brat_without_otter@pdf)
f_rwth <- ggplot(brat_without) + 
  geom_ribbon(aes(x=x, ymin=lcl, ymax=ucl), fill = "red", alpha = 0.3) +
  theme_classic() +
  geom_vline(xintercept = seq(0,2*pi-0.01,pi/2), col = "grey70") +
  scale_x_continuous(breaks = seq(0,2*pi-0.01,pi/2), labels = seq(0,23,6)) +
  geom_hline(yintercept = seq(0,0.4,0.1), col = "grey70") +
  scale_y_continuous(breaks = seq(0,0.4,0.1)) +
  xlab("Hour of day") + ylab("Relative activity") + ggtitle("Brown rat outside Otter territories")
f_rwth <- f_rwth + coord_polar()
grid.arrange(f_rw, f_rwth, nrow=2)

#p-value test
comparison <- compareCkern(mod_brat_with_otter, mod_brat_without_otter, reps = 1000)
print(comparison)

R.version.string















































