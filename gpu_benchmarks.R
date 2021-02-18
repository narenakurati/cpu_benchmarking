# Read in Datsets ---------------------------------------------------------
#first set working directory 

#read all CSVs in directory
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))

list_df = lapply(target_sets, get)
names(list_df) = target_sets

# Clean up ----------------------------------------------------------------
#fix the header issue and duplicate column names
for (i in 1:length(list_df)){
  colnames(list_df[[i]]) <- as.character(unlist(list_df[[i]][1,]))
  list_df[[i]] <- list_df[[i]][-1,]
  colnames(list_df[[i]]) <- make.unique(names(list_df[[i]]))
}

# GPU specific benchmarks ----------------------------------------------------
storage <- data.frame(run = target_sets, 
                      max_temp = rep(NA, 8), 
                      max_fan_perc = rep(NA, 8),
                      max_clock = rep(NA, 8),
                      max_power_draw = rep(NA, 8))

for (i in 1:length(list_df)){
  storage$max_temp[i] <- max(as.numeric(as.character(list_df[[i]]$`GPU Core`)))
  storage$max_fan_perc[i] <- max(as.numeric(as.character(list_df[[i]]$`GPU Fan`)))
  storage$max_clock[i] <- max(as.numeric(as.character(list_df[[i]]$`GPU Core.1`)))
  storage$max_power_draw[i] <- max(as.numeric(as.character(list_df[[i]]$`GPU Power`)))
}

