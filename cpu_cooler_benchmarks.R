library(ggplot2)
library(chron)
library(plotly)
library(svglite)

stock_cine <- read.csv("cine_2_stock.csv")
stock_blender <- read.csv("blender_2_stock.csv")
noctua_cine <- read.csv("cine_1_noctua.csv")
noctua_blender <- read.csv("blender_1_noctua.csv")

# Stock Cine --------------------------------------------------------------
names(stock_cine) <- stock_cine[1,]; stock_cine <- stock_cine[-1,]
colnames(stock_cine) <- make.unique(names(stock_cine))
store <- data.frame(time = rep(NA, nrow(stock_cine)))
for (i in 1:nrow(stock_cine)){
  store$time[i] <- unlist(strsplit(stock_cine$Time[i],' '))[2]
}
store$time <- times(store$time)
stock_cine$formatted_time <- store$time
stock_cine$second <- seq(1, nrow(stock_cine), 1)
  
ggplot(data = stock_cine, aes(x = second, y = `CPU Package.1`)) + geom_point()
plot_ly(data = stock_cine, x = ~second, y = ~`CPU Package.1`, type = 'scatter')

#8 to 98
stock_cine <- stock_cine[8:98,]
stock_cine$second <- seq(1, nrow(stock_cine), 1)
stock_cine$second

# Noctua Cine -------------------------------------------------------------
names(noctua_cine) <- noctua_cine[1,]; noctua_cine <- noctua_cine[-1,]
colnames(noctua_cine) <- make.unique(names(noctua_cine))
store <- data.frame(time = rep(NA, nrow(noctua_cine)))
for (i in 1:nrow(noctua_cine)){
  store$time[i] <- unlist(strsplit(noctua_cine$Time[i],' '))[2]
}
store$time <- times(store$time)
noctua_cine$formatted_time <- store$time
noctua_cine$second <- seq(1, nrow(noctua_cine), 1)

#ggplot(data = noctua_cine, aes(x = second, y = `CPU Package.1`)) + geom_point()
plot_ly(data = noctua_cine, x = ~second, y = ~`CPU Package.1`, type = 'scatter')

#6 to 92
noctua_cine <- noctua_cine[6:92,]
noctua_cine$second <- seq(1, nrow(noctua_cine), 1)
noctua_cine$second


# CINE -------------------------------------------------------------------
# stock_cine$cat <- rep("stock")
# noctua_cine$cat <- rep("noctua")

stock_cine_subset <- data.frame(second = stock_cine$second,
                   temp = stock_cine$`CPU Package.1`,
                   fan_speed = stock_cine$`Fan #2`,
                   avg_core_speed = rowMeans(apply(as.matrix(stock_cine[,c("CPU Core #1.2",
                                                                           "CPU Core #2.2",
                                                                           "CPU Core #3.2",
                                                                           "CPU Core #4.2",
                                                                           "CPU Core #5.2",
                                                                           "CPU Core #6.2")]), 2, as.numeric)),
                   cooler = rep("stock", nrow(stock_cine)),
                   noise = rep(55.28, nrow(stock_cine)))

stock_noctua_subset <- data.frame(second = noctua_cine$second,
                                temp = noctua_cine$`CPU Package.1`,
                                fan_speed = noctua_cine$`Fan #2`,
                                avg_core_speed = rowMeans(apply(as.matrix(noctua_cine[,c("CPU Core #1.2",
                                                                                        "CPU Core #2.2",
                                                                                        "CPU Core #3.2",
                                                                                        "CPU Core #4.2",
                                                                                        "CPU Core #5.2",
                                                                                        "CPU Core #6.2")]), 2, as.numeric)),
                                cooler = rep("noctua", nrow(noctua_cine)),
                                noise = rep(58.75, nrow(noctua_cine)))

cine <- rbind(stock_cine_subset, stock_noctua_subset)
cine$temp <- as.numeric(cine$temp)
cine$fan_speed <- as.numeric(cine$fan_speed)

ggplot(data = cine, aes(x = second, y = temp)) + geom_point(aes(color=cooler)) +
  scale_y_continuous(breaks = seq(min(cine$temp), max(cine$temp), 10), limits = c(min(cine$temp), max(cine$temp))) +
  ggtitle("Cinebench R20 CPU Temperatures by Cooler") +
  xlab("time (s)") +
  ylab("temperature (˚C)") +
  scale_color_manual(values=c("#1289A7", "#ED4C67"))
ggsave(file="cine_temp.svg", width=10, height=5)
  
ggplot(data = cine, aes(x = second, y = fan_speed)) + geom_point(aes(color=cooler)) +
  scale_y_continuous(breaks = seq(min(cine$fan_speed), max(cine$fan_speed), 100), limits = c(min(cine$fan_speed), max(cine$fan_speed))) +
  ggtitle("Cinebench R20 CPU Fan Speeds by Cooler") +
  xlab("time (s)") +
  ylab("fan speed (RPM)") +
  scale_color_manual(values=c("#1289A7", "#ED4C67"))
ggsave(file="cine_fan_speed.svg", width=10, height=5)

ggplot(data = cine, aes(x = second, y = avg_core_speed)) + geom_point(aes(color=cooler)) +
  scale_y_continuous(breaks = seq(min(cine$avg_core_speed), max(cine$avg_core_speed), 50), limits = c(min(cine$avg_core_speed), max(cine$avg_core_speed))) +
  ggtitle("Cinebench R20 CPU Clock Speeds by Cooler") +
  xlab("time (s)") +
  ylab("clock speed (MHz)") +
  scale_color_manual(values=c("#1289A7", "#ED4C67"))
ggsave(file="cine_clock_speed.svg", width=10, height=5)


# Stock Blender -----------------------------------------------------------
names(stock_blender) <- stock_blender[1,]; stock_blender <- stock_blender[-1,]
colnames(stock_blender) <- make.unique(names(stock_blender))
store <- data.frame(time = rep(NA, nrow(stock_blender)))
for (i in 1:nrow(stock_blender)){
  store$time[i] <- unlist(strsplit(stock_blender$Time[i],' '))[2]
}
store$time <- times(store$time)
stock_blender$formatted_time <- store$time
stock_blender$second <- seq(1, nrow(stock_blender), 1)

ggplot(data = stock_blender, aes(x = second, y = `CPU Package.1`)) + geom_point()
plot_ly(data = stock_blender, x = ~second, y = ~`CPU Package.1`, type = 'scatter')

#9 to 255
stock_blender <- stock_blender[9:255,]
stock_blender$second <- seq(1, nrow(stock_blender), 1)
stock_blender$second

# Noctua Blender -----------------------------------------------------------
names(noctua_blender) <- noctua_blender[1,]; noctua_blender <- noctua_blender[-1,]
colnames(noctua_blender) <- make.unique(names(noctua_blender))
store <- data.frame(time = rep(NA, nrow(noctua_blender)))
for (i in 1:nrow(noctua_blender)){
  store$time[i] <- unlist(strsplit(noctua_blender$Time[i],' '))[2]
}
store$time <- times(store$time)
noctua_blender$formatted_time <- store$time
noctua_blender$second <- seq(1, nrow(noctua_blender), 1)

ggplot(data = noctua_blender, aes(x = second, y = `CPU Package.1`)) + geom_point()
plot_ly(data = noctua_blender, x = ~second, y = ~`CPU Package.1`, type = 'scatter')

#8 to 252
noctua_blender <- noctua_blender[8:252,]
noctua_blender$second <- seq(1, nrow(noctua_blender), 1)
noctua_blender$second


# BLENDER -----------------------------------------------------------------


