source("transform_df.R")

NOAA2_28_10 <- read.table("Data/recon_NOAA2-1009A-IDA.txt")
AF305_28_09 <- read.table("Data/recon_AF305-0909A-IDA.txt")

noaa <- transform_df(NOAA2_28_10)
af <- transform_df(AF305_28_09)

# ggplot(noaa1, aes(x = Time, y = values, color = type)) + 
#   geom_line() +
#   scale_x_datetime(date_breaks = "15 minutes",
#                    date_labels = "%I:%M %p")

ggplot(noaa, aes(x=Time)) + 
  geom_line(aes(y = MSLP), color = "darkred") + 
  geom_line(aes(y = Temp)) + 
  scale_y_continuous(
    name = "Extrapolated Sea-Level Pressure (mb)"
  )

ggplot(af, aes(Time, `Wind Speed`)) + 
  geom_point(aes(colour = cut(`Error Code`, c(-Inf, 1, Inf))),
             size = 1) +
  geom_line(alpha = 0.5) +
  scale_color_manual(name = "Measurement",
                     values = c("[Inf,1]" = "green",
                                "(1,Inf]" = "red"),
                     
                     )


