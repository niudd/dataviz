library(dplyr)
library(ggplot2)
library(ggthemes)
library(grid)
library(plyr)
library(tidyr)
library(zoo)
#esquisse::esquisser()
setwd("~/Downloads/data_vis/trump-approval")

df <- read.csv('datasets/approval_polllist.csv')

allpoll <- df %>% 
  filter(subgroup == "All polls")

#allpoll <- allpoll[c("pollster", "weight", "approve", "disapprove", "createddate")]
allpoll <- allpoll[c("pollster", "weight", "approve", "disapprove", "enddate")]
colnames(allpoll)[5] <- "createddate"
allpoll$createddate <- as.POSIXct(strptime(allpoll$createddate, format = "%m/%d/%Y"))

x_axis <- c("2017-01-23", "2017-04-01", "2017-07-01", "2017-10-01", 
            "2018-01-01", "2018-04-01", "2018-07-01", "2018-10-01", 
            "2019-01-01", "2019-04-01", "2019-07-01", "2019-10-01", "2020-01-07")
x_axis <- as.POSIXct(strptime(x_axis, format = "%Y-%m-%d"))
x_labels <- c("JAN.23,2017", "APRIL", "JULY", "OCT.", "JAN.2018", "APRIL", "JULY", "OCT.", 
              "JAN.2019", "APRIL", "JULY", "OCT.", "JAN.2020")
xlims <- as.POSIXct(strptime(c("2017-01-23", "2020-02-01"), format = "%Y-%m-%d"))

##for title color
t1 <- textGrob(expression("How " * phantom(bold("popular")) * " / " * phantom(bold("unpopular")) * " is Donald Trump?"),
               x = 0.25, y = 1.02, gp = gpar(col = "black", fontsize=12))
t2 <- textGrob(expression(phantom("How ") * bold("popular") * phantom(" / unpopular is Donald Trump?")),
               x = 0.25, y = 1.02, gp = gpar(col = "#0EB116", fontsize=12))
t3 <- textGrob(expression(phantom("How popular / ") * bold("unpopular") * phantom(" is Donald Trump?")),
               x = 0.25, y = 1.02, gp = gpar(col = "#F4781D", fontsize=12))

rollm_app <- rollapply(allpoll$approve, width=30, FUN=mean, fill=NA, align = "right")
rollsd_app <- rollapply(allpoll$approve, width=60, FUN=sd, fill=NA, align = "right")
rollm_disapp <- rollapply(allpoll$disapprove, width=30, FUN=mean, fill=NA, align = "right")
rollsd_disapp <- rollapply(allpoll$disapprove, width=60, FUN=sd, fill=NA, align = "right")

p <- ggplot(allpoll, aes(createddate)) +
  geom_point(aes(y = approve, col = "Approve"), size=1.5, color='#A6E0BA', alpha=0.4) + 
  geom_point(aes(y = disapprove, col = "Disapprove"), size=1.5, color='#F3BF98', alpha=0.4) +
  
  geom_line(aes(y=rollm_app), color='#0EB116', size=0.75) +
  geom_line(aes(y=rollm_disapp), color='#F4781D', size=0.75) +
  
  geom_ribbon(aes(ymin=rollm_app-1.5*rollsd_app, 
                  ymax=rollm_app+1.5*rollsd_app),
              alpha=0.3, fill='#A6E0BA') + 
  geom_ribbon(aes(ymin=rollm_disapp-1.5*rollsd_disapp, 
                  ymax=rollm_disapp+1.5*rollsd_disapp), 
              alpha=0.3, fill='#F3BF98') + 
  
  geom_line(aes(y=50)) +

  scale_x_datetime(breaks = x_axis, labels = x_labels, expand = c(0, 0), limits = xlims) + 
  scale_y_continuous(breaks = c(20, 30, 40, 50, 60, 70, 80), 
                     labels = c("20%", "30%", "40%", "50%", "60%", "70%", "80%"),
                     limits = c(20, 80), expand = c(0, 0)) +
  
  labs(x = NULL, y = NULL, 
       title = ""
       #subtitle = paste0("An updating calculation of the president's approval ", 
      #                   "rating, accounting for each poll's quality, recency, sample size and ", 
      #                   "partisan lean. How it works")
       ) + 
  
  #theme(legend.position = 'none',
  #      # add some extra margin on top
  #      plot.margin = unit(c(4, 1, 1, 1), "lines")) +
  
  annotation_custom(grobTree(t1, t2, t3)) +
  
  #theme_fivethirtyeight(base_size = 8)
  #theme_economist(base_size = 8)
  theme_wsj(base_size = 8)

#p
#ggplotly(p)

# create gtable and remove clipping
g <- ggplot_gtable(ggplot_build(p))
g$layout$clip[g$layout$name == "panel"] <- "off"
# re-draw
grid.draw(g)


