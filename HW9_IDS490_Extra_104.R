dataset = read.csv("/Users/wanghaoran/Downloads/2015Data.csv", header = TRUE)


makePlotRegion = function(xlim, ylim, bgcolor, ylabels,
                          margins, cityName, xtop = TRUE, degree= FALSE) {
  # This function is to produce a blank plot that has 
  # the proper axes labels, background color, etc.
  # It is to be used for both the top and bottom plot.
  
  # The parameters are
  # xlim is a two element numeric vector used for the two
  #   end points of the x axis
  # ylim is the same as xlim, but for the y axis
  # ylabels is a numeric vector of labels for "tick marks"
  #   on the y axis
  # We don't need to x labels because they are Month names
  # margins specifies the size of the plot margins (see mar parameter in par)
  # cityName is a character string to use in the title
  # xtop indicates whether the month names are to appear
  # at the top of the plot or the bottom of the plot
  # 
  # See the assignment for a pdf image of the plot that is
  # produced as a result of calling this function.
  
  par(bg = bgcolor, mar = margins)
  plot(1:5,1:5, type = 'n', xlim = xlim, ylim = ylim, xaxt = 'n', yaxt = 'n',
       bty='n',ylab =" ", xlab = ' ')
  if(degree){
    L = parse(text = paste(ylabels, "*degree", sep = ""))
    axis(2,pos = xlim[1], las=2, col.ticks = bgcolor, at=ylabels, labels =L) 
    # plot the left vertical axis, side = 2
    axis(4,pos = xlim[2], las=2, col.ticks = bgcolor, at=ylabels, labels= L) 
    # plot the right vertical axis, side = 4
    # whether it is a top plot or not
  }else{
    axis(2,pos = xlim[1], las=2, col.ticks = bgcolor, at=ylabels) 
    # plot the left vertical axis, side = 2
    axis(4,pos = xlim[2], las=2, col.ticks = bgcolor, at=ylabels) 
    # plot the right vertical axis, side = 4
    # whether it is a top plot or not
  }
  if(xtop){
    for(i in 1:12){
      mtext(text = monthNames[i],at = ((cumDays[i]+cumDays[i+1])/2), side = 3)
      # add text into the margin, side indicates the location is top
      # length of cumDays is 13
    }
  }
  else{
    for (i in 1:12){
      mtext(text = monthNames[i], at = ((cumDays[i] +cumDays[i+1])/2), side= 1)
    }
  }
  # finished
}


drawTempRegion = function(day, high, low, col){
  # This plot will produce 365 rectangles, one for each day
  # It will be used for the record temps, normal temps, and 
  # observed temps
  
  # day - a numeric vector of 365 dates
  # high - a numeric vector of 365 high temperatures
  # low - a numeric vector of 365 low temperatures
  # col - color to fill the rectangles
  
  # use the rect function to plot the data over 365 days
  rect(day-0.5, low, day+0.5, high, col = col, border = col)
  # finished
}


addGrid = function(location, col, ltype, vertical = TRUE, above = TRUE) {
  # This function adds a set of parallel grid lines
  # It will be used to place vertical and horizontal lines
  # on both temp and precip plots
  
  # location is a numeric vector of locations for the lines
  # col - the color to make the lines
  # ltype - the type of line to make
  # vertical - indicates whether the lines are vertical or horizontal
  
  # this func will be called multi-times
  if(above==FALSE){
    if(vertical){
      for(i in location){
        abline(col = col, lty = ltype, v = i)
      }
    }else{
      for(i in location){
        abline(col = col, lty = ltype, h = i)
        # param h indicates that the line should be added at location i horizontally
      }
    }
  }else{
  if(vertical){
    for(i in location){
      if(i == 31){
        segments(x0=i,y0=90,y1=15,lty=ltype)
      }
      else if(i == 59){
        segments(x0=i,y0=90,y1=15,lty=ltype)
      }
      else if(i==181 | i==212){
        segments(x0=i,y0=115,y1=40,lty=ltype)
      }
      else{
        abline(col = col, lty = ltype, v = i)
      }
      # param v indicates that the line should be added at location i vertically
    }
  }
  else{
    for(i in location){
      abline(col = col, lty = ltype, h = i)
      # param h indicates that the line should be added at location i horizontally
    }
  }
  # finished
  }
}

monthPrecip = function(day, dailyprecip, normal){
  # This function adds one month's precipitation to the 
  #   precipitation plot.
  # It will be called 12 times, once for each month
  # It creates the cumulative precipitation curve,
  # fills the area below with color, add the total
  # precipitation for the month, and adds a reference
  # line and text for the normal value for the month
  
  # day a numeric vector of dates for the month
  # dailyprecip a numeric vector of precipitation recorded
  # for the month (any NAs can be set to 0)
  # normal a single value, which is the normal total precip
  #  for the month
  
  # called 12 times
  # use the polygon func to plot the area, use lines func to plot
  # the cumsum and the normal line, which is a horizontal line
  
  polygon(x=c(day,rev(day)), y=c(cumsum(dailyprecip),rep(0,length(day))), 
          col = "#EEC591",border = NA)
  lines(day,cumsum(dailyprecip),lwd = 3,col = "#6495ED")
  # cumsum line
  lines(day,rep(normal,length(day)),lwd = 2, col= "#EE6A50")
  # normal line
  firstDay = day[1]
  lastDay = day[length(day)]
  if(lastDay == 90){
    text(lastDay-4, max(cumsum(dailyprecip)), pos = 2,cex = 1.0, 
         labels = as.character(max(cumsum(dailyprecip))))
  }else{
    text(lastDay-4, max(cumsum(dailyprecip)), pos = 3,cex = 1.0, 
         labels = as.character(max(cumsum(dailyprecip))))
  }
  # add ref text for cumsum line
  text(firstDay+4, normal, pos = 3, cex = 1.0,
       labels = as.character(normal))
  # add ref text for normal line
  # finished
}

monthNames = c("January", "February", "March", "April",
               "May", "June", "July", "August", "September",
               "October", "November", "December")
daysInMonth = c(31, 28, 31, 30, 31, 30, 31, 
                31, 30, 31, 30, 31)
cumDays = cumsum(c(1, daysInMonth))


finalPlot = function(temp,precip){
  # The purpose of this function is to create the whole plot
  # Include here all of the set up that you need for
  # calling each of the above functions.
  # temp is the data frame sfoWeather or laxWeather
  # precip is the data frame sfoMonthlyPrecip or laxMonthlyPrecip
  
  
  # Here are some vectors that you might find handy
  
  monthNames = c("January", "February", "March", "April",
                 "May", "June", "July", "August", "September",
                 "October", "November", "December")
  daysInMonth = c(31, 28, 31, 30, 31, 30, 31, 
                  31, 30, 31, 30, 31)
  cumDays = cumsum(c(1, daysInMonth))
  
  # another data frame
  totalPrecip = as.numeric(as.character(precip$precip))
  normPrecip = as.numeric(as.character(precip$normal))
  ### Fill in the various stages with your code
  
  
  ### Add any additional variables that you will need here
  
  # get variables from data frame
  days = temp$Date
  
  obs_low = as.numeric(temp$ActualLow)
  obs_high = as.numeric(temp$ActualHigh)
  normal_low = temp$NormalLow
  normal_high = temp$NormalHigh
  record_low = temp$RecordLow
  record_high = temp$RecordHigh
  record_low_year = temp$RecordLowYear
  record_high_year = temp$RecordHighYear
  
  precip = suppressWarnings(as.numeric(as.character(temp$ActualPrecip)))
  record_precip = suppressWarnings(as.numeric(as.character(temp$RecordPrecip)))
  #precip_year = temp$PrecipYr
  # preprossing of the data
  record_precip[is.na(record_precip)] = 0
  precip[is.na(precip)] = 0
  # find which day has the highest/lowest temp(i.e. breaking days)
  high_value = obs_high[which(obs_high == record_high)]
  low_value = obs_low[which(obs_low == record_low)]
  high_days = which(obs_high == record_high)
  low_days =  which(obs_low == record_low)
  
  
  
  ### Set up the graphics device to plot to pdf and layout
  ### the two plots on one canvas
  ### pdf("", width = , height = )
  ### layout(  )
  pdf("SF_weather_2015.pdf",width = 14, height = 7)
  layout(c(1, 1, 1, 1, 2, 2))
  
  ### Call makePlotRegion to create the plotting region
  ### for the temperature plot
  
  y_lowest = floor(min(record_low/10))*10
  y_highest = ceiling(max(record_high/10))*10
  y_lim = c(y_lowest, y_highest)
  y_labels = seq(y_lowest,y_highest, by = 10)
  
  makePlotRegion(xlim = c(1,365), ylim = y_lim, bgcolor = "#FFE4C4", margins = c(5,2,5,2),
                 ylabels = y_labels, xtop=TRUE,degree = TRUE)
  ### Call drawTempRegion 3 times to add the rectangles for
  ### the record, normal, and observed temps
  drawTempRegion(1:365, record_high, record_low, col="#CDB79E")
  drawTempRegion(1:365, normal_high, normal_low, col="#8B7D6B")
  drawTempRegion(1:365, obs_high, obs_low, col="#CD5555")
  
  ### Call addGrid to add the grid lines to the plot
  # add vertical black dotted line(ltype = 3)
  addGrid(cumDays[-c(1,length(cumDays))]-1,"#000000",3)
  # add horizontal white ref line
  addGrid(y_labels, "#FFE4C4", 1, vertical = FALSE)
  
  ### Add the markers for the record breaking days
  # for highest tie
  
  text(high_days[which(high_value == max(high_value))]+0.5,
       max(high_value)+9,labels ="TIED RECORD", adj = 0)
  text(high_days[which(high_value == max(high_value))]+0.5,
       max(high_value)+5,labels = paste("HIGH:",max(high_value)), adj = 0)
  segments(x0=high_days[which(high_value == max(high_value))],y0=max(high_value),
           y1= max(high_value)+10)
  # for lowest tie
  text(low_days[which(low_value == min(low_value))]+0.5,
       max(low_value)-17,labels ="TIED RECORD",adj = 0)
  text(low_days[which(low_value == min(low_value))]+0.5,
       max(low_value)-21,labels = paste("LOW:",min(low_value)),adj = 0)
  segments(x0=low_days[which(low_value == min(low_value))],y0=min(low_value),
           y1= min(low_value)-16)
  ### Add the titles 
  #title(main = list("Los Angeles's Weather in 2011", cex = 2), adj = 0)
  title(main = list("San Francisco's Weather in 2015", cex = 2), adj = 0)
  text(3, ceiling(max(record_high/10))*10-2, labels = "Temperature", 
       cex = 1.5, font = 2, adj=0)
  text(3, ceiling(max(record_high/10))*10 - 7, 
       labels = "Bars represent range between ", cex = 1.5, adj = 0)
  text(3, ceiling(max(record_high/10))*10 - 11, 
       labels = "the daily observation.", cex = 1.5, adj = 0)
  
  # add the legend
  # RECORD HIGH/LOW
  segments(189,y_lowest+2, y1= y_lowest+17, col = "#CDB79E", lwd = 8)
  text(186,y_lowest+2, adj = 1,labels="RECORD LOW")
  text(186,y_lowest+17, adj = 1,labels="RECORD HIGH")
  
  # NORMAL RANGE
  segments(189,y_lowest+4, y1=y_lowest+9, col = "#8B7D6B",lwd = 8)
  segments(187,y_lowest+4, x1=189, col ="black")
  segments(187,y_lowest+10,x1=189, col ="black")
  segments(187,y_lowest+4,y1=y_lowest+10,col = "black")
  text(186,y_lowest+7, adj = 1, labels="NORMAL RANGE")
  
  # ACTUAL HIGH/LOW
  segments(189,y_lowest+8,y1= y_lowest+13, col = "#CD5555", lwd = 6)
  segments(189,y_lowest+8,x1=194,col="black")
  segments(189,y_lowest+13,x1=194,col="black")
  text(195,y_lowest+8, adj = 0,labels="ACTUAL LOW")
  text(195,y_lowest+13, adj = 0,labels="ACTUAL HIGH")
  
  ### Call makePlotRegion to create the plotting region
  ### for the precipitation plot
  
  y_precip_highest = ceiling(max(totalPrecip))
  y_precip_lowest = floor(min(totalPrecip))
  y_bottom_lim = c(y_precip_lowest,y_precip_highest)
  makePlotRegion(xlim = c(1,365), ylim = y_bottom_lim, 
                 bgcolor = "#FFE4C4", margins = c(5,2,5,2),
                 ylabels = 0:y_precip_highest, xtop=FALSE)
  
  ### Call monthPrecip 12 times to create each months 
  ### cumulative precipitation plot. To do this use 
  ### sapply(1:12, function(m) {
  ###             code
  ###             monthPrecip(XXXX)
  ###             }) 
  ### the anonymous function calls monthPrecip with the 
  ### appropriate arguments
  # add vertical black dotted line(ltype = 3)
  addGrid(cumDays[c(-1,-length(cumDays))]-1,"#000000",3,above = FALSE)
  # add horizontal white ref line
  addGrid(1:y_precip_highest, "#FFE4C4", 1, vertical = FALSE,above = FALSE)
  for(i in 1:12){
    days_in_month = daysInMonth[i]
    precip_index = cumDays[i]:(cumDays[i+1]-1)
    monthPrecip(precip_index,precip[precip_index],normal = normPrecip[i])
  }
  
  
  
  ### Call addGrid to add the grid lines to the plot
  
  ### Add the titles
  title(main = list("Precipitation", cex = 1.8), adj = 0)
  mtext( paste("Cumulative monthly precipitation in inches compared with normal monthly precipitation. Total precipitation in 2015 was", sum(precip), "inches."),
         line = 2, at=(180))
  ### Close the pdf device dev.off()
  dev.off()
}

### Call: finalPlot(temp = sfoWeather, precip = sfoMonthlyPrecip)
finalPlot(temp = dataset,precip = sfoMonthlyPrecip)
#finalPlot(temp = laxWeather, precip = laxMonthlyPrecip)
