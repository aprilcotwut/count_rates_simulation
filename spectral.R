# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# This program reads data developed from spectral.tcl into a csv with         #
# appropriate headers, and develops plots from the data gathered by running   #
# autospectral.xcm. The varying parameter (x-axis) scales logarithmically.    #
#                                                                             #
# Usage: R should be successfully installed. Either open R from the command   #
# line by typing "R" in the terminal or open RStudio. To run, type:           #
#         source("spectral.R")                                                #
# and when prompted, enter the name of the directory containing the counts    #
# file. Any changes to parameters in spectral.tcl will be automatically       #
# detected.                                                                   #
#                                                                             #
# Author: April Walker                                                        #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
library(dplyr)
library(ggplot2)
prompt = "Enter directory name. \n"
dir = readline(prompt)
filename = paste(dir, "/", dir, "counts.txt", sep="")
outputfile = paste(dir, "/", dir, "counts.csv", sep="")

#This sets the color scheme
c_scheme = c("#1f78b4", "#1fbe8f", "#2b8b24", "#b8d123", "#ffdb5c", "#ff6300",
             "#b30200", "#ff6a6a", "#ff26f0", "#6a3d9a")

if (file.exists(filename)) {
  data = read.table(file=filename, sep="", header = FALSE, colClasses = 'character')

  data = data[,c(16,14:15,1:13)]
  data = rename(data, 'nh'        = V1,
                      'norm'      = V2,
                      '0.3-10.0'  = V3,
                      '0.3-1.0'   = V4,
                      '1.0-2.0'   = V5,
                      '2.0-10.0'  = V6,
                      '0.5-7.0'   = V7,
                      '0.5-2.0'   = V8,
                      '2.0-7.0'   = V9,
                      '2.0-4.0'   = V10,
                      '4.0-6.0'   = V11,
                      '6.0-8.0'   = V12,
                      '4.0-8.0'   = V13,
                      'diskbb'    = V14,
                      'comptt'    = V15,
                      'gamma'     = V16)

  data = data %>% mutate_all(as.character)
  write.table(data, file=outputfile, sep='\t', row.names=FALSE)

  data = data %>% mutate_all(as.numeric)
  nh_vals = unique(data$nh)
  norm_vals = unique(data$norm)


  # for each band
  for (val in norm_vals) {
    tmp = subset(data, data$norm == val)
    plot_name = paste(dir, "/norm", val, ".ps", sep="")
    postscript(plot_name, width=500, height=500)
    plot(tmp$nh, tmp[,7], type = 'l', log="x", ylim = c(0,1), xlim=c(min(nh_vals),
    max(nh_vals)), col = c_scheme[1], main="Column Density vs. Count Rate Ratio
    for Various Bands", ylab="Count Ratio", xlab="Column Density (nH)")
    j = 2
    for (i in 8:16) {
      lines(tmp$nh, tmp[,i], col = c_scheme[j])
      j = j + 1
    }
    legend("topright", legend = names(data)[7:16], inset = .02, col = c_scheme, cex=0.8,
        lty = 1)
    dev.off()
  }

  for (val in nh_vals) {
    tmp = subset(data, data$nh == val)
    plot_name = paste(dir, "/nh", val, ".ps", sep="")
    postscript(plot_name, width=500, height=500)
    plot(tmp$norm, tmp[,7], type = 'l', log="x", ylim = c(0,1), xlim=c(min(norm_vals),
    max(norm_vals)), col = c_scheme[1], main = "Disk Normalization vs. Count Rate Ratio
    for Various Bands", ylab="Count Ratio", xlab="Disk Normalization")
    j = 2
    for (i in 8:16) {
      lines(tmp$norm, tmp[,i], col = c_scheme[j])
      j = j + 1
    }
    legend("topright", legend = names(data)[7:16], inset = .02, col = c_scheme, cex=0.8,
        lty = 1)
    dev.off()
  }
} else {
  print("Invalid directory")
}
