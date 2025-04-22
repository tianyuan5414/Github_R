#This script aims at compare pollen pre-picked for GC-MS analysed, and pollen
  #concentration from Donatella et. al

#Load necessary packages
{
  library(here)
  library(ggplot2)
}

#Read data of
  #1. Number of grains during pre-picking
  #2. Concentration of Pinus grains from Donatella
{
  #Setup parent directory for reading data
  setwd(here())
  setwd('..')
  tempDir <- getwd()
  
  #Read pre-picking data
  prePick <- read.csv(file = paste(tempDir, '/Data_R/Pre_Picking.csv',
                                   sep = '',
                                   collapse = NULL))
  
  #Read Pinus concentration data
  pinusConc <- read.csv(file = paste(tempDir, '/Data_R/Pinus_Conc.csv',
                                     sep = '',
                                     collapse = NULL))
  
  #Scale the data to the comparable range
  scalePrepick <- ceiling(max(prePick$number / prePick$gram) / 100) * 100
  scalePinusConc <- ceiling(max(pinusConc$pinus) / 10000) * 10000
  scalePinus <- pinusConc$pinus / scalePinusConc * scalePrepick
  
  #Find the depths where pollen grains were picked, and extract the corresponding
    #scaled Pinus concentration
  scalePinus <-approx(x = pinusConc$metri, y = scalePinus,
                      xout = prePick$depth)[[2]]
}

#Plot the comparison between pre-picking and Pinus concentration
{
  plotComp <- ggplot() +
    geom_bar(aes(x = prePick$depth,
                  y = scalePinus),
             fill = 'black',
             alpha = 1,
             stat = "identity",
             width = 0.1) +
    geom_bar(aes(x = prePick$depth,
                 y = prePick$number / prePick$gram),
             fill = 'red',
             alpha = 0.5,
             stat = "identity",
             width = 0.1) +
    scale_y_continuous(limits = c(0, scalePrepick),
                       breaks = seq(0, scalePrepick, by = 100),
                       name = 'Pre-picked grain (grains/g)',
                       sec.axis = 
                         sec_axis(
                          ~. * scalePinusConc / scalePrepick,
                          name = 'Pinus concentration (grains/g)'
                         )
                       ) +
    scale_x_continuous(name = 'Depth (m)') +
    theme_classic() +
    theme(
      axis.text.y  = element_text(color = 'red'),
      axis.title.y = element_text(color='red'),
      axis.text.y.right =  element_text(color = 'black'),
      axis.title.y.right = element_text(color='black')
    )
}