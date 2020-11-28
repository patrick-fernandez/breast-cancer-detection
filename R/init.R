library(tidyverse)
library(ggplot2)
library(caret)
library(MLeval)

data <- as_tibble(read.table("data/wdbc.data", sep = ","))
#rename variables
data <- data %>% rename(ID = V1, Diagnosis = V2, 
                        RadiusMean = V3, TextureMean = V4, PerimeterMean = V5,
                        AreaMean = V6, SmoothnessMean = V7, CompactnessMean = V8,
                        ConcavityMean = V9, ConcavePointsMean = V10, SymmetryMean = V11, 
                        FractalMean = V12, 
                        RadiusSD = V13, TextureSD = V14, PerimeterSD = V15, 
                        AreaSD = V16, SmoothnessSD = V17, CompactnessSD = V18, 
                        ConcavitySD = V19, ConcavePointsSD = V20,SymmetrySD = V21, 
                        FractalSD = V22, 
                        RadiusWorst = V23, TextureWorst = V24, PerimeterWorst = V25, 
                        AreaWorst = V26, SmoothnessWorst = V27, CompactnessWorst = V28, 
                        ConcavityWorst= V29, ConcavePointsWorst = V30, SymmetryWorst = V31, 
                        FractalWorst = V32)

##dataset is already clean
#Set Diagnosis as factor
data$Diagnosis <- as.factor(data$Diagnosis) %>% factor(c("M", "B"))

#Visualization
ggplot((data %>% select(3:32) %>% gather()), aes(value)) + 
  geom_histogram(bins = 20) +
  facet_wrap(~key, scales = 'free_x')