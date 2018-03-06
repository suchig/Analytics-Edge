# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(data.table) #reading in the data
library(dplyr) #dataframe manipulation
library(ggplot2) #viz
library(ranger) #the random forest implementation
library(plotly) #3D plotting
library(tidyr) #dataframe manipulation

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

fb <- fread("../input/train.csv")

# Any results you write to the current directory are saved as output.