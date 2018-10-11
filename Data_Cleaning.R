#### Data Cleaning ####
data = data.frame(read.csv("laptops.csv"))

#change to factor etc
summary(data)

### Split Screen Size to ratio and touchscreen ###
for(i in 1:length(data$ScreenResolution))
{
  data$Touchscreen[i] = grepl("Touchscreen", toString(data$ScreenResolution[i]))
  
  if(grepl("1920x", toString(data$ScreenResolution[i])))
  {
    data$Resolution[i] = "1920"
  }
  else if(grepl("1366x", toString(data$ScreenResolution[i])))
  {
    data$Resolution[i] = "1366"
  }
  else if(grepl("1440x", toString(data$ScreenResolution[i])))
  {
    data$Resolution[i] = "1440"
  }
  else if(grepl("1600x", toString(data$ScreenResolution[i])))
  {
    data$Resolution[i] = "1600"
  }
  else if(grepl("2560x", toString(data$ScreenResolution[i])))
  {
    data$Resolution[i] = "2560"
  }
  else if(grepl("3840x", toString(data$ScreenResolution[i])))
  {
    data$Resolution[i] = "3840"
  }
  else if(grepl("3200x", toString(data$ScreenResolution[i])))
  {
    data$Resolution[i] = "3200"
  }
  else if(grepl("2304x", toString(data$ScreenResolution[i])))
  {
    data$Resolution[i] = "2304"
  }
  else if(grepl("2736x", toString(data$ScreenResolution[i])))
  {
    data$Resolution[i] = "2736"
  }
  else if(grepl("2880x", toString(data$ScreenResolution[i])))
  {
    data$Resolution[i] = "2880"
  }
  else if(grepl("2256x", toString(data$ScreenResolution[i])))
  {
    data$Resolution[i] = "2256"
  }
  else if(grepl("2400x", toString(data$ScreenResolution[i])))
  {
    data$Resolution[i] = "2400"
  }
  else if(grepl("2160x", toString(data$ScreenResolution[i])))
  {
    data$Resolution[i] = "2160"
  }
  else
  {
    data$Resolution[i] = NaN
  }
}

for(i in 1:length(data$Resolution))
{
  if(as.double(data$Resolution[i]) > 2000)
  {
    data$Resolution[i] = "UHD"
  }
  else if(as.double(data$Resolution[i]) < 1280)
  {
    #None of the Screens are < 1280x720
    data$Resolution[i] = "SD"
  }
  else
  {
    data$Resolution[i] = "HD"
  }
}

### Set the levels of resolution ###
data$Resolution = factor(data$Resolution, c("UHD", "HD", "SD"))

### Split up Cpu into brand and number of cores ###
data$Cpu.Cores <- 0
for(i in 1:length(data$Cpu))
{
  if(grepl("Intel", toString(data$Cpu[i])))
  {
    data$Cpu.Brand[i] = "Intel"
    
    if(grepl("Atom", toString(data$Cpu[i])) || grepl("Quad", toString(data$Cpu[i])) || 
       grepl("i5", toString(data$Cpu[i])) || grepl("Xeon E3", toString(data$Cpu[i])))
    {
      data$Cpu.Cores[i] = "4"
    }
    else if(grepl("Dual", toString(data$Cpu[i])) || grepl("i3", toString(data$Cpu[i])) || 
            grepl("Core M", toString(data$Cpu[i])))
    {
      data$Cpu.Cores[i] = "2"
    }
    else if (grepl("i7", toString(data$Cpu[i])))
    {
      if (grepl("i7-8", toString(data$Cpu[i])) || grepl("i7-78", toString(data$Cpu[i])) ||
          grepl("i7-77", toString(data$Cpu[i])) || grepl("i7 8", toString(data$Cpu[i])) || grepl("i7 78", toString(data$Cpu[i])) ||
        grepl("i7 77", toString(data$Cpu[i])))
      {
        data$Cpu.Cores[i] = "4"
      }
      else
      {
        data$Cpu.Cores[i] = "2"
      }
    }
    else
    {
      data$Cpu.Cores[i] = NaN
    }
  }
  else if(grepl("AMD", toString(data$Cpu[i])))
  {
    data$Cpu.Brand[i] = "AMD"
    if(grepl("A10", toString(data$Cpu[i])) || grepl("A12", toString(data$Cpu[i])) ||
       grepl("A8", toString(data$Cpu[i])) || grepl("A9", toString(data$Cpu[i])) )
    {
      data$Cpu.Cores[i] = "4"
    }
    else
    {
      data$Cpu.Cores[i] = "2"
    }
  }
  else
  {
    #Should we remove the laptop with samsung processor as the only one?
    data$Cpu.Brand[i] = NaN
    data$Cpu.Cores[i] = "00"
  }
}

# Reorder Ram to be in ascending order
levels(data$Ram) = c("2GB", "4GB", "6GB", "8GB", "12GB", 
                     "16GB", "24GB", "32GB", "64GB")

data$Ram1 = as.double(substr(data$Ram, 1, nchar(as.character(data$Ram))-2))

### Split Memory into Size (Factor) and Type (Binary) ###
data$Memory.Size = 0
for(i in 1:length(data$Memory))
{
  if(grepl("TB", toString(data$Memory[i])))
  {
    if(grepl(" 1TB", toString(data$Memory[i])) || grepl(" 2TB", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "Hybrid"
    }
    else if(grepl("2TB", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "2TB"
    }
    else if(grepl("1TB", toString(data$Memory[i])) || grepl("1.0TB", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "1TB"
    }
    else
    {
      data$Memory.Size[i] = NaN
    }
  }
  else if(grepl("GB", toString(data$Memory[i])))
  {
    if(grepl("8GB", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "8GB"
    }
    else if(grepl("16GB", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "16GB"
    }
    else if(grepl("32GB", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "32GB"
    }
    else if(grepl("64GB", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "64GB"
    }
    else if(grepl("128GB", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "128GB"
    }
    else if(grepl("180GB", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "180GB"
    }
    else if(grepl("128GB", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "128GB"
    }
    else if(grepl("240GB", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "240GB"
    }
    else if(grepl("500GB", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "500GB"
    }
    else if(grepl("512GB", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "512GB"
    }
    else if(grepl("GB +", toString(data$Memory[i])))
    {
      data$Memory.Size[i] = "Hybrid"
    }
  }
  else
  {
    #To see if there are any missing laptops
    data$Memory.Size[i] = NaN
  }
}

### Reorder Memory.Size to ascending order ###
data$Memory.Size = factor(data$Memory.Size, c("8GB", "16GB", "32GB", "64GB", "128GB", "180GB", "240GB",
                                            "256GB", "500GB", "512GB", "1TB", "2TB", "Hybrid"))

### Split GPU into brand ###
for(i in 1:length(data$Gpu))
{
  if(grepl("AMD", data$Gpu[i]))
  {
    data$Gpu.Brand[i] = "AMD"
  }
  else if(grepl("Intel", data$Gpu[i]))
  {
    data$Gpu.Brand[i] = "Intel"
  }
  else if(grepl("Nvidia", data$Gpu[i]))
  {
    data$Gpu.Brand[i] = "Nvidia"
  }
  else
  {
    data$Gpu.Brand[i] = "ARM"
  }
}

### Change and Reorder OpSys ###
for(i in 1:length(data$OpSys))
{
  if(grepl("Windows", data$OpSys[i]))
  {
    data$Opsys[i] = "Windows"
  }
  else if(grepl("Chrome OS", data$OpSys[i]))
  {
    data$Opsys[i] = "Chrome"
  }
  else if(grepl("Mac", data$OpSys[i]) || grepl("OS X", data$OpSys[i]) || grepl("mac", data$OpSys[i]))
  {
    data$Opsys[i] = "Mac"
  }
  else
  {
    data$Opsys[i] = levels(data$OpSys)[data$OpSys[i]]
  }
}

### Remove kg from weight and change to continuous variable ###
data$Weight = as.double(substr(data$Weight, 1, nchar(as.character(data$Weight))-2))

### Remove old variables ###
data = subset(data, select = -c(X, Product, ScreenResolution, Cpu, Memory, OpSys, Gpu))

#Remove the NAN values and ones with samsung/ARM?
data = data[-c(1192)]
str(data)

data$Cpu.Cores = factor(data$Cpu.Cores, c("2", "4"))
data$Cpu.Brand = factor(data$Cpu.Brand, c("Intel", "AMD"))
data$Gpu.Brand = factor(data$Gpu.Brand, c("Intel", "AMD", "Nvidia"))
data$Opsys = factor(data$Opsys, c("Windows", "Linux", "Chrome", "Mac", "Android", "No OS"))
