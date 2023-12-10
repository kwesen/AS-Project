hcv <- read.csv("data.csv")
mis_id <- which(is.na(hcv["ALP"]))
ms_frame <- hcv[mis_id,]

temp <- hcv[hcv["Category"]==0,]

