hr_train = read.csv("C:/Users/h8man/Downloads/Project/housing_train.csv")
hr_test = read.csv("C:/Users/h8man/Downloads/Project/housing_test.csv")
library(dplyr)
library(tidyr)
hr_train$data = "train"
hr_test$data = "test"
hr_test$Price = NA
hr = rbind(hr_train,hr_test)
glimpse(hr)
names(hr)[sapply(hr, function(x) is.character(x))]
table(hr$Address)
#will drop this column as it contains too many unique values.
hr = hr%>%
  select(-Address,-Suburb)

CreateDummies = function(data,var,freq_cutoff=0){
  t = table(data[,var])
  t = t[t>freq_cutoff]
  t = sort(t)
  categories = names(t)[-1]
  for (cat in categories){
    name = paste(var,cat,sep = "-")
    name = gsub(" ","",name)
    name = gsub("-","_",name)
    name = gsub("\\?","Q",name)
    name = gsub("<","LT_",name)
    name = gsub("\\+",",",name)
    data[,name]= as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}
names(hr)[sapply(hr, function(x) is.character(x))]
col = c("Type","Method","SellerG","CouncilArea")
for(cat in col){
  hr = CreateDummies(hr,cat,100)
}
names(hr)[sapply(hr, function(x) is.integer(x))]
table(hr$Postcode)
table(hr$Bedroom2)
unique(hr$Bedroom2)
glimpse(hr)
hr = hr%>%
  mutate(Price = as.numeric(Price),
         Rooms = as.numeric(Rooms),
         Bathroom = as.numeric(Bathroom),
         Car = as.numeric(Car),
         Landsize = as.numeric(Landsize),
         BuildingArea = as.numeric(BuildingArea),
         Bedroom2 = as.numeric(Bedroom2))

hr = hr%>%
  mutate(Postcode = as.character(Postcode))
names(hr)[sapply(hr, function(x) is.character(x))]
cols = "Postcode"
for (cat in cols) {
  hr = CreateDummies(hr,cat,100)
  
}
hr = hr%>%
  mutate(YearBuilt = as.character(YearBuilt))
table(hr$YearBuilt)
unique(hr$YearBuilt)
lapply(hr, function(x) sum(is.na(x)))
table(hr$YearBuilt)
hr = hr%>%
  mutate(YearBuilt = ifelse(is.na(YearBuilt),"Unknown",YearBuilt))
table(hr$YearBuilt)
colss = "YearBuilt"
for (cat in colss) {
  hr = CreateDummies(hr,cat,80)
  
}
glimpse(hr)
names(hr[sapply(hr, function(x) is.character(x))])
lapply(hr, function(x) sum(is.na(x)))
hr_train = hr%>%
  filter(data == "train")%>%
  select(-data)
hr_test = hr%>%
  filter(data == "test")%>%
  select(-data,-Price)
lapply(hr_train, function(x) sum(is.na(x)))
hr_train = hr_train%>%
  mutate(Bedroom2 = round(ifelse(is.na(Bedroom2),mean(Bedroom2,na.rm = T),Bedroom2)),
         Landsize = round(ifelse(is.na(Landsize),mean(Landsize,na.rm = T),Landsize)),
         Car = round(ifelse(is.na(Car),mean(Car,na.rm = T),Car)),
      Bathroom = round(ifelse(is.na(Bathroom),mean(Bathroom,na.rm = T),Bathroom)),
          BuildingArea = round(ifelse(is.na(BuildingArea),mean(BuildingArea,na.rm = T),BuildingArea)))

hr_test = hr_test%>%
  mutate(Bathroom = ifelse(is.na(Bathroom),2,Bathroom),
         Bedroom2 = ifelse(is.na(Bedroom2),3,Bedroom2),
         Car = ifelse(is.na(Car),2,Car),
         Landsize = ifelse(is.na(Landsize),454,Landsize),
         BuildingArea = ifelse(is.na(BuildingArea),145,BuildingArea))

set.seed(2)
s = sample(1:nrow(hr_train),0.7*nrow(hr_train))
hr_train1 = hr_train[s,]
hr_train2 = hr_train[-s,]
library(car)
fit = lm(Price ~.-CouncilArea_,data = hr_train1)
sort(vif(fit),decreasing = T)[1:3]
# all vif values are under control
fit = step(fit)
summary(fit)
formula(fit)
fit = lm(Price ~ Rooms + Distance + Bedroom2 + Bathroom + Car  + 
           BuildingArea + Type_u + Type_h + Method_SP + Method_S + SellerG_Kay  
          + SellerG_RT 
    + SellerG_Marshall  + 
           SellerG_Jellis + SellerG_Nelson + CouncilArea_Whitehorse + 
           CouncilArea_Brimbank + CouncilArea_HobsonsBay + CouncilArea_Melbourne 
           + CouncilArea_PortPhillip + CouncilArea_Yarra + 
           CouncilArea_Maribyrnong + CouncilArea_Stonnington + 
           CouncilArea_MooneeValley + CouncilArea_Moreland + CouncilArea_Boroondara + 
           Postcode_3071 + Postcode_3147 + Postcode_3103 + Postcode_3124 + 
           Postcode_3127 + Postcode_3081 + Postcode_3207 + Postcode_3187 + 
           Postcode_3122 + Postcode_3031 + Postcode_3104 + Postcode_3181 + 
           Postcode_3015 + Postcode_3188 + Postcode_3186 + Postcode_3146 
           + Postcode_3141 + Postcode_3012 + Postcode_3072 + 
           Postcode_3204 + Postcode_3058 + Postcode_3163 + Postcode_3040 + 
           Postcode_3032 + Postcode_3121 + Postcode_3165 + Postcode_3046 + 
           Postcode_3020 + Postcode_3073  + YearBuilt_1890 + 
           YearBuilt_1910  + YearBuilt_1900  
            + YearBuilt_1970,data = hr_train1)
summary(fit)
rmse = mean(hr_train2$Price-predict(fit,newdata = hr_train2))**2%>%
  sqrt()
rmse
fit.final = lm(Price ~.,data = hr_train)
fit.final = step(fit.final)
formula(fit.final)
fit.final = lm(Price ~ Rooms + Distance + Bedroom2 + Bathroom + Car + Landsize + 
                 BuildingArea + Type_u + Type_h + Method_SP + Method_S + SellerG_Kay + 
                 SellerG_Miles + SellerG_Sweeney + SellerG_RT + SellerG_Marshall + 
                 SellerG_hockingstuart + SellerG_Jellis + SellerG_Nelson + 
                 CouncilArea_Whitehorse + CouncilArea_Brimbank + CouncilArea_HobsonsBay + 
                 CouncilArea_Bayside + CouncilArea_Melbourne + CouncilArea_Banyule + 
                 CouncilArea_Yarra + CouncilArea_Maribyrnong + CouncilArea_Stonnington + 
                 CouncilArea_Darebin + CouncilArea_MooneeValley + CouncilArea_Moreland + 
                 CouncilArea_Boroondara + Postcode_3071 + Postcode_3147 + 
                 Postcode_3013 + Postcode_3103 + Postcode_3124 + Postcode_3145 + 
                 Postcode_3127 + Postcode_3081 + Postcode_3207 + Postcode_3044 + 
                 Postcode_3187 + Postcode_3122 + Postcode_3031 + Postcode_3104 + 
                 Postcode_3181 + Postcode_3015 + Postcode_3011 + Postcode_3188 + 
                 Postcode_3101 + Postcode_3186 + Postcode_3084 + Postcode_3146 + 
                 Postcode_3056 + Postcode_3141 + Postcode_3012 + Postcode_3072 + 
                 Postcode_3204 + Postcode_3058 + Postcode_3163 + Postcode_3040 + 
                 Postcode_3032 + Postcode_3121 + Postcode_3165 + Postcode_3046 + 
                 Postcode_3020 + Postcode_3073 + YearBuilt_2012 + YearBuilt_1890 + 
                 YearBuilt_1910 + YearBuilt_1920 + YearBuilt_1930 + YearBuilt_1900 + 
                 YearBuilt_1950 + YearBuilt_1960 + YearBuilt_1970,data = hr_train)
summary(fit)
predict.IR = predict(fit.final,newdata = hr_test)
write.csv(predict.IR,"DeepakYadav_submission.csv",row.names = F)