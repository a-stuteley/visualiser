#csv of data files

#alldata = alldata[,-6]
#write.csv(alldata, file = "csvs.csv")
#for(i in 1:length(alldata[,1])){
#  print(i);(read.csv(alldata$file[i], sep = alldata$sep[i], header = TRUE))
#}

alldata = data.frame(type = NA, sep = NA, file = NA, ethn = NA, issue = NA, prior = NA, dep = NA, country = NA, geog = NA)
a = 1
alldata[a,] = c("d", ";", "data/denominator/popcencook.csv", "Total", NA, NA, NA, "Cook Islands", NA); a = a + 1
alldata[a,] = c("n", ";", "data/numerator/mortcook.csv", "Total", "Total Mortality", NA, NA, "Cook Islands", NA); a = a + 1
alldata[a,] = c("n", ";", "data/numerator/neocook.csv", "Total", "Neoplasms", NA, NA, "Cook Islands", NA); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/mort037totalnat.csv", "Total", "Mortality - Total", NA, NA, "New Zealand", "National"); a = a + 1
#alldata[a,] = c("n", ",", "data/numerator/mort037totalreg.csv", "Total", "Mortality - Total", NA, NA, "New Zealand", "Regional Council"); a = a + 1
#alldata[a,] = c("n", ",", "data/numerator/mort037totaltla.csv", "Total", "Mortality - Total", NA, NA, "New Zealand", "TLA"); a = a + 1
#alldata[a,] = c("n", ",", "data/numerator/mort037totaldhb.csv", "Total", "Mortality - Total", NA, NA, "New Zealand", "DHB"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/dep/mort1dep0637totalnat.csv", "Total", "Mortality - Total", NA, 1, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort2dep0637totalnat.csv", "Total", "Mortality - Total", NA, 2, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort3dep0637totalnat.csv", "Total", "Mortality - Total", NA, 3, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort4dep0637totalnat.csv", "Total", "Mortality - Total", NA, 4, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort5dep0637totalnat.csv", "Total", "Mortality - Total", NA, 5, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort6dep0637totalnat.csv", "Total", "Mortality - Total", NA, 6, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort7dep0637totalnat.csv", "Total", "Mortality - Total", NA, 7, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort8dep0637totalnat.csv", "Total", "Mortality - Total", NA, 8, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort9dep0637totalnat.csv", "Total", "Mortality - Total", NA, 9, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort10dep0637totalnat.csv", "Total", "Mortality - Total", NA, 10, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort12dep0637totalnat.csv", "Total", "Mortality - Total", NA, 12, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort34dep0637totalnat.csv", "Total", "Mortality - Total", NA, 34, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort56dep0637totalnat.csv", "Total", "Mortality - Total", NA, 56, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort78dep0637totalnat.csv", "Total", "Mortality - Total", NA, 78, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort910dep0637totalnat.csv", "Total", "Mortality - Total", NA, 910, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/mort037maorinat.csv", "Maori", "Mortality - Total", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/dep/mort1dep0637maorinat.csv", "Maori", "Mortality - Total", NA, 1, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort2dep0637maorinat.csv", "Maori", "Mortality - Total", NA, 2, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort3dep0637maorinat.csv", "Maori", "Mortality - Total", NA, 3, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort4dep0637maorinat.csv", "Maori", "Mortality - Total", NA, 4, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort5dep0637maorinat.csv", "Maori", "Mortality - Total", NA, 5, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort6dep0637maorinat.csv", "Maori", "Mortality - Total", NA, 6, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort7dep0637maorinat.csv", "Maori", "Mortality - Total", NA, 7, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort8dep0637maorinat.csv", "Maori", "Mortality - Total", NA, 8, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort9dep0637maorinat.csv", "Maori", "Mortality - Total", NA, 9, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort10dep0637maorinat.csv", "Maori", "Mortality - Total", NA, 10, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort12dep0637maorinat.csv", "Maori", "Mortality - Total", NA, 12, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort34dep0637maorinat.csv", "Maori", "Mortality - Total", NA, 34, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort56dep0637maorinat.csv", "Maori", "Mortality - Total", NA, 56, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort78dep0637maorinat.csv", "Maori", "Mortality - Total", NA, 78, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort910dep0637maorinat.csv", "Maori", "Mortality - Total", NA, 910, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/mort037nonmaorinat.csv", "Non-Maori", "Mortality - Total", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/dep/mort1dep0637nonmaorinat.csv", "Non-Maori", "Mortality - Total", NA, 1, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort2dep0637nonmaorinat.csv", "Non-Maori", "Mortality - Total", NA, 2, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort3dep0637nonmaorinat.csv", "Non-Maori", "Mortality - Total", NA, 3, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort4dep0637nonmaorinat.csv", "Non-Maori", "Mortality - Total", NA, 4, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort5dep0637nonmaorinat.csv", "Non-Maori", "Mortality - Total", NA, 5, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort6dep0637nonmaorinat.csv", "Non-Maori", "Mortality - Total", NA, 6, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort7dep0637nonmaorinat.csv", "Non-Maori", "Mortality - Total", NA, 7, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort8dep0637nonmaorinat.csv", "Non-Maori", "Mortality - Total", NA, 8, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort9dep0637nonmaorinat.csv", "Non-Maori", "Mortality - Total", NA, 9, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort10dep0637nonmaorinat.csv", "Non-Maori", "Mortality - Total", NA, 10, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort12dep0637nonmaorinat.csv", "Non-Maori", "Mortality - Total", NA, 12, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort34dep0637nonmaorinat.csv", "Non-Maori", "Mortality - Total", NA, 34, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort56dep0637nonmaorinat.csv", "Non-Maori", "Mortality - Total", NA, 56, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort78dep0637nonmaorinat.csv", "Non-Maori", "Mortality - Total", NA, 78, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort910dep0637nonmaorinat.csv", "Non-Maori", "Mortality - Total", NA, 910, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/mort037pacificnat.csv", "Pacific", "Mortality - Total", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mort037nonpacificnat.csv", "Non-Pacific", "Mortality - Total", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mort037asiannat.csv", "Asian", "Mortality - Total", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mort037nonasiannat.csv", "Non-Asian", "Mortality - Total", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mort037nonpacificnonmaorinat.csv", "Non-Pacific-Non-Maori", "Mortality - Total", NA, NA, "New Zealand", "National"); a = a + 1

#alldata[a,] = c("n", ",", "data/numerator/mort0pearcetotalnat.csv", "Total", "Mortality - Pearce Amenable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/dep/mort1dep06pearcetotalnat.csv", "Total", "Mortality - Pearce Amenable", NA, 1, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort2dep06pearcetotalnat.csv", "Total", "Mortality - Pearce Amenable", NA, 2, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort3dep06pearcetotalnat.csv", "Total", "Mortality - Pearce Amenable", NA, 3, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort4dep06pearcetotalnat.csv", "Total", "Mortality - Pearce Amenable", NA, 4, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort5dep06pearcetotalnat.csv", "Total", "Mortality - Pearce Amenable", NA, 5, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort6dep06pearcetotalnat.csv", "Total", "Mortality - Pearce Amenable", NA, 6, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort7dep06pearcetotalnat.csv", "Total", "Mortality - Pearce Amenable", NA, 7, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort8dep06pearcetotalnat.csv", "Total", "Mortality - Pearce Amenable", NA, 8, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort9dep06pearcetotalnat.csv", "Total", "Mortality - Pearce Amenable", NA, 9, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort10dep06pearcetotalnat.csv", "Total", "Mortality - Pearce Amenable", NA, 10, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort12dep06pearcetotalnat.csv", "Total", "Mortality - Pearce Amenable", NA, 12, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort34dep06pearcetotalnat.csv", "Total", "Mortality - Pearce Amenable", NA, 34, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort56dep06pearcetotalnat.csv", "Total", "Mortality - Pearce Amenable", NA, 56, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort78dep06pearcetotalnat.csv", "Total", "Mortality - Pearce Amenable", NA, 78, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort910dep06pearcetotalnat.csv", "Total", "Mortality - Pearce Amenable", NA, 910, "New Zealand", "National"); a = a + 1

#alldata[a,] = c("n", ",", "data/numerator/mort0pearcemaorinat.csv", "Maori", "Mortality - Pearce Amenable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/dep/mort1dep06pearcemaorinat.csv", "Maori", "Mortality - Pearce Amenable", NA, 1, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort2dep06pearcemaorinat.csv", "Maori", "Mortality - Pearce Amenable", NA, 2, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort3dep06pearcemaorinat.csv", "Maori", "Mortality - Pearce Amenable", NA, 3, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort4dep06pearcemaorinat.csv", "Maori", "Mortality - Pearce Amenable", NA, 4, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort5dep06pearcemaorinat.csv", "Maori", "Mortality - Pearce Amenable", NA, 5, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort6dep06pearcemaorinat.csv", "Maori", "Mortality - Pearce Amenable", NA, 6, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort7dep06pearcemaorinat.csv", "Maori", "Mortality - Pearce Amenable", NA, 7, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort8dep06pearcemaorinat.csv", "Maori", "Mortality - Pearce Amenable", NA, 8, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort9dep06pearcemaorinat.csv", "Maori", "Mortality - Pearce Amenable", NA, 9, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort10dep06pearcemaorinat.csv", "Maori", "Mortality - Pearce Amenable", NA, 10, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort12dep06pearcemaorinat.csv", "Maori", "Mortality - Pearce Amenable", NA, 12, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort34dep06pearcemaorinat.csv", "Maori", "Mortality - Pearce Amenable", NA, 34, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort56dep06pearcemaorinat.csv", "Maori", "Mortality - Pearce Amenable", NA, 56, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort78dep06pearcemaorinat.csv", "Maori", "Mortality - Pearce Amenable", NA, 78, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort910dep06pearcemaorinat.csv", "Maori", "Mortality - Pearce Amenable", NA, 910, "New Zealand", "National"); a = a + 1

#alldata[a,] = c("n", ",", "data/numerator/mort0pearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Amenable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/dep/mort1dep06pearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Amenable", NA, 1, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort2dep06pearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Amenable", NA, 2, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort3dep06pearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Amenable", NA, 3, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort4dep06pearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Amenable", NA, 4, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort5dep06pearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Amenable", NA, 5, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort6dep06pearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Amenable", NA, 6, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort7dep06pearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Amenable", NA, 7, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort8dep06pearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Amenable", NA, 8, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort9dep06pearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Amenable", NA, 9, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort10dep06pearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Amenable", NA, 10, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort12dep06pearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Amenable", NA, 12, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort34dep06pearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Amenable", NA, 34, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort56dep06pearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Amenable", NA, 56, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort78dep06pearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Amenable", NA, 78, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort910dep06pearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Amenable", NA, 910, "New Zealand", "National"); a = a + 1

#alldata[a,] = c("n", ",", "data/numerator/mort0notpearcetotalnat.csv", "Total", "Mortality - Pearce Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/dep/mort1dep06notpearcetotalnat.csv", "Total", "Mortality - Pearce Non-Amenable", NA, 1, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort2dep06notpearcetotalnat.csv", "Total", "Mortality - Pearce Non-Amenable", NA, 2, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort3dep06notpearcetotalnat.csv", "Total", "Mortality - Pearce Non-Amenable", NA, 3, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort4dep06notpearcetotalnat.csv", "Total", "Mortality - Pearce Non-Amenable", NA, 4, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort5dep06notpearcetotalnat.csv", "Total", "Mortality - Pearce Non-Amenable", NA, 5, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort6dep06notpearcetotalnat.csv", "Total", "Mortality - Pearce Non-Amenable", NA, 6, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort7dep06notpearcetotalnat.csv", "Total", "Mortality - Pearce Non-Amenable", NA, 7, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort8dep06notpearcetotalnat.csv", "Total", "Mortality - Pearce Non-Amenable", NA, 8, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort9dep06notpearcetotalnat.csv", "Total", "Mortality - Pearce Non-Amenable", NA, 9, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort10dep06notpearcetotalnat.csv", "Total", "Mortality - Pearce Non-Amenable", NA, 10, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort12dep06notpearcetotalnat.csv", "Total", "Mortality - Pearce Non-Amenable", NA, 12, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort34dep06notpearcetotalnat.csv", "Total", "Mortality - Pearce Non-Amenable", NA, 34, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort56dep06notpearcetotalnat.csv", "Total", "Mortality - Pearce Non-Amenable", NA, 56, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort78dep06notpearcetotalnat.csv", "Total", "Mortality - Pearce Non-Amenable", NA, 78, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort910dep06notpearcetotalnat.csv", "Total", "Mortality - Pearce Non-Amenable", NA, 910, "New Zealand", "National"); a = a + 1

#alldata[a,] = c("n", ",", "data/numerator/mort0notpearcemaorinat.csv", "Maori", "Mortality - Pearce Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/dep/mort1dep06notpearcemaorinat.csv", "Maori", "Mortality - Pearce Non-Amenable", NA, 1, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort2dep06notpearcemaorinat.csv", "Maori", "Mortality - Pearce Non-Amenable", NA, 2, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort3dep06notpearcemaorinat.csv", "Maori", "Mortality - Pearce Non-Amenable", NA, 3, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort4dep06notpearcemaorinat.csv", "Maori", "Mortality - Pearce Non-Amenable", NA, 4, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort5dep06notpearcemaorinat.csv", "Maori", "Mortality - Pearce Non-Amenable", NA, 5, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort6dep06notpearcemaorinat.csv", "Maori", "Mortality - Pearce Non-Amenable", NA, 6, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort7dep06notpearcemaorinat.csv", "Maori", "Mortality - Pearce Non-Amenable", NA, 7, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort8dep06notpearcemaorinat.csv", "Maori", "Mortality - Pearce Non-Amenable", NA, 8, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort9dep06notpearcemaorinat.csv", "Maori", "Mortality - Pearce Non-Amenable", NA, 9, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort10dep06notpearcemaorinat.csv", "Maori", "Mortality - Pearce Non-Amenable", NA, 10, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort12dep06notpearcemaorinat.csv", "Maori", "Mortality - Pearce Non-Amenable", NA, 12, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort34dep06notpearcemaorinat.csv", "Maori", "Mortality - Pearce Non-Amenable", NA, 34, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort56dep06notpearcemaorinat.csv", "Maori", "Mortality - Pearce Non-Amenable", NA, 56, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort78dep06notpearcemaorinat.csv", "Maori", "Mortality - Pearce Non-Amenable", NA, 78, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort910dep06notpearcemaorinat.csv", "Maori", "Mortality - Pearce Non-Amenable", NA, 910, "New Zealand", "National"); a = a + 1

#alldata[a,] = c("n", ",", "data/numerator/mort0notpearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/dep/mort1dep06notpearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Non-Amenable", NA, 1, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort2dep06notpearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Non-Amenable", NA, 2, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort3dep06notpearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Non-Amenable", NA, 3, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort4dep06notpearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Non-Amenable", NA, 4, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort5dep06notpearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Non-Amenable", NA, 5, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort6dep06notpearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Non-Amenable", NA, 6, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort7dep06notpearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Non-Amenable", NA, 7, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort8dep06notpearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Non-Amenable", NA, 8, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort9dep06notpearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Non-Amenable", NA, 9, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort10dep06notpearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Non-Amenable", NA, 10, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort12dep06notpearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Non-Amenable", NA, 12, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort34dep06notpearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Non-Amenable", NA, 34, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort56dep06notpearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Non-Amenable", NA, 56, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort78dep06notpearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Non-Amenable", NA, 78, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort910dep06notpearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Non-Amenable", NA, 910, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/mortpearcetotalnat.csv", "Total", "Mortality - Pearce Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortpearcemaorinat.csv", "Maori", "Mortality - Pearce Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortpearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Amenable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/mortnotpearcetotalnat.csv", "Total", "Mortality - Pearce Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnotpearcemaorinat.csv", "Maori", "Mortality - Pearce Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnotpearcenonmaorinat.csv", "Non-Maori", "Mortality - Pearce Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/mortpearcepacificnat.csv", "Pacific", "Mortality - Pearce Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortpearcenonpacificnat.csv", "Non-Pacific", "Mortality - Pearce Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortpearceasiannat.csv", "Asian", "Mortality - Pearce Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortpearcenonasiannat.csv", "Non-Asian", "Mortality - Pearce Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortpearcenonpacificnonmaorinat.csv", "Non-Pacific-Non-Maori", "Mortality - Pearce Amenable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/mortnotpearcepacificnat.csv", "Pacific", "Mortality - Pearce Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnotpearcenonpacificnat.csv", "Non-Pacific", "Mortality - Pearce Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnotpearceasiannat.csv", "Asian", "Mortality - Pearce Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnotpearcenonasiannat.csv", "Non-Asian", "Mortality - Pearce Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnotpearcenonpacificnonmaorinat.csv", "Non-Pacific-Non-Maori", "Mortality - Pearce Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1

#alldata[a,] = c("n", ",", "data/numerator/mort0raymontavtotalnat.csv", "Total", "Mortality - Raymont Avoidable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/dep/mort1dep06raymontavtotalnat.csv", "Total", "Mortality - Raymont Avoidable", NA, 1, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort2dep06raymontavtotalnat.csv", "Total", "Mortality - Raymont Avoidable", NA, 2, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort3dep06raymontavtotalnat.csv", "Total", "Mortality - Raymont Avoidable", NA, 3, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort4dep06raymontavtotalnat.csv", "Total", "Mortality - Raymont Avoidable", NA, 4, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort5dep06raymontavtotalnat.csv", "Total", "Mortality - Raymont Avoidable", NA, 5, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort6dep06raymontavtotalnat.csv", "Total", "Mortality - Raymont Avoidable", NA, 6, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort7dep06raymontavtotalnat.csv", "Total", "Mortality - Raymont Avoidable", NA, 7, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort8dep06raymontavtotalnat.csv", "Total", "Mortality - Raymont Avoidable", NA, 8, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort9dep06raymontavtotalnat.csv", "Total", "Mortality - Raymont Avoidable", NA, 9, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort10dep06raymontavtotalnat.csv", "Total", "Mortality - Raymont Avoidable", NA, 10, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort12dep06raymontavtotalnat.csv", "Total", "Mortality - Raymont Avoidable", NA, 12, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort34dep06raymontavtotalnat.csv", "Total", "Mortality - Raymont Avoidable", NA, 34, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort56dep06raymontavtotalnat.csv", "Total", "Mortality - Raymont Avoidable", NA, 56, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort78dep06raymontavtotalnat.csv", "Total", "Mortality - Raymont Avoidable", NA, 78, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort910dep06raymontavtotalnat.csv", "Total", "Mortality - Raymont Avoidable", NA, 910, "New Zealand", "National"); a = a + 1

#alldata[a,] = c("n", ",", "data/numerator/mort0raymontavmaorinat.csv", "Maori", "Mortality - Raymont Avoidable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/dep/mort1dep06raymontavmaorinat.csv", "Maori", "Mortality - Raymont Avoidable", NA, 1, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort2dep06raymontavmaorinat.csv", "Maori", "Mortality - Raymont Avoidable", NA, 2, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort3dep06raymontavmaorinat.csv", "Maori", "Mortality - Raymont Avoidable", NA, 3, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort4dep06raymontavmaorinat.csv", "Maori", "Mortality - Raymont Avoidable", NA, 4, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort5dep06raymontavmaorinat.csv", "Maori", "Mortality - Raymont Avoidable", NA, 5, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort6dep06raymontavmaorinat.csv", "Maori", "Mortality - Raymont Avoidable", NA, 6, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort7dep06raymontavmaorinat.csv", "Maori", "Mortality - Raymont Avoidable", NA, 7, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort8dep06raymontavmaorinat.csv", "Maori", "Mortality - Raymont Avoidable", NA, 8, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort9dep06raymontavmaorinat.csv", "Maori", "Mortality - Raymont Avoidable", NA, 9, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort10dep06raymontavmaorinat.csv", "Maori", "Mortality - Raymont Avoidable", NA, 10, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort12dep06raymontavmaorinat.csv", "Maori", "Mortality - Raymont Avoidable", NA, 12, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort34dep06raymontavmaorinat.csv", "Maori", "Mortality - Raymont Avoidable", NA, 34, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort56dep06raymontavmaorinat.csv", "Maori", "Mortality - Raymont Avoidable", NA, 56, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort78dep06raymontavmaorinat.csv", "Maori", "Mortality - Raymont Avoidable", NA, 78, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort910dep06raymontavmaorinat.csv", "Maori", "Mortality - Raymont Avoidable", NA, 910, "New Zealand", "National"); a = a + 1

#alldata[a,] = c("n", ",", "data/numerator/mort0raymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Avoidable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/dep/mort1dep06raymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Avoidable", NA, 1, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort2dep06raymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Avoidable", NA, 2, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort3dep06raymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Avoidable", NA, 3, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort4dep06raymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Avoidable", NA, 4, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort5dep06raymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Avoidable", NA, 5, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort6dep06raymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Avoidable", NA, 6, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort7dep06raymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Avoidable", NA, 7, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort8dep06raymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Avoidable", NA, 8, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort9dep06raymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Avoidable", NA, 9, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort10dep06raymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Avoidable", NA, 10, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort12dep06raymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Avoidable", NA, 12, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort34dep06raymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Avoidable", NA, 34, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort56dep06raymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Avoidable", NA, 56, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort78dep06raymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Avoidable", NA, 78, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort910dep06raymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Avoidable", NA, 910, "New Zealand", "National"); a = a + 1

#alldata[a,] = c("n", ",", "data/numerator/mort0notraymontavtotalnat.csv", "Total", "Mortality - Raymont Non-Avoidable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/dep/mort1dep06notraymontavtotalnat.csv", "Total", "Mortality - Raymont Non-Avoidable", NA, 1, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort2dep06notraymontavtotalnat.csv", "Total", "Mortality - Raymont Non-Avoidable", NA, 2, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort3dep06notraymontavtotalnat.csv", "Total", "Mortality - Raymont Non-Avoidable", NA, 3, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort4dep06notraymontavtotalnat.csv", "Total", "Mortality - Raymont Non-Avoidable", NA, 4, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort5dep06notraymontavtotalnat.csv", "Total", "Mortality - Raymont Non-Avoidable", NA, 5, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort6dep06notraymontavtotalnat.csv", "Total", "Mortality - Raymont Non-Avoidable", NA, 6, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort7dep06notraymontavtotalnat.csv", "Total", "Mortality - Raymont Non-Avoidable", NA, 7, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort8dep06notraymontavtotalnat.csv", "Total", "Mortality - Raymont Non-Avoidable", NA, 8, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort9dep06notraymontavtotalnat.csv", "Total", "Mortality - Raymont Non-Avoidable", NA, 9, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort10dep06notraymontavtotalnat.csv", "Total", "Mortality - Raymont Non-Avoidable", NA, 10, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort12dep06notraymontavtotalnat.csv", "Total", "Mortality - Raymont Non-Avoidable", NA, 12, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort34dep06notraymontavtotalnat.csv", "Total", "Mortality - Raymont Non-Avoidable", NA, 34, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort56dep06notraymontavtotalnat.csv", "Total", "Mortality - Raymont Non-Avoidable", NA, 56, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort78dep06notraymontavtotalnat.csv", "Total", "Mortality - Raymont Non-Avoidable", NA, 78, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort910dep06notraymontavtotalnat.csv", "Total", "Mortality - Raymont Non-Avoidable", NA, 910, "New Zealand", "National"); a = a + 1

#alldata[a,] = c("n", ",", "data/numerator/mort0notraymontavmaorinat.csv", "Maori", "Mortality - Raymont Non-Avoidable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/dep/mort1dep06notraymontavmaorinat.csv", "Maori", "Mortality - Raymont Non-Avoidable", NA, 1, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort2dep06notraymontavmaorinat.csv", "Maori", "Mortality - Raymont Non-Avoidable", NA, 2, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort3dep06notraymontavmaorinat.csv", "Maori", "Mortality - Raymont Non-Avoidable", NA, 3, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort4dep06notraymontavmaorinat.csv", "Maori", "Mortality - Raymont Non-Avoidable", NA, 4, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort5dep06notraymontavmaorinat.csv", "Maori", "Mortality - Raymont Non-Avoidable", NA, 5, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort6dep06notraymontavmaorinat.csv", "Maori", "Mortality - Raymont Non-Avoidable", NA, 6, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort7dep06notraymontavmaorinat.csv", "Maori", "Mortality - Raymont Non-Avoidable", NA, 7, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort8dep06notraymontavmaorinat.csv", "Maori", "Mortality - Raymont Non-Avoidable", NA, 8, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort9dep06notraymontavmaorinat.csv", "Maori", "Mortality - Raymont Non-Avoidable", NA, 9, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort10dep06notraymontavmaorinat.csv", "Maori", "Mortality - Raymont Non-Avoidable", NA, 10, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort12dep06notraymontavmaorinat.csv", "Maori", "Mortality - Raymont Non-Avoidable", NA, 12, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort34dep06notraymontavmaorinat.csv", "Maori", "Mortality - Raymont Non-Avoidable", NA, 34, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort56dep06notraymontavmaorinat.csv", "Maori", "Mortality - Raymont Non-Avoidable", NA, 56, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort78dep06notraymontavmaorinat.csv", "Maori", "Mortality - Raymont Non-Avoidable", NA, 78, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort910dep06notraymontavmaorinat.csv", "Maori", "Mortality - Raymont Non-Avoidable", NA, 910, "New Zealand", "National"); a = a + 1

#alldata[a,] = c("n", ",", "data/numerator/mort0notraymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Avoidable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/dep/mort1dep06notraymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Avoidable", NA, 1, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort2dep06notraymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Avoidable", NA, 2, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort3dep06notraymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Avoidable", NA, 3, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort4dep06notraymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Avoidable", NA, 4, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort5dep06notraymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Avoidable", NA, 5, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort6dep06notraymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Avoidable", NA, 6, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort7dep06notraymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Avoidable", NA, 7, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort8dep06notraymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Avoidable", NA, 8, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort9dep06notraymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Avoidable", NA, 9, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort10dep06notraymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Avoidable", NA, 10, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort12dep06notraymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Avoidable", NA, 12, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort34dep06notraymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Avoidable", NA, 34, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort56dep06notraymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Avoidable", NA, 56, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort78dep06notraymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Avoidable", NA, 78, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort910dep06notraymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Avoidable", NA, 910, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/mortraymontavtotalnat.csv", "Total", "Mortality - Raymont Avoidable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortraymontavmaorinat.csv", "Maori", "Mortality - Raymont Avoidable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortraymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Avoidable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/mortnotraymontavtotalnat.csv", "Total", "Mortality - Raymont Non-Avoidable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnotraymontavmaorinat.csv", "Maori", "Mortality - Raymont Non-Avoidable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnotraymontavnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Avoidable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/mortraymontavpacificnat.csv", "Pacific", "Mortality - Raymont Avoidable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortraymontavnonpacificnat.csv", "Non-Pacific", "Mortality - Raymont Avoidable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortraymontavasiannat.csv", "Asian", "Mortality - Raymont Avoidable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortraymontavnonasiannat.csv", "Non-Asian", "Mortality - Raymont Avoidable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortraymontavnonpacificnonmaorinat.csv", "Non-Pacific-Non-Maori", "Mortality - Raymont Avoidable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/mortnotraymontavpacificnat.csv", "Pacific", "Mortality - Raymont Non-Avoidable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnotraymontavnonpacificnat.csv", "Non-Pacific", "Mortality - Raymont Non-Avoidable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnotraymontavasiannat.csv", "Asian", "Mortality - Raymont Non-Avoidable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnotraymontavnonasiannat.csv", "Non-Asian", "Mortality - Raymont Non-Avoidable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnotraymontavnonpacificnonmaorinat.csv", "Non-Pacific-Non-Maori", "Mortality - Raymont Non-Avoidable", NA, NA, "New Zealand", "National"); a = a + 1

#alldata[a,] = c("n", ",", "data/numerator/mort0raymontamtotalnat.csv", "Total", "Mortality - Raymont Amenable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/dep/mort1dep06raymontamtotalnat.csv", "Total", "Mortality - Raymont Amenable", NA, 1, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort2dep06raymontamtotalnat.csv", "Total", "Mortality - Raymont Amenable", NA, 2, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort3dep06raymontamtotalnat.csv", "Total", "Mortality - Raymont Amenable", NA, 3, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort4dep06raymontamtotalnat.csv", "Total", "Mortality - Raymont Amenable", NA, 4, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort5dep06raymontamtotalnat.csv", "Total", "Mortality - Raymont Amenable", NA, 5, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort6dep06raymontamtotalnat.csv", "Total", "Mortality - Raymont Amenable", NA, 6, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort7dep06raymontamtotalnat.csv", "Total", "Mortality - Raymont Amenable", NA, 7, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort8dep06raymontamtotalnat.csv", "Total", "Mortality - Raymont Amenable", NA, 8, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort9dep06raymontamtotalnat.csv", "Total", "Mortality - Raymont Amenable", NA, 9, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort10dep06raymontamtotalnat.csv", "Total", "Mortality - Raymont Amenable", NA, 10, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort12dep06raymontamtotalnat.csv", "Total", "Mortality - Raymont Amenable", NA, 12, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort34dep06raymontamtotalnat.csv", "Total", "Mortality - Raymont Amenable", NA, 34, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort56dep06raymontamtotalnat.csv", "Total", "Mortality - Raymont Amenable", NA, 56, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort78dep06raymontamtotalnat.csv", "Total", "Mortality - Raymont Amenable", NA, 78, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort910dep06raymontamtotalnat.csv", "Total", "Mortality - Raymont Amenable", NA, 910, "New Zealand", "National"); a = a + 1

#alldata[a,] = c("n", ",", "data/numerator/mort0raymontammaorinat.csv", "Maori", "Mortality - Raymont Amenable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/dep/mort1dep06raymontammaorinat.csv", "Maori", "Mortality - Raymont Amenable", NA, 1, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort2dep06raymontammaorinat.csv", "Maori", "Mortality - Raymont Amenable", NA, 2, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort3dep06raymontammaorinat.csv", "Maori", "Mortality - Raymont Amenable", NA, 3, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort4dep06raymontammaorinat.csv", "Maori", "Mortality - Raymont Amenable", NA, 4, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort5dep06raymontammaorinat.csv", "Maori", "Mortality - Raymont Amenable", NA, 5, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort6dep06raymontammaorinat.csv", "Maori", "Mortality - Raymont Amenable", NA, 6, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort7dep06raymontammaorinat.csv", "Maori", "Mortality - Raymont Amenable", NA, 7, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort8dep06raymontammaorinat.csv", "Maori", "Mortality - Raymont Amenable", NA, 8, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort9dep06raymontammaorinat.csv", "Maori", "Mortality - Raymont Amenable", NA, 9, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort10dep06raymontammaorinat.csv", "Maori", "Mortality - Raymont Amenable", NA, 10, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort12dep06raymontammaorinat.csv", "Maori", "Mortality - Raymont Amenable", NA, 12, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort34dep06raymontammaorinat.csv", "Maori", "Mortality - Raymont Amenable", NA, 34, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort56dep06raymontammaorinat.csv", "Maori", "Mortality - Raymont Amenable", NA, 56, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort78dep06raymontammaorinat.csv", "Maori", "Mortality - Raymont Amenable", NA, 78, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort910dep06raymontammaorinat.csv", "Maori", "Mortality - Raymont Amenable", NA, 910, "New Zealand", "National"); a = a + 1

#alldata[a,] = c("n", ",", "data/numerator/mort0raymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Amenable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/dep/mort1dep06raymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Amenable", NA, 1, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort2dep06raymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Amenable", NA, 2, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort3dep06raymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Amenable", NA, 3, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort4dep06raymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Amenable", NA, 4, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort5dep06raymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Amenable", NA, 5, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort6dep06raymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Amenable", NA, 6, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort7dep06raymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Amenable", NA, 7, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort8dep06raymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Amenable", NA, 8, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort9dep06raymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Amenable", NA, 9, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort10dep06raymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Amenable", NA, 10, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort12dep06raymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Amenable", NA, 12, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort34dep06raymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Amenable", NA, 34, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort56dep06raymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Amenable", NA, 56, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort78dep06raymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Amenable", NA, 78, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort910dep06raymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Amenable", NA, 910, "New Zealand", "National"); a = a + 1

#alldata[a,] = c("n", ",", "data/numerator/mort0notraymontamtotalnat.csv", "Total", "Mortality - Raymont Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/dep/mort1dep06notraymontamtotalnat.csv", "Total", "Mortality - Raymont Non-Amenable", NA, 1, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort2dep06notraymontamtotalnat.csv", "Total", "Mortality - Raymont Non-Amenable", NA, 2, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort3dep06notraymontamtotalnat.csv", "Total", "Mortality - Raymont Non-Amenable", NA, 3, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort4dep06notraymontamtotalnat.csv", "Total", "Mortality - Raymont Non-Amenable", NA, 4, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort5dep06notraymontamtotalnat.csv", "Total", "Mortality - Raymont Non-Amenable", NA, 5, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort6dep06notraymontamtotalnat.csv", "Total", "Mortality - Raymont Non-Amenable", NA, 6, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort7dep06notraymontamtotalnat.csv", "Total", "Mortality - Raymont Non-Amenable", NA, 7, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort8dep06notraymontamtotalnat.csv", "Total", "Mortality - Raymont Non-Amenable", NA, 8, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort9dep06notraymontamtotalnat.csv", "Total", "Mortality - Raymont Non-Amenable", NA, 9, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort10dep06notraymontamtotalnat.csv", "Total", "Mortality - Raymont Non-Amenable", NA, 10, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort12dep06notraymontamtotalnat.csv", "Total", "Mortality - Raymont Non-Amenable", NA, 12, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort34dep06notraymontamtotalnat.csv", "Total", "Mortality - Raymont Non-Amenable", NA, 34, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort56dep06notraymontamtotalnat.csv", "Total", "Mortality - Raymont Non-Amenable", NA, 56, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort78dep06notraymontamtotalnat.csv", "Total", "Mortality - Raymont Non-Amenable", NA, 78, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort910dep06notraymontamtotalnat.csv", "Total", "Mortality - Raymont Non-Amenable", NA, 910, "New Zealand", "National"); a = a + 1

#alldata[a,] = c("n", ",", "data/numerator/mort0notraymontammaorinat.csv", "Maori", "Mortality - Raymont Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/dep/mort1dep06notraymontammaorinat.csv", "Maori", "Mortality - Raymont Non-Amenable", NA, 1, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort2dep06notraymontammaorinat.csv", "Maori", "Mortality - Raymont Non-Amenable", NA, 2, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort3dep06notraymontammaorinat.csv", "Maori", "Mortality - Raymont Non-Amenable", NA, 3, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort4dep06notraymontammaorinat.csv", "Maori", "Mortality - Raymont Non-Amenable", NA, 4, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort5dep06notraymontammaorinat.csv", "Maori", "Mortality - Raymont Non-Amenable", NA, 5, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort6dep06notraymontammaorinat.csv", "Maori", "Mortality - Raymont Non-Amenable", NA, 6, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort7dep06notraymontammaorinat.csv", "Maori", "Mortality - Raymont Non-Amenable", NA, 7, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort8dep06notraymontammaorinat.csv", "Maori", "Mortality - Raymont Non-Amenable", NA, 8, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort9dep06notraymontammaorinat.csv", "Maori", "Mortality - Raymont Non-Amenable", NA, 9, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort10dep06notraymontammaorinat.csv", "Maori", "Mortality - Raymont Non-Amenable", NA, 10, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort12dep06notraymontammaorinat.csv", "Maori", "Mortality - Raymont Non-Amenable", NA, 12, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort34dep06notraymontammaorinat.csv", "Maori", "Mortality - Raymont Non-Amenable", NA, 34, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort56dep06notraymontammaorinat.csv", "Maori", "Mortality - Raymont Non-Amenable", NA, 56, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort78dep06notraymontammaorinat.csv", "Maori", "Mortality - Raymont Non-Amenable", NA, 78, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort910dep06notraymontammaorinat.csv", "Maori", "Mortality - Raymont Non-Amenable", NA, 910, "New Zealand", "National"); a = a + 1

#alldata[a,] = c("n", ",", "data/numerator/mort0notraymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/dep/mort1dep06notraymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Amenable", NA, 1, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort2dep06notraymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Amenable", NA, 2, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort3dep06notraymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Amenable", NA, 3, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort4dep06notraymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Amenable", NA, 4, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort5dep06notraymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Amenable", NA, 5, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort6dep06notraymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Amenable", NA, 6, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort7dep06notraymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Amenable", NA, 7, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort8dep06notraymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Amenable", NA, 8, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort9dep06notraymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Amenable", NA, 9, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort10dep06notraymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Amenable", NA, 10, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort12dep06notraymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Amenable", NA, 12, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort34dep06notraymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Amenable", NA, 34, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort56dep06notraymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Amenable", NA, 56, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort78dep06notraymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Amenable", NA, 78, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/dep/mort910dep06notraymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Amenable", NA, 910, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/mortraymontamtotalnat.csv", "Total", "Mortality - Raymont Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortraymontammaorinat.csv", "Maori", "Mortality - Raymont Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortraymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Amenable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/mortnotraymontamtotalnat.csv", "Total", "Mortality - Raymont Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnotraymontammaorinat.csv", "Maori", "Mortality - Raymont Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnotraymontamnonmaorinat.csv", "Non-Maori", "Mortality - Raymont Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/mortraymontampacificnat.csv", "Pacific", "Mortality - Raymont Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortraymontamnonpacificnat.csv", "Non-Pacific", "Mortality - Raymont Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortraymontamasiannat.csv", "Asian", "Mortality - Raymont Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortraymontamnonasiannat.csv", "Non-Asian", "Mortality - Raymont Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortraymontamnonpacificnonmaorinat.csv", "Non-Pacific-Non-Maori", "Mortality - Raymont Amenable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/mortnotraymontampacificnat.csv", "Pacific", "Mortality - Raymont Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnotraymontamnonpacificnat.csv", "Non-Pacific", "Mortality - Raymont Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnotraymontamasiannat.csv", "Asian", "Mortality - Raymont Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnotraymontamnonasiannat.csv", "Non-Asian", "Mortality - Raymont Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnotraymontamnonpacificnonmaorinat.csv", "Non-Pacific-Non-Maori", "Mortality - Raymont Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1



alldata[a,] = c("n", ",", "data/numerator/morttobiastotalnat.csv", "Total", "Mortality - Tobias Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/morttobiasmaorinat.csv", "Maori", "Mortality - Tobias Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/morttobiasnonmaorinat.csv", "Non-Maori", "Mortality - Tobias Amenable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/mortnottobiastotalnat.csv", "Total", "Mortality - Tobias Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnottobiasmaorinat.csv", "Maori", "Mortality - Tobias Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnottobiasnonmaorinat.csv", "Non-Maori", "Mortality - Tobias Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/morttobiaspacificnat.csv", "Pacific", "Mortality - Tobias Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/morttobiasnonpacificnat.csv", "Non-Pacific", "Mortality - Tobias Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/morttobiasasiannat.csv", "Asian", "Mortality - Tobias Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/morttobiasnonasiannat.csv", "Non-Asian", "Mortality - Tobias Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/morttobiasnonpacificnonmaorinat.csv", "Non-Pacific-Non-Maori", "Mortality - Tobias Amenable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/mortnottobiaspacificnat.csv", "Pacific", "Mortality - Tobias Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnottobiasnonpacificnat.csv", "Non-Pacific", "Mortality - Tobias Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnottobiasasiannat.csv", "Asian", "Mortality - Tobias Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnottobiasnonasiannat.csv", "Non-Asian", "Mortality - Tobias Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnottobiasnonpacificnonmaorinat.csv", "Non-Pacific-Non-Maori", "Mortality - Tobias Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/mortmohtotalnat.csv", "Total", "Mortality - MoH Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortmohmaorinat.csv", "Maori", "Mortality - MoH Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortmohnonmaorinat.csv", "Non-Maori", "Mortality - MoH Amenable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/mortnotmohtotalnat.csv", "Total", "Mortality - MoH Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnotmohmaorinat.csv", "Maori", "Mortality - MoH Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnotmohnonmaorinat.csv", "Non-Maori", "Mortality - MoH Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/mortmohpacificnat.csv", "Pacific", "Mortality - MoH Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortmohnonpacificnat.csv", "Non-Pacific", "Mortality - MoH Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortmohasiannat.csv", "Asian", "Mortality - MoH Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortmohnonasiannat.csv", "Non-Asian", "Mortality - MoH Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortmohnonpacificnonmaorinat.csv", "Non-Pacific-Non-Maori", "Mortality - MoH Amenable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/mortnotmohpacificnat.csv", "Pacific", "Mortality - MoH Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnotmohnonpacificnat.csv", "Non-Pacific", "Mortality - MoH Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnotmohasiannat.csv", "Asian", "Mortality - MoH Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnotmohnonasiannat.csv", "Non-Asian", "Mortality - MoH Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnotmohnonpacificnonmaorinat.csv", "Non-Pacific-Non-Maori", "Mortality - MoH Non-Amenable", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ";", "data/numerator/gastric08totalnat.csv", "Total", "Cancer - Gastric Death", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ";", "data/numerator/gastric08maorinat.csv", "Maori", "Cancer - Gastric Death", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ";", "data/numerator/gastric08nonmaorinat.csv", "Non-Maori", "Cancer - Gastric Death", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/gastric08totalnatinc.csv", "Total", "Cancer - Gastric Incidence", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/gastric08maorinatinc.csv", "Maori", "Cancer - Gastric Incidence", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/gastric08nonmaorinatinc.csv", "Non-Maori", "Cancer - Gastric Incidence", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/priority12totalnat.csv", "Total", "Cancer - Priority", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/priority12maorinat.csv", "Maori", "Cancer - Priority", NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("n", ";", "data/numerator/priority12nonmaorinat.csv", "Non-Maori", "Cancer - Priority", NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("d", ";", "data/denominator/popestdisc95totalnat.csv", "Total", NA, NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("d", ";", "data/denominator/popestdisc90maorinat.csv", "Maori", NA, NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("d", ";", "data/denominator/popestdisc90nonmaorinat.csv", "Non-Maori", NA, NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("d", ";", "data/denominator/popest91totalnat.csv", "Total", NA, NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("d", ";", "data/denominator/popest91maorinat.csv", "Maori", NA, NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("d", ";", "data/denominator/popest91nonmaorinat.csv", "Non-Maori", NA, NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("d", ";", "data/denominator/popcentotalnat.csv", "Total", NA, NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("d", ";", "data/denominator/popcenmaorinat.csv", "Maori", NA, NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("d", ";", "data/denominator/popcennonmaorinat.csv", "Non-Maori", NA, NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("d", ";", "data/denominator/popcenpacificnat.csv", "Pacific", NA, NA, NA, "New Zealand", "National"); a = a + 1
alldata[a,] = c("d", ";", "data/denominator/popcenasiannat.csv", "Asian", NA, NA, NA, "New Zealand", "National"); a = a + 1

alldata[a,] = c("d", ";", "data/denominator/poptotalreg.csv", "Total", NA, NA, NA, "New Zealand", "Regional Council"); a = a + 1
alldata[a,] = c("d", ";", "data/denominator/poptotaltla.csv", "Total", NA, NA, NA, "New Zealand", "TLA"); a = a + 1
alldata[a,] = c("d", ";", "data/denominator/poptotaldhb.csv", "Total", NA, NA, NA, "New Zealand", "DHB"); a = a + 1


alldata[a,] = c("n", ",", "data/numerator/morttotalreg.csv", "Total", "Mortality - Total", NA, NA, "New Zealand", "Regional Council"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortmaorireg.csv", "Maori", "Mortality - Total", NA, NA, "New Zealand", "Regional Council"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnonmaorireg.csv", "Non-Maori", "Mortality - Total", NA, NA, "New Zealand", "Regional Council"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/morttotaltla.csv", "Total", "Mortality - Total", NA, NA, "New Zealand", "TLA"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortmaoritla.csv", "Maori", "Mortality - Total", NA, NA, "New Zealand", "TLA"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnonmaoritla.csv", "Non-Maori", "Mortality - Total", NA, NA, "New Zealand", "TLA"); a = a + 1

alldata[a,] = c("n", ",", "data/numerator/morttotaldhb.csv", "Total", "Mortality - Total", NA, NA, "New Zealand", "DHB"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortmaoridhb.csv", "Maori", "Mortality - Total", NA, NA, "New Zealand", "DHB"); a = a + 1
alldata[a,] = c("n", ",", "data/numerator/mortnonmaoridhb.csv", "Non-Maori", "Mortality - Total", NA, NA, "New Zealand", "DHB"); a = a + 1

alldata = alldata[,-6]
write.csv(alldata, file = "csvs.csv")