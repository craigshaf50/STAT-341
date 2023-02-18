library(regclass)

#Question 5
data("EX2.CENSUS")
associate(ResponseRate~HomeownerHH,data=EX2.CENSUS)

#Question 6
data("EX2.TIPS")
associate(Size_of_Party~Smoker,data=EX2.TIPS)

associate(Size_of_Party~Smoker,data=EX2.TIPS,permutations = 5000)

associate(Tip_in_USD~Bill_in_USD,data=EX2.TIPS)

associate(Day_Night~Smoker,data=EX2.TIPS)
