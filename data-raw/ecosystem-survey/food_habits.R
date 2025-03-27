# this captures the SQL necessary to generate the food habits csv that is used 
# to generate the food_habits.rda
if (F){
txt <- paste0("SELECT I.DATASOURCE,I.MISSION,I.SETNO,I.SDATE,I.STIME,I.STRAT,I.BOTTOM_TEMPERATURE,I.DEPTH,I.GEAR,I.SLATDD,I.SLONGDD,I.NAFO_ZONE,I.NAFO_SUBUNIT,
D.SPEC,D.FSHNO,D.FWT,D.FLEN,D.STOWGT,D.EMPTYWGT,D.FULLNESS,D.FGEN,
S.PREYSPECCD,S.PWT,S.PLEN,S.PNUM,S.DIGESTION
FROM 
MAR_FOODHABITS.SDDET D, 
MAR_FOODHABITS.SDINF  I, 
MAR_FOODHABITS.SDSTO  S
WHERE 
D.MISSION = I.MISSION AND D.SETNO = I.SETNO AND 
S.MISSION (+) = D.MISSION AND 
S.SETNO (+) = D.SETNO AND 
S.SPEC (+)= D.SPEC AND
S.FSHNO (+)= D.FSHNO AND
I.DATASOURCE = 'GS'
ORDER BY S.STO_KEY;")
}
food_habits <- read.csv("data-raw/ecosystem-survey/food_habits.csv")
save(food_habits, file = "food_habits.rda")
usethis::use_data(food_habits, overwrite = TRUE)
