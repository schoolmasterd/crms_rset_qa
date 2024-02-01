###CRMS RSET QA\QC tool###


#set path to folder
path <- "path/to/crms_rset_qa-main"
setwd(path)

in_path <- "Data/"
out_path <- "Output/"
input_file <- "CRMS_RSET_Sp23_CES_Jan.csv"
db_file <- "CRMS_RSET_Hist_LRO.csv"
#read in the new and old data
input_df <- read.csv(paste0(in_path, input_file), check.names = F)
db_df <- read.csv(paste0(in_path, db_file), check.names = F)

#load some useful functions
library(xtable)

#Tools
tool_path <- "Tools/"
preamb <-
  paste(readLines(paste0(tool_path, "html_preamble.txt")), collapse = "\n")

# UNIQUE STATIONS
tab_1 <-
  xtable(data.frame(
    `Current Stations` = unique(input_df$`Station ID`),
    check.names = F
  ),
  caption = "1. SUMMARY INFORMATION - UNIQUE STATIONS")

# FIRST DATE AND LAST DATE IN DATA SET
dates <-
  as.POSIXct(input_df$`Sample Date (mm/dd/yyyy)`, format = "%m/%d/%Y")
tab_2 <- xtable(data.frame(
  `First Date` = format(min(dates), "%m/%d/%Y"),
  `Last Date` = format(max(dates), "%m/%d/%Y"),
  check.names = F
),
caption = "2. FIRST DATE AND LAST DATE IN DATA SET")


#	SET ID AND PREVIOUS SET ID DIFFERENT (vs more recent in db)
new_stats <- unique(input_df$`Station ID`)
db_new <- db_df[db_df$`Station ID` %in% new_stats, ]
#change date field to date object
db_new$`Sample Date (mm/dd/yyyy)` <-
  as.POSIXct(db_new$`Sample Date (mm/dd/yyyy)`, format = "%m/%d/%Y")
last_date <- as.POSIXct(
  tapply(db_new$`Sample Date (mm/dd/yyyy)`,
         INDEX = db_new$`Station ID`, function(x)
           max(x)),
  origin = "1970-01-01"
)

#ans<-rep(NA,length(last_date))
#names(ans)<-names(last_date)
tab_3_pre <- list()
k = 1
for (i in 1:length(last_date)) {
  who_old <-
    which(
      db_new$`Station ID` == names(last_date)[i] &
        db_new$`Sample Date (mm/dd/yyyy)` == last_date[i]
    )
  who_new <- which(input_df$`Station ID` == names(last_date)[i])
  ans <-
    all(input_df[who_new, "SETID"] %in% unique(db_new[who_old, "SET ID"]))
  if (!ans) {
    tab_3_pre[[k]] <-
      data.frame(
        `STATION` = input_df[who_new, "Station ID"],
        `DATE` = input_df[who_new, "Sample Date (mm/dd/yyyy)"],
        `SET ID` = input_df[who_new, "SETID"],
        `PREVIOUS SET ID` = db_new[who_old, "SET ID"],
        check.names = F
      )
    k = k + 1
  }
}
if (length(tab_3_pre) == 0)
  tab_3 <-
  xtable(
    data.frame(
      `STATION` = NA,
      `DATE` = NA,
      `SET ID` = NA,
      `PREVIOUS SET ID` = NA,
      check.names = F
    ),
    caption = '3. SET ID AND PREVIOUS SET ID DIFFERENT'
  ) else
  tab_3 <- xtable(do.call("rbind", tab_3_pre),
                  caption = '3. SET ID AND PREVIOUS SET ID DIFFERENT')

# PERSONNEL AND PREVIOUS PERSONNEL DIFFERENT (vs more recent in db)

tab_4_pre <- list()
k = 1
for (i in 1:length(last_date)) {
  who_old <-
    which(
      db_new$`Station ID` == names(last_date)[i] &
        db_new$`Sample Date (mm/dd/yyyy)` == last_date[i]
    )
  who_new <- which(input_df$`Station ID` == names(last_date)[i])
  new_reader <-
    sapply(strsplit(input_df[who_new, "Personnel"], ";"), "[", 1)
  old_reader <-
    sapply(strsplit(db_new[who_old, "Personnel"], ";"), "[", 1)
  
  ans <- all(new_reader == old_reader)
  if (!ans) {
    tab_4_pre[[k]] <-
      data.frame(
        `STATION` = input_df[who_new[1], "Station ID"],
        `DATE` = input_df[who_new[1], "Sample Date (mm/dd/yyyy)"],
        `PERSONNEL` = new_reader[1],
        `PREVIOUS PERSONNEL` = old_reader[1],
        check.names = F
      )
    k = k + 1
  }
}
if (length(tab_4_pre) == 0)
  tab_4 <-
  xtable(
    data.frame(
      `STATION` = NA,
      `DATE` = NA,
      `PERSONNEL` = NA,
      `PREVIOUS PERSONNEL` = NA,
      check.names = F
    ),
    caption = '4. PERSONNEL AND PREVIOUS PERSONNEL DIFFERENT'
  ) else
  tab_4 <- xtable(do.call("rbind", tab_4_pre),
                  caption = '4. PERSONNEL AND PREVIOUS PERSONNEL DIFFERENT')



#COLLAR HEIGHT DIFFERENT FROM PREVIOUS COLLAR

tab_5_pre <- list()
k = 1
for (i in 1:length(last_date)) {
  who_old <-
    which(
      db_new$`Station ID` == names(last_date)[i] &
        db_new$`Sample Date (mm/dd/yyyy)` == last_date[i]
    )
  who_new <- which(input_df$`Station ID` == names(last_date)[i])
  new_collar <- input_df[who_new, "Direction (Collar Number)"]
  old_collar <- db_new[who_old, "Direction (Collar Number)"]
  str_old <-
    sapply(strsplit(db_new$`Site Conditions`[who_old], ";"), function(x)
      x[[grep("Rod to collar", x)]])
  str_new <-
    sapply(strsplit(input_df$`Site Conditions`[who_new], ";"), function(x)
      x[[grep("Rod to collar", x)]])
  new_ht <- as.numeric(gsub(".*?([0-9]+).*", "\\1", str_new))
  old_ht <- as.numeric(gsub(".*?([0-9]+).*", "\\1", str_old))
  
  ans <- all(new_ht == old_ht)
  if (!ans) {
    tab_5_pre[[k]] <-
      data.frame(
        `STATION` = input_df[who_new[1], "Station ID"],
        `DATE` = input_df[who_new[1], "Sample Date (mm/dd/yyyy)"],
        `CURRENT COLLAR` = new_ht[1],
        `PREVIOUS COLLAR` = old_ht[1],
        check.names = F
      )
    k = k + 1
  }
}
if (length(tab_5_pre) == 0)
  tab_5 <-
  xtable(
    data.frame(
      `STATION` = NA,
      `DATE` = NA,
      `CURRENT COLLAR` = NA,
      `PREVIOUS COLLAR` = NA,
      check.names = F
    ),
    caption = '5. COLLAR HEIGHT DIFFERENT FROM PREVIOUS COLLAR HEIGHT'
  ) else
  tab_5 <- xtable(do.call("rbind", tab_5_pre),
                  caption =
                    '5. COLLAR HEIGHT DIFFERENT FROM PREVIOUS COLLAR HEIGHT')


#COLLAR DIRECTION DIFFERENT FROM PREVIOUS COLLAR DIRECTION
#and
#NEW COLLAR NOT UNIQUE
tab_6_pre <- list()
k = 1
tab_7_pre <- list()
j = 1
for (i in 1:length(last_date)) {
  who_old <-
    which(
      db_new$`Station ID` == names(last_date)[i] &
        db_new$`Sample Date (mm/dd/yyyy)` == last_date[i]
    )
  who_new <- which(input_df$`Station ID` == names(last_date)[i])
  new_collar <-
    input_df[who_new, c("Direction (Collar Number)", "Direction (Compass Degrees)")]
  old_collar <-
    db_new[who_old, c("Direction (Collar Number)", "Direction (Compass Degrees)")]
  
  ans <- (new_collar[, 1] == old_collar[, 1] &
            new_collar[, 2] == old_collar[, 2])
  if (!all(ans)) {
    tab_6_pre[[k]] <-
      data.frame(
        `STATION` = input_df[who_new[!ans], "Station ID"],
        `DATE` = input_df[who_new[!ans], "Sample Date (mm/dd/yyyy)"],
        `CURRENT COLLAR` = new_collar[!ans, 1],
        `PREVIOUS COLLAR` = old_collar[!ans, 1],
        `CURRENT COLLAR DIRECTION` = new_collar[!ans, 2],
        `PREVIOUS COLLAR DIRECTION` = old_collar[!ans, 2],
        `DIFFERENCE IN DIRECTION` = old_collar[!ans, 2] -
          new_collar[!ans, 2],
        check.names = F
      )
    k = k + 1
  }
  #old_collar[!duplicated(old_collar[,2]),2]==new_collar[!duplicated(new_collar[,2]),2]&old_collar[!duplicated(old_collar[,2]),1]!=new_collar[!duplicated(new_collar[,2]),1]
  ans2 <- old_collar[, 2] == new_collar[, 2] &
    old_collar[, 1] != new_collar[, 1]
  
  if (any(ans2)) {
    tab_7_pre[[j]] <-
      data.frame(
        `STATION` = input_df[who_new[ans2], "Station ID"],
        `DATE` = input_df[who_new[ans2], "Sample Date (mm/dd/yyyy)"],
        `CURRENT COLLAR` = new_collar[ans2, 1],
        `PIN NUMBER` = input_df[who_new[ans2], "Pin Number"],
        `CURRENT COLLAR DIRECTION` = new_collar[ans2, 2],
        check.names = F
      )
    j = j + 1
  }
  
}
if (length(tab_6_pre) == 0)
  tab_6 <-
  xtable(
    data.frame(
      `STATION` = NA,
      `DATE` = NA,
      `CURRENT COLLAR` = NA,
      `PREVIOUS COLLAR` = NA,
      `CURRENT COLLAR DIRECTION` =
        NA,
      `PREVIOUS COLLAR DIRECTION` = NA,
      `DIFFERENCE IN DIRECTION` = NA,
      check.names = F
    ),
    caption = '6. COLLAR DIRECTION DIFFERENT FROM PREVIOUS COLLAR DIRECTION'
  ) else
  tab_6 <- xtable(do.call("rbind", tab_6_pre),
                  caption = '6. COLLAR DIRECTION DIFFERENT FROM PREVIOUS COLLAR DIRECTION')

if (length(tab_7_pre) == 0)
  tab_7 <-
  xtable(
    data.frame(
      `STATION` = NA,
      `DATE` = NA,
      `CURRENT COLLAR` = NA,
      `PIN NUMBER` = NA,
      `CURRENT COLLAR DIRECTION` =
        NA,
      check.names = F
    ),
    caption = '7. NEW COLLAR NOT UNIQUE'
  ) else
  tab_7 <- xtable(do.call("rbind", tab_7_pre),
                  caption = '7. NEW COLLAR NOT UNIQUE')

#'NO VERIFIED PIN HEIGHT AND NO OBSERVATION COMMENT

ans <- !is.na(input_df$`Observed Pin Height (mm)`) &
  is.na(input_df$`Verified Pin Height (mm)`) &
  input_df$`Observation Comments` == ""

if (!any(ans))
  tab_8 <-
  xtable(
    data.frame(
      `STATION` = NA,
      `DATE` = NA,
      `PIN HEIGHT` = NA,
      `VERIFIED PIN HEIGHT` = NA,
      `COMMENT` = NA,
      check.names = F
    ),
    caption = '8. NO VERIFIED PIN HEIGHT AND NO OBSERVATION COMMENT'
  ) else
  tab_8 <-
  xtable(
    data.frame(
      `STATION` = input_df$`Station ID`[ans],
      `DATE` = input_df$`Sample Date (mm/dd/yyyy)`[ans],
      `PIN HEIGHT` =
        input_df$`Observed Pin Height (mm)`[ans],
      `VERIFIED PIN HEIGHT` = input_df$`Verified Pin Height (mm)`[ans],
      `COMMENT` =
        input_df$`Observation Comments`[ans],
      check.names = F
    ),
    caption =
      '8. NO VERIFIED PIN HEIGHT AND NO OBSERVATION COMMENT'
  )

#'COMMENT WHEN VERIFIED PIN HEIGHT DATA OMMITED
ans <- is.na(input_df$`Verified Pin Height (mm)`)
tab_9 <-
  xtable(
    data.frame(
      `STATION` = input_df$`Station ID`[ans],
      `DATE` = input_df$`Sample Date (mm/dd/yyyy)`[ans],
      `CURRENT COLLAR` = input_df$`Direction (Collar Number)`[ans],
      `PIN NUMBER` = input_df$`Pin Number`[ans],
      `COMMENT` = input_df$`Observation Comments`[ans],
      check.names = F
    ),
    caption = '9. COMMENT WHEN VERIFIED PIN HEIGHT DATA OMMITED'
  )

#'COMMENT WHEN OMITTED AND NOT OMITTED SAME'
#use ans from last
input_df$`Observation Comments`[ans]
site_id <- sapply(strsplit(input_df$`Station ID`, "-"), "[", 1)
names(input_df)
tmp <-
  split(input_df[, c(
    "Station ID",
    "Sample Date (mm/dd/yyyy)",
    "Direction (Collar Number)",
    "Pin Number",
    "Verified Pin Height (mm)",
    "Observation Comments"
  )], site_id)

get_em <-
  sapply(tmp, function(x)
    which(is.na(x[, "Verified Pin Height (mm)"]))[x[is.na(x[, "Verified Pin Height (mm)"]), "Observation Comments"] %in%
                                                    x[!is.na(x[, "Verified Pin Height (mm)"]), "Observation Comments"]])
ind <- unlist(sapply(1:length(tmp), function(x)
  get_em[[x]] + (x - 1) * 36))
tab_10 <-
  xtable(do.call("rbind", tmp)[ind, ], caption = "10. COMMENT WHEN OMITTED AND NOT OMITTED SAME")

#'HOLE INCONSISTENT WHEN OMITTED AND NOT OMITTED SAME'
ans <- do.call("rbind", tmp)[ind, ]
tab_11 <- xtable(ans[grep("hole", ans$`Observation Comments`), ],
                 caption = '11. HOLE INCONSISTENT WHEN OMITTED AND NOT OMITTED SAME')

#	'ESTABLISHMENT DATE DIFFERENT FOR COLLAR'
input_tmp <-
  input_df[which(input_df$`Pin Number` == 1), c("Station ID",
                                                "Establishment Date (mm/dd/yyyy)",
                                                "Direction (Collar Number)")]
input_tmp$`Establishment Date (mm/dd/yyyy)`<-as.POSIXct(input_tmp$`Establishment Date (mm/dd/yyyy)`,
                                                        format="%m/%d/%Y")
tmp <-
  split(db_new[, c(
    "Station ID",
    "Sample Date (mm/dd/yyyy)",
    "Establishment Date (mm/dd/yyyy)",
    "Direction (Collar Number)",
    "Pin Number"
  )], db_new$`Station ID`)
db_tmp <-
  db_new[unlist(lapply(1:length(last_date), function(x)
    (tmp[[x]]$`Sample Date (mm/dd/yyyy)` == last_date[x]))), ]

db_tmp <-
  db_tmp[which(input_df$`Pin Number` == 1), c("Station ID",
                                              "Establishment Date (mm/dd/yyyy)",
                                              "Direction (Collar Number)")]
db_tmp$`Establishment Date (mm/dd/yyyy)`<-as.POSIXct(db_tmp$`Establishment Date (mm/dd/yyyy)`,
                                                        format="%m/%d/%Y")
tst_db <-
  merge(input_tmp,
        db_tmp,
        by = c("Station ID", "Direction (Collar Number)"))
names(tst_db)[3:4] <- c("Estab Date (New)", "Estab Date (Database)")
tab_12 <-
  xtable(tst_db[which(tst_db$`Estab Date (New)` != tst_db$`Estab Date (Database)`), ], caption =
           "12. ESTABLISHMENT DATE DIFFERENT FOR COLLAR")

#'FIRST SAMPLE DATE FOR COLLAR NOT SAME AS ESTABLISHMENT DATE'

#format establishment data at date object
db_new$`Establishment Date (mm/dd/yyyy)` <-
  as.POSIXct(db_new$`Establishment Date (mm/dd/yyyy)`, format = "%m/%d/%Y")
db_new[which(
  db_new$`Sample Date (mm/dd/yyyy)` == db_new$`Establishment Date (mm/dd/yyyy)` &
    db_new$`Pin Number` == 1
), c("Station ID",
     "Establishment Date (mm/dd/yyyy)",
     "Direction (Collar Number)")]

input_tmp <-
  input_df[which(input_df$`Pin Number` == 1), c("Station ID",
                                                "Establishment Date (mm/dd/yyyy)",
                                                "Direction (Collar Number)")]

dts_match <- NULL
for (i in 1:dim(input_tmp)[1]) {
  who <-
    which(
      db_new$`Station ID` == input_tmp$`Station ID`[i] &
        db_new$`Direction (Collar Number)` == input_tmp$`Direction (Collar Number)`[i] &
        db_new$`Pin Number` == 1
    )
  dts_match[i] <-
    db_new[who, ]$`Establishment Date (mm/dd/yyyy)`[which.min(db_new[who, ]$`Establishment Date (mm/dd/yyyy)`)] ==
    db_new[who, ]$`Sample Date (mm/dd/yyyy)`[which.min(db_new[who, ]$`Establishment Date (mm/dd/yyyy)`)]
}
tab_13 <-
  xtable(input_tmp[!dts_match, ], caption = "13. FIRST SAMPLE DATE FOR COLLAR NOT SAME AS ESTABLISHMENT DATE")
#'PIN DIFFERENCE OUTSIDE CI'
#get collar means and sd
pin_mu <-
  tapply(
    input_df$`Verified Pin Height (mm)`,
    INDEX = list(input_df$`Station ID`, input_df$`Direction (Collar Number)`),
    mean,
    na.rm = T
  )
pin_sig <-
  tapply(
    input_df$`Verified Pin Height (mm)`,
    INDEX = list(input_df$`Station ID`, input_df$`Direction (Collar Number)`),
    sd,
    na.rm = T
  )

pin_date <-
  tapply(
    input_df$`Sample Date (mm/dd/yyyy)`,
    INDEX = list(input_df$`Station ID`, input_df$`Direction (Collar Number)`),
    unique,
    na.rm = T
  )
len <- dim(input_df)[1]
ans <- rep(NA, len)
for (i in 1:len) {
  ans[i] <-
    input_df$`Verified Pin Height (mm)`[i] > pin_mu[input_df$`Station ID`[i], as.character(input_df$`Direction (Collar Number)`[i])] +
    1.96 * pin_sig[input_df$`Station ID`[i], as.character(input_df$`Direction (Collar Number)`[i])] |
    input_df$`Verified Pin Height (mm)`[i] < pin_mu[input_df$`Station ID`[i], as.character(input_df$`Direction (Collar Number)`[i])] -
    1.96 * pin_sig[input_df$`Station ID`[i], as.character(input_df$`Direction (Collar Number)`[i])]
}
#change NA to FALSE
ans[is.na(ans)] <- FALSE
output_df <-
  data.frame(
    "Station ID" = input_df$`Station ID`[ans],
    "Sample Date (mm/dd/yyyy)" = input_df$`Sample Date (mm/dd/yyyy)`[ans],
    "Direction (Collar Number)" = input_df$`Direction (Collar Number)`[ans],
    "Pin Number" = input_df$`Pin Number`[ans],
    "Verified Pin Height (mm)" = input_df$`Verified Pin Height (mm)`[ans],
    "Direction Mean (mm)" = pin_mu[cbind(input_df$`Station ID`[ans],
                                      as.character(input_df$`Direction (Collar Number)`[ans]))],
    "Direction SD (mm)" = pin_sig[cbind(input_df$`Station ID`[ans],
                                     as.character(input_df$`Direction (Collar Number)`[ans]))],
    check.names = FALSE
  )
tab_14 <- xtable(output_df, caption = "14. PIN DIFFERENCE OUTSIDE CI")

#'DIRECTION DIFFERENCE OUTSIDE CI OF SITE'

site_mu <-
  tapply(input_df$`Verified Pin Height (mm)`,
         input_df$`Station ID`,
         mean,
         na.rm = T)
site_sig <-
  tapply(input_df$`Verified Pin Height (mm)`,
         input_df$`Station ID`,
         sd,
         na.rm = T)

input_tmp <-
  data.frame(
    "Station ID" = rep(rownames(pin_mu), each=dim(pin_mu)[2]),
    "Sample Date (mm/dd/yyyy)"=c(pin_date[1,],pin_date[2,],pin_date[3,],pin_date[4,],pin_date[5,],pin_date[6,]),
    "Direction (Collar Number)" = rep(c(1L, 3L, 5L, 7L), 6),
    "Site Mean (mm)" = c(pin_mu[1, ], pin_mu[2, ], pin_mu[3, ], pin_mu[4, ], pin_mu[5, ], pin_mu[6, ]),
    check.names = F
  )
len <- dim(input_tmp)[1]
ans <- rep(NA, len)
for (i in 1:len)
  ans[i] <-
  input_tmp$`Site Mean (mm)`[i] > site_mu[input_tmp$`Station ID`[i]] + 1.96 *
  site_sig[input_tmp$`Station ID`[i]] |
  input_tmp$`Site Mean (mm)`[i] < site_mu[input_tmp$`Station ID`[i]] - 1.96 *
  site_sig[input_tmp$`Station ID`[i]]
ans[is.na(ans)] <- FALSE

output_df <-
  data.frame(
    input_tmp[ans, ],
    check.names = FALSE,
    "Site Mean (mm)" = site_mu[input_tmp$`Station ID`[ans]],
    "Site SD (mm)" = site_sig[input_tmp$`Station ID`[ans]]
  )
tab_15 <-
  xtable(output_df, caption = "15. DIRECTION DIFFERENCE OUTSIDE CI OF SITE")

if(dim(output_df)[1]>0){
com_list <- list()
len <- dim(output_df)[1]
for (i in 1:len)
  com_list[[i]] <-
  unique(input_df$`Observation Comments`[which(
    input_df$`Station ID` == output_df$`Station ID`[i] &
      input_df$`Direction (Collar Number)` == output_df$`Direction (Collar Number)`[i]
  )])

#'COMMENTS WHEN DIRECTION DIFFERENCE OUTSIDE CI OF SITE
#combine all unique, non-blank, comments for each
tab_16 <-
  xtable(data.frame(
    output_df[, c(1:3)],
    "Observation Comments" = sapply(com_list,
                                    function(x)
                                      paste(x[grep("\\w", x)], collapse = ";")),
    check.names = F
  ),
  caption = '16. COMMENTS WHEN DIRECTION DIFFERENCE OUTSIDE CI OF SITE')} else {
    tab_16 <-
  xtable(data.frame("Station ID"="","Sample Date (mm/dd/yyyy)"="","Direction (Collar Number)"="",
    "Observation Comments" = "",
    check.names = F
  ),
  caption = '16. COMMENTS WHEN DIRECTION DIFFERENCE OUTSIDE CI OF SITE')
}
#Omissions to check
who <- which(is.na(input_df$`Verified Pin Height (mm)`))
get_em <-
  grep("root|clump|depression|lump",
       input_df$`Observation Comments`[who])

who_1 <- rep(NA, length(who))
who_1[get_em] <- input_df$`Observation Comments`[who[get_em]]
if (length(get_em) > 0) {
  output_17 <-
    data.frame(
      "Station ID" = input_df$`Station ID`[who[get_em]],
      "Sample Date (mm/dd/yyyy)" = input_df$`Sample Date (mm/dd/yyyy)`[who[get_em]],
      "Direction (Collar Number)" = input_df$`Direction (Collar Number)`[who[get_em]],
      "Pin Number" = input_df$`Pin Number`[who[get_em]],
      "Flagged Comment" = who_1,
      check.names = F
    )
  write.csv(output_17, file = paste0(out_path, "Omissions to check.csv"))
}
#Inclusions to check
who <- which(!is.na(input_df$`Verified Pin Height (mm)`))
get_em <-
  grep(
    "stem|leaf|shoot|grass|leaves|track|animal|burrow|trail|nest|print|eat|graze",
    input_df$`Observation Comments`[who]
  )
if (length(get_em) > 0) {
  output_18 <-
    data.frame(
      "Station ID" = input_df$`Station ID`[who[get_em]],
      "Sample Date (mm/dd/yyyy)" = input_df$`Sample Date (mm/dd/yyyy)`[who[get_em]],
      "Direction (Collar Number)" = input_df$`Direction (Collar Number)`[who[get_em]],
      "Pin Number" = input_df$`Pin Number`[who[get_em]],
      "Flagged Comment" = who_1,
      check.names = F
    )
  write.csv(output_18, file = paste0(out_path, "Inclusions to check.csv"))
}

#create the html output
out_name <- strsplit(input_file, "\\.")[[1]][1]
min_date <- min(input_df$`Sample Date (mm/dd/yyyy)`)
max_date <- max(input_df$`Sample Date (mm/dd/yyyy)`)

sink(paste0(out_path, out_name, ".html"))
cat(preamb)
cat(paste0('<h1>RSET QC (', min_date, ' -- ', max_date, ')</h1>'), sep = "\n")
print(
  tab_1,
  type = "html",
  caption.placement = "top",
  include.rownames = FALSE,
  html.table.attributes = ''
)
cat("<br>")
print(
  tab_2,
  type = "html",
  caption.placement = "top",
  include.rownames = TRUE,
  html.table.attributes = ''
)
cat("<br>")
print(
  tab_3,
  type = "html",
  caption.placement = "top",
  include.rownames = FALSE,
  html.table.attributes = ''
)
cat("<br>")
print(
  tab_4,
  type = "html",
  caption.placement = "top",
  include.rownames = FALSE,
  html.table.attributes = ''
)
cat("<br>")
print(
  tab_5,
  type = "html",
  caption.placement = "top",
  include.rownames = FALSE,
  html.table.attributes = ''
)
cat("<br>")
print(
  tab_6,
  type = "html",
  caption.placement = "top",
  include.rownames = FALSE,
  html.table.attributes = ''
)
cat("<br>")
print(
  tab_7,
  type = "html",
  caption.placement = "top",
  include.rownames = FALSE,
  html.table.attributes = ''
)
cat("<br>")
print(
  tab_8,
  type = "html",
  caption.placement = "top",
  include.rownames = FALSE,
  html.table.attributes = ''
)
cat("<br>")
print(
  tab_9,
  type = "html",
  caption.placement = "top",
  include.rownames = FALSE,
  html.table.attributes = ''
)
cat("<br>")
print(
  tab_10,
  type = "html",
  caption.placement = "top",
  include.rownames = FALSE,
  html.table.attributes = ''
)
cat("<br>")
print(
  tab_11,
  type = "html",
  caption.placement = "top",
  include.rownames = FALSE,
  html.table.attributes = ''
)
cat("<br>")
print(
  tab_12,
  type = "html",
  caption.placement = "top",
  include.rownames = FALSE,
  html.table.attributes = ''
)
cat("<br>")
print(
  tab_13,
  type = "html",
  caption.placement = "top",
  include.rownames = FALSE,
  html.table.attributes = ''
)
cat("<br>")
print(
  tab_14,
  type = "html",
  caption.placement = "top",
  include.rownames = FALSE,
  html.table.attributes = ''
)
cat("<br>")
print(
  tab_15,
  type = "html",
  caption.placement = "top",
  include.rownames = FALSE,
  html.table.attributes = ''
)
cat("<br>")
print(
  tab_16,
  type = "html",
  caption.placement = "top",
  include.rownames = FALSE,
  html.table.attributes = ''
)
cat("<br>")
cat('</body>
    </html>')
sink()
