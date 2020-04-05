rm(list=ls())

foo <- read.csv("https://tinyurl.com/y2qv82ks")

# Only install the 2 packages below if not already installed
# install.packages("date")
# install.packages("Matching")

library(date)
library(Matching)

# dim(foo)
# 76772    14

names(foo)
#[1] "bank_code"      [2] "bank_name"      [3] "date_of_birth"  
#[4] "gender"         [5] "marital_status" [6]  "education"      
#[7] "occupation"     [8] "postal_code"    [9]  "district_code"  
#[10] "worker"        [11] "capital"       [12] "credit_proposal"
#[13] "status"        [14] "randomid"

# vars 1, 2, and 3 have 17,330, 68,029, and 1108 blanks (""), respectively
# vars 8 and 11 have 13,118 and 14,642 NAs, respectively

# create dummies indicating where the blanks and NAs are.
missing_bank_code <- rep(0, 76772)
missing_bank_name <- rep(0, 76772)
missing_date_of_birth <- rep(0, 76772)
NA_postal_code <- rep(0, 76772)
NA_capital <- rep(0, 76772)
NA_credit_proposal <- rep(0, 76772)

foo <- cbind(foo, missing_bank_code,
             missing_bank_name,
             missing_date_of_birth,
             NA_postal_code,
             NA_capital,
             NA_credit_proposal)

foo$missing_bank_code[which(foo$bank_code == "")] <- 1
foo$missing_bank_name[which(foo$bank_name == "")] <- 1
foo$missing_date_of_birth[which(foo$date_of_birth == "")] <- 1
foo$NA_capital[which(is.na(foo$capital) == TRUE)] <- 1
foo$NA_credit_proposal[which(is.na(foo$credit_proposal) == TRUE)] <- 1
foo$NA_postal_code[which(is.na(foo$postal_code) == TRUE)] <- 1

# change the dates to R-readable format
foo$R_date_of_birth <- as.character(foo[,3])
for(i in 1:length(foo[,3])) {foo$R_date_of_birth[i] <- as.date(foo$R_date_of_birth[i], order = 
                                                                 "dmy")}
foo$R_date_of_birth <- as.date(as.numeric(foo$R_date_of_birth))

oldest <- which(foo$R_date_of_birth < as.date("1-Jan-1910"))
youngest <- which(foo$R_date_of_birth > as.date("1 Jan 2001"))

foo$oldest <- rep(0, length(foo[,3]))
foo$youngest <- rep(0, length(foo[,3]))
foo$outlier_ages <- rep(0, length(foo[,3]))
foo$oldest[oldest] <- 1
foo$youngest[youngest] <- 1
foo$outlier_ages[c(oldest,youngest)] <- 1

foo$R_date_of_birth[which(is.na(foo$R_date_of_birth) == TRUE)] <- -9999999

# This obs with specific postal code makes no sense
foo <- foo[-which(foo$postal_code == 9151), ]

# To extract only the first digit of postal codes:
foo$postal_code1 <- foo$postal_code%/% 10000
foo$postal_code1[which(is.na(foo$postal_code1) == TRUE)] <- -9999999

# credit_proposal feature engineering
foo$credit_proposal[which(is.na(foo$credit_proposal) == TRUE)] <- 9999999

foo$credit_proposal_0 <- foo$credit_proposal == 0 & (is.na(foo$credit_proposal) == FALSE)
foo$credit_proposal_0to5 <- foo$credit_proposal > 0 & foo$credit_proposal < 5000000 & 
  (is.na(foo$credit_proposal) == FALSE)
foo$credit_proposal_5to10 <- foo$credit_proposal >= 5000000 & foo$credit_proposal < 10000000 & 
  (is.na(foo$credit_proposal) == FALSE)
foo$credit_proposal_10to20 <- foo$credit_proposal >= 10000000 & foo$credit_proposal < 20000000 & 
  (is.na(foo$credit_proposal) == FALSE)
foo$credit_proposal_20up <- foo$credit_proposal >= 20000000 & (is.na(foo$credit_proposal) == 
                                                                 FALSE)

foo$credit_proposal_transformed <-
  1*foo$credit_proposal_0 +
  2*foo$credit_proposal_0to5 +
  3*foo$credit_proposal_5to10 +
  4*foo$credit_proposal_10to20 +
  5*foo$credit_proposal_20up +
  6*foo$NA_credit_proposal

# NA capital
foo$capital[which(is.na(foo$capital) == TRUE)] <- 9999999

# capital feature engineering
foo$capital_0 <- foo$capital == 0 & (is.na(foo$capital) == FALSE)
foo$capital_0to2 <- foo$capital > 0 & foo$capital < 200000 & (is.na(foo$capital) == FALSE)
foo$capital_2to5 <- foo$capital >= 200000 & foo$capital < 500000 & (is.na(foo$capital) == FALSE)
foo$capital_5to10 <- foo$capital >= 500000 & foo$capital < 1000000 & (is.na(foo$capital) == 
                                                                        FALSE)
foo$capital_10to20 <- foo$capital >= 1000000 & foo$capital < 2000000 & (is.na(foo$capital) == 
                                                                          FALSE)
foo$capital_20to50 <- foo$capital >= 2000000 & foo$capital < 5000000 & (is.na(foo$capital) == 
                                                                          FALSE)
foo$capital_50up <- foo$capital >= 5000000 & (is.na(foo$capital) == FALSE)
foo$capital_transformed <-
  1*foo$capital_0 +
  2*foo$capital_0to2 +
  3*foo$capital_2to5 +
  4*foo$capital_5to10 +
  5*foo$capital_10to20 +
  6*foo$capital_20to50 +
  7*foo$capital_50up +
  8*foo$NA_capital

# worker feature engineering
# remove outlier in the control group (10 million workers)
foo <- foo[-which(foo$worker == max(foo$worker)),]

foo$worker_0 <- foo$worker == 0
foo$worker_1 <- foo$worker == 1
foo$worker_2 <- foo$worker == 2
foo$worker_3 <- foo$worker == 3
foo$worker_4 <- foo$worker == 4
foo$worker_5to9 <- foo$worker >=5 & foo$worker < 10
foo$worker_10to24 <- foo$worker >=10 & foo$worker < 25
foo$worker_25to99 <- foo$worker >=25 & foo$worker < 100
foo$worker_100up <- foo$worker >= 100

foo$worker_transformed <-
  1*foo$worker_0 +
  2*foo$worker_1 +
  3*foo$worker_2 +
  4*foo$worker_3 +
  5*foo$worker_4 +
  6*foo$worker_5to9 +
  7*foo$worker_10to24 +
  8*foo$worker_25to99 +
  9*foo$worker_100up


# Treatment Indicator
foo$treat <- foo$status == "Sudah"

# The only bank code (var 1) is PEM
# The only bank name (var 2) is PEMDA -- regional government, not a bank
# (var 3) 174 outlier ages
# (var 4) gender is 3 levels: men, women, and business entity ("LAKI-LAKI"   "PEREMPUAN"   "BADAN 
# USAHA")
# (var 5) marital_status: business agency, single, married ("BADAN USAHA" "BELUM KAWIN" "KAWIN")

### (var 6) EDUCATION
# "BADAN USAHA" "DIPLOMA"     "LAINNYA"     "SARJANA"     "SD"          "SMP"         "SMU"
# "BUSINESS AGENCY" "DIPLOMA" "OTHER" "GRADUATE" "Elementary School" "Middle School" "High School"

### (var 7) OCCUPATION
#[1] "KARYAWAN SWASTA"        "LAIN-LAIN/BADAN USAHA"  "NELAYAN"                "PEDAGANG"              
#[5] "PENSIUNAN/PURNAWIRAWAN" "PETANI"                 "PNS"                    "PROFESIONAL"          
#[9] "TNI/POLRI"              "WIRASWASTA"      

#[1] "PRIVATE EMPLOYEES" [2] "OTHERS / BUSINESS AGENCIES" [3] "FISHERMANS" [4] "TRADERS"
#[5] "PENSIONERS / PURNAWIRAWAN" [6] "FARMERS" [7] "CIVIL SERVANT" [8] "PROFESSIONAL"
#[9] "ARMY / POLICE" [10] "ENTREPRENEURS"

### (var 8) 938 unique POSTAL CODES

### (var 9) 63 unique DISTRICT CODES

### (var 10) : "worker". unknown meaning... 300+ unique numerical values (maybe no. of staff?)

### (var 11) (capital)
# 0%      25%      50%      75%     100%
# 0.00e+00 1.00e+06 1.00e+07 1.50e+07 1.75e+11

### (var 12) (credit proposal)
# 0%     25%     50%     75%    100%
# 0.0e+00 0.0e+00 1.0e+07 2.5e+07 5.0e+09

### (var 13) STATUS
# "Belum"    "Sudah"
# "Not Yet"  "Already"

### (var 14) randomid

foo_badan <- foo[which(foo$gender == "BADAN USAHA"), ]
foo_people <- foo[-which(foo$gender == "BADAN USAHA"), ]

######## CODE MODIFICATION SHOULD BEGIN HERE...
# To extract only the first digit of postal codes:
foo$dis_code1 <- foo$district%/% 100
foo$dis_code1[which(is.na(foo$dis_code1) == TRUE)] <- -9999999


X = cbind(foo$R_date_of_birth, foo$gender, foo$marital_status,
                foo$education, foo$occupation, foo$dis_code1,
                foo$worker, foo$capital, foo$credit_proposal,
                foo$worker_transformed, foo$capital_transformed, foo$credit_proposal_transformed,
                foo$missing_date_of_birth,
                foo$dis_code1,
                foo$NA_capital,
                foo$NA_credit_proposal, foo$R_date_of_birth)


Tr <- foo$treat

BalanceMat <- X

#setting the calipter do so that all dates are within a year of each other
genout <- GenMatch(Tr=Tr, X=X, BalanceMatrix=BalanceMat, estimand="ATT", M=1,
                          pop.size=5, max.generations=3, wait.generations=2,
                          exact = c(FALSE, TRUE, TRUE,
                                    TRUE, TRUE, TRUE,
                                    FALSE, FALSE, FALSE,
                                    TRUE, TRUE, TRUE,
                                    TRUE,
                                    TRUE,
                                    TRUE,
                                    TRUE, FALSE), caliper = c(1e16,1e16,1e16,1e16
                   ,1e16,1e16,1e16,1e16,1e16,1e16,
                   1e16,1e16,1e16,1e16,1e16,1e16,365/sd(foo$R_date_of_birth)))

mout <- Match(Tr=Tr, X=X, estimand="ATT", M=1,
                     exact = c(FALSE, TRUE, TRUE,
                              TRUE, TRUE, TRUE,
                              FALSE, FALSE, FALSE,
                              TRUE, TRUE, TRUE,
                              TRUE, TRUE, TRUE, TRUE, FALSE), 
Weight.matrix = diag(c(8.229663e+02, 3.248991e+02,
1.582134e+02, 6.569852e+02, 9.990704e+01, 2.693163e+02, 
3.513421e+02, 7.512798e+00, 1.233057e+02,
7.083123e+02, 9.801175e+02, 5.627142e+02,
6.751423e+02, 4.900635e+02, 1.064447e+02,
3.989456e+02, 2.866456e+02)))
summary(mout)


mb <- MatchBalance(foo$treat~
                            	foo$R_date_of_birth + foo$gender + foo$marital_status +
                            	foo$education + foo$occupation + foo$postal_code1 +
                            	foo$worker + foo$capital + foo$credit_proposal +
                            	foo$worker_transformed + foo$capital_transformed + 
                              foo$credit_proposal_transformed +
                            	foo$missing_date_of_birth +
                            	foo$NA_postal_code +
                            	foo$NA_capital +
                            	foo$NA_credit_proposal + foo$R_date_of_birth,
                         	match.out=mout, nboots=500)
