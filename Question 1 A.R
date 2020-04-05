#Question 1
set.seed(2324)

library(Matching)
foo <- read.csv(url("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00089202-1711/daughters.csv"))
first_lm <- lm(nowtot ~ Dems + Repubs + Christian+ age+ srvlng + demvote + hasgirls, data = foo)
summary(first_lm)

confint(first_lm, level = .95)

first_lm_gen <- GenMatch(foo$hasgirls, X = cbind(foo$Dems, foo$Repubs, foo$Christian, foo$age, foo$srvlng, foo$demvote), estimand = "ATT", pop.size = 20, nboots = 250)
mout <- Match(Tr = foo$hasgirls, X = cbind(foo$Dems, foo$Repubs, foo$Christian, foo$age, foo$srvlng, foo$demvote), Weight.matrix = first_lm_gen)
mb <- MatchBalance(foo$hasgirls ~ foo$Dems + foo$Repubs + foo$Christian+ foo$age+ foo$srvlng + foo$demvote, match.out = mout, nboots = 250)

mout_with_y <- Match(Y = foo$nowtot, Tr = foo$hasgirls, X = cbind(foo$Dems, foo$Repubs, foo$Christian, foo$age, foo$srvlng, foo$demvote), Weight.matrix = first_lm_gen)

#Part B

#creating a clolumn for a congerssperson having 2 boys or girls

for (i in 1:length(foo$nowtot)) {
  if (foo$ngirls[i] >= 2) {
    foo$is2girl[i] <- 1
  }
  else {
    foo$is2girl[i] <- 0
  }
}

for (i in 1:length(foo$nowtot)) {
  if (foo$nboys[i] >= 2) {
    foo$is2boy[i] <- 1
  }
  else {
    foo$is2boy[i] <- 0
  }
}

#this creates a selector variable for removing double-gender-chlidless congersspeople and then creates a new df
selector <- c()
count <- 1
for (i in 1:length(foo$nowtot)) {
  if (foo$is2boy[i] == 0 & foo$is2girl[i] == 0) {
    selector[count] <- i
    count <- count+1
  }
}

new_foo <- foo[-selector,]

second_lm <- lm(nowtot ~ Dems + Repubs + Christian+ age+ srvlng + demvote + is2girl, data = new_foo)

second_lm_gen <- GenMatch(new_foo$is2girl, X = cbind(new_foo$Dems, new_foo$Repubs, new_foo$Christian, new_foo$age, new_foo$srvlng, new_foo$demvote), estimand = "ATT", pop.size = 20, nboots = 250)
second_mout <- Match(Tr = new_foo$is2girl, X = cbind(new_foo$Dems, new_foo$Repubs, new_foo$Christian, new_foo$age, new_foo$srvlng, new_foo$demvote), Weight.matrix = second_lm_gen)
second_mb <- MatchBalance(new_foo$is2girl ~ new_foo$Dems + new_foo$Repubs + new_foo$Christian+ new_foo$age+ new_foo$srvlng + new_foo$demvote, match.out = second_mout, nboots = 250)

