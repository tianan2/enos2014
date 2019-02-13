##balance_check.r
###check to see if randomization created balanced samples in Enos experiment
####RdE September 2012


out.balance.test = xBalance(fmla = treatment ~ liberal+republican+obama.disapprove+ride.everyday+voted.2010+romney.voter+Hispanics.x+age+residency.new+hispanic.new+college+income.new+male+white, data = dat.all, report = c("std.diffs","z.scores","adj.means","chisquare.test"), strata = factor(dat.all$station))
print(out.balance.test)
xtable.out.balance.text = xtable(out.balance.test)
print(xtable.out.balance.text, file = 'balance_table.tex',
	floating = FALSE)

missing.balance.test = xBalance(fmla = missing ~ liberal+republican+obama.disapprove+ride.everyday+voted.2010+romney.voter+Hispanics+age+residency.new+hispanic.new+gender+college+us.born+income.new+male, data = dat.t1, report = c("std.diffs","z.scores","adj.means","chisquare.test"),na.rm=T)
print(missing.balance.test)

missing.balance.test = xBalance(fmla = missing ~ numberim+Remain+Englishlan+liberal+republican+obama.disapprove+ride.everyday+voted.2010+romney.voter+Hispanics+age+residency.new+hispanic.new+gender+college+us.born+income.new+male, data = dat.t1, report = c("std.diffs","z.scores","adj.means","chisquare.test"),na.rm=T)
print(missing.balance.test)

missing.balance.test = xBalance(fmla = missing ~ treatment, data = dat.t1, report = c("std.diffs","z.scores","adj.means","chisquare.test"),na.rm=T)
print(missing.balance.test)

