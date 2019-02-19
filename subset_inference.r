##subset_inference.r
	

repeats = c("numberim","Remain","Englishlan")

x.names = paste(repeats,".x",sep="")
y.names = paste(repeats,".y",sep="")

covariates = c('line.x')

var.names = c('Number of immigrants be increased?','Children of undocumented be allowed to stay?','English as official language?')

	
##dose response estimates comparison
final.mat = matrix(nrow = 0, ncol = 8)
subsets = c('ta','tb')
cat('beginning dose response inference \n')

for(subset in subsets){

	out.mat = matrix(nrow = length(repeats), ncol = 8)
	if(subset == 'ta'){
		dat.subset = dat.all[dat.all$t.time %in% c('t2a','t4a'),]
		}
	if(subset == 'tb'){
		dat.subset = dat.all[dat.all$t.time %in% c('t2b','t4b'),]
		}
	z.variable = 'treatment'
	
	for(j in 1:length(repeats)){
		dat.subset$x.new = (as.numeric(dat.subset[,x.names[j]])-1)/4  ##rescale x to 0-1
		dat.subset$y.new = (as.numeric(dat.subset[,y.names[j]])-1)/4  ##rescale y to 0-1
		dat.subset$Y = dat.subset$y.new - dat.subset$x.new
	
		dat.use = dat.subset[is.na(dat.subset$Y) == F,]
				
		x.sd = sd(dat.use$x.new,na.rm = T)
		x.mean = mean(dat.use$x.new,na.rm = T)
	
		station.treatment.table = table(dat.use$station,dat.use[,z.variable])
		no.control.stations = names(which(station.treatment.table[,1] == 0))
		no.treatment.stations = names(which(station.treatment.table[,2] == 0))
		dat.use = dat.use[!dat.use$station%in%c(no.control.stations,no.treatment.stations),]
		
		dat.use$station = factor(dat.use$station)
		dat.use$treated_unit = factor(dat.use$treated_unit)
		Xs = data.matrix(dat.use[,covariates])
		
		perms <- genperms(Z = dat.use[,z.variable], blockvar=dat.use$station, clustvar=dat.use$treated_unit)
		probs = genprobexact(Z = dat.use[,z.variable], blockvar=dat.use$station, clustvar=dat.use$treated_unit)
		ate = estate(Y = dat.use$Y, Z = dat.use[,z.variable], X = Xs, prob = probs)
		Ys = genouts(Y = dat.use$Y, Z = dat.use[,z.variable], ate = ate)
		distout <- gendist(Ys,perms, prob=probs)
		disp =	dispdist(distout, ate = ate, display.plot = F)
		
		##fill matrix
		out.mat[j,1] = repeats[j]
		out.mat[j,2] = subset
		out.mat[j,3] = nrow(dat.use)
		out.mat[j,4] = ate
		out.mat[j,5] = x.mean
		out.mat[j,6] = x.sd
		out.mat[j,7] = disp$quantile[1]
		out.mat[j,8] = disp$quantile[2]
		}
		final.mat = rbind(final.mat,out.mat)
	}
final.mat = as.data.frame(final.mat)
colnames(final.mat) = c('variable','subset','N','ate','x.mean','x.sd','quantile.lower','quantile.upper')
print(final.mat)
final.mat.dose = final.mat ##mat for creating graph later
	
##ideology comparison
final.mat = matrix(nrow = 0, ncol = 8)
subsets = c('liberals.only','moderates.only','conservatives.only')
cat('beginning ideology subset inference \n')

for(subset in subsets){ 

	out.mat = matrix(nrow = length(repeats), ncol = 8)
	if(subset == 'liberals.only'){
		dat.subset = dat.all[dat.all$ideology.x %in% c(1,2),]
		}
	if(subset == 'conservatives.only'){
		dat.subset = dat.all[dat.all$ideology.x %in% c(4,5),]
		}
	if(subset == 'moderates.only'){
		dat.subset = dat.all[dat.all$ideology.x == 3,]
		}
	z.variable = 'treatment'
	
	for(j in 1:length(repeats)){
		dat.subset$x.new = (as.numeric(dat.subset[,x.names[j]])-1)/4  ##rescale x to 0-1
		dat.subset$y.new = (as.numeric(dat.subset[,y.names[j]])-1)/4  ##rescale y to 0-1
		dat.subset$Y = dat.subset$y.new - dat.subset$x.new
			
		x.sd = sd(dat.use$x.new,na.rm = T)
		x.mean = mean(dat.use$x.new,na.rm = T)
		
		station.treatment.table = table(dat.use$station,dat.use[,z.variable])
		no.control.stations = names(which(station.treatment.table[,1] == 0))
		no.treatment.stations = names(which(station.treatment.table[,2] == 0))
		dat.use = dat.use[!dat.use$station%in%c(no.control.stations,no.treatment.stations),]
		
		dat.use$station = factor(dat.use$station)
		dat.use$treated_unit = factor(dat.use$treated_unit)
		Xs = data.matrix(dat.use[,covariates])
		
		perms <- genperms(Z = dat.use[,z.variable], blockvar=dat.use$station, clustvar=dat.use$treated_unit)
		probs = genprobexact(Z = dat.use[,z.variable], blockvar=dat.use$station, clustvar=dat.use$treated_unit)
		ate = estate(Y = dat.use$Y, Z = dat.use[,z.variable], X = Xs, prob = probs)

		Ys = genouts(Y = dat.use$Y, Z = dat.use[,z.variable], ate = ate)
		distout <- gendist(Ys,perms, prob=probs)
		disp =	dispdist(distout, ate = ate, display.plot = F)
		
		##fill matrix
		out.mat[j,1] = repeats[j]
		out.mat[j,2] = subset
		out.mat[j,3] = nrow(dat.use)
		out.mat[j,4] = ate
		out.mat[j,5] = x.mean
		out.mat[j,6] = x.sd
		out.mat[j,7] = disp$quantile[1]
		out.mat[j,8] = disp$quantile[2]
		
		}
	final.mat = rbind(final.mat,out.mat)
	}
final.mat = as.data.frame(final.mat)
colnames(final.mat) = c('variable','subset','N','ate','x.mean','x.sd','quantile.lower','quantile.upper')
print(final.mat)
final.mat.ideology = final.mat ##for graph later


##friends comparison
final.mat = matrix(nrow = 0, ncol = 8)
subsets = c('low.friends','high.friends','middle.friends')

cat('beginning friends response inference \n')

for(subset in subsets){ 

	out.mat = matrix(nrow = length(repeats), ncol = 8)
	if(subset == 'low.friends'){
		dat.subset = dat.all[dat.all$Friends.x == 0,]
		}
	if(subset == 'high.friends'){
		dat.subset = dat.all[dat.all$Friends.x >= 5,]
		}
	if(subset == 'middle.friends'){
		dat.subset = dat.all[dat.all$Friends.x > 0 & dat.all$Friends.x < 5,]
		}
	
	z.variable = 'treatment'
	
	for(j in 1:length(repeats)){
		dat.subset$x.new = (as.numeric(dat.subset[,x.names[j]])-1)/4  ##rescale x to 0-1
		dat.subset$y.new = (as.numeric(dat.subset[,y.names[j]])-1)/4  ##rescale y to 0-1
		dat.subset$Y = dat.subset$y.new - dat.subset$x.new
			
		dat.use = dat.subset[is.na(dat.subset$Y) == F,]
			
	
		x.sd = sd(dat.use$x.new,na.rm = T)
		x.mean = mean(dat.use$x.new,na.rm = T)
		
		station.treatment.table = table(dat.use$station,dat.use[,z.variable])
		no.control.stations = names(which(station.treatment.table[,1] == 0))
		no.treatment.stations = names(which(station.treatment.table[,2] == 0))
		dat.use = dat.use[!dat.use$station%in%c(no.control.stations,no.treatment.stations),]
		
		dat.use$station = factor(dat.use$station)
		dat.use$treated_unit = factor(dat.use$treated_unit)
		Xs = data.matrix(dat.use[,covariates])
		
		perms <- genperms(Z = dat.use[,z.variable], blockvar=dat.use$station, clustvar=dat.use$treated_unit)
		probs = genprobexact(Z = dat.use[,z.variable], blockvar=dat.use$station, clustvar=dat.use$treated_unit)

		ate = estate(Y = dat.use$Y, Z = dat.use[,z.variable], X = Xs, prob = probs)
		Ys = genouts(Y = dat.use$Y, Z = dat.use[,z.variable], ate = ate)
		distout <- gendist(Ys,perms, prob=probs)
		disp =	dispdist(distout, ate = ate, display.plot = F)
		
		##fill matrix
		out.mat[j,1] = repeats[j]
		out.mat[j,2] = subset
		out.mat[j,3] = nrow(dat.use)
		out.mat[j,4] = ate
		out.mat[j,5] = x.mean
		out.mat[j,6] = x.sd
		out.mat[j,7] = disp$quantile[1]
		out.mat[j,8] = disp$quantile[2]
		
		#print(disp)
		}
	final.mat = rbind(final.mat,out.mat)
	}
	final.mat = as.data.frame(final.mat)
	colnames(final.mat) = c('variable','subset','N','ate','x.mean','x.sd','quantile.lower','quantile.upper')
	print(final.mat)
	final.mat.friends = final.mat ##for graph
	


#######income subsets
subsets = c('low.income','middle.income', 'high.income')
final.mat = matrix(nrow = 0, ncol = 8)
cat('beginning income subset inference \n')
for(subset in subsets){ 

	out.mat = matrix(nrow = length(repeats), ncol = 8)
	
	if(subset == 'low.income'){
		dat.subset = dat.all[dat.all$income.new < 105000,]
		}
	if(subset == 'middle.income'){
		dat.subset = dat.all[dat.all$income.new >= 105000 & dat.all$income.new <= 135000,]
		}
	if(subset == 'high.income'){
		dat.subset = dat.all[dat.all$income.new > 135000,]
		}
	
	z.variable = 'treatment'
	
	for(j in 1:length(repeats)){
		dat.subset$x.new = (as.numeric(dat.subset[,x.names[j]])-1)/4  ##rescale x to 0-1
		dat.subset$y.new = (as.numeric(dat.subset[,y.names[j]])-1)/4  ##rescale y to 0-1
		dat.subset$Y = dat.subset$y.new - dat.subset$x.new
			
		dat.use = dat.subset[is.na(dat.subset$Y) == F,]
				
		x.sd = sd(dat.use$x.new,na.rm = T)
		x.mean = mean(dat.use$x.new,na.rm = T)

		station.treatment.table = table(dat.use$station,dat.use[,z.variable])
		no.control.stations = names(which(station.treatment.table[,1] == 0))
		no.treatment.stations = names(which(station.treatment.table[,2] == 0))
		dat.use = dat.use[!dat.use$station%in%c(no.control.stations,no.treatment.stations),]
		
		dat.use$station = factor(dat.use$station)
		dat.use$treated_unit = factor(dat.use$treated_unit)
		Xs = data.matrix(dat.use[,covariates])
		
		perms <- genperms(Z = dat.use[,z.variable], blockvar=dat.use$station, clustvar=dat.use$treated_unit)
		probs = genprobexact(Z = dat.use[,z.variable], blockvar=dat.use$station, clustvar=dat.use$treated_unit)

		ate = estate(Y = dat.use$Y, Z = dat.use[,z.variable], X = Xs, prob = probs)
		Ys = genouts(Y = dat.use$Y, Z = dat.use[,z.variable], ate = ate)
		distout <- gendist(Ys,perms, prob=probs)
		disp =	dispdist(distout, ate = ate, display.plot = F)
		
		##fill matrix
		out.mat[j,1] = repeats[j]
		out.mat[j,2] = subset
		out.mat[j,3] = nrow(dat.use)
		out.mat[j,4] = ate
		out.mat[j,5] = x.mean
		out.mat[j,6] = x.sd
		out.mat[j,7] = disp$quantile[1]
		out.mat[j,8] = disp$quantile[2]
		
		}
	final.mat = rbind(final.mat,out.mat)
	}
final.mat = as.data.frame(final.mat)
colnames(final.mat) = c('variable','subset','N','ate','x.mean','x.sd','quantile.lower','quantile.upper')
print(final.mat)
final.mat.income = final.mat  ##for later

	