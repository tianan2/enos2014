##main_results.r
####primary randomization inferece

###inference
repeats = c("numberim","Remain","Englishlan")

x.names = paste(repeats,".x",sep="")
y.names = paste(repeats,".y",sep="")

covariates = c('line.x')

###treated first
final.mat = matrix(nrow = 0, ncol = 8)

subsets = c('all','no.car')

cat('beginning inference \n')


for(subset in subsets){

	out.mat = matrix(nrow = length(repeats), ncol = 8)
	
	if(subset == 'all'){
		dat.subset = dat.all
		}
	if(subset ==  'no.car'){
		dat.subset = dat.all[dat.all$habits != 1,]
		}

		
	z.variable = 'treatment'
	
	for(j in 1:length(repeats)){
		dat.subset$x.new = (as.numeric(dat.subset[,x.names[j]])-1)/4  ##rescale x to 0-1
		dat.subset$y.new = (as.numeric(dat.subset[,y.names[j]])-1)/4  ##rescale y to 0-1
		dat.subset$Y = dat.subset$y.new - dat.subset$x.new
		
		dat.use = dat.subset[is.na(dat.subset$Y) == F,]
		
		x.sd = sd(dat.use$x.new,na.rm = T)
		x.mean = mean(dat.use$x.new,na.rm = T)
		y.mean = mean(dat.use$y.new,na.rm = T)
		y.treat = mean(dat.use$y.new[dat.use$treatment==1],na.rm = T)
	
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
		Ys = genouts(Y = dat.use$Y, Z = dat.use[,z.variable], ate = 0)
		distout <- gendist(Ys,perms, prob=probs)
		disp =	dispdist(distout, ate = ate, display.plot = F)
		
		out.mat[j,1] = repeats[j]
		out.mat[j,2] = subset
		out.mat[j,3] = nrow(dat.use)
		out.mat[j,4] = ate
		out.mat[j,5] = disp$greater.p.value
		out.mat[j,6] = disp$lesser.p.value
		out.mat[j,7] = x.sd
		out.mat[j,8] = x.mean
		}
	final.mat = rbind(final.mat,out.mat)
	}

final.mat = as.data.frame(final.mat)
colnames(final.mat) = c('variable','subset','N','ate','greater.p.value','lesser.p.value','x.sd','x.mean')
print(final.mat)

final.mat.main = final.mat ##final.mat for output creation later
