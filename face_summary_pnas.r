###face_summary.r
###give summary statistics for each face in confederate sample and comparison sample, make comparisons
###RdE February 2013


##se function
stderr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

out.list = list()
for(i in 1:3){
	if(i == 1){use.dat = conf.dat}	
	if(i == 2){use.dat = hisp.dat}
	if(i == 3){use.dat = white.dat}
	
	
	vars.keep = c('race_1','his','_imm','_for','fri','app','IQ','intim','suc','wea','UE','_pro','ind','you','_com','unu')


	out.mat = matrix(ncol = length(vars.keep), nrow = nrow(use.dat)*7) ##7 faces, so when data is stacked, that's waht you get
	colnames(out.mat) = vars.keep

	for(var in vars.keep){
		use.cols = grep(var,colnames(use.dat), fixed = T)
		this.dat = use.dat[,use.cols]
		col.dat = as.vector(as.matrix(this.dat))
		out.mat[,var] = col.dat
		}
	##recode 0/1
	out.mat[, c('_imm','_for','intim','fri','app','IQ','suc','wea','you','_pro','ind','UE','_com','unu')] = 
	(out.mat[, c('_imm','_for','intim','fri','app','IQ','suc','wea','you','_pro','ind','UE','_com','unu')]-1)/4	
	
	if(i == 1){conf.out = out.mat}	
	if(i == 2){hisp.out = out.mat}
	if(i == 3){white.out = out.mat}
	
	
	out.means = apply(out.mat,2,mean, na.rm = T)
	out.ses = apply(out.mat,2,stderr)
	
	out.final = rbind(out.means,out.ses)
	
	out.list[[i]] = out.final
	}


x.names = c('white','Hispanic','immigrant','foreign','friendly','approachable','intelligent','intimidating','successful','wealthy','unemployed','professional','industrious','young','commuter','unusual')


ylims = c(.5,ncol(out.list[[1]])*2)+.5
ylims = c(.5,ncol(out.list[[1]])*2)+.5
xlims = c(-.1,1)
yrange = (1:ncol(out.list[[1]]))*2

plot.sets = list(c(1,2,3,4),
	c(5,6,7,8),
	c(9,10,11,12),
	c(13,14,15,16)
	)

pdf('face_ratings_2.pdf',
	width = 9, height = 6.95
	)
par(las = 1)
par(mfrow = c(2,2))
par(mar = c(2.5, .5, 1.5, .5))
par(bty = 'n')
for(h in 1:length(plot.sets)){
	use.set = plot.sets[[h]]
	use.names = x.names[use.set] 
	plot(out.list[[1]][1,use.set],1:4,
		xlim = xlims,
		ylim = c(.5,4.5),
		type = 'n',
		yaxt = 'n',
		ylab = '',
		cex.lab = 1.75
		)
		
	for(i in 1:4){
		
		lab.place.mean = min(out.list[[1]][1,use.set][i],
						out.list[[2]][1,use.set][i],
						out.list[[3]][1,use.set][i]
						)
		lab.place.se = min(out.list[[1]][2,use.set][i],
						out.list[[2]][2,use.set][i],
						out.list[[3]][2,use.set][i]
						)
		text(x = lab.place.mean - (2*lab.place.se),
			y = i,
			labels = use.names[i],
			cex = 1.25,
			pos = 2
			)

		lines(x= c(out.list[[1]][1,use.set][i]+.015,out.list[[1]][1,use.set][i]+(1.96*out.list[[1]][2,use.set][i])),
			y = c(i,i),
			col = 'red',
			lty = 1)
		lines(x= c(out.list[[1]][1,use.set][i]-.015,out.list[[1]][1,use.set][i]-(1.96*out.list[[1]][2,use.set][i])),
			y = c(i,i),
			col = 'red',
			lty = 1)

		lines(x= c(out.list[[2]][1,use.set][i]+.015,out.list[[2]][1,use.set][i]+(1.96*out.list[[2]][2,use.set][i])),
			y = c(i+.25,i+.25),
			col = 'blue',
			lty = 1)
		lines(x= c(out.list[[2]][1,use.set][i]-.015,out.list[[2]][1,use.set][i]-(1.96*out.list[[2]][2,use.set][i])),
			y = c(i+.25,i+.25),
			col = 'blue',
			lty = 1)

		lines(x= c(out.list[[3]][1,use.set][i]+.015,out.list[[3]][1,use.set][i]+(1.96*out.list[[3]][2,use.set][i])),
			y = c(i-.25,i-.25),
			lty = 1)
		lines(x= c(out.list[[3]][1,use.set][i]-.015,out.list[[3]][1,use.set][i]-(1.96*out.list[[3]][2,use.set][i])),
			y = c(i-.25,i-.25),
			lty = 1)	
			}
	text(out.list[[1]][1,use.set],1:4,
		'C',
		col = 'red',
		cex = 1)
	text(out.list[[2]][1,use.set],1:4+.25,
		'H',
		col = 'blue',
		cex = 1)
	text(out.list[[3]][1,use.set],1:4-.25,
		'A',
	cex = 1)

	}
dev.off()