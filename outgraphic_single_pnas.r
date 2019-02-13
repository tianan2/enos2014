##outgraphic_single.r
###create ouptput plots
####RdE November 2012
	
# ####create output
output.vars = c('numberim','Remain','Englishlan')
var.names = c('Number of immigrants be increased?','Children of undocumented be allowed to stay?','English as official language?')

##graph presets
os = .4
line.os = .015
y.point = .75
ylims = c(0,1.1)
xlims = c(-.35,.35)
points.cex = 4
lab.cex = 1.5
line.lwd = 4.5
axis.cex = 1.25

colors = brewer.pal(3,'Paired')[1:2] ##colors for pairs used in plots below

####dose graph
pdf('dose_combined.pdf',
	width = 6.5, height = 4
	)

par(mfrow = c(3,1)) 
par(mar = c(5,0,1,0))
par(bty = 'n')

	
##dose response graph
out.mat = final.mat.dose[,c('variable','subset','ate','quantile.lower','quantile.upper')]
out.mat$ate = as.numeric(as.character(out.mat$ate))
out.mat$quantile.lower = as.numeric(as.character(out.mat$quantile.lower))
out.mat$quantile.upper = as.numeric(as.character(out.mat$quantile.upper))

out.mat.ta = out.mat[out.mat$subset == 'ta'&out.mat$variable %in% output.vars,]
out.mat.tb = out.mat[out.mat$subset == 'tb'&out.mat$variable %in% output.vars,]

for(i in 1:length(var.names)){
	plot(x  = out.mat.ta$ate[i], y = y.point, 
		xlim = xlims,
		ylim = ylims,
		ylab = '',
		xlab = var.names[i],
		yaxt = 'n',
		type = 'n',
		cex.lab = lab.cex,
		cex.axis = axis.cex)
	lines(x = c(out.mat.ta$quantile.lower[i],out.mat.ta$ate[i]-line.os), 
			y = c(y.point,y.point),
			lty = 1,
			col = colors[1],
			lwd = line.lwd)
	lines(x = c(out.mat.ta$ate[i]+line.os,out.mat.ta$quantile.upper[i]), 
			y = c(y.point,y.point),
			lty = 1,
			col = colors[1],
			lwd = line.lwd)
	lines(x = c(out.mat.tb$quantile.lower[i],out.mat.tb$ate[i]-line.os), 
			y = c(y.point-os,y.point-os),
			lty = 1,
			col = colors[2],
			lwd = line.lwd)
	lines(x = c(out.mat.tb$ate[i]+line.os,out.mat.tb$quantile.upper[i]), 
			y = c(y.point-os,y.point-os),
			lty = 1,
			col = colors[2],
			lwd = line.lwd)

	points(x  = out.mat.ta$ate[i], y = y.point,
		pch = 19,
		cex = points.cex,
		col = colors[1])
	points(x  = out.mat.tb$ate[i], y = y.point - os,
		pch = 1,
		cex = points.cex,
		col = colors[2])
			}
dev.off()
	
	
###########################################
###graph presets
os = .35
line.os = .01
y.point = .5
ylims = c(0,1.1)
xlims = c(-.35,.35)
points.cex = 1.25
lab.cex = 1.5
line.lwd = 2.5
axis.cex = 1

mean.label.x.os = .04
mean.label.y.os = .12
x.lim.size = 1
colors = brewer.pal(9,'Purples')[c(5,7,9)] ##colors.triple used in plots below

pdf('ideology_combined.pdf',
	width = 6.5, height = 4
	)
par(mfrow = c(3,1)) 
par(mar = c(5,0,1,0))
par(bty = 'n')
	
##ideology subset graph
out.mat = final.mat.ideology[,c('variable','subset','ate','x.mean','x.sd','quantile.lower','quantile.upper')]
out.mat$ate = as.numeric(as.character(out.mat$ate));out.mat$quantile.lower = as.numeric(as.character(out.mat$quantile.lower));out.mat$quantile.upper = as.numeric(as.character(out.mat$quantile.upper));out.mat$x.mean = as.numeric(as.character(out.mat$x.mean)); out.mat$x.sd = as.numeric(as.character(out.mat$x.sd))
out.mat[,c('x.mean','x.sd')] = round(out.mat[,c('x.mean','x.sd')],2)
out.mat$new.x.mean =  paste(out.mat$x.mean,' (',out.mat$x.sd,')',sep='')
out.mat.liberal = out.mat[out.mat$subset == 'liberals.only'&out.mat$variable %in% output.vars,]
out.mat.conservative = out.mat[out.mat$subset == 'conservatives.only'&out.mat$variable %in% output.vars,]
out.mat.moderate = out.mat[out.mat$subset == 'moderates.only'&out.mat$variable %in% output.vars,]

for(i in 1:length(var.names)){
	plot(x  = out.mat.moderate$ate[i], y = y.point, 
		xlim = xlims,
		ylim = ylims,
		ylab = '',
		xlab = var.names[i],
		yaxt = 'n',
		type = 'n',
		cex.lab = lab.cex,
		cex.axis = axis.cex)
	
		lines(x = c(out.mat.liberal$ate[i]+line.os,out.mat.liberal$quantile.upper[i]), 
			y = c(y.point+os,y.point+os),
			lty = 1,
			col = colors[1],
			lwd = line.lwd)
		lines(x = c(out.mat.liberal$quantile.lower[i],out.mat.liberal$ate[i]-line.os), 
			y = c(y.point+os,y.point+os),
			lty = 1,
			col = colors[1],
			lwd = line.lwd)

		lines(x = c(out.mat.moderate$ate[i]+line.os,out.mat.moderate$quantile.upper[i]), 
			y = c(y.point,y.point),
			lty = 1,
			col = colors[2],
			lwd = line.lwd)
		lines(x = c(out.mat.moderate$quantile.lower[i],out.mat.moderate$ate[i]-line.os), 
			y = c(y.point,y.point),
			lty = 1,
			col = colors[2],
			lwd = line.lwd)

		lines(x = c(out.mat.conservative$ate[i]+line.os,out.mat.conservative$quantile.upper[i]), 
			y = c(y.point-os,y.point-os),
			lty = 1,
			col = colors[3],
			lwd = line.lwd)
		lines(x = c(out.mat.conservative$quantile.lower[i],out.mat.conservative$ate[i]-line.os), 
			y = c(y.point-os,y.point-os),
			lty = 1,
			col = colors[3],
			lwd = line.lwd)

		###x means
		text(x  = out.mat.liberal$ate[i]+mean.label.x.os, y = y.point+os+mean.label.y.os,
			labels = out.mat.liberal$new.x.mean[i],
			cex = x.lim.size,
			col = colors[1])
		text(x  = out.mat.moderate$ate[i]+mean.label.x.os, y = y.point+mean.label.y.os,
			labels = out.mat.moderate$new.x.mean[i],
			cex = x.lim.size,
			col = colors[2])
		text(x  = out.mat.conservative$ate[i]+mean.label.x.os, y = y.point-os+mean.label.y.os,
			labels = out.mat.conservative$new.x.mean[i],
			cex = x.lim.size,
			col = colors[3])		

	###labels
	points(x  = out.mat.liberal$ate[i], y = y.point+os,
		pch = "L",
		cex = points.cex,
		col = colors[1])
	points(x  = out.mat.moderate$ate[i], y = y.point,
		pch = "M",
		cex = points.cex,
		col = colors[2])
	points(x  = out.mat.conservative$ate[i], y = y.point-os,
		pch = "C",
		cex = points.cex,
		col = colors[3])

	}
dev.off()

##########################################################
colors = brewer.pal(9,'Greens')[c(5,7,9)] ##colors.triple used in plots below

pdf('friends_combined.pdf',
	width = 6.5, height = 3.75
	)
par(mfrow = c(3,1)) 
par(mar = c(5,0,1,0))
par(bty = 'n')
		

###friends graph
out.mat = final.mat.friends[,c('variable','subset','ate','x.mean','x.sd','quantile.lower','quantile.upper')]
out.mat$ate = as.numeric(as.character(out.mat$ate));out.mat$quantile.lower = as.numeric(as.character(out.mat$quantile.lower));out.mat$quantile.upper = as.numeric(as.character(out.mat$quantile.upper));out.mat$x.mean = as.numeric(as.character(out.mat$x.mean)); out.mat$x.sd = as.numeric(as.character(out.mat$x.sd))
out.mat[,c('x.mean','x.sd')] = round(out.mat[,c('x.mean','x.sd')],2)
out.mat$new.x.mean =  paste(out.mat$x.mean,' (',out.mat$x.sd,')',sep='')

out.mat.low.friends = out.mat[out.mat$subset == 'low.friends'&out.mat$variable %in% output.vars,]
out.mat.middle.friends = out.mat[out.mat$subset == 'middle.friends'&out.mat$variable %in% output.vars,]
out.mat.high.friends = out.mat[out.mat$subset == 'high.friends'&out.mat$variable %in% output.vars,]


for(i in 1:length(var.names)){
	plot(x  = out.mat.middle.friends$ate[i], y = y.point, 
		xlim = xlims,
		ylim = ylims,
		ylab = '',
		xlab = var.names[i],
		yaxt = 'n',
		type = 'n',
		cex.lab = lab.cex,
		cex.axis = axis.cex)
	
		lines(x = c(out.mat.low.friends$ate[i]+line.os,out.mat.low.friends$quantile.upper[i]), 
			y = c(y.point+os,y.point+os),
			lty = 1,
			col = colors[1],
			lwd = line.lwd)
		lines(x = c(out.mat.low.friends$quantile.lower[i],out.mat.low.friends$ate[i]-line.os), 
			y = c(y.point+os,y.point+os),
			lty = 1,
			col = colors[1],
			lwd = line.lwd)

		lines(x = c(out.mat.middle.friends$ate[i]+line.os,out.mat.middle.friends$quantile.upper[i]), 
			y = c(y.point,y.point),
			lty = 1,
			col = colors[2],
			lwd = line.lwd)
		lines(x = c(out.mat.middle.friends$quantile.lower[i],out.mat.middle.friends$ate[i]-line.os), 
			y = c(y.point,y.point),
			lty = 1,
			col = colors[2],
			lwd = line.lwd)

		lines(x = c(out.mat.high.friends$ate[i]+line.os,out.mat.high.friends$quantile.upper[i]), 
			y = c(y.point-os,y.point-os),
			lty = 1,
			col = colors[3],
			lwd = line.lwd)
		lines(x = c(out.mat.high.friends$quantile.lower[i],out.mat.high.friends$ate[i]-line.os), 
			y = c(y.point-os,y.point-os),
			lty = 1,
			col = colors[3],
			lwd = line.lwd)

		###x means
		text(x  = out.mat.low.friends$ate[i]+mean.label.x.os, y = y.point+os+mean.label.y.os,
			labels = out.mat.low.friends$new.x.mean[i],
			cex = x.lim.size,
			col = colors[1])
		text(x  = out.mat.middle.friends$ate[i]+mean.label.x.os, y = y.point+mean.label.y.os,
			labels = out.mat.middle.friends$new.x.mean[i],
			cex = x.lim.size,
			col = colors[2])
		text(x  = out.mat.high.friends$ate[i]+mean.label.x.os, y = y.point-os+mean.label.y.os,
			labels = out.mat.high.friends$new.x.mean[i],
			cex = x.lim.size,
			col = colors[3])		

	###labels
	points(x  = out.mat.low.friends$ate[i], y = y.point+os,
		pch = "L",
		cex = points.cex,
		col = colors[1])
	points(x  = out.mat.middle.friends$ate[i], y = y.point,
		pch = "M",
		cex = points.cex,
		col = colors[2])
	points(x  = out.mat.high.friends$ate[i], y = y.point-os,
		pch = "H",
		cex = points.cex,
		col = colors[3])
	}
dev.off()


#############################################
colors = brewer.pal(9,'OrRd')[c(5,7,9)] ##colors.triple used in plots below

pdf('income_combined.pdf',
	width = 6.5, height = 3.75
	)
par(mfrow = c(3,1)) 
par(mar = c(5,0,1,0))
par(bty = 'n')


############income graph
out.mat = final.mat.income[,c('variable','subset','ate','x.mean','x.sd','quantile.lower','quantile.upper')]
out.mat$ate = as.numeric(as.character(out.mat$ate));out.mat$quantile.lower = as.numeric(as.character(out.mat$quantile.lower));out.mat$quantile.upper = as.numeric(as.character(out.mat$quantile.upper));out.mat$x.mean = as.numeric(as.character(out.mat$x.mean)); out.mat$x.sd = as.numeric(as.character(out.mat$x.sd))
out.mat[,c('x.mean','x.sd')] = round(out.mat[,c('x.mean','x.sd')],2)
out.mat$new.x.mean =  paste(out.mat$x.mean,' (',out.mat$x.sd,')',sep='')

out.mat.low.income = out.mat[out.mat$subset == 'low.income'&out.mat$variable %in% output.vars,]
out.mat.middle.income = out.mat[out.mat$subset == 'middle.income'&out.mat$variable %in% output.vars,]
out.mat.high.income = out.mat[out.mat$subset == 'high.income'&out.mat$variable %in% output.vars,]


for(i in 1:length(var.names)){
	plot(x  = out.mat.middle.income$ate[i], y = y.point, 
		xlim = xlims,
		ylim = ylims,
		ylab = '',
		xlab = var.names[i],
		yaxt = 'n',
		type = 'n',
		cex.lab = lab.cex,
		cex.axis = axis.cex)
	
		lines(x = c(out.mat.low.income$ate[i]+line.os,out.mat.low.income$quantile.upper[i]), 
			y = c(y.point+os,y.point+os),
			lty = 1,
			col = colors[1],
			lwd = line.lwd)
		lines(x = c(out.mat.low.income$quantile.lower[i],out.mat.low.income$ate[i]-line.os), 
			y = c(y.point+os,y.point+os),
			lty = 1,
			col = colors[1],
			lwd = line.lwd)

		lines(x = c(out.mat.middle.income$ate[i]+line.os,out.mat.middle.income$quantile.upper[i]), 
			y = c(y.point,y.point),
			lty = 1,
			col = colors[2],
			lwd = line.lwd)
		lines(x = c(out.mat.middle.income$quantile.lower[i],out.mat.middle.income$ate[i]-line.os), 
			y = c(y.point,y.point),
			lty = 1,
			col = colors[2],
			lwd = line.lwd)

		lines(x = c(out.mat.high.income$ate[i]+line.os,out.mat.high.income$quantile.upper[i]), 
			y = c(y.point-os,y.point-os),
			lty = 1,
			col = colors[3],
			lwd = line.lwd)
		lines(x = c(out.mat.high.income$quantile.lower[i],out.mat.high.income$ate[i]-line.os), 
			y = c(y.point-os,y.point-os),
			lty = 1,
			col = colors[3],
			lwd = line.lwd)

		###x means
		text(x  = out.mat.low.income$ate[i]+mean.label.x.os, y = y.point+os+mean.label.y.os,
			labels = out.mat.low.income$new.x.mean[i],
			cex = x.lim.size,
			col = colors[1])
		text(x  = out.mat.middle.income$ate[i]+mean.label.x.os, y = y.point+mean.label.y.os,
			labels = out.mat.middle.income$new.x.mean[i],
			cex = x.lim.size,
			col = colors[2])
		text(x  = out.mat.high.income$ate[i]+mean.label.x.os, y = y.point-os+mean.label.y.os,
			labels = out.mat.high.income$new.x.mean[i],
			cex = x.lim.size,
			col = colors[3])		

	###labels
	points(x  = out.mat.low.income$ate[i], y = y.point+os,
		pch = "L",
		cex = points.cex,
		col = colors[1])
	points(x  = out.mat.middle.income$ate[i], y = y.point,
		pch = "M",
		cex = points.cex,
		col = colors[2])
	points(x  = out.mat.high.income$ate[i], y = y.point-os,
		pch = "H",
		cex = points.cex,
		col = colors[3])

	}
dev.off()

