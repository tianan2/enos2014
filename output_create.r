#output_create.r
	
# ####create output
 output.vars = c('numberim','Remain','Englishlan')
 var.names = c('Number of immigrants be increased?','Children of undocumented be allowed to stay?','English as official language?')

 
 ##main results
 ##cumalative results output
 final.mat.use = rbind(final.mat.main,final.mat.prime)

 final.mat.use$greater.p.value = as.numeric(as.character(final.mat.use$greater.p.value)); final.mat.use$lesser.p.value = as.numeric(as.character(final.mat.use$lesser.p.value)); final.mat.use$ate = as.numeric(as.character(final.mat.use$ate)); final.mat.use$x.mean = as.numeric(as.character(final.mat.use$x.mean)); final.mat.use$x.sd = as.numeric(as.character(final.mat.use$x.sd)); final.mat.use$N = as.numeric(as.character(final.mat.use$N))
 final.mat.use$p.value = final.mat.use$greater.p.value

 final.mat.redact = final.mat.use[,c('variable','subset','ate','p.value','x.mean','x.sd','N')]
 final.mat.redact[,c('ate','p.value','x.mean','x.sd')] = round(final.mat.redact[,c('ate','p.value','x.mean','x.sd')],3)

 final.mat.redact$ate.new = paste(final.mat.redact$ate,' (',final.mat.redact$p.value,')',sep='')
 final.mat.redact$x.mean.new = paste(final.mat.redact$x.mean,' (',final.mat.redact$x.sd,')',sep='')

 out.mat.a = final.mat.redact[final.mat.redact$subset == 'all'&final.mat.redact$variable %in% output.vars,]

 out.mat.a = final.mat.redact[final.mat.redact$subset == 'all'&final.mat.redact$variable %in% output.vars,c('ate.new')]	
 out.mat.c = final.mat.redact[final.mat.redact$subset == 'no.car'&final.mat.redact$variable %in% output.vars,c('ate.new')]
 out.mat.x = final.mat.redact[final.mat.redact$subset == 'all'&final.mat.redact$variable %in% output.vars,c('x.mean.new')]
 Ns = c('N',max(final.mat.redact$N[final.mat.redact$subset=='all']),
	max(final.mat.redact$N[final.mat.redact$subset=='no.car']),
	max(final.mat.redact$N[final.mat.redact$subset=='all'])
	)

 h1 = c('',paste('(',1:3,')',sep = ''))
 h2 = c('','all respondents','waits on platform','all respondents')
 h3 = c('question','ATE (p)','CATE (p)','T1 levels (sd)')
 hs = rbind(h1,h2,h3)
 row.names(hs) = NULL
	
 out.mat = cbind(out.mat.a,cbind(out.mat.c,out.mat.x))
 out.mat = cbind(var.names,out.mat)
 out.mat = rbind(out.mat,Ns)
 
 out.mat = rbind(hs,out.mat)
 
 out.table = xtable(out.mat, digits = 3
	)
 print(out.table,file = 'cum_results_wide.tex',
	 floating = FALSE,
	 include.rownames = FALSE,
	 include.colnames = FALSE)


 ##prime results
 out.mat.p = final.mat.redact[final.mat.redact$subset == 'all.prime'&final.mat.redact$variable %in% output.vars,c('ate.new')]
 Ns = c('N',max(final.mat.redact$N[final.mat.redact$subset=='all.prime']),
	max(final.mat.redact$N[final.mat.redact$subset=='all'])
	)

 h1 = c('',paste('(',1:2,')',sep = ''))
 h2 = c('','prime experiment','all respondents')
 h3 = c('question','ATE (p)','ATE (p)')	
 hs = rbind(h1,h2,h3)
 row.names(hs) = NULL
	
 out.mat = cbind(out.mat.p,out.mat.a)
 out.mat = cbind(var.names,out.mat)
 out.mat = rbind(out.mat,Ns)
 out.mat = rbind(hs,out.mat)
 out.table = xtable(out.mat, digits = 3
	)
 print(out.table,file = 'prime_results_wide.tex',
	 floating = FALSE,
	 include.rownames = FALSE,
	 include.colnames = FALSE)
