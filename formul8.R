
 ####################------  Damage -------####################


 d=read.table("ALID.txt")

	library("nlstools")

		D=d$V2
		W=d$V1
		y=d$V3

	formulaExp <- as.formula(y ~ a+(D^b)*(W^c))

	s=nls(formulaExp, start = list(a = 2, b = 1.6, c = 1))
		summary(s)
			try(s)
				overview(s)



       yp=predict(s)
