/**********************************************************************
* Notas de Clase de Matematicas Actuariales del Seguro de Personas II ;
* Jose Enrique Perez ;
* Facultad de Ciencias. Universidad Nacional Autonoma de Mexico ;
**********************************************************************/

/***********
* Chapter 5;
* Exercise 1:
***********/


/*******************
/* Extracción de Insumos ;
*******************/

%let origen=/folders/myfolders/masp2/masp2;
%include "&origen./configuration.sas";

* Importing the loss distribution ;
FILENAME REFFILE1 "&origen./inputs/files/chapter5.xlsx";

PROC IMPORT DATAFILE=REFFILE1
	DBMS=XLSX
	OUT=WORK.ex1a;
	GETNAMES=YES;
	SHEET=ex1a;
RUN;

data input.lossdist (label="Loss distribution");
	format x comma3. fx fxa comma6.3;
	set WORK.ex1a;
	label x = "Loss amount (in 25 USD)"
		fx = "Probability"
		fxa = "Distribution function";
	if first.fx then fxa = fx;
	fxa + fx;
run;

* Importing the frequency distribution;

PROC IMPORT DATAFILE=REFFILE1
	DBMS=XLSX
	OUT=WORK.ex1b;
	GETNAMES=YES;
	SHEET=ex1b;
RUN;

data input.freqdist (label="Frequency probability function");
	format n comma3. pn pna comma6.3;
	set work.ex1b;
	label n = "Number of persons per insurance certificate"
	pn = "Probability"
	pna = "Distribution function";
	if first.pn then pna = pn;
	pna + pn;
	
run;

/*
* Exploratory analysis;
*/

ods graphics / reset width=6.4in height=4.8in imagemap noborder;

title 'Loss distribution';
proc sgplot data=input.lossdist;
	step x=x y=fx / lineattrs=(color=orange thickness=2);
	step x=x y=fxa / y2axis lineattrs=(color=blue thickness=2);
	xaxis grid;
	yaxis grid;
run;
title;

title 'Frequency distribution';
proc sgplot data=input.freqdist;
	step x=n y=pn / lineattrs=(color=red thickness=2);
	step x=n y=pna / y2axis lineattrs=(color=black thickness=2);
	xaxis grid;
	yaxis grid;
run;
title;

/*
* Solution of the exercise
*/

title "a) Determine the probability function of S";

* First, we calculate the convolution;

proc iml;
	edit input.freqdist;
	read all var _NUM_ into freqdist[colname=numVars];
	close input.freqdist; 	

	edit input.lossdist;
	read all var _NUM_ into lossdist[colname=numVars];
	close input.lossdist; 	
	
	maxl = max(lossdist[,1]);
	maxn = max(freqdist[,1]);
	maxs = maxl*maxn;
	nrow = maxl*maxn+1;
	ncol = maxn+2;
	
	S = J(nrow,ncol+1,0);
	
	do i = 0 to maxs;
		S[i+1,1]=i;
	end;
	
	do i = 2 to ncol;
		do j = 0 to maxs;
			if i = 2 & j = 0 then S[j+1,i] = 1; 
			if i >= 3 then 
				do k = 1 to maxl;
					do l = 0 to j;
						if (lossdist[k,1] + S[l+1,1]) = j then 
						S[j+1,i] = S[j+1,i] + S[l+1,i-1]*lossdist[k,2];
					end;
				end;
		end;
	end;
	
	
	aux2 = freqdist[,2];
	
	do j = 0 to maxs;
		aux1 = S[j+1,2:10];
		S[j+1,11]=aux1*aux2;
	end;
	
	create work.S from S;
	append from S;
	close work.S;

run;

data rslt.S(drop=col:);
	label s = "Total loss" ps = "Probability" psa = "Distribution function";
	format s comma3. ps psa comma16.8;
	set work.s;
	s = col1;
	ps = col11;
	if first.ps then psa = ps;
	psa + ps;	
run;

title2 "Probability function of S";
proc print data=rslt.S label;
run;

title2 'Total loss distribution';
proc sgplot data=rslt.s;
	step x=s y=ps / lineattrs=(color=green thickness=2);
	step x=s y=psa / y2axis lineattrs=(color=brown thickness=2);
	xaxis grid;
	yaxis grid;
run;


title "b) Determine the mean and standard deviation of total payments per employee (through definition and by $E[S]=E[X]E[N]$ and $Var(S)=E[N]Var(X)+E[X]^2 Var(N)$)";

proc iml;
	edit input.freqdist;
	read all var _NUM_ into freqdist[colname=numVars];
	close input.freqdist; 	

	edit input.lossdist;
	read all var _NUM_ into lossdist[colname=numVars];
	close input.lossdist; 
	
	print "Frequency distribution moments";

	enl = "E[N]";
	en = freqdist[,1]`*freqdist[,2];
	en2l = "E[N^2]";
	en2 = (freqdist[,1]#freqdist[,1])`*freqdist[,2];
	vnl = "Var[N]";
	vn = en2-en**2;
	
	print en[label=enl] en2[label=en2l] vn[label=vnl];
	

	print "Loss distribution moments, in units of 25 dollars";

	exl = "E[X]";
	ex = lossdist[,1]`*lossdist[,2];
	ex2l = "E[X^2]";
	ex2 = (lossdist[,1]#lossdist[,1])`*lossdist[,2];
	vxl = "Var[X]";
	vx = ex2-ex**2;
	
	print ex[label=exl] ex2[label=ex2l] vx[label=vxl];

	print "Total payments per employee moments, in units of 25 dollars";
	
	esl = "E[S]";
	es = ex*en;
	vsl = "Var[S]";
	vs = en*vx + ex**2*vn;
	sdsl = "SD[S]";
	sds = vs**0.5;
	
	print es[label=esl] sds[label=sdsl];
	
	call symputx("es",es,"L");
	
run;

title "c) Calculate P[S>E[S]]";

proc sql;
	select sum(ps) label="P[S>E[S]]"
	from rslt.S
	where s > &es.
	;
quit;

title 


