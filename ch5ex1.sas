/**********************************************************************
* Notas de Clase de Matematicas Actuariales del Seguro de Personas II ;
* Jose Enrique Perez ;
* Facultad de Ciencias. Universidad Nacional Autonoma de Mexico ;
**********************************************************************/

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

* a) Determine the probability function of S;

* First, we calculate the convolution;

proc iml;
	edit input.freqdist;
	read all var _NUM_ into freqdist[colname=numVars];
	close input.freqdist; 	

	edit input.lossdist;
	read all var _NUM_ into lossdist[colname=numVars];
	close input.freqdist; 	
	
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

title 'Total loss distribution';
proc sgplot data=rslt.s;
	step x=s y=ps / lineattrs=(color=green thickness=2);
	xaxis grid;
	yaxis grid;
run;
title;

title 'Total loss distribution';
proc sgplot data=rslt.s;
	step x=s y=ps / lineattrs=(color=green thickness=2);
	step x=s y=psa / y2axis lineattrs=(color=brown thickness=2);
	xaxis grid;
	yaxis grid;
run;
title;


