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

