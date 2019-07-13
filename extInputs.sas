/**********************************************************************
* Notas de Clase de Matematicas Actuariales del Seguro de Personas II ;
* Jose Enrique Perez ;
* Facultad de Ciencias. Universidad Nacional Autonoma de Mexico ;
**********************************************************************/

/*******************
/* Extracción de Insumos ;
*******************/

%let origen=/folders/myfolders/masp1;
%include "&origen./configuration.sas";

%web_drop_table(WORK.IMPORT);

* Importación de la tabla de mortalidad ;
FILENAME REFFILE1 "&origen./inputs/files/ilt.xlsx";

PROC IMPORT DATAFILE=REFFILE1
	DBMS=XLSX
	OUT=WORK.ilt;
	GETNAMES=YES;
RUN;

%web_open_table(WORK.IMPORT);

data input.ilt;
	format age comma3. l_x d_x comma30.2;
	set WORK.ilt;
	label age = "Age"
		l_x = "Number of lives at age x"
		d_x = "Number of deaths when age x";	
run;
