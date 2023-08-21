/**********************************************************************
 * Notas de Matemáticas Actuariales del Seguro de Personas II;
 * Jose Enrique Perez ;
 * Facultad de Ciencias. Universidad Nacional Autónoma de México ;
 **********************************************************************/

/*
Exercise 1
Term of the new insurance
*/

proc iml;	

	/* Equation */
	start func(m);
		/* # to multiply element by element */
		/* ## to power element by element */
		q=0.016381328;
		A=0.956332489;
		e =  q#(1-exp(-(m-17)#log(1.06)))/log(1.06) +1.06##(-(m-17))#(1-(m-17)#q)-A;
	   	return(e);
	finish;
	
	/* Plot the function */
	m = do(17,18,0.01);
	f = func(m);
   call Series(m,f) 
	grid={m f} label={"m" "f"}; 
	
	/* The root is in this inverval */
	intervals = {17   18}; 
	
	/* Solution */
	roots = froot("func", intervals);
	print roots;

quit;


