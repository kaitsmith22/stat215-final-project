/*************** Example code to create SAS library for IAI public use datasets ***************/

/** Step 1: Assign libname IAI to location where SAS datasets and formats are stored **/
/** NOTE: Replace string in double quotations with your directory where the IAI SAS datasets are stored **/
libname IAI "C:\My Files\IAI PUD\Datasets\SAS";  *;

/** Step 2: Set options to access format library **/
options fmtsearch=(IAI work); 

/** Step 3: View dataset contents (examples) **/
proc contents data=IAI.demographics; run;
proc contents data=IAI.form1; run;
