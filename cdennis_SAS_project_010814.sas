/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Friday, January 09, 2015     TIME: 11:16:32 AM
PROJECT: cdennis_SAS_project_010914
PROJECT PATH: P:\QAC\qac200\students\cdennis\Assignments\cdennis_SAS_project_010914.egp
---------------------------------------- */

/* Library assignment for Local.DATA1 */
Libname DATA1 V9 'P:\QAC\qac200\students\cdennis\Assignments' ;
/* Library assignment for Local.DATA1 */
Libname DATA1 V9 'P:\QAC\qac200\students\cdennis\Assignments' ;


/* Conditionally delete set of tables or views, if they exists          */
/* If the member does not exist, then no action is performed   */
%macro _eg_conditional_dropds /parmbuff;
	
   	%local num;
   	%local stepneeded;
   	%local stepstarted;
   	%local dsname;
	%local name;

   	%let num=1;
	/* flags to determine whether a PROC SQL step is needed */
	/* or even started yet                                  */
	%let stepneeded=0;
	%let stepstarted=0;
   	%let dsname= %qscan(&syspbuff,&num,',()');
	%do %while(&dsname ne);	
		%let name = %sysfunc(left(&dsname));
		%if %qsysfunc(exist(&name)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;

			%end;
				drop table &name;
		%end;

		%if %sysfunc(exist(&name,view)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;
			%end;
				drop view &name;
		%end;
		%let num=%eval(&num+1);
      	%let dsname=%qscan(&syspbuff,&num,',()');
	%end;
	%if &stepstarted %then %do;
		quit;
	%end;
%mend _eg_conditional_dropds;


/* Build where clauses from stored process parameters */

%macro _eg_WhereParam( COLUMN, PARM, OPERATOR, TYPE=S, MATCHALL=_ALL_VALUES_, MATCHALL_CLAUSE=1, MAX= , IS_EXPLICIT=0);

  %local q1 q2 sq1 sq2;
  %local isEmpty;
  %local isEqual isNotEqual;
  %local isIn isNotIn;
  %local isString;
  %local isBetween;

  %let isEqual = ("%QUPCASE(&OPERATOR)" = "EQ" OR "&OPERATOR" = "=");
  %let isNotEqual = ("%QUPCASE(&OPERATOR)" = "NE" OR "&OPERATOR" = "<>");
  %let isIn = ("%QUPCASE(&OPERATOR)" = "IN");
  %let isNotIn = ("%QUPCASE(&OPERATOR)" = "NOT IN");
  %let isString = (%QUPCASE(&TYPE) eq S or %QUPCASE(&TYPE) eq STRING );
  %if &isString %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%");
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq D or %QUPCASE(&TYPE) eq DATE %then 
  %do;
    %let q1=%str(%");
    %let q2=%str(%"d);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq T or %QUPCASE(&TYPE) eq TIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"t);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq DT or %QUPCASE(&TYPE) eq DATETIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"dt);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else
  %do;
    %let q1=;
    %let q2=;
	%let sq1=;
    %let sq2=;
  %end;
  
  %if "&PARM" = "" %then %let PARM=&COLUMN;

  %let isBetween = ("%QUPCASE(&OPERATOR)"="BETWEEN" or "%QUPCASE(&OPERATOR)"="NOT BETWEEN");

  %if "&MAX" = "" %then %do;
    %let MAX = &parm._MAX;
    %if &isBetween %then %let PARM = &parm._MIN;
  %end;

  %if not %symexist(&PARM) or (&isBetween and not %symexist(&MAX)) %then %do;
    %if &IS_EXPLICIT=0 %then %do;
		not &MATCHALL_CLAUSE
	%end;
	%else %do;
	    not 1=1
	%end;
  %end;
  %else %if "%qupcase(&&&PARM)" = "%qupcase(&MATCHALL)" %then %do;
    %if &IS_EXPLICIT=0 %then %do;
	    &MATCHALL_CLAUSE
	%end;
	%else %do;
	    1=1
	%end;	
  %end;
  %else %if (not %symexist(&PARM._count)) or &isBetween %then %do;
    %let isEmpty = ("&&&PARM" = "");
    %if (&isEqual AND &isEmpty AND &isString) %then
       &COLUMN is null;
    %else %if (&isNotEqual AND &isEmpty AND &isString) %then
       &COLUMN is not null;
    %else %do;
	   %if &IS_EXPLICIT=0 %then %do;
           &COLUMN &OPERATOR %unquote(&q1)&&&PARM%unquote(&q2)
	   %end;
	   %else %do;
	       &COLUMN &OPERATOR %unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2))
	   %end;
       %if &isBetween %then 
          AND %unquote(&q1)&&&MAX%unquote(&q2);
    %end;
  %end;
  %else 
  %do;
	%local emptyList;
  	%let emptyList = %symexist(&PARM._count);
  	%if &emptyList %then %let emptyList = &&&PARM._count = 0;
	%if (&emptyList) %then
	%do;
		%if (&isNotin) %then
		   1;
		%else
			0;
	%end;
	%else %if (&&&PARM._count = 1) %then 
    %do;
      %let isEmpty = ("&&&PARM" = "");
      %if (&isIn AND &isEmpty AND &isString) %then
        &COLUMN is null;
      %else %if (&isNotin AND &isEmpty AND &isString) %then
        &COLUMN is not null;
      %else %do;
	    %if &IS_EXPLICIT=0 %then %do;
            &COLUMN &OPERATOR (%unquote(&q1)&&&PARM%unquote(&q2))
	    %end;
		%else %do;
		    &COLUMN &OPERATOR (%unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2)))
		%end;
	  %end;
    %end;
    %else 
    %do;
       %local addIsNull addIsNotNull addComma;
       %let addIsNull = %eval(0);
       %let addIsNotNull = %eval(0);
       %let addComma = %eval(0);
       (&COLUMN &OPERATOR ( 
       %do i=1 %to &&&PARM._count; 
          %let isEmpty = ("&&&PARM&i" = "");
          %if (&isString AND &isEmpty AND (&isIn OR &isNotIn)) %then
          %do;
             %if (&isIn) %then %let addIsNull = 1;
             %else %let addIsNotNull = 1;
          %end;
          %else
          %do;		     
            %if &addComma %then %do;,%end;
			%if &IS_EXPLICIT=0 %then %do;
                %unquote(&q1)&&&PARM&i%unquote(&q2) 
			%end;
			%else %do;
			    %unquote(%nrstr(&sq1))&&&PARM&i%unquote(%nrstr(&sq2)) 
			%end;
            %let addComma = %eval(1);
          %end;
       %end;) 
       %if &addIsNull %then OR &COLUMN is null;
       %else %if &addIsNotNull %then AND &COLUMN is not null;
       %do;)
       %end;
    %end;
  %end;
%mend;

/* ---------------------------------- */
/* MACRO: enterpriseguide             */
/* PURPOSE: define a macro variable   */
/*   that contains the file system    */
/*   path of the WORK library on the  */
/*   server.  Note that different     */
/*   logic is needed depending on the */
/*   server type.                     */
/* ---------------------------------- */
%macro enterpriseguide;
%global sasworklocation;
%local tempdsn unique_dsn path;

%if &sysscp=OS %then %do; /* MVS Server */
	%if %sysfunc(getoption(filesystem))=MVS %then %do;
        /* By default, physical file name will be considered a classic MVS data set. */
	    /* Construct dsn that will be unique for each concurrent session under a particular account: */
		filename egtemp '&egtemp' disp=(new,delete); /* create a temporary data set */
 		%let tempdsn=%sysfunc(pathname(egtemp)); /* get dsn */
		filename egtemp clear; /* get rid of data set - we only wanted its name */
		%let unique_dsn=".EGTEMP.%substr(&tempdsn, 1, 16).PDSE"; 
		filename egtmpdir &unique_dsn
			disp=(new,delete,delete) space=(cyl,(5,5,50))
			dsorg=po dsntype=library recfm=vb
			lrecl=8000 blksize=8004 ;
		options fileext=ignore ;
	%end; 
 	%else %do; 
        /* 
		By default, physical file name will be considered an HFS 
		(hierarchical file system) file. 
		*/
		%if "%sysfunc(getoption(filetempdir))"="" %then %do;
			filename egtmpdir '/tmp';
		%end;
		%else %do;
			filename egtmpdir "%sysfunc(getoption(filetempdir))";
		%end;
	%end; 
	%let path=%sysfunc(pathname(egtmpdir));
    %let sasworklocation=%sysfunc(quote(&path));  
%end; /* MVS Server */
%else %do;
	%let sasworklocation = "%sysfunc(getoption(work))/";
%end;
%if &sysscp=VMS_AXP %then %do; /* Alpha VMS server */
	%let sasworklocation = "%sysfunc(getoption(work))";                         
%end;
%if &sysscp=CMS %then %do; 
	%let path = %sysfunc(getoption(work));                         
	%let sasworklocation = "%substr(&path, %index(&path,%str( )))";
%end;
%mend enterpriseguide;

%enterpriseguide

/* save the current settings of XPIXELS and YPIXELS */
/* so that they can be restored later               */
%macro _sas_pushchartsize(new_xsize, new_ysize);
	%global _savedxpixels _savedypixels;
	options nonotes;
	proc sql noprint;
	select setting into :_savedxpixels
	from sashelp.vgopt
	where optname eq "XPIXELS";
	select setting into :_savedypixels
	from sashelp.vgopt
	where optname eq "YPIXELS";
	quit;
	options notes;
	GOPTIONS XPIXELS=&new_xsize YPIXELS=&new_ysize;
%mend;

/* restore the previous values for XPIXELS and YPIXELS */
%macro _sas_popchartsize;
	%if %symexist(_savedxpixels) %then %do;
		GOPTIONS XPIXELS=&_savedxpixels YPIXELS=&_savedypixels;
		%symdel _savedxpixels / nowarn;
		%symdel _savedypixels / nowarn;
	%end;
%mend;

ODS PROCTITLE;
OPTIONS DEV=ACTIVEX;
GOPTIONS XPIXELS=0 YPIXELS=0;
FILENAME EGSRX TEMP;
ODS tagsets.sasreport13(ID=EGSRX) FILE=EGSRX
    STYLE=HtmlBlue
    STYLESHEET=(URL="file:///C:/Program%20Files/SASHome/SASEnterpriseGuide/6.1/Styles/HtmlBlue.css")
    NOGTITLE
    NOGFOOTNOTE
    GPATH=&sasworklocation
    ENCODING=UTF8
    options(rolap="on")
;

/*   START OF NODE: Assign Project Library (DATA1)   */
%LET _CLIENTTASKLABEL='Assign Project Library (DATA1)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\Assignments\cdennis_SAS_project_010914.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_010914.egp';

GOPTIONS ACCESSIBLE;
LIBNAME DATA1  "P:\QAC\qac200\students\cdennis\Assignments" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\Assignments\cdennis_SAS_project_010914.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_010914.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\Data\MEPS";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 11:15:18 AM
   By task: Data Set Attributes

   Input Data: P:\QAC\qac200\Data\MEPS\meps_fullyr_2012.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsFormeps_fullyr_2012);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=ECLIB000.meps_fullyr_2012 OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTContentsFormeps_fullyr_2012(LABEL="Contents Details for meps_fullyr_2012");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM WORK.CONTContentsFormeps_fullyr_2012
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTContentsFormeps_fullyr_2012 OUT=WORK.CONTContentsFormeps_fullyr_2012;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTContentsFormeps_fullyr_2012
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Selected Variables, Age 18 or Older   */
%LET _CLIENTTASKLABEL='Selected Variables, Age 18 or Older';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\Assignments\cdennis_SAS_project_010914.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_010914.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(DATA1.cdennis);

PROC SQL;
   CREATE TABLE DATA1.cdennis(label="cdennis") AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.MARRY12X, 
          t1.SEX, 
          t1.EDUCYR, 
          t1.EDUYRDEG, 
          t1.REGION12, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.ADAPPT42, 
          t1.ADEXPL42, 
          t1.ADLIST42, 
          t1.ADTLHW42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADILWW42, 
          t1.ADRTWW42, 
          t1.ADFFRM42, 
          t1.ADRTCR42, 
          t1.ADSPEC42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADNSMK42, 
          t1.ADEGMC42, 
          t1.ADSPRF42, 
          t1.ADILCR42, 
          t1.ADNDCR42, 
          t1.ADDPRS42, 
          t1.ADINTR42, 
          t1.PHQ242, 
          t1.ADDRBP42, 
          t1.ADHOPE42, 
          t1.ADNERV42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADWRTH42, 
          t1.ADEFRT42, 
          t1.K6SUM42, 
          t1.ADCAPE42, 
          t1.ADDOWN42, 
          t1.ADNRGY42, 
          t1.ADSOCA42, 
          t1.ADMALS42, 
          t1.ADPALS42, 
          t1.ADPAIN42, 
          t1.ADMWLM42, 
          t1.ADPWLM42, 
          t1.ADOVER42, 
          t1.ADSMOK42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADINSA42, 
          t1.ADGENH42, 
          t1.ADINSB42, 
          t1.ADCLIM42, 
          t1.ADDAYA42, 
          t1.ADLANG42, 
          t1.ADRISK42, 
          t1.SFFLAG42, 
          t1.ADPRX42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.INSCOV12, 
          t1.UNINS12, 
          t1.OBEPRV12, 
          t1.OPBEV12, 
          t1.OPAEV12, 
          t1.MCDEV12, 
          t1.MCREV12, 
          t1.TRIEV12, 
          t1.PRVEV12, 
          t1.YNOINS31, 
          t1.YNOINS42, 
          t1.YNOINS53, 
          t1.OFREMP31, 
          t1.OFREMP42, 
          t1.OFREMP53, 
          t1.OFFER31X, 
          t1.OFFER42X, 
          t1.OFFER53X, 
          t1.OFFHOU42, 
          t1.HELD31X, 
          t1.HELD42X, 
          t1.HELD53X, 
          t1.PMUNPR42, 
          t1.PMUNRS42, 
          t1.PMUNAB42, 
          t1.CHECK53, 
          t1.TYPEPE42, 
          t1.RTHLTH31, 
          t1.RTHLTH42, 
          t1.RTHLTH53, 
          t1.MESBPR42, 
          t1.MESHGT42, 
          t1.OBTOTV12, 
          t1.ERTOT12
      FROM EC100005.meps_fullyr_2012 t1
      WHERE t1.AGE12X >= 18
      ORDER BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: cdennis_SAS_project_010814   */
%LET SYSLAST=DATA1.CDENNIS;
%LET _CLIENTTASKLABEL='cdennis_SAS_project_010814';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\Assignments\cdennis_SAS_project_010914.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_010914.egp';
%LET _SASPROGRAMFILE='P:\QAC\qac200\students\cdennis\SAS Program Code\cdennis_SAS_project_010814.sas';

GOPTIONS ACCESSIBLE;

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 08, 2015 at 11:12:04 AM
   By task: Data Set Attributes1

   Input Data: Local:DATA1.CDENNIS
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsForCDENNIS);
TITLE"Data Set Attributes for subset data set";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";


PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=DATA1.CDENNIS ;

RUN;





GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;


/*   START OF NODE: One Way Frequencies fo r2012 MEPS data subset    */
%LET _CLIENTTASKLABEL='One Way Frequencies fo r2012 MEPS data subset ';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\Assignments\cdennis_SAS_project_010914.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_010914.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 11:15:18 AM
   By task: One Way Frequencies fo r2012 MEPS data subset 

   Input Data: Local:DATA1.CDENNIS
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:DATA1.CDENNIS
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.AGE12X, T.MARRY12X, T.SEX, T.EDUCYR, T.EDUYRDEG, T.REGION12, T.EMPST31, T.EMPST42, T.EMPST53, T.ADAPPT42, T.ADEXPL42, T.ADLIST42, T.ADTLHW42, T.ADINST42, T.ADEZUN42, T.ADRESP42, T.ADPRTM42, T.ADILWW42, T.ADRTWW42, T.ADFFRM42
		     , T.ADRTCR42, T.ADSPEC42, T.ADFHLP42, T.ADHECR42, T.ADNSMK42, T.ADEGMC42, T.ADSPRF42, T.ADILCR42, T.ADNDCR42, T.ADDPRS42, T.ADINTR42, T.PHQ242, T.ADDRBP42, T.ADHOPE42, T.ADNERV42, T.ADREST42, T.ADSAD42, T.ADWRTH42, T.ADEFRT42
		     , T.K6SUM42, T.ADCAPE42, T.ADDOWN42, T.ADNRGY42, T.ADSOCA42, T.ADMALS42, T.ADPALS42, T.ADPAIN42, T.ADMWLM42, T.ADPWLM42, T.ADOVER42, T.ADSMOK42, T.ADCMPD42, T.ADCMPM42, T.ADCMPY42, T.ADINSA42, T.ADGENH42, T.ADINSB42
		     , T.ADCLIM42, T.ADDAYA42, T.ADLANG42, T.ADRISK42, T.SFFLAG42, T.ADPRX42, T.MCS42, T.PCS42, T.INSCOV12, T.UNINS12, T.OBEPRV12, T.OPBEV12, T.OPAEV12, T.MCDEV12, T.MCREV12, T.TRIEV12, T.PRVEV12, T.YNOINS31, T.YNOINS42, T.YNOINS53
		     , T.OFREMP31, T.OFREMP42, T.OFREMP53, T.OFFER31X, T.OFFER42X, T.OFFER53X, T.OFFHOU42, T.HELD31X, T.HELD42X, T.HELD53X, T.PMUNPR42, T.PMUNRS42, T.PMUNAB42, T.CHECK53, T.TYPEPE42, T.RTHLTH31, T.RTHLTH42, T.RTHLTH53, T.MESBPR42
		     , T.MESHGT42, T.OBTOTV12, T.ERTOT12
	FROM DATA1.CDENNIS(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "MEPS 2012 Data Subset";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Claire Dennis";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES AGE12X /  SCORES=TABLE;
	TABLES MARRY12X /  SCORES=TABLE;
	TABLES SEX /  SCORES=TABLE;
	TABLES EDUCYR /  SCORES=TABLE;
	TABLES EDUYRDEG /  SCORES=TABLE;
	TABLES REGION12 /  SCORES=TABLE;
	TABLES EMPST31 /  SCORES=TABLE;
	TABLES EMPST42 /  SCORES=TABLE;
	TABLES EMPST53 /  SCORES=TABLE;
	TABLES ADAPPT42 /  SCORES=TABLE;
	TABLES ADEXPL42 /  SCORES=TABLE;
	TABLES ADLIST42 /  SCORES=TABLE;
	TABLES ADTLHW42 /  SCORES=TABLE;
	TABLES ADINST42 /  SCORES=TABLE;
	TABLES ADEZUN42 /  SCORES=TABLE;
	TABLES ADRESP42 /  SCORES=TABLE;
	TABLES ADPRTM42 /  SCORES=TABLE;
	TABLES ADILWW42 /  SCORES=TABLE;
	TABLES ADRTWW42 /  SCORES=TABLE;
	TABLES ADFFRM42 /  SCORES=TABLE;
	TABLES ADRTCR42 /  SCORES=TABLE;
	TABLES ADSPEC42 /  SCORES=TABLE;
	TABLES ADFHLP42 /  SCORES=TABLE;
	TABLES ADHECR42 /  SCORES=TABLE;
	TABLES ADNSMK42 /  SCORES=TABLE;
	TABLES ADEGMC42 /  SCORES=TABLE;
	TABLES ADSPRF42 /  SCORES=TABLE;
	TABLES ADILCR42 /  SCORES=TABLE;
	TABLES ADNDCR42 /  SCORES=TABLE;
	TABLES ADDPRS42 /  SCORES=TABLE;
	TABLES ADINTR42 /  SCORES=TABLE;
	TABLES PHQ242 /  SCORES=TABLE;
	TABLES ADDRBP42 /  SCORES=TABLE;
	TABLES ADHOPE42 /  SCORES=TABLE;
	TABLES ADNERV42 /  SCORES=TABLE;
	TABLES ADREST42 /  SCORES=TABLE;
	TABLES ADSAD42 /  SCORES=TABLE;
	TABLES ADWRTH42 /  SCORES=TABLE;
	TABLES ADEFRT42 /  SCORES=TABLE;
	TABLES K6SUM42 /  SCORES=TABLE;
	TABLES ADCAPE42 /  SCORES=TABLE;
	TABLES ADDOWN42 /  SCORES=TABLE;
	TABLES ADNRGY42 /  SCORES=TABLE;
	TABLES ADSOCA42 /  SCORES=TABLE;
	TABLES ADMALS42 /  SCORES=TABLE;
	TABLES ADPALS42 /  SCORES=TABLE;
	TABLES ADPAIN42 /  SCORES=TABLE;
	TABLES ADMWLM42 /  SCORES=TABLE;
	TABLES ADPWLM42 /  SCORES=TABLE;
	TABLES ADOVER42 /  SCORES=TABLE;
	TABLES ADSMOK42 /  SCORES=TABLE;
	TABLES ADCMPD42 /  SCORES=TABLE;
	TABLES ADCMPM42 /  SCORES=TABLE;
	TABLES ADCMPY42 /  SCORES=TABLE;
	TABLES ADINSA42 /  SCORES=TABLE;
	TABLES ADGENH42 /  SCORES=TABLE;
	TABLES ADINSB42 /  SCORES=TABLE;
	TABLES ADCLIM42 /  SCORES=TABLE;
	TABLES ADDAYA42 /  SCORES=TABLE;
	TABLES ADLANG42 /  SCORES=TABLE;
	TABLES ADRISK42 /  SCORES=TABLE;
	TABLES SFFLAG42 /  SCORES=TABLE;
	TABLES ADPRX42 /  SCORES=TABLE;
	TABLES MCS42 /  SCORES=TABLE;
	TABLES PCS42 /  SCORES=TABLE;
	TABLES INSCOV12 /  SCORES=TABLE;
	TABLES UNINS12 /  SCORES=TABLE;
	TABLES OBEPRV12 /  SCORES=TABLE;
	TABLES OPBEV12 /  SCORES=TABLE;
	TABLES OPAEV12 /  SCORES=TABLE;
	TABLES MCDEV12 /  SCORES=TABLE;
	TABLES MCREV12 /  SCORES=TABLE;
	TABLES TRIEV12 /  SCORES=TABLE;
	TABLES PRVEV12 /  SCORES=TABLE;
	TABLES YNOINS31 /  SCORES=TABLE;
	TABLES YNOINS42 /  SCORES=TABLE;
	TABLES YNOINS53 /  SCORES=TABLE;
	TABLES OFREMP31 /  SCORES=TABLE;
	TABLES OFREMP42 /  SCORES=TABLE;
	TABLES OFREMP53 /  SCORES=TABLE;
	TABLES OFFER31X /  SCORES=TABLE;
	TABLES OFFER42X /  SCORES=TABLE;
	TABLES OFFER53X /  SCORES=TABLE;
	TABLES OFFHOU42 /  SCORES=TABLE;
	TABLES HELD31X /  SCORES=TABLE;
	TABLES HELD42X /  SCORES=TABLE;
	TABLES HELD53X /  SCORES=TABLE;
	TABLES PMUNPR42 /  SCORES=TABLE;
	TABLES PMUNRS42 /  SCORES=TABLE;
	TABLES PMUNAB42 /  SCORES=TABLE;
	TABLES CHECK53 /  SCORES=TABLE;
	TABLES TYPEPE42 /  SCORES=TABLE;
	TABLES RTHLTH31 /  SCORES=TABLE;
	TABLES RTHLTH42 /  SCORES=TABLE;
	TABLES RTHLTH53 /  SCORES=TABLE;
	TABLES MESBPR42 /  SCORES=TABLE;
	TABLES MESHGT42 /  SCORES=TABLE;
	TABLES OBTOTV12 /  SCORES=TABLE;
	TABLES ERTOT12 /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Recode Variables   */
%LET _CLIENTTASKLABEL='Recode Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\Assignments\cdennis_SAS_project_010914.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_010914.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.SUBSET_MEPS2012_Managed);

PROC SQL;
   CREATE TABLE WORK."SUBSET_MEPS2012_Managed"n AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.MARRY12X, 
          t1.SEX, 
          t1.EDUCYR, 
          t1.EDUYRDEG, 
          t1.REGION12, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.ADAPPT42, 
          t1.ADEXPL42, 
          t1.ADLIST42, 
          t1.ADTLHW42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADILWW42, 
          t1.ADRTWW42, 
          t1.ADFFRM42, 
          t1.ADRTCR42, 
          t1.ADSPEC42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADNSMK42, 
          t1.ADEGMC42, 
          t1.ADSPRF42, 
          t1.ADILCR42, 
          t1.ADNDCR42, 
          t1.ADDPRS42, 
          t1.ADINTR42, 
          t1.PHQ242, 
          t1.ADDRBP42, 
          t1.ADHOPE42, 
          t1.ADNERV42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADWRTH42, 
          t1.ADEFRT42, 
          t1.K6SUM42, 
          t1.ADCAPE42, 
          t1.ADDOWN42, 
          t1.ADNRGY42, 
          t1.ADSOCA42, 
          t1.ADMALS42, 
          t1.ADPALS42, 
          t1.ADPAIN42, 
          t1.ADMWLM42, 
          t1.ADPWLM42, 
          t1.ADOVER42, 
          t1.ADSMOK42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADINSA42, 
          t1.ADGENH42, 
          t1.ADINSB42, 
          t1.ADCLIM42, 
          t1.ADDAYA42, 
          t1.ADLANG42, 
          t1.ADRISK42, 
          t1.SFFLAG42, 
          t1.ADPRX42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.INSCOV12, 
          t1.UNINS12, 
          t1.OBEPRV12, 
          t1.OPBEV12, 
          t1.OPAEV12, 
          t1.MCDEV12, 
          t1.MCREV12, 
          t1.TRIEV12, 
          t1.PRVEV12, 
          t1.YNOINS31, 
          t1.YNOINS42, 
          t1.YNOINS53, 
          t1.OFREMP31, 
          t1.OFREMP42, 
          t1.OFREMP53, 
          t1.OFFER31X, 
          t1.OFFER42X, 
          t1.OFFER53X, 
          t1.OFFHOU42, 
          t1.HELD31X, 
          t1.HELD42X, 
          t1.HELD53X, 
          t1.PMUNPR42, 
          t1.PMUNRS42, 
          t1.PMUNAB42, 
          t1.CHECK53, 
          t1.TYPEPE42, 
          t1.RTHLTH31, 
          t1.RTHLTH42, 
          t1.RTHLTH53, 
          t1.MESBPR42, 
          t1.MESHGT42, 
          t1.OBTOTV12, 
          t1.ERTOT12, 
          /* ADDAYA42_Health_Limits_Activity */
            (CASE 
               WHEN -1 = t1.ADDAYA42 THEN .
               WHEN -9 = t1.ADDAYA42 THEN .
               ELSE t1.ADDAYA42
            END) LABEL="SAQ Health Limits Activity (Recoded Missing)" AS ADDAYA42_Health_Limits_Activity, 
          /* ADCLIM42_Climbing_Stairs */
            (CASE 
               WHEN -1 = t1.ADCLIM42 THEN .
               WHEN -8 = t1.ADCLIM42 THEN .
               WHEN -9 = t1.ADCLIM42 THEN .
               ELSE t1.ADCLIM42
            END) LABEL="SAQ: Health LImits Climibing Stairs " AS ADCLIM42_Climbing_Stairs, 
          /* ADPALS42_4Wks */
            (CASE 
               WHEN -1 = t1.ADPALS42 THEN .
               WHEN -9 = t1.ADPALS42 THEN .
               ELSE t1.ADPALS42
            END) LABEL="SAQ 4 Wks Accmp less b/c phys prbs (recoded missing) " AS ADPALS42_4Wks, 
          /* ADPWLM42_WorkLimit */
            (CASE 
               WHEN -1 = t1.ADPWLM42 THEN .
               WHEN -9 = t1.ADPWLM42 THEN .
               ELSE t1.ADPWLM42
            END) LABEL="SAQ 4Wks:Work Limit b/c Phys Probs (recoded missing)" AS ADPWLM42_WorkLimit, 
          /* ADMALS42_Mental_Accomp */
            (CASE 
               WHEN -1 = t1.ADMALS42 THEN .
               WHEN -9 = t1.ADMALS42 THEN .
               ELSE t1.ADMALS42
            END) LABEL="SAQ Accomplished less bc mental probs (recoded missing) " AS ADMALS42_Mental_Accomp, 
          /* ADWLM42_WorkLimit__Mental */
            (CASE 
               WHEN -1 = t1.ADMWLM42 THEN .
               WHEN -7 = t1.ADMWLM42 THEN .
               WHEN -8 = t1.ADMWLM42 THEN .
               WHEN -9 = t1.ADMWLM42 THEN .
               ELSE t1.ADMWLM42
            END) LABEL="SAQ 4 Wks Work Limt b/c mnt probs" AS ADWLM42_WorkLimit__Mental, 
          /* ADPAIN42_Pain_Limits_Work */
            (CASE 
               WHEN -1 = t1.ADPAIN42 THEN .
               WHEN -9 = t1.ADPAIN42 THEN .
               ELSE t1.ADPAIN42
            END) LABEL="SAQ 4 Wks: Pain limits normal work (recoded missing) " AS ADPAIN42_Pain_Limits_Work, 
          /* ADCAPE42_Calm */
            (CASE 
               WHEN -1 = t1.ADCAPE42 THEN .
               WHEN -8 = t1.ADCAPE42 THEN .
               WHEN -9 = t1.ADCAPE42 THEN .
               ELSE t1.ADCAPE42
            END) LABEL="SAQ Felt Calm/Peaceful  (recoded missing)" AS ADCAPE42_Calm, 
          /* ADNRGY42_LotEnergy */
            (CASE 
               WHEN -1 = t1.ADNRGY42 THEN .
               WHEN -9 = t1.ADNRGY42 THEN .
               ELSE t1.ADNRGY42
            END) LABEL="SAQ: Had a lot of energy (recoded missing) " AS ADNRGY42_LotEnergy, 
          /* ADGENH42_health_general */
            (CASE 
               WHEN -1 = t1.ADGENH42 THEN .
               WHEN -8 = t1.ADGENH42 THEN .
               WHEN -9 = t1.ADGENH42 THEN .
               ELSE t1.ADGENH42
            END) LABEL="SAQ Health in general  (recoded missing) " AS ADGENH42_health_general, 
          /* ADDOWN42_Felt_Down */
            (CASE 
               WHEN -1 = t1.ADDOWN42 THEN .
               WHEN -8 = t1.ADDOWN42 THEN .
               WHEN -9 = t1.ADDOWN42 THEN .
               ELSE t1.ADDOWN42
            END) LABEL="SAQ 4wks: felt down (recoded missing) " AS ADDOWN42_Felt_Down, 
          /* ADSOCA42_Health_Social_Activity */
            (CASE 
               WHEN -1 = t1.ADSOCA42 THEN .
               WHEN -9 = t1.ADSOCA42 THEN .
               ELSE t1.ADSOCA42
            END) LABEL="SAQ health sotpped social activity (recoded missing) " AS ADSOCA42_Health_Social_Activity, 
          /* ADAPPT42_Num_MedOfficeVisits */
            (CASE 
               WHEN -1 = t1.ADAPPT42 THEN .
               WHEN -8 = t1.ADAPPT42 THEN .
               WHEN -9 = t1.ADAPPT42 THEN .
               ELSE t1.ADAPPT42
            END) LABEL="SAQ 12 Mos: # visits to med off for care (recoded missing)" AS ADAPPT42_Num_MedOfficeVisits, 
          /* ADILWW42_CareWhenNeeded */
            (CASE 
               WHEN -1 = t1.ADILWW42 THEN .
               WHEN -9 = t1.ADILWW42 THEN .
            END) LABEL="SAQ 12 Mos Got care when needed (recoded missing) " AS ADILWW42_CareWhenNeeded, 
          /* ADAPPT42_NumAppts */
            (CASE 
               WHEN -1 = t1.ADAPPT42 THEN .
               WHEN -8 = t1.ADAPPT42 THEN .
               WHEN -9 = t1.ADAPPT42 THEN .
               ELSE t1.ADAPPT42
            END) LABEL="SAQ # appts to office for care (recoded missing) " AS ADAPPT42_NumAppts, 
          /* ADLIST42_Doctor_listens */
            (CASE 
               WHEN -1 = t1.ADLIST42 THEN .
               WHEN -7 = t1.ADLIST42 THEN .
               WHEN -9 = t1.ADLIST42 THEN .
               ELSE t1.ADLIST42
            END) LABEL="SAQ 12 Mo: Doctor listened to you (recoded missing)" AS ADLIST42_Doctor_listens, 
          /* ADHECR42_Qual_HealthC */
            (CASE 
               WHEN -1 = t1.ADHECR42 THEN .
               WHEN -9 = t1.ADHECR42 THEN .
               ELSE t1.ADHECR42
            END) LABEL="SAQ 12 Mo: Rating of health care (recoded missing)" AS ADHECR42_Qual_HealthC, 
          /* ADSMOK42_currently_smoke */
            (CASE 
               WHEN -1 = t1.ADSMOK42 THEN .
               WHEN -9 = t1.ADSMOK42 THEN .
               ELSE t1.ADSMOK42
            END) LABEL="SAQ: currently smoke (recoded missing) " AS ADSMOK42_currently_smoke, 
          /* ADINSA42_Do_Not_Need_Insur */
            (CASE 
               WHEN -1 = t1.ADINSA42 THEN .
               WHEN -7 = t1.ADINSA42 THEN .
               WHEN -8 = t1.ADINSA42 THEN .
               WHEN -9 = t1.ADINSA42 THEN .
               ELSE t1.ADINSA42
            END) LABEL="SAQ: Do not need health insurance (recoded missing) " AS ADINSA42_Do_Not_Need_Insur, 
          /* ADINSB42_HealthInsur */
            (CASE 
               WHEN -1 = t1.ADINSB42 THEN .
               WHEN -7 = t1.ADINSB42 THEN .
               WHEN -8 = t1.ADINSB42 THEN .
               WHEN -9 = t1.ADINSB42 THEN .
               ELSE t1.ADINSB42
            END) LABEL="SAQ: Health Insurance not worth cost (recoded missing)" AS ADINSB42_HealthInsur, 
          /* EDUCYR_Years_edu */
            (CASE 
               WHEN -1 = t1.EDUCYR THEN .
               WHEN -7 = t1.EDUCYR THEN .
               WHEN -8 = t1.EDUCYR THEN .
               WHEN -9 = t1.EDUCYR THEN .
               ELSE t1.EDUCYR
            END) LABEL="years of educ when first entered meps " AS EDUCYR_Years_edu, 
          /* EDUYRDEG_Yrs_Edu */
            (CASE 
               WHEN -1 = t1.EDUCYR THEN .
               WHEN -7 = t1.EDUCYR THEN .
               WHEN -8 = t1.EDUCYR THEN .
               WHEN -9 = t1.EDUCYR THEN .
               ELSE t1.EDUCYR
            END) LABEL="Years of education or highest degree " AS EDUYRDEG_Yrs_Edu, 
          /* MARRY12x_MaritalStatus */
            (CASE 
               WHEN -7 = t1.MARRY12X THEN .
               WHEN -9 = t1.MARRY12X THEN .
               ELSE t1.MARRY12X
            END) LABEL="Marital Satus " AS MARRY12x_MaritalStatus, 
          /* EMPST42_EmploymentStatus */
            (CASE 
               WHEN -1 = t1.EMPST31 THEN .
               WHEN -7 = t1.EMPST31 THEN .
               WHEN -8 = t1.EMPST31 THEN .
               WHEN -9 = t1.EMPST31 THEN .
               ELSE t1.EMPST31
            END) LABEL="Employment Status 4/2 " AS EMPST42_EmploymentStatus, 
          /* EMPST31_Employ_Stat */
            (CASE 
               WHEN -1 = t1.EMPST31 THEN .
               WHEN -7 = t1.EMPST31 THEN .
               WHEN -8 = t1.EMPST31 THEN .
               WHEN -9 = t1.EMPST31 THEN .
               ELSE t1.EMPST31
            END) LABEL="Employment Status 3/1 " AS EMPST31_Employ_Stat, 
          /* EMPST53_Employ_Stat */
            (CASE 
               WHEN -7 = t1.EMPST53 THEN .
               WHEN -8 = t1.EMPST53 THEN .
               WHEN -9 = t1.EMPST53 THEN .
               ELSE t1.EMPST53
            END) LABEL="Employment Status 5/3 " AS EMPST53_Employ_Stat, 
          /* PHQ242_Overall_Feelings */
            (CASE 
               WHEN -1 = t1.PHQ242 THEN .
               WHEN -9 = t1.PHQ242 THEN .
               ELSE t1.PHQ242
            END) LABEL="SAQ 2Wk Overall rating of feelings " AS PHQ242_Overall_Feelings, 
          /* ADNDCR42_NeedCare12Mo */
            (CASE 
               WHEN -1 = t1.ADNDCR42 THEN .
               WHEN -8 = t1.ADNDCR42 THEN .
               WHEN -9 = t1.ADNDCR42 THEN .
               ELSE t1.ADNDCR42
            END) LABEL="SAQ 12Mo: need any care, test, treatment " AS ADNDCR42_NeedCare12Mo, 
          /* PMUNPR42_ProbUnableRx */
            (CASE 
               WHEN -1 = t1.PMUNPR42 THEN .
               WHEN -8 = t1.PMUNPR42 THEN .
               WHEN -9 = t1.PMUNPR42 THEN .
               ELSE t1.PMUNPR42
            END) LABEL="prob unable to to get nec pres med (recoded missing) " AS PMUNPR42_ProbUnableRx, 
          /* PMUNAB42_unable_necRx */
            (CASE 
               WHEN -1 = t1.PMUNAB42 THEN .
               WHEN -7 = t1.PMUNAB42 THEN .
               WHEN -8 = t1.PMUNAB42 THEN .
               WHEN -9 = t1.PMUNAB42 THEN .
               ELSE t1.PMUNAB42
            END) LABEL="unable to get nec pres med (recoded missing)" AS PMUNAB42_unable_necRx, 
          /* YNOINS31_Why_NoHC */
            (CASE 
               WHEN -1 = t1.YNOINS31 THEN .
               WHEN -7 = t1.YNOINS31 THEN .
               WHEN -8 = t1.YNOINS31 THEN .
               WHEN -9 = t1.YNOINS31 THEN .
               ELSE t1.YNOINS31
            END) LABEL="Why not eiigible health insur (recoded missing) " AS YNOINS31_Why_NoHC, 
          /* YNOINS42_why_noHc */
            (CASE 
               WHEN -1 = t1.YNOINS42 THEN .
               WHEN -7 = t1.YNOINS42 THEN .
               WHEN -8 = t1.YNOINS42 THEN .
               WHEN -9 = t1.YNOINS42 THEN .
               ELSE t1.YNOINS42
            END) LABEL="Why not eligible health ins 4/2 " AS YNOINS42_why_noHc, 
          /* YNOINS53_Why_NoHC */
            (CASE 
               WHEN -1 = t1.YNOINS53 THEN .
               WHEN -7 = t1.YNOINS53 THEN .
               WHEN -8 = t1.YNOINS53 THEN .
               WHEN -9 = t1.YNOINS53 THEN .
               ELSE t1.YNOINS53
            END) LABEL="Why not eligible health ins 5/3 " AS YNOINS53_Why_NoHC, 
          /* HELD31x_HealthInsurHeld  */
            (CASE 
               WHEN -1 = t1.HELD31X THEN .
               WHEN -7 = t1.HELD31X THEN .
               WHEN -8 = t1.HELD31X THEN .
               WHEN -9 = t1.HELD31X THEN .
               ELSE t1.HELD31X
            END) LABEL="health insurance held from rd 3/1 (recoded missing) " AS 'HELD31x_HealthInsurHeld 'n, 
          /* HELD42x_HealthInsheld */
            (CASE 
               WHEN -1 = t1.HELD42X THEN .
               WHEN -7 = t1.HELD42X THEN .
               WHEN -8 = t1.HELD42X THEN .
               WHEN -9 = t1.HELD42X THEN .
               ELSE t1.HELD42X
            END) LABEL="health insurance held from 4/2 (recoded missing) " AS HELD42x_HealthInsheld, 
          /* HELD53X_heldInsur */
            (CASE 
               WHEN -1 = t1.HELD53X THEN .
               WHEN -7 = t1.HELD53X THEN .
               WHEN -8 = t1.HELD53X THEN .
               WHEN -9 = t1.HELD53X THEN .
               ELSE t1.HELD53X
            END) LABEL="Health insur held from 5/3 (recoded missing) " AS HELD53X_heldInsur, 
          /* RTHLTH31_healthStatus */
            (CASE 
               WHEN -1 = t1.RTHLTH31 THEN .
               WHEN -7 = t1.RTHLTH31 THEN .
               WHEN -8 = t1.RTHLTH31 THEN .
               WHEN -9 = t1.RTHLTH31 THEN .
               ELSE t1.RTHLTH31
            END) LABEL="perceived health status 3/1 (recoded missing) " AS RTHLTH31_healthStatus, 
          /* RTHLTH42_healthStatus */
            (CASE 
               WHEN -1 = t1.RTHLTH42 THEN .
               WHEN -7 = t1.RTHLTH42 THEN .
               WHEN -8 = t1.RTHLTH42 THEN .
               WHEN -9 = t1.RTHLTH42 THEN .
               ELSE t1.RTHLTH42
            END) LABEL="perceived health status 4/2 (recoded missing) " AS RTHLTH42_healthStatus, 
          /* RTHLTH53_healthStatus */
            (CASE 
               WHEN -1 = t1.RTHLTH53 THEN .
               WHEN -7 = t1.RTHLTH53 THEN .
               WHEN -8 = t1.RTHLTH53 THEN .
               WHEN -9 = t1.RTHLTH53 THEN .
               ELSE t1.RTHLTH53
            END) LABEL="perceived health status 5/3 " AS RTHLTH53_healthStatus, 
          /* MESBPR42_bloodPressure */
            (CASE 
               WHEN -1 = t1.MESBPR42 THEN .
               WHEN -8 = t1.MESBPR42 THEN .
               ELSE t1.MESBPR42
            END) LABEL="Dr Checked blood pressure " AS MESBPR42_bloodPressure
      FROM DATA1.CDENNIS t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis   */
%LET _CLIENTTASKLABEL='Table Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\Assignments\cdennis_SAS_project_010914.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_010914.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 11:15:19 AM
   By task: Table Analysis

   Input Data: Local:WORK.SUBSET_MEPS2012_MANAGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPS2012_MANAGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADDAYA42, T.ADDAYA42_Health_Limits_Activity, T.ADAPPT42_Num_MedOfficeVisits, T.ADAPPT42, T.ADCAPE42, T.ADCAPE42_Calm, T.ADCLIM42_Climbing_Stairs, T.ADCLIM42, T.ADDOWN42_Felt_Down, T.ADDOWN42, T.ADGENH42, T.ADGENH42_health_general
		     , T.ADHECR42, T.ADHECR42_Qual_HealthC, T.ADILWW42, T.ADILWW42_CareWhenNeeded, T.ADINSA42_Do_Not_Need_Insur, T.ADINSA42, T.ADINSB42_HealthInsur, T.ADINSB42, T.ADLIST42, T.ADLIST42_Doctor_listens, T.ADMALS42
		     , T.ADMALS42_Mental_Accomp, T.ADNDCR42, T.ADNDCR42_NeedCare12Mo, T.ADNRGY42, T.ADNRGY42_LotEnergy, T.ADPAIN42, T.ADPAIN42_Pain_Limits_Work, T.ADPALS42, T.ADPALS42_4Wks, T.ADPWLM42, T.ADPWLM42_WorkLimit, T.ADSMOK42, T.ADSMOK42_currently_smoke
		     , T.ADSOCA42, T.ADSOCA42_Health_Social_Activity, T.EDUCYR, T.EDUCYR_Years_edu, T.EDUYRDEG, T.EDUYRDEG_Yrs_Edu, T.EMPST31, T.EMPST31_Employ_Stat, T.EMPST42, T.EMPST42_EmploymentStatus, T.EMPST53, T.EMPST53_Employ_Stat
		     , T.HELD31X, T.HELD31x_HealthInsurHeld, T.HELD42X, T.HELD42x_HealthInsheld, T.HELD53X, T.HELD53X_heldInsur, T.MARRY12X, T.MARRY12x_MaritalStatus, T.MESBPR42, T.MESBPR42_bloodPressure, T.PHQ242, T.PHQ242_Overall_Feelings
		     , T.PMUNAB42, T.PMUNAB42_unable_necRx, T.PMUNPR42, T.PMUNPR42_ProbUnableRx, T.RTHLTH31_healthStatus, T.RTHLTH42, T.RTHLTH42_healthStatus, T.RTHLTH53, T.RTHLTH53_healthStatus, T.YNOINS31, T.YNOINS31_Why_NoHC, T.YNOINS42
		     , T.YNOINS42_why_noHc, T.YNOINS53, T.YNOINS53_Why_NoHC, T.RTHLTH31
	FROM WORK.SUBSET_MEPS2012_MANAGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADDAYA42 * ADDAYA42_Health_Limits_Activity /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADAPPT42 * ADAPPT42_Num_MedOfficeVisits /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADCAPE42 * ADCAPE42_Calm /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADCLIM42 * ADCLIM42_Climbing_Stairs /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADDOWN42 * ADDOWN42_Felt_Down /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADGENH42 * ADGENH42_health_general /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADHECR42 * ADHECR42_Qual_HealthC /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADILWW42 * ADILWW42_CareWhenNeeded /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADINSA42 * ADINSA42_Do_Not_Need_Insur /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADINSB42 * ADINSB42_HealthInsur /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADLIST42 * ADLIST42_Doctor_listens /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADNDCR42 * ADNDCR42_NeedCare12Mo /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADMALS42 * ADMALS42_Mental_Accomp /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADNDCR42 * ADNDCR42_NeedCare12Mo /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADNRGY42 * ADNRGY42_LotEnergy /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADPAIN42 * ADPAIN42_Pain_Limits_Work /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADPALS42 * ADPALS42_4Wks /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADPWLM42 * ADPWLM42_WorkLimit /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADSMOK42 * ADSMOK42_currently_smoke /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADSOCA42 * ADSOCA42_Health_Social_Activity /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES EDUCYR * EDUCYR_Years_edu /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES EDUYRDEG * EDUYRDEG_Yrs_Edu /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES EMPST31 * EMPST31_Employ_Stat /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES EMPST42 * EMPST42_EmploymentStatus /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES EMPST53 * EMPST53_Employ_Stat /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES HELD31X * HELD31x_HealthInsurHeld /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES HELD42X * HELD42x_HealthInsheld /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES HELD53X * HELD53X_heldInsur /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES MARRY12X * MARRY12x_MaritalStatus /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES MESBPR42 * MESBPR42_bloodPressure /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES PHQ242 * PHQ242_Overall_Feelings /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES PMUNPR42 * PMUNPR42_ProbUnableRx /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES RTHLTH31 * RTHLTH31_healthStatus /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES RTHLTH42 * RTHLTH42_healthStatus /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES RTHLTH53 * RTHLTH53_healthStatus /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES YNOINS31 * YNOINS31_Why_NoHC /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES YNOINS42 * YNOINS42_why_noHc /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES YNOINS53 * YNOINS53_Why_NoHC /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
