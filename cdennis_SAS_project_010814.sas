/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Tuesday, January 13, 2015     TIME: 3:48:10 PM
PROJECT: cdennis_SAS_project_011315
PROJECT PATH: P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011315.egp
---------------------------------------- */

/* Library assignment for Local.MYDATA1 */
Libname MYDATA1 V9 'P:\QAC\qac200\students\cdennis' ;
/* Library assignment for Local.MYDATA1 */
Libname MYDATA1 V9 'P:\QAC\qac200\students\cdennis' ;


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

/*   START OF NODE: Assign Project Library (MYDATA1)   */
%LET _CLIENTTASKLABEL='Assign Project Library (MYDATA1)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
LIBNAME MYDATA1  "P:\QAC\qac200\students\cdennis" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder   */
LIBNAME EC100014 "P:\QAC\qac200\students\cdennis";


%LET _CLIENTTASKLABEL='Query Builder';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_REV_RECODEDSUBSET_0001);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_REV_RECODEDSUBSET_0001 AS 
   SELECT t1.Categ_Education, 
          t1.Marry_Category, 
          t1.SUMSF12_categoriacal, 
          t1.DUPERSID, 
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
          t1.ADDAYA42_Health_Limits_Activity, 
          t1.ADCLIM42_Climbing_Stairs, 
          t1.ADPALS42_4Wks, 
          t1.ADPWLM42_WorkLimit, 
          t1.ADMALS42_Mental_Accomp, 
          t1.ADWLM42_WorkLimit__Mental, 
          t1.ADPAIN42_Pain_Limits_Work, 
          t1.ADCAPE42_Calm, 
          t1.ADNRGY42_LotEnergy, 
          t1.ADGENH42_health_general, 
          t1.ADDOWN42_Felt_Down, 
          t1.ADSOCA42_Health_Social_Activity, 
          t1.ADAPPT42_Num_MedOfficeVisits, 
          t1.ADILWW42_CareWhenNeeded, 
          t1.ADAPPT42_NumAppts, 
          t1.ADLIST42_Doctor_listens, 
          t1.ADHECR42_Qual_HealthC, 
          t1.ADSMOK42_currently_smoke, 
          t1.ADINSA42_Do_Not_Need_Insur, 
          t1.ADINSB42_HealthInsur, 
          t1.EDUCYR_Years_edu, 
          t1.EDUYRDEG_Yrs_Edu, 
          t1.MARRY12x_MaritalStatus, 
          t1.EMPST42_EmploymentStatus, 
          t1.EMPST31_Employ_Stat, 
          t1.EMPST53_Employ_Stat, 
          t1.PHQ242_Overall_Feelings, 
          t1.ADNDCR42_NeedCare12Mo, 
          t1.PMUNPR42_ProbUnableRx, 
          t1.PMUNAB42_unable_necRx, 
          t1.YNOINS31_Why_NoHC, 
          t1.YNOINS42_why_noHc, 
          t1.YNOINS53_Why_NoHC, 
          t1.HELD31x_HealthInsurHeld, 
          t1.HELD42x_HealthInsheld, 
          t1.HELD53X_heldInsur, 
          t1.RTHLTH31_healthStatus, 
          t1.RTHLTH42_healthStatus, 
          t1.RTHLTH53_healthStatus, 
          t1.MESBPR42_bloodPressure, 
          t1.ADGENH42_REV, 
          t1.ADPAIN42_Rev, 
          t1.ADCAPE42_Rev, 
          t1.ADNRGY42_Rev, 
          t1.Sum_SF12, 
          t1.ADEZUN42_EasilyUndertood, 
          t1.ADRESP42_DocRespect, 
          t1.ADEXPL_doctorExplained, 
          t1.Quality_Doc_SUM, 
          t1.Category_Doc_Quality, 
          /* INFULLYR */
            (1) LABEL="flag FULLYR " AS INFULLYR
      FROM EC100014.query_for_rev_recodedsubset_0004 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies   */
%LET _CLIENTTASKLABEL='One-Way Frequencies';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 3:42:20 PM
   By task: One-Way Frequencies

   Input Data: Local:MYDATA1.QUERY_FOR_FILTER_FOR_MERGE1
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT,
		WORK.OneWayFreqOfXRAY_RecodeInQUERY_F,
		WORK.OneWayFreqOfMRI_RecodeInQUERY_FO);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA1.QUERY_FOR_FILTER_FOR_MERGE1
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.XRAY_Recode, T.MRI_Recode
	FROM MYDATA1.QUERY_FOR_FILTER_FOR_MERGE1 as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES XRAY_Recode / 	OUT=WORK.OneWayFreqOfXRAY_RecodeInQUERY_F(LABEL="Cell statistics for XRAY_Recode analysis of MYDATA1.QUERY_FOR_FILTER_FOR_MERGE1")
 SCORES=TABLE;
	TABLES MRI_Recode / 	OUT=WORK.OneWayFreqOfMRI_RecodeInQUERY_FO(LABEL="Cell statistics for MRI_Recode analysis of MYDATA1.QUERY_FOR_FILTER_FOR_MERGE1")
 SCORES=TABLE;
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


/*   START OF NODE: Query Builder3   */
%LET _CLIENTTASKLABEL='Query Builder3';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.COUNT);

PROC SQL;
   CREATE TABLE WORK."COUNT"n AS 
   SELECT t1.DUPERSID, 
          /* COUNT_of_DUPERSID */
            (COUNT(t1.DUPERSID)) AS COUNT_of_DUPERSID
      FROM MYDATA1.QUERY_FOR_FILTER_FOR_MERGE1 t1
      GROUP BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder4   */
%LET _CLIENTTASKLABEL='Query Builder4';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA1.MergeWithCount);

PROC SQL;
   CREATE TABLE MYDATA1.MergeWithCount(label="MergeWithCount") AS 
   SELECT t1.XRAY_Recode, 
          t2.DUPERSID AS DUPERSID2, 
          t2.COUNT_of_DUPERSID, 
          t1.MRI_Recode, 
          t1.DUPERSID, 
          t1.INER, 
          t1.DUPERSID1, 
          t1.Categ_Education, 
          t1.Marry_Category, 
          t1.SUMSF12_categoriacal, 
          t1.AGE12X, 
          t1.MARRY12X, 
          t1.SEX, 
          t1.EDUCYR, 
          t1.EDUYRDEG, 
          t1.REGION12, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.DUID, 
          t1.PID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
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
          t1.ADDAYA42_Health_Limits_Activity, 
          t1.ADCLIM42_Climbing_Stairs, 
          t1.ADPALS42_4Wks, 
          t1.ADPWLM42_WorkLimit, 
          t1.ADMALS42_Mental_Accomp, 
          t1.ADWLM42_WorkLimit__Mental, 
          t1.ADPAIN42_Pain_Limits_Work, 
          t1.ADCAPE42_Calm, 
          t1.ADNRGY42_LotEnergy, 
          t1.ADGENH42_health_general, 
          t1.ADDOWN42_Felt_Down, 
          t1.ADSOCA42_Health_Social_Activity, 
          t1.ADAPPT42_Num_MedOfficeVisits, 
          t1.ADILWW42_CareWhenNeeded, 
          t1.ADAPPT42_NumAppts, 
          t1.ADLIST42_Doctor_listens, 
          t1.ADHECR42_Qual_HealthC, 
          t1.ADSMOK42_currently_smoke, 
          t1.ADINSA42_Do_Not_Need_Insur, 
          t1.ADINSB42_HealthInsur, 
          t1.EDUCYR_Years_edu, 
          t1.EDUYRDEG_Yrs_Edu, 
          t1.MARRY12x_MaritalStatus, 
          t1.EMPST42_EmploymentStatus, 
          t1.EMPST31_Employ_Stat, 
          t1.EMPST53_Employ_Stat, 
          t1.PHQ242_Overall_Feelings, 
          t1.ADNDCR42_NeedCare12Mo, 
          t1.PMUNPR42_ProbUnableRx, 
          t1.PMUNAB42_unable_necRx, 
          t1.YNOINS31_Why_NoHC, 
          t1.YNOINS42_why_noHc, 
          t1.YNOINS53_Why_NoHC, 
          t1.HELD31x_HealthInsurHeld, 
          t1.HELD42x_HealthInsheld, 
          t1.HELD53X_heldInsur, 
          t1.RTHLTH31_healthStatus, 
          t1.RTHLTH42_healthStatus, 
          t1.RTHLTH53_healthStatus, 
          t1.MESBPR42_bloodPressure, 
          t1.ADGENH42_REV, 
          t1.ADPAIN42_Rev, 
          t1.ADCAPE42_Rev, 
          t1.ADNRGY42_Rev, 
          t1.Sum_SF12, 
          t1.ADEZUN42_EasilyUndertood, 
          t1.ADRESP42_DocRespect, 
          t1.ADEXPL_doctorExplained, 
          t1.Quality_Doc_SUM, 
          t1.Category_Doc_Quality, 
          t1.INFULLYR
      FROM MYDATA1.QUERY_FOR_FILTER_FOR_MERGE1 t1
           FULL JOIN WORK.COUNT t2 ON (t1.DUPERSID = t2.DUPERSID);
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis   */
%LET _CLIENTTASKLABEL='Distribution Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 3:42:21 PM
   By task: Distribution Analysis

   Input Data: Local:MYDATA1.MERGEWITHCOUNT
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
ODS GRAPHICS ON;
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA1.MERGEWITHCOUNT
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.COUNT_of_DUPERSID
	FROM MYDATA1.MERGEWITHCOUNT as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: Count of DUPERSID";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Claire Dennis";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
	FREQ
;
	VAR COUNT_of_DUPERSID;
	HISTOGRAM   COUNT_of_DUPERSID / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;
ODS GRAPHICS OFF;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder5   */
%LET _CLIENTTASKLABEL='Query Builder5';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA1.Categorical_ERvisits_merged);

PROC SQL;
   CREATE TABLE MYDATA1.Categorical_ERvisits_merged(label="Categorical_ERvisits_merged") AS 
   SELECT t1.XRAY_Recode, 
          t1.DUPERSID2, 
          t1.COUNT_of_DUPERSID, 
          t1.MRI_Recode, 
          t1.DUPERSID, 
          t1.INER, 
          t1.DUPERSID1, 
          t1.Categ_Education, 
          t1.Marry_Category, 
          t1.SUMSF12_categoriacal, 
          t1.AGE12X, 
          t1.MARRY12X, 
          t1.SEX, 
          t1.EDUCYR, 
          t1.EDUYRDEG, 
          t1.REGION12, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.DUID, 
          t1.PID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
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
          t1.ADDAYA42_Health_Limits_Activity, 
          t1.ADCLIM42_Climbing_Stairs, 
          t1.ADPALS42_4Wks, 
          t1.ADPWLM42_WorkLimit, 
          t1.ADMALS42_Mental_Accomp, 
          t1.ADWLM42_WorkLimit__Mental, 
          t1.ADPAIN42_Pain_Limits_Work, 
          t1.ADCAPE42_Calm, 
          t1.ADNRGY42_LotEnergy, 
          t1.ADGENH42_health_general, 
          t1.ADDOWN42_Felt_Down, 
          t1.ADSOCA42_Health_Social_Activity, 
          t1.ADAPPT42_Num_MedOfficeVisits, 
          t1.ADILWW42_CareWhenNeeded, 
          t1.ADAPPT42_NumAppts, 
          t1.ADLIST42_Doctor_listens, 
          t1.ADHECR42_Qual_HealthC, 
          t1.ADSMOK42_currently_smoke, 
          t1.ADINSA42_Do_Not_Need_Insur, 
          t1.ADINSB42_HealthInsur, 
          t1.EDUCYR_Years_edu, 
          t1.EDUYRDEG_Yrs_Edu, 
          t1.MARRY12x_MaritalStatus, 
          t1.EMPST42_EmploymentStatus, 
          t1.EMPST31_Employ_Stat, 
          t1.EMPST53_Employ_Stat, 
          t1.PHQ242_Overall_Feelings, 
          t1.ADNDCR42_NeedCare12Mo, 
          t1.PMUNPR42_ProbUnableRx, 
          t1.PMUNAB42_unable_necRx, 
          t1.YNOINS31_Why_NoHC, 
          t1.YNOINS42_why_noHc, 
          t1.YNOINS53_Why_NoHC, 
          t1.HELD31x_HealthInsurHeld, 
          t1.HELD42x_HealthInsheld, 
          t1.HELD53X_heldInsur, 
          t1.RTHLTH31_healthStatus, 
          t1.RTHLTH42_healthStatus, 
          t1.RTHLTH53_healthStatus, 
          t1.MESBPR42_bloodPressure, 
          t1.ADGENH42_REV, 
          t1.ADPAIN42_Rev, 
          t1.ADCAPE42_Rev, 
          t1.ADNRGY42_Rev, 
          t1.Sum_SF12, 
          t1.ADEZUN42_EasilyUndertood, 
          t1.ADRESP42_DocRespect, 
          t1.ADEXPL_doctorExplained, 
          t1.Quality_Doc_SUM, 
          t1.Category_Doc_Quality, 
          t1.INFULLYR, 
          /* Category_Num_ER */
            (CASE  
               WHEN t1.COUNT_of_DUPERSID=1 
            THEN 1
            WHEN  t1.COUNT_of_DUPERSID=2
               THEN 2
               WHEN t1.COUNT_of_DUPERSID>2 and t1.COUNT_of_DUPERSID<=5
               THEN 3
            WHEN t1.COUNT_of_DUPERSID>5 and t1.COUNT_of_DUPERSID<=15
            THEN 4
               END) LABEL="Categorical Rep of # of ER Visits (DUPERSID count) " AS Category_Num_ER
      FROM MYDATA1.MERGEWITHCOUNT t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis   */
%LET _CLIENTTASKLABEL='Table Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 3:42:22 PM
   By task: Table Analysis

   Input Data: Local:MYDATA1.CATEGORICAL_ERVISITS_MERGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA1.CATEGORICAL_ERVISITS_MERGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Category_Num_ER, T.COUNT_of_DUPERSID
	FROM MYDATA1.CATEGORICAL_ERVISITS_MERGED as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Claire Dennis";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES Category_Num_ER * COUNT_of_DUPERSID /
		NOROW
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


/*   START OF NODE: One-Way Frequencies1   */
%LET _CLIENTTASKLABEL='One-Way Frequencies1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 3:42:23 PM
   By task: One-Way Frequencies1

   Input Data: Local:MYDATA1.CATEGORICAL_ERVISITS_MERGED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA1.CATEGORICAL_ERVISITS_MERGED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.Category_Num_ER
	FROM MYDATA1.CATEGORICAL_ERVISITS_MERGED as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Claire Dennis";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES Category_Num_ER /  SCORES=TABLE;
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


/*   START OF NODE: Query Builder1   */
LIBNAME EC100016 "P:\QAC\qac200\Data\MEPS";


%LET _CLIENTTASKLABEL='Query Builder1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_MEPS_ER_2012_SAS7BDAT);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_MEPS_ER_2012_SAS7BDAT AS 
   SELECT /* INER */
            (1) LABEL="flag for INER" AS INER, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU
      FROM EC100016.meps_er_2012 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder2   */
%LET _CLIENTTASKLABEL='Query Builder2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA1.MERGE1);

PROC SQL;
   CREATE TABLE MYDATA1.MERGE1(label="MERGE1") AS 
   SELECT t1.Categ_Education, 
          t1.Marry_Category, 
          t1.SUMSF12_categoriacal, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.MARRY12X, 
          t1.SEX, 
          t1.EDUCYR, 
          t1.EDUYRDEG, 
          t1.REGION12, 
          t1.EMPST31, 
          t1.EMPST42, 
          t2.INER, 
          t2.DUID, 
          t2.PID, 
          t2.DUPERSID AS DUPERSID1, 
          t2.EVNTIDX, 
          t2.EVENTRN, 
          t2.ERHEVIDX, 
          t2.FFEEIDX, 
          t2.PANEL, 
          t2.MPCDATA, 
          t2.ERDATEYR, 
          t2.ERDATEMM, 
          t2.ERDATEDD, 
          t2.SEEDOC, 
          t2.VSTCTGRY, 
          t2.VSTRELCN, 
          t2.LABTEST, 
          t2.SONOGRAM, 
          t2.XRAYS, 
          t2.MAMMOG, 
          t2.MRI, 
          t2.EKG, 
          t2.EEG, 
          t2.RCVVAC, 
          t2.ANESTH, 
          t2.THRTSWAB, 
          t2.OTHSVCE, 
          t2.SURGPROC, 
          t2.MEDPRESC, 
          t2.ERICD1X, 
          t2.ERICD2X, 
          t2.ERICD3X, 
          t2.ERPRO1X, 
          t2.ERCCC1X, 
          t2.ERCCC2X, 
          t2.ERCCC3X, 
          t2.FFERTYPE, 
          t2.FFBEF12, 
          t2.ERXP12X, 
          t2.ERTC12X, 
          t2.ERFSF12X, 
          t2.ERFMR12X, 
          t2.ERFMD12X, 
          t2.ERFPV12X, 
          t2.ERFVA12X, 
          t2.ERFTR12X, 
          t2.ERFOF12X, 
          t2.ERFSL12X, 
          t2.ERFWC12X, 
          t2.ERFOR12X, 
          t2.ERFOU12X, 
          t2.ERFOT12X, 
          t2.ERFXP12X, 
          t2.ERFTC12X, 
          t2.ERDSF12X, 
          t2.ERDMR12X, 
          t2.ERDMD12X, 
          t2.ERDPV12X, 
          t2.ERDVA12X, 
          t2.ERDTR12X, 
          t2.ERDOF12X, 
          t2.ERDSL12X, 
          t2.ERDWC12X, 
          t2.ERDOR12X, 
          t2.ERDOU12X, 
          t2.ERDOT12X, 
          t2.ERDXP12X, 
          t2.ERDTC12X, 
          t2.IMPFLAG, 
          t2.PERWT12F, 
          t2.VARSTR, 
          t2.VARPSU, 
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
          t1.ADDAYA42_Health_Limits_Activity, 
          t1.ADCLIM42_Climbing_Stairs, 
          t1.ADPALS42_4Wks, 
          t1.ADPWLM42_WorkLimit, 
          t1.ADMALS42_Mental_Accomp, 
          t1.ADWLM42_WorkLimit__Mental, 
          t1.ADPAIN42_Pain_Limits_Work, 
          t1.ADCAPE42_Calm, 
          t1.ADNRGY42_LotEnergy, 
          t1.ADGENH42_health_general, 
          t1.ADDOWN42_Felt_Down, 
          t1.ADSOCA42_Health_Social_Activity, 
          t1.ADAPPT42_Num_MedOfficeVisits, 
          t1.ADILWW42_CareWhenNeeded, 
          t1.ADAPPT42_NumAppts, 
          t1.ADLIST42_Doctor_listens, 
          t1.ADHECR42_Qual_HealthC, 
          t1.ADSMOK42_currently_smoke, 
          t1.ADINSA42_Do_Not_Need_Insur, 
          t1.ADINSB42_HealthInsur, 
          t1.EDUCYR_Years_edu, 
          t1.EDUYRDEG_Yrs_Edu, 
          t1.MARRY12x_MaritalStatus, 
          t1.EMPST42_EmploymentStatus, 
          t1.EMPST31_Employ_Stat, 
          t1.EMPST53_Employ_Stat, 
          t1.PHQ242_Overall_Feelings, 
          t1.ADNDCR42_NeedCare12Mo, 
          t1.PMUNPR42_ProbUnableRx, 
          t1.PMUNAB42_unable_necRx, 
          t1.YNOINS31_Why_NoHC, 
          t1.YNOINS42_why_noHc, 
          t1.YNOINS53_Why_NoHC, 
          t1.HELD31x_HealthInsurHeld, 
          t1.HELD42x_HealthInsheld, 
          t1.HELD53X_heldInsur, 
          t1.RTHLTH31_healthStatus, 
          t1.RTHLTH42_healthStatus, 
          t1.RTHLTH53_healthStatus, 
          t1.MESBPR42_bloodPressure, 
          t1.ADGENH42_REV, 
          t1.ADPAIN42_Rev, 
          t1.ADCAPE42_Rev, 
          t1.ADNRGY42_Rev, 
          t1.Sum_SF12, 
          t1.ADEZUN42_EasilyUndertood, 
          t1.ADRESP42_DocRespect, 
          t1.ADEXPL_doctorExplained, 
          t1.Quality_Doc_SUM, 
          t1.Category_Doc_Quality, 
          t1.INFULLYR
      FROM WORK.QUERY_FOR_REV_RECODEDSUBSET_0001 t1
           FULL JOIN WORK.QUERY_FOR_MEPS_ER_2012_SAS7BDAT t2 ON (t1.DUPERSID = t2.DUPERSID);
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Filter and Sort   */
%LET _CLIENTTASKLABEL='Filter and Sort';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA1.FILTER_FOR_MERGE1);

PROC SQL;
   CREATE TABLE MYDATA1.FILTER_FOR_MERGE1(label="FILTER_FOR_MERGE1") AS 
   SELECT t1.DUPERSID, 
          t1.INER, 
          t1.DUPERSID1, 
          t1.Categ_Education, 
          t1.Marry_Category, 
          t1.SUMSF12_categoriacal, 
          t1.AGE12X, 
          t1.MARRY12X, 
          t1.SEX, 
          t1.EDUCYR, 
          t1.EDUYRDEG, 
          t1.REGION12, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.DUID, 
          t1.PID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
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
          t1.ADDAYA42_Health_Limits_Activity, 
          t1.ADCLIM42_Climbing_Stairs, 
          t1.ADPALS42_4Wks, 
          t1.ADPWLM42_WorkLimit, 
          t1.ADMALS42_Mental_Accomp, 
          t1.ADWLM42_WorkLimit__Mental, 
          t1.ADPAIN42_Pain_Limits_Work, 
          t1.ADCAPE42_Calm, 
          t1.ADNRGY42_LotEnergy, 
          t1.ADGENH42_health_general, 
          t1.ADDOWN42_Felt_Down, 
          t1.ADSOCA42_Health_Social_Activity, 
          t1.ADAPPT42_Num_MedOfficeVisits, 
          t1.ADILWW42_CareWhenNeeded, 
          t1.ADAPPT42_NumAppts, 
          t1.ADLIST42_Doctor_listens, 
          t1.ADHECR42_Qual_HealthC, 
          t1.ADSMOK42_currently_smoke, 
          t1.ADINSA42_Do_Not_Need_Insur, 
          t1.ADINSB42_HealthInsur, 
          t1.EDUCYR_Years_edu, 
          t1.EDUYRDEG_Yrs_Edu, 
          t1.MARRY12x_MaritalStatus, 
          t1.EMPST42_EmploymentStatus, 
          t1.EMPST31_Employ_Stat, 
          t1.EMPST53_Employ_Stat, 
          t1.PHQ242_Overall_Feelings, 
          t1.ADNDCR42_NeedCare12Mo, 
          t1.PMUNPR42_ProbUnableRx, 
          t1.PMUNAB42_unable_necRx, 
          t1.YNOINS31_Why_NoHC, 
          t1.YNOINS42_why_noHc, 
          t1.YNOINS53_Why_NoHC, 
          t1.HELD31x_HealthInsurHeld, 
          t1.HELD42x_HealthInsheld, 
          t1.HELD53X_heldInsur, 
          t1.RTHLTH31_healthStatus, 
          t1.RTHLTH42_healthStatus, 
          t1.RTHLTH53_healthStatus, 
          t1.MESBPR42_bloodPressure, 
          t1.ADGENH42_REV, 
          t1.ADPAIN42_Rev, 
          t1.ADCAPE42_Rev, 
          t1.ADNRGY42_Rev, 
          t1.Sum_SF12, 
          t1.ADEZUN42_EasilyUndertood, 
          t1.ADRESP42_DocRespect, 
          t1.ADEXPL_doctorExplained, 
          t1.Quality_Doc_SUM, 
          t1.Category_Doc_Quality, 
          t1.INFULLYR
      FROM MYDATA1.MERGE1 t1
      WHERE t1.INER = 1 AND t1.INFULLYR = 1;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data   */
%LET _CLIENTTASKLABEL='List Data';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011315.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011315.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 3:42:25 PM
   By task: List Data

   Input Data: Local:MYDATA1.FILTER_FOR_MERGE1
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA1.FILTER_FOR_MERGE1
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.DUPERSID, T.DUPERSID1, T.INER
	FROM MYDATA1.FILTER_FOR_MERGE1 as T
;
QUIT;
TITLE;
TITLE1 "Report Listing";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=100)
	OBS="Row number"
	LABEL
	;
	VAR DUPERSID DUPERSID1 INER;
RUN;
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
