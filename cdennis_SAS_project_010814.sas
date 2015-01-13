/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Monday, January 12, 2015     TIME: 8:22:24 PM
PROJECT: cdennis_SAS_project_011214
PROJECT PATH: P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011214.egp
---------------------------------------- */

/* Library assignment for Local.DATA0112 */
Libname DATA0112 V9 'P:\QAC\qac200\students\cdennis' ;
/* Library assignment for Local.DATA0112 */
Libname DATA0112 V9 'P:\QAC\qac200\students\cdennis' ;


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

/*   START OF NODE: Assign Project Library (DATA0112)   */
%LET _CLIENTTASKLABEL='Assign Project Library (DATA0112)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011214.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011214.egp';

GOPTIONS ACCESSIBLE;
LIBNAME DATA0112  "P:\QAC\qac200\students\cdennis" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011214.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011214.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\students\cdennis";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:17:20 PM
   By task: Data Set Attributes

   Input Data: P:\QAC\qac200\students\cdennis\query_for_rev_recodedsubset.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsForquery_for_rev_rec);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=ECLIB000.query_for_rev_recodedsubset OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTContentsForquery_for_rev_rec(LABEL="Contents Details for query_for_rev_recodedsubset");
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
			typemem LABEL="Data Set Type" FROM WORK.CONTContentsForquery_for_rev_rec
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='QUERY_FOR_REV_RECODEDSUBSET';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTContentsForquery_for_rev_rec OUT=WORK.CONTContentsForquery_for_rev_rec;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTContentsForquery_for_rev_rec
		WHERE memname='QUERY_FOR_REV_RECODEDSUBSET';
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


/*   START OF NODE: Query Builder   */
LIBNAME EC100011 "P:\QAC\qac200\students\cdennis";


%LET _CLIENTTASKLABEL='Query Builder';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011214.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011214.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_REV_RECODEDSUBSET_SAS7);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_REV_RECODEDSUBSET_SAS7 AS 
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
          /* Sum_SF12 */
            
            (SUM(t1.ADGENH42_REV,t1.ADDAYA42_Health_Limits_Activity,t1.ADCLIM42_Climbing_Stairs,t1.ADPALS42_4Wks,t1.ADPWLM42_WorkLimit,t1.ADMALS42_Mental_Accomp,t1.ADWLM42_WorkLimit__Mental,t1.ADPAIN42_Rev,t1.ADCAPE42_Rev,t1.ADNRGY42_Rev,t1.ADDOWN42_Felt_Down,t1.ADSOCA42_Health_Social_Activity)) 
            LABEL="Sum of SF12 mental health questions " AS Sum_SF12
      FROM EC100011.query_for_rev_recodedsubset t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data   */
%LET _CLIENTTASKLABEL='List Data';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011214.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011214.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:17:20 PM
   By task: List Data

   Input Data: Local:WORK.QUERY_FOR_REV_RECODEDSUBSET_SAS7
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_REV_RECODEDSUBSET_SAS7
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Sum_SF12, T.ADNRGY42_Rev, T.ADDAYA42_Health_Limits_Activity, T.ADGENH42_REV, T.ADPALS42_4Wks, T.ADPWLM42_WorkLimit, T.ADMALS42_Mental_Accomp, T.ADCAPE42_Rev, T.ADPAIN42_Rev, T.ADSOCA42_Health_Social_Activity, T.ADDOWN42_Felt_Down
		     , T.ADCLIM42_Climbing_Stairs, T.ADWLM42_WorkLimit__Mental
	FROM WORK.QUERY_FOR_REV_RECODEDSUBSET_SAS7 as T
;
QUIT;
TITLE;
TITLE1 "Check Aggregate Variable Coding";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=100)
	OBS="Row number"
	LABEL
	;
	VAR Sum_SF12 ADNRGY42_Rev ADDAYA42_Health_Limits_Activity ADGENH42_REV ADPALS42_4Wks ADPWLM42_WorkLimit ADMALS42_Mental_Accomp ADCAPE42_Rev ADPAIN42_Rev ADSOCA42_Health_Social_Activity ADDOWN42_Felt_Down ADCLIM42_Climbing_Stairs
	  ADWLM42_WorkLimit__Mental;
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


/*   START OF NODE: Summary Statistics for Sum SF12   */
%LET _CLIENTTASKLABEL='Summary Statistics for Sum SF12';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011214.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011214.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:17:20 PM
   By task: Summary Statistics for Sum SF12

   Input Data: Local:WORK.QUERY_FOR_REV_RECODEDSUBSET_SAS7
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_REV_RECODEDSUBSET_SAS7
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Sum_SF12
	FROM WORK.QUERY_FOR_REV_RECODEDSUBSET_SAS7(FIRSTOBS=1 ) as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results for 2012 MEPS sum SF12";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Claire Dennis";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR Sum_SF12;

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


/*   START OF NODE: Distribution Analysis of Sum SF 12 Variable    */
%LET _CLIENTTASKLABEL='Distribution Analysis of Sum SF 12 Variable ';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011214.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011214.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:17:21 PM
   By task: Distribution Analysis of Sum SF 12 Variable 

   Input Data: Local:WORK.QUERY_FOR_REV_RECODEDSUBSET_SAS7
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
ODS GRAPHICS ON;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_REV_RECODEDSUBSET_SAS7
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Sum_SF12
	FROM WORK.QUERY_FOR_REV_RECODEDSUBSET_SAS7(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: Sum of SF 12 Variables";
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
;
	VAR Sum_SF12;
	HISTOGRAM   Sum_SF12 / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=CX333399 WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
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


/*   START OF NODE: Query Builder1   */
%LET _CLIENTTASKLABEL='Query Builder1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011214.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011214.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_REV_RECODEDSUBSET_0001);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_REV_RECODEDSUBSET_0001 AS 
   SELECT /* SUMSF12_categoriacal  */
            (CASE  
               WHEN t1.Sum_SF12>=2 and t1.Sum_SF12<41
               THEN 1
               WHEN t1.Sum_SF12>=41 and t1.Sum_SF12<48
               THEN 2
               WHEN t1.Sum_SF12>=48 and t1.Sum_SF12<52
               THEN 3  
               ELSE 4
            END) LABEL="Sum of SF12 based on quantiles " AS 'SUMSF12_categoriacal 'n, 
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
          t1.Sum_SF12
      FROM WORK.QUERY_FOR_REV_RECODEDSUBSET_SAS7 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies   */
%LET _CLIENTTASKLABEL='One-Way Frequencies';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011214.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011214.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:17:21 PM
   By task: One-Way Frequencies

   Input Data: Local:WORK.QUERY_FOR_REV_RECODEDSUBSET_0001
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_REV_RECODEDSUBSET_0001
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.Sum_SF12, T.SUMSF12_categoriacal
	FROM WORK.QUERY_FOR_REV_RECODEDSUBSET_0001 as T
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
	TABLES Sum_SF12 /  SCORES=TABLE;
	TABLES SUMSF12_categoriacal /  SCORES=TABLE;
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


/*   START OF NODE: Table Analysis   */
%LET _CLIENTTASKLABEL='Table Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011214.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011214.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:17:21 PM
   By task: Table Analysis

   Input Data: Local:WORK.QUERY_FOR_REV_RECODEDSUBSET_0001
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_REV_RECODEDSUBSET_0001
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Sum_SF12, T.SUMSF12_categoriacal
	FROM WORK.QUERY_FOR_REV_RECODEDSUBSET_0001 as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis Sum Categorical Check";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Claire Dennis";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES Sum_SF12 * SUMSF12_categoriacal /
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


/*   START OF NODE: Query Builder2   */
%LET _CLIENTTASKLABEL='Query Builder2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011214.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011214.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_REV_RECODEDSUBSET);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_REV_RECODEDSUBSET AS 
   SELECT t1.SUMSF12_categoriacal, 
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
          /* ADEZUN42_EasilyUndertood */
            (CASE 
               WHEN -1 = t1.ADEZUN42 THEN .
               WHEN -9 = t1.ADEZUN42 THEN .
               ELSE t1.ADEZUN42
            END) LABEL="SAQ doctor given instruction ez undstd (recoded missing) " AS ADEZUN42_EasilyUndertood, 
          /* ADRESP42_DocRespect */
            (CASE 
               WHEN -1 = t1.ADRESP42 THEN .
               WHEN -9 = t1.ADRESP42 THEN .
               ELSE t1.ADRESP42
            END) LABEL="SAQ doctor respects (recoded missing) " AS ADRESP42_DocRespect, 
          /* ADEXPL_doctorExplained */
            (CASE 
               WHEN -1 = t1.ADEXPL42 THEN .
               WHEN -9 = t1.ADEXPL42 THEN .
               ELSE t1.ADEXPL42
            END) LABEL="SAQ doc explained so understd (recoded missing) " AS ADEXPL_doctorExplained
      FROM WORK.QUERY_FOR_REV_RECODEDSUBSET_0001 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder3   */
%LET _CLIENTTASKLABEL='Query Builder3';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011214.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011214.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_REV_RECODEDSUBSET_0002);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_REV_RECODEDSUBSET_0002 AS 
   SELECT t1.SUMSF12_categoriacal, 
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
          /* Quality_Doc_SUM */
            
            (SUM(t1.ADEZUN42_EasilyUndertood,t1.ADRESP42_DocRespect,t1.ADEXPL_doctorExplained,t1.ADLIST42_Doctor_listens)) 
            LABEL="Sum of Doctor Quality Variables " AS Quality_Doc_SUM
      FROM WORK.QUERY_FOR_REV_RECODEDSUBSET t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: make permanent    */
%LET _CLIENTTASKLABEL='make permanent ';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011214.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011214.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(DATA0112.QUERY_FOR_REV_RECODEDSUBSET_0003);

PROC SQL;
   CREATE TABLE DATA0112.QUERY_FOR_REV_RECODEDSUBSET_0003(label="QUERY_FOR_REV_RECODEDSUBSET_0003") AS 
   SELECT t1.SUMSF12_categoriacal, 
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
          t1.Quality_Doc_SUM
      FROM WORK.QUERY_FOR_REV_RECODEDSUBSET_0002 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Summary Statistics   */
%LET _CLIENTTASKLABEL='Summary Statistics';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011214.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011214.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:17:22 PM
   By task: Summary Statistics

   Input Data: Local:DATA0112.QUERY_FOR_REV_RECODEDSUBSET_0003
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:DATA0112.QUERY_FOR_REV_RECODEDSUBSET_0003
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Quality_Doc_SUM
	FROM DATA0112.QUERY_FOR_REV_RECODEDSUBSET_0003(FIRSTOBS=1 ) as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics for Doctor Quality Aggregate Variable";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Claire Dennis";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR Quality_Doc_SUM;

RUN;
ODS GRAPHICS ON;
TITLE;
/*-----------------------------------------------------
 * Use PROC UNIVARIATE to generate the histograms.
 */

TITLE;
TITLE1 "Summary Statistics for Doctor Quality Aggregate Variable";
TITLE2 "Histograms";
PROC UNIVARIATE DATA=WORK.SORTTempTableSorted	NOPRINT	;
	VAR Quality_Doc_SUM;

			HISTOGRAM ;

RUN; QUIT;
ODS GRAPHICS OFF;
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


/*   START OF NODE: Cateogrical Doc Quality    */
%LET _CLIENTTASKLABEL='Cateogrical Doc Quality ';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011214.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011214.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_REV_RECODEDSUBSET_0003);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_REV_RECODEDSUBSET_0003 AS 
   SELECT t1.SUMSF12_categoriacal, 
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
          /* Category_Doc_Quality */
            (CASE  
               WHEN t1.Quality_Doc_SUM>=1 and t1.Quality_Doc_SUM<12
               THEN 1
            WHEN t1.Quality_Doc_SUM>=12 and t1.Quality_Doc_SUM<14
               THEN 2
            WHEN t1.Quality_Doc_SUM>=14 and t1.Quality_Doc_SUM<16
               THEN 3
               ELSE 4
            END) LABEL="Cateogrical Doctor Quality Communication (high value=high quality) " AS Category_Doc_Quality
      FROM DATA0112.QUERY_FOR_REV_RECODEDSUBSET_0003 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Doctor Quality   */
%LET _CLIENTTASKLABEL='Doctor Quality';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011214.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011214.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:17:22 PM
   By task: Doctor Quality

   Input Data: Local:WORK.QUERY_FOR_REV_RECODEDSUBSET_0003
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
ODS GRAPHICS ON;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_REV_RECODEDSUBSET_0003
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Category_Doc_Quality
	FROM WORK.QUERY_FOR_REV_RECODEDSUBSET_0003 as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: Aggregate Doctor Communication Quality";
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
;
	VAR Category_Doc_Quality;
	HISTOGRAM   Category_Doc_Quality / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=CX003366 WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
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


/*   START OF NODE: Marry12x , Education    */
%LET _CLIENTTASKLABEL='Marry12x , Education ';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011214.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011214.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:17:22 PM
   By task: Marry12x , Education 

   Input Data: Local:WORK.QUERY_FOR_REV_RECODEDSUBSET_0003
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
ODS GRAPHICS ON;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_REV_RECODEDSUBSET_0003
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MARRY12x_MaritalStatus, T.EDUCYR_Years_edu
	FROM WORK.QUERY_FOR_REV_RECODEDSUBSET_0003 as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis";
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
;
	VAR MARRY12x_MaritalStatus EDUCYR_Years_edu;
	HISTOGRAM   MARRY12x_MaritalStatus EDUCYR_Years_edu / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
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


/*   START OF NODE: Summary Statistics1   */
%LET _CLIENTTASKLABEL='Summary Statistics1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011214.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011214.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:17:23 PM
   By task: Summary Statistics1

   Input Data: Local:WORK.QUERY_FOR_REV_RECODEDSUBSET_0003
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_REV_RECODEDSUBSET_0003
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MARRY12x_MaritalStatus, T.EDUYRDEG_Yrs_Edu
	FROM WORK.QUERY_FOR_REV_RECODEDSUBSET_0003 as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Claire Dennis";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR MARRY12x_MaritalStatus EDUYRDEG_Yrs_Edu;

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


/*   START OF NODE: Query Builder4   */
%LET _CLIENTTASKLABEL='Query Builder4';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011214.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011214.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_REV_RECODEDSUBSET_0004);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_REV_RECODEDSUBSET_0004 AS 
   SELECT /* Categ_Education */
            (CASE  
               WHEN t1.EDUYRDEG_Yrs_Edu>=0 and t1.EDUYRDEG_Yrs_Edu<12
               THEN 1
            WHEN t1.EDUYRDEG_Yrs_Edu=12 
               THEN 2
            WHEN t1.EDUYRDEG_Yrs_Edu>12 and  t1.EDUYRDEG_Yrs_Edu<15
               THEN 3
               ELSE 4
            END) LABEL="Categorical Representation of Education Level " AS Categ_Education, 
          /* Marry_Category */
            (CASE  
               WHEN t1.MARRY12x_MaritalStatus=1
               THEN 1 
            WHEN t1.MARRY12x_MaritalStatus=2 or t1.MARRY12x_MaritalStatus=3 or t1.MARRY12x_MaritalStatus=4
               THEN 2
              ELSE 3
            END) LABEL="Categorization of Marry12x" AS Marry_Category, 
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
          t1.Category_Doc_Quality
      FROM WORK.QUERY_FOR_REV_RECODEDSUBSET_0003 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis1   */
%LET _CLIENTTASKLABEL='Table Analysis1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\cdennis\cdennis_SAS_project_011214.egp';
%LET _CLIENTPROJECTNAME='cdennis_SAS_project_011214.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 8:17:23 PM
   By task: Table Analysis1

   Input Data: Local:WORK.QUERY_FOR_REV_RECODEDSUBSET_0004
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_REV_RECODEDSUBSET_0004
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Categ_Education, T.Marry_Category, T.MARRY12x_MaritalStatus, T.EDUYRDEG_Yrs_Edu
	FROM WORK.QUERY_FOR_REV_RECODEDSUBSET_0004 as T
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
	TABLES EDUYRDEG_Yrs_Edu * Categ_Education /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES MARRY12x_MaritalStatus * Marry_Category /
		NOROW
		NOPERCENT
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
