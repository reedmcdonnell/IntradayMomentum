options nosource nodate nocenter nonumber  ps=max ls=72;
%let wrds=wrds-cloud.wharton.upenn.edu 4016;
options comamid=TCP remote=WRDS;
signon username=_prompt_;

RSUBMIT;

/* STEP 1: RETRIEVE DAILY TRADE AND QUOTE (DTAQ) FILES */
libname ct '/wrds/nyse/sasdata/taqms/ct';

/* Retrieve Trade data */
data DailyTrade;
	/* Enter Trade file names in YYYYMMDD format for the same dates */
    set ct.ctm_201909;
	where (("9:00:00.0"t) <= time_m <= ("9:30:00.0"t)) or
	(("16:00:00.0"t) <= time_m <= ("16:30:00.0"t));
run;

