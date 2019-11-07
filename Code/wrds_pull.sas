options nosource nodate nocenter nonumber ps=max ls=72;
%let wrds=wrds-cloud.wharton.upenn.edu 4016;
options comamid=TCP remote=WRDS;
signon username=_prompt_;

RSUBMIT; /* Execute following on server after logging in */
/****** Input area **************************/

libname cq '/wrds/nyse/sasdata/taqms/ct'
%let taq_ds=taqmsec.ctm_20150105 ;     * data set you are interested (example for all daily files on December 1995);
%let start_time = '9:30:00't;    * starting time;
%let interval_seconds = 1;    * interval is 15*60 seconds (15 minutes);
 
/****** End of input area **********************/
 
 
/* Extract data for one day for 3 stocks, we consider the time
  between  9:30am to 4:30pm,  only retrieve SYMBOL DATE TIME and PRICE; */
data tempx;
     set &taq_ds(keep=symbol date time price);
     where symbol in ('IBM','SPY')
     and time between '9:30:00't and '16:30:00't;
     by symbol date time;
     retain itime rtime iprice; *Carry time and price values forward;
        format itime rtime time12.;
     if first.symbol=1 or first.date=1 then do;
        */Initialize time and price when new symbol or date starts;*/
        rtime=time;
        iprice=price;
        itime= &start_time;
     end;
     if time >= itime then do; /*Interval reached;*/
           output; /*rtime and iprice hold the last observation values;*/
           itime = itime + &interval_seconds;
           do while(time >= itime); /*need to fill in all time intervals;*/
               output;
               itime = itime + &interval_seconds;
           end;
    end;
    rtime=time;
    iprice=price;
    keep symbol date itime iprice rtime;
run;
 
Title "Final output -- &interval_seconds seconds";
proc print data=tempx (obs=400);
     var symbol date itime iprice rtime;
run;
