unit BatchCommon7552;
(***************************************************
 This file was created by scorpion7552
 
 And now, all you need for batch modes
               
look just after end. there's a skeleton showing you how to use these routines
1.0 initial release
1.1 batchFormatMovieName : keep single characters (i.e numbers)
***************************************************)

uses
	StringUtils7552;

var
	bestAddr, bestTxt, lookField2, lookmovie, looktxt: String;
	batchlogfic, batchCaller, batchLang, batchAbort, batchtyp, batchUrl: String;
	BatchMode, bestWeight, batchField2, maxweight: Integer;
	batchList, batchlog, confbatch, excludel: TstringList;
  BatchMsg: array of string;
  batchLookOK, batchconf: Boolean;

//------------------------------------------------------------------------------
// look for the best entry in batchList matching lookmovie/lookField2 
// returns bestWeight, bestAddr and bestTxt
//------------------------------------------------------------------------------
procedure LookBest;
var
	Address, field2, name, txt: string;
	filmnum, weight, weightm, weightd, i: integer;
	
begin
	for filmnum := 0 to batchList.Count -1 do
	begin
// parse address, movie_name and field2
    parseToken(batchList.GetString(filmnum), sepchar1);
    Address := token.GetString(0);                 // url
    name := token.GetString(1);                    // movie name
    field2 := token.GetString(2);                  // director, year or ''
    token.Free;
//
    txt := '"'+name+'/'+field2+'"';                // may be bestTxt
    if (field2 <> '') and (batchField2 = fieldDirector) then
  		field2 := batchFormatDirector(field2);   // format director (not other fields)
		name := batchFormatMovieName(name);        // and movie name like in initBatchLook
// weight of field2 if provided
    weightd :=0;	
    if batchField2 <> 99 then
    begin
      weightd := CompareWords(lookField2, field2);
// ignore if weightd=0 and the 2 fields are not empty (complete unmatch)
      if (weightd = 0) and (lookField2 <> '') and (field2 <> '') then continue;
    end;
   	weightm := CompareWords(lookmovie, name);      // movie_name weight
   	if weightm = 0 then continue;                  // ignore if no match
// priority 1) movie_name 2) field2
    weight := (weightm + weightd)*1000000 + weightm*1000 + weightd;
  	if weight > bestWeight then                            // current = best
 		begin                                                  
   		bestWeight := weight;
	  	bestAddr := Address;
		  bestTxt := txt;
   		if bestWeight = maxweight then break;            // exact match: we get it
	  end;
	end;    {for filmnum} 
	if bestWeight > 0 then                              // we have found something
	begin 
		if bestWeight < maxweight then            // don't log result if exact match 
			LogMessage(batchCaller+': >> '+looktxt+' '+BatchMsg[0]+' '+bestTxt+' ('+BatchMsg[1]+'='+IntToStr(bestWeight)+')');
	end else                                                    // nothing
			LogMessage(batchCaller+': '+BatchMsg[2]+' '+looktxt);  
end;

//------------------------------------------------------------------------------
// test if batch applicable for the current item
// initialize variables for batch mode 
// returns batchLookOK = True if correct
//------------------------------------------------------------------------------
procedure initBatchLook;

begin
  batchLookOK := False;                                             // init flag
	case BatchMode of
	1:
		begin
// search by url: if no url or another site then ignore
     	lookmovie := GetField(fieldUrl);     
	    if (lookmovie <> '') and (Pos(batchUrl, lookmovie) > 0) then
	     	batchLookOK := True
	    else
	     	LogMessage(batchCaller+BatchMsg[5]+lookmovie+'"');       
   	end;
	2:
		begin
// search by movie name + field2
      lookmovie := GetMovieName;                            // movie name
			if lookmovie <> '' then	
			begin
        batchLookOK := True;
        if batchField2 = 0 then batchField2 := fieldDirector;         // default
      	lookField2 := '';
        if batchField2 <> 99 then     
        	lookField2 := GetField(batchField2);            // field2 provided
      	looktxt := '"'+lookmovie+'/'+lookField2+'"';      // text for messages
      	if (lookField2 <> '') and (batchField2 = fieldDirector) then
  	      lookField2 := batchFormatDirector(lookField2);  // format director
        lookmovie := batchFormatMovieName(lookmovie);     // and movie_name
        bestWeight := 0;                                    // init best weight
      	bestAddr := '';                                     // init page address found
      	bestTxt := '';                                      // and corresponding text
// max weight
      	if batchField2 <> 99 then maxweight := 200100100      // 2 fields 
      	else maxweight := 100100000;                          // 1 field
			end else
				LogMessage(batchCaller+BatchMsg[6]);                  	// no movie name
		end;
	end;      {case}
end;

//------------------------------------------------------------------------------
// format director name (for batch comparison)
//------------------------------------------------------------------------------
function batchFormatDirector(str: string) :string;
begin
	str := CleanString(str);                         // lowercases without accents
// suppress 'and' to keep only names & forenames
// i.e for 'Joel and Ethan COEN' we just keep 'joel ethan coen'
	str := StringReplace(str, ' and ', ' ');
	str := StringReplace(str, ' und ', ' ');
	str := StringReplace(str, ' et ', ' ');
	str := StringReplace(str, ' y ', ' ');
	str := StringReplace(str, ' e ', ' ');
	str := StringReplace(str, ' & ', ' ');
	str := StringReplace(str, ' &amp; ', ' ');
	result := Trim(str);
end;

//------------------------------------------------------------------------------
// format movie name (for batch comparison)
//------------------------------------------------------------------------------
function batchFormatMovieName(str: string) :string;
var
  memo: TStringList;
  i: Integer;
  Value, newstr: String;
  
begin
	memo := TStringList.Create;
	str := CleanString(str);                         // lowercases without accents
// remove useless characters 
	str := StringReplace(str, '*', '');              // cf M*A*S*H*
	memo.Text := StringReplace(str, ' ', crlf);      // parse words
// remove all words contained in excludl (but keep single characters 1.1)
// because I don't want a match for just 1 article
  newstr := '';
  for i := 0 to memo.Count -1 do
  begin
    Value := memo.GetString(i);
    if excludel.IndexOfName(Value) <> -1 then continue; 
    newstr := newstr + ' ' + Value;
  end;           {for i}
  newstr := Trim(newstr);
  if newstr <> '' then str := newstr; // if all removed, we keep original string  
	result := str;
	memo.Free;
end;

//------------------------------------------------------------------------------
// display a warning message (normal mode) or add message to log (batch mode)
//------------------------------------------------------------------------------
procedure LogMessage(m: string);
begin
	if BatchMode > 0 then 
		AddToLog(BatchMsg[3]+' '+GetField(fieldNumber)+': '+m)
	else
		ShowWarning(m); 
end;

//------------------------------------------------------------------------------
// initialise batch log
// initBatchLog(Lang);
// Lang = 'FR' Français
//        'EN' or '' English 
//------------------------------------------------------------------------------
procedure initBatchLog(s: string);
var
  Value: String;
  
begin
// name of batch log file
  if batchCaller = '' then batchCaller := 'Unkown';  
  batchCaller := StringReplace(batchCaller, ' ', '_');         // no blanks here
  batchLang := AnsiUpperCase(s);                               // uppercases
	batchlogfic := 'c:\amc_'+batchCaller+'_batchlog.txt';  
// initializations
	case batchLang of
	'FR': initBatchFR;                 // en Français
	else initBatchEN;                  // in English (default) 
	end; 
// user must confirm his choice
	batchconf := ShowConfirmation(confbatch.Text);
	confbatch.Free;
	if not batchconf then
	begin
		batchlog.Free; 
		batchAbort := 'y';                                // flag batch mode aborted
		exit;
	end;
	batchlog.SaveToFile(batchlogfic);                   // save on disk right now
// initialize exclude list for batchFormatMovieName
  excludel := TStringList.Create;
// articles - note: don't remove 'die' (cf die hard)
  Value := 'le la les un une des de du the an der das dem den ein eine einen einer ';
  Value := Value+'eines einem uno una el los las unos unas il lo ';  
// and some commonly used words
  Value := Value + 'and et und y & &amp;';
	excludel.Text := StringReplace(Value+'=', ' ', '='+crlf);  // for IndexOfName
end;	

//------------------------------------------------------------------------------
// messages in English
//------------------------------------------------------------------------------
procedure initBatchEN;
begin
// init log file
  if BatchMode = 0 then batchtyp := 'batch mode (url)'
  else
  begin 
   	case batchField2 of
 	  99: batchtyp := 'batch mode (movie name)';
 	  fieldYear: batchtyp := 'batch mode (movie name + year)';
 	  else
 	    batchtyp := 'batch mode (movie name + director)';
 	  end;
 	end;
	batchlog := TStringList.Create;
 	batchlog.Add('starting '+batchtyp);
  if BatchMode = 2 then
  begin
  	batchlog.Add('weight = zzzxxxyyy with');
  	batchlog.Add('xxx weight of movie name, yyy weight of 2nd field (if any) and zzz summ of both');
    batchlog.Add('each weight = percentage of number of searched words/found');
    batchlog.Add('100 = exact match');
	end;
	batchlog.Add(StringOfChar('*',80));
// confirmation text
	confbatch := TStringList.Create;
	confbatch.Add('You have selected the '+batchtyp);
	confbatch.Add('Have you saved your database?');
	confbatch.Add('');
 	confbatch.Add('At the end of treatement:'); 
	confbatch.Add('- look at the file '+batchlogfic+' for errors/infos');
 	confbatch.Add('- the movies found will be checked, the others not (for the selection)');
	confbatch.Add(' (see: Tools/Preferences/Movie list/checkboxes)');
	confbatch.Add('');
	confbatch.Add('confirm your choice');	
// texts for messages
	SetArrayLength(BatchMsg,7);      
  BatchMsg[0]:='retained';
  BatchMsg[1]:='weight';
  BatchMsg[2]:='no match for';
  BatchMsg[3]:='item';
  BatchMsg[4]:='batch mode aborted';
  BatchMsg[5]:=': ignored url="';
  BatchMsg[6]:=': ignored (no movie name)';
end;	

//------------------------------------------------------------------------------
// messages en Français
//------------------------------------------------------------------------------
procedure initBatchFR;
begin
// initialisation du fichier log
// init log file
  if BatchMode = 0 then batchtyp := 'mode batch(url)'
  else
  begin 
   	case batchField2 of
 	  99: batchtyp := 'mode batch (nom du film)';
 	  fieldYear: batchtyp := 'mode batch (nom du film + année)';
 	  else
 	    batchtyp := 'mode batch (nom du film + réalisateur)';
 	  end;
 	end;
	batchlog := TStringList.Create;  
 	batchlog.Add('démarrage '+batchtyp);
  if BatchMode = 2 then
	begin
  	batchlog.Add('poids = zzzxxxyyy avec');
  	batchlog.Add('xxx poids du nom du film, yyy poids du 2ème champ (si disponible) et zzz somme des 2');
    batchlog.Add('chaque poids = pourcentage du nombre de mots cherchés/trouvés');
    batchlog.Add('100 = correspondance exacte');
	end;
	batchlog.Add(StringOfChar('*',80));
// message de confirmation
	confbatch := TStringList.Create;
	confbatch.Add('Vous avez sélectionné le '+batchtyp);
	confbatch.Add('Avez-vous sauvegardé votre base?');
	confbatch.Add('');
	confbatch.Add('En fin de traitement:'); 
	confbatch.Add('- consultez le fichier '+batchlogfic+' pour les erreurs/infos');
	confbatch.Add('- les films trouvés seront cochés, les autres non (pour la sélection)');
	confbatch.Add(' (voir: outils/préférences/liste des films/cases à cocher)');
	confbatch.Add(''); 
	confbatch.Add('confirmez votre choix');	
// textes pour messages
	SetArrayLength(BatchMsg,7);      
  BatchMsg[0]:='retenu';
  BatchMsg[1]:='poids';
  BatchMsg[2]:='pas de correspondance pour';
  BatchMsg[3]:='fiche';
  BatchMsg[4]:='mode batch annulé';
  BatchMsg[5]:=': url ignorée="';
  BatchMsg[6]:=': ignorée (pas de nom de film)';
end;	

//------------------------------------------------------------------------------
// add a message in the log and save the log to disk
// (because I don't know when it's finished...)
//------------------------------------------------------------------------------
procedure AddToLog(m: string);
begin
	batchlog.Add(m);
	batchlog.SaveToFile(batchlogfic);
end;

end.

{ here I put a skeleton to show you how to use these routines in your own scripts
It's based on the AllMovie script but only relevant parts are shown here.
 
Note that even if you use only mode = 0 or 1 (that is, not the movie/field2 batch mode)
you can use some of these routines

In the caller script, you must define some things (the declarations are here)
 
>> BatchMode: Integer; 0=normal mode, 1=batch mode(url), 2=batch mode (name+director or year or nothing) 
    it is defined in the options, but it's better to put it in a variable
    (probably faster and can be redefined in multi-sites scripts)

>> batchCaller: String;  ident of caller without blank (i.e site_name) 
    for batch log filename and messages (really useful for multi-sites scripts)
                    
>> batchUrl: String;   base Url of the site (for mode 1)
                    
>> batchField2: Integer  (for mode 2) ident of second field to test (you have always movie name, haven't you)
    depending on what is returned by the search engine
      = fieldDirector : second field = director(s) name(s)
        >>> Best choice if you have this field
        >>> this is the default if you don't initialize the variable
      = fieldYear: second field = year
        >>> if you have no director, this may be enough but be careful: the years must match exactly
            (cf public release date vs production date: sometimes, difference of 'one year')
      = 99 : no second field; very bad choice (cf duplicates like 'Batman', or remakes like 'the producers')
             but sometimes we have only movie name, so may be better than nothing...

>> batchList: TStringList;  list of movie_name/director_or_year returned by the search engine (without tags)
     each line = page_address(=url)+sepchar1+movie_name+sepchar1+director_or_year_or_nothing+sepchar1
     note that you can add whatever you want after the last sepchar1
     (look in StringUtils7552.pas for sepchar1 definition)
               
  you must first call initBatchlog('lang')
  with lang = 'FR' for French language 
              'EN' or '' for English
  (sorry, I don't speak other languages)  
note that if user doesn't confirm his choice, the variable batchAbort will be non blank  
}

(***************************************************

Ant Movie Catalog importation script
www.antp.be/software/moviecatalog/

[Infos]
Authors={your name}
Title={site name}
Description={some texte} / Normal/Batch mode © scorpion7552 : see Comments tab
Site={site url}
Language={language}
Version={yourversion}
Requires={amc version}
Comments=batch mode: 2 possible modes: from the memorized url ({your site} only) or from movie name + {director or year or nothing : choose} (results not guaranted!)|Don't forget to save your database before executing the batch mode, sort the titles by number and select a reasonable number of movies at a time|At the end of each round, a log file is created (informations and errors - beware: this file is re-create each time you run the batch mode)
License=This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
GetInfo=1             {mandatory for scripting mode}

[Options]
{... your own options}
Mode=0|0|0=normal mode|1=batch mode (url)|2=batch mode (name + director_or_year_or what_you_want,_I_don't_care)

***************************************************)

// needs the following units
// BatchCommon7552.pas (which includes StringUtils1.pas and StringUtils7552.pas)
//

program {your program name};
uses
	BatchCommon7552;

var
  MovieName: String;
  BatchMode: Integer;
  movieok: boolean;

//------------------------------------------------------------------------------
// the routine to get the movie list matching MovieName
//------------------------------------------------------------------------------
procedure GetList;   
var
	Address, PageNext, PagePrev, name: String;
	pagenum, i: Integer;
	found, postp: Boolean;
{note that the structure includes a pagenext/pageprev mechanism}
begin
	pagenum := 0;                                             // page counter
	batchList := TStringList.Create;                          // init batch list
  postp := True;                                     // 'postpage' indicator
// init address for 1st research 
	Address := {the 1st url}; 
	repeat
	PageNext := '';
	PagePrev := '';
	found := False;
// current page
	pagenum := pagenum + 1;
	PickTreeClear;                                                    // init list
	PickTreeAdd('{your text}', '');
// sometimes, 1st time it's a postpage [may be, it's depends on your site]
// after, it's always a getpage (pagenext/pageprev selected last time)
	if postp then      
  begin
    postp := False;
  	Page := PostPage(Address, [the search argument]);
  end 
  else
    Page := Getpage(Address);
{here you check the received page, as usual}
	if {this is not the good page} then
	begin
    LogMessage('Error while reading selection page');
		batchList.Free;
		exit;
	end;
// it's here where we should memorize PagePrev/PageNext
	if PagePrev <> '' then           
		PickTreeAdd('<<< previous page', PagePrev);
	for {the received lines}
	begin
{ 
 extract the movie list as usual: extract address (url), movie name and director or year
 when you have found a movie, then do the following
}
    if BatchMode = 0 then
  		PickTreeAdd(name+' ('+{director and/or year or what you want}+') ', Address)
		else
	   	batchList.Add(Address+sepchar1+name+sepchar1+{director or year or nothing}+sepchar1);
		found := True;
  end;                       {for nb lines}
 	if PageNext <> '' then           
		PickTreeAdd('>>> next page', PageNext);
	if not found then                       // nothing found for moviename
	begin
   	LogMessage('No movie found for "'+MovieName+'"');
		batchList.Free;
		exit;
	end;
	if BatchMode > 0 then
	begin               
// *** batch mode : look for best weight   
// note: if there is a page next, 2 solutions
// 1) work only with the actual list
// 2) continue to valorize batchList with the other pages (limit the number of pages i.e 3 max)
// in that skeleton, I choose opt 1)
		LookBest;
		if bestWeight > 0 then                            // we have found something
				AnalyzeMoviePage(bestAddr);                                 // page film	
		break;                                                              // leave
	end else
// *** normal mode
	begin                   
		if PickTreeExec(Address) then
		begin
			if (Address <> PageNext) and (Address <> PagePrev) then
			begin
				AnalyzeMoviePage(Address);                          // selected movie
				break;                                              // and it's finished
			end;   
			if Address = PagePrev then pagenum := pagenum -2;  // we are at least on page 2
		end else
			LogMessage('No movie selected'); 			
	end;
	until (Address = '');           {repeat}
	batchList.Free;
end;

//------------------------------------------------------------------------------
// ANALYZE MOVIE PAGE
//------------------------------------------------------------------------------
procedure AnalyzeMoviePage(Address: string);

begin
{
do as usual
when you are sure that this is the good page, then set
}
  movieok := True;

end;

{...... your routines ..........}

//------------------------------------------------------------------------------
// process batch mode
//------------------------------------------------------------------------------
procedure xxxBatch;
begin
	SetField(fieldChecked, '');                        // init movie in treatement
	initBatchLook;                                     // test and init 
	if batchLookOK then                                // batch mode accepted
	begin;
  	case BatchMode of
	  1: AnalyzeMoviePage(GetField(fieldUrl));                   // search by url
  	2: 
      begin;
      	MovieName := GetMovieName;
        GetList;                                       // search by name/director
      end;
  	end;      {case}
    if movieok then
		  SetField(fieldChecked, 'x');                    // movie ok = checked  
  end;     
// wait a little to not stress the site and so have time to cancel the script         
  Sleep(500);              
end;
	
//------------------------------------------------------------------------------
// process normal mode
//------------------------------------------------------------------------------
procedure xxxNorm;
begin
	MovieName := GetMovieName;    
	repeat
	if not Input('{site name} Import', 'Enter movie name', MovieName) or (MovieName = '') then exit;
	GetList;
	until movieok;
end;
	
//------------------------------------------------------------------------------
//  start here
//------------------------------------------------------------------------------                                               
begin
	if batchAbort <> '' then exit;                           // batch mode aborted
	if firstcall <> 'done' then
	begin                                             // 1st call: init parameters
		firstcall := 'done';
		if not CheckVersion(3,5,0) then              {check amc version}
		begin
{yes, it's ShowMessage that MUST be used...}
			ShowMessage('This script requires a newer version of Ant Movie Catalog (at least the version 3.5.0)');		
			batchAbort := 'y';
			exit;
		end;	
// get user's parms (used more than once)
		BatchMode := GetOption('Mode');         {mandatory}
//
		batchCaller := '{your site name}';         {mandatory}
		batchUrl := {base url of the site};        {mandatory for mode 1} 
{
here you can set batchField2 if you don't use the default (Director name)
  	batchField2 := fieldYear;       or batchField2 := 99;
}
//
		if BatchMode > 0 then                      // batch mode: confirm the choice
		begin
{
actually, there's only EN and FR
But if you want to add your own language, look at initBatchLog
and send me a copy of your routine so I can include it in the next release
}
			initBatchLog('EN');                                            // init log
			if batchAbort <> '' then exit;
		end;
	end;
// let's go
  movieok := False;
	if BatchMode = 0 then
		xxxNorm
	else
		xxxBatch; 
end.
