unit ItalianMultisitePas;

{
  Main ItalianMultisite var, constants and functions
}
uses
  StringUtils7552, ItalianSharedPas, MyMoviesPas, FilmTvPas, FilmScoopPas, KultVideoPas, ComingSoonPas, MoviePlayerPas, NientePopcornPas, ImdbPas;
  
const
  IM_UnitVersion = 4;

var
  IM_BestSource, IM_BestFieldTranslatedTitle, IM_BestFieldOriginalTitle, IM_BestFieldDirector, IM_BestFieldActors,
  IM_BestFieldCategory, IM_BestFieldCountry, IM_BestFieldYear, IM_BestFieldCertification, IM_BestFieldDescription,
  IM_BestFieldComments, IM_BestFieldLength, IM_BestFieldURL, IM_BestFieldRating: string;
  IM_OrigFieldTranslatedTitle, IM_OrigFieldOriginalTitle, IM_OrigFieldDirector,
  IM_OrigFieldActors, IM_OrigFieldCategory, IM_OrigFieldCountry, IM_OrigFieldYear,
  IM_OrigFieldCertification, IM_OrigFieldDescription, IM_OrigFieldComments, IM_OrigFieldLength,
  IM_OrigFieldURL, IM_OrigFieldRating: string;
  IM_GlobalScore: integer;
  IM_UserDisabledDescr: boolean;  
  
  IM_Sources: array of string;
  IM_ArrayIndex: integer;

// --------------------------------------------------------------------------------------------------------------------------------------
//                                               "public" functions e procedures
// --------------------------------------------------------------------------------------------------------------------------------------

procedure ItalianMultisiteMain;
var
  len: integer;
begin
  if not CheckVersion(4, 2, 3) then
  begin
    SharedShowMessage('Questo script richiede una versione aggiornata di Ant Movie Catalog (versione 4.2.3.3 o successiva)');
    exit;
  end;
  
  SharedInitCommon('', false);
  if GetParam('IMDB Attori numero') = '' then
    SetParam('IMDB Attori numero', '10');
  if GetParam('IMDB Localizzazione') = '' then
    SetParam('IMDB Localizzazione', 'Italy');
  if GetParam('IMDB Locandina pixel') = '' then
    SetParam('IMDB Locandina pixel', '720');
  if GetParam('IMDB Ricerca') = '' then
    SetParam('IMDB Ricerca', '1');
  if GetParam('Priorita'' ComingSoon') = '' then
    SetParam('Priorita'' ComingSoon', '5');
  if GetParam('Priorita'' FilmScoop') = '' then
    SetParam('Priorita'' FilmScoop', '3');
  if GetParam('Priorita'' FilmTV') = '' then
    SetParam('Priorita'' FilmTV', '8');
  if GetParam('Priorita'' KultVideo') = '' then
    SetParam('Priorita'' KultVideo', '2');
  if GetParam('Priorita'' MoviePlayer') = '' then
    SetParam('Priorita'' MoviePlayer', '6');
  if GetParam('Priorita'' MyMovies') = '' then
    SetParam('Priorita'' MyMovies', '7');
  if GetParam('Priorita'' NientePopcorn') = '' then
    SetParam('Priorita'' NientePopcorn', '1');    

  if (not ItalianMultisiteInitSitesOrder) AND (GetParam('IMDB Ricerca') = '0') then
  begin
    SharedShowMessage('Verifica la priorita'' dei siti, almeno un sito deve avere un valore di priorita'' compreso tra 1 e 9');
    exit;
  end;

  // test if user disabled Description, try adding a dot to description, if length does not increase the field is disabled by user configuration
  len := Length(GetField(fieldDescription));
  SharedSecureSetField(fieldDescription, GetField(fieldDescription) + '.');
  IM_UserDisabledDescr := (len = Length(GetField(fieldDescription)));
  SharedSecureSetField(fieldDescription, copy(GetField(fieldDescription), 1, Length(GetField(fieldDescription)) - 2));

  // ItalianMultisiteMain LOOP
  ItalianMultisiteInitBestFields();   
  ItalianMultisiteStoreOrigFields();  
  SharedRetrieveSearchElements();
  if (SharedGetForceExit()) then
    exit;
 
  for IM_ArrayIndex := 0 to (GetArrayLength(IM_Sources) - 1) do
    if (IM_Sources[IM_ArrayIndex] <> '') then
      if (ItalianMultisiteSearchInSite(IM_Sources[IM_ArrayIndex]) OR SharedGetForceExit) then
        exit;
       
  // Never found a valid description, best research result is restored (if any) and associated picture is taken
  ItalianMultisiteRestoreBestFields(); 
  // SharedGetSource = 'IMDb' if "Ricerca da URL" = 1 AND the url contains a valid imdb URL
  if (SharedGetSource() = 'IMDb') then
    SharedUnserializeMe(IMDbCoreMultiSite(SharedSerializeMe()))
  else
    if (GetParam('IMDB Ricerca') <> '0') then    
      ItalianMultisiteSiteEngine('IMDb');
  
  // no movie found, resetting
  if (SharedGetMoviesFound() = 0) then
    ItalianMultisiteRestoreOrigFields();
end;

// --------------------------------------------------------------------------------------------------------------------------------------
//                                               "private" functions e procedures
// --------------------------------------------------------------------------------------------------------------------------------------


// -------------------------------------------  
// INIT SITES ORDER, ORDER SOURCES BY PRIORITY
// -------------------------------------------
function ItalianMultisiteInitSitesOrder(): boolean;
var
  str: string;
  sitenames: array of string;
  priority: integer;

begin
  SharedRetrieveMovieUrl();
  result := (SharedGetSource() <> '');
  if result then
  begin
    if (SharedGetSource() = 'IMDb') then
      SetArrayLength(IM_Sources, 0)
    else
    begin      
      SetArrayLength(IM_Sources, 1);
      IM_Sources[0] := SharedGetSource();
    end;
    exit;    
  end;

  SetArrayLength(IM_Sources, 7);
  SetArrayLength(sitenames, 7);
  sitenames[0] := 'FilmTV';
  sitenames[1] := 'MyMovies';
  sitenames[2] := 'FilmScoop';
  sitenames[3] := 'ComingSoon';
  sitenames[4] := 'KultVideo';
  sitenames[5] := 'MoviePlayer';
  sitenames[6] := 'NientePopcorn';  

  str := '';
  for IM_ArrayIndex := 0 to GetArrayLength(sitenames) - 1 do
  begin
    if GetParam('Priorita'' ' + sitenames[IM_ArrayIndex]) = '' then
      SetParam('Priorita'' ' + sitenames[IM_ArrayIndex], IntToStr(IM_ArrayIndex + 1));
    priority := StrToInt(GetParam('Priorita'' ' + sitenames[IM_ArrayIndex]), 0);
    if (priority > 9) then
      priority := 9; // fix too high values, max is 9
    if (priority > 0) then // skip sites with priority 0
      str := str + IntToStr(10 - priority) + ',' + sitenames[IM_ArrayIndex] + crlf;
    // In ReorderList 1 > 9, so I use 10 - 1 to get my order priority
  end;
  if str = '' then
    exit;

  str := ReorderList(str, crlf) + crlf;
  IM_ArrayIndex := 0;
  repeat
    IM_Sources[IM_ArrayIndex] := TextBetween(str, ',', crlf);
    str := Stringreplace(str, ',' + IM_Sources[IM_ArrayIndex] + crlf, '');
    IM_ArrayIndex := IM_ArrayIndex + 1;
  until (pos(',', str) = 0);
  result := true;
end;



procedure ItalianMultisiteSiteEngine(xSource: string);
begin
  SharedInitCommon(xSource, false);
  case xSource of
    'FilmTV': SharedUnserializeMe(FilmTvCoreMultiSite(SharedSerializeMe()));
    'MyMovies': SharedUnserializeMe(MyMoviesCoreMultiSite(SharedSerializeMe()));
    'FilmScoop': SharedUnserializeMe(FilmScoopCoreMultiSite(SharedSerializeMe()));
    'ComingSoon': SharedUnserializeMe(ComingSoonCoreMultiSite(SharedSerializeMe()));
    'KultVideo': SharedUnserializeMe(KultVideoCoreMultiSite(SharedSerializeMe()));
    'MoviePlayer': SharedUnserializeMe(MoviePlayerCoreMultiSite(SharedSerializeMe()));
    'NientePopcorn': SharedUnserializeMe(NientePopcornCoreMultiSite(SharedSerializeMe()));    
    'IMDb': SharedUnserializeMe(IMDbCoreMultiSite(SharedSerializeMe()));
  end;
end;



function ItalianMultisiteSearchInSite(xSource: string): boolean;
begin
  result := false;
  // Searching on xsource
  ItalianMultisiteSiteEngine(xSource);
  if ((SharedGetMoviesFound() = 0) OR SharedGetForceExit()) then
  begin
    // no movie found, resetting
    ItalianMultisiteRestoreOrigFields();
    exit;
  end;
  
  if ((FullTrim(GetField(fieldDescription)) <> '') OR IM_UserDisabledDescr OR (GetOption('Trama italiana') = 0)) then
  begin  
    result := true;
    SharedGetPicture();    
    if GetParam('IMDB Ricerca') <> '0' then
      ItalianMultisiteSiteEngine('IMDb');
    SharedSetForceExit(true);
    exit; // Found a result with a description, job done
  end;
  ItalianMultisiteStoreResults(xSource);
  ItalianMultisiteRestoreOrigFields;
end;

// -----------------------
// STORE THE ORIGINAL FIELDS VALUE, BEFORE CHANGING THEM
// IN:  none
// -----------------------  
procedure ItalianMultisiteStoreOrigFields;
begin
  IM_GlobalScore := 0;
  IM_BestSource := '';
  IM_OrigFieldTranslatedTitle := getField(fieldTranslatedTitle);
  IM_OrigFieldOriginalTitle := getField(fieldOriginalTitle);
  IM_OrigFieldDirector := getField(fieldDirector);
  IM_OrigFieldActors := getField(fieldActors);
  IM_OrigFieldCategory := getField(fieldCategory);
  IM_OrigFieldCountry := getField(fieldCountry);
  IM_OrigFieldYear := getField(fieldYear);
  IM_OrigFieldCertification := getField(fieldCertification);
  IM_OrigFieldDescription := getField(fieldDescription);
  IM_OrigFieldComments := getField(fieldComments);
  IM_OrigFieldLength := getField(fieldLength);
  IM_OrigFieldURL := getField(fieldURL);
  IM_OrigFieldRating := getField(fieldRating);
end;

// -----------------------
// RESTORE THE ORIGINAL FIELDS VALUE (PREVIOUSLY STORED)
// IN:  none
// -----------------------  
procedure ItalianMultisiteRestoreOrigFields;
begin
  setField(fieldTranslatedTitle, IM_OrigFieldTranslatedTitle);
  setField(fieldOriginalTitle, IM_OrigFieldOriginalTitle);
  setField(fieldDirector, IM_OrigFieldDirector);
  setField(fieldActors, IM_OrigFieldActors);
  setField(fieldCategory, IM_OrigFieldCategory);
  setField(fieldCountry, IM_OrigFieldCountry);
  setField(fieldYear, IM_OrigFieldYear);
  setField(fieldCertification, IM_OrigFieldCertification);
  setField(fieldDescription, IM_OrigFieldDescription);
  setField(fieldComments, IM_OrigFieldComments);
  setField(fieldLength, IM_OrigFieldLength);
  setField(fieldURL, IM_OrigFieldURL);
  setField(fieldRating, IM_OrigFieldRating);
end;

// -----------------------
// RESTORE THE BEST FIELDS STORED (THE MOST DETAILED AND ACCURATE SEARCH RESULT)
// IN:  none
// -----------------------  
function ItalianMultisiteRestoreBestFields: string;
begin
  setField(fieldTranslatedTitle, IM_BestFieldTranslatedTitle);
  setField(fieldOriginalTitle, IM_BestFieldOriginalTitle);
  setField(fieldDirector, IM_BestFieldDirector);
  setField(fieldActors, IM_BestFieldActors);
  setField(fieldCategory, IM_BestFieldCategory);
  setField(fieldCountry, IM_BestFieldCountry);
  setField(fieldYear, IM_BestFieldYear);
  setField(fieldCertification, IM_BestFieldCertification);
  setField(fieldDescription, IM_BestFieldDescription);
  setField(fieldComments, IM_BestFieldComments);
  setField(fieldLength, IM_BestFieldLength);
  setField(fieldURL, IM_BestFieldURL);
  setField(fieldRating, IM_BestFieldRating);
  result := IM_BestSource;
end;


// -----------------------
// INIT THE BEST FIELDS STORED
// IN:  none
// -----------------------  
procedure ItalianMultisiteInitBestFields;
begin
  IM_BestFieldTranslatedTitle := '';
  IM_BestFieldOriginalTitle := '';
  IM_BestFieldDirector := '';
  IM_BestFieldActors := '';
  IM_BestFieldCategory := '';
  IM_BestFieldCountry := '';
  IM_BestFieldYear := '';
  IM_BestFieldCertification := '';
  IM_BestFieldDescription := '';
  IM_BestFieldComments := '';
  IM_BestFieldLength := '';
  IM_BestFieldURL := '';
  IM_BestFieldRating := '';
  IM_BestSource := '';
end;


// -----------------------
// MATCH THE RESULT SCORE WITH THE BEST SCORE AND, IF BETTER, THEN SAVE ALL FIELDS VALUES
// IN:  none
// -----------------------  
procedure ItalianMultisiteStoreResults(mySource: string);
var
  localScore: integer;
  localFieldTranslatedTitle, localFieldOriginalTitle, localFieldDirector, localFieldActors,
  localFieldCategory, localFieldCountry, localFieldYear, localFieldCertification, localFieldDescription,
  localFieldComments, localFieldLength, localFieldURL, localFieldRating: string;
begin
  localScore := 0;
  localFieldTranslatedTitle := getField(fieldTranslatedTitle);
  if localFieldTranslatedTitle <> '' then
    localScore := localScore + 1;
  localFieldOriginalTitle := getField(fieldOriginalTitle);
  if localFieldOriginalTitle <> '' then
    localScore := localScore + 1;
  localFieldDirector := getField(fieldDirector);
  if localFieldDirector <> '' then
    localScore := localScore + 1;
  localFieldActors := getField(fieldActors);
  if localFieldActors <> '' then
    localScore := localScore + 1;
  localFieldCategory := getField(fieldCategory);
  if localFieldCategory <> '' then
    localScore := localScore + 1;
  localFieldCountry := getField(fieldCountry);
  if localFieldCountry <> '' then
    localScore := localScore + 1;
  localFieldYear := getField(fieldYear);
  if localFieldYear <> '' then
    localScore := localScore + 1;
  localFieldCertification := getField(fieldCertification);
  if localFieldCertification <> '' then
    localScore := localScore + 1;
  localFieldDescription := getField(fieldDescription);
  if localFieldDescription <> '' then
    localScore := localScore + 100; // description is the most important field
  localFieldComments := getField(fieldComments);
  if localFieldComments <> '' then
    localScore := localScore + 1;
  localFieldLength := getField(fieldLength);
  localFieldURL := getField(fieldURL);
  localFieldRating := getField(fieldRating);
  if localFieldRating <> '' then
    localScore := localScore + 1;
  if localScore > IM_GlobalScore then
  begin
    IM_GlobalScore := localScore;
    IM_BestSource := mySource;
    IM_BestFieldTranslatedTitle := localFieldTranslatedTitle;
    IM_BestFieldOriginalTitle := localFieldOriginalTitle;
    IM_BestFieldDirector := localFieldDirector;
    IM_BestFieldActors := localFieldActors;
    IM_BestFieldCategory := localFieldCategory;
    IM_BestFieldCountry := localFieldCountry;
    IM_BestFieldYear := localFieldYear;
    IM_BestFieldCertification := localFieldCertification;
    IM_BestFieldComments := localFieldComments;
    IM_BestFieldLength := localFieldLength;
    IM_BestFieldURL := localFieldURL;
    IM_BestFieldRating := localFieldRating;
  end;
end;

end.