unit ItalianSharedPas;

{
  Shared var, constants and functions for all italian units
}

uses
  StringUtils7552;

const
  SH_UnitVersion = 10;

  SH_DebugMode = false;
  SH_DebugFileFullPath = 'D:\antMovieCatalog.debug.txt';
  SH_TitDel = ' || ';

var
  SH_ForceExit, SH_StandAloneMode: boolean;
  SH_Source, SH_MovieUrl, SH_MovieName, SH_OriginalStr, SH_TranslatedStr, SH_DirectorName, SH_YearDate: string;
  SH_LatestPageHtml, SH_LatestPageUrl: string;
  SH_CoverUrl, SH_CoverReferral: string;  
  SH_LatestMovieUrlInPickTree: string;
  SH_OverwriteOriginalTitle: integer;
  SH_MoviesFound: integer;

  
// --------------------------------------------------------------------------------------------------------------------------------------
//                                               "public" functions e procedures
// --------------------------------------------------------------------------------------------------------------------------------------

function SharedGetDebugMode: boolean;
begin
  result := SH_DebugMode;
end;  
  
function SharedGetStandAloneMode: boolean;
begin
  result := SH_StandAloneMode;
end;

procedure SharedSetStandAloneMode(value: boolean);
begin
  SH_StandAloneMode := value;
end;
  
function SharedGetForceExit: boolean;
begin
  result := SH_ForceExit;
end;  

procedure SharedSetForceExit(value: boolean);
begin
  SH_ForceExit := value;
end; 

function SharedGetMoviesFound: integer;
begin
  result := SH_MoviesFound;
end;  

procedure SharedSetMoviesFound(value: integer);
begin
  SH_MoviesFound := value;
end; 
  
function SharedGetSource: string;
begin
  result := SH_Source;
end;

procedure SharedSetSource(value: string);
begin
  SH_Source := value;
end;

function SharedGetMovieUrl: string;
begin
  result := SH_MovieUrl;
end;

procedure SharedSetMovieUrl(value: string);
begin
  SH_MovieUrl := value;
end;

function SharedGetMovieName: string;
begin
  result := SH_MovieName;
end;

function SharedGetOriginalStr: string;
begin
  result := SH_OriginalStr;
end;

procedure SharedSetOriginalStr(title: string);
begin
  SH_OriginalStr := title;
end;

function SharedGetTranslatedStr: string;
begin
  result := SH_TranslatedStr;
end;

procedure SharedSetTranslatedStr(title: string);
begin
  SH_TranslatedStr := title;
end;

function SharedGetDirectorName: string;
begin
  result := SH_DirectorName;
end;

procedure SharedSetDirectorName(director: string);
begin
  SH_DirectorName := director;
end;

function SharedGetYearDate: string;
begin
  result := SH_YearDate;
end;

procedure SharedSetYearDate(yeardate: string);
begin
  SH_YearDate := yeardate;
end;

function SharedGetOverwriteOriginalTitle: integer;
begin
  result := SH_OverwriteOriginalTitle;
end;

function SharedGetLatestPageHtml: string;
begin
  result := SH_LatestPageHtml;
end;

function SharedGetLatestPageUrl: string;
begin
  result := SH_LatestPageUrl;
end;

// -----------------------
// Import image
// -----------------------
procedure SharedGetPicture;
begin
  if (CanSetPicture AND (SH_CoverUrl <> '')) then
  begin  
    SharedDebugAppendToFile(SH_CoverUrl, '[ItalianSharedPas][SharedGetPicture] Referral: ' + SH_CoverReferral);
    GetPicture2(SH_CoverUrl, SH_CoverReferral);
  end;  
end;

// -------------------------------
// Save Image info
// IN:  image url and url referral
// -------------------------------
procedure SharedSavePictureInfo(Url, Referral: string);
begin
  SH_CoverUrl := Url;
  SH_CoverReferral := Referral;
end;

// -------------------------------
// Show Message
// IN:  UTF8 valid string
// -------------------------------
procedure SharedShowMessage(utf8s: string);
begin
  showMessage(StringReplace(utf8s, '&', '&&'));
end;


function SharedSerializeMe: string;
begin
  // example of a serialized object : O:5:"myobj":3:{s:1:"b";b:0;s:1:"s";s:5:"prova";s:1:"i";i:5;}
  result := 'O:' + IntToStr(Length('ItalianSharedPas')) + ':"ItalianSharedPas":15:{';
  result := result + 's:' + IntToStr(Length('SH_ForceExit')) + ':"SH_ForceExit";b:';
  if (SH_ForceExit) then
    result := result + '1;'
  else
    result := result + '0;';
  result := result + 's:' + IntToStr(Length('SH_StandAloneMode')) + ':"SH_StandAloneMode";b:';
  if (SH_StandAloneMode) then
    result := result + '1;'
  else
    result := result + '0;';  
  result := result + 's:' + IntToStr(Length('SH_Source')) + ':"SH_Source";s:' + IntToStr(Length(SH_Source)) + ':"' + SH_Source + ';';
  result := result + 's:' + IntToStr(Length('SH_MovieUrl')) + ':"SH_MovieUrl";s:' + IntToStr(Length(SH_MovieUrl)) + ':"' + SH_MovieUrl + ';';
  result := result + 's:' + IntToStr(Length('SH_MovieName')) + ':"SH_MovieName";s:' + IntToStr(Length(SH_MovieName)) + ':"' + SH_MovieName + ';';
  result := result + 's:' + IntToStr(Length('SH_OriginalStr')) + ':"SH_OriginalStr";s:' + IntToStr(Length(SH_OriginalStr)) + ':"' + SH_OriginalStr + ';';
  result := result + 's:' + IntToStr(Length('SH_TranslatedStr')) + ':"SH_TranslatedStr";s:' + IntToStr(Length(SH_TranslatedStr)) + ':"' + SH_TranslatedStr + ';';
  result := result + 's:' + IntToStr(Length('SH_DirectorName')) + ':"SH_DirectorName";s:' + IntToStr(Length(SH_DirectorName)) + ':"' + SH_DirectorName + ';';
  result := result + 's:' + IntToStr(Length('SH_YearDate')) + ':"SH_YearDate";s:' + IntToStr(Length(SH_YearDate)) + ':"' + SH_YearDate + ';';
  result := result + 's:' + IntToStr(Length('SH_LatestPageHtml')) + ':"SH_LatestPageHtml";s:' + IntToStr(Length(SH_LatestPageHtml)) + ':"' + SH_LatestPageHtml + ';';
  result := result + 's:' + IntToStr(Length('SH_LatestPageUrl')) + ':"SH_LatestPageUrl";s:' + IntToStr(Length(SH_LatestPageUrl)) + ':"' + SH_LatestPageUrl + ';';
  result := result + 's:' + IntToStr(Length('SH_CoverUrl')) + ':"SH_CoverUrl";s:' + IntToStr(Length(SH_CoverUrl)) + ':"' + SH_CoverUrl + ';';
  result := result + 's:' + IntToStr(Length('SH_CoverReferral')) + ':"SH_CoverReferral";s:' + IntToStr(Length(SH_CoverReferral)) + ':"' + SH_CoverReferral + ';';
  result := result + 's:' + IntToStr(Length('SH_LatestMovieUrlInPickTree')) + ':"SH_LatestMovieUrlInPickTree";s:' + IntToStr(Length(SH_LatestMovieUrlInPickTree)) + ':"' + SH_LatestMovieUrlInPickTree + ';'; 
  result := result + 's:' + IntToStr(Length('SH_OverwriteOriginalTitle')) + ':"SH_OverwriteOriginalTitle";i:' + IntToStr(SH_OverwriteOriginalTitle) + ';';
  result := result + 's:' + IntToStr(Length('SH_MoviesFound')) + ':"SH_MoviesFound";i:' + IntToStr(SH_MoviesFound) + ';';
  result := result + '}'; 
end;

function SharedUnserializeMe(serializedObj: string): string;
var
  str, propertyName, propertyType, propertyValueStr: string;
  propertyValueBool: boolean;
  propertyValueInt, propertyValueStrLength: integer;
begin
  // example of a serialized object : O:5:"myobj":3:{s:1:"b";b:0;s:1:"s";s:5:"prova";s:1:"i";i:5;}
  str := serializedObj;
  Delete(str, 1, pos('{', str));
  while (pos('"', str) > 0) do
  begin
    // str = s:1:"b";b:0;s:1:"s";s:5:"prova";s:1:"i";i:5;
    propertyName := textBetween(str, '"', '"'); // b
    propertyType := textBetween(str, ';', ':'); // b
    Delete(str, 1, pos(';', str)); // b:0;s:1:"s";s:5:"prova";s:1:"i";i:5;
    case propertyType of
    'b':
      if (textBetween(str, ':', ';') = '0') then
        propertyValueBool := false
      else
        propertyValueBool := true;      
    'i':    
      propertyValueInt := StrToInt(textBetween(str, ':', ';'), 0);
    's':
      begin
        propertyValueStrLength := StrToInt(textBetween(str, ':', ':'), 0);
        Delete(str, 1, pos('"', str));
        propertyValueStr := copy(str, 1, propertyValueStrLength);
        Delete(str, 1, propertyValueStrLength);
      end;
    end;
    Delete(str, 1, pos(';', str));
    case propertyName of
    'SH_ForceExit': SH_ForceExit := propertyValueBool;
    'SH_StandAloneMode': SH_StandAloneMode := propertyValueBool;
    'SH_Source': SH_Source := propertyValueStr;
    'SH_MovieUrl': SH_MovieUrl := propertyValueStr;
    'SH_MovieName': SH_MovieName := propertyValueStr;
    'SH_OriginalStr': SH_OriginalStr := propertyValueStr;
    'SH_TranslatedStr': SH_TranslatedStr := propertyValueStr;
    'SH_DirectorName': SH_DirectorName := propertyValueStr;
    'SH_YearDate': SH_YearDate := propertyValueStr;
    'SH_LatestPageHtml': SH_LatestPageHtml := propertyValueStr;
    'SH_LatestPageUrl': SH_LatestPageUrl := propertyValueStr;
    'SH_CoverUrl': SH_CoverUrl := propertyValueStr;
    'SH_CoverReferral': SH_CoverReferral := propertyValueStr;
    'SH_LatestMovieUrlInPickTree': SH_LatestMovieUrlInPickTree := propertyValueStr;
    'SH_OverwriteOriginalTitle': SH_OverwriteOriginalTitle := propertyValueInt;
    'SH_MoviesFound': SH_MoviesFound := propertyValueInt;
    end;
  end;
end;


// -----------------------
// SET MOVIE FIELDS
// IN:  field name and field value
// OUT: set Ant field
// -----------------------
procedure SharedSecureSetField(fieldname: integer; fieldvalue: string);
begin
  if ((GetOption('Sovrascrivi campi') = 1) or (FullTrim(GetField(fieldname)) = '')) then
    SetField(fieldname, fieldvalue);
end;


// -----------------------
// SET ALL MOVIE FIELDS
// IN:  all fields values
// OUT: set all Ant fields
// -----------------------
procedure SharedSecureSetAllFields(originalTitle, translatedTitle, rating, year, producers, writers, composers, directors, actors, duration, categories, certification, countries, description, comments, url, picture: string);
begin
  SharedSecureSetField(fieldDate, DateToStr(Date));
  if (originalTitle = '') then
    originalTitle := translatedTitle;  
  SharedSecureSetField(fieldTranslatedTitle, translatedTitle);
  if (SharedGetOverwriteOriginalTitle() = 0) then
    // inserimento/aggiornamento "sicuro" 
    SharedSecureSetField(fieldOriginalTitle, originalTitle)
  else
    // forzo l'aggiornamento perche' il titolo originale l'ho usato e poi modificato in fase di input, presumibilmente contiene qualche carattere "sporco"
    SetField(fieldOriginalTitle, originalTitle);
  SharedSecureSetField(fieldDescription, description);
  if (comments = description) then
    SharedSecureSetField(fieldComments, '')
  else  
    SharedSecureSetField(fieldComments, comments);
  SharedSecureSetField(fieldRating, rating);
  SharedSecureSetField(fieldCertification, certification);
  SharedSecureSetField(fieldCountry, countries);
  SharedSecureSetField(fieldYear, year);
  SharedSecureSetField(fieldCategory, categories);
  // evita di sovrascrivere la durata se già impostata
  if ((duration <> '') AND (getField(fieldLength) = '')) then 
    SharedSecureSetField(fieldLength, duration);
  SharedSecureSetField(FieldProducer, producers);
  SharedSecureSetField(FieldWriter, writers);
  SharedSecureSetField(FieldComposer, composers);
  SharedSecureSetField(FieldDirector, directors);
  SharedSecureSetField(fieldActors, actors);
  SharedSecureSetField(fieldURL, url);
  if picture <> '' then
  begin
    SharedSavePictureInfo(picture, url);
    if (SharedGetStandAloneMode()) then
      SharedGetPicture();
  end;
end;


procedure SharedPickTreeCreate;
var ExtendedTitle: string;
begin
  SharedSetMoviesFound(0);
  SH_LatestMovieUrlInPickTree := '';
  ExtendedTitle := SH_Source + ' - Risultati ricerca per "' + SH_MovieName;
  if ((SH_YearDate <> '') AND (SH_DirectorName <> '')) then
  begin
    if (SH_DirectorName <> '') AND (SH_YearDate <> '') then
      ExtendedTitle := ExtendedTitle + ' (' + SH_DirectorName + ', ' + SH_YearDate + ')'
    else
    if (SH_DirectorName <> '') then
      ExtendedTitle := ExtendedTitle + ' (' + SH_DirectorName + ')'
    else
    if (SH_YearDate <> '') then
      ExtendedTitle := ExtendedTitle + ' (' + SH_YearDate + ')';
  end;
  PickTreeClear;
  PickTreeAdd(SH_Source + ' - Risultati ricerca per "' + ExtendedTitle + '":', '');
  if ((SH_StandAloneMode) or (SharedGetSource() = 'IMDb')) then
    PickTreeAdd('>> interrompi la ricerca di questo titolo in questo sito --->>', 'about:blank')  
  else
  if (SharedGetSource() <> 'IMDb') then
    PickTreeAdd('>> continua la ricerca su un altro sito --->>', 'about:blank');
end;


procedure SharedPickTreeAdd(filmTranslatedTitle, filmOriginalTitle, filmDirectors, filmYear, filmUrl: string);
var
  ExtendedTitle: string;
begin
  if ((GetOption('Ricerca con Regista e Anno') = 1) AND (filmYear <> '') AND (SH_YearDate <> '')) then
    if ((StrToInt(filmYear, 0) < (StrToInt(SH_YearDate, 0) - 5)) OR (StrToInt(filmYear, 0) > (StrToInt(SH_YearDate, 0) + 5))) then
      exit;
  if (filmTranslatedTitle = '') AND (filmOriginalTitle = '') then
    exit;

  if (filmTranslatedTitle <> '') AND (filmOriginalTitle <> '') then
    ExtendedTitle := filmOriginalTitle + ' - ' + filmTranslatedTitle
  else
  if (filmTranslatedTitle <> '') then
    ExtendedTitle := filmTranslatedTitle
  else
    ExtendedTitle := filmOriginalTitle;
  
  if (filmDirectors <> '') AND (filmYear <> '') then
    ExtendedTitle := ExtendedTitle + ' (' + filmDirectors + ', ' + filmYear + ')'
  else
  if (filmDirectors <> '') then
    ExtendedTitle := ExtendedTitle + ' (' + filmDirectors + ')'
  else
  if (filmYear <> '') then
    ExtendedTitle := ExtendedTitle + ' (' + filmYear + ')';    

  SharedSetMoviesFound(SharedGetMoviesFound() + 1);
  SH_LatestMovieUrlInPickTree := filmUrl;
  PickTreeAdd(ExtendedTitle, filmUrl);
end;

procedure SharedPickTreeExec();
begin
  if (SharedGetMoviesFound() > 0) AND (SharedGetMovieUrl() = '') then
  begin
    SharedSetForceExit(not PickTreeExec(SH_MovieUrl));
    SH_MovieUrl := URLEncode(SharedUTF8ToCP1252Decode(SH_MovieUrl));
    if ((SharedGetForceExit()) or (SharedGetMovieUrl() = 'about:blank') or (copy(SharedGetMovieUrl(), 1, 5) = 'next:')) then 
      SharedSetMoviesFound(0);
  end;
end;


// restituisce true se la url contiene un codice imdb id (ovvero tt1234567) presente o nel campo file path o nel campo url
function SharedPickTreeImdbIdMatching(filmURL: string): boolean;
var 
  ttImdbid: string;
begin
  result := false;
  if (filmURL <> '') then
  begin
    ttImdbid := textBetween((filmURL + '/'), '/tt', '/');
    if (ttImdbid <> '') then
      result := (pos('tt' + ttImdbid, AnsiLowerCase(GetField(fieldUrl) + GetField(fieldFilePath))) > 0);
  end;
end;  

// c'è matching se almeno uno tra titolo originale e tradotto corrisponde esattamente
// E se il nome del regista somiglia al 95% o più
// E se la distanza tra l'anno trovato e quello voluto non è superiore a +/-1 anno
// Il matching viene cercato solo se l'opzione Ricerca con Regista e Anno è pari a 1 oppure se sto cercando in IMDB
function SharedPickTreeMatching(filmOriginalTitle, filmTranslatedTitle, filmYear, filmDirectors, filmURL: string): boolean;
var
  title1Input, title2Input, title1Shared, title2Shared, directors1, directors2: string;
begin
  title1Shared := SharedSimplifyString(SharedGetOriginalStr(), false);
  title2Shared := SharedSimplifyString(SharedGetTranslatedStr(), false);
  if ((title1Shared = '') OR (title2Shared = '')) then
  begin
  // questo si verifica, ad esempio, quando il nome nel campo titolo tradotto è quello importato dal filename, esempio: Il tempo delle api (Anitori, 2017)---webrip.audio ita
    title1Shared := SharedSimplifyString(SharedGetMovieName(), false);
    title2Shared := title1Shared;
  end;
  
  title1Input := SharedSimplifyString(filmOriginalTitle, false);
  title2Input := SharedSimplifyString(filmTranslatedTitle, false);  
  if (title2Input = '') then
    title2Input := title1Input
  else
  if (title1Input = '') then
    title1Input := title2Input;  

  // confronta titolo, anno e regista. Il confronto sul titolo si rende necessario perché talvolta lo stesso regista, nello stesso anno o l'anno dopo, gira un sequel
  directors1 := SharedSimplifyString(fulltrim(SH_DirectorName), true);
  directors2 := SharedSimplifyString(fulltrim(filmDirectors), true);
  result := (((GetOption('Ricerca con Regista e Anno') = 1) or (SH_Source = 'IMDb')) and (SH_DirectorName <> '') AND (filmDirectors <> '') and (filmYear <> '') and
             ((title1Shared = title1Input) or (title2Shared = title2Input) or (title1Shared = title2Input) or (title2Shared = title1Input)) and
             ((filmYear = SH_YearDate) or (filmYear = IntToStr(StrToInt(SH_YearDate, 0) + 1)) or (filmYear = IntToStr(StrToInt(SH_YearDate, 0) - 1))) and
             (((CompareWords(directors1, directors2) >= 80) or (CompareWords(directors2, directors1) >= 80) or (CompareWords(SharedSplitStringInCharacter(directors2, ' '), SharedSplitStringInCharacter(directors1, ' ')) >= 80))));
  if ((not result) and (SH_Source = 'IMDb')) then
    result := SharedPickTreeImdbIdMatching(filmURL);
  if (result) then
  begin
    PickTreeClear;
    SharedSetMoviesFound(1);  
  end;
end;


procedure SharedPickTreeClose(paramNextPageAvailable: boolean);
begin
  if (paramNextPageAvailable) then
    PickTreeAdd('>> continua la ricerca su questo sito --->>', 'next:')   
  else
  if (SharedGetStandAloneMode() AND (SharedGetMoviesFound() = 1) AND (SharedGetMovieUrl() = '')) then
    SharedSetMovieUrl(SH_LatestMovieUrlInPickTree); 
end;


procedure SharedHTTPPostPage(paramUrl, paramData: string);
begin
  SharedHTTPGetPostPage(paramUrl, 'POST', 'UTF8', paramData, '', '', '', '');
end;


procedure SharedHTTPGetPage(paramUrl: string);
begin
  SharedHTTPGetPostPage(paramUrl, 'GET', 'UTF8', '', '', '', '', '');
end;

procedure SharedHTTPGetPostPage(paramUrl, paramRequestMethod, paramOriginalCharset, paramData, paramReferer, paramCookie, paramContentTypeAccepted, paramHeaders: string);
begin
  SH_LatestPageUrl := paramUrl;
  if (paramUrl = '') then
    SH_LatestPageHtml := ''
  else
  begin
    RaiseConnectionErrors(false);
    if (paramRequestMethod = 'GET') then
      SH_LatestPageHtml := GetPage5(paramUrl, paramReferer, paramCookie, paramContentTypeAccepted, paramHeaders)
    else
      SH_LatestPageHtml := PostPage(paramUrl, paramData);
    RaiseConnectionErrors(true);       
    if (paramOriginalCharset = 'UTF8') then
      SH_LatestPageHtml := SharedUTF8ToCP1252Decode(SH_LatestPageHtml)
    else
    if (paramOriginalCharset = 'Unknown') then
      SH_LatestPageHtml := SharedUnknownToCP1252Decode(SH_LatestPageHtml);
  end;  
  SharedDebugAppendToFile(SH_LatestPageHtml, '[ItalianSharedPas][SharedHTTP' + paramRequestMethod + 'Page] ' + paramUrl + '   ' + paramData);
end;


// -----------------------------------
// APPEND TEXT COMMENTED TO DEBUG FILE
// IN: content text and comment text
// -----------------------------------
procedure SharedDebugAppendToFile(content, comment: string);
var
	page: TStringList;
begin
  if not SH_DebugMode then // debug
    exit;
  page := TStringList.Create;
  if (FileExists(SH_DebugFileFullPath)) then
    page.LoadFromFile(SH_DebugFileFullPath);
  page.Text := page.Text + '--- [' + SH_Source + ']' + comment + ' START -----------------------------------------' + crlf + content + crlf + '--- [' + SH_Source + ']' + comment + ' END -------------------------------------------' + crlf;
  page.SaveToFile(SH_DebugFileFullPath);
  page.Free;  
end;



// -----------------------------------
// RETRIEVE THE URL OF MOVIE TO SEARCH
// IN:  none
// -----------------------------------  
procedure SharedRetrieveMovieUrl;
var
  domain, mysource, myUrl: string;
begin
  SH_MovieUrl := '';
  SH_MovieName := '';
  SH_TranslatedStr := '';
  SH_OriginalStr := '';
  SH_OverwriteOriginalTitle := 1;
  SH_DirectorName := '';
  SH_YearDate := '';  
  mysource := '';
  if (getOption('Ricerca da URL') = 1) then
  begin
    domain := AnsiLowerCase(textBetween(getField(FieldUrl), '://', '/'));     
    case domain of
    'www.comingsoon.it', 'comingsoon.it': if (GetParam('Priorita'' ComingSoon') <> '0') then mysource := 'ComingSoon';
    'www.film.tv.it', 'film.tv.it': if (GetParam('Priorita'' FilmTV') <> '0') then mysource := 'FilmTV';
    'www.filmscoop.it', 'filmscoop.it': if (GetParam('Priorita'' FilmScoop') <> '0') then mysource := 'FilmScoop'; 
    'www.kultvideo.com', 'kultvideo.com': if (GetParam('Priorita'' KultVideo') <> '0') then mysource := 'KultVideo';
    'www.mymovies.it', 'mymovies.it': if (GetParam('Priorita'' MyMovies') <> '0')then mysource := 'MyMovies';
    'www.movieplayer.it', 'movieplayer.it': if (GetParam('Priorita'' MoviePlayer') <> '0') then mysource := 'MoviePlayer';
    'www.nientepopcorn.it', 'nientepopcorn.it': if (GetParam('Priorita'' NientePopcorn') <> '0') then mysource := 'NientePopcorn';
    'www.imdb.com', 'imdb.com': if ((GetParam('IMDB Ricerca') <> '0') AND RegExprSetExec('tt[0-9]+', getField(FieldUrl))) then mysource := 'IMDb';
    end;
    if ((mysource <> '') AND ((SharedGetSource() = '') OR (SharedGetSource() = mysource))) then // if it's ItalianMultisite script or a StandAlone script ...
    begin
      myUrl := fulltrim(getField(FieldUrl));
      if (mysource = 'IMDb') then
      begin
        myUrl := StringReplace(myUrl, 'http:', 'https:');
        if (copy(myUrl, Length(myUrl), 1) <> '/') then
          myUrl := myUrl + '/';
        myUrl := myUrl + 'reference';         
      end
      else if (mysource = 'FilmTV') then
        myUrl := StringReplace(myUrl, 'http://www.film.tv.it/scheda.php/', 'https://www.filmtv.it/');
      SharedSetMovieUrl(myUrl);
      SharedSetMoviesFound(1);
      SharedSetSource(mysource);
    end;
  end;
end;

// ------------------------------------
// RETRIEVE THE NAME OF MOVIE TO SEARCH
// IN:  none
// ------------------------------------  
procedure SharedRetrieveMovieName;
var
  parenthesisOpenedPos, parenthesisClosedPos, commaPos: integer;
  tmpStr: string;
begin
  SH_TranslatedStr := FullTrim(GetField(fieldTranslatedTitle));
  if (pos(SH_TitDel, SH_TranslatedStr) > 0) then
    SH_TranslatedStr := copy(SH_TranslatedStr, 1, pos(SH_TitDel, SH_TranslatedStr) - 1);  
  SH_OriginalStr := FullTrim(GetField(fieldOriginalTitle));
  SH_OverwriteOriginalTitle := 1;
  // if the only source is iMDB then use original title instead of translated one
  if ((GetParam('Priorita'' ComingSoon') = '0') AND (GetParam('Priorita'' FilmScoop') = '0') AND (GetParam('Priorita'' FilmTV') = '0') AND (GetParam('Priorita'' KultVideo') = '0') AND (GetParam('Priorita'' MoviePlayer') = '0') AND (GetParam('Priorita'' MyMovies') = '0') AND (GetParam('Priorita'' NientePopcorn') = '0') AND (GetParam('IMDB Ricerca') <> '0') AND (SH_OriginalStr <> '')) then
    SH_MovieName := SH_OriginalStr
  else
  begin  
    if (SH_TranslatedStr <> '') then
      SH_MovieName := SH_TranslatedStr
    else
    begin
      SH_MovieName := SH_OriginalStr;
      if (SH_OriginalStr <> '') then
        SH_OverwriteOriginalTitle := 0;		  
    end
  end;  
  
  SH_MovieName := StringReplace(SH_MovieName, '’', '''');

  // Retrieve director name and year from title. Accepted title example "nome film (director, year) other text". Example: "Junkyard dog (Bass, 2010) dvdrip ita ac3 eng aac"
  parenthesisOpenedPos := pos('(', SH_MovieName);
  if (parenthesisOpenedPos > 0) then
  begin
    commaPos := pos(',', TextAfter(SH_MovieName, '(')) + parenthesisOpenedPos;
    parenthesisClosedPos := pos(')', TextAfter(SH_MovieName, '(')) + parenthesisOpenedPos;
  end  
  else
  begin	  
    commaPos := 0;
    parenthesisClosedPos := 0;		  
  end;  
  SH_DirectorName := '';
  SH_YearDate := '';
  if (GetOption('Ricerca con Regista e Anno') = 1) then
  begin
    if ((parenthesisOpenedPos <> 0) AND (commaPos > parenthesisOpenedPos) AND (parenthesisClosedPos <> commaPos)) then
    begin
      SH_DirectorName := FullTrim(copy(SH_MovieName, parenthesisOpenedPos + 1, commaPos - parenthesisOpenedPos - 1)); 
      SH_YearDate := FullTrim(copy(SH_MovieName, commaPos + 1, parenthesisClosedPos - commaPos - 1));
      if (IntToStr(StrToInt(SH_YearDate, 0)) <> SH_YearDate) then
      begin
        // switch year and director values
        tmpStr := SH_YearDate;
        SH_YearDate := SH_DirectorName;
        SH_DirectorName := tmpStr;
      end;
      SH_MovieName := FullTrim(copy(SH_MovieName, 1, parenthesisOpenedPos - 1));  
      SH_OverwriteOriginalTitle := 1;
      // title is asked if Modalita' batch = 0 or undefined or Modalita' batch option does not exist
      if (GetOption('Modalita'' batch') = 0) then
        SharedAskForTitle; 
    end
    else
    begin
      SH_YearDate := getField(fieldYear);
      SH_DirectorName := getField(fieldDirector);
      SharedAskForTitle; 
    end;
  end
  else
  begin  
    SharedAskForTitle; 
  end;
end;


// --------------------------------------------------------------------------------------------------
// DEFINE AND RETRIEVE WHICH ELEMENTS USE FOR SEARCHING: MOVIEURL/SOURCE OR MOVIE TITLE/YEAR/DIRECTOR
// IN:  none
// -------------------------------------------------------------------------------------------------- 
procedure SharedRetrieveSearchElements;
begin
  SharedRetrieveMovieUrl();
  if (SharedGetMovieUrl() = '') then
    SharedRetrieveMovieName();
end;


procedure SharedAskForTitle;
var inputTitle: string;
begin
  if (GetOption('Modalita'' batch') = 1) then
    SH_MovieName := SH_OriginalStr
  else
  begin  
    if (SH_Source <> '') then 
      inputTitle := SH_Source
    else
      inputTitle := 'Italian Multisite';    
    SharedSetForceExit(not Input(inputTitle, 'Inserire il titolo del film:', SH_MovieName));
    if SharedGetForceExit() then
      exit;
  end;   

  if ((SH_OverwriteOriginalTitle = 0) and (SH_MovieName <> SH_OriginalStr)) then
    // Used 'Original title' field value but it was manually changed in input field, so field 'Original title' will be overwritten in any case
    SH_OverwriteOriginalTitle := 1;
end;



function SharedSimplifyString(str: string; isDirector: boolean): string;
var
  res: string;
  i, ch, a1, a2, b1, b2: integer;
  cond, prevCond: boolean;
begin
  str := AnsiLowerCaseNoAccents(str);
  str := StringReplace(str, 'ß', 'ss');
  res := '';
	i := 1;
  a1 := ord('a');
  a2 := ord('z');  
  b1 := ord('0');
  b2 := ord('9');  
  prevCond := false;
	while (i <= Length(str)) do
  begin
    ch := ord(copy(str, i, 1));
    // in director string, skip pointed name, example j. in "robert j. adams"
    if (isDirector AND (i < Length(str)) AND (copy(str, (i + 1), 1) = '.')) then
    begin
      i := i + 2;
      continue;
    end;  
    cond := (((ch >= a1) and (ch <= a2)) OR ((ch >= b1) and (ch <= b2)));
    if (cond) then
      res := res + chr(ch)
    else
      // replace the not alphanumeric char with a space but only if previous char is not a space too, else do nothing
      if (prevCond AND isDirector) then
        res := res + ' ';  
    prevCond := cond;
    i := i + 1;
  end;  
  result := res;
end;



function SharedSplitStringInCharacter(str, delimitator: string): string;
var
  res: string;
  i: integer;
begin
  res := '';
	i := 1;
	while (i <= Length(str)) do
  begin
    res := res + copy(str, i, 1) + delimitator;
    i := i + 1;
  end;  
  result := copy(res, 1, Length(res) - 1);
end;



function SharedUTF8ToCP1252Decode(str: string): string;
begin  
  result := UTF8Decode(str);
  // Sometimes UTF8Decode fails and return a void string, then I manually decode UTF8 to CP1252 
  // (test url: https://www.film.tv.it/film/26509/la-mala-educacion/ )
  if (result = '') then
  begin
    SharedDebugAppendToFile(str, '[ItalianSharedPas][SharedUTF8ToCP1252Decode]UTF8Decode failed');
{    
    str := StringReplace(str, 'â€™', '''');   
    str := StringReplace(str, 'Ã ', 'à');
    str := StringReplace(str, 'Ã¡', 'á');
    str := StringReplace(str, 'Ã©', 'è');
    str := StringReplace(str, 'ã©', 'é');  
    str := StringReplace(str, 'Ã¨', 'è');
    str := StringReplace(str, 'Ãˆ', 'È');
    str := StringReplace(str, 'Ã­', 'í');
    str := StringReplace(str, 'Ã¬', 'ì');
    str := StringReplace(str, 'Ã²', 'ò');
    str := StringReplace(str, 'Ã¹', 'ù');
    str := StringReplace(str, ('Ã' + chr(182)), 'ö'); 
    str := StringReplace(str, 'Ã¤', 'ä');     
    str := StringReplace(str, 'â‚¬', '€'); 
    str := StringReplace(str, 'â€š', '‚'); 
    str := StringReplace(str, 'Æ’', 'ƒ'); 
    str := StringReplace(str, 'â€ž', '„'); 
    str := StringReplace(str, 'â€¦', '…');
    str := StringReplace(str, 'â€ ', '†'); 
    str := StringReplace(str, 'â€¡', '‡'); 
    str := StringReplace(str, 'Ë†', 'ˆ'); 
    str := StringReplace(str, 'â€°', '‰');
    str := StringReplace(str, 'Å ', 'Š'); 
    str := StringReplace(str, 'â€¹', '‹');
    str := StringReplace(str, 'Å’', 'Œ'); 
    str := StringReplace(str, 'Å½', 'Ž'); 
    str := StringReplace(str, 'â€˜', '‘'); 
    str := StringReplace(str, '´', '’'); 
    str := StringReplace(str, 'â€œ', '“');
    str := StringReplace(str, 'â€', '”'); 
    str := StringReplace(str, 'â€¢', '•'); 
    str := StringReplace(str, 'â€“', '–');
    str := StringReplace(str, 'â€”', '—');
    str := StringReplace(str, 'Ëœ', '˜ '); 
    str := StringReplace(str, 'â„¢', '™');
    str := StringReplace(str, 'Å¡', 'š'); 
    str := StringReplace(str, 'â€º', '›');
    str := StringReplace(str, 'Å“', 'œ'); 
    str := StringReplace(str, 'Å¾', 'ž'); 
    str := StringReplace(str, 'Å¸', 'Ÿ'); 
}
    result := SharedUTF8ToCP1252Decode2(str); 
    SharedDebugAppendToFile(result, '[ItalianSharedPas][SharedUTF8ToCP1252Decode]UTF8Decode manually decoded');    
  end;  
end;

function SharedUTF8ToCP1252Decode2(str: string): string;
begin
{
  // vietnamiti
  str := StringReplace(str, (chr(225) + chr(186) + chr(128)), 'W');
  str := StringReplace(str, (chr(225) + chr(186) + chr(129)), 'w');
  str := StringReplace(str, (chr(225) + chr(186) + chr(130)), 'W');
  str := StringReplace(str, (chr(225) + chr(186) + chr(131)), 'w');
  str := StringReplace(str, (chr(225) + chr(186) + chr(132)), 'W');
  str := StringReplace(str, (chr(225) + chr(186) + chr(133)), 'w');
  str := StringReplace(str, (chr(225) + chr(186) + chr(134)), 'W');
  str := StringReplace(str, (chr(225) + chr(186) + chr(135)), 'w');
  str := StringReplace(str, (chr(225) + chr(186) + chr(136)), 'W');
  str := StringReplace(str, (chr(225) + chr(186) + chr(137)), 'w');
  str := StringReplace(str, (chr(225) + chr(186) + chr(138)), 'X');
  str := StringReplace(str, (chr(225) + chr(186) + chr(139)), 'x');
  str := StringReplace(str, (chr(225) + chr(186) + chr(140)), 'X');
  str := StringReplace(str, (chr(225) + chr(186) + chr(141)), 'x');
  str := StringReplace(str, (chr(225) + chr(186) + chr(142)), 'Y');
  str := StringReplace(str, (chr(225) + chr(186) + chr(143)), 'y');
  str := StringReplace(str, (chr(225) + chr(186) + chr(144)), 'Z');
  str := StringReplace(str, (chr(225) + chr(186) + chr(145)), 'z');
  str := StringReplace(str, (chr(225) + chr(186) + chr(146)), 'Z');
  str := StringReplace(str, (chr(225) + chr(186) + chr(147)), 'z');
  str := StringReplace(str, (chr(225) + chr(186) + chr(148)), 'Z');
  str := StringReplace(str, (chr(225) + chr(186) + chr(149)), 'z');
  str := StringReplace(str, (chr(225) + chr(186) + chr(150)), 'h');
  str := StringReplace(str, (chr(225) + chr(186) + chr(151)), 't');
  str := StringReplace(str, (chr(225) + chr(186) + chr(152)), 'w');
  str := StringReplace(str, (chr(225) + chr(186) + chr(153)), 'y');
  str := StringReplace(str, (chr(225) + chr(186) + chr(154)), 'a');
  str := StringReplace(str, (chr(225) + chr(186) + chr(155)), 'f');
  str := StringReplace(str, (chr(225) + chr(186) + chr(156)), 'f');
  str := StringReplace(str, (chr(225) + chr(186) + chr(157)), 'f');
  str := StringReplace(str, (chr(225) + chr(186) + chr(158)), 'B');
  str := StringReplace(str, (chr(225) + chr(186) + chr(159)), 'd');
  str := StringReplace(str, (chr(225) + chr(186) + chr(160)), 'A');
  str := StringReplace(str, (chr(225) + chr(186) + chr(161)), 'a');
  str := StringReplace(str, (chr(225) + chr(186) + chr(162)), 'A');
  str := StringReplace(str, (chr(225) + chr(186) + chr(163)), 'a');
  str := StringReplace(str, (chr(225) + chr(186) + chr(164)), 'A');
  str := StringReplace(str, (chr(225) + chr(186) + chr(165)), 'a');
  str := StringReplace(str, (chr(225) + chr(186) + chr(166)), 'A');
  str := StringReplace(str, (chr(225) + chr(186) + chr(167)), 'a');
  str := StringReplace(str, (chr(225) + chr(186) + chr(168)), 'A');
  str := StringReplace(str, (chr(225) + chr(186) + chr(169)), 'a');
  str := StringReplace(str, (chr(225) + chr(186) + chr(170)), 'A');
  str := StringReplace(str, (chr(225) + chr(186) + chr(171)), 'a');
  str := StringReplace(str, (chr(225) + chr(186) + chr(172)), 'A');
  str := StringReplace(str, (chr(225) + chr(186) + chr(173)), 'a');
  str := StringReplace(str, (chr(225) + chr(186) + chr(174)), 'A');
  str := StringReplace(str, (chr(225) + chr(186) + chr(175)), 'a');
  str := StringReplace(str, (chr(225) + chr(186) + chr(176)), 'A');
  str := StringReplace(str, (chr(225) + chr(186) + chr(177)), 'a');
  str := StringReplace(str, (chr(225) + chr(186) + chr(178)), 'A');
  str := StringReplace(str, (chr(225) + chr(186) + chr(179)), 'a');
  str := StringReplace(str, (chr(225) + chr(186) + chr(180)), 'A');
  str := StringReplace(str, (chr(225) + chr(186) + chr(181)), 'a');
  str := StringReplace(str, (chr(225) + chr(186) + chr(182)), 'A');
  str := StringReplace(str, (chr(225) + chr(186) + chr(183)), 'a');
  str := StringReplace(str, (chr(225) + chr(186) + chr(184)), 'E');
  str := StringReplace(str, (chr(225) + chr(186) + chr(185)), 'e');
  str := StringReplace(str, (chr(225) + chr(186) + chr(186)), 'E');
  str := StringReplace(str, (chr(225) + chr(186) + chr(187)), 'e');
  str := StringReplace(str, (chr(225) + chr(186) + chr(188)), 'E');
  str := StringReplace(str, (chr(225) + chr(186) + chr(189)), 'e');
  str := StringReplace(str, (chr(225) + chr(186) + chr(190)), 'E');
  str := StringReplace(str, (chr(225) + chr(186) + chr(191)), 'e');
  
  str := StringReplace(str, (chr(225) + chr(187) + chr(128)), 'E');
  str := StringReplace(str, (chr(225) + chr(187) + chr(129)), 'e');
  str := StringReplace(str, (chr(225) + chr(187) + chr(130)), 'E');
  str := StringReplace(str, (chr(225) + chr(187) + chr(131)), 'e');
  str := StringReplace(str, (chr(225) + chr(187) + chr(132)), 'E');
  str := StringReplace(str, (chr(225) + chr(187) + chr(133)), 'e');
  str := StringReplace(str, (chr(225) + chr(187) + chr(134)), 'E');
  str := StringReplace(str, (chr(225) + chr(187) + chr(135)), 'e');
  str := StringReplace(str, (chr(225) + chr(187) + chr(136)), 'I');
  str := StringReplace(str, (chr(225) + chr(187) + chr(137)), 'i');
  str := StringReplace(str, (chr(225) + chr(187) + chr(138)), 'I');
  str := StringReplace(str, (chr(225) + chr(187) + chr(139)), 'i');
  str := StringReplace(str, (chr(225) + chr(187) + chr(140)), 'O');
  str := StringReplace(str, (chr(225) + chr(187) + chr(141)), 'o');
  str := StringReplace(str, (chr(225) + chr(187) + chr(142)), 'O');
  str := StringReplace(str, (chr(225) + chr(187) + chr(143)), 'o');
  str := StringReplace(str, (chr(225) + chr(187) + chr(144)), 'O');
  str := StringReplace(str, (chr(225) + chr(187) + chr(145)), 'o');
  str := StringReplace(str, (chr(225) + chr(187) + chr(146)), 'O');
  str := StringReplace(str, (chr(225) + chr(187) + chr(147)), 'o');
  str := StringReplace(str, (chr(225) + chr(187) + chr(148)), 'O');
  str := StringReplace(str, (chr(225) + chr(187) + chr(149)), 'o');
  str := StringReplace(str, (chr(225) + chr(187) + chr(150)), 'O');
  str := StringReplace(str, (chr(225) + chr(187) + chr(151)), 'o');
  str := StringReplace(str, (chr(225) + chr(187) + chr(152)), 'O');
  str := StringReplace(str, (chr(225) + chr(187) + chr(153)), 'o');
  str := StringReplace(str, (chr(225) + chr(187) + chr(154)), 'O');
  str := StringReplace(str, (chr(225) + chr(187) + chr(155)), 'o');
  str := StringReplace(str, (chr(225) + chr(187) + chr(156)), 'O');
  str := StringReplace(str, (chr(225) + chr(187) + chr(157)), 'o');
  str := StringReplace(str, (chr(225) + chr(187) + chr(158)), 'O');
  str := StringReplace(str, (chr(225) + chr(187) + chr(159)), 'o');
  str := StringReplace(str, (chr(225) + chr(187) + chr(160)), 'O');
  str := StringReplace(str, (chr(225) + chr(187) + chr(161)), 'o');
  str := StringReplace(str, (chr(225) + chr(187) + chr(162)), 'O');
  str := StringReplace(str, (chr(225) + chr(187) + chr(163)), 'o');
  str := StringReplace(str, (chr(225) + chr(187) + chr(164)), 'U');
  str := StringReplace(str, (chr(225) + chr(187) + chr(165)), 'u');
  str := StringReplace(str, (chr(225) + chr(187) + chr(166)), 'U');
  str := StringReplace(str, (chr(225) + chr(187) + chr(167)), 'u');
  str := StringReplace(str, (chr(225) + chr(187) + chr(168)), 'U');
  str := StringReplace(str, (chr(225) + chr(187) + chr(169)), 'u');
  str := StringReplace(str, (chr(225) + chr(187) + chr(170)), 'U');
  str := StringReplace(str, (chr(225) + chr(187) + chr(171)), 'u');
  str := StringReplace(str, (chr(225) + chr(187) + chr(172)), 'U');
  str := StringReplace(str, (chr(225) + chr(187) + chr(173)), 'u');
  str := StringReplace(str, (chr(225) + chr(187) + chr(174)), 'U');
  str := StringReplace(str, (chr(225) + chr(187) + chr(175)), 'u');
  str := StringReplace(str, (chr(225) + chr(187) + chr(176)), 'U');
  str := StringReplace(str, (chr(225) + chr(187) + chr(177)), 'u');
  str := StringReplace(str, (chr(225) + chr(187) + chr(178)), 'Y');
  str := StringReplace(str, (chr(225) + chr(187) + chr(179)), 'y');
  str := StringReplace(str, (chr(225) + chr(187) + chr(180)), 'Y');
  str := StringReplace(str, (chr(225) + chr(187) + chr(181)), 'y');
  str := StringReplace(str, (chr(225) + chr(187) + chr(182)), 'Y');
  str := StringReplace(str, (chr(225) + chr(187) + chr(183)), 'y');
  str := StringReplace(str, (chr(225) + chr(187) + chr(184)), 'Y');
  str := StringReplace(str, (chr(225) + chr(187) + chr(185)), 'y');
  str := StringReplace(str, (chr(225) + chr(187) + chr(186)), 'IL');
  str := StringReplace(str, (chr(225) + chr(187) + chr(187)), 'll');
  str := StringReplace(str, (chr(225) + chr(187) + chr(188)), '6');
  str := StringReplace(str, (chr(225) + chr(187) + chr(189)), '6');
  str := StringReplace(str, (chr(225) + chr(187) + chr(190)), 'Y');
  str := StringReplace(str, (chr(225) + chr(187) + chr(191)), 'y');  
}  
  
  
  // caratteri rumeni
  str := StringReplace(str, (chr(200) + chr(128)), 'A');
  str := StringReplace(str, (chr(200) + chr(129)), 'a');
  str := StringReplace(str, (chr(200) + chr(130)), 'A');
  str := StringReplace(str, (chr(200) + chr(131)), 'a');
  str := StringReplace(str, (chr(200) + chr(132)), 'E');
  str := StringReplace(str, (chr(200) + chr(133)), 'e');
  str := StringReplace(str, (chr(200) + chr(134)), 'E');
  str := StringReplace(str, (chr(200) + chr(135)), 'e');
  str := StringReplace(str, (chr(200) + chr(136)), 'I');
  str := StringReplace(str, (chr(200) + chr(137)), 'i');
  str := StringReplace(str, (chr(200) + chr(138)), 'I');
  str := StringReplace(str, (chr(200) + chr(139)), 'i');
  str := StringReplace(str, (chr(200) + chr(140)), 'O');
  str := StringReplace(str, (chr(200) + chr(141)), 'o');
  str := StringReplace(str, (chr(200) + chr(142)), 'O');
  str := StringReplace(str, (chr(200) + chr(143)), 'o');
  str := StringReplace(str, (chr(200) + chr(144)), 'R');
  str := StringReplace(str, (chr(200) + chr(145)), 'r');
  str := StringReplace(str, (chr(200) + chr(146)), 'R');
  str := StringReplace(str, (chr(200) + chr(147)), 'r');
  str := StringReplace(str, (chr(200) + chr(148)), 'U');
  str := StringReplace(str, (chr(200) + chr(149)), 'u');
  str := StringReplace(str, (chr(200) + chr(150)), 'U');
  str := StringReplace(str, (chr(200) + chr(151)), 'u');
  str := StringReplace(str, (chr(200) + chr(152)), 'S');
  str := StringReplace(str, (chr(200) + chr(153)), 's');
  str := StringReplace(str, (chr(200) + chr(154)), 'T');
  str := StringReplace(str, (chr(200) + chr(155)), 't');
  str := StringReplace(str, (chr(200) + chr(156)), '3');
  str := StringReplace(str, (chr(200) + chr(157)), '3');
  str := StringReplace(str, (chr(200) + chr(158)), 'H');
  str := StringReplace(str, (chr(200) + chr(159)), 'h');
  str := StringReplace(str, (chr(200) + chr(160)), 'n');
  str := StringReplace(str, (chr(200) + chr(161)), 'd');
  str := StringReplace(str, (chr(200) + chr(162)), '8');
  str := StringReplace(str, (chr(200) + chr(163)), '8');
  str := StringReplace(str, (chr(200) + chr(164)), 'Z');
  str := StringReplace(str, (chr(200) + chr(165)), 'z');
  str := StringReplace(str, (chr(200) + chr(166)), 'A');
  str := StringReplace(str, (chr(200) + chr(167)), 'a');
  str := StringReplace(str, (chr(200) + chr(168)), 'E');
  str := StringReplace(str, (chr(200) + chr(169)), 'e');
  str := StringReplace(str, (chr(200) + chr(170)), 'O');
  str := StringReplace(str, (chr(200) + chr(171)), 'o');
  str := StringReplace(str, (chr(200) + chr(172)), 'O');
  str := StringReplace(str, (chr(200) + chr(173)), 'o');
  str := StringReplace(str, (chr(200) + chr(174)), 'O');
  str := StringReplace(str, (chr(200) + chr(175)), 'o');
  str := StringReplace(str, (chr(200) + chr(176)), 'O');
  str := StringReplace(str, (chr(200) + chr(177)), 'o');
  str := StringReplace(str, (chr(200) + chr(178)), 'Y');
  str := StringReplace(str, (chr(200) + chr(179)), 'y');
  str := StringReplace(str, (chr(200) + chr(180)), 'l');
  str := StringReplace(str, (chr(200) + chr(181)), 'n');
  str := StringReplace(str, (chr(200) + chr(182)), 't');
  str := StringReplace(str, (chr(200) + chr(183)), 'j');
  str := StringReplace(str, (chr(200) + chr(184)), 'cb');
  str := StringReplace(str, (chr(200) + chr(185)), 'cp');
  str := StringReplace(str, (chr(200) + chr(186)), 'A');
  str := StringReplace(str, (chr(200) + chr(187)), 'C');
  str := StringReplace(str, (chr(200) + chr(188)), 'c');
  str := StringReplace(str, (chr(200) + chr(189)), 'L');
  str := StringReplace(str, (chr(200) + chr(190)), 'T');
  str := StringReplace(str, (chr(200) + chr(191)), 's');


  // standard

  str := StringReplace(str, (chr(194) + chr(10)), ' ');
  str := StringReplace(str, (chr(194) + chr(11)), '°');  
  str := StringReplace(str, (chr(194) + chr(160)), ' ');  
  str := StringReplace(str, (chr(194) + chr(161)), '¡');
  str := StringReplace(str, (chr(194) + chr(162)), '¢');
  str := StringReplace(str, (chr(194) + chr(163)), '£');
  str := StringReplace(str, (chr(194) + chr(164)), '¤');
  str := StringReplace(str, (chr(194) + chr(165)), '¥');
  str := StringReplace(str, (chr(194) + chr(166)), '¦');
  str := StringReplace(str, (chr(194) + chr(167)), '§');
  str := StringReplace(str, (chr(194) + chr(168)), '¨');
  str := StringReplace(str, (chr(194) + chr(169)), '©');
  str := StringReplace(str, (chr(194) + chr(170)), 'ª');
  str := StringReplace(str, (chr(194) + chr(171)), '«');
  str := StringReplace(str, (chr(194) + chr(172)), '¬');
  str := StringReplace(str, (chr(194) + chr(173)), '­');
  str := StringReplace(str, (chr(194) + chr(174)), '®');
  str := StringReplace(str, (chr(194) + chr(175)), '¯');
  str := StringReplace(str, (chr(194) + chr(176)), '°');
  str := StringReplace(str, (chr(194) + chr(177)), '±');
  str := StringReplace(str, (chr(194) + chr(178)), '²');
  str := StringReplace(str, (chr(194) + chr(179)), '³');
  str := StringReplace(str, (chr(194) + chr(180)), '´');
  str := StringReplace(str, (chr(194) + chr(181)), 'µ');
  str := StringReplace(str, (chr(194) + chr(182)), '¶');
  str := StringReplace(str, (chr(194) + chr(183)), '·');
  str := StringReplace(str, (chr(194) + chr(184)), '¸');
  str := StringReplace(str, (chr(194) + chr(185)), '¹');
  str := StringReplace(str, (chr(194) + chr(186)), 'º');
  str := StringReplace(str, (chr(194) + chr(187)), '»');
  str := StringReplace(str, (chr(194) + chr(188)), '¼');
  str := StringReplace(str, (chr(194) + chr(189)), '½');
  str := StringReplace(str, (chr(194) + chr(190)), '¾');
  str := StringReplace(str, (chr(194) + chr(191)), '¿');
  str := StringReplace(str, (chr(195) + chr(8)), 'À');
  str := StringReplace(str, (chr(195) + chr(9)), 'Ð');
  str := StringReplace(str, (chr(195) + chr(10)), 'à');
  str := StringReplace(str, (chr(195) + chr(11)), 'ð');
  str := StringReplace(str, (chr(195) + chr(128)), 'À');
  str := StringReplace(str, (chr(195) + chr(129)), 'Á');

  str := StringReplace(str, (chr(195) + chr(134)), 'Æ');
  str := StringReplace(str, (chr(195) + chr(135)), 'Ç');
  str := StringReplace(str, (chr(195) + chr(136)), 'È');
  str := StringReplace(str, (chr(195) + chr(137)), 'É');
  str := StringReplace(str, (chr(195) + chr(138)), 'Ê');
  str := StringReplace(str, (chr(195) + chr(139)), 'Ë');
  str := StringReplace(str, (chr(195) + chr(140)), 'Ì');
  str := StringReplace(str, (chr(195) + chr(141)), 'Í');
  str := StringReplace(str, (chr(195) + chr(142)), 'Î');
  str := StringReplace(str, (chr(195) + chr(143)), 'Ï');
  str := StringReplace(str, (chr(195) + chr(144)), 'Ð');
  str := StringReplace(str, (chr(195) + chr(145)), 'Ñ');
  str := StringReplace(str, (chr(195) + chr(146)), 'Ò');
  str := StringReplace(str, (chr(195) + chr(147)), 'Ó');
  str := StringReplace(str, (chr(195) + chr(148)), 'Ô');
  str := StringReplace(str, (chr(195) + chr(149)), 'Õ');
  str := StringReplace(str, (chr(195) + chr(150)), 'Ö');
  str := StringReplace(str, (chr(195) + chr(151)), '×');
  str := StringReplace(str, (chr(195) + chr(152)), 'Ø');
  str := StringReplace(str, (chr(195) + chr(153)), 'Ù');
  str := StringReplace(str, (chr(195) + chr(154)), 'Ú');
  str := StringReplace(str, (chr(195) + chr(155)), 'Û');
  str := StringReplace(str, (chr(195) + chr(156)), 'Ü');
  str := StringReplace(str, (chr(195) + chr(157)), 'Ý');
  str := StringReplace(str, (chr(195) + chr(158)), 'Þ');
  str := StringReplace(str, (chr(195) + chr(159)), 'ß');
  str := StringReplace(str, (chr(195) + chr(160)), 'à');
  str := StringReplace(str, (chr(195) + chr(161)), 'á');
  str := StringReplace(str, (chr(195) + chr(162)), 'â');
  str := StringReplace(str, (chr(195) + chr(163)), 'ã');
  str := StringReplace(str, (chr(195) + chr(164)), 'ä');
  str := StringReplace(str, (chr(195) + chr(165)), 'å');
  str := StringReplace(str, (chr(195) + chr(166)), 'æ');
  str := StringReplace(str, (chr(195) + chr(167)), 'ç');
  str := StringReplace(str, (chr(195) + chr(168)), 'è');
  str := StringReplace(str, (chr(195) + chr(169)), 'é');
  str := StringReplace(str, (chr(195) + chr(170)), 'ê');
  str := StringReplace(str, (chr(195) + chr(171)), 'ë');
  str := StringReplace(str, (chr(195) + chr(172)), 'ì');
  str := StringReplace(str, (chr(195) + chr(173)), 'í');
  str := StringReplace(str, (chr(195) + chr(174)), 'î');
  str := StringReplace(str, (chr(195) + chr(175)), 'ï');
  str := StringReplace(str, (chr(195) + chr(176)), 'ð');
  str := StringReplace(str, (chr(195) + chr(177)), 'ñ');
  str := StringReplace(str, (chr(195) + chr(178)), 'ò');
  str := StringReplace(str, (chr(195) + chr(179)), 'ó');
  str := StringReplace(str, (chr(195) + chr(180)), 'ô');
  str := StringReplace(str, (chr(195) + chr(181)), 'õ');
  str := StringReplace(str, (chr(195) + chr(182)), 'ö');
  str := StringReplace(str, (chr(195) + chr(183)), '÷');
  str := StringReplace(str, (chr(195) + chr(184)), 'ø');
  str := StringReplace(str, (chr(195) + chr(185)), 'ù');
  str := StringReplace(str, (chr(195) + chr(186)), 'ú');
  str := StringReplace(str, (chr(195) + chr(187)), 'û');
  str := StringReplace(str, (chr(195) + chr(188)), 'ü');
  str := StringReplace(str, (chr(195) + chr(189)), 'ý');
  str := StringReplace(str, (chr(195) + chr(190)), 'þ');
  str := StringReplace(str, (chr(195) + chr(191)), 'ÿ');
  str := StringReplace(str, (chr(196) + chr(8)), 'A');
  str := StringReplace(str, (chr(196) + chr(9)), 'Ð');
  str := StringReplace(str, (chr(196) + chr(10)), 'G');
  str := StringReplace(str, (chr(196) + chr(11)), 'I');  
  str := StringReplace(str, (chr(196) + chr(128)), 'A');
  str := StringReplace(str, (chr(196) + chr(129)), 'a');
  str := StringReplace(str, (chr(196) + chr(130)), 'A');
  str := StringReplace(str, (chr(196) + chr(131)), 'a');
  str := StringReplace(str, (chr(196) + chr(132)), 'A');
  str := StringReplace(str, (chr(196) + chr(133)), 'a');
  str := StringReplace(str, (chr(196) + chr(134)), 'C');
  str := StringReplace(str, (chr(196) + chr(135)), 'c');
  str := StringReplace(str, (chr(196) + chr(136)), 'C');
  str := StringReplace(str, (chr(196) + chr(137)), 'c');
  str := StringReplace(str, (chr(196) + chr(138)), 'C');
  str := StringReplace(str, (chr(196) + chr(139)), 'c');
  str := StringReplace(str, (chr(196) + chr(140)), 'C');
  str := StringReplace(str, (chr(196) + chr(141)), 'c');
  str := StringReplace(str, (chr(196) + chr(142)), 'D');
  str := StringReplace(str, (chr(196) + chr(143)), 'd');
  str := StringReplace(str, (chr(196) + chr(144)), 'Ð');
  str := StringReplace(str, (chr(196) + chr(145)), 'd');
  str := StringReplace(str, (chr(196) + chr(146)), 'E');
  str := StringReplace(str, (chr(196) + chr(147)), 'e');
  str := StringReplace(str, (chr(196) + chr(148)), 'E');
  str := StringReplace(str, (chr(196) + chr(149)), 'e');
  str := StringReplace(str, (chr(196) + chr(150)), 'E');
  str := StringReplace(str, (chr(196) + chr(151)), 'e');
  str := StringReplace(str, (chr(196) + chr(152)), 'E');
  str := StringReplace(str, (chr(196) + chr(153)), 'e');
  str := StringReplace(str, (chr(196) + chr(154)), 'E');
  str := StringReplace(str, (chr(196) + chr(155)), 'e');
  str := StringReplace(str, (chr(196) + chr(156)), 'G');
  str := StringReplace(str, (chr(196) + chr(157)), 'g');
  str := StringReplace(str, (chr(196) + chr(158)), 'G');
  str := StringReplace(str, (chr(196) + chr(159)), 'g');
  str := StringReplace(str, (chr(196) + chr(160)), 'G');
  str := StringReplace(str, (chr(196) + chr(161)), 'g');
  str := StringReplace(str, (chr(196) + chr(162)), 'G');
  str := StringReplace(str, (chr(196) + chr(163)), 'g');
  str := StringReplace(str, (chr(196) + chr(164)), 'H');
  str := StringReplace(str, (chr(196) + chr(165)), 'h');
  str := StringReplace(str, (chr(196) + chr(166)), 'H');
  str := StringReplace(str, (chr(196) + chr(167)), 'h');
  str := StringReplace(str, (chr(196) + chr(168)), 'I');
  str := StringReplace(str, (chr(196) + chr(169)), 'i');
  str := StringReplace(str, (chr(196) + chr(170)), 'I');
  str := StringReplace(str, (chr(196) + chr(171)), 'i');
  str := StringReplace(str, (chr(196) + chr(172)), 'I');
  str := StringReplace(str, (chr(196) + chr(173)), 'i');
  str := StringReplace(str, (chr(196) + chr(174)), 'I');
  str := StringReplace(str, (chr(196) + chr(175)), 'i');
  str := StringReplace(str, (chr(196) + chr(176)), 'I');
  str := StringReplace(str, (chr(196) + chr(177)), 'i');
  str := StringReplace(str, (chr(196) + chr(178)), 'IJ');
  str := StringReplace(str, (chr(196) + chr(179)), 'ij');
  str := StringReplace(str, (chr(196) + chr(180)), 'J');
  str := StringReplace(str, (chr(196) + chr(181)), 'j');
  str := StringReplace(str, (chr(196) + chr(182)), 'K');
  str := StringReplace(str, (chr(196) + chr(183)), 'k');
  str := StringReplace(str, (chr(196) + chr(184)), '?');
  str := StringReplace(str, (chr(196) + chr(185)), 'L');
  str := StringReplace(str, (chr(196) + chr(186)), 'l');
  str := StringReplace(str, (chr(196) + chr(187)), 'L');
  str := StringReplace(str, (chr(196) + chr(188)), 'l');
  str := StringReplace(str, (chr(196) + chr(189)), 'L');
  str := StringReplace(str, (chr(196) + chr(190)), 'l');
  str := StringReplace(str, (chr(196) + chr(191)), 'L');
  str := StringReplace(str, (chr(197) + chr(8)), 'l');
  str := StringReplace(str, (chr(197) + chr(9)), 'O');
  str := StringReplace(str, (chr(197) + chr(10)), 'Š');
  str := StringReplace(str, (chr(197) + chr(11)), 'U');
  str := StringReplace(str, (chr(197) + chr(128)), 'l');
  str := StringReplace(str, (chr(197) + chr(129)), 'L');
  str := StringReplace(str, (chr(197) + chr(130)), 'l');
  str := StringReplace(str, (chr(197) + chr(131)), 'N');
  str := StringReplace(str, (chr(197) + chr(132)), 'n');
  str := StringReplace(str, (chr(197) + chr(133)), 'N');
  str := StringReplace(str, (chr(197) + chr(134)), 'n');
  str := StringReplace(str, (chr(197) + chr(135)), 'N');
  str := StringReplace(str, (chr(197) + chr(136)), 'n');
  str := StringReplace(str, (chr(197) + chr(137)), 'n');
  str := StringReplace(str, (chr(197) + chr(138)), 'N');
  str := StringReplace(str, (chr(197) + chr(139)), 'n');
  str := StringReplace(str, (chr(197) + chr(140)), 'O');
  str := StringReplace(str, (chr(197) + chr(141)), 'o');
  str := StringReplace(str, (chr(197) + chr(142)), 'O');
  str := StringReplace(str, (chr(197) + chr(143)), 'o');
  str := StringReplace(str, (chr(197) + chr(144)), 'O');
  str := StringReplace(str, (chr(197) + chr(145)), 'o');
  str := StringReplace(str, (chr(197) + chr(146)), 'Œ');
  str := StringReplace(str, (chr(197) + chr(147)), 'œ');  
  str := StringReplace(str, (chr(197) + chr(148)), 'R');
  str := StringReplace(str, (chr(197) + chr(149)), 'r');
  str := StringReplace(str, (chr(197) + chr(150)), 'R');
  str := StringReplace(str, (chr(197) + chr(151)), 'r');
  str := StringReplace(str, (chr(197) + chr(152)), 'R');
  str := StringReplace(str, (chr(197) + chr(153)), 'r');
  str := StringReplace(str, (chr(197) + chr(154)), 'S');
  str := StringReplace(str, (chr(197) + chr(155)), 's');
  str := StringReplace(str, (chr(197) + chr(156)), 'S');
  str := StringReplace(str, (chr(197) + chr(157)), 's');
  str := StringReplace(str, (chr(197) + chr(158)), 'S');
  str := StringReplace(str, (chr(197) + chr(159)), 's');
  str := StringReplace(str, (chr(197) + chr(160)), 'Š');
  str := StringReplace(str, (chr(197) + chr(161)), 'š');  
  str := StringReplace(str, (chr(197) + chr(162)), 'T');
  str := StringReplace(str, (chr(197) + chr(163)), 't');
  str := StringReplace(str, (chr(197) + chr(164)), 'T');
  str := StringReplace(str, (chr(197) + chr(165)), 't');
  str := StringReplace(str, (chr(197) + chr(166)), 'T');
  str := StringReplace(str, (chr(197) + chr(167)), 't');
  str := StringReplace(str, (chr(197) + chr(168)), 'U');
  str := StringReplace(str, (chr(197) + chr(169)), 'u');
  str := StringReplace(str, (chr(197) + chr(170)), 'U');
  str := StringReplace(str, (chr(197) + chr(171)), 'u');
  str := StringReplace(str, (chr(197) + chr(172)), 'U');
  str := StringReplace(str, (chr(197) + chr(173)), 'u');
  str := StringReplace(str, (chr(197) + chr(174)), 'U');
  str := StringReplace(str, (chr(197) + chr(175)), 'u');
  str := StringReplace(str, (chr(197) + chr(176)), 'U');
  str := StringReplace(str, (chr(197) + chr(177)), 'u');
  str := StringReplace(str, (chr(197) + chr(178)), 'U');
  str := StringReplace(str, (chr(197) + chr(179)), 'u');
  str := StringReplace(str, (chr(197) + chr(180)), 'W');
  str := StringReplace(str, (chr(197) + chr(181)), 'w');
  str := StringReplace(str, (chr(197) + chr(182)), 'Y');
  str := StringReplace(str, (chr(197) + chr(183)), 'y');
  str := StringReplace(str, (chr(197) + chr(184)), 'Ÿ');  
  str := StringReplace(str, (chr(197) + chr(185)), 'Z');
  str := StringReplace(str, (chr(197) + chr(186)), 'z');
  str := StringReplace(str, (chr(197) + chr(187)), 'Z');
  str := StringReplace(str, (chr(197) + chr(188)), 'z');
  str := StringReplace(str, (chr(197) + chr(189)), 'Ž');
  str := StringReplace(str, (chr(197) + chr(190)), 'ž');  
  str := StringReplace(str, (chr(197) + chr(191)), 'f');
   
  str := StringReplace(str, (chr(195) + chr(130)), 'Â');  
  str := StringReplace(str, (chr(195) + chr(132)), 'Ä');
  str := StringReplace(str, (chr(195) + chr(133)), 'Å');
  
  // questa per ultima
  str := StringReplace(str, (chr(195) + chr(131)), 'Ã');
  result := str;
end;



function SharedUnknownToCP1252Decode(str: string): string;
begin
  SharedDebugAppendToFile(str, '[ItalianSharedPas][SharedUnknownToCP1252Decode]before decoding');  

  // note: Â:194, Ã:195, Ä:196, Å:197, ©:169, °:176  
{  
  // caratteri vietnamiti
  str := StringReplace(str, (chr(195) + chr(161) + chr(194) + chr(186) + chr(194)), (chr(225) + chr(186)));
  str := StringReplace(str, (chr(195) + chr(161) + chr(194) + chr(187) + chr(194)), (chr(225) + chr(187)));
}  

  // caratteri rumeni
  str := StringReplace(str, (chr(195) + chr(136) + chr(194)), chr(200)); 
  // caratteri standard
  str := StringReplace(str, (chr(195) + chr(130) + chr(194)), chr(194));  
  str := StringReplace(str, (chr(195) + chr(131) + chr(194)), chr(195));
  str := StringReplace(str, (chr(195) + chr(132) + chr(194)), chr(196)); 
  str := StringReplace(str, (chr(195) + chr(133) + chr(194)), chr(197));  
  
  //result := UTF8Decode(str);
  result := SharedUTF8ToCP1252Decode2(str);
  SharedDebugAppendToFile(result, '[ItalianSharedPas][SharedUnknownToCP1252Decode]after decoding');
end;


// --------------------------------------------------------------------------------------------------------
// UrlEncode working: does not encode new line, carrier feed, tab, backspace and other invisible characters
// IN: string to encode, raw url encode mode (see difference in php between urlencode and rawurlencode)
// OUT: string encoded
// --------------------------------------------------------------------------------------------------------
function SharedAdvancedUrlEncode(str: string; raw: boolean): string;
var
  oldchar, newchar: string;
begin
  result := '';
  while (Length(str) > 0) do
  begin
    oldchar := copy(str, 1, 1);
    Delete(str, 1, 1);
    newchar := oldchar;
    case oldchar of
      #10: newchar := '%0a';
      #13: newchar := '%0d';
      #160: newchar := '%a0';
      #127: newchar := '%7f';
      #129: newchar := '%81';
      #141: newchar := '%8d';
      #143: newchar := '%8f';
      #144: newchar := '%90';
      #157: newchar := '%9d';
      #173: newchar := '%ad';
      'æ': newchar := '%00';
      '`': newchar := '%60';
      ':': newchar := '%3a';
      ';': newchar := '%3b';
      '<': newchar := '%3c';
      '=': newchar := '%3d';
      '>': newchar := '%3e';
      '?': newchar := '%3f';
      '@': newchar := '%40';
      '{': newchar := '%7b';
      '|': newchar := '%7c';
      '}': newchar := '%7d';
      '€': newchar := '%80';
      '!': newchar := '%21';
      '"': newchar := '%22';
      '‚': newchar := '%82';
      '#': newchar := '%23';
      'ƒ': newchar := '%83';
      '$': newchar := '%24';
      '„': newchar := '%84';
      '%': newchar := '%25';
      '…': newchar := '%85';
      '&': newchar := '%26';
      '†': newchar := '%86';
      '''': newchar := '%27';
      '‡': newchar := '%87';
      '(': newchar := '%28';
      'ˆ': newchar := '%88';
      ')': newchar := '%29';
      '‰': newchar := '%89';
      '*': newchar := '%2a';
      'Š': newchar := '%8a';
      '+': newchar := '%2b';
      '[': newchar := '%5b';
      '‹': newchar := '%8b';
      ',': newchar := '%2c';
      '\': newchar := '%5c';
      'Œ': newchar := '%8c';
      ']': newchar := '%5d';
      '^': newchar := '%5e';
      'Ž': newchar := '%8e';
      '/': newchar := '%2f';
      'À': newchar := '%c0';
      'ð': newchar := '%f0';
      '‘': newchar := '%91';
      'Á': newchar := '%c1';
      'ñ': newchar := '%f1';
      '’': newchar := '%92';
      'Â': newchar := '%c2';
      'ò': newchar := '%f2';
      '“': newchar := '%93';
      'Ã': newchar := '%c3';
      'ó': newchar := '%f3';
      '”': newchar := '%94';
      'Ä': newchar := '%c4';
      'ô': newchar := '%f4';
      '•': newchar := '%95';
      'Å': newchar := '%c5';
      'õ': newchar := '%f5';
      '–': newchar := '%96';
      'Æ': newchar := '%c6';
      'ö': newchar := '%f6';
      '—': newchar := '%97';
      'Ç': newchar := '%c7';
      '÷': newchar := '%f7';
      '˜': newchar := '%98';
      'È': newchar := '%c8';
      'ø': newchar := '%f8';
      '™': newchar := '%99';
      'É': newchar := '%c9';
      'ù': newchar := '%f9';
      'š': newchar := '%9a';
      'Ê': newchar := '%ca';
      'ú': newchar := '%fa';
      '›': newchar := '%9b';
      'Ë': newchar := '%cb';
      'û': newchar := '%fb';
      'œ': newchar := '%9c';
      'Ì': newchar := '%cc';
      'ü': newchar := '%fc';
      'Í': newchar := '%cd';
      'ý': newchar := '%fd';
      'ž': newchar := '%9e';
      'Î': newchar := '%ce';
      'þ': newchar := '%fe';
      'Ÿ': newchar := '%9f';
      'Ï': newchar := '%cf';
      'ÿ': newchar := '%ff';
      'Ð': newchar := '%d0';
      '¡': newchar := '%a1';
      'Ñ': newchar := '%d1';
      '¢': newchar := '%a2';
      'Ò': newchar := '%d2';
      '£': newchar := '%a3';
      'Ó': newchar := '%d3';
      'Ô': newchar := '%d4';
      '¥': newchar := '%a5';
      'Õ': newchar := '%d5';
      '|': newchar := '%a6';
      'Ö': newchar := '%d6';
      '§': newchar := '%a7';
      '¨': newchar := '%a8';
      'Ø': newchar := '%d8';
      '©': newchar := '%a9';
      'Ù': newchar := '%d9';
      'ª': newchar := '%aa';
      'Ú': newchar := '%da';
      '«': newchar := '%ab';
      'Û': newchar := '%db';
      '¬': newchar := '%ac';
      'Ü': newchar := '%dc';
      'Ý': newchar := '%dd';
      '®': newchar := '%ae';
      'Þ': newchar := '%de';
      '¯': newchar := '%af';
      'ß': newchar := '%df';
      '°': newchar := '%b0';
      'à': newchar := '%e0';
      '±': newchar := '%b1';
      'á': newchar := '%e1';
      '²': newchar := '%b2';
      'â': newchar := '%e2';
      '³': newchar := '%b3';
      'ã': newchar := '%e3';
      '´': newchar := '%b4';
      'ä': newchar := '%e4';
      'µ': newchar := '%b5';
      'å': newchar := '%e5';
      '¶': newchar := '%b6';
      'æ': newchar := '%e6';
      '·': newchar := '%b7';
      'ç': newchar := '%e7';
      '¸': newchar := '%b8';
      'è': newchar := '%e8';
      '¹': newchar := '%b9';
      'é': newchar := '%e9';
      'º': newchar := '%ba';
      'ê': newchar := '%ea';
      '»': newchar := '%bb';
      'ë': newchar := '%eb';
      '¼': newchar := '%bc';
      'ì': newchar := '%ec';
      '½': newchar := '%bd';
      'í': newchar := '%ed';
      '¾': newchar := '%be';
      'î': newchar := '%ee';
      '¿': newchar := '%bf';
      'ï': newchar := '%ef';
      '~': 
        case raw of
        true: newchar := '~';
        false: newchar := '%7e';
        end;
      ' ':
        case raw of
        true: newchar := '%20'; // raw url encoding
        false: newchar := '+'; // encoding
        end;
    end;
    result := result + newchar;  
  end;
end;


// ------------------------------------
// INIT SETUP UNIT VARIABLES AND STATES
// IN:  Source, stand alone mode
// OUT: None
// ------------------------------------
procedure SharedInitCommon(SourceParam: string; isStandAloneParam: boolean);
begin
  SharedSetSource(SourceParam);
  SharedSetForceExit(false);
  SharedSetStandAloneMode(isStandAloneParam);
  SharedSetMovieUrl('');
  SharedSetMoviesFound(0); 
  SH_LatestMovieUrlInPickTree := '';  
  if (((SourceParam = '') AND (not isStandAloneParam)) OR isStandAloneParam) then // full init
  begin 
    SH_MovieName := '';
    SH_OriginalStr := '';
    SH_TranslatedStr := '';
    SH_DirectorName := '';
    SH_YearDate := '';
    SH_LatestPageHtml := '';
    SH_LatestPageUrl := '';
    SH_CoverUrl := '';
    SH_CoverReferral := '';
    SH_LatestMovieUrlInPickTree := '';    
    SH_OverwriteOriginalTitle := 0;
  end;
end;


// --------------------------------------------
// SETUP & MAIN CONTROLS FOR STAND ALONE SCRIPT
// IN:  Source
// OUT: None
// --------------------------------------------  
procedure SharedInitStandAlone(SourceParam: string);
begin
  if not CheckVersion(4, 2, 3) then
  begin
    SharedShowMessage('Questo script richiede una versione aggiornata di Ant Movie Catalog (versione 4.2.3.3 o successiva)');
    exit;
  end;

  if StringUtils1_Version < 7 then
  begin
    SharedShowMessage('Questo script richiede una versione aggiornata di StringUtils1.pas (versione 7 o successiva)');
    exit;
  end;

  SharedInitCommon(SourceParam, true);  
  SharedRetrieveSearchElements();
end;

// --------------------------------------------------------------------------------------------------------------------------------------
//                                               "private" functions e procedures
// --------------------------------------------------------------------------------------------------------------------------------------

end.
