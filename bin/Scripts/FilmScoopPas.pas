unit FilmScoopPas;

{
  Main var, constants and functions
}
uses
  ItalianSharedPas;

const
  FS_UnitVersion = 3;

  FS_UrlBase = 'http://www.filmscoop.it';
  FS_QueryBase = FS_UrlBase + '/ricerca/risultati.asp?t=';
  FS_QueryFilm = FS_UrlBase + '/film_al_cinema/';
  FS_ImagePath = FS_UrlBase + '/locandine/';

  FS_cStartNumRis = 'Numero di risultati: <strong>'; // Result Number start Marker
  FS_cEndNumRis = '</strong> - Pagin';               // Result Number end Marker
  FS_cStartId = 'href="/film_al_cinema/';      // ID start marker
  FS_cEndId = '"';                             // ID end marker
  FS_cStartTitle = '<h1 class="TitoloFilmUpper"'; // Title start marker
  FS_cEndTitle = '</a>';                          // Title end marker
  FS_cStartTranslTitle = 'Titolo Originale</strong>: ';        // Translated title start marker
  FS_cEndTranslTitle = '</h2>';                                // Translated title end marker
  FS_cStartImg = '<img src="http://www.filmscoop.it/locandine/';               // Image start marker
  FS_cEndImg = '" alt';                                                        // Image end marker
  FS_cStartDirector = 'Regia</strong>:';               // Director start marker
  FS_cStartCast = 'Interpreti</strong>:';              // Actor start marker
  FS_cStartCategory = 'Genere</strong>:';       // Catogory start marker
  FS_cEndCategory = '</a>';                    // Category end marker
  FS_cStartDuration = 'Durata</strong>:';       // Duration start marker
  FS_cEndDuration = '<br />';                    // Duration end marker
  FS_cStartCountry = '<strong>Nazionalit&agrave;</strong>:';     // Country start Marker
  FS_cEndCountry = '<strong>Genere';                             // Country end marker
  FS_cStartrecensione = '<div id="TestoRecensione">';
  FS_cEndrecensione = '</div>';
  FS_cStartYear = ' ';                     // Year start marker
  FS_cEndYear = '<br />';                 // Year end marker
  FS_cStartDesc = '<h2 style="margin:25px 0 0 0;padding:0;font-weight:bold;">Trama del film';
  // Description start marker
  FS_cEndDesc = '</p>';
  // Description end marker
  FS_cStartComm = '<div align="center" class="comtext" style="margin-top:8px;">Commenti:';
  // Comments start marker
  FS_cMidComm = '<div class="divCommentoLeft"';
  FS_cEndComm = '<script type="text/javascript">';
  // Comments end marker
  FS_cStartTitleList = '<strong>';            // Title list start marker
  FS_cEndTitleList = '</strong>';             // Title list end marker
  FS_cStartYearList = '(';                    // Year list start marker
  FS_cEndYearList = ')';                      // Year list end marker  
  FS_cStartOriginalTitleList = '<em>';        // Original title list start marker
  FS_cEndOriginalTitleList = '</em>';         // Original title list end marker
  FS_cStartDirectorList = '<a';              // Director list start marker
  FS_cEndDirectorList = '</a>';              // Director list end marker  


// --------------------------------------------------------------------------------------------------------------------------------------
//                                               "public" functions e procedures
// --------------------------------------------------------------------------------------------------------------------------------------

// -----------------------------------
// Main program (for MultiSite)
// IN: properties values (serialized)
// OUT: properties values (serialized)
// -----------------------------------
function FilmScoopCoreMultiSite(serializedValues: string): string;
begin
  SharedUnserializeMe(serializedValues);
  // Analyze search page and get movie url (user choice)
  if (SharedGetMoviesFound() = 0) then
    FilmScoopSearchResults();
  if (SharedGetMoviesFound() > 0) then // Analyze movie page and set movie fields
    FilmScoopSetMovieFields();
  result := SharedSerializeMe();
end;

// ---------------------------------
// ANALYZE FIRST SEARCH RESULT PAGE:
// IN:  none
// ---------------------------------
procedure FilmScoopSearchResults;
var
  myUrl: string;
  myTotal, pageNumberMovies: integer;
  nextPageAvailable: boolean;
begin
  pageNumberMovies := 0;
  repeat
    SharedSetMovieUrl('');
    SharedSetMoviesFound(0);
    pageNumberMovies := pageNumberMovies + 1; 
    // the lower case and the advanced url encode are needed for titles as "Il Poliziotto È Marcio"
    myUrl := FS_QueryBase + SharedAdvancedUrlEncode(AnsiLowerCase(SharedGetMovieName()), false) + '&to=&r=&i=&n=&g=0&an=&a=&o=Anno&Submit=Cerca&p=' + IntToStr(pageNumberMovies);  
    SharedHTTPGetPage(myUrl);

    myTotal := StrToInt(Textbetween(SharedGetLatestPageHtml(), FS_cStartNumRis, FS_cEndNumRis), 0);
    SharedSetMoviesFound(myTotal);
    if myTotal = 0 then
      exit;

    nextPageAvailable := FilmScoopPopulatePickTree();   
    if (SharedGetMovieUrl() = '') then  
      if ((SharedGetMoviesFound() = 0) AND nextPageAvailable) then 
        // se non ho trovato nessun risultato in questa pagina e se posso andare avanti ci vado
        SharedSetMovieUrl('next:')   
      else       
        SharedPickTreeExec();       
  until (copy(SharedGetMovieUrl(), 1, 5) <> 'next:');
end;


// -----------------------
// ANALYZE MOVIE DATA PAGE
// IN:  none
// -----------------------
procedure FilmScoopSetMovieFields;
var
  originalTitle, translatedTitle, rating, year, producers, directors, actors, duration, categories, certification, countries, description, comments, picture: string;
  pageStr, descrLink, descrAll, allComments, extrValue, start_descrLink, cImage, cValue, SaveValue, str_min: string;
  ore, minu, minuti: integer;
begin
  // Get packed title main page
  descrAll := '';
  SharedHTTPGetPage(SharedGetMovieUrl()); 
  pageStr := textbefore(SharedGetLatestPageHtml(), '<div class="BoxLeft">', '');                  //2018.05.29
  start_descrLink := '<a href="/cgi-bin/recensioni/';
  if pos(start_descrLink, pageStr) > 0 then
  begin
    descrLink := start_descrLink + textbetween(pageStr, start_descrLink, '"');
    descrLink := textafter(descrLink, '"');
    descrLink := FS_UrlBase + descrLink;
    SharedHTTPGetPage(descrLink);
    descrAll := FilmScoopExtractDescription();  
  end;
  // TRANSLATED TITLE
  cValue := FS_cStartTitle + textbetween(pageStr, FS_cStartTitle, FS_cEndTitle) + FS_cEndTitle;
  HTMLRemoveTags(cValue);
  HTMLDecode(cValue);
  cValue := fulltrim(cValue);
  translatedTitle := AnsiUpFirstLetter(AnsiLowerCase(cValue));

  // FILM IMAGE
  cValue := textbetween(pageStr, FS_cStartImg, FS_cEndImg);
  if cValue <> '' then
    cValue := FS_ImagePath + cValue;
  picture := cValue;
  
  // ORIGINAL TITLE
  cValue := textbetween(pageStr, FS_cStartTranslTitle, FS_cEndTranslTitle);
  HTMLRemoveTags(cValue);
  HTMLDecode(cValue);
  originalTitle := AnsiUpFirstLetter(AnsiLowerCase(cValue));
      
  // DIRECTOR
  cValue := textbetween(pageStr, FS_cStartDirector, FS_cStartCast);
  HTMLRemoveTags(cValue);
  HTMLDecode(cValue);
  directors := fulltrim(cValue);
  
  // ACTORS
  cValue := textbetween(pageStr, FS_cStartCast, FS_cStartDuration);
  HTMLRemoveTags(cValue);
  HTMLDecode(cValue);
  actors := fulltrim(cValue);

  // COUNTRY
  cValue := textbetween(pageStr, FS_cStartCountry, FS_cEndCountry);
  HTMLRemoveTags(cValue);
  HTMLDecode(cValue);
  cValue := fulltrim(cValue);
  saveValue := cValue;
  cValue := textbefore(saveValue, ' 2', '');             //caratteri prima dell'anno di edizione
  if cValue = '' then
    cValue := textbefore(saveValue, ' 1', '');             //caratteri prima dell'anno di edizione
  countries := cValue;
    
  // YEAR
  cValue := textbetween(pageStr, FS_cStartCountry, FS_cEndCountry);
  HTMLRemoveTags(cValue);
  HTMLDecode(cValue);
  cValue := fulltrim(cValue);
  saveValue := cValue;
  cValue := textafter(saveValue, ' 2');               //caratteri dopo l'anno di edizione
  if cValue <> '' then
    cValue := '2' + cValue                           //caratteri dopo l'anno di edizione
  else
    cValue := '1' + textafter(saveValue, ' 1');
  year := cValue;

  // CATEGORY
  cValue := textbetween(pageStr, FS_cStartCategory, FS_cEndCategory);
  HTMLRemoveTags(cValue);
  HTMLDecode(cValue);
  cValue := fulltrim(cValue);
  categories := AnsiUpFirstLetter(AnsiLowerCase(cValue));

  // COMMENTS
  allComments := FS_cStartComm + textbetween(pageStr, FS_cStartComm, FS_cEndComm) + FS_cMidComm + FS_cEndComm;
  SharedDebugAppendToFile(allComments, '[FilmScoopPas][FilmScoopSetMovieFields] comments html');
  extrValue := FilmScoopCommentsFormat(allComments, extrValue);
  if length(extrValue) < 5 then                              //lunghezza '--- ' iniziale ai commenti
    extrValue := '';
  comments := extrValue;

  // DESCRIPTION
  cValue := FS_cStartDesc + textbetween(pageStr, FS_cStartDesc, FS_cEndDesc) + FS_cEndDesc;
  cValue := '<p' + textBetween(cValue, '<p', '</p>') + '</p>';
  HTMLRemoveTags(cValue);
  HTMLDecode(cValue);
  cValue := fulltrim(cValue);
  if (cValue = '-') then
    cValue := '';
  if descrAll <> '' then
    if (cValue = '') then
      cValue := 'Recensione' + CRLF + descrAll
    else
      cValue := cValue + CRLF + CRLF + 'Recensione' + CRLF + descrAll;
  description := cValue;    

  // DURATA
  cValue := textbetween(pageStr, '<strong>Durata</strong>:', '<br />');
  cValue := textafter(cValue, 'h ');
  //-------------- da discogs
  ore := StrToInt(TextBefore(cValue, '.', ''), 0);
  str_min := TextAfter(cValue, '.');
  minuti := StrToInt(str_min, 0);
  if minuti > 0 then
    Minu := ore * 60 + Minuti;
  if minuti = 0 then
    Minu := ore;
  str_min := IntToStr(Minu);
  if (str_min = '0') then
    str_min := '';
  duration := str_min;
  //------------ fine discogs

  SharedSecureSetAllFields(originalTitle, translatedTitle, '', year, '', '', '', directors, actors, duration, categories, '', countries, description, comments, SharedGetMovieUrl(), picture);
end;


// --------------------------------------------------------------------------------------------------------------------------------------
//                                               "private" functions e procedures
// --------------------------------------------------------------------------------------------------------------------------------------

function FilmScoopExtractDescription(): string;
var
  cValue: string;
begin
  cValue := textbetween(SharedGetLatestPageHtml(), FS_cStartrecensione, FS_cEndrecensione);
  HTMLRemoveTags(cValue);
  HTMLDecode(cValue);
  result := fulltrim(cValue);
end;

function FilmScoopCommentsFormat(paramAllComments, paramExtrValue: string):string;
var
  startchar, endchar: string;
  startspoil, endspoil, delstr: string;
  allComments, precomments, extrValue, cValue: string;
  ctr_giri: integer;
begin
  allComments := paramAllComments;
  extrValue := paramExtrValue;
  ctr_giri := 1;
  startChar := '<div class="divCommentoLeft"';
  endChar := '<div class="divCommentoLeft"';
  cValue := textbetween(allComments, startchar, startchar) + endChar;
  SharedDebugAppendToFile(cValue, '[FilmScoopPas][FilmScoopCommentsFormat] comments html - cValue main');

  repeat
    ctr_giri := ctr_giri + 1;
    allComments := '<div>' + stringReplace(allComments, cValue, '');
    //2018.06.01    allComments := startChar + allComments + endChar;         //2018.06.01
    cValue := textbefore(cValue, '<div class="risposte">', '');
    SharedDebugAppendToFile(cValue, '[FilmScoopPas][FilmScoopCommentsFormat] comments html - cValue in repeat loop');

    // elimina spoiler      --------------------
    startspoil := '<p class="spoiler">';
    endspoil := '</p>';
    delstr := startspoil + textbetween(cValue, startspoil, endspoil) + endspoil;
    cValue := stringreplace(cValue, delstr, '');
    delstr := '<strong>SPOILER</strong>';
    cValue := stringreplace(cValue, delstr, '');
    // fine elimina spoiler --------------------
    cValue := stringreplace(cValue, ' / 10', ' / 10. ---  ');
    delstr := 'id="divCommentoLeft';
    delstr := delstr + textbetween(cValue, 'id="divCommentoLeft', '>') + '>';
    cValue := stringreplace(cValue, delstr, '');

    HTMLRemoveTags(cValue);
    HTMLDecode(cValue);
    cValue := fulltrim(cValue);
    extrValue := extrValue + CRLF + CRLF + '--- ' + cValue;
    precomments := allComments;
    cValue := textbetween(allComments, startchar, startchar) + endChar;
    //    if cValue = (startchar + endchar) then                                                                       //2018.06.01
    //       cValue := startchar + textbetween(PreComments, startchar, '<div class="risposte">');   //2018.06.01
  until cValue = endChar;
  //    all'uscita del loop, l'ultimo commento è contenuto in save_cValue. devo aggiungerlo!
  result := fulltrim(extrValue);
end;

// ----------------------------------------------------------------------------
// FILL PICKTREE CONTROL WITH LINKS & TITLES or RETURN ONE PAGE LINK
// if year and director match return the right page link else populate PickTree
// IN:  none
// OUT: if exists a next page with further search results
// ----------------------------------------------------------------------------
function FilmScoopPopulatePickTree(): boolean;
var
  pageStr, cFilmId, cFilmTitle, cFilmOriginalTitle, cDirector, cYear, indirizzo, save_indirizzo: string;
  StartPos, EndPos: integer;
begin
  SharedPickTreeCreate(); 
  pageStr := SharedGetLatestPageHtml();
  StartPos := pos(FS_cStartId, pageStr);
  while ((StartPos > 0) AND (SharedGetMovieUrl() = '')) do
  begin
    Delete(pageStr, 1, StartPos - 1);
    cFilmId := textbetween(pageStr, FS_cStartId, FS_cEndId);                        // Get ID
    HTMLRemoveTags(cFilmId);
    indirizzo := FS_QueryFilm + cFilmId;                                       //2018.05.25
    cFilmTitle := textbetween(pageStr, FS_cStartTitleList, FS_cEndTitleList);       // Get Title
    HTMLRemoveTags(cFilmTitle);
    HTMLDecode(cFilmTitle);
    Delete(pageStr, 1, pos(FS_cEndTitleList, pageStr));   
    cYear := textbetween(pageStr, FS_cStartYearList, FS_cEndYearList);
    HTMLRemoveTags(cYear);
    cFilmOriginalTitle := textbetween(pageStr, FS_cStartOriginalTitleList, FS_cEndOriginalTitleList);
    HTMLRemoveTags(cFilmOriginalTitle);
    HTMLDecode(cFilmOriginalTitle);
    Delete(pageStr, 1, pos(FS_cEndOriginalTitleList, pageStr));
    cDirector := '<' + textbetween(pageStr, FS_cStartDirectorList, FS_cEndDirectorList);
    HTMLRemoveTags(cDirector);
    HTMLDecode(cDirector);
    if (indirizzo <> save_indirizzo) and (pos('#trailer', indirizzo) = 0) then
    begin
      if SharedPickTreeMatching(cFilmOriginalTitle, cFilmTitle, cYear, cDirector, '') then
        SharedSetMovieUrl(indirizzo)
      else  
        SharedPickTreeAdd(cFilmTitle, cFilmOriginalTitle, cDirector, cYear, indirizzo);
      save_indirizzo := indirizzo;
    end;     
    EndPos := pos(FS_cStartId, pageStr);
    Delete(pageStr, 1, EndPos - 1);
    StartPos := pos(FS_cStartId, pageStr);
  end;
  result := (pos('class="paginazioneA">Ultima</a>', SharedGetLatestPageHtml()) > 0);
  if (result AND (GetOption('Ricerca con Regista e Anno') = 1) AND (SharedGetYearDate() <> '') AND ((StrToInt(SharedGetYearDate, 0) - 5) > StrToInt(cYear, 0))) then
  // essendo questa una ricerca decrescente ordinata per anno, se ho impostato un anno di riferimento da tenere in considerazione durante la ricerca
  // e se l'anno relativo all'ultimo record trovato è più vecchio di 5 anni rispetto all'anno di riferimento allora non ha senso cercare oltre
    result := false;
  SharedPickTreeClose(result);
end;

end.
