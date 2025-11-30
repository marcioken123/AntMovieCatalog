unit MoviePlayerPas;

{
  Main var, constants and functions
}
uses
  ItalianSharedPas;

const
  MP_UnitVersion = 4;

  MP_UrlBase = 'https://movieplayer.it';

// --------------------------------------------------------------------------------------------------------------------------------------
//                                               "public" functions e procedures
// --------------------------------------------------------------------------------------------------------------------------------------

// -----------------------------------
// Main program (for MultiSite)
// IN: properties values (serialized)
// OUT: properties values (serialized)
// -----------------------------------
function MoviePlayerCoreMultiSite(serializedValues: string): string;
begin
  SharedUnserializeMe(serializedValues);
  // Analyze search page and get movie url (user choice)
  if (SharedGetMoviesFound() = 0) then
    MoviePlayerSearchResults();
  if (SharedGetMoviesFound() > 0) then // Analyze movie page and set movie fields
    MoviePlayerSetMovieFields();  
  result := SharedSerializeMe();    
end;


// --------------------------------
// ANALYZE FIRST SEARCH RESULT PAGE
// IN:  none
// --------------------------------
procedure MoviePlayerSearchResults;
var
  myUrl: string;
  pageNumberMovies, pageNumberSerie: integer;
  searchForMovies, searchForSerie: boolean;
begin
  // Looking for movies  
  pageNumberMovies := 0;
  pageNumberSerie := 0;
  searchForMovies := true;
  searchForSerie := true;
  repeat
    SharedSetMovieUrl('');
    SharedSetMoviesFound(0);
    SharedPickTreeCreate();  
    
    if (searchForMovies) then
    begin
      pageNumberMovies := pageNumberMovies + 1;    
      myUrl := 'https://movieplayer.it/ricerca/?q=' + UrlEncode(SharedGetMovieName()) + '&ct=film.film&pagina=' + IntToStr(pageNumberMovies) + '&querystring_key=pagina';
      SharedHTTPGetPage(myUrl);
      searchForMovies := MoviePlayerPopulatePickTree(false, 'movie', (not searchForSerie));
    end;
    if (searchForSerie AND (SharedGetMovieUrl() = '')) then
    begin
      // Looking for serietv
      pageNumberSerie := pageNumberSerie + 1;
      myUrl := 'https://movieplayer.it/ricerca/?q=' + SharedAdvancedUrlEncode(SharedGetMovieName(), false) + '&ct=serietv.serietv&pagina=' + IntToStr(pageNumberSerie) + '&querystring_key=pagina';
      SharedHTTPGetPage(myUrl);
      searchForSerie := MoviePlayerPopulatePickTree(searchForMovies, 'serie', true);  
    end;
    SharedPickTreeExec();
  until (not ((copy(SharedGetMovieUrl(), 1, 5) = 'next:') AND (searchForMovies OR searchForSerie)));  
end;


// -----------------------
// ANALYZE MOVIE DATA PAGE
// IN:  none
// -----------------------
procedure MoviePlayerSetMovieFields;
var
  html, originalTitle, translatedTitle, countries, picture, description, year, actors, directors, rating, duration, categories: string;
begin
  SharedHTTPGetPage(SharedGetMovieUrl());
  
  // tutti i valori si trovano in un json posizionato nell'head ad eccezione del titolo originale del film, del paese e del cast
  originalTitle := textBetween(SharedGetLatestPageHtml(), 'Titolo originale</dt>', '</dd>');
  HTMLRemoveTags(originalTitle);
  HTMLDecode(originalTitle);
  originalTitle := fulltrim(originalTitle);
  
  countries := textBetween(SharedGetLatestPageHtml(), '>Paese</dt>', '</dd>');
  HTMLRemoveTags(countries);
  HTMLDecode(countries);
  countries := fulltrim(countries);
  
  actors := htmlSerializePeople(textBetween(SharedGetLatestPageHtml(), '<section class="section-scheda section-scheda--cast">', '</section>'));
  
  // tutti gli altri campi li trovo in questo json
  html := textBetween(SharedGetLatestPageHtml(), '<script type="application/ld+json">', '</script>');
  HTMLDecode(html);
  
  if (pos('"@type": "TVSeason"', html) > 0) then
  begin
    translatedTitle := textBetween(SharedGetLatestPageHtml(), '<meta property="og:title" content="', '"');
  end
  else
  begin
    translatedTitle := textBetween(html, '"name": "', '"');
    translatedTitle := stringReplace(translatedTitle, ' (FILM TV)', '');  
  end;
  description := textBetween(html, '"description": "', '"');  
  picture := textBetween(html, '"image": "', '"');  
  year := textBetween(html, '"startDate": "', '-');  
  directors := jsonSerializePeople(textBetween(html, '"director": [', ']'));  
  rating := textBetween(html, '"ratingValue": ', '}');
  rating := fulltrim(rating);  
  duration := textBetween(html, '"duration": "PT', 'M"');  
  categories := textBetween(html, '"genre": "', '"');
  
  SharedSecureSetAllFields(originalTitle, translatedTitle, rating, year, '', '', '', directors, actors, duration, categories, '', countries, description, '', SharedGetMovieUrl(), picture);
end;


// --------------------------------------------------------------------------------------------------------------------------------------
//                                               "private" functions e procedures
// --------------------------------------------------------------------------------------------------------------------------------------


function jsonSerializePeople(peopleJson: string): string;
var
  jsonPersonBlock: string;
  posTag: integer;
begin
  posTag := pos('{', peopleJson);
  result := '';
  while (posTag > 0) do
  begin
    Delete(peopleJson, 1, posTag);
    posTag := pos('{', peopleJson);
    if (posTag > 0) then
      jsonPersonBlock := copy(peopleJson, 1, posTag)
    else
      jsonPersonBlock := peopleJson;  
    result := result + textBetween(peopleJson, '"name": "', '"') + ', ';
  end;
  if (Length(result) > 0) then
    result := copy(result, 1, Length(result) - 2);
end;


function htmlSerializePeople(peoplehtml: string): string;
var
  htmlPersonBlock, asName: string;
  posTag: integer;
begin
  posTag := pos('<tr>', peoplehtml);
  result := '';
  while (posTag > 0) do
  begin
    Delete(peoplehtml, 1, posTag);
    posTag := pos('<tr>', peoplehtml);
    if (posTag > 0) then
      htmlPersonBlock := copy(peoplehtml, 1, posTag)
    else
      htmlPersonBlock := peoplehtml;  
    result := result + textBetween(peoplehtml, '<span class="name">', '</span>');
    asName := textBetween(peoplehtml, '<span class="character font-italic">', '</span>');
    if (asName <> '') then
      result := result + ' (as ' + asName + ')';
    result := result + ', ';  
  end;
  if (Length(result) > 0) then
    result := copy(result, 1, Length(result) - 2);
end;

// -----------------------------------------
// FILL PICKTREE CONTROL WITH LINKS & TITLES
// IN:  none
// OUT: if a next page available exists
// -----------------------------------------
function MoviePlayerPopulatePickTree(paramNextPageAvailable: boolean; movieType: string; lastResearch: boolean): boolean;
var
  html, htmlMovieBlock, movieUrl, movieTitle, movieYear, movieDirectors, tagMovieBlock: string;
  posTag: integer;
begin
  html := SharedGetLatestPageHtml();
  result := (pos('endless_container', html) > 0);
  tagMovieBlock := '<li class="search-entry search-result-entry"';
  posTag := pos(tagMovieBlock, html);
  // ciclo nel codice html della pagina
  while (posTag > 0) do
  begin
    // rimuovo l'html fino al tag iniziale che indivua il blocco delle info che cerco
    Delete(html, 1, posTag);
    
    // seleziono solo l'html di un risultato, ovvero quello che va fino al prossimo tagMovieBlock oppure, nel caso dell'ultimo risultato, fino a fine pagina
    posTag := pos(tagMovieBlock, html);
    if (posTag > 0) then
      htmlMovieBlock := copy(html, 1, posTag)
    else
      htmlMovieBlock := html;
      
    // elimino del codice che mi complica il parser
    htmlMovieBlock := StringReplace(htmlMovieBlock, '<span class="search-entry-spacer search-hidden-xs">|</span>', '');
    
    // salvo gli altri dati del film
    movieUrl := textBetween(htmlMovieBlock, '<a href="', '">');
    
    movieTitle := textBetween(htmlMovieBlock, '<a href="' + movieUrl + '">', '</a>');
    HTMLDecode(movieTitle);
    
    movieYear := textBetween(htmlMovieBlock, '<span class="search-entry-date search-hidden-xs">', '</span>');
    movieYear := fulltrim(movieYear);
    if (Length(movieYear) > 0) then
      movieYear := copy(movieYear, Length(movieYear) - 3, 4);
    
    Delete(htmlMovieBlock, 1 , pos('search-result-abstract', htmlMovieBlock));
    movieDirectors := textBetween(htmlMovieBlock, 'Di ', '.');
    HTMLDecode(movieDirectors);
    
    movieUrl := MP_UrlBase + movieUrl;

    if SharedPickTreeMatching(movieTitle, '', movieYear, movieDirectors, '') then
      SharedSetMovieUrl(movieUrl)
    else  
      SharedPickTreeAdd(movieTitle + ' [' + movieType + ']', '', movieDirectors, movieYear, movieUrl);   
      
  end;
  if (lastResearch) then
    SharedPickTreeClose((paramNextPageAvailable OR result));
end;

end.
