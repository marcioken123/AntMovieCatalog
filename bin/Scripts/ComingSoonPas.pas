unit ComingsoonPas;

{
  Main var, constants and functions
}
uses
  ItalianSharedPas;

const
  CS_UnitVersion = 6;
  
  CS_UrlBase = 'https://www.comingsoon.it';
  
// --------------------------------------------------------------------------------------------------------------------------------------
//                                               "public" functions e procedures
// --------------------------------------------------------------------------------------------------------------------------------------

// -----------------------------------
// Main program (for MultiSite)
// IN: properties values (serialized)
// OUT: properties values (serialized)
// -----------------------------------
function ComingSoonCoreMultiSite(serializedValues: string): string;
begin
  SharedUnserializeMe(serializedValues);
  // Analyze search page and get movie url (user choice)
  if (SharedGetMoviesFound() = 0) then
    ComingSoonSearchResults();
  if (SharedGetMoviesFound() > 0) then // Analyze movie page and set movie fields
    ComingSoonSetMovieFields();  
  result := SharedSerializeMe();    
end;

// --------------------------------
// ANALYZE FIRST SEARCH RESULT PAGE
// IN:  none
// --------------------------------
procedure ComingSoonSearchResults();
var
  nextUrl: string;
  pageNumberMovies, pageNumberSerie: integer;  
  nextPageAvailable: boolean;
  searchForMovies, searchForSerie, explodeSerieLink: boolean;  
begin   
  // loop until user select next page in pick tree   
  pageNumberMovies := 0;
  pageNumberSerie := 0; 
  searchForMovies := true;
  searchForSerie := true;  
  repeat
    // explodeSerieLink è true se SharedGetMovieUrl() è del tipo: next:https://www.comingsoon.it/serietv/the-walking-dead/112/scheda/
    // ovvero se l'utente ha selezionato una scheda (nella ricerca interna a ComingSoon) per poter scegliere la stazione
    explodeSerieLink := ((copy(SharedGetMovieUrl(), 1, 5) = 'next:') AND (SharedGetMovieUrl() <> 'next:'));
    if (explodeSerieLink) then
      nextUrl := SharedGetMovieUrl();
      
    SharedSetMovieUrl('');
    SharedSetMoviesFound(0);
    SharedPickTreeCreate();
        
    if (not explodeSerieLink) then
    begin
      // effettuo una ricerca usando il motore interno del sito, cercando sia tra i film sia tre le serie      
      if (searchForMovies) then
      begin
        // cerco prima tra i film ...
        pageNumberMovies := pageNumberMovies + 1;    
        nextUrl := 'https://www.comingsoon.it/film/?titolo=' + UrlEncode(SharedGetMovieName()) + '&orderby=TITOLO';
        if (pageNumberSerie > 1) then
          nextUrl := nextUrl + '&page=' + IntToStr(pageNumberSerie);        
        SharedHTTPGetPage(nextUrl);
        searchForMovies := ComingSoonPopulatePickTreeInternal(false, 'movie', (not searchForSerie));
      end;
      if (searchForSerie AND (SharedGetMovieUrl() = '')) then
      begin
        // ... e, se non ho trovato il film sicuramente corrispondente, allora cerco ANCHE tra le serie
        pageNumberSerie := pageNumberSerie + 1;
        nextUrl := 'https://www.comingsoon.it/serietv/ricerca/?titolo=' + SharedAdvancedUrlEncode(SharedGetMovieName(), false) + '&orderby=titolo';
        if (pageNumberSerie > 1) then
          nextUrl := nextUrl + '&page=' + IntToStr(pageNumberSerie);
        SharedHTTPGetPage(nextUrl);
        searchForSerie := ComingSoonPopulatePickTreeInternal(searchForMovies, 'serie', true);  
      end;    
      // se ho ricevuto una pagina vuota in risposta avviso l'utente che deve modificare lo user agent
      if (SharedGetLatestPageHtml() = '') then
        sharedShowMessage('Timeout durante l''accesso a Coming Soon.' + CRLF + 'Verifica che nel file della lingua selezionata' + CRLF + '(ad esempio: "C:\Program Files (x86)\Ant Movie Catalog\Languages\Italian.lng"),' + CRLF + 'sotto la riga "[GetScriptWin]", ci sia questa riga:' + CRLF + 'http.Request.UserAgent=Mozilla/5.0 (Windows NT 6.3; Win64; x64; rv:77.0) Gecko/20100101 Firefox/77.0' + CRLF + 'Se non ci fosse, aggiungila e poi riavvia Ant Movie Catalog');
    end
    else
    begin
      // questo è il caso in cui l'utente, a seguito di una ricerca interna al sito di ComingSoon, ha selezionato
      // la voce per visualizzare tutti gli episodi di una serie. La nextUrl è del tipo next:https://www.comingsoon.it/...
      nextUrl := copy(nextUrl, 6, (Length(nextUrl) - 5)); // rimuovo il prefisso next:         
      SharedHTTPGetPage(nextUrl);
      ComingSoonPopulatePickTreeEpisodes();
    end;
    nextUrl := '';         
    
    if (SharedGetMovieUrl() = '') then  
      if ((SharedGetMoviesFound() = 0) AND (nextUrl <> '')) then 
        // se non ho trovato nessun risultato in questa pagina (perché erano risultati non pertinenti)
        // E se posso andare avanti ALLORA ci vado
        SharedSetMovieUrl('next:')   
      else       
        SharedPickTreeExec();  
  until (not ((copy(SharedGetMovieUrl(), 1, 5) = 'next:') AND (searchForMovies OR searchForSerie)));
end;


// -----------------------
// ANALYZE MOVIE DATA PAGE
// IN:  none
// -----------------------
procedure ComingSoonSetMovieFields;
begin
  if not ComingSoonIsUrlValid(SharedGetMovieUrl()) then
  begin
    SharedSetMoviesFound(0);
    SharedSetMovieUrl('');
    exit;
  end; 
  
  SharedHTTPGetPage(SharedGetMovieUrl());
  if pos('/film/', SharedGetMovieUrl()) > 1 then
    ComingSoonExtractMovie()
  else 
  if pos('/episodi', SharedGetMovieUrl()) > 1 then
    ComingSoonExtractSeason()
  else // è sicuramente una serie con /scheda nella url
    ComingSoonExtractSerie();      
end;

// --------------------------------------------------------------------------------------------------------------------------------------
//                                               "private" functions e procedures
// --------------------------------------------------------------------------------------------------------------------------------------



// -------------------------------------
// Verify if the url is valid
// IN: url to check
// OUT: true (valid) | false (not valid)
// -------------------------------------
function ComingSoonIsUrlValid(myUrl: string):boolean;
begin
  result := (pos('/anticipazioni/', myUrl) = 0)
        and (pos('/foto/', myUrl) = 0)
        and (pos('/games/', myUrl) = 0)
        and (pos('/gossip/', myUrl) = 0)
        and (pos('/news/', myUrl) = 0)
        and (pos('/personaggi/', myUrl) = 0)
        and (pos('/speciali/', myUrl) = 0)
        and (pos('/sport/', myUrl) = 0)
        and (pos('/streaming/', myUrl) = 0)
        and (pos('/video/', myUrl) = 0)
        and not (((pos('/film/', myUrl) < 2) AND (pos('/scheda', myUrl) < 2) AND (pos('/episodi', myUrl) < 2))
      OR ((pos('/film/', myUrl) > 1) AND (AnsiLastPosEx('/scheda', myUrl, true, true) = 0)));
end;

//--------------------------------------------------------------------
// ESTRAE Attori DA SCHEDA Film (presente solamente in quella pagina)
//--------------------------------------------------------------------
function ComingSoonExtractCast(paramValue: string): string;
var
  MyLabel: string;
begin
   MyLabel := TextBetween(paramValue, '<b>Attori:</b>', '</div>');           //fs fin qui i dati ci sono
   MyLabel := TextBetween(MyLabel, '<span>', '</span>');           //fs fin qui i dati ci sono
   HTMLRemoveTags(MyLabel);
   if (MyLabel = '') then // nuovo loop attori
      MyLabel := TextBetween(paramValue, '<!-- SLIDER HOME 7-10 -->', '<!-- /FINE SLIDER HOME 7-10 -->');           //fs fin qui i dati ci sono
   result := MyLabel;
end;

//--------------------------------------------------------------------
// ESTRAE Attori DA SCHEDA Film (presente solamente in quella pagina)
//--------------------------------------------------------------------
function ComingSoonExtractPicture(paramValue: string): string;
begin
  result := TextBetween(paramValue, '<link rel="image_src" href="', '">'); 
  if ((pos('def_ico.gif', result) > 0) OR (pos('imgdb/locandine/ico/def.jpg', result) > 0)) then
    result := '';
end;

//-------------------------------------------------------------------
// ESTRAE LE INFORMAZIONI RESTANTI E LE SALVA NELLA SCHEDA
// IN:
// - titleSuffix, una stringa con informazioni sul numero di stagione
// - HtmlPage: HTML della pagina da esaminare
// - url: url da salvare in fieldUrl
// - description: contenuto da salvare in fieldDescription
// - comments: contenuto da salvare in fieldComments
// - picture: url della locandina da salvare
//-------------------------------------------------------------------
procedure ComingSoonCommonData(titleSuffix, HtmlPage, url, description, comments, picture: string);
var
  originalTitle, translatedTitle, rating, year, producers, composers, directors, writers, actors, duration, categories, certification, countries: string;
  HtmlPart: string;
begin
  HtmlPart := textbetween(HtmlPage, '<h1 class="titolo h1', '/h1>') + titleSuffix;
  if length(HtmlPart) > 0 then
  begin
    HtmlPart := textbetween(HtmlPart, '>', '<');  
    HtmlPart := AnsiMixedCase(AnsiLowerCase(HtmlPart), ' ');
    HtmlPart := stringReplace(HtmlPart, '/', '');  
  end;
  translatedTitle := HtmlPart;

  HtmlPart := textbetween(HtmlPage, '<div class="sottotitolo h3">( ', ' )');
  HtmlPart := AnsiMixedCase(AnsiLowerCase(HtmlPart), ' ');
  if ((pos('/serietv', SharedGetLatestPageUrl()) > 0) AND (titleSuffix = '')) then
    HtmlPart := HtmlPart + ' (Serie TV)';
  originalTitle := HtmlPart;
      
  HtmlPart := TextBetween(HtmlPage, '<b>Genere:</b>', '</div>');
  HTMLRemoveTags(HtmlPart);
  categories := FormatText(HtmlPart);

  HtmlPart := TextBetween(HtmlPage, '<b>Regia:</b>', '</div>');
  HTMLRemoveTags(HtmlPart);
  directors := FormatText(HtmlPart);

  actors := ComingSoonExtractCast(HtmlPage);

  HtmlPart := TextBetween(HtmlPage, '<b>Anno:</b>', '</div>');
  HTMLRemoveTags(HtmlPart);
  year := FormatText(HtmlPart);

  HtmlPart := TextBetween(HtmlPage, '<b>Paese:</b>', '</div>');
  HTMLRemoveTags(HtmlPart);
  countries := FormatText(HtmlPart);

  HtmlPart := TextBetween(HtmlPage, '<b>Produzione:</b>', '</div>');
  HTMLRemoveTags(HtmlPart);
  producers := FormatText(HtmlPart);

  HtmlPart := TextBetween(HtmlPage, '<b>Durata:</b>', 'min</span></div>');
  HTMLRemoveTags(HtmlPart);
  duration := FormatText(HtmlPart);
   
  HtmlPart := TextBetween(HtmlPage, 'Musiche:', '</div>');
  HTMLRemoveTags(HtmlPart);
  composers := stringReplace(FormatText(HtmlPart), CRLF, '');

  HtmlPart := TextBetween(HtmlPage, '<li><span>Sceneggiatura:</span>: ', '</div>');
  HTMLRemoveTags(HtmlPart);
  HtmlPart := stringReplace(FormatText(HtmlPart), CRLF, '');
  if (HtmlPart = '') then
  begin
    HtmlPart := TextBetween(HtmlPage, '<b>Ideatore:</b>', '</div>');
    HTMLRemoveTags(HtmlPart);
    HtmlPart := stringReplace(FormatText(HtmlPart), CRLF, '');
  end;
  writers := HtmlPart;
   
  SharedSecureSetAllFields(originalTitle, translatedTitle, '', year, producers, writers, composers, directors, actors, duration, categories, '', countries, description, comments, url, picture);
end;

//-----------------------------------
// ESTRAE INFORMAZIONI DA SCHEDA Film
//-----------------------------------
procedure ComingSoonExtractMovie;
var
  HtmlPart, HtmlPage, description, comments, picture: string;
begin
  HtmlPage := SharedGetLatestPageHtml();
  HTMLdecode(HtmlPage); 
   
// picture
  picture := ComingSoonExtractPicture(HtmlPage);
   
// description
  HtmlPart := textbetween(HtmlPage, '<span id="trama-', '</div>') + '</div>';
  if (pos('</h2>', HtmlPart) > 0) then
    HtmlPart := textbetween(HtmlPart, '</h2>', '</div>')
  else  
    HtmlPart := textbetween(HtmlPart, '</span>', '</div>');
  HTMLRemoveTags(HtmlPart);
  if (pos('Guardalo subito su ', HtmlPart) > 0) then
    HtmlPart := copy(HtmlPart, 1, pos('Guardalo subito su ', HtmlPart) - 2);  
  HtmlPart := FormatText(HtmlPart);    
  HtmlPart := stringReplace(HtmlPart, (CRLF + ' '), CRLF);
  HtmlPart := fulltrim(HtmlPart);
  // talvolta la descrizione è compresa tra il testo TRAMA BREVE e TRAMA LUNGA
  description := textBetween(HtmlPart, 'TRAMA BREVE', 'TRAMA LUNGA');
  if (description = '') then
    description := HtmlPart;

// comments
  HtmlPart := textbetween(HtmlPage, '<span id="curiosita-su-', '</div>') + '</div>';
  if (pos('</h2>', HtmlPart) > 0) then
    HtmlPart := textbetween(HtmlPart, '</h2>', '</div>')
  else  
    HtmlPart := textbetween(HtmlPart, '</span>', '</div>');
  HTMLRemoveTags(HtmlPart);
  if (pos('Leggi la recensione completa d', HtmlPart) > 0) then
    HtmlPart := copy(HtmlPart, 1, pos('Leggi la recensione completa d', HtmlPart) - 2);
  HtmlPart := fulltrim(FormatText(HtmlPart));
  comments := HtmlPart;
   
  ComingSoonCommonData('', HtmlPage, SharedGetLatestPageUrl(), description, comments, picture); 
end;

//------------------------------------------------------------------------------
// ESTRAE INFORMAZIONI DA SCHEDA + EPISODI SE E' UNA SERIE
//------------------------------------------------------------------------------
procedure ComingSoonExtractSerie();
var
  HtmlPart, HtmlPage, description, comments, picture: string;

begin
  HtmlPage := SharedGetLatestPageHtml();
  HTMLdecode(HtmlPage);
  HtmlPage := stringReplace(HtmlPage, UTF8Decode('>CURIOSITÀ:<'), UTF8Decode('>Curiosità:<'));
  
// picture
  picture := ComingSoonExtractPicture(HtmlPage);  

// description
  HtmlPart := TextBetween(HtmlPage, '<!-- Trama -->', UTF8Decode('<!-- Curiosità -->'));
  HtmlPart := TextBetween(HtmlPart, '<div>', '</div>');
  HtmlPart := TextBetween(HtmlPart, '<p>', '</p>');
  description := HtmlPart;
  HtmlPart := TextBetween(HtmlPage, UTF8Decode('<!-- Curiosità -->'), '<!-- voci full -->') + '<!-- voci full -->';
  HtmlPart := TextBetween(HtmlPart, '<h2 class="anchor">', '<!-- voci full -->');
  HtmlPart := TextBetween(HtmlPart, '<p>', '</p>');
  description := description + CRLF + HtmlPart;  
  HTMLRemoveTags(description);
  description := fulltrim(description);
   
// comments
  HtmlPart := '<span id="curiosita' + TextBetween(HtmlPage, '<h2 class="h2 anchor"><span id="curiosita', '</div>');
  HTMLRemoveTags(HtmlPart);
  HtmlPart := FormatText(HtmlPart);
  comments := stringReplace(HtmlPart, (CRLF + ' '), CRLF);

  ComingSoonCommonData('', HtmlPage, SharedGetLatestPageUrl(), description, comments, picture);
end;

//-----------------------------------------------------------------
// ESTRAE INFORMAZIONI DA EPISODI DI UNA SERIE + SCHEDA DELLA SERIE
//-----------------------------------------------------------------
Procedure ComingSoonExtractSeason();
var
  HtmlPage, HtmlPart, description, episodeLength, comments, SeasonText, url, picture: string;
Begin
  SeasonText := ' (' + textafter(SharedGetLatestPageUrl(), '/episodi/') + ')';
  
  HtmlPage := SharedGetLatestPageHtml();
  url := SharedGetLatestPageUrl();
  HTMLdecode(HtmlPage);
  HtmlPage := stringReplace(HtmlPage, '<BLOCKQUOTE', '<blockquote');  
  
// picture
  picture := ComingSoonExtractPicture(HtmlPage);    
  
// description  
  HtmlPart := TextBetween(HtmlPage, '<title>', '</title>');
  HtmlPart := stringReplace(HtmlPart, ' Episodi ', ' (') + ')';
  HtmlPart := 'Lista episodi' + TextBetween(HtmlPage, '<h2 class="h2">Lista episodi', '<!-- /LISTA EPISODI SERIE TV  -->');
  SharedDebugAppendToFile(HtmlPart, '[ComingSoonPas][ComingSoonExtractSeason] episodes Extr_original');
  HTMLRemoveTags(HtmlPart);
  //fs2019-10-14   HtmlPart := stringReplace(HtmlPart, ' min', (' min' + CRLF) );                  //fs2019-10-14
  SharedDebugAppendToFile(HtmlPart, '[ComingSoonPas][ComingSoonExtractSeason] episodes Extr_original decoded');
  HtmlPart := FormatText(HtmlPart);
  SharedDebugAppendToFile(HtmlPart, '[ComingSoonPas][ComingSoonExtractSeason] episodes Extr_original decoded formatted');
  HtmlPart := StringReplace(HtmlPart, (' ' + CRLF), ' ');
  HtmlPart := StringReplace(HtmlPart, ('  '), CRLF);
  HtmlPart := StringReplace(HtmlPart, (CRLF + CRLF + CRLF), CRLF);
  HtmlPart := StringReplace(HtmlPart, (CRLF + CRLF + CRLF), CRLF);
  HtmlPart := StringReplace(HtmlPart, (CRLF + CRLF), CRLF);
  HtmlPart := StringReplace(HtmlPart, (CRLF + ' '), CRLF);
  HtmlPart := StringReplace(HtmlPart, (CRLF + 'Regia:'), '      Regia:');
  HtmlPart := StringReplace(HtmlPart, (CRLF + 'Sceneggiatura:'), '      Sceneggiatura:');
  HtmlPart := StringReplace(HtmlPart, (CRLF + 'Durata:'), '     Durata:');
  HtmlPart := HtmlPart + CRLF;
  
  // elimino la durata dagli episodi
  episodeLength:= '     Durata:' + TextBetween(HtmlPart, '     Durata:', CRLF) + CRLF;
  while length(episodeLength) > (length('     Durata:') + length(CRLF)) do
  begin
    episodeLength:= '     Durata:' + TextBetween(HtmlPart, '     Durata:', CRLF) + CRLF;
    HtmlPart := stringReplace(HtmlPart, episodeLength, CRLF);
  end;

  description := textBetween(HtmlPage, '<!-- Trama -->', '</p>');
  HTMLRemoveTags(description);
  description := fulltrim(description);
  description := fulltrim(description + CRLF + CRLF + CRLF + HtmlPart);

  // aggiungo commenti per la serie 
  HtmlPart := TextBetween(HtmlPage, '<blockquote>', '</blockquote>');
  HtmlPart := stringReplace(HtmlPart, UTF8Decode('>CURIOSITÀ<'), UTF8Decode('>Curiosità<'));
  HtmlPart := TextAfter(HtmlPart, UTF8Decode('<div class="h3">Curiosità</div>'));
  HTMLRemoveTags(HtmlPart);
  HtmlPart := FormatText(HtmlPart);
  HtmlPart := stringReplace(HtmlPart, CRLF, '');
  description := fullTrim(description + CRLF + CRLF + HtmlPart);

//comments
  // !! IMPORTANTE !!
  // Dopo aver ricavato tutte le informazioni per description e per la locandina
  // cambio URL e passo alla URL della scheda principale: recupero la descrizione e la salvo nel campo comments
  SharedHTTPGetPage(textbefore(SharedGetLatestPageUrl(), '/episodi/', '') + '/scheda/');
  HtmlPage := SharedGetLatestPageHtml();
  HTMLdecode(HtmlPage);
  comments := Textbetween(HtmlPage, '<!-- Trama -->', '</div>');
  comments := TextAfter(comments, '</h2>');
  HTMLRemoveTags(comments);
  comments := fullTrim(comments);

// l'html da esaminare è quello della scheda principale (l'ultima url aperta qui sopra), mentre i campi
// url, description, comments (che ricavo qui sotto) e picture sono quelli ricavati in questa procedure
  ComingSoonCommonData(SeasonText, HtmlPage, url, description, comments, picture);
end;
// ********************* fine estrazione episodi *****************************************



// -----------------------------------------
// FILL PICKTREE CONTROL WITH LINKS & TITLES
// IN:  none
// OUT: if a next page available exists
// -----------------------------------------
function ComingSoonPopulatePickTreeInternal(paramNextPageAvailable: boolean; movieType: string; lastResearch: boolean): boolean;
var
  html, htmlMovieBlock, movieUrl, movieTitle, movieYear, movieDirectors, tagMovieBlock: string;
  posTag: integer;
begin
  html := SharedGetLatestPageHtml();
  result := (pos('<i class=''fa fa-forward''></i></a></li><li class=''page-item disabled''>', html) = 0);
  // tagMovieBlock è il mio "marker" che indivua il blocco delle info che cerco relative ad un risultato della ricerca
  tagMovieBlock := '<div class="testo plr0">';
  posTag := pos(tagMovieBlock, html);
  // ciclo nel codice html della pagina
  while (posTag > 0) do
  begin
    // la movieUrl si trova prima del marker, la recupero
    movieUrl := TextBefore(html, tagMovieBlock, 'href=');
    movieUrl := TextBetween(movieUrl, '"', '"');
    movieUrl := CS_UrlBase + movieUrl;
  
    // rimuovo l'html fino al marker
    Delete(html, 1, posTag);
    
    // seleziono solo l'html di un risultato, ovvero quello che va fino al prossimo tagMovieBlock oppure, nel caso dell'ultimo risultato, fino a fine pagina
    posTag := pos(tagMovieBlock, html);
    if (posTag > 0) then
      htmlMovieBlock := copy(html, 1, posTag)
    else
      htmlMovieBlock := html;
         
    // salvo gli altri dati del film
    // il movieTitle può essere incluso tra <div class="titolo h3"></div> oppure <div class="titolo h3 lrg"></div>
    movieTitle := textBetween(htmlMovieBlock, '<div class="titolo h3', '/div>');
    movieTitle := textBetween(movieTitle, '>', '<');
    HTMLDecode(movieTitle);
    
    movieYear := textBetween(htmlMovieBlock, 'Anno:', '</span>');
    HTMLRemoveTags(movieYear);
    movieYear := fullTrim(movieYear);
    
    movieDirectors := textBetween(htmlMovieBlock, 'Regia:', '</span>');
    HTMLRemoveTags(movieDirectors);
    movieDirectors := fullTrim(movieDirectors);
   
    if SharedPickTreeMatching(movieTitle, '', movieYear, movieDirectors, '') then
      SharedSetMovieUrl(movieUrl)
    else  
    begin
      SharedPickTreeAdd(movieTitle + ' [' + movieType + ']', '', movieDirectors, movieYear, movieUrl);   
      if (movieType = 'serie') then
        SharedPickTreeAdd(movieTitle + ' [' + movieType + '] mostra episodi -->', '', movieDirectors, movieYear, 'next:' + movieUrl);
    end;  
      
  end;
  if (lastResearch) then
    SharedPickTreeClose((paramNextPageAvailable OR result));
end;

// ----------------------------------------------------------------------
// FILL PICKTREE CONTROL WITH LINKS & TITLES (ONLY SEASONS FOR 1 SERIETV)
// IN:  none
// ----------------------------------------------------------------------
procedure ComingSoonPopulatePickTreeEpisodes();
var
  html, htmlMovieBlock, movieUrl, movieTitle, movieYear, movieDirectors, tagMovieBlock: string;
  posTag: integer;
begin
  html := SharedGetLatestPageHtml();
  html := textBetween(html, '<div class="row serie-tv">', '</section>');
  // tagMovieBlock è il mio "marker" che indivua il blocco delle info che cerco relative ad un risultato della ricerca
  tagMovieBlock := '<a ';
  posTag := pos(tagMovieBlock, html);
  // ciclo nel codice html della pagina
  while (posTag > 0) do
  begin
    // rimuovo l'html fino al marker
    Delete(html, 1, posTag);
      
    // seleziono solo l'html di un risultato, ovvero quello che va fino al prossimo tagMovieBlock oppure, nel caso dell'ultimo risultato, fino a fine pagina
    posTag := pos(tagMovieBlock, html);
    if (posTag > 0) then
      htmlMovieBlock := copy(html, 1, posTag)
    else
      htmlMovieBlock := html;
         
    // salvo i dati degli episodi
    movieUrl := TextBetween(htmlMovieBlock, 'href="', '"');
    movieUrl := CS_UrlBase + movieUrl;
    
    movieTitle := textBetween(htmlMovieBlock, 'title="', '"');
    HTMLDecode(movieTitle);
    
    movieYear := textBetween(htmlMovieBlock, 'Anno:', '</div>');
    HTMLRemoveTags(movieYear);
    movieYear := fullTrim(movieYear);
    
    movieDirectors := textBetween(htmlMovieBlock, 'Regia:', '</div>');
    HTMLRemoveTags(movieDirectors);
    movieDirectors := fullTrim(movieDirectors);
    if SharedPickTreeMatching(movieTitle, '', movieYear, movieDirectors, '') then
      SharedSetMovieUrl(movieUrl)
    else  
      SharedPickTreeAdd(movieTitle + ' [serie]', '', movieDirectors, movieYear, movieUrl);      
  end;
  SharedPickTreeClose(false);
end;

end.
