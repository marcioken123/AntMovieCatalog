unit NientePopcornPas;

{
  Main var, constants and functions
}
uses
  ItalianSharedPas;

const
  NP_UnitVersion = 3;

  NP_UrlBase = 'https://www.nientepopcorn.it';
  NP_UrlSearch = NP_UrlBase + '/cerca-un-film/?titolo=';



// --------------------------------------------------------------------------------------------------------------------------------------
//                                               "public" functions e procedures
// --------------------------------------------------------------------------------------------------------------------------------------

// -----------------------------------
// Main program (for MultiSite)
// IN: properties values (serialized)
// OUT: properties values (serialized)
// -----------------------------------
function NientePopcornCoreMultiSite(serializedValues: string): string;
begin
  SharedUnserializeMe(serializedValues);
  // Analyze search page and get movie url (user choice)
  if (SharedGetMoviesFound() = 0) then
    NientePopcornSearchResults();
  if (SharedGetMoviesFound() > 0) then // Analyze movie page and set movie fields
    NientePopcornSetMovieFields();  
  result := SharedSerializeMe();
end;


// --------------------------------
// ANALYZE FIRST SEARCH RESULT PAGE
// IN:  none
// --------------------------------
procedure NientePopcornSearchResults();
var
  myUrl, myUrlFirstPart: string;  
  pageNumberMovies: integer;  
begin
  pageNumberMovies := 0;
  myUrlFirstPart := NP_UrlSearch + UrlEncode(SharedGetMovieName());
  myUrlFirstPart := myUrlFirstPart + '&pagina=';

  repeat
    SharedSetMovieUrl('');
    SharedSetMoviesFound(0);
    pageNumberMovies := pageNumberMovies + 1;
    myUrl := myUrlFirstPart + IntToStr(pageNumberMovies);  
    SharedHTTPGetPage(myUrl);        
    NientePopcornPopulatePickTree();
    if (SharedGetMovieUrl() = '') then  
      SharedPickTreeExec();
  until (copy(SharedGetMovieUrl(), 1, 5) <> 'next:');
end;


// -----------------------
// ANALYZE MOVIE DATA PAGE
// IN:  none
// -----------------------
procedure NientePopcornSetMovieFields;
begin
  NientePopcornRetriveMovieData(SharedGetMovieUrl(), true);
end;


// --------------------------------------------------------------------------------------------------------------------------------------
//                                               "private" functions e procedures
// --------------------------------------------------------------------------------------------------------------------------------------


function NientePopcornTextBetween(str, delimiterA, delimiterB, delimeterB2: string): string;
var s: string;
begin
  s := TextBetween(SharedGetLatestPageHtml(), delimiterA, delimiterB);
  if (delimeterB2 <> '') then
    if (pos(delimeterB2, s) > 0) then
      s := copy(s, 1, pos(delimeterB2, s));
  HTMLRemoveTags(s);    
  HTMLDecode(s);
  s := fulltrim(s);
  // rimuove eventuali , finali, comunemente lasciate per il campo attori
  if (copy(s, length(s), 1) = ',') then
    s := copy(s, 1, (length(s) - 1));
  result := fulltrim(s);
end;

procedure NientePopcornRetriveMovieData(myUrl:string; fulldata: boolean);
var
  originalTitle, translatedTitle, rating, year, producers, writers, composers, directors, actors, duration, categories, certification, countries, description, comments, picture: string;
  pos_func, PosCast: integer;
  SaveField, DelField, Json, TempField, director, actor: string;
begin
  SharedHTTPGetPage(myUrl);

  originalTitle := NientePopcornTextBetween(SharedGetLatestPageHtml(), '<strong>Titolo Originale</strong>: ', '<br>', '');  
  translatedTitle := NientePopcornTextBetween(SharedGetLatestPageHtml(), '<span class="scheda-film_titolo">', '</span>', '');  
  // se il titolo originale è in un charset che non riesco a tradurre correttamente (ovvero composto solo da ?), uso il titolo tradotto
  if (fulltrim(StringReplace(originalTitle, '?', '')) = '') then
    originalTitle := translatedTitle;
  year := NientePopcornTextBetween(SharedGetLatestPageHtml(), '<span  class="film_anno">/ ', '</span>', '');    
  directors := NientePopcornTextBetween(SharedGetLatestPageHtml(), '<strong>Regia</strong>: ', '<br>', '</p>');
  if (not fulldata) then
    // prendo solo il cognome per il matching con SharedPickTreeMatching
    while (pos(' ', directors) > 0) do
      Delete(directors, 1, pos(' ', directors));       

  if (fulldata) then
  begin 
    // la trama potrebbe contenere elementi del tipo <i style="font-size:11px;"> oppure <div pertanto prendo il testo fino a <
    // in futuro questo potrebbe costituire un problema in presenza di tag, ad oggi non presenti
    description := NientePopcornTextBetween(SharedGetLatestPageHtml(), '<p class="trama">', '<', ''); 
    // trama ancora da compilare
    if (pos('su nientepopcorn il Social Network sul Cinema', description) > 0) then
      description := '';

    categories := NientePopcornTextBetween(SharedGetLatestPageHtml(), '<strong>Genere</strong>: ', '<br>', '</p>');    
    actors := NientePopcornTextBetween(SharedGetLatestPageHtml(), '<strong>Attori principali</strong>: ', '<br>', 'Mostra tutti');    
    countries := NientePopcornTextBetween(SharedGetLatestPageHtml(), '<strong>Produzione</strong>: ', '<br>', '</p>');
    producers := NientePopcornTextBetween(SharedGetLatestPageHtml(), '<strong>Produttore</strong>: ', '<br>', '</p>');
    writers := NientePopcornTextBetween(SharedGetLatestPageHtml(), '<strong>Sceneggiatura/Autore</strong>: ', '<br>', '</p>');
    composers := NientePopcornTextBetween(SharedGetLatestPageHtml(), '<strong>Colonna sonora</strong>: ', '<br>', '</p>');
    picture := TextBetween(SharedGetLatestPageHtml(), '<div class="locandina_sf" ', '</div>');
    picture := TextBetween(picture, 'src="', '"');          
    comments := '';
    
    // dati presenti solo nel json iniziale
    Json := TextBetween(SharedGetLatestPageHtml(), '"@type":"Movie"', '</script>');  
    certification := Fulltrim(TextBetween(Json, '"contentRating":"', '"'));
    rating := Fulltrim(TextBetween(Json, '"ratingValue":', ','));   
    
    SharedSecureSetAllFields(originalTitle, translatedTitle, rating, year, producers, writers, composers, directors, actors, duration, categories, certification, countries, description, comments, SharedGetMovieUrl(), picture);
  end
  else if SharedPickTreeMatching(originalTitle, translatedTitle, year, directors, '') then
    SharedSetMovieUrl(myUrl)
  else
    SharedPickTreeAdd(translatedTitle, originalTitle, directors, year, myUrl);
end;


// ----------------------------------------------------------------------------
// FILL PICKTREE CONTROL WITH LINKS & TITLES
// IN:  none
// OUT: none
// ----------------------------------------------------------------------------
procedure NientePopcornPopulatePickTree();
var
  TempUrl, TempTitle, TempYear, Page, PageBlock: string;
  nextPageAvailable: boolean;
begin
  Page := SharedGetLatestPageHtml();
  SharedPickTreeCreate();
  nextPageAvailable := false;  
  TempUrl := textBetween(Page, '<script type="text/javascript">location.href = "', '"');
  if (TempUrl <> '') then
  begin
    // è stato fatto un redirect all'unico risultato di ricerca corrispondente al termine cercato
    // non potendo essere certo che il risultato sia corretto, passo per SharedPickTreeAdd() previsto in NientePopcornRetriveMovieData
    // SharedPickTreeAdd() adotterà i criteri standard di verifica controllando non solo i titoli ma anche anno e regista, evitando così falsi positivi 
    NientePopcornRetriveMovieData(NP_UrlBase + TempUrl, false);
  end
  else if (pos('</h1></div><h3 class="film-non-trovato">', Page) = 0) then
  begin 
    Page := textBetween(Page, '<div class="result">', '<h3 class="film-non-trovato">');
    if Page <> '' then
    begin
      Page := '<div class="result">' + Page;
      while (pos('<div class="result">', Page) > 0) do
      begin    
        Delete(Page, 1, pos('<div class="result">', Page) - 1);
        // PageBlock contiene l'html relativo ad un singolo film trovato
        PageBlock := textBetween(Page, '<div class="result"', '<div class="clearboth"');
        Delete(Page, 1, pos('<div class="result">', Page) + 10);  // cancello un po' del tag iniziale, altrimenti il while non avanza

        TempTitle := TextBetween(PageBlock, '<span class="scheda-film_titolo">', '</span>');
        HTMLDecode(TempTitle);
        TempYear := Textbetween(PageBlock, '<span class="film_anno">/ ', '</span>');
        TempUrl := Textbetween(PageBlock, '<p class="trama">', '</a>');
        TempUrl := Textbetween(TempUrl, '<a href="', '"');    
        if ((IntToStr(StrToInt(TempYear, 0)) = TempYear) AND (TempUrl <> '')) then      
          SharedPickTreeAdd(TempTitle, '', '', TempYear, NP_UrlBase + TempUrl);         
      end;
      // quando siamo all'ultima pagina il codice html è: alt="pagina successiva"/></span></li>
      nextPageAvailable := (pos('alt="pagina successiva"/></a>', SharedGetLatestPageHtml()) > 0);
    end;
  end;  
  SharedPickTreeClose(nextPageAvailable);
end;

end.
