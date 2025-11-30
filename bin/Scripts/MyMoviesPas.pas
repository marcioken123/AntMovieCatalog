unit MyMoviesPas;

{
  Main var, constants and functions
}
uses
  ItalianSharedPas;

const
  MM_UnitVersion = 5;

  MM_UrlBase = 'https://www.mymovies.it';
  MM_UrlSearch = MM_UrlBase + '/database/ricerca/avanzata/?titolo=';  
  MM_UrlSearchParameters = '&titolo_orig=&regista=&attore=&id_genere=-1&nazione=&clausola1=-1&anno_prod=&clausola2=-1&stelle=-1&id_manif=-1&anno_manif=&disponib=-1&ordinamento=0&submit=Inizia+ricerca+%C2%BB';  

  MM_ImagePath = MM_UrlBase + '/filmclub/';
  MM_Apice = #39;
  MM_NoComment = '(Riceverai le informazioni pochi giorni prima della messa in onda)';
  // commenti inutili  "(la recensione più amata dal pubblico)"
  MM_TextMostLoved = '(la recensione più amata dal pubblico)';
  MM_cStartNumRis = '. Ho trovato '; // Result Number start Marker
  MM_cEndNumRis = ' film.</h3>';               // Result Number end Marker
  MM_cStartId = 'recensione.asp?id=';   // ID start marker
  MM_EndId = '" title="';              // ID end marker
  MM_cStartNewTitle = '<meta property="og:title" content="';
  MM_cEndNewTitle = '"';
  MM_cStartTitle = 'Titolo originale <em>';             // Title start marker
  MM_cEndTitle = '</em>';              // Title end marker
  MM_cStartTranslTitle = '<h1 style="margin-bottom:3px;">';        // Translated title start marker
  MM_cEndTranslTitle = '</h1>';         // Translated title end marker
  MM_cStartImage2 = '<img src="';       // Image start marker
  MM_cEndImage2 = '"';                  // Image end marker
  MM_cStartDirector = 'Un film di <a href="';      // Director start marker
  MM_cEndDirector = '</a>.';                       // Director end marker
  MM_cStartCategory = '<a title="Film ';   // Catogory start marker
  MM_cEndCategory = '</a>';            // Category first end marker
  MM_cStartCategory2 = '">';   // Catogory start marker
  MM_cEndCategory2 = '</a>';            // Category second end marker
  MM_cStartDuration = 'durata ';        // Duration start marker
  MM_cEndDuration = ' min.';          // Duration end marker
  MM_cStartYear = '<meta property="og:title" content="';              // Year start marker
  MM_cEndYear = '/>';                 // Year end marker
  MM_cStartYear2 = '(';   // year start marker
  MM_cEndYear2 = ')';            // year second end marker
  MM_cStartDesc = '<div id="recensione">';          // Description start marker
  MM_cStartDesc2 = '<td rowspan="2" valign="top">';          // Description start marker
  MM_cEndDesc = '<div style="text-align:right;">';  // Description end marker

  MM_cStartCast = 'Con <a href="';              // Actor start marker
  MM_cEndCast = '<a title="';
  MM_cEndCast2 = 'Titolo originale <em>';      // Actor end marker
  //  MM_cStartImage = 'src="https://pad.mymovies.it/filmclub/';     //fs 2018.02.07
  MM_cStartImage = '<meta property="og:image" content="';       //fs 2018.02.07
  MM_cEndImage = '"';


// --------------------------------------------------------------------------------------------------------------------------------------
//                                               "public" functions e procedures
// --------------------------------------------------------------------------------------------------------------------------------------

// -----------------------------------
// Main program (for MultiSite)
// IN: properties values (serialized)
// OUT: properties values (serialized)
// -----------------------------------
function MyMoviesCoreMultiSite(serializedValues: string): string;
begin
  SharedUnserializeMe(serializedValues);
  // Analyze search page and get movie url (user choice)
  if (SharedGetMoviesFound() = 0) then
    MyMoviesSearchResults();
  if (SharedGetMoviesFound() > 0) then // Analyze movie page and set movie fields
    MyMoviesSetMovieFields();  
  result := SharedSerializeMe();
end;


// --------------------------------
// ANALYZE FIRST SEARCH RESULT PAGE
// IN:  none
// --------------------------------
procedure MyMoviesSearchResults();
var
  myUrl, myUrlFirstPart: string;  
  pageNumberMovies: integer;  
begin
  pageNumberMovies := 0;
  myUrlFirstPart := MM_UrlSearch + SharedAdvancedUrlEncode(UTF8Encode(SharedGetMovieName()), false);
  if (SharedGetYearDate() = '') then
    myUrlFirstPart := myUrlFirstPart + MM_UrlSearchParameters
  else // search for movies with year >= (user year - 5)
    myUrlFirstPart := myUrlFirstPart + StringReplace(StringReplace(MM_UrlSearchParameters, 'anno_prod=', 'anno_prod=' + IntToStr(StrToInt(SharedGetYearDate(), 0) - 5)), 'clausola1=-1', 'clausola1=%3E%3D');
  myUrlFirstPart := myUrlFirstPart + '&page=';

  repeat
    SharedSetMovieUrl('');
    SharedSetMoviesFound(0);
    pageNumberMovies := pageNumberMovies + 1;
    myUrl := myUrlFirstPart + IntToStr(pageNumberMovies);  
    SharedHTTPGetPage(myUrl);        
    MyMoviesPopulatePickTree();
    SharedPickTreeExec();
    // se al primo tentativo non trovo alcun risultato, ripeto la ricerca basandola sul titolo originale anziché sul titolo tradotto (comportamento di default)
    // esempio: se con Lawnmower non ho trovato alcun risultato allora ripeto la ricerca come titolo originale
    if ((SharedGetMoviesFound() = 0) AND (pageNumberMovies = 1) AND (pos('?titolo=', myUrlFirstPart) > 0)) then 
    begin
      SharedSetMovieUrl('next:');
      pageNumberMovies := 0;
      myUrlFirstPart := StringReplace(myUrlFirstPart, '?titolo=', '?titolo_orig=');
      myUrlFirstPart := StringReplace(myUrlFirstPart, '&titolo_orig=', '&titolo=');
    end;
  until (copy(SharedGetMovieUrl(), 1, 5) <> 'next:');
end;


// -----------------------
// ANALYZE MOVIE DATA PAGE
// IN:  none
// -----------------------
procedure MyMoviesSetMovieFields;
begin
  if (pos('/dizionario/recensione.asp?id=', SharedGetMovieUrl()) <> 0) then
    SharedHTTPGetPage(StringReplace(SharedGetMovieUrl(), 'http:', 'https:'))
  else
    SharedHTTPGetPage(SharedGetMovieUrl());
  if (pos('<!-- webTrekk Espresso -->', SharedGetLatestPageHtml()) > 0) then
    MyMoviesRedirectUrl();
  if (pos('var_carica_taboola', SharedGetLatestPageHtml()) > 0) then
    MyMoviesExtractPageTypeB()
  else
    MyMoviesExtractPageTypeA();
end;


// --------------------------------------------------------------------------------------------------------------------------------------
//                                               "private" functions e procedures
// --------------------------------------------------------------------------------------------------------------------------------------

function MyMoviesMyTrim(Value: string): string;
var
  ExitLoop: boolean;
  NewField, OldField: string;
  OldLgthValue, NewLgthValue: integer;
begin
  NewField := ' ';
  OldField := #9;
  Value := StringReplace(Value, OldField, NewField);
  OldField := #10;
  Value := StringReplace(Value, OldField, NewField);
  OldField := #13;
  Value := StringReplace(Value, OldField, NewField);
  OldField := '  ';
  Value := StringReplace(Value, OldField, NewField);
  Newfield := '&';
  OldField := '&';
  Value := StringReplace(Value, OldField, NewField);
  Newfield := 'è';
  OldField := 'è';
  Value := StringReplace(Value, OldField, NewField);
  ExitLoop := false;
  OldLgthValue := length(Value);
  HTMLRemoveTags(Value);
  Value := FullTrim(Value);
  OldField := '  ';
  NewField := ' ';
  repeat
    Value := StringReplace(Value, OldField, NewField);
    NewLgthValue := length(Value);
    if OldLgthValue = NewLgthValue then
      ExitLoop := true
    else
      OldLgthValue := NewLgthValue;
  until ExitLoop;

  result := Value;
end;

//-------------------------------------------------
// Retrieve the right url from the page and open it
//-------------------------------------------------
procedure MyMoviesRedirectUrl;          
var
  myUrl: string;
begin
  myUrl := textbetween(SharedGetLatestPageHtml(), '<!-- webTrekk Espresso -->', '<!-- /webTrekk Espresso -->');
  HTMLRemoveTags(myUrl);
  myUrl := textbetween(myUrl, (' pageHref = ' + MM_Apice), (MM_Apice + ';'));
  if (pos('/dizionario/recensione.asp?id=', myUrl) > 0) then
    SharedHTTPGetPage(myUrl);
end;


function matchDescriptionComments(description, comments: string): string;
var
  des, com: string;
begin
  des := StringReplace(description, chr(13), '');
  des := StringReplace(des, chr(10), '');
  des := StringReplace(des, ' ', '');  
  com := StringReplace(comments, chr(13), '');
  com := StringReplace(com, chr(10), '');
  com := StringReplace(com, ' ', '');  
  if (pos(com, des) > 0) then
    result := description
  else if (pos(des, com) > 0) then
    result := comments
  else
    result := '';
end;

//----------------------------
procedure MyMoviesExtractPageTypeB;
var
  originalTitle, translatedTitle, rating, year, producers, directors, actors, duration, categories, certification, countries, description, comments, picture: string;
  Trama_e_critica, TempField, init_char, end_char: string;
  lgth_delimiters: integer;
begin
  TempField := TextBetween(SharedGetLatestPageHtml(), MM_cStartNewTitle, MM_cEndNewTitle);
  translatedTitle := MyMoviesMyTrim(TempField);

  // Get Original Title
  originalTitle := TextBetween(SharedGetLatestPageHtml(), 'Titolo originale</td>', '</tr>');
  HTMLRemoveTags(originalTitle);
  
  // Get rating
  rating := TextBetween(SharedGetLatestPageHtml(), '"ratingValue": "', '"');
  if (rating <> '') then
    rating := FloatToStr(StrToFloat(rating) * 2);

  // Get Director
  TempField := TextBetween(SharedGetLatestPageHtml(), '<tr><td valign="top">Regia di</td>', '</tr>');
  HTMLRemovetags(TempField);
  directors := Fulltrim(TempField);

  // Get Actors
  TempField := TextBetween(SharedGetLatestPageHtml(), '<tr><td valign="top">Attori</td>', '</tr>');
  HTMLRemovetags(TempField); 
  if SharedGetDebugMode() then
    TempField := TempField + CRLF + '(new_form)';
  actors := TempField;            //attori principali

  // Get Category
  TempField := TextBetween(SharedGetLatestPageHtml(), '<tr><td valign="top">Genere</td>', '</tr>');
  HTMLRemovetags(TempField);
  TempField := fulltrim(TempField);
  // remove final commas if present
  if copy(TempField, Length(TempField), 1) = ',' then
    TempField := copy(TempField, 1, Length(TempField) - 1);
  categories := TempField;

  // Get Duration
  TempField := TextBetween(SharedGetLatestPageHtml(), '<tr><td valign="top">Durata</td>', '</tr>');
  HTMLRemovetags(TempField);
  TempField := fulltrim(Textbefore(TempField, ' ', ''));
  duration := TempField;

  // Get Country
  TempField := TextBetween(SharedGetLatestPageHtml(), '<td valign="top">Produzione</td>', '</tr>') + '</tr>';
  //  TempField := '<tr> href=' + TextBetween(TempField, 'href=', '</tr>');
  TempField := '<tr>' + TextBetween(TempField, 'href=', '</tr>');
  HTMLRemovetags(TempField);
  //  TempField := MyMoviesMyTrim(TempField);
  TempField := textafter(TempField, '>');
  countries := TempField;

  // Get Year
  TempField := Textbetween(SharedGetLatestPageHtml(), '<td valign="top">Anno</td>', '</tr>');
  HTMLRemovetags(TempField);
  year := TempField;
  
  // Get Producers
  TempField := Textbetween(SharedGetLatestPageHtml(), 'distribuito da ', '</a>');
  HTMLRemovetags(TempField);
  producers := TempField; 

  // Get film image
  TempField := TextBetween(SharedGetLatestPageHtml(), '<amp-img layout="responsive" class="stonda3" width="230" height="330" src="', '"></amp-img>');
  TempField := StringReplace(TempField, 'https', 'http');
  if ((TempField = '') OR (pos('twitter_ico.gif', TempField) > 0)) then
    TempField := '';
  picture := TempField;

  // Get certification
  TempField := TextBetween(SharedGetLatestPageHtml(), '<td valign="top">Rating</td><td>Consigli per la visione di bambini e ragazzi: ', '</tr>');
  if TempField <> '' then
  begin
    HTMLRemovetags(TempField);
    TempField := MyMoviesMyTrim(TempField);
    TempField := StringReplace(TempField, 'Film per tutti', 'PT');
  end;
  certification := TempField;

  // Description Field
  Trama_e_critica := fulltrim(TextBetween(SharedGetLatestPageHtml(), '<p class="highlights mm-margin-auto mm-center mm-margin-t16">', '<div'));
  HTMLRemovetags(Trama_e_critica);
  Trama_e_critica := fulltrim(Trama_e_critica);
  if Trama_e_critica = '' then
  begin
    init_char := '<div class="sottotitolo_autore mm-letter-spacing-2 mm-center mm-show-xs mm-hide-sm">';
    end_char := '<div class="mm-right mm-margin-r8"">';
    Trama_e_critica := TextBetween(SharedGetLatestPageHtml(), init_char, end_char);
    if (pos('<p class="corpo">', Trama_e_critica) > 0) then
      Trama_e_critica := copy(Trama_e_critica, pos('<p class="corpo">', Trama_e_critica), length(Trama_e_critica));
    HTMLRemovetags(Trama_e_critica);
    Trama_e_critica := fulltrim(Trama_e_critica);
    if Trama_e_critica = '' then //2023-04-29
    begin    
      // se non ho ancora trovato la trama prendo il codice html incluso tra <p class="corpo"> e </p>.
      // Siccome però tale codice potrebbe essere presente anche nel blocco cercato in precedenza, prima cancello totalmente il blocco (con lo StringReplace) e poi effettuo la ricerca
      Trama_e_critica := TextBetween(StringReplace(SharedGetLatestPageHtml(), (init_char + TextBetween(SharedGetLatestPageHtml(), init_char, end_char) + end_char), ''), '<p class="corpo">', '</p>');
      HTMLRemovetags(Trama_e_critica);
      Trama_e_critica := fulltrim(Trama_e_critica);
    end;    
  end;

  SharedDebugAppendToFile(trama_e_critica, '[MyMoviesPas][MyMoviesExtractPageTypeB] comments html');

  Trama_e_critica := MyMoviesMyTrim(Trama_e_critica);  
  if (pos('a cura della redazione ', Trama_e_critica) = 1) then
    Delete(Trama_e_critica, 1, Length('a cura della redazione '));
  
  // Comments Field
  init_char := 'Recensione di';
  end_char := '<div class="clear10">';
  //   TempField := '<p ' + init_char + textBetween(Trama_e_critica, init_char, end_char) + end_char;
  TempField := fulltrim(TextBetween(SharedGetLatestPageHtml(), init_char, end_char)) + end_char;
  if length(TempField) = length(end_char) then
  begin
    init_char := '<div class="sottotitolo_autore mm-letter-spacing-2 mm-center mm-show-xs mm-hide-sm">';
    end_char := '<div class="mm-right mm-margin-r8"">';
    TempField := fulltrim(TextBetween(SharedGetLatestPageHtml(), init_char, end_char));
  end;

  Init_char := '<p class="corpo">';
  end_char := '</p>';                                              //2018.04.24
  TempField := init_char + TextBetween(TempField, init_char, end_char) + end_char;
  //fin qui temp_field OK per bush...
  comments := TempField;
  HTMLRemovetags(comments);

  End_char := '<a target="_blank"';
  TempField := init_char + fulltrim(TextBetween(SharedGetLatestPageHtml(), init_char, end_char)) + end_char;
  lgth_delimiters := (length(init_char) + length(end_char));
  if length(TempField) = lgth_delimiters then                                                 //2018.04.25fs
  begin                                                                      //2018.04.25fs
    Init_char := '<p class="corpo">';                                        //2018.04.25fs
    end_char := '</p>';                                                      //2018.04.25fs
    TempField := init_char + fulltrim(TextBetween(SharedGetLatestPageHtml(), init_char, end_char)) + end_char;  //2018.04.25fs
  end;                                                                       //2018.04.25fs
  TempField := StringReplace(TempField, 'Il film Shadow è disponibile in streaming. Scopri MYmovieslive', '');
  TempField := StringReplace(TempField,
    'Questo film è disponibile in versione digitale, scopri il miglior prezzo: TROVASTREAMING', '');
  HTMLRemovetags(TempField);
  TempField := Fulltrim(TempField);
  TempField := MyMoviesMyTrim(TempField);

  // check if description and comments are inverted
  Trama_e_critica := StringReplace(Trama_e_critica, ' ...', '...');
  if ((pos('...', Trama_e_critica) > 0) AND (copy(Trama_e_critica, 1, (pos('...', Trama_e_critica) - 1)) = copy(TempField, 1, (pos('...', Trama_e_critica) - 1)))) then
    description := TempField
  else
  begin
    description := Trama_e_critica;                            //2018.04.20fs
    if ((TempField <> Trama_e_critica) AND (pos(Trama_e_critica, TempField) > 0)) then
      comments := StringReplace(TempField, Trama_e_critica, '');
  end;
  HTMLDecode(description);
  description := fulltrim(description);
  HTMLDecode(comments);
  comments := fulltrim(comments);
  
  TempField := matchDescriptionComments(description, comments);
  if (TempField <> '') then
  begin
    description := TempField;
    comments := '';
  end;
  
  SharedSecureSetAllFields(originalTitle, translatedTitle, rating, year, producers, '', '', directors, actors, duration, categories, certification, countries, description, comments, SharedGetMovieUrl(), picture);
end;

//----------------------------

//----------------------------
procedure MyMoviesExtractPageTypeA;
//----------------------------
var
  originalTitle, translatedTitle, rating, year, directors, actors, duration, categories, certification, countries, description, comments, picture: string;
  pos_func, PosCast: integer;
  SaveField, DelField, TempField: string;
begin
  TempField := TextBetween(SharedGetLatestPageHtml(), MM_cStartTranslTitle, MM_cEndTranslTitle);
  translatedTitle := MyMoviesMyTrim(TempField); 

  // Get Original Title
  originalTitle := TextBetween(SharedGetLatestPageHtml(), MM_cStartTitle, MM_cEndTitle);

  // Get Director
  TempField := TextBetween(SharedGetLatestPageHtml(), MM_cStartDirector, MM_cEndDirector) + MM_cEndDirector;
  TempField := TextBetween(TempField, '>', MM_cEndDirector);
  HTMLRemovetags(TempField);
  TempField := Fulltrim(TempField);
  directors := TempField;
  
  // Get rating
  rating := TextBetween(SharedGetLatestPageHtml(), 'valutazione media tra critica e pubblico: ', ' stell');
  if (rating <> '') then
    rating := IntToStr(StrToInt(rating, 0) * 2);

  // Get Actors
  TempField := MM_cStartCast + TextBetween(SharedGetLatestPageHtml(), MM_cStartCast, MM_cEndCast);     // da 'con:'   a Titolo
  PosCast := pos(MM_cStartCast, SharedGetLatestPageHtml()) + length(TempField);
  SaveField := TempField;
  TempField := TextBetween(SharedGetLatestPageHtml(), MM_cStartCast, MM_cEndCast2);   // da 'con:'   a Titolo Originale
  if length(TempField) > 0 then
    TempField := '<a href="' + TempField
  else
    TempField := '<a href="' + SaveField;
  DelField := '<div id="attori_espandi"' + TextBetween(TempField, '<div id="attori_espandi"', '</div>');
  TempField := StringReplace(TempField, DelField, '');
  DelField := '<div id="attori_comprimi"' + TextBetween(TempField, '<div id="attori_comprimi"', '</div>');
  TempField := StringReplace(TempField, DelField, '');
  TempField := StringReplace(TempField, 'Formato Serie TV', '');
  TempField := MyMoviesMyTrim(TempField);
  if SharedGetDebugMode() then
    TempField := TempField + CRLF + '(old_form)';
  actors := TempField;

  // Get Category
  TempField := TextBetween(SharedGetLatestPageHtml(), MM_cStartCategory, MM_cEndcategory) + MM_cEndcategory;
  TempField := TextBetween(TempField, MM_cStartCategory2, MM_cEndcategory2);
  //  TempField := TempField + MM_cEndcategory;
  //  TempField := TextBetween(TempField, '">' , MM_cEndcategory);
  TempField := fulltrim(TempField);
  // remove final commas if present
  if copy(TempField, Length(TempField), 1) = ',' then
    TempField := copy(TempField, 1, Length(TempField) - 1);
  categories := TempField;

  // Get Duration
  TempField := TextBetween(SharedGetLatestPageHtml(), '<td valign="top">Durata</td>', '</tr>');
  HTMLRemovetags(TempField);
  duration := fulltrim(StringReplace(TempField, ' minuti', ''));

  // Get Country
  TempField := SharedGetLatestPageHtml();
//  if MM_PosDuration > 0 then
//    Delete(TempField, 1, MM_PosDuration - 1)
//  else
  if PosCast > 0 then
    Delete(TempField, 1, PosCast - 1);
  TempField := TextBetween(TempField, '-', ' <strong>');
  TempField := MyMoviesMyTrim(TempField);
  pos_func := pos('function(', TempField);
  if ((pos('Transitional', TempField) > 0) OR (pos_func > 0)) then
    TempField := '';
  countries := TempField;

  // Get Year
  TempField := SharedGetLatestPageHtml();
  TempField := Textbetween(TempField, MM_cStartYear, MM_cEndYear);
  //  TempField := Textbetween (TempField, '(', ')');
  TempField := TextBetween(TempField, MM_cStartYear2, MM_cEndYear2);
  year := TempField; 

  // Get film image
  TempField := TextBetween(SharedGetLatestPageHtml(), MM_cStartImage, MM_cEndImage);
  if TempField = '' then
    TempField := TextBetween(SharedGetLatestPageHtml(), MM_cStartImage2, MM_cEndImage2);

  if ((TempField = '') OR (pos('twitter_ico.gif', TempField) > 0)) then
    TempField := '';
  picture := TempField;

  TempField := TextBetween(SharedGetLatestPageHtml(), '<td valign="top">Rating</td><td>Consigli per la visione di bambini e ragazzi: ',
    '</tr>');
  if TempField <> '' then
  begin
    HTMLRemovetags(TempField);
    TempField := MyMoviesMyTrim(TempField);
    TempField := StringReplace(TempField, 'Film per tutti', 'PT');
  end;
  certification := TempField;

  // Get Description
  TempField := TextBetween(SharedGetLatestPageHtml(), MM_cStartDesc, MM_cEndDesc) + MM_cEndDesc;
  TempField := TextBetween(TempField, MM_cStartDesc2, MM_cEndDesc) + MM_cEndDesc;
  TempField := MyMoviesMyTrim(TempField);
  TempField := StringReplace(TempField, 'Il film Shadow è disponibile in streaming. Scopri MYmovieslive', '');
  TempField := StringReplace(TempField,
    'Questo film è disponibile in versione digitale, scopri il miglior prezzo: TROVASTREAMING', '');
  HTMLDecode(TempField);
  //2018.04.22fs    TempField := fulltrim(TempField);
  description := TempField;

  // Get Comments
  //   TempField := Textbetween(SharedGetLatestPageHtml(), '<p style="padding-right:3px; margin-top:15px;">', '<a href="https://www.mymovies.it/dizionario/critica');
  TempField := Textbetween(SharedGetLatestPageHtml(), '">Rassegna stampa</a>', '</td>') + '</td>';
  TempField := Textbetween(TempField, '<p style="padding-right:3px; margin-top:15px;">',
    '<a href="https://www.mymovies.it/dizionario/critica');
  TempField := MyMoviesMyTrim(TempField);
  HTMLDecode(TempField);
  comments := TempField;
  
  TempField := matchDescriptionComments(description, comments);
  if (TempField <> '') then
  begin
    description := TempField;
    comments := '';
  end;
  
  SharedSecureSetAllFields(originalTitle, translatedTitle, rating, year, '', '', '', directors, actors, duration, categories, certification, countries, description, comments, SharedGetMovieUrl(), picture);
end;


// ----------------------------------------------------------------------------
// FILL PICKTREE CONTROL WITH LINKS & TITLES
// IN:  none
// OUT: none
// ----------------------------------------------------------------------------
procedure MyMoviesPopulatePickTree();
var
  TempIdFilm, TempTitle, TempAuthor, TempYear, Page, PageBlock: string;
  nextPageAvailable: boolean;
begin
  Page := SharedGetLatestPageHtml();
  SharedSetMoviesFound(StrToInt(Textbetween(Page, MM_cStartNumRis, MM_cEndNumRis), 0));
  if SharedGetMoviesFound() = 0 then      
    exit;

  SharedPickTreeCreate(); 
  while (pos('<div class="linkblu" style="padding:3px;">', Page) > 0) do
  begin
    Delete(Page, 1, pos('<div class="linkblu" style="padding:3px;">', Page) - 1);
    // PageBlock contiene l'html relativo ad un singolo film trovato
    PageBlock := textBetween(Page, '<div class="linkblu" style="padding:3px;">', '<div class="linkblu" style="padding:3px;">');
    if (Length(PageBlock) = 0) then // ultimo blocco, prendo tutto l'html fino a fine pagina
      PageBlock := Page;
    Delete(Page, 1, pos('<div class="linkblu" style="padding:3px;">', Page) + 10);  // cancello un po' del tag iniziale, altrimenti il while non avanza
    if (pos('http://www.mymovies.it/dizionario/recensione.asp?id=', PageBlock) > 0) then
    begin
      Delete(PageBlock, 1, pos('http://www.mymovies.it/dizionario/recensione.asp?id=', PageBlock) - 1);
      TempIdFilm := copy(PageBlock, 1, pos('"', PageBlock) - 1);
      TempTitle := TextBetween(PageBlock, 'title="', '">');
      TempTitle := Fulltrim(TempTitle);
      TempAuthor := Textbetween(PageBlock, 'Un film di <b>', '</b>');
      TempAuthor := Textbetween(TempAuthor, '">', '</a>');
      TempYear := Textbetween(PageBlock, '<a href="http://www.mymovies.it/film/?anno=', '">');      //Vjrgil
      if SharedPickTreeMatching(UTF8Decode(TempTitle), '', TempYear, TempAuthor, '') then
      begin
        SharedSetMovieUrl(TempIdFilm);
        exit;
      end;
      SharedPickTreeAdd(UTF8Decode(TempTitle), '', UTF8Decode(TempAuthor), UTF8Decode(TempYear), TempIdFilm);
    end;
  end;
  
  nextPageAvailable := (pos('>Pagina successiva &raquo;</a>', SharedGetLatestPageHtml()) > 0);
  SharedPickTreeClose(nextPageAvailable);
end;

end.
