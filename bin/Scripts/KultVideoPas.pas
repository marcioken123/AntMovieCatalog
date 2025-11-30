unit KultVideoPas;

{
  Main var, constants and functions
}
uses
  ItalianSharedPas;
  
const
  KV_UnitVersion = 2;  
  KV_UrlBase = 'http://www.kultvideo.com/';

// --------------------------------------------------------------------------------------------------------------------------------------
//                                               "public" functions e procedures
// --------------------------------------------------------------------------------------------------------------------------------------

// -----------------------------------
// Main program (for MultiSite)
// IN: properties values (serialized)
// OUT: properties values (serialized)
// -----------------------------------
function KultVideoCoreMultiSite(serializedValues: string): string;
begin
  SharedUnserializeMe(serializedValues);
  // Analyze search page and get movie url (user choice)
  if (SharedGetMoviesFound() = 0) then
    KultVideoSearchResults();
  if (SharedGetMoviesFound() > 0) then // Analyze movie page and set movie fields
    KultVideoSetMovieFields();  
  result := SharedSerializeMe();    
end;

// --------------------------------
// ANALYZE FIRST SEARCH RESULT PAGE
// IN:  none
// --------------------------------
procedure KultVideoSearchResults;
var
  postData: string;
begin
  SharedHTTPGetPage('http://www.kultvideo.com/search/AdvancedSearch.aspx?__lang=it-IT');
  postData := '__EVENTTARGET=&__EVENTARGUMENT=&__VIEWSTATE=' + SharedAdvancedUrlEncode(textBetween(SharedGetLatestPageHtml(), 'id="__VIEWSTATE" value="', '"'), false) +'&__VIEWSTATEGENERATOR=' + SharedAdvancedUrlEncode(textBetween(SharedGetLatestPageHtml(), 'id="__VIEWSTATEGENERATOR" value="', '"'), false) + '&__lang=&ctl00%24tbSearchField=&ctl00%24MainContent%24tbCritTitle=' + SharedAdvancedUrlEncode(UTF8Encode(SharedGetMovieName()), false) +'&ctl00%24MainContent%24tbCritDirector=&ctl00%24MainContent%24tbCritCast=&ctl00%24MainContent%24cbxlArticleType%240=on&ctl00%24MainContent%24cbxlArticleType%241=on&ctl00%24MainContent%24cbxlArticleType%242=on&ctl00%24MainContent%24cbxlArticleType%243=on&ctl00%24MainContent%24cbxlArticleType%244=on&ctl00%24MainContent%24btnAdvSearch=Trova&ctl00%24tbLoginUsername=&ctl00%24tbLoginPassword=';
  
  repeat
    SharedSetMovieUrl('');
    SharedSetMoviesFound(0);
    SharedHTTPPostPage('http://www.kultvideo.com/search/AdvancedSearch.aspx?__lang=it-IT', postData);
    if (pos('Nessun articolo al momento disponibile per la selezione effettuata', SharedGetLatestPageHtml()) > 0) then
      exit;
    postData := KultVideoPopulatePickTree();
    if (SharedGetMovieUrl() = '') then
      if ((SharedGetMoviesFound() = 0) AND (postData <> '')) then 
        // se non ho trovato nessun risultato in questa pagina e se posso andare avanti ci vado
        SharedSetMovieUrl('next:')   
      else       
        SharedPickTreeExec();   
  until (copy(SharedGetMovieUrl(), 1, 5) <> 'next:');  
end;

// Analisi ed estrazione dati dalla pagina del film
procedure KultVideoSetMovieFields();
var
  //Fine: Integer;
  Page: TStringList;
  originalTitle, translatedTitle, rating, year, producers, directors, actors, duration, categories, certification, countries, description, comments, picture: string;
  Line, Line2, Line3, Comm: string;
  InitChar, EndChar, SaveNationYear, trama_trovata: string;
  LineNr: Integer;
  BeginPos, EndPos: Integer;
  Field: integer;
  PageStr: string;  
begin
  SharedHTTPGetPage(SharedGetMovieUrl() + '&adtst=1');
  Page := TStringList.Create;
  Page.Text := SharedGetLatestPageHtml();
  PageStr := Page.Text;

  // Immagine
  LineNr := FindLine('id="article_sheet_picture"', Page, 0);
  Line := '';
  if LineNr>-1 then
  begin
    LineNr := LineNr + 1;
    Line := Page.GetString(LineNr);
    Line := TextBetween(Line, '<img src="', '" width="');
    if Line <> '' then
      Line := KV_UrlBase + Line;
  end;
  picture := Line;

  // Cerca il titolo tradotto
  InitChar := '<td align="left" valign="top" class="article_sheet_filmtitle">';  //tipo 1
  BeginPos := Pos(InitChar, PageStr);
  Delete(PageStr, 1, BeginPos - 1);
  Line := '>' + TextBetween(PageStr, InitChar, '<') + '<';
  Line := TextBetween(Line, '>', '<');
  HTMLRemoveTags(Line);
  HTMLDecode(Line);
  Line := Fulltrim(Line);
  translatedTitle := KultVideoMixedCaseText(UTF8Decode(Line));

  // Cerca il titolo originale
  InitChar := '<span class="article_sheet_filmsubtitle">';
  BeginPos := Pos(InitChar, PageStr);
  if BeginPos = 0 then
  begin
     InitChar := '<span class="article_sheet_subtitle">';
     BeginPos := Pos(InitChar, PageStr);
  end;
  Delete(PageStr, 1, BeginPos - 1);
  EndChar := '<';
  LineNr := FindLine(InitChar, Page, 0);
  Line := Page.GetString(LineNr);
  InitChar := '>';
  Line := InitChar + TextBetween(Line, '<span class="article_sheet_filmsubtitle">', EndChar) + EndChar;
  Line := TextBetween(Line, '>', EndChar);
  BeginPos := Pos(InitChar, PageStr);
  HTMLRemoveTags(Line);
  HTMLDecode(Line);
  Line := Fulltrim(Line);
  Line := UTF8Decode(Line);
  originalTitle := KultVideoMixedCaseText(Line);
       
  // Cerca nazionalità e anno
  Delete(PageStr, 1, BeginPos - 1);
  InitChar := '<td';
  EndChar :=  '</td>';
  Line := InitChar + Textbetween(PageStr, InitChar, EndChar) + EndChar;
  HTMLRemoveTags(Line);
  HTMLDecode(Line);
  Line := Fulltrim(Line);  //'Francia (1987) - Colore'
  SaveNationYear := Line;
  Line := TextBefore(SaveNationYear, '(', '');
  countries := KultVideoMixedCaseText(Trim(Line));
// esempio:  MovieName := TextBefore(MovieName, '[', '') + TextAfter(MovieName, ']');

  year := TextBetween(SaveNationYear, '(', ')');

  // Cerca genere
  InitChar := '<span class="article_sheet_datalabel">';
  Line := Textbetween(PageStr, InitChar, '</td>');
  BeginPos := Pos(InitChar, PageStr);
  Delete(PageStr, 1, BeginPos);
  Line := stringreplace(Line, 'Genere:', '');
  HTMLRemoveTags(Line);
  HTMLDecode(Line);
  Line := Fulltrim(Line);
  categories := KultVideoMixedCaseText(Line);

  // Cerca regia
  InitChar := '<span class="article_sheet_datalabel">';
  Line := Textbetween(PageStr, InitChar, '</td>');
  BeginPos := Pos(InitChar, PageStr);
  Delete(PageStr, 1, BeginPos);
  Line := stringreplace(Line, 'Regia:', '');
  HTMLRemoveTags(Line);
  HTMLDecode(Line);
  Line := Fulltrim(Line);
  directors := KultVideoMixedCaseText(UTF8Decode(Line));

  // Cerca cast
  InitChar := '<span class="article_sheet_datalabel">';
  Line := Textbetween(PageStr, InitChar, '</td>');
  BeginPos := Pos(InitChar, PageStr);
  Delete(PageStr, 1, BeginPos);
  Line := stringreplace(Line, 'Cast:', '');
  HTMLRemoveTags(Line);
  HTMLDecode(Line);
  Line := Fulltrim(Line);
  actors := KultVideoMixedCaseText(UTF8Decode(Line));
 
  // Cerca distributore (al posto del produttore)
  InitChar := '<span class="article_sheet_datalabel">';
  Line := Textbetween(PageStr, InitChar, '</td>');
  BeginPos := Pos(InitChar, PageStr);
  Delete(PageStr, 1, BeginPos);
//  Line := stringreplace(Line, 'Cast:', '');
  HTMLRemoveTags(Line);
  HTMLDecode(Line);
  Line := stringreplace(Line, crlf, '');
  Line := stringreplace(Line, #09, '');
  Line := Fulltrim(Line);
  producers := UTF8Decode(Line);

  // Cerca la durata
  InitChar := '<span class="article_sheet_datalabel">Durata:'; //tipo 1 http://www.kultvideo.com/articles/ArticleSheet.aspx?__langG=it-IT&aid=6821
//  InitChar := '<span class="article_sheet_datalabel">';   //tipo 2
  Line := Textbetween(PageStr, InitChar, '</td>');
  BeginPos := Pos(InitChar, PageStr);
  Delete(PageStr, 1, BeginPos);
  Line := stringreplace(Line, 'Durata:', '');
  HTMLRemoveTags(Line);
  HTMLDecode(Line);
  Line := Fulltrim(Line);
  duration := Line;

  comm := '';
  BeginPos := 0;
  Trama_Trovata := 'no';
  InitChar := 'Trama:</span>';
  BeginPos := Pos(InitChar, PageStr);
  if  BeginPos > 0 then
      trama_trovata := 'si';
  if  BeginPos = 0 then
      begin
        InitChar := 'Descrizione articolo:</span>';
        BeginPos := Pos(InitChar, PageStr);
      end;
  Delete(PageStr, 1, BeginPos-1);
  Line := Textbetween(PageStr, InitChar, '</td>');
  HTMLRemoveTags(Line);
  HTMLDecode(Line);
  Line := stringreplace(Line, crlf, '');
  Line := stringreplace(Line, #09, '');
  Line := Fulltrim(Line);
  description := UTF8Decode(Line);

//  InitChar := '<span class="article_sheet_datalabel">';
  InitChar := 'Descrizione articolo:</span>';
  Line := Textbetween(PageStr, InitChar, '</td>');
  BeginPos := Pos(InitChar, PageStr);
  Delete(PageStr, 1, BeginPos);
  HTMLRemoveTags(Line);
  HTMLDecode(Line);
  Line := stringreplace(Line, crlf, '');
  Line := stringreplace(Line, #09, '');
  Line := Fulltrim(Line);
//  Comm := Comm + Line + crlf;
  comments := UTF8Decode(Line);
  Page.Free;
  
  SharedSecureSetAllFields(originalTitle, translatedTitle, '', year, producers, '', '', directors, actors, duration, categories, '', countries, description, comments, SharedGetMovieUrl(), picture);
end;


// --------------------------------------------------------------------------------------------------------------------------------------
//                                               "private" functions e procedures
// --------------------------------------------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------
// Formatta la stringa cercando le prime lettere rendendole maiuscole
// ------------------------------------------------------------------
function KultVideoMixedCaseText(str: string): string;
begin
  str := AnsiLowerCase(str);
  str := AnsiMixedCase(str, ' -/');
  Result := str;
end;


// ------------------------------------------------------
// FILL PICKTREE CONTROL WITH LINKS & TITLES
// IN:  none
// OUT: post data parameters needed for next url research
// ------------------------------------------------------
function KultVideoPopulatePickTree(): string;
var
  Page: TStringList;
  LineNr, Linesup: Integer;
  Line, Supporto: string;
  MyMovieTitle, MyMovieUrl, MyLastValidMovieUrl: string;
  BeginPos, EndPos: Integer;
  nextPageAvailable: Boolean;
begin 
  SharedPickTreeCreate();
  Page := TStringList.Create;
  Page.Text := SharedGetLatestPageHtml(); 
  LineNr := 0;
  LineNr := FindLine('<div class="artlist_artpicture">', Page, LineNr);
  MyLastValidMovieUrl := '';
//  LineSup := LineNr;
  While (LineNR <> -1) Do
  Begin
    Line := Page.GetString(LineNr);  
  // Crea l'url per la pagina completa del film
    LineNr := LineNr + 1;
    Line := Page.GetString(LineNr);
    MyMovieUrl := TextBetween(Line, '<a href="/', '" alt="');
    MyMovieUrl := KV_UrlBase + MyMovieUrl;
    Page.SetString(lineNR, ' ');

  //Estrazione Tipo supporto
    Linesup := FindLine('src="../img/entities/articletype/', Page, Linesup);
    Supporto := Page.GetString(Linesup);
    Linesup := LineSup + 1;
    //Decodifica il supporto
    Supporto := TextBetween(Supporto, 'articletype/', '"');
    if supporto = '1_1a.jpg' then
      supporto := ' (dvd)';
    if supporto = '2_1a.jpg' then
      supporto := ' (BluRay)';
    if supporto = '4_1a.gif' then
      supporto := ' (VHS)';
    if (supporto = '10_1a.jpg') or (supporto = '15_1a.jpg')or (supporto = '18_1a.jpg') then
      supporto := ' (poster)';

  //Estrazione Titolo film
    LineNr := FindLine('<td class="artlist_arttitle">', Page, LineNr);
    LineNr := LineNr + 1;
    MyMovieTitle := Page.GetString(LineNr);
    //Ripulisce il titolo
    HTMLRemoveTags(MyMovieTitle);
    HTMLDecode(MyMovieTitle);
    MyMovieTitle := Fulltrim(MyMovieTitle);
    HTMLRemoveTags(MyMovieTitle);
    HTMLDecode(MyMovieTitle);

    MyMovieTitle := Fulltrim(MyMovieTitle) + Supporto;
    MyMovieTitle := UTF8Decode(MyMovieTitle);
    if (pos('CD - ', MyMovieTitle) <> 1) then
    begin
      MyLastValidMovieUrl := MyMovieUrl;
      SharedPickTreeAdd(MyMovieTitle, '', '', '', MyMovieUrl);
    end;      
      
    LineNr := FindLine('<div class="artlist_artpicture">', Page, LineNr);
  end;
  Page.Free;
  
  Line := textBetween(SharedGetLatestPageHtml(), 'pager_boxed_item_current', 'pager_boxed_item');
  if (Line = '') then
    result := ''
  else
  begin  
    Line := textBetween(Line, '(', ')');
    result := '__EVENTTARGET=' + SharedAdvancedUrlEncode(textBetween(Line, '''', ''''), false);
    result := result + '&__EVENTARGUMENT=' + SharedAdvancedUrlEncode(textBetween(Line, ',''', ''''), false);
    result := result + '&__VIEWSTATE=' + SharedAdvancedUrlEncode(textBetween(SharedGetLatestPageHtml(), 'id="__VIEWSTATE" value="', '"'), false);
    result := result + '&__VIEWSTATEGENERATOR=' + SharedAdvancedUrlEncode(textBetween(SharedGetLatestPageHtml(), '__VIEWSTATEGENERATOR" value="', '"'), false);
    result := result + '&__lang=';
    result := result + '&ctl00%24tbSearchField=' + SharedAdvancedUrlEncode(textBetween(SharedGetLatestPageHtml(), 'name="ctl00$tbSearchField" type="text" value="' , '"'), false);
    result := result + '&ctl00_MainContent_objArticlesList_sortOrder=TA';
    result := result + '&ctl00%24MainContent%24objArticlesList_sortOrder=TA';
    result := result + '&ctl00%24tbLoginUsername=';
    result := result + '&ctl00%24tbLoginPassword=';
  end;
  nextPageAvailable := (result <> '');
  SharedPickTreeClose(nextPageAvailable);
end;

end.
