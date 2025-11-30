unit FilmTvPas;

{
  Main var, constants and functions
}
uses
  ItalianSharedPas;

const
  FT_UnitVersion = 4;

  //Query constants
  FT_UrlBase = 'https://www.filmtv.it';
  FT_QueryBase = FT_UrlBase + '/cerca/?titolo=';
  FT_QueryFilm = FT_UrlBase + '';

  //Search page constants
  FT_Apice = #39;
  FT_xFilmSearchStart = '<article';                              // HTML start marker movie block
  FT_xFilmSearchEnd = '</article>';                              // HTML end marker movie block
  FT_xResultTypeStart = '<div class="etichetta-serp">';
  // Block type start marker. Possible blocks: FILM, RECENSIONE, POST ...
  FT_xResultTypeEnd = '<';                                       // Block type end marker
  FT_xFilmUrlStart = '<h2 class="title-item-scheda"><a href="';  // URL start marker
  FT_xFilmUrlEnd = '"';                                          // URL end marker
  FT_xTitleStart = ' title="';                                   // Title start marker
  FT_xTitleEnd = '"';                                            // Title end marker
  FT_xOriginalTitleStart = '<li>Titolo originale: <span>';       // Original title start marker
  FT_xOriginalTitleEnd = '<span>';                               // Original title end marker
  FT_xDirectorsStart = 'Regia: <span>';                          // Directors start marker
  FT_xDirectorsEnd = '</span>';                                  // Directors end marker
  FT_xYearStart = '<span class="anno">';                         // Year start marker
  FT_xYearEnd = '</span>';                                       // Year end marker

  //Film page constants
  FT_cTranslTitleStart = '<div class="wrap-head">';    // Translated title start marker
  FT_cTranslTitleEnd = '<div class=';                  // Translated title end marker
  FT_cOrigTitleStart = '[<i>';                         // Original title start marker
  FT_cOrigTitleEnd = '</i>';                           // Original title end marker
  FT_cImgLinkExtStart = '<figure class="locandina">';  // Image start marker
  FT_cImgLinkExtEnd = '</figure>';                     // Image end marker  
  FT_cImgLinkIntStart = '<img src="';                  // Image start marker
  FT_cImgLinkIntEnd = '"';                             // Image end marker
  FT_cDirectorStart = 'Regia di';                      // Single Director start marker
  FT_cDirectorEnd = '</p>';                            // Single Director end marker
  FT_cCastStart = 'Con</span>';                        // Cast start marker
  FT_cCastEnd = '<strong>';                            // Cast end marker
  FT_cDescStart = '<h2>Trama</h2>';                    // Description start marker  //FS2012-11-11
  FT_cDescEnd = '</p>';                                // Description end marker
  FT_cCommStart = '<blockquote>';                      //fs2013-02-11
  FT_cCommEnd = '</blockquote>';                       // Comment end marker
  FT_cRatingStart = '<a class="vedi-voti"';
  FT_cRatingEnd = '</a>';


// --------------------------------------------------------------------------------------------------------------------------------------
//                                               "public" functions e procedures
// --------------------------------------------------------------------------------------------------------------------------------------

// -----------------------------------
// Main program (for MultiSite)
// IN: properties values (serialized)
// OUT: properties values (serialized)
// -----------------------------------
function FilmTvCoreMultiSite(serializedValues: string): string;
begin
  SharedUnserializeMe(serializedValues);
  // Analyze search page and get movie url (user choice)
  if (SharedGetMoviesFound() = 0) then
    FilmTvSearchResults();
  if (SharedGetMoviesFound() > 0) then // Analyze movie page and set movie fields
    FilmTvSetMovieFields();  
  result := SharedSerializeMe();    
end;


// --------------------------------
// ANALYZE FIRST SEARCH RESULT PAGE
// IN:  none
// --------------------------------
procedure FilmTvSearchResults;
var
  myUrl: string;
  pageNumberMovies: integer;
begin
  pageNumberMovies := 0;
  repeat
    SharedSetMovieUrl('');
    SharedSetMoviesFound(0);
    pageNumberMovies := pageNumberMovies + 1; 
    myUrl := AnsiLowerCaseNoAccents(SharedGetMovieName());
    myUrl := FT_QueryBase + SharedAdvancedUrlEncode(myUrl, false);
    if (SharedGetYearDate() <> '') then
      myUrl := myUrl + '&dal=' + IntToStr(StrToInt(SharedGetYearDate(), 0) - 5) + '&al=' + IntToStr(StrToInt(SharedGetYearDate(), 0) + 5);
    myUrl := myUrl + '&p=' + IntToStr(pageNumberMovies);
    SharedHTTPGetPage(myUrl);
    if (pos('Non hai trovato nulla!', SharedGetLatestPageHtml()) > 0) then
      exit;
    FilmTvPopulatePickTree();
    SharedPickTreeExec();
  until (copy(SharedGetMovieUrl(), 1, 5) <> 'next:');
end;


// -----------------------
// ANALYZE MOVIE DATA PAGE
// IN:  none
// -----------------------
procedure FilmTvSetMovieFields;
var
  originalTitle, translatedTitle, rating, year, producers, directors, actors, duration, categories, certification, countries, description, comments, picture: string;
  i, h, m: integer;
  CharPos, lungo, posbar: integer;
  TempStr, TempImg, hh, mm, temp: string;
  PosParAperta, PosParChiusa: integer;
  imageLink: string;
  sep1, sep2: string;
  htmlStr, tempvar: string;
  dati_generali, Title_Orig, Title_Transl, Paese, Anno, Categoria, durata, voto: string;
begin
  SharedHTTPGetPage(SharedGetMovieUrl());
  htmlStr := Stringreplace(SharedGetLatestPageHtml(), 'â€™', '´');
  Title_Transl := RemoveHTML(TextBetween(htmlStr, FT_cTranslTitleStart, FT_cTranslTitleEnd));
  Title_Transl := AnsiMixedCase(AnsiLowerCase(FullTrim(Title_Transl)), ' ');
  if (copy(Title_Transl, (Length(Title_Transl) - 3), 4) = ' (i)') then
    Title_Transl := copy(Title_Transl, 1, (Length(Title_Transl) - 4))
  else
  if (copy(Title_Transl, (Length(Title_Transl) - 4), 5) = ' (ii)') then
    Title_Transl := copy(Title_Transl, 1, (Length(Title_Transl) - 5))
  else
  if (copy(Title_Transl, (Length(Title_Transl) - 5), 6) = ' (iii)') then
    Title_Transl := copy(Title_Transl, 1, (Length(Title_Transl) - 6));   
    
  //get Translated title
  translatedTitle := Title_Transl;

  //Get original title
  Dati_generali := FullTrim(TextBetween(htmlStr, '<ul class="info cf">', '</ul>'));          //fs2016.09.28
  Title_Orig := TextBetween(Dati_generali, '<li><h2>', '</h2></li>');
  lungo := (length(Title_Orig) + 18);
  Title_Orig := FullTrim(Title_Orig);
  originalTitle := AnsiMixedCase(AnsiLowerCase(Title_Orig), '');
    
  //Get Rating, duration, countries, categories, country, year
  if (length(Title_Orig) > 0) then          //altri paesi
  begin
    Delete(Dati_generali, 1, lungo);
    Paese := TextBetween(Dati_generali, '<li>', '<time>');                //fs2016.09.29
    Anno := TextBetween(Dati_generali, '<time>', '</time>');
    Categoria := FullTrim(Textbetween(Dati_generali, '<span>Genere:</span>', '</li>'));
    durata := FullTrim(Textbetween(Dati_generali, '<span>durata</span>', '</li>'));
  end
  else
  begin                                              //Italia
    Paese := TextBetween(Dati_generali, '<li>', '<time>');
    Anno := TextBetween(Dati_generali, '<time>', '</time>');
    Categoria := FullTrim(Textbetween(Dati_generali, '<span>Genere:</span>', '</li>'));
    durata := FullTrim(Textbetween(Dati_generali, '<span>durata</span>', '</li>'));
  end;
  HTMLRemoveTags(Paese);
  PosBar := AnsiLastPosEx(',', Paese, true, true);
  if PosBar > 0 then
    Delete(Paese, (posbar), (length(Paese) - PosBar));           //cut old filename
  countries := FullTrim(Paese);
  voto := FT_cRatingStart + TextBetween(htmlStr, FT_cRatingStart, '</a>') + FT_cRatingEnd;
  rating := TextBetween(Voto, '<meter max="10" min="0" value="', '"');
  year := Anno;
  HTMLRemoveTags(Categoria);
  categories := Categoria;
  duration := Textbefore(durata, FT_Apice, '');

  //Get Director
  temp := TextBetween(htmlstr, FT_cDirectorStart, FT_cDirectorEnd);
  HTMLRemoveTags(temp);
  HTMLDecode(temp);
  directors := FullTrim(temp);

  //Get Actors
  temp := TextBetween(htmlStr, FT_cCastStart, FT_cCastEnd);
  HTMLRemoveTags(temp);                       //fs 2012-04-03
  HTMLDecode(temp);
  actors := FullTrim(temp);

  //Get description
  SharedDebugAppendToFile(htmlstr, '[FilmTvPas][FilmTvSetMovieFields] comments html');
  temp := Textbetween(htmlStr, FT_cDescStart, FT_cDescEnd);
  HTMLRemoveTags(temp);                       //fs 2012-04-03
  HTMLDecode(temp);
  temp := FullTrim(temp);
  if (temp = 'Trama in preparazione') then
    temp := '';
  description := temp;  

  //Get comments
  temp := TextBetween(HtmlStr, FT_cCommStart, FT_cCommEnd);
  HTMLRemoveTags(temp);
  HTMLDecode(temp);
  comments := FullTrim(temp);

  //Get image     https://ftv01.stbm.it/imgbank/GALLERYXL/R201404/the_english_teacher_poster_ita.jpg
  temp := TextBetween(htmlStr, FT_cImgLinkExtStart, FT_cImgLinkExtEnd); 
  temp := TextBetween(temp, FT_cImgLinkIntStart, FT_cImgLinkIntEnd);
  if (pos('/no_locandina.jpg', temp) > 0) then
    temp := ''
  else // Check if it's a valid JPEG image and not a WEBP or invalid image
  if (copy(GetPage(temp), 0, 4) <> (Chr(195) + Chr(191) + Chr(195) + Chr(152))) then  
    temp := '';
  picture := temp;

  SharedSecureSetAllFields(originalTitle, translatedTitle, rating, year, '', '', '', directors, actors, duration, categories, '', countries, description, comments, SharedGetMovieUrl(), picture);
end;


// --------------------------------------------------------------------------------------------------------------------------------------
//                                               "private" functions e procedures
// --------------------------------------------------------------------------------------------------------------------------------------

// -----------------------------------------------------------
// Returns a text field from an input string strIn (html page)
// delIn and delOut: text field delimiters
// -----------------------------------------------------------
function FilmTvGetTextDelimited(lIn: string; lOut: string; var strIn: string): string;
var
  StartPos, len: integer;
  tempStr: string;
begin
  StartPos := pos(lIn, strIn);
  if StartPos <> 0 then
  begin
    StartPos := StartPos + Length(lIn);
    tempStr := copy(strIn, StartPos, StartPos + length(strIn));
    len := pos(lOut, tempStr);
    if len = 0 then
      result := ''
    else
      result := copy(strIn, StartPos, len - 1); // get Text Out
    Delete(strIn, 1, StartPos - 1 + len - 1 + length(lOut) - 2);
  end
  else
    result := '';
  HTMLDecode(result);
end;

// -----------------------------------------
// FILL PICKTREE CONTROL WITH LINKS & TITLES
// IN:  none
// -----------------------------------------
procedure FilmTvPopulatePickTree();
var
  filmHTML, filmHTMLfull, filmUrl, partialUrl, resultType: string;
  filmTitle, filmOriginalTitle, filmYear, filmDirectors: string;
  nextPageAvailable: boolean;
begin
  SharedPickTreeCreate();  
  filmHTMLfull := SharedGetLatestPageHtml();
  repeat
    filmHTML := FilmTvGetTextDelimited(FT_xFilmSearchStart, FT_xFilmSearchEnd, filmHTMLfull);
    resultType := FilmTvGetTextDelimited(FT_xResultTypeStart, FT_xResultTypeEnd, filmHTML);
    if ((resultType <> 'RECENSIONE') and (resultType <> 'POST') and (resultType <> 'NICKNAME')) then
    begin
      partialUrl := FilmTvGetTextDelimited(FT_xFilmUrlStart, FT_xFilmUrlEnd, filmHTML);
      if (partialUrl <> '') then
      begin
        filmUrl := FT_UrlBase + partialUrl;
        filmTitle := FilmTvGetTextDelimited(FT_xTitleStart, FT_xTitleEnd, filmHTML);
        filmOriginalTitle := FilmTvGetTextDelimited(FT_xOriginalTitleStart, FT_xOriginalTitleEnd, filmHTML);
        filmYear := FilmTvGetTextDelimited(FT_xYearStart, FT_xYearEnd, filmHTML);
        filmDirectors := FilmTvGetTextDelimited(FT_xDirectorsStart, FT_xDirectorsEnd, filmHTML);
        if pos('/nick/', filmUrl) = 0 then
          if SharedPickTreeMatching(filmOriginalTitle, filmTitle, filmYear, filmDirectors, '') then
            SharedSetMovieUrl(filmUrl)
          else  
            SharedPickTreeAdd(filmTitle, filmOriginalTitle, filmDirectors, filmYear, filmUrl);
      end;
    end;
  until ((partialUrl = '') OR (SharedGetMovieUrl() <> ''));
  
  nextPageAvailable := (pos('"Vai alla prossima pagina"', filmHTMLfull) > 0);
  SharedPickTreeClose(nextPageAvailable);
end;

end.
