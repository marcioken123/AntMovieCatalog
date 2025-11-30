unit ImdbPas;

{
  Main IMDb var, constants and functions
}
uses
  ItalianSharedPas;

const
  ID_UnitVersion = 12;

var
  MovieName: string;
  MovieURL: string;
  MovieNumber: string;
  MovieYear: string;
  UpdateFile: TStringList;
  

// -----------------------------------
// Main program (for MultiSite)
// IN: properties values (serialized)
// OUT: properties values (serialized)
// -----------------------------------
function IMDbCoreMultiSite(serializedValues: string): string;
var
  italianDescription, italianComments: string;
begin
  italianDescription := getField(fieldDescription);     
  italianComments := getField(fieldComments);
  SharedUnserializeMe(serializedValues); 
  if (GetField(fieldYear) <> '') then
    SharedSetYearDate(GetField(fieldYear));    
  if (GetField(fieldDirector) <> '') then
    SharedSetDirectorName(GetField(fieldDirector)); 
  if (GetField(fieldTranslatedTitle) <> '') then
  begin
    SharedSetTranslatedStr(GetField(fieldTranslatedTitle));    
    // if translated and original titles are both set, use original title (IMDb advanced search doesn't look for translated titles)
    SH_MovieName := GetField(fieldOriginalTitle);
  end;  
  if (GetField(fieldOriginalTitle) <> '') then
    SharedSetOriginalStr(GetField(fieldOriginalTitle));    

  // Analyze search page and get movie url (user choice)
  if (SharedGetMoviesFound() = 0) then
    IMDbSearchResults();
  if (SharedGetMoviesFound() > 0) then // Analyze movie page and set movie fields
    IMDbSetMovieFields();
  
  if (italianDescription <> '') then
    SharedSecureSetField(fieldDescription, italianDescription);
  SharedSecureSetField(fieldComments, italianComments);

  result := SharedSerializeMe();  
end;



procedure IMDbSearchResults;
var
  myUrl, myImdbId: string;
  pageNumberMovies: integer;
begin
  pageNumberMovies := 0;   
  
  // accetta come input sia "tt1234567" che "qualcosa tt1234567 qualcosa"
  myImdbId := 'tt' + TextBetween(('.' + fulltrim(StringReplace(SharedGetMovieName(), ' ', '.')) + '.'), '.tt', '.');    
  if ((Length(myImdbId) > 6) AND (RegExprSetExec('tt[0-9]+', myImdbId))) then
  begin
    SharedSetMovieUrl('https://www.imdb.com/it/title/' + myImdbId + '/reference');
    SharedSetMoviesFound(1);
  end
  else
  begin
    repeat
      SharedSetMovieUrl('');
      SharedSetMoviesFound(0);
      // primo tentativo: ricerca avanzata
      myUrl := 'https://www.imdb.com/it/search/title/?title=' + SharedAdvancedUrlEncode(UTF8Encode(SharedGetMovieName()), false) + '&title_type=feature,tv_movie,tv_series,tv_episode,tv_special,tv_miniseries,short,video,tv_short&adult=include';
      if (SharedGetYearDate() <> '') then
        myUrl := myUrl + '&release_date=' + SharedAdvancedUrlEncode((IntToStr(StrToInt(SharedGetYearDate(), 0) - 3) + '-01-01,' +
          IntToStr(StrToInt(SharedGetYearDate(), 0) + 3) + '-12-31'), false);
      if (pageNumberMovies > 0) then
        myUrl := myUrl + '&start=' + IntToStr(((pageNumberMovies * 50) + 1)) + '&ref_=adv_nxt';
      SharedHTTPGetPage(myUrl);     
      IMDbPopulatePickTree();

      // se il primo tentativo fallisce (succede quando la ricerca avviene con un titolo che per iMDB non è l'originale), provo con la ricerca semplice
      {
      if (SharedGetMoviesFound() = 0) then
      begin
        myUrl := 'https://www.imdb.com/find/?q=' + SharedAdvancedUrlEncode(UTF8Encode(SharedGetMovieName()), false) + '&s=tt&exact=true&ref_=fn_tt_ex';
      end;
      }
      
      // se non ho trovato nessun titolo oppure non ho trovato una url certa e avevo impostato la ricerca IMDB a 1 allora esco
      if ((SharedGetMovieUrl() = '') AND (GetParam('IMDB Ricerca') = '1')) then
        SharedSetMoviesFound(0);
      if (SharedGetMoviesFound() = 0) then
      begin
        SharedSetForceExit(true);
        exit;
      end;  

      SharedPickTreeExec();
      pageNumberMovies := pageNumberMovies + 1; 
    until (copy(SharedGetMovieUrl(), 1, 5) <> 'next:');
  end;
end;




// ***** retrieve director from PageText

function IMDBRetrieveDirectorFromPage(PageText: string): string;
var
  Value: string;
begin
  Value := getField(fieldDirector);
  jsonMultipleValuesToList(PageText, '"directorsPageTitle":[', '"PrincipalCreditsForCategory"}]', '"nameText":{"text":"', '"', fieldDirector, 0);
  Result := getField(fieldDirector);
  setField(fieldDirector, Value);
end;



// -----------------------
// ANALYZE MOVIE DATA PAGE
// IN:  none
// -----------------------
procedure IMDbSetMovieFields;
begin
  if (SharedGetLatestPageUrl() <> SharedGetMovieUrl()) then
    SharedHTTPGetPage(SharedGetMovieUrl()); 
  if (SharedGetLatestPageHtml() <> '') then
    AnalyzeMoviePage(SharedGetLatestPageHtml());
  IMDbFinalize();
end;



procedure IMDbPopulatePickTree;
var
  PageText, JsonString, MText, MTitleType, MTitle, MTitleTranslated, MYear, Mdirectors, MURL, MURLS: string;
  NextPageAvailable, firstResult: boolean;  
begin
  NextPageAvailable := false; //(pos('<span class="ipc-see-more__text">', SharedGetLatestPageHtml()) > 0);
  SharedPickTreeCreate();  
  MURLS := '';
  firstResult := true; 
  if ((pos('<span>No results.</span>', SharedGetLatestPageHtml()) = 0) AND (pos('"searchResults":{', SharedGetLatestPageHtml()) <> 0)) then
  begin
    PageText := textBetween(SharedGetLatestPageHtml(), '"searchResults":{', '},"searchInput":{'); 
    repeat
      Delete(PageText, 1, (pos('"canRate"', PageText) + 9));
      if (pos('"canRate"', PageText) = 0) then
        MText := PageText
      else
        MText := copy(PageText, 1, pos('"canRate"', PageText));
      MTitle := textBetween(MText, '"originalTitleText":"', '","');
      MTitleTranslated := textBetween(MText, '"titleText":"', '","');
      MURL := 'https://www.imdb.com/it/title/' + textBetween(MText, '"titleId":"', '","') + '/reference';
      MURLS := MURLS + MURL + ' ';
      MYear := textBetween(MText, '"year":', ',');
      JsonString := textBetween(MText, '"titleType":{', '},');
      MTitleType := textBetween(JsonString, '"text":"', '"');
      if (MTitleType = '') then
        MTitleType := textBetween(JsonString, '"id":"', '"');
      // i primi risultati sono probabilmente quelli giusti ma ho bisogno del regista per conferma e se non ce l'ho lo recupero (ma solo una volta a ciclo perché devo fare una chiamata ad una pagina esterna)
      Mdirectors := '';
      if (SharedGetDirectorName() = '') then
        SharedPickTreeAdd(MTitle, '[' + MTitleType + '] ' + MTitleTranslated, Mdirectors, MYear, MURL)
      else
      begin
        // conosco il regista: lo ricavo (solo per il primo risultato restituito) e verifico se matcha 
        if ((firstResult) and (not SharedPickTreeImdbIdMatching(MURL))) then
        begin
          SharedHTTPGetPage(MURL);
          Mdirectors := IMDBRetrieveDirectorFromPage(SharedGetLatestPageHtml());    
          firstResult := false;         
        end;
        // se tutto matcha ho trovato il mio film, altrimenti mi limito ad aggiungere i dati alla lista
        if (SharedPickTreeMatching(MTitle, MTitleTranslated, MYear, Mdirectors, MURL)) then
          SharedSetMovieUrl(MURL)
        else  
          SharedPickTreeAdd(MTitle, '[' + MTitleType + '] ' + MTitleTranslated, Mdirectors, MYear, MURL);
      end;  
    until ((pos('"canRate"', PageText) = 0) OR (SharedGetMovieUrl() <> ''));
  end;
  
  SharedPickTreeClose(NextPageAvailable);
end;


procedure IMDbFinalize;
begin
  if ((GetOption('IMDB Locandina italiana') = 1) OR (GetPictureStatus() = picStatusNone)) then
    SharedGetPicture();
  if (GetField(fieldTranslatedTitle) = '') then
    SetField(fieldTranslatedTitle, SharedGetOriginalStr());
end;



//----------------------------------------------------------------------------------------------- 
 
function ConvertToASCII(AText: string): string;
begin
  Result := UTF8Decode(AText);
  if Result = '' then
    Result := AText; // in case of a UTF8 decoding error
  if GetOption('IMDB Converti in ASCII') = 1 then
    Result := Cp1252ToASCII(Result);
end;

function AKACountry: string;
begin
  case AnsiLowerCase(fulltrim(GetParam('IMDB Localizzazione'))) of
  'andorra': result := 'AD';
  'united arab emirates': result := 'AE';
  'afghanistan': result := 'AF';
  'antigua and barbuda': result := 'AG';
  'anguilla': result := 'AI';
  'albania': result := 'AL';
  'armenia': result := 'AM';
  'angola': result := 'AO';
  'antarctica': result := 'AQ';
  'argentina': result := 'AR';
  'american samoa': result := 'AS';
  'austria': result := 'AT';
  'australia': result := 'AU';
  'aruba': result := 'AW';
  'åland islands': result := 'AX';
  'azerbaijan': result := 'AZ';
  'bosnia and herzegovina': result := 'BA';
  'barbados': result := 'BB';
  'bangladesh': result := 'BD';
  'belgium': result := 'BE';
  'burkina faso': result := 'BF';
  'bulgaria': result := 'BG';
  'bahrain': result := 'BH';
  'burundi': result := 'BI';
  'benin': result := 'BJ';
  'saint barthélemy': result := 'BL';
  'bermuda': result := 'BM';
  'brunei darussalam': result := 'BN';
  'bolivia (plurinational state of)': result := 'BO';
  'bonaire, sint eustatius and saba': result := 'BQ';
  'brazil': result := 'BR';
  'bahamas': result := 'BS';
  'bhutan': result := 'BT';
  'bouvet island': result := 'BV';
  'botswana': result := 'BW';
  'belarus': result := 'BY';
  'belize': result := 'BZ';
  'canada': result := 'CA';
  'cocos (keeling) islands': result := 'CC';
  'congo, democratic republic of the': result := 'CD';
  'central african republic': result := 'CF';
  'congo': result := 'CG';
  'switzerland': result := 'CH';
  'côte d''ivoire': result := 'CI';
  'cook islands': result := 'CK';
  'chile': result := 'CL';
  'cameroon': result := 'CM';
  'china': result := 'CN';
  'colombia': result := 'CO';
  'costa rica': result := 'CR';
  'cuba': result := 'CU';
  'cabo verde': result := 'CV';
  'curaçao': result := 'CW';
  'christmas island': result := 'CX';
  'cyprus': result := 'CY';
  'czechia': result := 'CZ';
  'germany': result := 'DE';
  'djibouti': result := 'DJ';
  'denmark': result := 'DK';
  'dominica': result := 'DM';
  'dominican republic': result := 'DO';
  'algeria': result := 'DZ';
  'ecuador': result := 'EC';
  'estonia': result := 'EE';
  'egypt': result := 'EG';
  'western sahara': result := 'EH';
  'eritrea': result := 'ER';
  'spain': result := 'ES';
  'ethiopia': result := 'ET';
  'finland': result := 'FI';
  'fiji': result := 'FJ';
  'falkland islands (malvinas)': result := 'FK';
  'micronesia (federated states of)': result := 'FM';
  'faroe islands': result := 'FO';
  'france': result := 'FR';
  'gabon': result := 'GA';
  'united kingdom of great britain and northern ireland': result := 'GB';
  'grenada': result := 'GD';
  'georgia': result := 'GE';
  'french guiana': result := 'GF';
  'guernsey': result := 'GG';
  'ghana': result := 'GH';
  'gibraltar': result := 'GI';
  'greenland': result := 'GL';
  'gambia': result := 'GM';
  'guinea': result := 'GN';
  'guadeloupe': result := 'GP';
  'equatorial guinea': result := 'GQ';
  'greece': result := 'GR';
  'south georgia and the south sandwich islands': result := 'GS';
  'guatemala': result := 'GT';
  'guam': result := 'GU';
  'guinea-bissau': result := 'GW';
  'guyana': result := 'GY';
  'hong kong': result := 'HK';
  'heard island and mcdonald islands': result := 'HM';
  'honduras': result := 'HN';
  'croatia': result := 'HR';
  'haiti': result := 'HT';
  'hungary': result := 'HU';
  'indonesia': result := 'ID';
  'ireland': result := 'IE';
  'israel': result := 'IL';
  'isle of man': result := 'IM';
  'india': result := 'IN';
  'british indian ocean territory': result := 'IO';
  'iraq': result := 'IQ';
  'iran (islamic republic of)': result := 'IR';
  'iceland': result := 'IS';
  'italy': result := 'IT';
  'jersey': result := 'JE';
  'jamaica': result := 'JM';
  'jordan': result := 'JO';
  'japan': result := 'JP';
  'kenya': result := 'KE';
  'kyrgyzstan': result := 'KG';
  'cambodia': result := 'KH';
  'kiribati': result := 'KI';
  'comoros': result := 'KM';
  'saint kitts and nevis': result := 'KN';
  'korea (democratic people''s republic of)': result := 'KP';
  'korea, republic of': result := 'KR';
  'kuwait': result := 'KW';
  'cayman islands': result := 'KY';
  'kazakhstan': result := 'KZ';
  'lao people''s democratic republic': result := 'LA';
  'lebanon': result := 'LB';
  'saint lucia': result := 'LC';
  'liechtenstein': result := 'LI';
  'sri lanka': result := 'LK';
  'liberia': result := 'LR';
  'lesotho': result := 'LS';
  'lithuania': result := 'LT';
  'luxembourg': result := 'LU';
  'latvia': result := 'LV';
  'libya': result := 'LY';
  'morocco': result := 'MA';
  'monaco': result := 'MC';
  'moldova, republic of': result := 'MD';
  'montenegro': result := 'ME';
  'saint martin (french part)': result := 'MF';
  'madagascar': result := 'MG';
  'marshall islands': result := 'MH';
  'north macedonia': result := 'MK';
  'mali': result := 'ML';
  'myanmar': result := 'MM';
  'mongolia': result := 'MN';
  'macao': result := 'MO';
  'northern mariana islands': result := 'MP';
  'martinique': result := 'MQ';
  'mauritania': result := 'MR';
  'montserrat': result := 'MS';
  'malta': result := 'MT';
  'mauritius': result := 'MU';
  'maldives': result := 'MV';
  'malawi': result := 'MW';
  'mexico': result := 'MX';
  'malaysia': result := 'MY';
  'mozambique': result := 'MZ';
  'namibia': result := 'NA';
  'new caledonia': result := 'NC';
  'niger': result := 'NE';
  'norfolk island': result := 'NF';
  'nigeria': result := 'NG';
  'nicaragua': result := 'NI';
  'netherlands, kingdom of the': result := 'NL';
  'norway': result := 'NO';
  'nepal': result := 'NP';
  'nauru': result := 'NR';
  'niue': result := 'NU';
  'new zealand': result := 'NZ';
  'oman': result := 'OM';
  'panama': result := 'PA';
  'peru': result := 'PE';
  'french polynesia': result := 'PF';
  'papua new guinea': result := 'PG';
  'philippines': result := 'PH';
  'pakistan': result := 'PK';
  'poland': result := 'PL';
  'saint pierre and miquelon': result := 'PM';
  'pitcairn': result := 'PN';
  'puerto rico': result := 'PR';
  'palestine, state of': result := 'PS';
  'portugal': result := 'PT';
  'palau': result := 'PW';
  'paraguay': result := 'PY';
  'qatar': result := 'QA';
  'réunion': result := 'RE';
  'romania': result := 'RO';
  'serbia': result := 'RS';
  'russian federation': result := 'RU';
  'rwanda': result := 'RW';
  'saudi arabia': result := 'SA';
  'solomon islands': result := 'SB';
  'seychelles': result := 'SC';
  'sudan': result := 'SD';
  'sweden': result := 'SE';
  'singapore': result := 'SG';
  'saint helena, ascension and tristan da cunha': result := 'SH';
  'slovenia': result := 'SI';
  'svalbard and jan mayen': result := 'SJ';
  'slovakia': result := 'SK';
  'sierra leone': result := 'SL';
  'san marino': result := 'SM';
  'senegal': result := 'SN';
  'somalia': result := 'SO';
  'suriname': result := 'SR';
  'south sudan': result := 'SS';
  'sao tome and principe': result := 'ST';
  'el salvador': result := 'SV';
  'sint maarten (dutch part)': result := 'SX';
  'syrian arab republic': result := 'SY';
  'eswatini': result := 'SZ';
  'turks and caicos islands': result := 'TC';
  'chad': result := 'TD';
  'french southern territories': result := 'TF';
  'togo': result := 'TG';
  'thailand': result := 'TH';
  'tajikistan': result := 'TJ';
  'tokelau': result := 'TK';
  'timor-leste': result := 'TL';
  'turkmenistan': result := 'TM';
  'tunisia': result := 'TN';
  'tonga': result := 'TO';
  'türkiye': result := 'TR';
  'trinidad and tobago': result := 'TT';
  'tuvalu': result := 'TV';
  'taiwan, province of china[note 1]': result := 'TW';
  'tanzania, united republic of': result := 'TZ';
  'ukraine': result := 'UA';
  'uganda': result := 'UG';
  'united states minor outlying islands': result := 'UM';
  'united states': result := 'US';
  'uruguay': result := 'UY';
  'uzbekistan': result := 'UZ';
  'holy see': result := 'VA';
  'saint vincent and the grenadines': result := 'VC';
  'venezuela (bolivarian republic of)': result := 'VE';
  'virgin islands (british)': result := 'VG';
  'virgin islands (u.s.)': result := 'VI';
  'viet nam': result := 'VN';
  'vanuatu': result := 'VU';
  'wallis and futuna': result := 'WF';
  'samoa': result := 'WS';
  'yemen': result := 'YE';
  'mayotte': result := 'YT';
  'south africa': result := 'ZA';
  'zambia': result := 'ZM';
  'zimbabwe': result := 'ZW';
  else 
  begin
    ShowMessage('Invalid country (' + GetParam('IMDB Localizzazione') + '), set default one (United States of America). Please use a valid country written in english as it is on IMDb, e.g. one of these: United States, Canada, Mexico, Brazil, Argentina, Australia, India, Italy, Spain, Portugal, France, Germany, Netherlands, United Kingdom, Ireland, Finland, Norway, Sweden, Switzerland, ....');
    SetParam('IMDB Localizzazione', 'United States of America');
    result := 'US';  
  end;  
  end;
end;


// ***** decode rapidApi charset ****

function decodeRapidApiCharsetToCP1252(str: string): string;
begin
  // Romanian chars fix
  str := StringReplace(str, (chr(195) + chr(136) + chr(194)), chr(200)); 
  
  // UTF-8 fix
  str := StringReplace(str, (chr(195) + chr(130) + chr(194)), chr(194));  
  str := StringReplace(str, (chr(195) + chr(131) + chr(194)), chr(195));
  str := StringReplace(str, (chr(195) + chr(132) + chr(194)), chr(196)); 
  str := StringReplace(str, (chr(195) + chr(133) + chr(194)), chr(197));  
  
  result := UTF8Decode(str);
  if (result = '') then
    result := str;
end;


// ***** retrieve alternative titles from an external api site *****
procedure retrieveTitlesFromExternalAPI(Headers, Url, FirstElem, RegionElem, LanguageElem, TitleElem, ErrorMsg: string);
var
  JsonResponse, Delimiter, Lang, Country, MyCountry, CountryTitle, CountryTitleLowercase: string;
  WorldwideTitles, TranslatedTitles, CountryTitles: string;
begin
  Delimiter := ' || ';
  WorldwideTitles := Delimiter;
  TranslatedTitles := Delimiter;
  CountryTitles := Delimiter;
  JsonResponse := GetPage5(Url, '', '', '', Headers);
  // if rapidapi fails, retry again
  if (JsonResponse = '') then
    JsonResponse := GetPage5(Url, '', '', '', Headers);
  JsonResponse := decodeRapidApiCharsetToCP1252(JsonResponse);
  MyCountry := AKACountry();
  // remove unneeded json (first part)  
  Delete(JsonResponse, 1, pos(FirstElem, JsonResponse));    
  if (pos(RegionElem, JsonResponse) = 0) then
    ShowMessage(ErrorMsg)
  else
  while (pos(RegionElem, JsonResponse) > 0) do
  begin
    Lang := copy(JsonResponse, 0, pos(RegionElem, JsonResponse));
    Lang := fulltrim(textbetween(Lang, LanguageElem,'"')); // note: "language" could be not defined for some regions     
    Country := fulltrim(textbetween(JsonResponse, RegionElem,'"'));   
    CountryTitle := fulltrim(textbetween(JsonResponse, TitleElem,'"'));  
    CountryTitleLowercase := AnsiLowerCase(CountryTitle);        
    Delete(JsonResponse, 1, (pos('}', JsonResponse) + 1)); // remove this entry    
    if ((Lang = 'ar') OR (Lang = 'bg') OR (Lang = 'yue') OR (Lang = 'hi') OR (Lang = 'ja') OR (Lang = 'cmn') OR (Lang = 'ru') OR (Country = 'VN')) then
      CountryTitle := ''
    else
    if ((Country = 'AE') OR (Country = 'BY') OR (Country = 'BG') OR (Country = 'CN') OR (Country = 'EG') OR (Country = 'GR') OR (Country = 'HK') OR (Country = 'IN') OR (Country = 'KZ') OR (Country = 'KR') OR (Country = 'JP') OR (Country = 'RU') OR (Country = 'SA') OR (Country = 'RS') OR (Country = 'RU') OR (Country = 'TH') OR (Country = 'TW') OR (Country = 'UA') OR (Country = 'VN')) then
      if ((pos('Ð', CountryTitle) > 0) OR (pos(chr(186), CountryTitle) > 0) OR (pos(chr(191), CountryTitle) > 0) OR (pos(chr(194), CountryTitle) > 0) OR (pos(chr(195), CountryTitle) > 0) OR (pos(chr(196), CountryTitle) > 0) OR (pos(chr(197), CountryTitle) > 0) OR (pos(chr(198), CountryTitle) > 0) OR (pos(chr(206), CountryTitle) > 0) OR (pos(chr(208), CountryTitle) > 0) OR (pos(chr(190), CountryTitle) > 0) OR (pos(chr(180), CountryTitle) > 0) OR (pos(chr(228), CountryTitle) > 0) OR (pos(chr(229), CountryTitle) > 0) OR (pos(chr(230), CountryTitle) > 0) OR (pos(chr(231), CountryTitle) > 0) OR (pos(chr(232), CountryTitle) > 0)) then        
        CountryTitle := ''; 
    if (CountryTitle <> '') then
    begin
      if (Country = MyCountry) then
      begin
        if (pos(Delimiter + CountryTitle + Delimiter, CountryTitles) = 0) then
          CountryTitles := CountryTitles + CountryTitle + Delimiter;
        WorldwideTitles := StringReplace(WorldwideTitles, Delimiter + CountryTitle + Delimiter, Delimiter);
        TranslatedTitles := StringReplace(TranslatedTitles, Delimiter + CountryTitle + Delimiter, Delimiter);
      end  
      else if ((CountryTitleLowercase <> AnsiLowerCase(GetField(fieldOriginalTitle))) AND (pos(Delimiter + CountryTitleLowercase + Delimiter, AnsiLowerCase(CountryTitles)) = 0) AND (pos(Delimiter + CountryTitleLowercase + Delimiter, AnsiLowerCase(WorldwideTitles)) = 0)) then
      begin
        if ((Country = 'XWW') OR (Country = 'US') OR (Country = '')) then
        begin
          WorldwideTitles := WorldwideTitles + CountryTitle + Delimiter;
          TranslatedTitles := StringReplace(TranslatedTitles, Delimiter + CountryTitle + Delimiter, Delimiter);
        end  
        else if (pos(Delimiter + CountryTitle + Delimiter, TranslatedTitles) = 0) then  
          TranslatedTitles := TranslatedTitles + CountryTitle + Delimiter;
      end;
    end; 
  end;
  
  // all titles
  if (GetOption('IMDB Titolo Tradotto') = 1) then  
  begin  
    if ((Length(WorldwideTitles) <= Length(Delimiter)) AND (Length(CountryTitles) <= Length(Delimiter))) then
    begin
      // if no country titles or worldwide titles are available, I set as first translated title the original one.
      TranslatedTitles := Delimiter + GetField(fieldOriginalTitle) + TranslatedTitles;
    end
    else
    begin
      if (Length(WorldwideTitles) > Length(Delimiter)) then
        TranslatedTitles := copy(WorldwideTitles, 1, Length(WorldwideTitles) - Length(Delimiter)) + TranslatedTitles;
      if (Length(CountryTitles) > Length(Delimiter)) then
        TranslatedTitles := copy(CountryTitles, 1, Length(CountryTitles) - Length(Delimiter)) + TranslatedTitles;
    end;    
    if (Length(TranslatedTitles) > Length(Delimiter)) then
      SharedSecureSetField(fieldTranslatedTitle, copy(TranslatedTitles, Length(Delimiter) + 1, (Length(TranslatedTitles) - Length(Delimiter) - Length(Delimiter))))
    else if (GetField(fieldTranslatedTitle) = '') then
      SharedSecureSetField(fieldTranslatedTitle, GetField(fieldOriginalTitle));  
  end
  else
  begin
  // only one title
    if (Length(CountryTitles) > Length(Delimiter)) then
      SharedSecureSetField(fieldTranslatedTitle, textBetween(CountryTitles, Delimiter, Delimiter))
    else if (Length(WorldwideTitles) > Length(Delimiter)) then
      SharedSecureSetField(fieldTranslatedTitle, textBetween(WorldwideTitles, Delimiter, Delimiter));
  end;  
end;



// ***** retrieve alternative titles from RapidAPI *****
procedure retrieveTitlesFromRapidAPI;
var Headers, Url, FirstElem, RegionElem, LanguageElem, TitleElem, ErrorMsg: string;
begin
  Headers := 'X-RapidAPI-Key=' + GetParam('RapidAPIKey') + Chr(13) + Chr(10) + 'X-RapidAPI-Host=imdb8.p.rapidapi.com';
  Url := 'https://imdb8.p.rapidapi.com/title/get-versions?tconst=tt' + MovieNumber;
  FirstElem := '"alternateTitles":[';
  RegionElem := '"region":"';
  LanguageElem := '"language":"';
  TitleElem := '"title":"';
  ErrorMsg := 'RapidApi failed twice, please retry. If still fails, check your API Key, your monthly limit usage and your routing to imdb8.p.rapidapi.com';
  retrieveTitlesFromExternalAPI(Headers, Url, FirstElem, RegionElem, LanguageElem, TitleElem, ErrorMsg);
end;



// ***** retrieve alternative titles from imdbapi.dev *****
procedure retrieveTitlesFromImdbAPI;
var Headers, Url, FirstElem, RegionElem, LanguageElem, TitleElem, ErrorMsg: string;
begin
  Headers := 'accept=application/json';
  Url := 'https://api.imdbapi.dev/titles/tt' + MovieNumber + '/akas';
  FirstElem := '"akas":[';
  RegionElem := '"country":{"code":"';
  LanguageElem := '"language":{"code":"';
  TitleElem := '"text":"';
  ErrorMsg := 'ImdbApi failed twice, please retry. If still fails, check your routing to api.imdbapi.dev';
  retrieveTitlesFromExternalAPI(Headers, Url, FirstElem, RegionElem, LanguageElem, TitleElem, ErrorMsg);
end;


// ***** retrieve (un)spoilt content from json from RapidAPI *****

function retrieveTriviaPartFromRapidAPI(JsonResponse, tagFrom, tagTo: string): string;
var JsonContent, Txt, TriviaType: string;
begin
  JsonContent := textBetween(JsonResponse, tagFrom, tagTo);
  if (JsonContent = '') then
    JsonContent := TextAfter(JsonResponse, tagFrom);   
  
  Result := '';
  while (pos('"text":', JsonContent) > 0) do
  begin
    JsonContent := textAfter(JsonContent, '"text":');
    Txt := textBetween(JsonContent, '"', '"');
    JsonContent := textAfter(JsonContent, Txt);
    TriviaType := textBetween(JsonContent, '"', '}');
    TriviaType := textBetween(TriviaType, '"type": "', '"');
    JsonContent := textAfter(JsonContent, '}');
    Result := Result + '- ';
    if (TriviaType <> '') then
      Result := Result + '[' + StringReplace(TriviaType, '-', ' ') + '] ';
    Result := Result + Txt + Chr(13) + Chr(10);
  end;  
end;


// ***** retrieve trivia from RapidAPI *****

function retrieveTriviaFromRapidAPI(): string;
var
  JsonResponse, Headers: string;
  Trivia, Spoiler, AposReplace: string;
begin
  AposReplace := 'fj57hg785gh4jg7gh48g45g'; // any unique string ...
  Trivia := '';
  Spoiler := '';
  Headers := 'X-RapidAPI-Key=' + GetParam('RapidAPIKey') + Chr(13) + Chr(10) + 'X-RapidAPI-Host=imdb8.p.rapidapi.com';   
  JsonResponse := GetPage5('https://imdb8.p.rapidapi.com/title/get-trivia?tconst=tt' + MovieNumber, '', '', '', Headers);
  // if rapidapi fails, retry again 
  if (JsonResponse = '') then
    JsonResponse := GetPage5('https://imdb8.p.rapidapi.com/title/get-trivia?tconst=tt' + MovieNumber, '', '', '', Headers);
  JsonResponse := decodeRapidApiCharsetToCP1252(JsonResponse);  
  JsonResponse := StringReplace(JsonResponse, '\"', AposReplace);
  
  // remove unneeded json (first part)  
  Trivia := retrieveTriviaPartFromRapidAPI(JsonResponse, '"unspoilt":', '"spoilt":');   
  Spoiler := retrieveTriviaPartFromRapidAPI(JsonResponse, '"spoilt":', '"unspoilt":');
  Trivia := StringReplace(Trivia, AposReplace, '"');
  Spoiler := StringReplace(Spoiler, AposReplace, '"');
  
  Result := Chr(13) + Chr(10) + Trivia;
  if (Spoiler <> '') then
    Result := Result + Chr(13) + Chr(10) + 'SPOILERS:' + Chr(13) + Chr(10) + Spoiler;
end;



function cleanHtml(str: string): string;
begin
  Result := StringReplace(StringReplace(StringReplace(StringReplace(str, '\u0026' , '&'), '\u003c' , '<'), '\u003e' , '>'), '\"' , '"');
  HTMLRemoveTags(Result);
  HTMLDecode(Result);  
end;



procedure ImportAwards(fieldName: integer);
var
  PageText, Value, Value2, Tmp, Tmp2, FieldValue, ValuesDelimiter: string;
begin
  PageText := ConvertToASCII(GetPage(MovieUrl+'/awards')); 

  ValuesDelimiter := #13#10 + #13#10;  
   
  if (CanSetField(fieldName)) then
  begin
    FieldValue := '';
    Value := TextBetween(PageText,'"categories":[', ']},"requestContext":');
    Tmp := TextBetween(Value, '"name":"', '"');
    while (Tmp <> '') do
    begin      
      if (FieldValue <> '') then
        FieldValue := FieldValue + ValuesDelimiter;
             
      FieldValue := FieldValue + cleanHtml(Tmp);
      Value := TextAfter(Value, Tmp);
      
      if (Pos('"name":"', Value) > 0) then
        Value2 := TextBetween('"' + Value, '"', '"name":"')
      else
        Value2 := Value;     

      Tmp2 := TextBetween(Value2, '"rowTitle":"', '","');
      if (Tmp2 <> '') then
        FieldValue := FieldValue + ', ' + cleanHtml(Tmp2);
        
      //Tmp2 := TextBetween(Value2, '"rowSubTitle":"', '"');
      //if (Tmp2 <> '') then
      //  FieldValue := FieldValue + ':' + #32 + cleanHtml(Tmp2);      
        
      Tmp2 := TextBetween(Value2, '"text":"', '"');
      if (Tmp2 <> '') then
        FieldValue := FieldValue + #13#10 + cleanHtml(Tmp2);     
        
      Tmp2 := TextBetween(Value2, '"caption":"', '"');
      if (Tmp2 <> '') then
        FieldValue := FieldValue + #13#10 + cleanHtml(Tmp2);
      
      Tmp := TextBetween(Value, '"name":"', '"');    
    end;
    
    if (FieldValue <> '') then
    begin
      if (getField(fieldName) <> '') then
        SharedSecureSetField(fieldName, getField(fieldName) + #13#10 + #13#10 + 'AWARDS: ' + #13#10 + FieldValue)
      else  
        SharedSecureSetField(fieldName, 'AWARDS: ' + #13#10 + FieldValue);
    end;  
  end;  
end; 


// ***** retrieve values inside (json) text *****
// PageText : json text
// DelimiterListFrom, DelimiterListTo : main text selection
// DelimiterItemFrom, DelimiterItemTo : item -> sub text selection (one or more)
// Limit: 0 (take all items), -1 (take last item), n (take only the first n items)
    
procedure jsonMultipleValuesToList(PageText, DelimiterListFrom, DelimiterListTo, DelimiterItemFrom, DelimiterItemTo: string; fieldName, limit: integer);
var
  Value, Value2, Tmp, Tmp2, FieldValue, ValuesDelimiter: string;
  Counter, ActorsLayout: integer;
begin
  // specific actor parameters
  if (GetOption('IMDB Attori elenco') = 0) then
    ActorsLayout := 0
  else
    ActorsLayout := GetOption('IMDB Attori e ruoli');
    
  if ((fieldName = fieldActors) AND (ActorsLayout <> 0) AND (ActorsLayout <> 2)) then
    ValuesDelimiter := #13#10
  else if (((fieldName = fieldCategory) AND (GetOption('IMDB Categoria') = 2))
        OR ((fieldName = fieldCountry) AND (GetOption('IMDB Paese') = 2))
        OR ((fieldName = fieldLanguages) AND (GetOption('IMDB Lingue') = 2))
        OR ((fieldName = fieldAudioFormat) AND (GetOption('IMDB Formato audio') = 2))) then      
    ValuesDelimiter := ' / '
  else  
    ValuesDelimiter := ', ';  
    
  if (((fieldName <> 0) AND CanSetField(fieldName)) OR ((fieldName = 0) AND CanSetPicture() AND (GetOption('IMDB Locandina tipologia') > 0))) then
  begin
    FieldValue := '';
    Counter := 0;
    Value := TextBetween(PageText, DelimiterListFrom, DelimiterListTo);
    Tmp := TextBetween(Value, DelimiterItemFrom, DelimiterItemTo);
    if (Tmp = '') then
      Tmp := TextBetween(Value, StringReplace(DelimiterItemFrom, ' ', ''), DelimiterItemTo);
    while ((Tmp <> '') AND ((Counter < limit) OR (limit <= 0))) do
    begin      
      Value := DelimiterItemTo + TextAfter(Value, Tmp + DelimiterItemTo);
      
      if (limit = -1) then
        FieldValue := ''
      else if (FieldValue <> '') then
        FieldValue := FieldValue + ValuesDelimiter;
      
      if ((fieldName = fieldVideoFormat) or (fieldName = fieldResolution)) then
        Tmp := StringReplace(Tmp, ' ', '');  
        
      FieldValue := FieldValue + cleanHtml(Tmp);
      Counter := Counter + 1;

      
      if ((fieldName = fieldActors) AND (ActorsLayout > 1)) then
      begin
        // Actor Role      
        if (Pos('"rowTitle":"', Value) > 0) then
          Value2 := TextBetween(DelimiterItemTo + Value, DelimiterItemTo, DelimiterItemFrom)
        else
          Value2 := Value;     

        Tmp2 := TextBetween(Value2, '"characters":["', '"]');
        if (Tmp2 <> '') then
        begin
          // fix this: "characters":["Claudette Dusseault","Self"] -> Claudette Dusseault / Self
          Tmp2 := StringReplace(Tmp2, '","', ' / ');        
          if (ActorsLayout = 4) then
            FieldValue := FieldValue + ' ... ' + cleanHtml(Tmp2)
          else
            FieldValue := FieldValue + ' (as ' + cleanHtml(Tmp2) + ')';
        end;
        if (ActorsLayout = 4) then
        begin
          Tmp2 := TextBetween(Value2, '"attributes":"', '"');
          if (Tmp2 <> '') then
            FieldValue := FieldValue + ' ' + cleanHtml(Tmp2);
        end;  
      end;  
      
      Tmp := TextBetween(Value, DelimiterItemFrom, DelimiterItemTo);
      if (Tmp = '') then
        Tmp := TextBetween(Value, StringReplace(DelimiterItemFrom, ' ', ''), DelimiterItemTo);      
    end;
    
    if (fieldName = 0) then
    begin   
      if (CanSetPicture() and (FieldValue <> '')) then
      begin
        if ((GetOption('IMDB Locandina tipologia') = 3) OR (GetOption('IMDB Locandina tipologia') = 5)) then
        begin
          SharedSavePictureInfo(TextBefore(FieldValue, '._', '') + '._V1_SY' + GetParam('IMDB Locandina pixel') + '_AL_.jpg', MovieURL);
        end
        else
          SharedSavePictureInfo(FieldValue, MovieURL);
      end;    
    end  
    else
    begin
      if (fieldName = fieldLength) then
        FieldValue := IntToStr(StrToInt(fieldValue, 10) div 60)
      else if ((fieldName = fieldRating) AND (GetOption('IMDB Voto arrotondamento') = 1)) then
        FieldValue := IntToStr(Round(StrToFloat(FieldValue)));
      SharedSecureSetField(fieldName, FieldValue);
    end;          
  end;  
end; 




// ***** analyzes the page containing movie information *****

procedure AnalyzeMoviePage(PageText: string);
var
  FullValue, LastValue, Value, Value1, Value2, Tmp, Tmp2: string;
  limit, fieldName, updated, Count, p: Integer;
begin 
  MovieNumber := textBetween(SharedGetLatestPageUrl(), '/tt', '/');
  MovieURL := 'https://www.imdb.com/it/title/tt' + MovieNumber;
  // URL
  if CanSetField(fieldURL) then
    SetField(fieldURL, MovieURL);

  // original title
  jsonMultipleValuesToList(PageText, '"originalTitleText":{', '}', '"text":"', '"', fieldOriginalTitle, 1);
  
  // year
  jsonMultipleValuesToList(PageText, '"releaseYear":{', '}', '"year":', ',', fieldYear, 1);  

  // picture  
  if ((GetOption('IMDB Locandina italiana') = 0) OR (GetPictureStatus() = picStatusNone)) then
  begin
    case GetOption('IMDB Locandina tipologia') of
    1,2:
      jsonMultipleValuesToList(PageText, 'class="ipc-image" loading="eager"', '>', 'src="', '"', 0, 1);
    3:  
      jsonMultipleValuesToList(PageText, '"aboveTheFoldData":{', '"mainColumnData":{', '{"url":"', '"', 0, 1);
    4,5:
      jsonMultipleValuesToList(GetPage5(('https://api.imdbapi.dev/titles/tt' + MovieNumber), '', '', '', 'accept=application/json'), '"primaryImage":', '}', '"url":"', '",', 0, 1);    
    end;
  end;
   
  // Directors
  jsonMultipleValuesToList(PageText, '"directorsPageTitle":[', '"PrincipalCreditsForCategory"}]', '"nameText":{"text":"', '"', fieldDirector, 0);
  
  // Actors
  if (GetOption('IMDB Attori elenco') = 0) then
    jsonMultipleValuesToList(PageText, '"castPageTitle":{"edges":[', ']', '"text":"', '"', fieldActors, 0)
  else 
  begin
    if (GetOption('IMDB Attori elenco') = 2) then
      limit := StrToInt(GetParam('IMDB Attori numero'), 10)
    else
      limit := 0;  
    jsonMultipleValuesToList(PageText, '"id":"cast","name":"', '"listItemType"', '"rowTitle":"', '"', fieldActors, limit);
  end;

  // Composers
  // name can be Composer or Composers ...
  jsonMultipleValuesToList(PageText, '{"id":"composer","name":"', '],"listItemType":', '"rowTitle":"', '"', fieldComposer, 0);
  
  // Country
  case GetOption('IMDB Paese') of
  0:  
    jsonMultipleValuesToList(PageText, '"countriesDetails":{"countries":[', ']', '"text":"', '"', fieldCountry, 1);  
  1,2:
    jsonMultipleValuesToList(PageText, '"countriesDetails":{"countries":[', ']', '"text":"', '"', fieldCountry, 0);
  end;         
  
  
  // Category
  case GetOption('IMDB Categoria') of
  0:  
    jsonMultipleValuesToList(PageText, '"genres":{"genres":[', ']', '"text":"', '"', fieldCategory, 1);
  1,2:
    jsonMultipleValuesToList(PageText, '"genres":{"genres":[', ']', '"text":"', '"', fieldCategory, 0);
  end;  
  
  // Audio format
  case GetOption('IMDB Formato audio') of  
  0:
    jsonMultipleValuesToList(PageText, '"soundMixes":{"items":[', '"__typename":"SoundMixes"}', '"text":"', '"', fieldAudioFormat, 1);
  1,2:
    jsonMultipleValuesToList(PageText, '"soundMixes":{"items":[', '"__typename":"SoundMixes"}', '"text":"', '"', fieldAudioFormat, 0);
  end;  
  
  // Language
  case GetOption('IMDB Lingue') of
  0:  
    jsonMultipleValuesToList(PageText, '"spokenLanguages":{"spokenLanguages":[', ']', '"text":"', '"', fieldLanguages, 1);  
  1,2:
    jsonMultipleValuesToList(PageText, '"spokenLanguages":{"spokenLanguages":[', ']', '"text":"', '"', fieldLanguages, 0);
  end;     
  
  // Description
  if (GetOption('IMDB Trama') = 1) then
    jsonMultipleValuesToList(PageText, '"summaries":', '"__typename":"PlotConnection"}', '"plaidHtml":"', '","__typename":"Markdown"}', fieldDescription, 1);
  if ((GetOption('IMDB Trama') = 0) OR (GetField(fieldDescription) = '')) then
    jsonMultipleValuesToList(PageText, '"plot":{"plotText":{', '}', '"plainText":"', '","', fieldDescription, 1);

  // Aspect ratio       
  if (GetOption('IMDB Proporzioni video') = 1) then
    jsonMultipleValuesToList(PageText, '{"aspectRatios":{"items":[', ']', '"aspectRatio":"', '"', fieldVideoFormat, 1)
  else if (GetOption('IMDB Proporzioni video') = 2) then
    jsonMultipleValuesToList(PageText, '{"aspectRatios":{"items":[', ']', '"aspectRatio":"', '"', fieldResolution, 1);
  
  // Length
  jsonMultipleValuesToList(PageText, '"runtime":{', '}', '"seconds":', ',', fieldLength, 1);
   
  // Producers
  // name can be Producer or Producers ...
  jsonMultipleValuesToList(PageText, '{"id":"producer","name":"', '],"listItemType":', '"rowTitle":"', '"', fieldProducer, 0);  
  
  // Writers
  // name can be Writer or Writers ...
  jsonMultipleValuesToList(PageText, '{"id":"writer","name":"', '],"listItemType":', '"rowTitle": "', '"', fieldWriter, 0);
   
  // Rating
  jsonMultipleValuesToList(PageText, '"ratingsSummary":', '}', '"aggregateRating":', ',', fieldRating, 1);
  
  // TranslatedTitle
  if (CanSetField(fieldTranslatedTitle) AND (GetOption('IMDB Titolo tradotto') > 0)) then
  begin    
    updated := 0;
    if (GetOption('IMDB Titolo tradotto') = 0) then
    begin    
      jsonMultipleValuesToList(PageText, '"titleText":{', '}', '"text":"', '"', fieldTranslatedTitle, 1);
      if (GetField(fieldTranslatedTitle) <> '') then
        updated := 1;
    end;
    if ((updated = 0) and (GetOption('IMDB Titolo tradotto') > 0)) then
    begin
      if (GetParam('RapidAPIKey') <> '') then
        retrieveTitlesFromRapidAPI()
      else
        retrieveTitlesFromImdbAPI();
    end;    
  end;
  

  // Comments
  if CanSetField(fieldComments) then
  begin
    case GetOption('IMDB Commenti') of
      0, 1:
        begin
          Value2 := '';
          p := 0;
          FullValue := ConvertToASCII(GetPage(GetField(fieldURL)+'/reviews'));
          FullValue := TextAfter(FullValue, '<section class="ipc-page-section ipc-page-section--base ipc-page-section--sp-pageMargin">');
          while FullValue <> '' do
          begin
            Value := TextBetween(FullValue, '<article','</article>');
            if Value = '' then
            begin
              Break;
            end;
            Value1 := TextBetween(Value, '<div class="ipc-html-content-inner-div" role="presentation">','</div>');
            Value1 := StringReplace(Value1, #13#10, ' ');
            Value1 := StringReplace(Value1, '<br/><br/>', #13#10);
            Value1 := StringReplace(Value1, '<br/>', #13#10);
            HtmlRemoveTags(Value1);
            Value1 := FullTrim(Value1);
            if Value1 = '' then
            begin
              FullValue := TextAfter(FullValue, '</article>');
              Continue;
            end;
            if GetOption('IMDB Commenti') = 1 then
            begin
              p := p + 1;
              Value2 := Value2 + inttostr(p) + '. ';
            end;
            Value2 := Value2 + TextBetween(Value, '<h3 class="ipc-title__text">','</h3>');
            Value2 := Value2 + ' (by ' + TextAfter (TextBetween(Value, 'data-testid="author-link"','</a>'),'>');
            Value2 := Value2 + #32 + 'on ' + TextBetween(Value, '<li role="presentation" class="ipc-inline-list__item review-date">', '</li>') + ')' ;
            Value2 := Value2 + #13#10 + #13#10 + Value1;
            if (GetOption('IMDB Commenti') = 0) or (p = 10) then
            begin
              Break;
            end;
            FullValue := TextAfter(FullValue, '</article>');
            if FullValue <> '' then
            begin
              Value2 := Value2 +  #13#10 + #13#10;
            end;
          end;
          HTMLRemoveTags(Value2);
          HTMLDecode(Value2);
          SetField(fieldComments, Value2);
        end;
      2:
        begin
          SetField(fieldComments, '');
        end;
    end;
  end;
  
  // Trivia
  if GetOption('IMDB Curiosita''') > 0 then
  begin
    if ((GetOption('IMDB Curiosita''') = 1) OR (GetOption('IMDB Curiosita''') = 3)) then 
      fieldName := fieldDescription
    else
      fieldName := fieldComments;
    Value2 := GetField(fieldName);
    case GetOption('IMDB Curiosita''') of
      1,2:
        begin          
          jsonMultipleValuesToList(PageText, '"trivia":{"edges":[', '"__typename":"TriviaEdge"}]', '"plaidHtml":"', '","__typename":"Markdown"', fieldName, 1);
          Value := getField(fieldName);
          Value := StringReplace(Value, #13#10, '');
          while (Pos('  ', Value) > 0) do
            Value := StringReplace(Value, '  ', '');
          while (Pos('<span class="linksoda">', Value) > 0) do
          begin
            Value := StringReplace(Value, TextBetween(Value, '<span class="linksoda">', '</div>'), '');
            Value := StringReplace(Value, '<span class="linksoda"></div>', '');
          end;
          Value := StringReplace(Value, 'Link this trivia', '');
          Value := StringReplace(Value, '<div class="sodatext">', #13#10 + '- ');
          Value := StringReplace(Value, TextBetween(Value, '<script type="text/javascript">', '</script>'), '');
          HTMLRemoveTags(Value);
          HTMLDecode(Value);
          Value := RegExprSetReplace('\s[\d,]+\s+of\s+[\d,]+\sfound\sthis\sinterestingInteresting\?YesNo\|', Value, '', false);
          Value := RegExprSetReplace('\s*Is\sthis\sinteresting\?Interesting\?YesNo\|', Value, '', false);
          Value := RegExprSetReplace('Spoilers\sThe\strivia\sitems?\sbelow\smay\sgive\saway\simportant\splot\spoints\.\s*', Value, #13#10 + 'Spoilers:' + #13#10, false);
          Value := RegExprSetReplace('\s*See\salsoGoofs.*', Value, '', false);     
        end;
      3,4:
        if (GetParam('RapidAPIKey') <> '') then
          Value := retrieveTriviaFromRapidAPI()
        else  
          Value := '';
    end;

    if Value = '' then
      Value := Value2
    else if Value2 <> '' then
      Value := Value2 + #13#10 + #13#10 + 'IMDB TRIVIA: ' + Trim(Value)
    else
      Value :=  'IMDB TRIVIA: ' + Value;
    SetField(fieldName, Value);
  end;  
  
  // TagLine
  if (GetOption('IMDB Slogan') > 0) then
  begin
    if (GetOption('IMDB Slogan') = 1) then
      fieldName := fieldDescription
    else
      fieldName := fieldComments;
    Value2 := getField(fieldName);
    jsonMultipleValuesToList(PageText, '"taglines":', '"__typename":"TaglineEdge"', '"text":"', '","__typename":"Tagline"', fieldName, 1);
    Value := getField(fieldName);
    if (Value = '') then
      setField(fieldName, Value2)
    else if (Value2 <> '') then
      setField(fieldName, Value + #13#10 + #13#10 + Value2);
  end;
  
  // UserRatings
  if GetOption('IMDB Voto campo') > 0 then
  begin
    if (GetOption('IMDB Voto campo') = 1) then
      fieldName := fieldMediaType
    else
      fieldName := fieldComments;
    if (canSetField(fieldName)) then
    begin    
      Value2 := getField(fieldName);
      jsonMultipleValuesToList(PageText, '"ratingsSummary":', '}', '"voteCount":', ',', fieldName, 1);
      Value := 'User Rating: ' + GetField(fieldRating) + ' out of 10  (with ' + getField(fieldName) + ' votes)';
      if (GetOption('IMDB Voto campo') = 1) then
      begin
        if (getField(fieldName) <> '') then
          setField(fieldName, Value);
      end
      else if (getField(fieldName) = '') then
        setField(fieldName, Value2)
      else if (Value2 = '') then
        setField(fieldName, Value)
      else
        setField(fieldName, Value2 + #13#10 + #13#10 + Value);
    end;
  end;

  // Awards
  case GetOption('IMDB Premi') of
    1:
      ImportAwards(fieldDescription);
    2:
      ImportAwards(fieldComments);      
  end;  
  
  // Classification && MPAA
  // Classification is the classification of "UserCountry Parameter" Country (if "UserCountry Parameter" is void, get the first value)
  // MPAA is the classification of United States
  if (GetParam('IMDB Localizzazione') <> '') then
    jsonMultipleValuesToList(PageText, '"certificates":', '"__typename":"CertificatesConnection"}', '"edges":[', GetParam('IMDB Localizzazione'), fieldCertification, 1)
  else
    jsonMultipleValuesToList(PageText, '"certificates":', '"__typename":"CertificatesConnection"}', '"edges":[', '"Certificate"', fieldCertification, 1);
  jsonMultipleValuesToList(getField(fieldCertification) + '_________', '"node":', '_________', '"rating":"', '"', fieldCertification, -1)

  case GetOption('IMDB MPAA') of
  1:
    begin
      if (GetParam('IMDB Localizzazione') = 'United States') then
        SetField(fieldMediaType, getField(fieldCertification))
      else  
      begin
        jsonMultipleValuesToList(PageText, '"certificates":', '"__typename":"CertificatesConnection"}', '"edges":[', 'United States', fieldMediaType, 1);
        jsonMultipleValuesToList(getField(fieldMediaType) + '_________', '"node":', '_________', '"rating":"', '"', fieldMediaType, -1);
      end;  
    end;  
  2:  
    begin
      Value2 := getField(fieldComments);
      if (GetParam('IMDB Localizzazione') = 'United States') then
        Value := getField(fieldCertification)
      else
      begin      
        jsonMultipleValuesToList(PageText, '"certificates":', '"__typename":"CertificatesConnection"}', '"edges":[', 'United States', fieldComments, 1);
        jsonMultipleValuesToList(getField(fieldComments) + '_________', '"node":', '_________', '"rating":"', '"', fieldComments, -1);
        Value := getField(fieldComments);
      end;
      if (Value = '') then
        setField(fieldComments, Value2)
      else if (Value2 = '') then
        setField(fieldComments, 'Rated ' + Value)
      else
        setField(fieldComments, Value2 + #13#10 + #13#10 + 'Rated ' + Value);
    end;  
  end;  
end;
  
end.