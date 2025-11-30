unit StringUtils7552;

{
  See comments in StringUtils1.pas for general infos
  
  This file was created by scorpion7552
  Of course, you can use these functions in your scripts :
  simply add "StringUtils7552" in the uses clause of your script.
	note that I also use StringUtils1 here, so you get the 2 for the same price...
	
	BUT remember that I have created this file for my own needs and that these
	functions/procedures may change one day or another without notification,
	... or even disappear 
version 1.0 initial release
version 1.1 add some functions
version 1.2 FormatLine and RemoveYear
version 1.3 change FormatMovieName 
version 1.4 character ',' moved from spec1 to spec2 (sometimes we get 'name1,name2' instead of 'name1, name2)
            add ’ to spec2 (pseudo quote)
}
uses
	StringUtils1;

const
	crlf = #13#10;                        // carriage return/line feed
// I use the 2 next characters to separate fields in a string
// (for parsing without confusion with real characters)
	sepchar1 = #02;                       // separator 1
	sepchar2 = #03;                       // separator 2
	sepchar3 = #04;                       // internal separator - don't use it outside 
// special characters
	spec1 = '" ! ? : ; / ( )';
	spec2 = ''' ’ . - ,';

var
	FormatUTF8: Integer;                      // flag: see FormatText
	memoAdr, memoTxt, token: TStringList;     // memoAdr, memoTxt: see SelectMovie

// general purpose

//------------------------------------------------------------------------------
// sort a StringList (ascending)
//------------------------------------------------------------------------------
procedure SortList(stringl: TStringList);
var
	i1, i2, imin: Integer;
	min, min2: String;
 
begin
	for i1 := 0 to stringl.count -2 do
	begin
		min := stringl.GetString(i1);         // current = min
		imin := i1;
// search the smallest entry in next 
		for i2 := i1 +1 to stringl.count-1 do
		begin
			min2 := stringl.GetString(i2);
			if min2 < min then 
			begin                              // current (i2) = new min
				min := min2;                     // memorize it and continue
				imin := i2;
			end;
		end;    {for i2}
		if imin <> i1 then                   // swap current (i1) and new min (imin)
			stringl.Exchange(i1, imin);	
	end;      {for i1}
end;

//------------------------------------------------------------------------------
// parse a string into tokens
// parseToken(initial_string, separator);
// in initial_string, the tokens are separated by character(s) sepchar 
// separator can be a combinaison of characters (i.e char1+char2 or 'xyz' or '<tag...>')
// returns the TStringList token
//------------------------------------------------------------------------------
procedure parseToken(str, sepchar: String);
begin
  token := TStringList.Create;
  token.Text := str;                                          // initial string
  if sepchar <> crlf then
    token.Text := StringReplace(token.Text, sepchar, crlf);   // separate fields
// Don't waste memory: free the TStringList when finished (token.Free;)
end;

//------------------------------------------------------------------------------
// returns the number of words of a string
//------------------------------------------------------------------------------
function Words(str: string) :integer;
var
	vcountwords: TStringList;
	
begin
	vcountwords := TStringList.Create;
	vcountwords.Text := StringReplace(str, ' ', crlf);              // parse words
	result := vcountwords.Count;
	vcountwords.Free;
end;

//------------------------------------------------------------------------------
// returns percentage (integer) of words of string1 found in string2 
// of course, both strings must be formatted using the same routine
// (like CleanString for example)
// note that 100 means exact match
//------------------------------------------------------------------------------
function CompareWords(str1, str2: string) :integer;
var
	wcount, i: integer;
	strl1, strl2: TStringList;
	w: string;

begin
	wcount := 0;                                    // counter 
	strl1 := TStringList.Create;
	strl1.Text := StringReplace(str1, ' ', crlf);   // parse words
	strl2 := TStringList.Create;
// strange: we can use IndexOfName and GetName but not GetValue and IndexOf ???
	strl2.Text := StringReplace(str2+'=', ' ', '='+crlf);  // for IndexOfName
	for i := 0 to strl1.Count -1 do                 // look for words of string1
	begin
		w := strl1.GetString(i);                      // current word of string1
		if w = '' then continue;
		if strl2.IndexOfName(w) <> -1 then            // match
			wcount := wcount +1;
	end;  {for i}                      
	if strl1.Count > 0 then                     // don't like 'divide by zero' !!!
	begin
		wcount := (wcount * 100) div strl1.Count;                      // percentage
// if all words of string1 have been found in string2 (in any order)
// but string2 is longer than string1, we can't count that as exact match !
		if (wcount = 100) and (strl2.Count > strl1.Count) then
			wcount := wcount - (strl2.Count - strl1.Count);
	end;
	result := wcount; 
	strl1.Free;
	strl2.Free;
end;

//------------------------------------------------------------------------------
// reorder a list contained in a string (ascending alphabetic order)
// input: the items to sort are separated by carriage return/line feed (crlf)
// at the end they will be separated by the string 'separator' (if any)
// sorted_list := ReorderList(orig_list, separator);
//------------------------------------------------------------------------------
function ReorderList(wholetext, str: string) : string;
var
  i: Integer;
	memo: TStringList;  
	
begin
	memo := TStringList.Create;
	memo.Text := wholetext;
	for i := 0 to memo.Count-1 do                                // trim the lines
		memo.SetString(i, Trim(memo.GetString(i)));
	SortList(memo);
	wholetext := memo.Text;
  wholetext := Left(wholetext, Length(wholetext) -2);  // remove last crlf before replacing
  if str <> '' then
  	wholetext := StringReplace(wholetext, crlf, str);
	memo.Free;
	result := wholetext
end;

//------------------------------------------------------------------------------
// returns the movie name stored in amc
//------------------------------------------------------------------------------
Function GetMovieName:string;
begin
	result := GetField(fieldTranslatedTitle);                  // first translated
	if result = '' then	result := GetField(fieldOriginalTitle);     // or original   
end;

//------------------------------------------------------------------------------
// returns a string translated to lowercase without accents 
//------------------------------------------------------------------------------
function AnsiLowerCaseNoAccents(str1: string) :string;
begin
	str1 := AnsiLowerCase(str1); 
	str1 := StringReplace(str1, 'à', 'a'); 
	str1 := StringReplace(str1, 'á', 'a');	     
	str1 := StringReplace(str1, 'â', 'a');
	str1 := StringReplace(str1, 'ã', 'a');	
	str1 := StringReplace(str1, 'ä', 'a'); 
	str1 := StringReplace(str1, 'ã', 'a');
	str1 := StringReplace(str1, 'é', 'e');   
	str1 := StringReplace(str1, 'è', 'e');
	str1 := StringReplace(str1, 'ë', 'e');
	str1 := StringReplace(str1, 'ê', 'e'); 
	str1 := StringReplace(str1, 'ï', 'i');
	str1 := StringReplace(str1, 'î', 'i');
	str1 := StringReplace(str1, 'ì', 'i');
	str1 := StringReplace(str1, 'í', 'i');
	str1 := StringReplace(str1, 'ô', 'o');
	str1 := StringReplace(str1, 'ö', 'o');
	str1 := StringReplace(str1, 'õ', 'o');
	str1 := StringReplace(str1, 'ò', 'o');
	str1 := StringReplace(str1, 'ó', 'o');
	str1 := StringReplace(str1, 'ù', 'u');
	str1 := StringReplace(str1, 'ü', 'u');  
	str1 := StringReplace(str1, 'û', 'u');
	str1 := StringReplace(str1, 'ú', 'u');
	str1 := StringReplace(str1, 'ç', 'c');
	str1 := StringReplace(str1, 'ñ', 'n');
	result := str1;
end;

//------------------------------------------------------------------------------
// returns a string with 1st article removed (first word only)
//------------------------------------------------------------------------------
function RemoveArticles(str1: string) :string;
var
  Articles: TStringList;
  str2: String;

begin
	Articles := TStringList.Create;  
  Articles.Add('le=');        
  Articles.Add('la=');
  Articles.Add('l''=');     
  Articles.Add('l’=');
  Articles.Add('l=');
  Articles.Add('les=');
  Articles.Add('des=');
  Articles.Add('un=');
  Articles.Add('une=');
  Articles.Add('the=');
  Articles.Add('a=');
  Articles.Add('an=');
  Articles.Add('der=');
  Articles.Add('das=');
  Articles.Add('die=');
  Articles.Add('dem=');
  Articles.Add('den=');
  Articles.Add('ein=');
  Articles.Add('eine=');
  Articles.Add('einen=');
  Articles.Add('einer=');
  Articles.Add('eines=');
  Articles.Add('einem=');
  Articles.Add('uno=');
  Articles.Add('una=');
  Articles.Add('el=');
  Articles.Add('los=');
  Articles.Add('las=');
  Articles.Add('unos=');
  Articles.Add('unas=');  
  Articles.Add('il=');
  Articles.Add('lo=');
  Articles.Add('i=');
  str2 := AnsiLowerCase(TextBefore(str1, ' ', ''));   // 1 st word
	if Articles.IndexOfName(str2) <> -1 then            // match an article
  	str1 := RemainingText;                            // keep the remaining
  Articles.Free;
	result := Trim(str1); 
end;

//------------------------------------------------------------------------------
// remove text between brackets
// text := RemoveBrackets(initial_text);
//------------------------------------------------------------------------------
function RemoveBrackets(wholetext: string) : string;
var 
	str1: String;
	i: Integer;
		
begin
  str1 := Trim(TextBefore(wholetext, '[', ''));
  if str1 <> '' then
  begin
    if Pos(']', RemainingText) > 0 then
      wholetext := str1+' '+Trim(TextAfter(RemainingText, ']'));  // + end of text or ''
  end;
  result := Trim(wholetext);
end;

//------------------------------------------------------------------------------
// returns a string with special characters translated (&amp; &nbsp; etc...)
//------------------------------------------------------------------------------
function TranslateSpecial(str1: string) :string;
begin
// sometimes, pages are coded with &amp;nbsp; (yes, I have seen that...)
// Don't know why they don't code directly &nbsp; (mistake ?)
// so first change '&amp;' with '&', then HTMLDecode will be happy 
	str1 := StringReplace(str1, '&amp;', '&');
// translate special characters  
	HTMLDecode(str1);
	result := Trim(str1);
end;

//------------------------------------------------------------------------------
// returns a string with blanks and crlf compacted (and more...)
// if flag = 'spec1' : remove special characters (spec1)
// and/or flag = 'spec2' : replace special characters (spec2) with blank
//------------------------------------------------------------------------------
function CompactString(str1, flag: String) :string;
var
	s1, m, str2 : string;
	i: integer;

begin
	if flag <> '' then                                 // replace &amp; ... stuffs          
		str1 := TranslateSpecial(str1); 
	m := '';
	str2 := '';
	i := 0;
	while (i < Length(str1)) do
	begin
		i := i + 1;
		s1 := Copy(str1, i, 1);                        // current character of str1
		if s1 = #09 then s1 := ' ';                    // replace Tab ('09'x) with blank
		if (s1 <> ' ') and (flag <> '') then           // treat special characters
		begin
			if Pos('spec1', flag) > 0 then                 // remove spec1
				if Pos(s1, spec1) > 0 then continue;
			if Pos('spec2', flag) > 0 then                 // replace spec2
				if Pos(s1, spec2) > 0 then s1 := ' ';
		end;
		if (s1 = ' ') and (s1 = m) then continue;      // ignore multiple blanks
		if s1 = #13 then                               // cr: maybe crlf
		begin
			s1 := Copy(str1, i, 2);
			i := i +1;
			if (s1 = crlf) and (s1 = m) then continue;   // ignore multiple crlf
		end;
		m := s1;                                       // memo current character(s)
		str2 := str2+s1;                               // and store
	end;         {while i < length(str1)}
	result := Trim(str2);
end;

//------------------------------------------------------------------------------
// returns a string formatted according to the following convention
// final_text := TranslateText(initial_text, format_type);
// format_type (integer)
// 0 : no change
// 1 : change all characters to lowercase
// 2 : change all characters to uppercase
// 3 : first character to uppercase, the others to lowercase
// 4 : all first characters of words to uppercase, the others to lowercase
//------------------------------------------------------------------------------
function TranslateText(str1: string; f: integer) :string;
begin
	case f of
	1: result := AnsiLowerCase(str1);
	2: result := AnsiUpperCase(str1);
	3: result := AnsiUpFirstLetter(AnsiLowerCase(str1)); 
	4: result := AnsiMixedCase(AnsiLowerCase(str1), ' '); 
	else result := str1;
	end; 
end;

//------------------------------------------------------------------------------
// dump a string to disk 
// DumpPage(path_of_the_file,string);
// path_of_the_file = complete path (ie: 'c:\temp\myfile.txt')
// note: the directory (if any) must be created before
//------------------------------------------------------------------------------
procedure DumpPage(filePath, WholeText: string);
var
	page: TStringList;
 
begin
	page := TStringList.Create;
	page.Text := WholeText;
	page.SaveToFile(filePath);
	page.Free;
end;
	
//------------------------------------------------------------------------------
// create and display a list (of movies or what you want) 
// and returns the selected address or ''
// addr := SelectMovie('title_for_display');
// note: global TStringList's must be initialized
//       memoAdr. = url of page (or what you want)
//       memoTxt. = text to display (you can separate tokens with sepchar1)
//------------------------------------------------------------------------------
function SelectMovie(title: string) :string;
var
	Address: String;
	i: integer;

begin
	PickTreeClear;                                   // clear list
	PickTreeAdd(title, '');
	for i:= 0 to memoTxt.Count -1 do                 // create the list
    PickTreeAdd(StringReplace(memoTxt.GetString(i), sepchar1, ''), memoAdr.GetString(i));  
	result := '';
	if PickTreeExec(Address) then result := Address;
end;
	
// more or less specific
		
//------------------------------------------------------------------------------
// returns the url contained in a string without edition
// addr := GetUrl(string_containing_url, start_from_or_'',base_url_or_'');
//------------------------------------------------------------------------------
function GetUrl(WholeText, StartFrom, urlb: string) :string;
var
	i: Integer;
	delim: char;

begin
	result := '';
	if StartFrom <> '' then                        // if StartFrom = '', start from begining of string
	begin
		i := Pos(StartFrom, WholeText); 
		if i = 0 then exit;                          // StartFrom not found
		Delete(WholeText,1, i -1);                   // delete characters before StartFrom
	end; 
	i := Pos('HREF=', AnsiUpperCase(WholeText));   // start of url: href= 
	if i = 0 then exit;                            // no href= found
	Delete(WholeText,1, i +4);                     // skip href=
	WholeText := TextBefore(WholeText, '>', '');   // stop at the end of tag
	delim := StrGet(WholeText, 1);                 // delimiter = " or ' or nothing special
	if (delim = '''') or (delim = '"') then        // skip ' or " 
		Delete(WholeText, 1, 1)                      
	else
		delim := ' ';                                 // no delimiter: stop at first blank if any
	i := Pos(delim, WholeText);   
	if i > 0 then	Delete(WholeText,i, Length(WholeText));
	WholeText := StringReplace(WholeText, '&amp;', '&');
	WholeText := StringReplace(WholeText, '../', '');    // cf relative address
	WholeText := StringReplace(WholeText, './', '');     
	WholeText := urlb + WholeText;                       // add base url if any 
	result := Trim(WholeText);
end;

//------------------------------------------------------------------------------
// get url just before a text
// url := GetUrlBefore(text_containing_url, text_to_look_for, base_url_or_'');
//------------------------------------------------------------------------------
function GetUrlBefore(wholetext, str, urlb: string) : string;
var 
	id: string;
	i: integer;
		
begin
// the url is before the searched text
	i := Pos(str, wholetext);
	id := left(wholetext, i);
// and after the last '<a ' 
	i := LastPos('<a ', AnsiLowerCase(id));
	result := GetUrl(Copy(id, i, length(id)), '', urlb);
end;

//------------------------------------------------------------------------------
// returns a string formatted for display - special stuffs
// see comments in FormatText2
// note that FormatText is suitable for multi-lines fields
// for single-line fields, use FormatLine
//------------------------------------------------------------------------------
function FormatText(initialText: string) :string;
begin
// paragraphs (HTML tags) = crlf (that's my choice, isn't it?)
	initialText := StringReplace(initialText, '</p>', sepchar3);
	initialText := StringReplace(initialText, '<p>', sepchar3);
// now "standard" formatting
	initialtext := FormatText2(initialText);
// put back the crlf's
	result := StringReplace(initialtext, sepchar3, crlf);
end;

//------------------------------------------------------------------------------
// returns a string formatted for display - this text may contain html tags 
// and special characters( &amp; &lt; &gt; &quot; &nbsp;)
// formatted_text := FormatText2(initial_text);
// if your text is coded using UTF-8 then you must code in the caller script:
//    FormatUTF8 := 1;
// if all your texts are in pure ASCII, then you have nothing to do ...
// or if you have a mix, then you must set FormatUTF8 accordingly (0 or 1)
//------------------------------------------------------------------------------
function FormatText2(initialText: string) :string;
var
	s: char;
	i: integer;
 
begin
	result := '';
	if initialText = '' then exit;                           // nothing to convert
	if FormatUTF8 = 1 then
	begin
		initialText := UTF8Decode(initialText);                // UTF-8 to ASCII
// some strange characters not translated....
		initialText := StringReplace(initialText, #160, ' ');  // 'A0'x 
	end;
// suppress HTML tags and translate special characters (&amp; ...)
  initialText := RemoveHTML(initialText);
// suppress formatting characters at the begining and at the end of string 
// (except sepchar1 and sepchar2)
	while (initialText <> '') do
	begin
	s := StrGet(initialText, 1);                   // 1st character of initialText
 	if (s = #0) or (s > #32) or (s = sepchar1) or (s = sepchar2) then break;  // ended
	Delete(initialText, 1, 1);                       // out
	end;
//
	while (initialText <> '') do
	begin
	i := Length(initialText);
	s := StrGet(initialText, i);                  // last character of initialText
	if (s = #0) or (s > #32) or (s = sepchar1) or (s = sepchar2) then break;  // ended 
	Delete(initialText, i, 1);                       // out
	end;
// and compact string (leaving spec1 and spec2 asis)
	result := CompactString(initialText, '');
end;

//------------------------------------------------------------------------------
// remove HTML tags and special characters( &amp; &lt; &gt; &quot; &nbsp;)
// formatted_text := RemoveHTML(initial_text);
//------------------------------------------------------------------------------
function RemoveHTML(initialText: string) :string;
begin
	HTMLRemoveTags(initialText);
	result := TranslateSpecial(initialText);
end;

//------------------------------------------------------------------------------
// returns a string formatted for display - for single-line fields
// formatted_line := FormatLine(initial_line);
//------------------------------------------------------------------------------
function FormatLine(initialText: string) :string;
begin
// "standard" formatting
	result := FormatText2(initialText);
// if there are some carriage return/linefeed, then remove them
  result := StringReplace(result, crlf, '');
end;

//------------------------------------------------------------------------------
// returns the movie name formatted (for input to search engines)
// note that this is not very universal, but maybe can fit your needs... 
//------------------------------------------------------------------------------
function FormatMovieName(str: string) :string;

begin
	str := FormatMovieNameStd(str);              // lower case without accents
// compact string and remove spec1 (don't treat spec2 here)
	result := CompactString(str, 'spec1'); 
end;

//------------------------------------------------------------------------------
// returns the movie name formatted (for input to search engines)
// note that this is not very universal, but maybe can fit your needs... 
//------------------------------------------------------------------------------
function FormatMovieNameStd(str: string) :string;
var
	i, j: integer;
	oldstr: String;
	
begin
	str := AnsiLowerCaseNoAccents(str);              // lower case without accents
  oldstr := str;
// sometimes, movie names are coded as 'usual title/alternate title'
// keep only the usual name
	i := Pos('/', str);                  
	if i > 0 then str := Left(str, i-1);
// and sometimes, there's (year) : remove it
  str := RemoveYear(str);
// some search engines limit the number of key words used
// so it's better to remove the 1st article (if more than 3 words)
	if Words(str) > 3 then str := RemoveArticles(str);
	if str = '' then str := oldstr;                       // all have been removed
	result := str;
end;

//------------------------------------------------------------------------------
// returns a string with all special characters suppressed (for comparisons)
//------------------------------------------------------------------------------
function CleanString(str1: string) :string;
begin;
	str1 := AnsiLowerCaseNoAccents(str1);          // lowercase without accents
	result := CompactString(str1, 'spec1 spec2');  // compact string (treat spec1 and spec2)
end;

//------------------------------------------------------------------------------
// returns a string with (year) removed
// formatted_text := RemoveYear(initial_text);
//------------------------------------------------------------------------------
function RemoveYear(str: string) :String;
var
	i: integer;
	
begin
	i := Pos('(19', str);              // (19xx) ou (20xx) that's enough, no ? 
	if i = 0 then
		i := Pos('(20', str);
	if i > 0 then                                 
	begin
    if Copy(str, i+5, 1) = ')' then
    begin
      Delete(str, i, 6);
      str := StringReplace(str, '  ', ' ');                  // compact blanks
    end;
	end;
	result := str;
end;

end.
