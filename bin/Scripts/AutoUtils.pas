unit AutoUtils;

(***************************************************
AutoUtils by HappyTalk 2006.
Unit to add string, soundex and other functions to ANT
scripts. You may use unit in your scripts.
Please do not modify this original file, make a newly
named one. you can redistribute it and/or modify it under
the terms of the GNU General Public License

--------------------------------------------------
Version = 2007.03.22
Currently used by
Auto.TV.Com
Auto.IMDB.Com
Auto.Tools scripts
--------------------------------------------------

***************************************************)


const AutoUtils_Version = '2007.03.22';

//-----------------------------
// MATHS FUNCTIONS
//-----------------------------
// rets min of 2 values
function Min(X, Y: Integer): Integer;
begin
  if X < Y then Min := X else Min := Y;
end;

// rets max of 2 values
function Max(X, Y: Integer): Integer;
begin
  if X > Y then Max := X else Max := Y;
end;



//-----------------------------
// STRING FUNCTIONS
//-----------------------------
Function stringReverse(S : String): String;
Var
   i : Integer;
Begin
   Result := '';
   For i := Length(S) DownTo 1 Do
   Begin
     Result := Result + Copy(S,i,1) ;
   End;
End;

//finds pos of last sFind in sStr
Function revpos(sFind, sStr: string; p: Integer) : Integer;
var
p2: Integer;
begin
  result := 0;
  if Length(Sstr) >= p then
  begin
    //p2 := pos(sFind, copy(sStr, p, length(sStr)- p + 1)) + p - 1;
    p2 := pos(sFind, StrMid(sStr, p, 0)) + p - 1;
    if p2 >= p then result := p2
  end;
end;

// returns the pos of nth instance of sFind found in sStr going from reverse of string to start
Function nposrev(sFind, sStr: string; n: Integer) : Integer;
var
sFindRev, sStrRev: string;
p: Integer;
begin
  sFindRev := stringReverse(sFind);
  sStrRev := stringReverse(sStr);

  p := npos(sFindRev, sStrRev, n);
  if p > 0 then
    result := Length(sStr) - p - Length(sFind) + 2
  else
    result := 0;
end;

// returns pos of sFind in sStr from position p in str
Function ppos(sFind, sStr: string; p: Integer) : Integer;
var
p2: Integer;
begin
  result := 0;
  if Length(Sstr) >= p then
  begin
    //p2 := pos(sFind, copy(sStr, p, length(sStr)- p + 1)) + p - 1;
    p2 := pos(sFind, StrMid(sStr, p, 0)) + p - 1;
    if p2 >= p then result := p2
  end;
end;

// returns the pos of nth instance of sFind found in sStr or 0 if none.
Function npos(sFind, sStr: string; n: Integer) : Integer;
var
p: Integer;
begin
  result := 0;
  p := 0;
  repeat
    p := ppos(sFind, sStr, p+1);
    n := n - 1;
  until (p = 0) or (n = 0);
  result := p;
end;

// gets the right hand string AFTER nth instance of sFind in sStr,
// if bReverse nth instance is nth from end working backwards
Function nposRight(sFind, sStr: string; n: Integer; bReverse: boolean) : string;
var
p: integer;
begin
  result := '';
  if bReverse then
    p := nposrev(sFind, sStr, n) + Length(sFind) // set to pos AFTER nth occurrence
  else
    p := npos(sFind, sStr, n) + Length(sFind); // set to pos AFTER nth occurrence
  if (p > Length(sFind)) and (p <= Length (sStr)) then
    result := StrMid(sStr, p, 0);
end;

// gets the left hand string BEFORE nth instance of sFind in sStr
Function nposLeft(sFind, sStr: string; n: Integer; bReverse: boolean) : string;
var
p: integer;
begin
  result := '';
  if bReverse then
    p := nposrev(sFind, sStr, n) - 1 // set to pos BEFORE nth occurrence
  else
    p := npos(sFind, sStr, n) - 1; // set to pos BEFORE nth occurrence

  if (p > 0) then
    result := StrLeft(sStr, p);
end;


Function StrLeft(str: string; len: Integer) : string;
begin
  result := copy(str,1,len);
end;

Function StrRight(str: string; len: Integer) : string;
begin
  result := copy(str,Max(1,length(str)-len+1), len);
end;

// rets len chars from position p or rest if len=0
Function StrMid(str: string; p, len: Integer) : string;
begin
  if len = 0 then len := length(str)- p + 1;
  result := copy(str,p,len);
end;


// replaces all occurences of sFind with sReplace in sStr from after n'th instance of sFind
function nposReplaceString(sFind, sReplace: string; sStr: string; n:Integer): string;
var
  p: Integer;
begin
  p := npos(sFind, sStr, n) + 1;
  if p > 1 then
    sStr := posReplaceString(sFind, '', sReplace, sStr, p);
  result := sStr;
end;


// replaces all occurences of text between sFindBeg & sFindEnd with sReplace in sStr from position FBeg onwards
// sFindEnd can be '' to only replace sFindBeg's not range
// sReplace can be '' to just erase each occurence
function posReplaceString(sFindBeg, sFindEnd, sReplace: string; sStr: string; FBeg: Integer): string;
var
  FEnd: Integer;
begin
  if FBeg < 1 then FBeg := 1;
  while (true) do
  begin
    FBeg := ppos(sFindBeg, sStr, FBeg);
    if FBeg = 0 then break;

    if sFindEnd <> '' then
    begin
      FEnd := ppos(sFindEnd, sStr, FBeg+1);
      if FEnd = 0 then break;
      FEnd := FEnd + Length(sFindEnd);
    end else
      FEnd := FBeg + Length(sFindBeg);

    delete(sStr, FBeg, FEnd-FBeg);
    if sReplace <> '' then insert(sReplace, sStr, FBeg);
    FBeg := FBeg + Length(sReplace);
    if FBeg > Length(sStr) then Break;
  end;
  result := sStr;
end;


// Removes all space from start and end and ensures there is no more than SpCnt consecutive space inside string
// also Strips HTML out if required
Function StripSpace(s: string; SpCnt: Integer; StripHTML: Boolean) : string;
var
i, cnt: Integer;
s2, ch: string;
begin
  s2 := '';
  s := Trim(s);
  For i := 1 To Length(s) do
  begin
    ch := copy(s, i, 1);
    if (ch = ' ') then
    begin
      if (cnt < SpCnt) then
      begin
        s2 := s2 + ch;
        cnt := cnt + 1;
      end;
    end else
    begin
      s2 := s2 + ch;
      cnt := 0;
    end;
  end;

  if (StripHTML) and (Length(s2) > 0)  then
  begin
    HTMLRemoveTags(s2);
    HTMLDecode(s2);
  end;

  result := s2;
end;


// ConvertAlphaSpace: converts certain punc chars to space(only allows 1 consecitive space) + removes numbers & rets rest as lower case
Function ConvertAlphaSpace(s: string) : string;
var
i: Integer;
s2, ch: string;
begin
    s := AnsiLowerCase(s);
    s2 := '';
    For i := 1 To Length(s) do
    begin
        ch := copy(s, i, 1);
        if (ch >= 'a') and (ch <= 'z') then
          s2 := s2 + ch
        else
        begin
          case ch of
            ' ', '-', ':', '*', '?', '"', '<', '>', '.', '_', '\', '/', '|' : If StrRight(s2, 1) <> ' ' Then s2 := s2 + ' ';
          end;
        end;
    end;
    result := Trim(s2);
end;

// ConvertAlpha: removes from a string all non alpha chars (inc spaces) and rets rest as lower case
Function ConvertAlpha(s: string) : string;
var
i: Integer;
s2, ch: string;
begin
    s := AnsiLowerCase(s);
    s2 := '';
    For i := 1 To Length(s) do
    begin
      ch := copy(s, i, 1);
      if (ch >= 'a') and (ch <= 'z') then
        s2 := s2 + ch;
    end;
    result := s2;
end;

// ConvertAlphaNum: removes from a string all non alphanum chars (inc spaces) and rets rest as lower case
Function ConvertAlphaNum(s: string) : string;
var
i: Integer;
s2, ch: string;
begin
    s := AnsiLowerCase(s);
    s2 := '';
    For i := 1 To Length(s) do
    begin
      ch := copy(s, i, 1);
      if ((ch >= 'a') and (ch <= 'z')) or ((ch >= '0') and (ch <= '9')) then
        s2 := s2 + ch;
    end;
    result := s2;
end;


//-----------------------------
// LOCATE TEXT FUNCTIONS
//-----------------------------
// accumulates all lines between those containing FindBeg & FindEnd strings (inclusive) offset by OffBeg & OffEnd
// BegFind can = EndFind to get same line if BegOff=0
function PageTextBetween(BegFind: string; BegOff: Integer; EndFind: string; EndOff: Integer; Page: TStringList; LineNr: Integer; StripHTML: Boolean): string;
var
BegPos, EndPos, i: Integer;
Line: string;
begin
  result := '';
  if BegFind = '' then
    BegPos := LineNr // if no beg string go from current pos
  else
    BegPos := FindLine(BegFind, Page, LineNr);// + BegOff;

  if BegPos > -1 then
  begin
    if EndFind = '' then
      EndPos := BegPos
    else
      EndPos := FindLine(EndFind, Page, BegPos);

    if EndPos > -1 then
    begin
      BegPos := BegPos + BegOff;
      EndPos := EndPos + EndOff;
      for i := BegPos to EndPos do
      begin
        Line := Line + Page.GetString(i);
      end;
      Line := Trim(Line);
      if StripHTML then
      begin
        HTMLRemoveTags(Line);
        HTMLDecode(Line);
      end;
      result := Line;
    end;
  end;
end;

// rets text between BegFind & EndFind use BegOff & EndOff to reposition
function LineTextBetween(BegFind: string; BegOff: Integer; EndFind: string; EndOff: Integer; Line: string; StripHTML: Boolean): string;
var
BegPos, EndPos, i: Integer;
begin
  result := '';
  BegPos := pos(BegFind, Line);
  if BegPos = -1 then exit;
  EndPos := ppos(EndFind, Line, BegPos+1);
  if EndPos = -1 then exit;

  BegPos := BegPos + Length(BegFind) + BegOff; //Beg = 1st char after BegFind
  EndPos := EndPos + EndOff; //End = 1st Char off EndFind

  if (BegPos <= EndPos) and (BegPos > 0) and (EndPos < Length(Line)) then
  begin
    Line := copy(Line, BegPos, EndPos-BegPos);
    if StripHTML then
    begin
      HTMLRemoveTags(Line);
      HTMLDecode(Line);
    end;
    result := Line;
  end;
end;


function FindLine(Pattern: string; List: TStringList; StartAt: Integer): Integer;
var
  i, Cnt: Integer;
begin
  result := -1;
  if StartAt < 0 then
    StartAt := 0;
  Cnt := List.Count-1;
  for i := StartAt to Cnt do
    if Pos(Pattern, List.GetString(i)) <> 0 then
    begin
      result := i;
      Break;
    end;
end;


function FindLineNoCase(Pattern: string; List: TStringList; StartAt: Integer): Integer;
var
  i: Integer;
begin
  result := -1;
  Pattern :=  AnsiLowerCase(Pattern);
  if StartAt < 0 then
    StartAt := 0;
  for i := StartAt to List.Count-1 do
    if Pos(Pattern, AnsiLowerCase(List.GetString(i))) <> 0 then
    begin
      result := i;
      Break;
    end;
end;


function FindLineAlpha(Pattern: string; List: TStringList; StartAt: Integer): Integer;
var
  i: Integer;
begin
  result := -1;
  Pattern :=  ConvertAlpha(Pattern);
  if StartAt < 0 then
    StartAt := 0;
  for i := StartAt to List.Count-1 do
    if Pos(Pattern, ConvertAlpha(List.GetString(i))) <> 0 then
    begin
      result := i;
      Break;
    end;
end;


// do fuzzy search
function FindLineSoundEx(Pattern: string; List: TStringList; StartAt: Integer): Integer;
var
  i: Integer;
begin
  result := -1;
  Pattern :=  ConvertSoundEx(Pattern);
  if StartAt < 0 then
    StartAt := 0;
  for i := StartAt to List.Count-1 do
    if SoundExComp(Pattern,List.GetString(i)) then
    begin
      result := i;
      Break;
    end;
end;




//-----------------------------
// SOUNDEX FUNCTIONS
//-----------------------------
Function ConvertSoundEx(sSent: string) : string;
var
Pos1,Pos2,SLen: Integer;
s, wrd: string;
begin
    sSent := ConvertAlphaSpace(sSent);
    //sSent = ValidateChars(sSent) 'replace dodgy chars with spaces
    SLen := Length(sSent);
    Pos1 := 1;
    s := '';
    Repeat
        Pos2 := ppos( ' ', sSent,Pos1); //look for rest of str
        If Pos2 = 0 Then Pos2 := SLen + 1;
        wrd := copy(sSent, Pos1, Pos2 - Pos1);
        s := s + SoundEx(wrd);
        Pos1 := Pos2 + 1;
    Until Pos1 > SLen;
    result := s;
end;

//takes 2 soundex strings looks for soundex string(s) sStr2 in sStr1. does as 4 char comps
Function SoundExIn(sFind, sStr: string) : Boolean;
var SLen, i, MatchCnt: Integer;
begin
    SLen := Length(sFind) DIV 4;
    MatchCnt := 0;
    for i := 0 to SLen-1 do
    begin
        if (pos(copy(sFind, i * 4 + 1, 4), sStr) > 0) Then MatchCnt := MatchCnt + 1;
    end;
    result := ((MatchCnt * 100) DIV SLen) >= 60; //greater than 75% match => match
End;

//takes 2 normal strings and soundex converts. Then compares if str2 is in str1
Function SoundExComp(sFind, sStr : string) : Boolean;
var
  r: boolean;
begin
    r := SoundExIn(ConvertSoundEx(sFind), ConvertSoundEx(sStr));
    result := r
End;

//converts a string into soundex. 4 chars per word
Function SoundEx(sWord: String) : String;
var Num, sChar, sLastCode: string;
    lWordLength, i: Integer;
begin
    sWord := AnsiUpperCase(sWord);
    Num := copy(sWord, 1, 1); // Get the first letter
    sLastCode := GetSoundCodeNumber(Num);
    lWordLength := Length(sWord);

    // Create the code starting at the second letter.
    for i := 2 To lWordLength do
    begin
        sChar := GetSoundCodeNumber(copy(sWord, i, 1));
       
        // If two letters that are the same are next to each other only count one of them
        if (Length(sChar) > 0) And (sLastCode <> sChar) Then
        begin
          Num := Num + sChar;
          sLastCode := sChar;
        end;
    end;

    result := copy(Num + '    ', 1, 4); // Make sure code is exactly 4 chars
end;

//The letters A,E,I,O,U,Y,H,W and other characters are not coded.
function GetSoundCodeNumber(sChar: string) : String;
var
  SC: string;
begin
  SC := '';

// comma seperating this case statement = memory leaks???, hence done like this
  Case sChar of
    'B' : SC := '1';
    'F' : SC := '1';
    'P' : SC := '1';
    'V' : SC := '1';
    'C' : SC := '2';
    'G' : SC := '2';
    'J' : SC := '2';
    'K' : SC := '2';
    'Q' : SC := '2';
    'S' : SC := '2';
    'X' : SC := '2';
    'Z' : SC := '2';
    'D' : SC := '3';
    'T' : SC := '3';
    'L' : SC := '4';
    'N' : SC := '5';
    'M' : SC := '5';
    'R' : SC := '6';
  end;

  result := SC;
end;



//-----------------------------
// FIELD FUNCTIONS
//-----------------------------
// removes dots after 4th dot
function FixTitles(sStr: string): string;
begin
  result := '';   
  if sStr = '' then exit;
  result := nposReplaceString('.', ' ', sStr, 4); // replace '.' with ' ' after 4th '.' change the 4 to ? as required
end;


/// IMDB info has 'actorname (as partname)' this changes that to 'actor1,actor2,actor3'
function FixActors(sStr: string): string;
begin
  result := '';   
  if sStr = '' then exit;
  sStr := posReplaceString(' (', '), ',',', sStr, 1); // replace ' (.....), '
  sStr := posReplaceString(' (', ')','', sStr, 1); // replace '.' with ' ' after 4th '.'
  sStr := posReplaceString('(', '','', sStr, 1); // erase any remaining '('
  sStr := posReplaceString(')', '','', sStr, 1); // erase any remaining ')'
  // sStr := posReplaceString(', ', '',',', sStr, 1); // remove spaces between ,'s
  result :=  sStr;
end;




//-----------------------------
// OTHER FUNCTIONS
//-----------------------------
// rets url for large amazon pic given title. If ShowPicker = true will prompt with choices (if any)
function GetAmazonPicUrl(Title: String; ShowPicker: boolean) : String;
var
  Page: TStringList;
  LineNr, MovieCnt: Integer;
  Line, Address, Match: string;
begin
  result := '';
  MovieCnt := 0;
  Page := TStringList.Create;
  Address := 'http://www.amazon.com/s/ref=nb_ss_gw/103-7540265-9891830?url=search-alias%3Ddvd&field-keywords=' + StringReplace(UrlEncode(Title),'+', '%20');
  Page.Text := GetPage(Address);

  PickTreeClear;
  PickTreeAdd('Amazon matches for "' + Title + '" (' + GetField(fieldSource) + ')', '');
  LineNr := -1;
  repeat
    LineNr := FindLine('<span class="srTitle">', Page, LineNr + 1);
    if LineNr < 0 then Break;
    Line := Page.GetString(LineNr);
    Address := LineTextBetween('"http', -4, '">', 0, Line, False);
    HTMLRemoveTags(Line);
    if (Line <> '') and (Address <> '') then
    begin
      PickTreeAdd(Line, Address);
      if MovieCnt = 0 then Match := Address;
      MovieCnt := MovieCnt + 1;
    end;
  until (false);// or (LineNr > EndLine) or ((AutoFlag >= 3) and (SeCnt > 0));

  if MovieCnt <> 0 then //if no movies to select from may be it has gone straight to only choice so carry on
  begin
    if ShowPicker then
    begin
      if PickTreeExec(Address) = false then exit; //user select from all episodes
    end else
      Address := Match; //set to 1st match

    Page.Text := GetPage(Address); // get main movie page
  end;

// main movie page
  Line := PageTextBetween('registerImage("original_image"', 0, '', 0, Page, 0, False); //get the line
  Address := LineTextBetween('<a href="+''"''+"', 0, '"+''"''+" target="', 0, Line, False);
  if Address = '' then exit;

// movie large image page
  Page.Text := GetPage(Address);
  Line := PageTextBetween('imagePlaceHolder', 1, '', 1, Page, 0, False); //get the line after the 'imageplaceholder' one
  Address := LineTextBetween('<img src="', 0, '.jpg"', 4, Line, False);
  result := Address;
End;



//-----------------------------
// END
//-----------------------------
begin
end.