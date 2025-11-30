(***************************************************

Ant Movie Catalog importation script
www.antp.be/software/moviecatalog/

[Infos]
Authors=
Title=StringUtils1.pas
Description=
Site=
Language=?
Version=
Requires=3.5.1
Comments=
License=
GetInfo=0

[Options]

***************************************************)

unit StringUtils1;

const StringUtils1_Version = 8;

{
  PLEASE READ THIS BEFORE MODIFYING THIS FILE :
  If you want to put some of your functions in an external
  file because they are common to several of your scripts,
  please create a new file your your functions
  (e.g. StringUtils2 if they are also functions to work on
  strings) rather than add them in this file.
  So each "unit" (.pas files) belong to one script creator
  and there is no risk that two people modify it at the
  same time.
}

{
  This file was created by Antoine Potten, originally
  for IMDB script.
  Of course, you can use these functions in your scripts,
  they're made for that. Simply add "StringUtils1" in the
  uses clause of your script.
}

{
  History
  -------
  v.2:  Added a version number, few minor changes in the functions,
        and the FindLine function used by lots of old scripts (they
        would all have to be modified to use this file).
  v.3:  Added FindFullLine function
  v.4:  ?
  v.5:  "FullTrim" rewritten
  v.6:  "Cp1252ToASCII" added
  v.7:  "RemoveSpaces" added
}

var
  RemainingText: string;
  {
    when calling...     this variable contains...
    ------------        ----------------------
    TextBefore          the text after SearchText
    TextBetween         the text after AfterText
  }

// ***** Like the Pos function, but returns the last occurence instead of the first one *****

function LastPos(ASearch: string; AText: string): Integer;
var
  CurPos, PrevPos: Integer;
begin
  PrevPos := 0;
  CurPos := Pos(ASearch, AText);
  while CurPos > 0 do
  begin
    if PrevPos = 0 then
      PrevPos := CurPos
    else
      PrevPos := PrevPos + CurPos + Length(ASearch) - 1;
    Delete(AText, 1, CurPos + Length(ASearch) - 1);
    CurPos := Pos(ASearch, AText);
  end;
  Result := PrevPos;
end;

// *****
{    Returns the text before SearchText, but not before BeginLimit (if it is not empty),
    It takes the last occurence of BeginLimit found before the position of SearchText  }

function TextBefore(WholeText: string; SearchText: string; BeginLimit: string): string;
var
  FoundPos, PrevPos: Integer;
  WorkText: string;
begin
  RemainingText := WholeText;
  Result := '';
  FoundPos := Pos(SearchText, WholeText);
  if FoundPos = 0 then
    Exit;
  WorkText := Copy(WholeText, 1, FoundPos - 1);
  RemainingText := Copy(WholeText, FoundPos + Length(SearchText), Length(WholeText));
  if BeginLimit <> '' then
  begin
    FoundPos := LastPos(BeginLimit, WorkText);
    if FoundPos = 0 then
      Exit
    else
      FoundPos := FoundPos + Length(BeginLimit);
  end
  else
    FoundPos := 1;
  Result := Copy(WorkText, FoundPos, Length(WorkText));
end;

// ***** Returns the text after SearchText *****

function TextAfter(WholeText: string; SearchText: string): string;
var
  FoundPos: Integer;
begin
  Result := '';
  FoundPos := Pos(SearchText, WholeText);
  if FoundPos = 0 then
    Exit;
  Result := Copy(WholeText, FoundPos + Length(SearchText), Length(WholeText));
end;

// *****
{    Returns the text between BeforeText and AfterText (without these two strings),
     It takes the first AfterText occurence found after the position of BeforeText  }

function TextBetween(WholeText: string; BeforeText: string; AfterText: string): string;
var
  FoundPos: Integer;
  WorkText: string;
begin
  RemainingText := WholeText;
  Result := '';
  FoundPos := Pos(BeforeText, WholeText);
  if FoundPos = 0 then
    Exit;
  WorkText := Copy(WholeText, FoundPos + Length(BeforeText), Length(WholeText));
  FoundPos := Pos(AfterText, WorkText);
  if FoundPos = 0 then
    Exit;
  Result := Copy(WorkText, 1, FoundPos - 1);
  RemainingText := Copy(WorkText, FoundPos + Length(AfterText), Length(WorkText));
end;

// *****
{    Searches for a partial text of one of the items of a TStringList
     Returns -1 if not found   }

function FindLine(Pattern: string; List: TStringList; StartAt: Integer): Integer;
var
  i: Integer;
begin
  result := -1;
  if StartAt < 0 then
    StartAt := 0;
  for i := StartAt to List.Count-1 do
    if Pos(Pattern, List.GetString(i)) <> 0 then
    begin
      result := i;
      Break;
    end;
end;

// *****
{    Searches for a full text of one of the items of a TStringList
     Returns -1 if not found   }

function FindFullLine(Line: string; List: TStringList; StartAt: Integer): Integer;
var
  i: Integer;
begin
  result := -1;
  if StartAt < 0 then
    StartAt := 0;
  for i := StartAt to List.Count-1 do
    if Line = List.GetString(i) then
    begin
      result := i;
      Break;
    end;
end;

// *****
{    Removes tabs and linebreaks in addition of spaces on both sides of the string  }
// rewritten by bad4u

function FullTrim(Value: string): string;
var
  ExitLoop: Boolean;
begin
  Result := '';
  ExitLoop := False;
  repeat
    case copy(Value, 1, 1) of
      ' ', #9, #10, #13:    Value := copy(Value, 2, Length(Value)-1);
    else
      ExitLoop := True;
    end;
  until ExitLoop;
  ExitLoop := False;
  repeat
    case copy(Value, Length(Value), 1) of
      ' ', #9, #10, #13:    Value := copy(Value, 1, Length(Value)-1);
    else
      ExitLoop := True;
    end;
  until ExitLoop;
  Result := Value;
end;

// *****
{    Same as FullTrim but also remove all double spaces, tabs (and possibly linebreaks)
     found in the middle of the string: only single spaces are kept }

function RemoveSpaces(AText: string; LineBreaksToo: Boolean): string;
begin
  Result := StringReplace(AText, #9, '');
  if LineBreaksToo then
  begin
    Result := StringReplace(StringReplace(Result, #10, ''), #13, '');
  end;
  Result := Trim(Result);
  while Pos('  ', Result) > 0 do
  begin
    Result := StringReplace(Result, '  ', ' ');
  end;
end;


// *****
{    Converts windows-1252 and iso-8859-1 characters to ASCII characters  }

function Cp1252ToASCII(Value: string): string;
begin
  HTMLDecode(Value);
  Value := StringReplace(Value, 'à', 'a');
  Value := StringReplace(Value, 'á', 'a');
  Value := StringReplace(Value, 'â', 'a');
  Value := StringReplace(Value, 'ã', 'a');
  Value := StringReplace(Value, 'ä', 'ae');
  Value := StringReplace(Value, 'å', 'a');
  Value := StringReplace(Value, 'æ', 'a');
  Value := StringReplace(Value, 'ç', 'c');
  Value := StringReplace(Value, 'è', 'e');
  Value := StringReplace(Value, 'é', 'e');
  Value := StringReplace(Value, 'ê', 'e');
  Value := StringReplace(Value, 'ë', 'e');
  Value := StringReplace(Value, 'ì', 'i');
  Value := StringReplace(Value, 'í', 'i');
  Value := StringReplace(Value, 'î', 'i');
  Value := StringReplace(Value, 'ï', 'i');
  Value := StringReplace(Value, 'ð', 'dh');
  Value := StringReplace(Value, 'ñ', 'n');
  Value := StringReplace(Value, 'ò', 'o');
  Value := StringReplace(Value, 'ó', 'o');
  Value := StringReplace(Value, 'ô', 'o');
  Value := StringReplace(Value, 'õ', 'o');
  Value := StringReplace(Value, 'ö', 'o');
  Value := StringReplace(Value, 'ø', 'o');
  Value := StringReplace(Value, 'ù', 'u');
  Value := StringReplace(Value, 'ú', 'u');
  Value := StringReplace(Value, 'û', 'u');
  Value := StringReplace(Value, 'ü', 'u');
  Value := StringReplace(Value, 'ý', 'y');
  Value := StringReplace(Value, 'þ', 'th');
  Value := StringReplace(Value, 'ÿ', 'y');
  Value := StringReplace(Value, 'ß', 'ss');
  Value := StringReplace(Value, '¡', '');
  Value := StringReplace(Value, '¢', 'cent');
  Value := StringReplace(Value, '£', 'pound');
  Value := StringReplace(Value, '¥', 'yen');
  Value := StringReplace(Value, '€', 'euro');
  Value := StringReplace(Value, '©', '(c)');
  Value := StringReplace(Value, 'ª', 'a');
  Value := StringReplace(Value, '«', '"');
  Value := StringReplace(Value, '®', '(r)');
  Value := StringReplace(Value, '°', 'deg.');
  Value := StringReplace(Value, '±', '+/-');
  Value := StringReplace(Value, '²', '^2');
  Value := StringReplace(Value, '³', '^3');
  Value := StringReplace(Value, '´', '''');
  Value := StringReplace(Value, 'µ', 'micro');
  Value := StringReplace(Value, '·', '-');
  Value := StringReplace(Value, 'º', 'o');
  Value := StringReplace(Value, '»', '"');
  Value := StringReplace(Value, '¼', '1/4');
  Value := StringReplace(Value, '½', '1/2');
  Value := StringReplace(Value, '¾', '3/4');
  Value := StringReplace(Value, '¿', '');
  Value := StringReplace(Value, 'À', 'A');
  Value := StringReplace(Value, 'Á', 'A');
  Value := StringReplace(Value, 'Â', 'A');
  Value := StringReplace(Value, 'Ã', 'A');
  Value := StringReplace(Value, 'Ä', 'A');
  Value := StringReplace(Value, 'Å', 'A');
  Value := StringReplace(Value, 'Æ', 'AE');
  Value := StringReplace(Value, 'Ç', 'C');
  Value := StringReplace(Value, 'È', 'E');
  Value := StringReplace(Value, 'É', 'E');
  Value := StringReplace(Value, 'Ê', 'E');
  Value := StringReplace(Value, 'Ë', 'E');
  Value := StringReplace(Value, 'Ì', 'I');
  Value := StringReplace(Value, 'Í', 'I');
  Value := StringReplace(Value, 'Î', 'I');
  Value := StringReplace(Value, 'Ï', 'I');
  Value := StringReplace(Value, 'Ð', 'DH');
  Value := StringReplace(Value, 'Ñ', 'N');
  Value := StringReplace(Value, 'Ò', 'O');
  Value := StringReplace(Value, 'Ó', 'O');
  Value := StringReplace(Value, 'Ô', 'O');
  Value := StringReplace(Value, 'Õ', 'O');
  Value := StringReplace(Value, 'Ö', 'O');
  Value := StringReplace(Value, '×', 'x');
  Value := StringReplace(Value, 'Ø', 'O');
  Value := StringReplace(Value, 'Ù', 'U');
  Value := StringReplace(Value, 'Ú', 'U');
  Value := StringReplace(Value, 'Û', 'U');
  Value := StringReplace(Value, 'Ü', 'U');
  Value := StringReplace(Value, 'Ý', 'Y');
  Value := StringReplace(Value, 'Þ', 'TH');
  Value := StringReplace(Value, '¹', '^1');
  Value := StringReplace(Value, '§', '[section]');
  Value := StringReplace(Value, '¶', '[paragraph]');
  Value := StringReplace(Value, '¤', '[currency]');
  Value := StringReplace(Value, '„', '"');
  Value := StringReplace(Value, '…', '...');
  Value := StringReplace(Value, '‰', '/1000');
  Value := StringReplace(Value, 'Š', 'S');
  Value := StringReplace(Value, 'Œ', 'OE');
  Value := StringReplace(Value, 'Ž', 'Z');
  Value := StringReplace(Value, '‘', '''');
  Value := StringReplace(Value, '’', '''');
  Value := StringReplace(Value, '“', '"');
  Value := StringReplace(Value, '”', '"');
  Value := StringReplace(Value, '•', '-');
  Value := StringReplace(Value, '–', '-');
  Value := StringReplace(Value, '—', '-');
  Value := StringReplace(Value, '™', '(tm)');
  Value := StringReplace(Value, 'š', 's');
  Value := StringReplace(Value, 'œ', 'oe');
  Value := StringReplace(Value, 'ž', 'z');
  Value := StringReplace(Value, 'Ÿ', 'Y');
  Result := Value;
end;

// *****
{    UTF8DecodeCorr

     Special workaround because: UTF8 decoding error

     Coding is based on "Filmstarts.de.ifs" function MyUTF8Decode():
     The Delphi UTF8Decode() function has the bug that 4-Byte UTF-8 chars
     break the decoding and the resulting string is empty.
     If this happens, this function replaces the 4-Byte UTF-8 chars
     (Binary 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx) with ? and calls UTF8Decode() again.

     and

     based on a remark at 'StringUtils7552.pas' 
}
function UTF8DecodeCorr(p_in_Text: string) :string;
var
  v_TextCalc: string;
begin
  v_TextCalc := ''; // init
 
  if p_in_Text <> '' then
  begin
    v_TextCalc :=  UTF8Decode(p_in_Text);
   
    if v_TextCalc = '' then
      // Replace all 4-Byte UTF-8 chars with ? and decode again
      v_TextCalc := UTF8Decode(RegExprSetReplace('[\xF0-\xF7][\x80-\xBF][\x80-\xBF][\x80-\xBF]', p_in_Text, '?', False));
       
    if v_TextCalc = '' then
      // RegExprSetReplace() not successfull or UTF8Decode() still returns an empty string. Do not decode.
      v_TextCalc := p_in_Text;
   
    // some strange characters not translated.... - according to library 'StringUtils7552.pas'
        v_TextCalc := StringReplace2(v_TextCalc, #160, ' ', False, True);  // hex'A0' (special-space)
      
      end;
      
      result := v_TextCalc;
end;

// *****
begin
end.
