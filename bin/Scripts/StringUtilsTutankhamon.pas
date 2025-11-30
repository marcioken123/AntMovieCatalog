(***************************************************

Ant Movie Catalog importation script
www.antp.be/software/moviecatalog/

[Infos]
Authors=Tutankhamon (<link>tutan@zaslavl.by</link>)
Title=StringUtilsTutankhamon
Description=
Site=http://zaslavl.by
Language=RU
Version=0.2 (29-Aug-2009)
Requires=3.5.0
Comments=
License=
GetInfo=0

[Options]

***************************************************)

unit StringUtilsTutankhamon;

const StringUtils_Version = 0.2;

{
История
----------------------------------------------------

ver 0.1       - первая публичная версия
ver 0.2       * исправлен "FullTrim" и "ClearSpecialChars"
              + добавлен "CntChRepet" (Подсчёт количества вхождений символа в строке)
}

var
  RemainingText: string;

// ***** Как и функция Pos, но возвращает позицию последнего вхождения вместо первого *****

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

// ***** Возвращает текст перед SearchText, но не раньше BeginLimit (если он не пуст). Он занимает последнее появление перед BeginLimit *****

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

// ***** Возвращает текст после SearchText *****

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

// ***** Возвращает текст между BeforeText и AfterText (без этих двух строк) *****

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

// ***** Возвращает текст между BeforeText и AfterText (с этими двумя строками) *****

function TextBetweenInc(WholeText: string; BeforeText: string; AfterText: string): string;
var
  FoundPos: Integer;
  WorkText: string;
begin
  RemainingText := WholeText;
  Result := '';
  FoundPos := Pos(BeforeText, WholeText);
  if FoundPos = 0 then
    Exit;
  WorkText := Copy(WholeText, FoundPos, Length(WholeText));
  FoundPos := Pos(AfterText, WorkText);
  if FoundPos = 0 then
    Exit;
  Result := Copy(WorkText, 1, FoundPos - 1 + Length(AfterText));
  RemainingText := Copy(WorkText, FoundPos + Length(AfterText), Length(WorkText));
end;

// ***** Возвращает номер строчки первого вхождения от StartAt в TStringList, возвращает -1 если не найдено *****

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

// ***** Возвращает номер первого вхождения полной строчки от StartAt в TStringList, возвращает -1 если не найдено *****

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

// ***** Возвращает отредактированный текст. Убирает табуляцию, двойные пробелы *****

function ClearSpecialChars(Value: string): string;
begin
  Result := '';
  while Pos(#9, Value) > 0 do
    Result := StringReplace(Value, #9, '');
  while Pos('  ', Value) > 0 do
    Result := StringReplace(Value, '  ', ' ');
end;

// ***** Удаление табуляции, пробелов и разрывов строк в начале и конце *****

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

// ***** Подсчёт количества вхождений символа в строке *****

function CntChRepet(InputStr: string; InputSubStr: char): integer;
var
  i: integer;
  x: string;
begin
  result := 0;
  for i := 1 to length(InputStr) do
    begin
      x:= Copy(InputStr, i, 1);
      if x = InputSubStr then
        Result := Result + 1;
    end;
end;

begin
end.
