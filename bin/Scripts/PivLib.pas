(***************************************************


    Pivello's Library - Rel. 1.0 - 27.02.2005

  Free use and distribute; no license required ;)


***************************************************)

unit PivLib;
  
var
  Page: TStringList;
  TmpPageStr: string;


// --------------------------------------------------
// INITIALIZATION & FINALIZATION of library
// IN:  none
// OUT: none
// --------------------------------------------------
procedure InitializePivLib;
begin
  Page := TStringList.Create;
  TmpPageStr := '';
end;

procedure FinalizePivLib;
begin
  Page.Free;
end;


// --------------------------------------------------
// PAGE PACKING (remove extra spaces, tabs & CR)
// IN:  page Url     (string)
// OUT: compact page (string)
// --------------------------------------------------
function RemoveExtraChars(Url: string): string;
var
  Temp: string;
  PackedPage: string;
  CharPos: Integer;
  n: Integer;
begin
  Page.Text := GetPage(Url);
  for n := 0 to Page.Count - 1 do
    PackedPage := PackedPage + ' ' + Page.GetString(n);
  repeat
    CharPos := pos('  ', PackedPage);
    if CharPos = 0 then
      CharPos := pos(#9, PackedPage);
    if CharPos <> 0 then
      begin
        Temp := copy(PackedPage, 1, CharPos - 1);
        Delete(PackedPage, 1, CharPos);
        PackedPage := Temp + PackedPage;
      end;
  until((pos('  ', PackedPage) = 0) and (pos(#9, PackedPage) = 0));
  result := PackedPage;
end;


// --------------------------------------------------
// GET FIELD VALUES FROM PACKED PAGE
// IN:  Start marker     (String)
//      End marker       (string)
//      Cut Start Marker (bool)
//      Cut End Marker   (bool)
// OUT: value            (string)
// --------------------------------------------------
function GetValue(MyPageStr:  string; StartMarker, EndMarker: string;
  CutStartMarker, CutEndMarker: boolean): string;
var
  StartPos: integer;
  EndPos: integer;
  Value: string;
begin
  Value := '';
  StartPos := pos(StartMarker, MyPageStr);
  if (StartPos > 0) then
    begin
      Delete(MyPageStr, 1, StartPos - 1);
    end;
  StartPos := pos(StartMarker, MyPageStr);
  EndPos := pos(EndMarker, MyPageStr);
  if ((StartPos > 0)and(EndPos > 0)) then
    begin
      if CutStartMarker then
        StartPos := StartPos + length(StartMarker);
      if not CutEndMarker then
        EndPos := EndPos + length(EndMarker);
      Value := copy(MyPageStr, StartPos, EndPos-Startpos);
      Delete(MyPageStr, 1, EndPos);
    end;
  TmpPageStr := MyPageStr;
  result := Value;
end;


// --------------------------------------------------
// GET TEMPORARY PageStr
// IN:  none
// OUT: TmpPageStr (string)
// --------------------------------------------------
function GetPageStr: string;
begin
  result := TmpPageStr;
end;


end.
