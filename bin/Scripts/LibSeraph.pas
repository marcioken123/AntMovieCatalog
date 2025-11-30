unit LibSeraph;

var
  Page: string;

   // Get the page from url cleaning tabs characters
   // IN :
   // Url (string)
   // OUT :
   procedure GetCleanPage(Url: string);
   var
     PackedPage: string;
    cLine: string;
     n: Integer;
     htmlPage: TStringList;
   begin
     htmlPage := TStringList.Create;
     htmlPage.Text := GetPage(Url);
     for n := 0 to htmlPage.Count - 1 do begin
      cLine := StringReplace(htmlPage.GetString(n),#9,'');
       PackedPage := PackedPage + ' ' + cLine;
    end;
     Page := PackedPage;
     htmlPage.Free;
   end;
   
   // --------------------------------------------------
   // Get text between the marker than clean it
   // IN:  Start marker     (String)
   //      End marker       (string)
   //      Cut End Marker   (bool)
   //      Adjust extracted text (bool)
   // OUT: value            (string)
   // --------------------------------------------------
   function GetValue(cStartMarker,cEndMarker: string;bCutEnd,bAdjust: boolean): string;
   var
     StartPos: integer;
     EndPos: integer;
     Value: string;
   begin
     Value := '';
     StartPos := pos(cStartMarker, Page);
     if ( StartPos = 0 ) then exit; // Not found

     Delete(Page, 1, StartPos - 1 + length(cStartMarker));
     EndPos := pos(cEndMarker, Page);
     EndPos := EndPos - 1;
     if ( EndPos > 0 ) then begin
       Value := copy(Page, 0, EndPos);
       if bCutEnd then
         EndPos := EndPos + length(cEndMarker);
       Delete(Page, 1, EndPos);
       if bAdjust then
         Value := '<' + Value;
     end;
     HTMLRemoveTags(Value);
     HTMLDecode(Value);
     result := Trim(Value);
   end;
end;