unit MyAnimeListUtils;

uses StringUtils1;

const

    HTML_CODES =  '&hellip;=…,' + '&laquo;=«,'  + '&raquo;=»,' + '&lsaquo;=‹,' +
                  '&rsaquo;=›,' + '&lsquo;=‘,'  + '&rsquo;=’,' + '&ldquo;=“,'  +
                  '&rdquo;=”,'  + '&sbquo;=,,'  + '&bdquo;=„,' + '&apos;='','  +
                  '&quot;="';
                  
    ASCENDING = TRUE;
    DESCENDING = FALSE;


//------------------------------------------------

function ValidatePercent(Value:string) : Boolean;
var
    i, num: Integer;
    ch: string;
    
begin
    result := FALSE;
    
    for i := 1 to Length(Value) do begin
        ch := Copy(Value,i,1);
        if (Pos(ch,'0123456789') = 0) or (i > 3)
            then EXIT;
    end;
    
    num := StrToInt(Value,0);
    if (num >= 0) and (num <= 100)
        then result := TRUE;
end;

//------------------------------------------------

procedure Sort(List:TStringList ; Invert:Boolean ; StartAt,EndAt:Integer);
var
    str1, str2: string;
    order: Boolean;
    
begin
    if (EndAt <= StartAt) then EXIT;
    
    str1 := List.GetString(StartAt);
    str2 := List.GetString(StartAt + 1);
    
    order := Length(str1) < Length(str2);
    if Invert then order := not order;
    
    if order and (Length(str1) <> Length(str2)) then begin
        List.SetString(StartAt + 1,str1);
        List.SetString(StartAt,str2);
    end;
    
    str1 := '';
    str2 := '';
    
    Sort(List,Invert,StartAt + 1,EndAt);
end;

//------------------------------------------------

procedure SortList(List:TStringList ; SortType:Boolean);
var
    i: Integer;
    
begin
    for i := 1 to List.Count do Sort(List,SortType,0,List.Count - i);
end;

//------------------------------------------------

function CompareText(Str1, Str2: string) :integer;
var
    list1, list2: TStringList;
    i, lineNr, charsMatch, totalChars:Integer;
    
begin
    if (Str1 = '') or (Str2 = '') then begin
        result := 0;
        EXIT;
    end;
    
    list1 := TStringList.Create;
    list2 := TStringList.Create;
    list1.Text := StringReplace(Str1,' ',#13#10);
    list2.Text := StringReplace(Str2,' ',#13#10);
    SortList(list1,DESCENDING);
    SortList(list2,DESCENDING);
    
    for i := 0 to (list1.Count - 1) do begin
        lineNr := FindLine(list1.GetString(i),list2,0);
        
        if (lineNr > -1) then begin
            charsMatch := charsMatch + Length(list1.GetString(i));
            Str1 := StringReplace(Str1,list1.GetString(i),'');
            list2.SetString(lineNr,#2);
        end;
        
        if (StringReplace(list2.Text,#2#13#10,'') = '') then Break;
    end;
    
    list1.Free;
    list2.Free;
    totalChars := Length(StringReplace(Str2,' ','')) + Length(StringReplace(Str1,' ',''));
    result := (charsMatch * 100) div totalChars;
end;

//------------------------------------------------

function BlankAndCompact(Str:string ; CharsToBlank:string) : string;
var
    ch: string;
    atPos: Integer;
    
begin
    for atPos := 1 to Length(CharsToBlank) do begin
        ch := Copy(CharsToBlank,atPos,1);
        Str := StringReplace(Str,ch,' ');
    end;
    
    while (Pos('  ',Str) > 0) do Str := StringReplace(Str,'  ',' ');
        
    result := Trim(Str);
end;

//------------------------------------------------

function RemoveNonAnsiHtmlNum(Str:string) : string;
var
    text, htmlNum, htmlCode: string;
    
begin
    htmlNum := TextBetween(Str,'&#',';');
    
    if (htmlNum <> '') then
        text := Copy(Str,1,Length(Str));
    
    while (htmlNum <> '') do begin
        htmlCode := '&#' + htmlNum + ';';
        
        if (StrToInt(htmlNum,0) > 255) then
            Str := StringReplace(Str,htmlCode,'?');
            
        text := StringReplace(text,htmlCode,'');
        htmlNum := TextBetween(text,'&#',';');
    end;
    
    result := Str;
end;

//------------------------------------------------

function HTMLDecodePatch(Str:string ; CodeTable:string) : string;
var
    table: TStringList;
    code, decode: string;
    i: Integer;
    
begin
    table := TStringList.Create;
    table.CommaText := CodeTable;
    
    for i := 0 to (table.Count - 1) do begin
        code := table.GetName(i);
        decode := TextAfter(table.GetString(i),'=');
        Str := StringReplace(Str,code,decode);
    end;
    
    table.Free;
    result := Str;
end;

//------------------------------------------------

function funcHTMLDecode(Str:string) : string;
begin
    Str := HTMLDecodePatch(Str,HTML_CODES);
    Str := RemoveNonAnsiHtmlNum(Str);
    HTMLDecode(Str);
    result := Str;
end;

//------------------------------------------------

function funcUTF8Decode(Str:string) : string;
begin
    result := UTF8Decode(Str);
    if (result = '') then result := Str;
end;

//------------------------------------------------

function funcHTMLRemoveTags(Str:string) : string;
begin
    HTMLRemoveTags(Str);
    result := Str;
end;

//------------------------------------------------

function TextDecode(Text:string ; StartAt:string ; EndAt:string) : string;
begin
    result :=
        FullTrim(
            funcHTMLDecode(
                funcUTF8Decode(
                    funcHTMLRemoveTags(
                        TextBetween(Text,StartAt,EndAt)
    ))));
end;

end.