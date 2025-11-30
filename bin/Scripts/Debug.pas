unit Debug;

const
  SourcePath = 'e:\download\';

function GetLocalPage(Address: string): string;
var
  Page: TStringList;
  FileName: string;
begin
  Page := TStringList.Create;
  FileName := Address;
  Input('', 'File to read:', FileName);
  Page.LoadFromFile(SourcePath + FileName);
  Result := Page.Text;
  Page.Free;
end;

function SavePage(Address: string; FileName: string): string;
var
  Page: TStringList;
begin
  Page := TStringList.Create;
  Page.Text := GetPage(Address);
  Page.SaveToFile(FileName);
  Result := Page.Text;
  Page.Free;
end;

procedure SaveToFile(Text: string; FileName: string);
var
  Page: TStringList;
begin
  Page := TStringList.Create;
  Page.Text := Text;
  Page.SaveToFile(FileName);
  Page.Free;
end;

begin
end.
