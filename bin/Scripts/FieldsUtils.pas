(***************************************************

Ant Movie Catalog importation script
www.antp.be/software/moviecatalog/

[Infos]
Authors=SoulSnake
Title=FieldsUtils.pas
Description=Functions to select, get or set fields and custom fields more easily
Site=http://mickaelvanneufville.online.fr/AMCU/scripts/
Language=Multi
Version=1.0 du (29/07/2012)
Requires=4.1.0
Comments=
License=
GetInfo=0

[Options]

***************************************************)

unit FieldsUtils;

const FieldsUtils_Version = 1.1;

{
  History
  -------
  v.1.0:  Add SelectFieldOrCustomField, GetFieldOrCustomFieldIdx, IsCustomFieldIdx,
          GetFieldOrCustomFieldTag, GetFieldOrCustomFieldName,
          GetFieldOrCustomField, CanSetFieldOrCustomField and SetFieldOrCustomField functions.
  v.1.1:  Change default title and message of PickTree in SelectFieldOrCustomField function (for AMC 4.1.2 and higher)
          Add SelectFieldOrCustomField2 function to define title and message of PickTree (for AMC 4.1.2 and higher)
}

{
  SelectFieldOrCustomField, GetFieldOrCustomFieldIdx, IsCustomFieldIdx,
  GetFieldOrCustomFieldTag, GetFieldOrCustomFieldName,
  GetFieldOrCustomField, CanSetFieldOrCustomField and SetFieldOrCustomField functions 
}

// Return idx of selected field or custom field
function SelectFieldOrCustomField2(Title: string; Msg: string): Integer;
var
  SelectedField: string;
  i: Integer;
begin
  PickTreeClear;
  if CheckVersion(4,1,2) = True then
    PickTreeTitle(Title);
  for i := 0 to GetFieldCount-1 do
    if i <> fieldNumber then
      PickTreeAdd(GetFieldName(i) + ' (' + GetFieldTag(i) + ')', IntToStr(i));
  for i := 0 to GetCustomFieldCount-1 do
    PickTreeAdd(GetCustomFieldName(GetCustomFieldTag(i)) + ' (' + GetCustomFieldTag(i) + ')', IntToStr(GetFieldCount + i));
  if CheckVersion(4,1,2) = True then
    PickTreeExec3(Msg, SelectedField)
  else
    PickTreeExec(SelectedField);
  if SelectedField <> '' then
    result := StrToInt(SelectedField, 0)
  else
    result := -1;
  if CheckVersion(4,1,2) = True then
    PickTreeDefaultTitle;
end;

function SelectFieldOrCustomField(): Integer;
begin
  result := SelectFieldOrCustomField2('Field selection', ''); 
end;

// Return idx of field or custom field if it exists, -1 otherwise
// For fields (not custom fields) idx is the same as fields constants (e.g. fieldNumber, fieldOriginalTitle)
// But if you only have field tag, you can call this function to retrieve idx
function GetFieldOrCustomFieldIdx(fieldTag: string): Integer;
var
  i: Integer;
begin
  result := -1;
  for i := 0 to GetCustomFieldCount-1 do
    if AnsiUpperCase(GetCustomFieldTag(i)) = AnsiUpperCase(fieldTag) then
    begin
      result := GetFieldCount + i;
      break;
    end;
  if result = -1 then
    for i := 0 to GetFieldCount-1 do
      if AnsiUpperCase(GetFieldTag(i)) = AnsiUpperCase(fieldTag) then
      begin
        result := i;
        break;
      end;
end;

// Return true if the specified idx refers to a custom field, false otherwise
function IsCustomFieldIdx(idx: Integer): Boolean;
begin
  result := false;
  if (idx >= GetFieldCount) and (idx < (GetFieldCount + GetCustomFieldCount)) then
    result := true;
end;

// Return the tag of field or custom field
function GetFieldOrCustomFieldTag(idx: Integer): string;
begin
  result := '';
  if idx < GetFieldCount then
    result := GetFieldTag(idx)
  else if idx < GetFieldCount + GetCustomFieldCount then
    result := GetCustomFieldTag(idx - GetFieldCount);
end;

// Return the name of field or custom field
function GetFieldOrCustomFieldName(idx: Integer): string;
begin
  result := '';
  if idx < GetFieldCount then
    result := GetFieldName(idx)
  else if idx < GetFieldCount + GetCustomFieldCount then
    result := GetCustomFieldName(GetCustomFieldTag(idx - GetFieldCount));
end;

// Return the value of field or custom field
function GetFieldOrCustomField(idx: Integer): string;
begin
  result := '';
  if idx < GetFieldCount then
    result := GetField(idx)
  else if idx < GetFieldCount + GetCustomFieldCount then
    result := GetCustomField(GetCustomFieldTag(idx - GetFieldCount));
end;

// Return true if value of field or custom field can be set, false otherwise
function CanSetFieldOrCustomField(idx: Integer): Boolean;
begin
  result := false;
  if idx < GetFieldCount then
    result := CanSetField(idx)
  else if idx < (GetFieldCount + GetCustomFieldCount) then
    result := CanSetCustomField(GetCustomFieldTag(idx - GetFieldCount));
end;

// Set the value of field or custom fields
procedure SetFieldOrCustomField(idx: Integer; fieldValue: string);
var
  fieldTag: string;
begin
  if idx < GetFieldCount then
  begin
    if CanSetField(idx) then
      SetField(idx, fieldValue);
  end
  else if idx < (GetFieldCount + GetCustomFieldCount) then
  begin
    fieldTag := GetCustomFieldTag(idx - GetFieldCount);
    if CanSetCustomField(fieldTag) then
      SetCustomField(fieldTag, fieldValue);
  end;
end;

// *****
begin
end.
