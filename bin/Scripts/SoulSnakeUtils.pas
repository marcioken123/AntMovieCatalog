(***************************************************

Ant Movie Catalog importation script
www.antp.be/software/moviecatalog/

[Infos]
Authors=SoulSnake
Title=SoulSnakeUtils.pas
Description=SoulSnake utils
Site=http://mickaelvanneufville.online.fr/AMCU/scripts/
Language=?
Version=1.3 du (29/07/2012)
Requires=4.1.0
Comments=
License=
GetInfo=0

[Options]

***************************************************)

unit SoulSnakeUtils;

const SoulSnakeUtils_Version = 1.3;

{
  History
  -------
  v.1.0:  Add FormatFilename, GenerateFilename,
          GenerateFilenameWithGetOptions functions.
  v.1.1:  English version.
  v.1.2:  Add SelectFieldOrCustomField, GetFieldOrCustomField, CanSetFieldOrCustomField and SetFieldOrCustomField functions.
  v.1.3:  Move SelectFieldOrCustomField, GetFieldOrCustomField, CanSetFieldOrCustomField and SetFieldOrCustomField functions in library FieldsUtils.pas
}


{
  Format filename for windows and linux.
}
function FormatFilename(title : string) : string;
begin
  result := title;
  result := StringReplace(result, '?', '');
  result := StringReplace(result, ':', '-');
  result := StringReplace(result, '<', '');
  result := StringReplace(result, '>', '');
  result := StringReplace(result, '"', '');
  result := StringReplace(result, '*', '');
  result := StringReplace(result, '|', '-');
  result := StringReplace(result, '/', '-');
  result := StringReplace(result, '\', '-');
  result := StringReplace(result, '  ', ' ');
  result := StringReplace(result, '( ', '(');
  result := StringReplace(result, ' )', ')');
  result := Trim(result);
end;

{
  Generate filename according to the following local options :
optNumber|1=Yes|0=No
optTitle|0=Translated title [Original title]|1=Translated title|2=Original title
optYear|1=Yes|0=No
optLanguages|1=Yes|0=No
optSubtitles|1=Yes|0=No
optCategories|1=Yes|0=No
optSource|1=Yes|0=No
optVideoFormat|1=Yes|0=No
optAudioFormat|1=Yes|0=No
optFrameRate|1=Yes|0=No
}
function GenerateFilename(optNumber : Integer;
optTitle : Integer;
optYear : Integer;
optLanguages : Integer;
optSubtitles : Integer;
optCategories : Integer;
optSource : Integer;
optVideoFormat : Integer;
optAudioFormat : Integer;
 optFrameRate : Integer) : string;
var
  filename, tmp, tmp2 : string;
  i, maxsize : Integer;

begin
  maxsize := 220;
  filename := '';

  if (GetField(fieldOriginalTitle) <> '') or (GetField(fieldTranslatedTitle) <> '') then
  begin
    filename := '$$$$';

    if (optNumber = 1) then
    begin
      i := StrToInt(GetField(fieldNumber), 0);
      tmp := '';
      if (i < 10) then
        tmp := tmp + '0';
      if (i < 100) then
        tmp := tmp + '0';
      if (i < 1000) then
        tmp := tmp + '0';
      filename := StringReplace(filename, '$$$$', '[' + tmp + IntToStr(i) + '] $$$$');
    end;
    
    if (GetField(fieldTranslatedTitle) <> '') and ((GetField(fieldOriginalTitle) = '') or (optTitle = 0) or (optTitle = 1)) then
      filename := StringReplace(filename, '$$$$', GetField(fieldTranslatedTitle) + '$$$$');

    if (GetField(fieldOriginalTitle) <> '') and ((GetField(fieldTranslatedTitle) = '') or (optTitle = 2)) then
      filename := StringReplace(filename, '$$$$', GetField(fieldOriginalTitle) + '$$$$');

    if (length(filename) < maxsize) and (optYear = 1) and (GetField(fieldYear) <> '') then
    begin
      tmp := filename + ' ' + GetField(fieldYear);
      if (length(tmp) < maxsize) then
        filename := tmp;
    end;

    if (length(filename) < maxsize) and (optLanguages = 1) and (GetField(fieldLanguages) <> '') then
    begin
      tmp2 := GetField(fieldLanguages);
      tmp2 := StringReplace(tmp2, 'FR (VFF)', 'VFF');
      tmp2 := StringReplace(tmp2, 'FR (VFQ)', 'VFQ');
      tmp2 := StringReplace(tmp2, ', ', ',');
      tmp2 := StringReplace(tmp2, ' ,', ',');
      tmp2 := StringReplace(tmp2, ',', ' ');
      tmp := filename + ' ' + tmp2;
      if (length(tmp) < maxsize) then
        filename := tmp;
    end;
    
    if (length(filename) < maxsize) and (optAudioFormat = 1) and (GetField(fieldAudioFormat) <> '') then
    begin
      tmp2 := GetField(fieldAudioFormat);
      // tmp2 := StringReplace(tmp2, 'MPEG-1/2 L3', 'MP3');
      // tmp2 := StringReplace(tmp2, 'MPEG-1 Audio layer 3', 'MP3');
      // tmp2 := StringReplace(tmp2, 'WMA2', 'WMA');
      tmp2 := StringReplace(tmp2, ', ', ' ');
      tmp := filename + ' ' + tmp2;
      if (length(tmp) < maxsize) then
        filename := tmp;
    end;
    
    if (length(filename) < maxsize) and (optVideoFormat = 1) and (GetField(fieldVideoFormat) <> '') then
    begin
      tmp2 := GetField(fieldVideoFormat);
      tmp2 := StringReplace(tmp2, 'DivX ', 'DivX');
      tmp2 := StringReplace(tmp2, ', ', ',');
      tmp2 := StringReplace(tmp2, ' ,', ',');
      tmp2 := StringReplace(tmp2, ',', ' ');
      tmp := filename + ' ' + tmp2;
      if (length(tmp) < maxsize) then
        filename := tmp;
    end;
    
    if (length(filename) < maxsize) and (optFrameRate = 1) and (GetField(fieldFrameRate) <> '') then
    begin
      tmp2 := GetField(fieldFrameRate);
      tmp2 := StringReplace(tmp2, '.000', '');
      tmp2 := tmp2 + 'ims';
      tmp := filename + ' ' + tmp2;
      if (length(tmp) < maxsize) then
        filename := tmp;
    end;

    if (length(filename) < maxsize) and (optSubtitles = 1) and (GetField(fieldSubtitles) <> '') then
    begin
      tmp2 := GetField(fieldSubtitles);
      tmp2 := StringReplace(tmp2, ', ', ',');
      tmp2 := StringReplace(tmp2, ' ,', ',');
      tmp2 := StringReplace(tmp2, ',', ' ST');
      tmp := filename + ' ST' + tmp2;
      if (length(tmp) < maxsize) then
        filename := tmp;
    end;

    if (length(filename) < maxsize) and (optSource = 1) and (GetField(fieldSource) <> '') then
    begin
      tmp2 := GetField(fieldSource);
      tmp2 := StringReplace(tmp2, ', ', ',');
      tmp2 := StringReplace(tmp2, ' ,', ',');
      tmp2 := StringReplace(tmp2, ',', ' ');
      tmp2 := StringReplace(tmp2, ' Version Originale', '');
      if (GetField(fieldMediaType) <> StringReplace(GetField(fieldMediaType), 'RIP', '')) then
      begin
        tmp2 := StringReplace(tmp2, 'DVD', 'DVDRip');
        tmp2 := StringReplace(tmp2, 'BD', 'BDRip');
        tmp2 := StringReplace(tmp2, 'HDDVD', 'HDDVDRip');
      end;
      tmp := filename + ' ' + tmp2;
      if (length(tmp) < maxsize) then
        filename := tmp;
    end;
    
    if (length(filename) < maxsize) and (optCategories = 1) and (GetField(fieldCategory) <> '') then
    begin
      tmp2 := GetField(fieldCategory);
      tmp2 := StringReplace(tmp2, ', ', ',');
      tmp2 := StringReplace(tmp2, ' ,', ',');
      tmp2 := StringReplace(tmp2, ',', ' ');
      if (optSource = 1) then
      begin
        tmp2 := StringReplace(tmp2, 'Animation', '');
        tmp2 := StringReplace(tmp2, '  ', ' ');
        tmp2 := StringReplace(tmp2, 'Walt Disney', '');
        tmp2 := StringReplace(tmp2, '  ', ' ');
        tmp2 := StringReplace(tmp2, 'Manga', '');
        tmp2 := StringReplace(tmp2, '  ', ' ');
        tmp2 := StringReplace(tmp2, 'Humour', '');
        tmp2 := StringReplace(tmp2, '  ', ' ');
        tmp2 := StringReplace(tmp2, 'Concert', '');
        tmp2 := StringReplace(tmp2, '  ', ' ');
        tmp2 := StringReplace(tmp2, 'Théâtre', '');
        tmp2 := StringReplace(tmp2, '  ', ' ');
        tmp2 := StringReplace(tmp2, 'Comédie musicale', '');
        tmp2 := StringReplace(tmp2, '  ', ' ');
        tmp2 := StringReplace(tmp2, 'Série TV', '');
        tmp2 := StringReplace(tmp2, '  ', ' ');
        tmp2 := StringReplace(tmp2, 'Série', '');
        tmp2 := StringReplace(tmp2, '  ', ' ');
        tmp2 := StringReplace(tmp2, 'Documentaire', '');
        tmp2 := StringReplace(tmp2, '  ', ' ');
        tmp2 := Trim(tmp2);
      end;
      tmp := filename + ' ' + tmp2;
      if (length(tmp) < maxsize) then
        filename := tmp;
    end;

    if (length(filename) < maxsize) and (optTitle = 0) and (GetField(fieldOriginalTitle) <> '') and (GetField(fieldTranslatedTitle) <> '') and (GetField(fieldOriginalTitle) <> GetField(fieldTranslatedTitle)) then
    begin
      tmp := StringReplace(filename, '$$$$', ' [' + GetField(fieldOriginalTitle) + ']$$$$');
      if (length(tmp) < maxsize) then
        filename := tmp;
    end;

    filename := StringReplace(filename, '$$$$', '');
    filename := FormatFilename(filename);

    if (length(filename) > maxsize - 4) then
    begin
      filename := copy(filename, 0, maxsize - 4);
      filename := Trim(filename);
    end;
  end;
  result := filename;
end;

{
  Generate filename according to the following general options :
Number=1|1|1=Yes|0=No
Title=0|0|0=Translated title [Original title]|1=Translated title|2=Original title
Year=1|1|1=Yes|0=No
Languages=1|1|1=Yes|0=No
Subtitles=1|1|1=Yes|0=No
Categories=1|0|1=Yes|0=No
Source=1|0|1=Yes|0=No
VideoFormat=1|0|1=Yes|0=No
AudioFormat=1|0|1=Yes|0=No
Framerate=1|0|1=Yes|0=No
}
function GenerateFilenameWithGetOptions() : string;
begin
  result := GenerateFilename(GetOption('Number'), GetOption('Title'), GetOption('Year'),
    GetOption('Languages'), GetOption('Subtitles'), GetOption('Categories'),
    GetOption('Source'), GetOption('VideoFormat'), GetOption('AudioFormat'), GetOption('Framerate'));
end;

// *****
begin
end.
