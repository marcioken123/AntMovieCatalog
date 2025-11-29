(************************************************************************
 *                                                                      *
 *   Ant Movie Catalog 4.x                                              *
 *   (C) 2000-2018 Antoine Potten, Mickaël Vanneufville                 *
 *   http://www.antp.be/software                                        *
 *                                                                      *
 ************************************************************************
 *                                                                      *
 *   This program is free software; you can redistribute it and/or      *
 *   modify it under the terms of the GNU General Public License        *
 *   as published by the Free Software Foundation; either version 2     *
 *   of the License, or (at your option) any later version.             *
 *                                                                      *
 *   This program is distributed in the hope that it will be useful,    *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of     *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *
 *   GNU General Public License for more details.                       *
 *                                                                      *
 ************************************************************************)

program AntMovieCatalog;

uses
  Graphics in 'DelphiFix\Graphics.pas',
  HTMLGif2 in 'DelphiFix\HTMLGif2.pas',
  HTMLSubs in 'DelphiFix\HTMLSubs.pas',
  JConsts in 'DelphiFix\JConsts.pas',
  JPEG in 'DelphiFix\JPEG.pas',
  StdCtrls in 'DelphiFix\StdCtrls.pas',
  ComCtrls in 'DelphiFix\ComCtrls.pas',
  TB2Dock in 'DelphiFix\TB2Dock.pas',
  AntTranslator in 'Common\AntTranslator.pas',
  FileManager in 'Common\FileManager.pas',
  MediaInfo in 'Common\MediaInfo.pas',
  base in 'Common\base.pas' {BaseDlg},
  inputform in 'Common\inputform.pas' {InputWin},
  messageform in 'Common\messageform.pas' {MessageWin},
  memoform in 'Common\memoform.pas' {MemoWin},
  listform in 'Common\listform.pas' {ListWin},
  frameLanguage in 'Common\frameLanguage.pas' {LanguageFrame: TFrame},
  functions_files in 'Common\functions_files.pas',
  functions_gui in 'Common\functions_gui.pas',
  functions_html in 'Common\functions_html.pas',
  functions_str in 'Common\functions_str.pas',
  functions_xml in 'Common\functions_xml.pas',
  functions_video in 'Common\functions_video.pas',
  functions_sys in 'Common\functions_sys.pas',
  functions_tbx in 'Common\functions_tbx.pas',
  functions_img in 'Common\functions_img.pas',
  Forms,
  SysUtils,
  Controls,
  Windows,
  Global in 'src\providers\Global.pas',
  getscript_readscripts in 'src\providers\getscript_readscripts.pas',
  PictureSelection in 'src\providers\PictureSelection.pas',
  movieclass in 'src\providers\movieclass.pas',
  fields in 'src\providers\fields.pas',
  ProgramSettings in 'src\providers\programsettings.pas',
  loanhistory in 'src\providers\loanhistory.pas',
  ConstValues in 'src\providers\ConstValues.pas',
  threaddownload in 'src\providers\threaddownload.pas',
  movieclass_old in 'src\providers\movieclass_old.pas',
  getscript_debug in 'src\providers\getscript_debug.pas',
  getmedia in 'src\providers\getmedia.pas',
  getscript_xml in 'src\providers\getscript_xml.pas',
  interfaces in 'src\providers\interfaces.pas',
  import2_engines in 'src\providers\import2_engines.pas',
  ExpressionParser in 'src\providers\ExpressionParser.pas',
  rmkFunctions in 'src\providers\rmkFunctions.pas',
  rmkGradient in 'src\providers\rmkGradient.pas',
  getscript_stringlistex in 'src\providers\getscript_stringlistex.pas',
  stringlistex in 'src\providers\stringlistex.pas',
  main in 'src\main.pas' {MainWindow},
  options in 'src\options.pas' {OptionsWin},
  about in 'src\about.pas' {AboutWin},
  export in 'src\export.pas' {ExportWin},
  number in 'src\number.pas' {NumberWin},
  loan in 'src\loan.pas' {LoanWin},
  progress in 'src\progress.pas' {ProgressWin},
  framefields in 'src\framefields.pas' {FieldsFrame: TFrame},
  sort in 'src\sort.pas' {SortWin},
  properties in 'src\properties.pas' {PropertiesWin},
  splash in 'src\splash.pas' {SplashWin},
  stats in 'src\stats.pas' {StatsWin},
  framesortby in 'src\framesortby.pas' {SortByFrame: TFrame},
  getscript in 'src\getscript.pas' {GetScriptWin},
  getscript_picktree in 'src\getscript_picktree.pas' {PickTreeWin},
  getscript_picklist in 'src\getscript_picklist.pas' {PickListWin},
  pictureform in 'src\pictureform.pas' {PictureWin},
  framemovie in 'src\framemovie.pas' {MovieFrame: TFrame},
  options_defaultvalues in 'src\options_defaultvalues.pas' {DefaultValuesWin},
  printform in 'src\printform.pas' {PrintWin},
  languageselect in 'src\languageselect.pas' {LanguageWin},
  frameincludemov in 'src\frameincludemov.pas' {IncludemovFrame: TFrame},
  getscript_results in 'src\getscript_results.pas' {ScriptResultsWin},
  FramePictureOperation in 'src\FramePictureOperation.pas' {PictureOperationFrame: TFrame},
  PictureDragDrop in 'src\PictureDragDrop.pas' {PictureDragDropWin},
  getscript_properties in 'src\getscript_properties.pas' {ScriptPropertiesWin},
  import2 in 'src\import2.pas' {ImportWin2},
  import2_frame in 'src\import2_frame.pas' {ImportFrame: TFrame},
  import2_frameCsv in 'src\import2_frameCsv.pas' {ImportFrameCSV: TFrame},
  import2_frameQuery in 'src\import2_frameQuery.pas' {ImportFrameQuery: TFrame},
  import2_frameDir in 'src\import2_frameDir.pas' {ImportFrameDir: TFrame},
  framemoviecustom in 'src\framemoviecustom.pas' {MovieFrameCustom: TFrame},
  framemovieextras in 'src\framemovieextras.pas' {MovieFrameExtras: TFrame},
  stringfilter in 'src\stringfilter.pas' {StringFilterWin},
  FramePictureSelectionOptions in 'src\FramePictureSelectionOptions.pas' {PictureSelectOptionsFrame: TFrame},
  picturesmanager in 'src\picturesmanager.pas' {PicturesManagerWin},
  FramePictureOperationExport in 'src\FramePictureOperationExport.pas' {PictureOperationExportFrame: TFrame},
  FrameHtmlTemplateEdit in 'src\FrameHtmlTemplateEdit.pas' {HTMLTemplateEdit: TFrame},
  HTMLEditor in 'src\HTMLEditor.pas' {HTMLEditorWin},
  frameextra in 'src\frameextra.pas' {ExtraFrame: TFrame},
  extrasedit in 'src\extrasedit.pas' {ExtrasEditWin},
  framesortbyextras in 'src\framesortbyextras.pas' {ExtrasSortByFrame: TFrame},
  extrasrenumber in 'src\extrasrenumber.pas' {ExtrasRenumberWin},
  getscript_extrasresults in 'src\getscript_extrasresults.pas' {ScriptExtrasResultsWin},
  options_extradefaultvalues in 'src\options_extradefaultvalues.pas' {ExtraDefaultValuesWin},
  frameincludepic in 'src\frameincludepic.pas' {IncludepicFrame: TFrame},
  framefilenaming in 'src\framefilenaming.pas' {FileNamingFrame: TFrame},
  customfieldsmanager in 'src\customfieldsmanager.pas' {CustomFieldsManagerWin},
  renumber in 'src\renumber.pas' {RenumberWin},
  Gopage in 'src\Gopage.pas' {GoPageForm},
  importmethod in 'src\importmethod.pas' {ImportMethodWin},
  PreviewForm in 'src\PreviewForm.pas' {PreviewForm},
  PrintStatusForm in 'src\PrintStatusForm.pas' {PrnStatusForm};

{$R *.res}
{$R MANIFEST.RES}
{$R TOOLBARS.RES}
{$R ICONS.RES}
{$R TEMPLATES.RES}

{$I+}

begin
  SetWaitCursor;
  try
    {$IFDEF DISABLETHEMES}
    IsThemedXP := False;
    Graphics.DefFontData.Name := 'MS Sans Serif';
    {$ELSE}
    if IsWindowsNT then
      Graphics.DefFontData.Name := 'MS Shell Dlg 2'
    else
      Graphics.DefFontData.Name := 'MS Shell Dlg';
    {$ENDIF}
    Graphics.DefFontData.Charset := DEFAULT_CHARSET;
    Application.Initialize;
    Application.Title := 'Ant Movie Catalog 4';
    Application.HelpFile := '';
    Settings := TSettings.Create;
    Translator := TAntTranslator.Create(nil);
    Settings.Load;
    SplashWin := TSplashWin.Create(nil);
    with SplashWin do
    begin
      ProgressBar1.Max := 70;
      ProgressBar1.Position := 0;
      if Settings.rOptions.rDisplay.Logo then
        Show;
      Application.ProcessMessages;
      ProgressBar1.Position := 11;
      ProgressBar1.Position := 10;
      Application.CreateForm(TMainWindow, MainWindow);
  Application.CreateForm(TCustomFieldsManagerWin, CustomFieldsManagerWin);
  Application.CreateForm(TRenumberWin, RenumberWin);
  Application.CreateForm(TGoPageForm, GoPageForm);
  Application.CreateForm(TImportMethodWin, ImportMethodWin);
  Application.CreateForm(TPrnStatusForm, PrnStatusForm);
  strFields := MainWindow.Fields.Strings;
      strExtraFields := MainWindow.ExtraFields.Strings;
      strMedia := MainWindow.Media.Strings;
      strPictureStatus := MainWindow.PictureStatus.Strings;
      ProgressBar1.Position := 21;
      ProgressBar1.Position := 20;
      Application.CreateForm(TMessageWin, MessageWin);
      Application.CreateForm(TInputWin, InputWin);
      ProgressBar1.Position := 31;
      ProgressBar1.Position := 30;
      Application.CreateForm(TNumberWin, NumberWin);
      Application.CreateForm(TProgressWin, ProgressWin);
      ProgressBar1.Position := 41;
      ProgressBar1.Position := 40;
    end;
  finally
    RestoreCursor;
  end;
  Application.Run;
  SplashWin.Free;
  Translator.Free;
  Settings.Free;
end.

