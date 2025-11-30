(*=============================================================
	Gigibop's Library
	Authors=Gigibop (luca.marcato@gmail.com)
	Description= Useful Procedures and Functions
	Version=1.0
 	
	ChangeLog:
	27.08.2010 v. 1.0: First revision (Gigibop)

	Joyride... Si o no??? Ma anche no!!!
=============================================================*)

Unit GiLib;
uses
	StringUtils1;
Var
  TPage: TStringList;
  
//===================================================
// Constructor/Finalize lib
//===================================================
Procedure NewGiLib;
Begin
  TPage := TStringList.Create;  
End;

Procedure FinalizeGiLib;
Begin
  TPage.Free;
End;

//===================================================
// Remove Tabs from a string
//===================================================
Function GRemoveTabs(sVal : String) : String;
Begin
	Repeat
		sVal:= StringReplace(sVal, '   ', '');
	Until (pos('   ',sVal) = 0);
	
	result := sVal;
end;

//===================================================
// Left and Right Trim from a string
//===================================================
Function GLeftRightTrim(sVal: String): String;
Var
	bExit: Boolean;
Begin
    
	bExit := False;
  
	Repeat
		Case copy(sVal, 1, 1) Of ' ', #9, #10, #13:
			sVal := copy(sVal, 2, Length(sVal)-1);
		Else
			bExit := True;
		End;
	Until bExit;
  
	bExit := False;
  
	Repeat
		Case copy(sVal, Length(sVal), 1) Of ' ', #9, #10, #13: 
			sVal := copy(sVal, 1, Length(sVal)-1);
		Else
			bExit := True;
		End;
	Until bExit;
  
	result := sVal;
End;

//===================================================
// Html Decode and Trim
//===================================================
Function GHtmlDecode(sVal: String ; bTrim: boolean) :String;
Begin
	  
	HTMLDecode(sVal);
   
	If bTrim = True Then
		sVal := GLeftRightTrim(sVal);
   
	result := sVal;
End;

//===================================================
// Html Decode, Trim and Remove Tabs
//===================================================
Function GHtmlDecodeAndClean(sVal: String; bTabs: Boolean; bTrim: Boolean) :String;
Begin

	HTMLRemoveTags(sVal);
	HTMLDecode(sVal);
  
	If bTabs = True Then
		sVal := GRemoveTabs(sVal);
	If bTrim = True Then
		sVal := GLeftRightTrim(sVal);
		
	result := sVal;
End;

//===================================================
// Join Multiple Lines in a String
//===================================================
Function GJoinMultipleLines(sPage: TStringList; sTextFrom: String; sTextTo: String; iStartLine: Integer) :String;
Var
	IEnd,I : Integer;
	sLine: String;
Begin

	IEnd := 0;
	sLine := '';
	
	iStartLine := FindLine(sTextFrom,sPage,iStartLine);

	If iStartLine > 0 Then IEnd := FindLine(sTextTo,sPage,iStartLine);

	For I:= iStartLine To IEnd Do
	Begin
		sLine:= sLine + sPage.GetString(i);;
	End;

  result := sLine;

End;

Procedure GDebug(sVal: String; sFileName: String);
Begin
  
  TPage.Text := sVal;
  TPage.SaveToFile(sFileName);
  TPage.Free;
End;
  
End.