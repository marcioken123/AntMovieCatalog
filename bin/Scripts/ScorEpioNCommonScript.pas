(***************************************************

Ant Movie Catalog importation script
www.antp.be/software/moviecatalog/

[Infos]
Authors=
Title=ScorEpioNCommonScript.pas
Description=
Site=
Language=EN,FR
Version=26 du 22/02/2013
Requires=3.5.1
Comments=Modifié par Dedej
License=
GetInfo=0

[Options]

***************************************************)

unit ScorEpioNCommonScript;
(*******************************************************************************************************************
Partie commune à tous les scripts écris par ScorEpioN

AutoUpdate(LastUPD, VersionScript, NomScript) : Vérifie automatiquement la présence d'une mise à jour
coupeDate(Date,Option) : récupére le jour le mois ou l'année d'une date au format JJ/MM/SSAA
compareDate(Date1, Date2) : retourne le résultat de SSAAMMJJ - SSAAMMJJ
execMenuMAJ(VersionScript,NomScript) : Menu permettant la mise à jour du script
verifVersion(NomScript) : permet de vérifier et télécharger la dernière version d'un script
nameOfFile(NomScript) : retourne le nom du fuchier correspondant au script
verifVersionScriptCommun(Line) : permet de vérifier si l'on a besoin d'une nouvelle version su script commun
contactScorEpioN() : ouvre dans votre navigateur le topic de mes scripts sur le forum AMC.
dlAllScripts() : télécharge tous mes scripts.
downloadScript(NomScript); : télécharge la dernière version du script
menuScorEpioN() : rajoute à la liste de résultats les fonctions précédentes
execMenuScorEpioN(urlDomain, Address) : si le choix appartient à menuScorEpioN retourne vrai sinon retourne faux
MonDebug(Name, Line : String) : permet créer un fichier (dans le repertoire des scripts) contenant une chaine de caractère
valueNotEmpty(ValeurA, ValeurB : String) : retourne ValeurA si non vide, ValeurB autrement
DeleteTitle(field) : supprime le titre du champs si "en double"
formatTitre(titre,option) : retourne le titre avec la casse choisie en option
findInfo(Debut, Fin, Line, Option) : retourne la chaine de caractère entre "début" et "fin" avec/sans retour chariot
copyUntilLast(Value, Symbol) : copier une chaine jusqu'a la derniere occurence d'un caractere
supprSubString(Debut, Fin, Line) : retourne la chaine Line sans la sous chaine comprise entre debut et fin (inclus)
subStringExist(SousChaine, Chaine) : retourne Vrai si la sous chaine est trouvée
RCN(Nombre) : retourne Nombre retour chariot
coupeInfo(Debut, Fin, Condition, Line) : retourne la chaine jusqu'a ce que la sous-chaine comprise entre début et fin différe de Condition
deleteAsianChar(Value) : efface les caractères asiatique (attention doit s'éxécuter avant HTMLDecode)
deleteTab(Value) : supprime les tablulations
deleteMultiReturn(Value) : supprime les retours à la ligne multiple
deleteMultiSpace(Value) : supprime les espaces multiples
deleteEnd(Value, EndValue) : supprime une EndValue de Value si EndValue termine la chaine
gestionSeparateur(Value, Separateur) : reformate les listes avec des séparateurs
MonSetField(field,value) : Rentre les infos dans le catalogue si non nulles
MonGetPicture(value: string) : Récupére l'image si value non nulle
FormatFloat(value: String) : Retourne une chaine de caractère formater pour faire des calculs avec des float;
isDate(value: String) : Retourne la date si est une date à 4 chiffres, chaine vide autrement
supprQuote(title) : remplace les " par des ''
cleanTitle(title) : retourne un titre nettoyé des tags, et extensions (supprExt(title, ext))
transformCountry(country) : transforme les nationalités en nom de pays dans le champs Pays
supprimeLesAccents(NomFilm) : supprime les caractères accentués
initLesArticles() : crée la liste des articles
supprimeLesArticles(NomFilm) : supprime les articles
articleDebutTitre(title,Debut,Fin) : Remet les articles en début de titre
conversionNR(NomFilm, Sens) : converti les chiffres romains en chiffre (et inversement) // Sens = 'RvN' ou 'NvR'
recupTitreRecherche(option) : récupére le titre original ou traduit suivant l'option
titreDouble(option) : garde le titre voulue en option si titre en double
moveComments() : mets le champs commentaire a la suite du champs description
batch(Nom) : on donne le nom du site, et on lance le dialogue pour choisir la destination
createLog() : création du fichier de log
AddToLog() : ajoute une ligne au fichier de log
beforeUpdate() : ajoute au fichier de log les informations de la fiche avant mise à jour
afterUpdate() : ajoute au fichier de log les informations de la fiche après mise à jour
Champsmodifiables() : test si utilisation de champs modifiables et renvoie la liste

*******************************************************************************************************************)
const
	FreeUrl = 'http://joel.desseaux.free.fr/ScorEpioncommonscript/';
	VersionCommun = '25 du 07/03/2012';
	RC = #13#10;

var
  log, script : TStringList;
  fichierlog, adresselog, action : string;
  newSC, newS, execlog, autoUPD : Boolean;
  compteurMAJ : Integer;


//------------------------------------------------------------------------------
// AUTO-UPDATE
//------------------------------------------------------------------------------

procedure AutoUpdate(LastUPD, CurrentScript, NameScript : string);
var
  diff : Integer;
begin
  if LastUPD <> '' then
  begin
    diff := compareDate(GetField(fieldDate), LastUPD);
    if (diff > 15) then
    begin
      autoUPD := True;
      verifVersion(NameScript,CurrentScript)
      autoUPD := False;
    end;
  end;
end;

//------------------------------------------------------------------------------
// COUPE UNE DATE
//------------------------------------------------------------------------------

function coupeDate(aDate : String; Option : Integer) : String;
begin
  Case Option of
    0 : aDate := copy(aDate, 1, 2); // Jour
    1 : aDate := copy(aDate, 4, 2); // Mois
    2 : aDate := copy(aDate, 7, 4); // Annee
 else ShowMessage('Mauvais choix d''option');
 end;
 result := aDate;
end;

//------------------------------------------------------------------------------
// COMPARE DEUX DATES ET RETOURNE LE NOMBRE DE JOURS ENTRE
//------------------------------------------------------------------------------

function compareDate(Date1, Date2 : String) : Integer;
var SSAAA1, MM1, JJ1, SSAAA2, MM2, JJ2 : String;
begin
  SSAAA1 := coupeDate(Date1,2);
  MM1 := coupeDate(Date1,1);
  JJ1 := coupeDate(Date1,0);
  SSAAA2 := coupeDate(Date2,2);
  MM2 := coupeDate(Date2,1);
  JJ2 := coupeDate(Date2,0);
  result := StrToInt(SSAAA1+MM1+JJ1,2) - StrToInt(SSAAA2+MM2+JJ2,2);
end;

//------------------------------------------------------------------------------
// MENU DE MISE A JOUR
//------------------------------------------------------------------------------

procedure execMenuMAJ(CurrentScript,NameScript : String);
begin
  PickTreeClear;
  PickTreeAdd('Menu de mise à jour', '');
  PickTreeAdd('Verifier si vous avez la dernière version du script '+NameScript, 'version');
  PickTreeAdd('Lister tous mes scripts', 'liste');
  PickTreeAdd('Télécharger tous mes scripts', 'allScripts');
  PickTreeAdd('Pour me contacter', 'contact');
  PickTreeExec(action);

  if (action = 'version') then
       verifVersion(NameScript,CurrentScript)
  else if (action = 'contact') then
       contactScorEpioN()
  else if (action = 'allScripts') then
       dlAllScripts()
  else if (action = 'liste') then
       listeScriptScorEpioN();

{
// A mettre dans option
Mise à jour=1|1|0=Oui|1=Non

// A mettre au début du programme principal
if GetOption('Mise à jour') = 0 then
    begin
       execMenuMAJ(VersionScript,NomScript);
       exit;
    end;
}
end;

//------------------------------------------------------------------------------
// VERIFIER LA VERSION DU SCRIPT
//------------------------------------------------------------------------------

procedure verifVersion(NameScript, VersionCourante : String);
var
   Line, LineCommon, NewVersion, LeMessage, Larticle, NomFichier, ContenuScript : String;
   BeginPos, EndPos : Integer;
begin
  newSC := False;  // Il n'y a pas de maj à faire.
  newS := False;
  LeMessage := '';
  Larticle := 'la mise';
  Line := GetPage(FreeUrl+nameOfFile(NameScript));
  //LineCommon := Line;
  BeginPos := pos('Version=', Line);
  delete(Line,1, BeginPos + 8);
  EndPos := pos(' du', Line);
  NewVersion := copy(Line, 1, EndPos - 1);
  // TEST LA PRESENCE DE NOUVELLES VERSIONS
  if (verifVersionScriptCommun() = True) then
    newSC := True;
  if (nouvelleVersion(VersionCourante, NewVersion) = True) then
    newS := True;
  // EN FONCTION DES MAJ A FAIRE LES MESSAGES CHANGENT
  if (newS = True) then
    LeMessage := 'Il y a une nouvelle version du script ' + NameScript + ' : '+NewVersion+'.'+RC;
  if (newSC = True) then
    LeMessage := LeMessage+'Il y a une nouvelle version du script commun.';
  if (newS = True) and (newSC = True) then
    Larticle := 'les mises';
  // AJOUTE L'AVERTISSEMENT
  LeMessage := LeMessage+RCN(2)+'Cliquer sur ''''OUI'''' pour effectuer '+Larticle+' à jour dans le répertoire :'+RCN(2)+''''+dirScripts+''''+RCN(2)+'Cliquer sur ''''NON'''' annuler.';
  if (newS = False) and (newSC = False) then
  begin
    if autoUPD <> True then
      ShowInformation('Pas de mise à jour à effectuer.');
  end else
    if (ShowConfirmation(LeMessage) = True) then // TELECHARGE LES SCRIPTS
    begin
      if (newS = True) then
        downloadScript(NameScript);
      if (newSC = True) then
        downloadScript('SCOREPIONCOMMONSCRIPT');
    end else
      exit;
end;

//------------------------------------------------------------------------------
// RETOURNE LE NOM DU FICHIER
//------------------------------------------------------------------------------

function nameOfFile(NameScript : String) : String;
var
  Articles: array of string;
  i: integer;
  NomFichier, NomScriptTest  : String;
begin
  SetArrayLength(Articles,10);
//  Articles[0]:='ALLOCINE|Allocine%20(FR).ifs';
  Articles[0]:='AMAZON.FR|Amazon%20(FR).ifs';
  Articles[1]:='AMAZON.FR (IMAGE ONLY)|Amazon%20Image%20(FR).ifs';
//  Articles[3]:='ANIMEKA|Animeka%20(FR).ifs';
//  Articles[2]:='CINEMASIE|Cinemasie%20(FR).ifs';
  Articles[2]:='ERREURS DE FILMS|Erreurs%20de%20films%20(FR).ifs';
  Articles[3]:='NIHON-FR|Nihon%20(FR).ifs';
//  Articles[4]:='UPDATE FIELDS|Update%20Fields%20(FR-US).ifs';
  Articles[4]:='SCOREPIONCOMMONSCRIPT|ScorEpioNCommonScript.pas';
  Articles[5]:='CINEMOVIES|Cinemovies%20(FR).ifs';
  Articles[6]:='YUSUKETEAM|Yusuketeam%20(FR).ifs';
  Articles[7]:='DVDPOST|DVDPost%20(FR).ifs';
  Articles[8]:='HTMLTOCHM|HtmlToChm.ifs';
  Articles[9]:='CDISCOUNT|Cdiscount%20EAN%20(FR).ifs';

  for i := 0 to GetArrayLength(articles)-1 do
  begin
    NomScriptTest := copy(Articles[i],0,pos('|',Articles[i])-1);
    if NomScriptTest = NameScript then
    begin
      NomFichier := copy(Articles[i],pos('|',Articles[i])+1,length(Articles[i]));
      Break;
    end;
  end;
  result := NomFichier;
end;

//------------------------------------------------------------------------------
// INDIQUE SI UNE NOUVELLE VERSION DE ScorEpioNCommonScript EST DISPONIBLE
//------------------------------------------------------------------------------

function verifVersionScriptCommun() : Boolean;
var
   LineCommon : String;
   BeginPos : Integer;
begin
  LineCommon := GetPage(FreeUrl + 'ScorEpioNCommonScript.pas');
  BeginPos := pos('Version=', LineCommon);
  delete(LineCommon,1, BeginPos + 9);
  result := nouvelleVersion(VersionCommun, LineCommon);
end;

//------------------------------------------------------------------------------
// INDIQUE SI UNE NOUVELLE VERSION DE SCRIPT EST DISPONIBLE
//------------------------------------------------------------------------------

function nouvelleVersion(versionC, versionN : String) : Boolean;
begin
  if (StrToInt(copy(versionN, 1, pos('du', versionN) - 2),2) > StrToInt(copy(versionC, 1, pos('du', versionC) - 2),2)) then
  begin
    result := True;
  end else
    result := False;
end;

//------------------------------------------------------------------------------
// OUVRE LA PAGE DU FORUM
//------------------------------------------------------------------------------
procedure contactScorEpioN();
begin
  Launch('http://forum.antp.be/phpbb2/viewtopic.php?t=1453', '');
end;

//------------------------------------------------------------------------------
// TÉLÉCHARGE TOUS MES SCRIPTS
//------------------------------------------------------------------------------
procedure dlAllScripts();
begin
  Launch('http://joel.desseaux.free.fr/ScorEpioncommonscript/ScriptsScorEpionDedej.rar', '');
end;

//------------------------------------------------------------------------------
// LISTE TOUS MES SCRIPTS
//------------------------------------------------------------------------------
procedure listeScriptScorEpioN();
var
  Line, ContenuScript : String;
  BeginPos, EndPos : Integer;
begin
  Line := GetPage(FreeUrl);
  repeat
  BeginPos := pos('<IMG SRC="/icons/unknown.gif"', Line);
  delete(Line,1, BeginPos);
  BeginPos := pos('<A HREF="', Line);
  delete(Line,1, BeginPos + 8);
  EndPos := pos('">', Line);
  If pos('.rar',copy(Line, 1, EndPos - 1)) = 0 then
  begin
   ContenuScript := ContenuScript + (copy(Line, 1, EndPos - 1));
   BeginPos := pos('</A>', Line);
   delete(Line,1, BeginPos + 3);
   EndPos := pos(RC, Line);
   ContenuScript := ContenuScript + copy(Line, 1, EndPos);
  end else
   delete(Line,1, pos(RC, Line)+ 2);
  until pos('<IMG SRC="/icons/', Line) = 0;
  ContenuScript := StringReplace(ContenuScript, '%20', ' ');
  //HTMLRemoveTags(ContenuScript);
  //HTMLDecode(ContenuScript);
  showmessage(ContenuScript);
end;

//------------------------------------------------------------------------------
// TELECHARGE LE SCRIPT
//------------------------------------------------------------------------------
procedure downloadScript(NameScript : String);
var
  NomFichier, ContenuScript : String;
begin
  NomFichier := nameOfFile(NameScript);
  Sleep(500);
  ContenuScript := GetPage(FreeUrl+NomFichier);
  script := TStringList.Create;
  script.Add(ContenuScript);
  NomFichier := StringReplace(NomFichier, '%20', ' ');
  script.SaveToFile(dirScripts+NomFichier);
  script.Free;
end;

//------------------------------------------------------------------------------
// AFFICHAGE DU MENU
//------------------------------------------------------------------------------
procedure menuScorEpioN();
begin
  PickTreeAdd(' ', '');
  PickTreeAdd('Verifier si vous avez la dernière version (New version)', 'version');
  PickTreeAdd('Lister tous mes scripts', 'liste');
  PickTreeAdd('Pour me contacter', 'contact');
end;

{ Canevas à mettre dans le script pour menuScorEpioN si on n'utilise pas execMenuScorEpioN

if (Address = 'version') then
        begin
          verifVersion(NomScript,VersionScript);
        end else if (Address = 'contact') then
        begin
          contactScorEpioN();
        end else if (Address = 'liste') then
        begin
          listeScriptScorEpioN();
        end else
}

//------------------------------------------------------------------------------
// FUNCTION POUR EXECUTER ACTION DU MENU ScorEpioN
//------------------------------------------------------------------------------

function execMenuScorEpioN(urlDomain, Address : String) : Boolean;
begin
  if (pos(urlDomain, Address) = 0) then
  begin
    if (Address = 'version') then
       verifVersion(NameScript,CurrentScript)
    else if (Address = 'contact') then
       contactScorEpioN()
    else if (Address = 'liste') then
       listeScriptScorEpioN();
    result := True;
  end else
    result := False;
end;

{ Canevas à mettre dans le script pour menuScorEpioN si on utilise execMenuScorEpioN

if execMenuScorEpioN(urlDomain, Address) = False then
           AnalysePageFilm(Address);
}

//------------------------------------------------------------------------------
// PROCEDURE POUR AVOIR UN FICHIER DE DEBUG
//------------------------------------------------------------------------------
procedure MonDebug(Name, Line : String);
var
    debugFile : TStringList;
begin
	  debugFile := TStringList.Create;
    debugFile.Text := Line;
    debugFile.SaveToFile(dirScripts+'DEBUG_'+Name+'.TXT');
    debugFile.Free;
end;

//------------------------------------------------------------------------------
// RETOURNE LA VALEUR A SI NON VIDE OU ET LA VALEUR B
//------------------------------------------------------------------------------

function valueNotEmpty(ValeurA, ValeurB : String) : string;
begin
   if ValeurA <> '' then
      result := ValeurA
   else
      result := ValeurB;
end;

//------------------------------------------------------------------------------
// SUPPRIME TITRE EN DOUBLE
//------------------------------------------------------------------------------
procedure DeleteTitle(field : Integer);
begin
    if Trim(AnsiUpperCase(GetField(fieldTranslatedTitle))) = Trim(AnsiUpperCase(GetField(fieldOriginalTitle))) then
    begin
      if (CanSetField(field)) then
        SetField(field, '');
    end;
end;

//------------------------------------------------------------------------------
// MET LE TITRE AU BON FORMAT
//------------------------------------------------------------------------------

function formatTitre(titre : String; option : Integer) : string;
begin
  if (option = 0) then
  begin
     titre := AnsiLowerCase(titre);
  end else if (option = 1) then
  begin
     titre := AnsiUpperCase(titre);
  end else if (option = 2) then
  begin
     //titre := AnsiLowerCase(titre);
     titre := AnsiUpFirstLetter(titre);
  end else if (option = 3) then
  begin
     titre := AnsiLowerCase(titre);
     titre := AnsiMixedCase(titre,' -''(.');
  end else if (option = 4) then
  begin
     titre := titre;
  end;
  titre := StringReplace(titre, 'Usa', 'USA');
  result := titre;
end;

//------------------------------------------------------------------------------
// TROUVE LA CHAINE VOULUE
//------------------------------------------------------------------------------

function findInfo(Debut, Fin, Line, Option : String) : string;
var
   infos : String;
   BeginPos, EndPos : Integer;
begin
   infos := '';
   BeginPos := pos(Debut, Line);
   if BeginPos > 0 then
   begin
    delete(Line, 1, BeginPos+length(Debut)-1);
    EndPos := pos(Fin, Line);
    infos := copy(Line,0,EndPos-1);
    if option = '-1' then                              // Pour garder les sauts de ligne
    begin
      infos := StringReplace(infos, '<BR>', RC);
      infos := StringReplace(infos, '<br>', RC);
      infos := StringReplace(infos, '&#8211;', '-');
      HTMLRemoveTags(infos);
      infos := deleteAsianChar(infos);
      HTMLDecode(infos);
    end else
    if option = '0' then                               // Pour supprimer les sauts de ligne
    begin
      HTMLRemoveTags(infos);
      infos := deleteAsianChar(infos);
      HTMLDecode(infos);
      infos := StringReplace(infos, RC, '');
    end else
    if option = '1' then                               // Pour supprimer les sauts de ligne et ne pas enlever les balises
    begin
      infos := deleteAsianChar(infos);
      HTMLDecode(infos);
      infos := StringReplace(infos, RC, '');
    end else
    if option = '2' then                               // Pour supprimer les sauts de ligne
    begin
      HTMLRemoveTags(infos);
      HTMLDecode(infos);
      infos := StringReplace(infos, RC, '');
    end else
    if option = '3' then                              // Pour garder les sauts de ligne et ne pas enlever les balises
    begin
      infos := StringReplace(infos, '<BR>', RC);
      infos := StringReplace(infos, '<br>', RC);
      infos := StringReplace(infos, '&#8211;', '-');
      infos := deleteAsianChar(infos);
      HTMLDecode(infos);
    end else
    if option = '4' then                               // Pour ne rien faire
    begin
      result := infos;
    end;
   end;
   result := Trim(infos);
end;

//------------------------------------------------------------------------------
// COPIER UNE CHAINE JUSQU'A LA DERNIERE OCCURENCE D'UN CARACTERE
//------------------------------------------------------------------------------

function copyUntilLast(Value, Symbol : string) : String;
var
  chaine, temp : String;
begin
  chaine := '';
  repeat
        if pos(Symbol,Value) <> 0 then
           temp := copy(Value,1,pos(Symbol,Value));
      chaine := chaine + temp;
      delete(Value, 1, length(temp));
  until (pos(Symbol,Value) = 0);
  result := chaine;
end;

//------------------------------------------------------------------------------
// SUPPRIME UNE SOUS CHAINE
//------------------------------------------------------------------------------

function supprSubString(Debut, Fin, Line : String) : string;
var
   infos : String;
begin
   infos := findInfo(Debut, Fin, Line, '4');
   Line := StringReplace(Line,Debut+infos+Fin,'');
   result := Line;
end;

//------------------------------------------------------------------------------
// EXISTANCE D'UNE SOUS CHAINE
//------------------------------------------------------------------------------

function subStringExist(SousChaine, Chaine : String) : Boolean;
var
   infos : String;
begin
   if pos(SousChaine, Chaine) <> 0 then
     result := True
   else
      result := False;
end;

//------------------------------------------------------------------------------
// EXISTANCE D'UNE SOUS CHAINE
//------------------------------------------------------------------------------

function RCN(Nombre : Integer) : String;
var
  Value : String;
  Occurence : Integer;
begin
   Value := '';
   if (Nombre > 0) then
   begin
     Occurence := 0;
     repeat
       Value := Value+RC;
       Occurence := Occurence+1;
     until (Occurence = Nombre);
   end;
   result := Value;
end;

//------------------------------------------------------------------------------
// COUPE UNE CHAINE SI UN CRITERE N'EST PAS RESPECTE
//------------------------------------------------------------------------------

function coupeInfo(Debut, Fin, Condition, Line : String) : string;
var
   infos, lacondition : String;
   BeginPos, EndPos, FinalPos : Integer;
begin
   infos := Line;
   if Condition = '' then
    infos := infos+Debut+'FIN2BOUCLE'+Fin
   else
    infos := infos+Debut+''+Fin;
   BeginPos := pos(Debut, infos);
   if BeginPos > 0 then
   begin
    FinalPos := 0;
    repeat
      BeginPos := pos(Debut, infos);
      delete(infos, 1, BeginPos+length(Debut)-1);
      //FinalPos := FinalPos+BeginPos+length(Debut)-1;
      EndPos := pos(Fin, infos);
      lacondition := copy(infos,1,EndPos-1);
      delete(infos, 1, EndPos+length(Fin)-1)
      if lacondition = Condition then
        FinalPos := FinalPos+BeginPos+length(Debut)-1+length(lacondition)+EndPos+length(Fin)-1
      else
        FinalPos := FinalPos+BeginPos+length(Debut)-1;
    until (lacondition <> Condition);
    infos := copy(Line,1,FinalPos-1);
   end;
   HTMLRemoveTags(infos);
   result := Trim(infos);
end;

//------------------------------------------------------------------------------
// SUPPRESSION DES CARACTERES ASIATIQUES
//------------------------------------------------------------------------------

(*function deleteAsianChar(Value : string) : String;
begin
  repeat
      Value:= StringReplace(Value, '&#'+findInfo('&#', ';', Value,'0')+';', '');
  until (pos('&#',Value) = 0);
  result := Value;
end;*)


function deleteAsianChar(Value : string) : String;
var
  taille :Integer;
begin
  repeat
    if (copy(Value, pos('&#',Value)+7, 1) = ';') then
      taille := 8
    else
      taille := 7;
    delete (Value, pos('&#',Value), taille);
  until (pos('&#',Value) = 0);
  result := Value;
end;

//------------------------------------------------------------------------------
// SUPPRESSION DES TABLULATIONS
//------------------------------------------------------------------------------

function deleteTab(Value : string) : String;
begin
  repeat
      Value:= StringReplace(Value, '	', '');
  until (pos('	',Value) = 0);
  result := Value;
end;

//------------------------------------------------------------------------------
// SUPPRESSION DES RETOURS A LA LIGNE MULTIPLES
//------------------------------------------------------------------------------

function deleteMultiReturn(Value : string) : String;
begin
  repeat
      Value:= StringReplace(Value, RC+' ', RC);
  until (pos(RC+' ',Value) = 0);
  repeat
      Value:= StringReplace(Value, RCN(2), RC);
  until (pos(RCN(2),Value) = 0);
  result := Value;
end;

//------------------------------------------------------------------------------
// SUPPRESSION DES ESPACES MULTIPLES
//------------------------------------------------------------------------------

function deleteMultiSpace(Value : string) : String;
begin
  repeat
      Value:= StringReplace(Value, '  ', ' ');
  until (pos('  ',Value) = 0);
  result := Value;
end;

//------------------------------------------------------------------------------
// SUPPRIME UNE SOUS CHAINE D'UNE CHAINE SI TERMINE LA CHAINE
//------------------------------------------------------------------------------

function deleteEnd(Value, EndValue : String) : String;
begin
  if copy(Value,length(Value)-length(EndValue)+1,length(Value)) = EndValue then
        Value := copy(Value,1,length(Value)-length(EndValue));
  result := Value;
end;

//------------------------------------------------------------------------------
// GESTION DES LISTES AVEC VIRGULES
//------------------------------------------------------------------------------

function gestionSeparateur(Value, Separateur : String) : String;
begin
  // Remplacement des "," par ">>>||<<<"
  repeat
      Value:= StringReplace(Value, Separateur,'>>>||<<<');
  until (pos(Separateur,Value) = 0);
  // Remplacement des ">>>||<<<" par ", "
  repeat
      Value:= StringReplace(Value, '>>>||<<<',Separateur+' ');
  until (pos('>>>||<<<',Value) = 0);
  Value := deleteMultiSpace(Value);
  // Remplacement des " ," par ", "
  repeat
      Value:= StringReplace(Value, ' '+Separateur,Separateur+' ');
  until (pos(' '+Separateur,Value) = 0);
  Value := deleteMultiSpace(Value);
  result := trim(Value);
end;

//------------------------------------------------------------------------------
// STOCKE LA VALEUR DANS LE CHAMP SPÉCIFIÉ SI LA VALEUR EST NON NULLE
//------------------------------------------------------------------------------

procedure MonSetField(field: Integer; value: string);
begin
if value <> '' then
  SetField(field,supprQuote(value));
end;

//------------------------------------------------------------------------------
// STOCKE L'IMAGE SI LA VALEUR EST NON NULLE
//------------------------------------------------------------------------------

procedure MonGetPicture(value: string);
begin
if value <> '' then
  GetPicture(value);
end;

//------------------------------------------------------------------------------
// RETOURNE UNE VALEUR COMPATIBLE AVEC DES CALCULS DE FLOAT
//------------------------------------------------------------------------------

function FormatFloat(value: String) : String;
begin
value := StringReplace(value, ',', '.');
if pos('.',value) = 0 then
  value := value+'.0';
result := value;
end;

//------------------------------------------------------------------------------
// VERIFIE QUE LE CHAMPS EST BIEN UNE DATE
//------------------------------------------------------------------------------

function isDate(value: String) : String;
var
  test : String;
begin
  test := copy(value, 1, 2);
  if (test = '19') or (test = '20') then
    result := value
  else
    result := '';
end;

//------------------------------------------------------------------------------
// REMPLACE " par ''
//------------------------------------------------------------------------------

function supprQuote(title : String) : String;
begin
  result := StringReplace(title, '"', '''''');
end;

//------------------------------------------------------------------------------
// NETTOIE LE TITRE DU FICHIER POUR AVOIR LE TITRE DE FILM
//------------------------------------------------------------------------------

function supprExt(title, ext : String) : String; // Supprime l'extension et ce qui suit
var
  i : Integer;
begin
  i := pos(ext,title);
  if i <> 0 then
    title := copy(title,1,i-1);
  result := title;
end;

function supprChemin(title : String) : String; // Supprime les chemins fichiers (C:\...)
var
  i : Integer;
begin
  i := pos('\',title);
  if i = 3 then
  begin
    while i <> 0 do
    begin
      title := copy(title,i+1,length(title)-1);
      i := pos('\',title);
    end;
  end;
  result := title;
end;

function cleanTitle(title : String) : string;
var
  i,j, fin : Integer;
  temp : String;

begin
  title := AnsiUpperCase(title);

  if title <> '' then
  begin
  title := supprChemin(title);
// Nettoie les tags fichiers, merci Atmosfear pour les tags
  title := supprExt(title, '.DVD');
  title := supprExt(title, '.DIVX');
  title := supprExt(title, '.FREN');
  title := supprExt(title, '.GERM');
  title := supprExt(title, '.INT');
  title := supprExt(title, '.LIM');
  title := supprExt(title, '.PROP');
  title := supprExt(title, '.REPACK');
  title := supprExt(title, '.PROP');
  title := supprExt(title, '.SUBB');
  title := supprExt(title, '.UNSUB');
  title := supprExt(title, '.WS');
  title := supprExt(title, '.XVID');
  title := supprExt(title, '.AC3');
  title := supprExt(title, '.UNRAT');
  title := supprExt(title, '.AVI');
  title := supprExt(title, '.OGM');
  title := supprExt(title, '.MKV');
  title := supprExt(title, '.WMV');
  title := supprExt(title, '.MPG');
  title := supprExt(title, '.MPEG');
  title := StringReplace(title, '. ', '><');
  title := StringReplace(title, '.', ' ');
  title := StringReplace(title, ',', ' ');
  title := StringReplace(title, '_', ' ');
  title := StringReplace(title, ':', '');
  title := StringReplace(title, ' - ', ' ');
  title := StringReplace(title, '-', ' ');
  title := StringReplace(title, '  ', ' ');
  title := StringReplace(title, '><', '. ');

   i := 0;
// Nettoie les tags de team
   if (pos('(',title) <> 0) then
   begin
     i := pos('(',title);
     temp := copy(title,0,i-1);
     j := pos(')',title);
     fin := Length(title);
     title := temp + copy(title,j+1,fin);
   end;

   if (pos('[',title) <> 0) then
   begin
     i := pos('[',title);
     temp := copy(title,1,i-1);
     j := pos(']',title);
     fin := Length(title);
     title := temp + copy(title,j+1,fin);
   end;
  end;
  title := Trim(title);
  title := AnsiLowerCase(title);
  title := formatTitre(title ,GetOption('Format du Titre'));
  if (GetOption('Format du Titre') = -1) then
    title := formatTitre(title ,GetOption('Casse Choisie'));
  result := title;
end;

//------------------------------------------------------------------------------
// TRANSFORME NATIONALITE EN PAYS [ Changelog 22/02/2013 Ajout de nationalités au féminin ]
//------------------------------------------------------------------------------

function transformCountry(country : String) : string;
begin
  country := AnsiLowerCase(country);
  // Liste des nationalités trouvée sur moviecovers
  country := StringReplace(country, 'afghan', 'Afghanistan');
  country := StringReplace(country, 'albanais', 'Albanie');
  country := StringReplace(country, 'algérienne', 'Algérie'); // Ajout du 22/02/2013
  country := StringReplace(country, 'algérien', 'Algérie');
  country := StringReplace(country, 'allemande', 'Allemagne');
  country := StringReplace(country, 'allemand', 'Allemagne');
  country := StringReplace(country, 'américaine', 'USA');
  country := StringReplace(country, 'américain', 'USA');
  country := StringReplace(country, 'argentin', 'Argentine');
  country := StringReplace(country, 'arménien', 'Arménie');
  country := StringReplace(country, 'australienne', 'Australie');
  country := StringReplace(country, 'australien', 'Australie');
  country := StringReplace(country, 'autrichienne', 'Autriche'); // Ajout du 22/02/2013
  country := StringReplace(country, 'autrichien', 'Autriche');
  country := StringReplace(country, 'bangladais', 'Bangladesh');
  country := StringReplace(country, 'belge', 'Belgique');
  country := StringReplace(country, 'beninois', 'Benin');
  country := StringReplace(country, 'bosniaque', 'Bosnie');
  country := StringReplace(country, 'botswanais', 'Botswana');
  country := StringReplace(country, 'bouthanais', 'Bouthan');
  country := StringReplace(country, 'brésilienne', 'Brésil'); // Ajout du 22/02/2013
  country := StringReplace(country, 'brésilien', 'Brésil');
  country := StringReplace(country, 'britannique', 'Grande-Bretagne');
  country := StringReplace(country, 'bulgare', 'Bulgarie');
  country := StringReplace(country, 'burkinabè', 'Burkina Faso');
  country := StringReplace(country, 'cambodgien', 'Cambodge');
  country := StringReplace(country, 'camerounais', 'Cameroun');
  country := StringReplace(country, 'canadienne', 'Canada'); // Ajout du 22/02/2013
  country := StringReplace(country, 'canadien', 'Canada');
  country := StringReplace(country, 'chilien', 'Chili');
  country := StringReplace(country, 'chinoise', 'Chine');
  country := StringReplace(country, 'chinois', 'Chine');
  country := StringReplace(country, 'colombienne', 'Colombie'); // Ajout du 22/02/2013
  country := StringReplace(country, 'colombien', 'Colombie');
  country := StringReplace(country, 'congolais', 'Congo');
  country := StringReplace(country, 'cubain', 'Cuba');
  country := StringReplace(country, 'danoise', 'Danemark'); // Ajout du 22/02/2013
  country := StringReplace(country, 'danois', 'Danemark');
  country := StringReplace(country, 'ecossais', 'Ecosse');
  country := StringReplace(country, 'egyptien', 'Egypte');
  country := StringReplace(country, 'espagnole', 'Espagne');
  country := StringReplace(country, 'espagnol', 'Espagne');
  country := StringReplace(country, 'estonienne', 'Estonie'); // Ajout du 22/02/2013
  country := StringReplace(country, 'estonien', 'Estonie');
  country := StringReplace(country, 'européen', 'UE');
  country := StringReplace(country, 'finlandaise', 'Finlande'); // Ajout du 22/02/2013
  country := StringReplace(country, 'finlandais', 'Finlande');
  country := StringReplace(country, 'française', 'France');
  country := StringReplace(country, 'français', 'France');
  country := StringReplace(country, 'gabonais', 'Gabon');
  country := StringReplace(country, 'georgien', 'Géorgie');
  country := StringReplace(country, 'grec', 'Grèce');
  country := StringReplace(country, 'guinéen', 'Guinée');
  country := StringReplace(country, 'haïtien', 'Haïti');
  country := StringReplace(country, 'hollandais', 'Pays-Bas');
  country := StringReplace(country, 'néerlandais', 'Pays-Bas');
  country := StringReplace(country, 'hong-kongais', 'Hong-Kong');
  country := StringReplace(country, 'hongroise', 'Hongrie'); // Ajout du 22/02/2013
  country := StringReplace(country, 'hongrois', 'Hongrie');
  country := StringReplace(country, 'indien', 'Inde');
  country := StringReplace(country, 'indonésien', 'Indonésie');
  country := StringReplace(country, 'irakien', 'Irak');
  country := StringReplace(country, 'iranien', 'Iran');
  country := StringReplace(country, 'irlandais', 'Irlande');
  country := StringReplace(country, 'islandaise', 'Islande'); // Ajout du 22/02/2013
  country := StringReplace(country, 'islandais', 'Islande');
  country := StringReplace(country, 'israélienne', 'Israël'); // Ajout du 22/02/2013
  country := StringReplace(country, 'israélien', 'Israël');
  country := StringReplace(country, 'italienne', 'Italie'); // Ajout du 22/02/2013
  country := StringReplace(country, 'italien', 'Italie');
  country := StringReplace(country, 'ivoirien', 'Côte d''Ivoire');
  country := StringReplace(country, 'jamaïcain', 'Jamaïque');
  country := StringReplace(country, 'japonaise', 'Japon');
  country := StringReplace(country, 'japonais', 'Japon');
  country := StringReplace(country, 'jordanienne', 'Jordanie'); // Ajout du 22/02/2013
  country := StringReplace(country, 'kazakh', 'Kazakhstan');
  country := StringReplace(country, 'kirghiz', 'Kirghizistan');
  country := StringReplace(country, 'kurde', 'Kurdistan');
  country := StringReplace(country, 'lettonien', 'Lettonie');
  country := StringReplace(country, 'libanais', 'Liban');
  country := StringReplace(country, 'liechtensteinois', 'Liechtenstein');
  country := StringReplace(country, 'lituanien', 'Lituanie');
  country := StringReplace(country, 'luxembourgeoise', 'Luxembourg'); // Ajout du 22/02/2013
  country := StringReplace(country, 'luxembourgeois', 'Luxembourg');
  country := StringReplace(country, 'macédonien', 'Macédoine');
  country := StringReplace(country, 'malaisien', 'Malaisie');
  country := StringReplace(country, 'malienne', 'Mali'); // Ajout du 22/02/2013
  country := StringReplace(country, 'malien', 'Mali');
  country := StringReplace(country, 'maltais', 'Malte');
  country := StringReplace(country, 'marocain', 'Maroc');
  country := StringReplace(country, 'mauritanien', 'Mauritanie');
  country := StringReplace(country, 'mexicaine', 'Mexique'); // Ajout du 22/02/2013
  country := StringReplace(country, 'mexicain', 'Mexique');
  country := StringReplace(country, 'néo-zélandaise', 'Nouvelle-Zélande'); // Ajout du 22/02/2013
  country := StringReplace(country, 'néo-zélandais', 'Nouvelle-Zélande');
  country := StringReplace(country, 'nigérien', 'Nigéria');
  country := StringReplace(country, 'nord-coréen', 'Corée du Nord');
  country := StringReplace(country, 'norvégienne', 'Norvége'); // Ajout du 22/02/2013
  country := StringReplace(country, 'norvégien', 'Norvége');
  country := StringReplace(country, 'pakistanais', 'Pakistan');
  country := StringReplace(country, 'palestinien', 'Palestine');
  country := StringReplace(country, 'péruvienne', 'Pérou'); // Ajout du 22/02/2013
  country := StringReplace(country, 'péruvien', 'Pérou');
  country := StringReplace(country, 'philippin', 'Philippine'); // correction du 22/02/2013 [country := StringReplace(country, 'philippiens', 'Philippine');]
  country := StringReplace(country, 'polonaise', 'Pologne'); // Ajout du 22/02/2013
  country := StringReplace(country, 'polonais', 'Pologne');
  country := StringReplace(country, 'portugaise', 'Portugal'); // Ajout du 22/02/2013
  country := StringReplace(country, 'portugais', 'Portugal');
  country := StringReplace(country, 'roumain', 'Roumanie');
  country := StringReplace(country, 'russe', 'Russie');
  country := StringReplace(country, 'sénégalais', 'Sénégal');
  country := StringReplace(country, 'serbe', 'Serbie');
  country := StringReplace(country, 'serbo-croate', 'Serbie, Croatie');
  country := StringReplace(country, 'singapourienne', 'Singapour'); // Ajout du 22/02/2013
  country := StringReplace(country, 'singapourien', 'Singapour');
  country := StringReplace(country, 'slovaque', 'Slovaquie');
  country := StringReplace(country, 'soviétique', 'URSS');
  country := StringReplace(country, 'sri-lankais', 'Sri-Lanka');
  country := StringReplace(country, 'sud-africain', 'Afrique du Sud');
  country := StringReplace(country, 'sud-coréenne', 'Corée du Sud');
  country := StringReplace(country, 'sud-coréen', 'Corée du Sud');
  country := StringReplace(country, 'suédoise', 'Suède'); // Ajout du 22/02/2013
  country := StringReplace(country, 'suédois', 'Suède');
  country := StringReplace(country, 'suisse', 'Suisse');
  country := StringReplace(country, 'tadjik', 'Tadjikistan');
  country := StringReplace(country, 'taïwanais', 'Taïwan');
  country := StringReplace(country, 'tchadien', 'Tchad');
  country := StringReplace(country, 'tchèque', 'République Tchèque');
  country := StringReplace(country, 'thaïlandais', 'Thaïlande');
  country := StringReplace(country, 'tunisien', 'Tunisie');
  country := StringReplace(country, 'turque', 'Turquie'); // Ajout du 22/02/2013
  country := StringReplace(country, 'turc', 'Turquie');
  country := StringReplace(country, 'usa', 'USA');
  country := StringReplace(country, 'ukranienne', 'Ukraine'); // Ajout du 22/02/2013
  country := StringReplace(country, 'ukranien', 'Ukraine');
  country := StringReplace(country, 'uruguayen', 'Uruguay');
  country := StringReplace(country, 'vénézuélienne', 'Vénézuéla'); // Ajout du 22/02/2013
  country := StringReplace(country, 'vénézuélien', 'Vénézuéla');
  country := StringReplace(country, 'vietnamien', 'Vietnam');
  country := StringReplace(country, 'yougoslave', 'Yougoslavie');
  country := StringReplace(country, 'république République', 'République');
  country := formatTitre(country ,GetOption('Format du Titre'));
  if (GetOption('Format du Titre') = -1) then
    country := formatTitre(country ,GetOption('Casse Choisie'));
  result := country;
end;

//------------------------------------------------------------------------------
// SUPPRIME LES ACCENTS
//------------------------------------------------------------------------------

function supprimeLesAccents(NomFilm : String) : string;
begin
     NomFilm := AnsiLowerCase(NomFilm);
     NomFilm := StringReplace(NomFilm, 'à', 'a');
  	 NomFilm := StringReplace(NomFilm, 'á', 'a');
  	 NomFilm := StringReplace(NomFilm, 'â', 'a');
  	 NomFilm := StringReplace(NomFilm, 'ã', 'a');
  	 NomFilm := StringReplace(NomFilm, 'ä', 'a');
  	 NomFilm := StringReplace(NomFilm, 'ã', 'a');
  	 NomFilm := StringReplace(NomFilm, 'é', 'e');
  	 NomFilm := StringReplace(NomFilm, 'è', 'e');
  	 NomFilm := StringReplace(NomFilm, 'ë', 'e');
  	 NomFilm := StringReplace(NomFilm, 'ê', 'e');
  	 NomFilm := StringReplace(NomFilm, 'ï', 'i');
  	 NomFilm := StringReplace(NomFilm, 'î', 'i');
  	 NomFilm := StringReplace(NomFilm, 'ì', 'i');
  	 NomFilm := StringReplace(NomFilm, 'í', 'i');
  	 NomFilm := StringReplace(NomFilm, 'ô', 'o');
  	 NomFilm := StringReplace(NomFilm, 'ö', 'o');
  	 NomFilm := StringReplace(NomFilm, 'õ', 'o');
  	 NomFilm := StringReplace(NomFilm, 'ò', 'o');
  	 NomFilm := StringReplace(NomFilm, 'ó', 'o');
  	 NomFilm := StringReplace(NomFilm, 'ü', 'u');
  	 NomFilm := StringReplace(NomFilm, 'û', 'u');
  	 NomFilm := StringReplace(NomFilm, 'ú', 'u');
  	 NomFilm := StringReplace(NomFilm, 'ç', 'c');
  	 NomFilm := StringReplace(NomFilm, 'ñ', 'n');
     NomFilm := StringReplace(NomFilm, 'ô', 'o');
     result := NomFilm;
end;

//------------------------------------------------------------------------------
// INITIALISE LA LISTE DES ARTICLES
//------------------------------------------------------------------------------

function initLesArticles() : array of string;
var
  Articles: array of string;
  i: integer;
begin
  SetArrayLength(Articles,32);
  Articles[0]:='le ';
  Articles[1]:='la ';
  Articles[2]:='l''';
  Articles[3]:='l ';
  Articles[4]:='les ';
  Articles[5]:='des ';
  Articles[6]:='un ';
  Articles[7]:='une ';
  Articles[8]:='the ';
  Articles[9]:='a ';
  Articles[10]:='an ';
  Articles[11]:='der ';
  Articles[12]:='das ';
  Articles[13]:='die ';
  Articles[14]:='dem ';
  Articles[15]:='den ';
  Articles[16]:='ein ';
  Articles[17]:='eine ';
  Articles[18]:='einen ';
  Articles[19]:='einer ';
  Articles[20]:='eines ';
  Articles[21]:='einem ';
  Articles[22]:='uno ';
  Articles[23]:='una ';
  Articles[24]:='el ';
  Articles[25]:='los ';
  Articles[26]:='las ';
  Articles[27]:='unos ';
  Articles[28]:='unas ';
  Articles[29]:='il ';
  Articles[30]:='lo ';
  Articles[31]:='i ';
  result := Articles;
end;

//------------------------------------------------------------------------------
// SUPPRIME LES ARTICLES
//------------------------------------------------------------------------------

function supprimeLesArticles(NomFilm : String) : string;
var
  Articles: array of string;
  i: integer;
begin
  Articles := initLesArticles();
  NomFilm := AnsiLowerCase(NomFilm);
// supprimer les articles
  for i := 0 to GetArrayLength(Articles)-1 do
  begin
    if Pos(Articles[i], NomFilm) = 1 then
    begin
      NomFilm := Copy(NomFilm, Length(Articles[i])+1, length(NomFilm));
      Break;
    end;
  end;
  result := NomFilm;
end;

//------------------------------------------------------------------------------
// REMET LES ARTICLES EN DEBUT DE TITRE
//------------------------------------------------------------------------------

function articleDebutTitre(title,Debut,Fin : String) : String;
var
  Articles: array of string;
  i: integer;
begin
  title := AnsiLowerCase(title);
  Articles := initLesArticles();
  for i := 0 to GetArrayLength(Articles)-1 do
  begin
    if Pos(Debut+trim(Articles[i])+Fin, title) <> 0 then
    begin
      title := Articles[i]+title;
      title := StringReplace(title, Debut+trim(Articles[i])+Fin, '');
      Break;
    end;
  end;
  result := title;
end;

//------------------------------------------------------------------------------
// CONVERTI LES CHIFFRES ROMAINS EN NOMBRE ET INVERSEMENT
//------------------------------------------------------------------------------

function conversionNR(NomFilm, Sens : String) : string; // Sens = RvN ou NvR
var
  nombresR, nombresN : array of string;
  i: integer;
begin
  NomFilm := AnsiLowerCase(NomFilm);
  NomFilm := NomFilm+' ';
  SetArrayLength(nombresR,5);
  nombresR[0]:=' i ';
  nombresR[1]:=' ii ';
  nombresR[2]:=' iii ';
  nombresR[3]:=' iv ';
  nombresR[4]:=' v ';
  SetArrayLength(nombresN,5);
  nombresN[0]:=' 1 ';
  nombresN[1]:=' 2 ';
  nombresN[2]:=' 3 ';
  nombresN[3]:=' 4 ';
  nombresN[4]:=' 5 ';

  if (Sens = 'RvN') then
  begin
    for i := 0 to GetArrayLength(nombresR)-1 do
    begin
      if Pos(nombresR[i], NomFilm) <> 0 then
      begin
        NomFilm := StringReplace(NomFilm, nombresR[i], nombresN[i]);
        Break;
      end;
    end;
  end else
  if (Sens = 'NvR') then
  begin
    for i := 0 to GetArrayLength(nombresN)-1 do
    begin
      if Pos(nombresN[i], NomFilm) <> 0 then
      begin
        NomFilm := StringReplace(NomFilm, nombresN[i], nombresR[i]);
        Break;
      end;
    end;
  end;
  NomFilm := Trim(NomFilm);
  NomFilm := formatTitre(NomFilm ,GetOption('Format du Titre'));
  if (GetOption('Format du Titre') = -1) then
    NomFilm := formatTitre(NomFilm ,GetOption('Casse Choisie'));
  result := NomFilm;
end;

//------------------------------------------------------------------------------
// RECHERCHE SUR LE TITRE ORIGINAL OU TRADUIT
//------------------------------------------------------------------------------

function recupTitreRecherche(option : Integer) : string;
var
  NomFilm : String;
begin
if (option = 0) then
    begin
      NomFilm := GetField(fieldTranslatedTitle);
      if NomFilm = '' then
        NomFilm := GetField(fieldOriginalTitle);
    end else
    if (option = 1) then
    begin
      NomFilm := GetField(fieldOriginalTitle);
      if NomFilm = '' then
        NomFilm := GetField(fieldTranslatedTitle);
    end;
    NomFilm := cleanTitle(NomFilm);
    result := NomFilm;
end;

//------------------------------------------------------------------------------
// AFFICHAGE DES TITRES SI ORIGINAL ET TRADUIT IDENTIQUE
//------------------------------------------------------------------------------

procedure titreDouble(option : Integer);
begin
  if (option = 0) then
  begin
    exit;
  end else if (option = 1) then
  begin
    if CanSetField(fieldTranslatedTitle) then
      DeleteTitle(fieldTranslatedTitle);
  end else if (option = 2) then
  begin
    if CanSetField(fieldOriginalTitle) then
      DeleteTitle(fieldOriginalTitle);
  end;
end;

//------------------------------------------------------------------------------
// FONCTION METS LE CHAMPS COMMENTAIRE A LA SUITE DU CHAMPS DESCRIPTION
//------------------------------------------------------------------------------
procedure moveComments();
begin
   SetField(fieldDescription,GetField(fieldDescription)+RCN(2)+GetField(fieldComments));
   SetField(fieldComments,'');
end;

//------------------------------------------------------------------------------
// PROCEDURE D'ECRITURE D'UN FICHIER DE LOG
//------------------------------------------------------------------------------

procedure batch(Nom : String); // DIALOGUE POUR DETERMINER LA DESTINATION (à utiliser au lancement du script)
begin
  execlog := True;
  adresselog := GetStatic('path');
  if adresselog = '' then
  begin
    SetStatic('path',dirScripts);
    adresselog := GetStatic('path');
  end;
  fichierlog := 'BATCH-'+Nom+'.TXT';
  if (ShowConfirmation('Le fichier de log se nomme : '+fichierlog+'.'+RC+'Le répertoire de destination est : '+RCN(2)+adresselog+RCN(2)+'Cliquer sur ''''OUI'''' pour valider.'+RC+'Cliquer sur ''''NON'''' pour donner un nouveau chemin.') = True) then
  begin
    createLog();
  end else
  begin
    if Input('Choix du path pour le fichier de log', 'Entrez le répertoire de destination :', adresselog) then
    begin
      if (copy(adresselog,length(adresselog),length(adresselog)) <> '\') then
        adresselog := adresselog+'\';
      SetStatic('path',adresselog);
      adresselog := GetStatic('path');
      createLog();
    end else
    begin
      execlog := False;
      exit;
    end;
  end;
end;

procedure createLog(); // CREATION DU FICHIER DE LOG
begin
	log := TStringList.Create;
    log.Text := '*****************************************************************************';
    log.SaveToFile(adresselog+fichierlog);
    compteurMAJ := 0;
end;

procedure AddToLog(m: string); // AJOUTE UNE LIGNE AU FICHIER DE LOG
begin
	if execlog then
	begin
      log.Add(m);
	  log.SaveToFile(adresselog+fichierlog);
	end;
end;

procedure beforeUpdate(); // AJOUTE LES INFOS AVANT MISE A JOUR (à utiliser avant de récupérer des infos)
var
    chaine, lecompteurMAJ : String;
begin
	if execlog then
	begin
      chaine := '';
      compteurMAJ := compteurMAJ+1;
      if compteurMAJ < 10 then
        lecompteurMAJ := '00'+IntToStr(compteurMAJ)
      else if compteurMAJ < 100 then
        lecompteurMAJ := '0'+IntToStr(compteurMAJ)
      else lecompteurMAJ := IntToStr(compteurMAJ);

      AddToLog('************************** Traitement n° '+lecompteurMAJ+' ********************************');
      AddToLog('Informations avant mise à jour du film n°'+GetField(fieldNumber)+'.');
      chaine := 'Titre traduit : '+GetField(fieldTranslatedTitle)+' - Titre original : '+GetField(fieldOriginalTitle);
      AddToLog(chaine);
      chaine := 'Réalisateur : '+GetField(fieldDirector)+' - Année : '+GetField(fieldYear);
      AddToLog(chaine);
      chaine := 'Adresse Web : '+GetField(fieldURL);
      AddToLog(chaine);
      chaine := '';
    end;
end;

procedure afterUpdate(); // AJOUTE LES INFOS APRES MISE A JOUR (à utiliser après la récupération des infos)
var
    chaine : String;
begin
	if execlog then
	begin
      chaine := '';
      AddToLog('Informations après mise à jour');
      chaine := 'Titre traduit : '+GetField(fieldTranslatedTitle)+' - Titre original : '+GetField(fieldOriginalTitle);
      AddToLog(chaine);
      chaine := 'Réalisateur : '+GetField(fieldDirector)+' - Année : '+GetField(fieldYear);
      AddToLog(chaine);
      chaine := 'Adresse Web  : '+GetField(fieldURL);
      AddToLog(chaine);
      AddToLog('*****************************************************************************');
      SetField(fieldChecked, 'x');
      chaine := '';
  end;
end;

function CustomField(Nom : String): string; // Test si champs modifiables
var
   SelectedField : String;
   Tag, i : Integer;
   begin
    Tag := GetCustomFieldCount;
    PickTreeClear;                                                    // init list
    PickTreeAdd('Champ(s) modifiable(s) pour ''' + Nom + '''','');
    PickTreeAdd('Champs d''origine','Champs d''origine');
    for i := 0 to Tag -1 do
     begin
     SelectedField := GetCustomFieldTag(i);
     PickTreeAdd(SelectedField,SelectedField);
     end;
    PickTreeExec(SelectedField);
    if SelectedField <> '' then
     result := SelectedField;
   end;
end.