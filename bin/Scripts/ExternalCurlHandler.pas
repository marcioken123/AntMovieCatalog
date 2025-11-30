// 2025/07/21 - Initial version: MrObama2022
// 2025/10/12 - Code simplification and spanish comments: Garada

unit ExternalCurlHandler;

uses 
  StringUtils7552;

const
  // Values that alter the operation of the script. 
  // Valores que cambian el comportamiento del script
  
  // Use Visual Basic Script to hide curl windows, disable if you do not want to or cannot use VBS (p.e. Linux)
  // Usar Visual Basic para ocultar la ventana de cURL, desabilitar si no quiere o no puede usar VBS (p. ej. Linux)
  UseVBS = True; 
  // (optional) if you want get your scripts directory clean, set here your tmp working dir, example C:\Users\YOURWINDOWSUSER\AppData\Local\Temp\
  // (Opcional) Si quiere mantener su carpeta de scripts limpia, especifique una carpeta temporal de trabajo. P. ej. C:\Users\YOURWINDOWSUSER\AppData\Local\Temp\
  tmpDir = ''; 
  //Time to wait (ms) before make a call to URL, p.e. 2001
  // Tiempo en ms a esperar antes de hacer na llamada a la URL, p. ej. 2001
  delayBetweenRequest = 0; 
  // if you use Windows 7 or 8 download curl.exe for Windows (it's free https://curl.se/) and set here the right path
  // Si usa Windows 7 u 8 debe descargar cURL,exe (gratuito: https://curl.se/) y especificar la ruta correcta
  curlPath = 'curl.exe'; 
  // Max time to wait for response in ms
  // Tiempo máximo de espera por la respuesta en ms
  TimeOut = 10000; 
  
  vbsScript = 'ExternalCurlHandler.vbs';
  curlOutput = 'curlOutput.html';
  curlUserAgent = 'Mozilla/5.0 (compatible; Ant Movie Catalog)';
  //curlUserAgent = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36 AntMovieCatalog/4';

var
  InstallerPath: string;
  
function GetPage5Advanced(address: string; referer: string; cookies: string; content: string; headers: string): string;
var
  cnt: integer;
  fileContent: TStringList;
  curlOutputResult: string;
  sCommand: string;
begin
  Result := '';
  
  // Create VBS file if not exists
  if setupScript then
  begin
    // Delete temporal files
    if fileExists(InstallerPath + curlOutput) then
      DeleteFile(InstallerPath + curlOutput);  
    curlOutputResult := curlOutput + '.result';
    if fileExists(InstallerPath + curlOutputResult) then
      DeleteFile(InstallerPath + curlOutputResult);  
    
    // CURL parameters:
    // Create info file with return codes and possible errors, it's created after page is downloaded
    sCommand := '-w "%output{' + InstallerPath + curlOutputResult + '}%{url}\nExitCode: %{exitcode}\nErrorMsg: %{errormsg}\nResponseCode: %{http_code}"';
    // Download page and save to file
    sCommand := sCommand + ' -L --output "' + InstallerPath + curlOutput + '" --url "' + address + '" ' + '-H "Accept: text/html, */*" -H "Accept-Language: it" -H "DNT: 1" -H "Priority: u=0, i" -H "Sec-Ch-Ua: \"Not)A;Brand\";v=\"8\", \"Chromium\";v=\"138\", \"Google Chrome\";v=\"138\"" -H "Sec-Ch-Ua-Mobile: ?0" -H "Sec-Ch-Ua-Platform: \"Windows\"" -H "Sec-Fetch-Dest: Document" -H "Sec-Fetch-Mode: Navigate" -H "Sec-Fetch-Site: None" -H "Sec-Fetch-User: ?1" -H "Upgrade-Insecure-Requests: 1" -H "User-Agent: ' + curlUserAgent + '"';
    //test slow connection, don't uncomment
    //sCommand := '--limit-rate 1 ' + sCommand;
    //test proxy connection, don't uncomment
    //sCommand := '-x "socks5://127.0.0.1:9150/" ' + sCommand;

    Sleep(delayBetweenRequest);
    
    if UseVBS then
    begin
      // Launch CURL. Change " for #1 as VBS uses " for parameters. The VBS script undone the changes.
      Launch('wscript.exe', '"' +InstallerPath + vbsScript + '" "' + curlPath + '" ' + StringReplace(sCommand, '"', #1)); 
    end
    else
      Launch(curlPath, sCommand); 
    
    // Wait for end info file or timeout
    cnt := 0;
    while (not FileExists(InstallerPath + curlOutputResult)) and (cnt < TimeOut div 50) do
    begin
      cnt := cnt + 1; 
      Sleep(50);
    end;  
    
    // if info file exists
    if (fileExists(InstallerPath + curlOutputResult)) then
    begin
      fileContent := TStringList.Create;
      try
        // Read and delete info file 
        fileContent.LoadFromFile(InstallerPath + curlOutputResult); 
        DeleteFile(InstallerPath + curlOutputResult);
        
        // if return error
        if TextBetween(fileContent.Text, 'ErrorMsg: ', #13) <> '' then
          ShowError('Error downloading page.' + #13 + fileContent.Text)
        else if (fileExists(InstallerPath + curlOutput)) then // if downloaded page exits
        begin
          // Read and delete downloaded page
          fileContent.LoadFromFile(InstallerPath + curlOutput);
          DeleteFile(InstallerPath + curlOutput);
          // Return page
          Result := fileContent.Text;
          
          //if Pos('</html>', Result) < 1 then
          //  ShowError('TRIM!!!');
        end
        else // no error, no download, no timeout...
          ShowError('The page did not download');
      finally
        fileContent.Free;
      end;
    end
    else // if not: timeout
      ShowError('Internal Timeout!!');
  end
  else
  begin
    Sleep(delayBetweenRequest);
    Result := GetPage5(address, referer, cookies, content, headers);
  end;  
end;


function setupScript: boolean;
var 
  ScriptContent: TStringList;
begin
  Result := False;
  
  // initialize working path
  if (tmpDir <> '') then
    InstallerPath := tmpDir
  else  
    InstallerPath := dirScripts;
  InstallerPath := IncludeTrailingPathDelimiter(InstallerPath);
  
  // Create a generic VBS script that
  // open a command with parameters in hidden window
  if UseVBS then
  if (not FileExists(InstallerPath + vbsScript)) then
  begin
    ScriptContent := TStringList.Create;
    ScriptContent.Add('Dim Args()');
    ScriptContent.Add('ReDim Args(WScript.Arguments.Count - 1)');
    ScriptContent.Add('Args(0) = """" & WScript.Arguments(0) & """"');
    ScriptContent.Add('For i = 1 To WScript.Arguments.Count - 1');
    ScriptContent.Add('   Args(i) = Replace(WScript.Arguments(i), chr(1), chr(34))');
    ScriptContent.Add('Next');
    ScriptContent.Add('CreateObject("Wscript.Shell").Run Join(Args), 0, False');
    ScriptContent.SaveToFile(InstallerPath + vbsScript); 
    ScriptContent.Free;   
  end;

  Result := true;  
end;  

begin
end.