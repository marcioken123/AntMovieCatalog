Dim Args()
ReDim Args(WScript.Arguments.Count - 1)
Args(0) = """" & WScript.Arguments(0) & """"
For i = 1 To WScript.Arguments.Count - 1
   Args(i) = Replace(WScript.Arguments(i), chr(1), chr(34))
Next
CreateObject("Wscript.Shell").Run Join(Args), 0, False
