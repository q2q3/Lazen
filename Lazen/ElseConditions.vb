Public Class ElseConditions
    Public Shared Function start(line, linescounter, code)
        'if(1 = 1){
        '   print("1 = 1");
        '}else{
        '   print("1 != 1");
        '}elseif(1 = 2){
        '
        'if(1 = 1){
        '
        'if(1 = 2){
        '}
        '}else{
        '}
        '   print("1 = 2");
        '}elsewhile(1 = 3){
        '   print("1 = 3");
        '}
        If line.ToString.Replace(" ", "").ToLower.StartsWith("}else") Then
            Dim whichOne As String = ""
            If line.ToString.ToLower.Contains("(") Then
                whichOne = FormatConverters.removeSpacesAtBeginningAndEnd(line.ToString.ToLower.Substring(0, line.ToString.IndexOf("("))).Substring(1)
            Else
                whichOne = FormatConverters.removeSpacesAtBeginningAndEnd(line.ToString.ToLower.Substring(0)).Substring(1, FormatConverters.removeSpacesAtBeginningAndEnd(line.ToString.ToLower.Substring(0)).LastIndexOf("{") - 1)
            End If
            'Dim whichOne = FormatConverters.removeSpacesAtBeginningAndEnd(line.ToString.ToLower.Substring(0, line.ToString.IndexOf("("))).Substring(1)
            MsgBox("whichone: " & whichOne)

            If whichOne.ToLower = "else" Then

                Dim lineStart = linescounter
                '   For i = lineStart To 0 Step -1
                '   Dim getLine = FormatConverters.removeSpacesAtBeginningAndEnd(Interpret.entireCode.Split(ControlChars.Lf)(i))
                ' MsgBox("linetoanalyse : " & getLine)
                Dim ouvrantes = 0
                    Dim modified = 0
                    Dim tellToExitFor = False

                    ElseIf whichOne.ToLower = "elseif" Then

            ElseIf whichOne.ToLower = "elsewhile" Then

            End If
        End If
    End Function
End Class