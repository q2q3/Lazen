Public Class Interpret
    Public Shared UsedFunctionsListBox As New ListBox
    Public Shared UsedFunctionsInConditions As New ListBox
    Public Shared UsedLinesForFunctions As New ListBox
    Public Shared entireCode As String = ""
    Shared codeLinesDelimiter = ControlChars.Lf
    Public Shared Function faitPartieDesLignesUtilisees(line As Long) As Boolean
        Dim found = False
        For Each i In UsedFunctionsListBox.Items
            If i = line Then
                found = True
                Exit For
            End If
        Next
        Return found
    End Function
    Public Shared Function faitPartieDesLignesUtiliseesIfConditions(line As Long) As Boolean
        Dim UsedFunctionsListBox As New ListBox
        Dim UsedFunctionsInConditions As New ListBox
        Dim found = False
        For Each i As String In UsedFunctionsInConditions.Items
            'MsgBox(i & " / " & line)
            If i = line Then
                found = True
                Exit For
            End If
        Next
        Return found
    End Function
    '  Public Shared Sub privateInterpreting(code As String, linescounter As Long)
    '   Dim lineAccessible As New TextBox

    '   End Sub
    Public Shared Sub interpretLine(line As String, code As String, linescounter As Long, lineAccessible As TextBox)
        ' MsgBox("linebypass: " & line & " / linecounter: " & linesCounter)
        '   Dim LeFaisPartieTruque = faitPartieDesLignesUtilisees(linesCounter)
        'Dim LeFaisPartieTruqueIfConditions = faitPartieDesLignesUtiliseesIfConditions(linesCounter)
        '   MsgBox(line & " / lefaispartie: " & LeFaisPartieTruque & " / token: " & token)
        ' If token = "force" Then
        '  LeFaisPartieTruque = False
        '  End If
        '  If iftoken = "force" Then
        '    LeFaisPartieTruqueIfConditions = False
        ' End If
        '  If LeFaisPartieTruqueIfConditions = True Then
        '   MsgBox("lefaispartietruqueifconditions: " & line & " / token: " & iftoken)
        '    End If
        '    If LeFaisPartieTruque = True Then
        '    MsgBox("lefaispartietruque" & " token " & token)
        '    End If
        ' If Not LeFaisPartieTruqueIfConditions Then
        'If Not LeFaisPartieTruque Then
        'Voids.createNewVoid(linesCounter, line, code)
        Voids.startVoid(line, linescounter)
        ForLoops.Start(line, code, linescounter)
        ClassersInterpreter.start(line)
        Variables.start(line)
        Dim resultIfCondition = IFconditions.start(line, code, linescounter)
        If IsNumeric(resultIfCondition) Then
            lineAccessible.Text = resultIfCondition.ToString
        Else
        End If

        'Voids.start(line)

        Dim resultVoidStart = Voids.start(line, linescounter, code)
        If IsNumeric(resultVoidStart) Then
            lineAccessible.Text = resultVoidStart.ToString
        Else
        End If

        Dim resultFunctionsStart = Functions.start(line, linescounter, code)
        If IsNumeric(resultFunctionsStart) Then
            lineAccessible.Text = resultFunctionsStart.ToString
        Else
        End If

        Dim resultWhileStart = WhileLoops.Start(line, linescounter, code)
        If IsNumeric(resultWhileStart) Then
            lineAccessible.Text = resultWhileStart.ToString
        Else
        End If
        If FormatConverters.removeSpacesAtBeginningAndEnd(line).ToLower.StartsWith("return") Then
            If UsedLinesForFunctions.Items.Contains(linescounter + 1) Then
                Dim returnOfFunction = FormatConverters.getExpression(FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(6))))
                Dim FunctionName = Functions.getNameOfFunction(linescounter + 1).ToString.ToLower
                    Dim indexOfFunctionName = Functions.listOfFunctionNamesForReturn.Items.IndexOf(FunctionName)
                    Functions.listOfFunctionNamesForReturn.Items.RemoveAt(Functions.listOfFunctionNamesForReturn.Items.IndexOf(FunctionName))
                    Functions.listOfFunctionReturns.Items.RemoveAt(indexOfFunctionName)

                    Functions.listOfFunctionNamesForReturn.Items.Add(FunctionName)
                    Functions.listOfFunctionReturns.Items.Add(returnOfFunction)
                    ' Functions.listOfFunctionReturns.Items.Add(returnOfFunction)
                End If
            End If

        'MsgBox("continue: " & line)
        If FormatConverters.removeSpacesAtBeginningAndEnd(line).ToLower.StartsWith("print") Then
            Dim splitFormatConverters = FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(5)
            Dim AbleToReadConverter = FormatConverters.getExpression(FormatConverters.ConvertToAbleToRead(splitFormatConverters))
            MsgBox(AbleToReadConverter, , "")
        End If



        If FormatConverters.removeSpacesAtBeginningAndEnd(line).ToLower.StartsWith("editvariable") Then
            'editvariable(varaiblename;;classer :: newvalue);
            Dim splitFormatConverters = FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(12)
            Dim splitFormatConvertToAbleToRead = FormatConverters.ConvertToAbleToRead(splitFormatConverters)
            Dim patronvariableclasser = FormatConverters.getExpression(FormatConverters.removeSpacesAtBeginningAndEnd(splitFormatConvertToAbleToRead.Split("::")(0)))
            Dim variablename = FormatConverters.getClasserAndVariableDelimited(patronvariableclasser, "variable")
            Dim variableNewValue = FormatConverters.getExpression(FormatConverters.removeSpacesAtBeginningAndEnd(splitFormatConvertToAbleToRead.Split("::")(2)))
            Dim variableClasser = FormatConverters.getClasserAndVariableDelimited(patronvariableclasser, "classer")
            If Not patronvariableclasser.Contains(";;") Then
                'error cause variable doesn't contains a classer
                'Exit Sub
            End If
            Variables.EditVariable(variablename, variableNewValue, variableClasser)

        End If
    End Sub
    Shared linenumber = 0
    Public Shared Sub Start(code As String)
        Dim lineAccessible As New TextBox
        lineAccessible.Text = "0"
        Dim splitCode = code.Replace(vbTab, "").Split(codeLinesDelimiter)
        Dim linesCounter As Long = 0
        For linenumbers = 0 To splitCode.Count - 1
            ' If Not Long.Parse(lineAccessible.Text) > splitCode.Count - 1 Then
            If Not Long.Parse(lineAccessible.Text) > splitCode.Count - 1 Then
                ' MsgBox("code: " & code)
                Dim line = splitCode(Long.Parse(lineAccessible.Text))
                ' MsgBox("code : " & code & " / " & linesCounter.ToString)
                interpretLine(line, code, linesCounter, lineAccessible)
                '     End If
                '  End If
                'define(dc) lol; 
                'define(dc) lol = "Salut";
                lineAccessible.Text = Long.Parse(lineAccessible.Text + 1).ToString
                linesCounter = Long.Parse(lineAccessible.Text)
                ' linenumber += Long.Parse(lineAccessible.Text + 1)
            End If
            '   End If
        Next
    End Sub

End Class