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
    Public Shared lineBoostCopy As Long = 0
    Public Shared Function interpretLine(line As String, code As String, linescounter As Long, lineAccessible As TextBox) As String
        'MsgBox("line: " & line)
        Voids.startVoid(line, linescounter)
        ForLoops.Start(line, code, linescounter)
        ClassersInterpreter.start(line)
        Variables.start(line)
        VariableModification.start(line, linescounter)

        Dim returnStart = Returns.start(line)
        If returnStart = "exit" Then
            Return "exit"
        End If

        Dim resultIfCondition = IFconditions.start(line, code, linescounter)
        If IsNumeric(resultIfCondition) Then
            lineAccessible.Text = resultIfCondition.ToString
        Else
        End If
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
        Dim resultElseConditions = ElseConditions.start(line, linescounter, code)
        If IsNumeric(resultElseConditions) Then
            lineAccessible.Text = resultElseConditions.ToString
        Else
        End If
        'MsgBox("continue: " & line)
        If FormatConverters.getBeforeParenthesis(FormatConverters.removeSpacesAtBeginningAndEnd(line)).ToLower = "print" Then
            Dim splitFormatConverters As String = FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(5)
            Dim AbleToReadConverter As String = FormatConverters.getExpression(FormatConverters.ConvertToAbleToRead(splitFormatConverters))
            MsgBox(AbleToReadConverter, , "")
        End If

        Return ""
    End Function
    Shared linenumber = 0
    Public Shared Sub Start(code As String, Optional activated As Boolean = False, Optional lineBoost As Long = 0)
        Try
            Dim lineAccessible As New TextBox
            lineAccessible.Text = "0"
            Dim splitCode = code.Replace(vbTab, "").Split(codeLinesDelimiter)
            Dim linesCounter As Long = 0
            If activated Then
                lineBoostCopy = lineBoost
            End If
            For linenumbers = 0 To splitCode.Count - 1
                ' If Not Long.Parse(lineAccessible.Text) > splitCode.Count - 1 Then
                If Not Long.Parse(lineAccessible.Text) > splitCode.Count - 1 Then
                    ' MsgBox("code: " & code)
                    Dim line = splitCode(Long.Parse(lineAccessible.Text))
                    ' MsgBox("code : " & code & " / " & linesCounter.ToString)
                    Dim interpretAndGetResult As String = interpretLine(line, code, linesCounter, lineAccessible)

                    If interpretAndGetResult = "exit" Then
                        Exit Sub
                    End If
                    '     End If
                    '  End If
                    'define(dc) lol; 
                    'define(dc) lol = "Salut";
                    lineAccessible.Text = Long.Parse(lineAccessible.Text + 1).ToString
                    linesCounter = Long.Parse(lineAccessible.Text)
                    ' linenumber += Long.Parse(lineAccessible.Text + 1)

                    lineBoostCopy += 1
                End If
                '   End If
            Next
        Catch ex As StackOverflowException
            'pup error cause an infinite loop occured
        End Try
    End Sub

End Class