Public Class Interpret
    Public Shared UsedFunctionsListBox As New ListBox
    Public Shared UsedFunctionsInConditions As New ListBox
    Public Shared UsedLinesForFunctionsAndVoids As New ListBox

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
            If i = line Then
                found = True
                Exit For
            End If
        Next
        Return found
    End Function
    Public Shared lineBoostCopy As Long = 0
    Public Shared Function interpretLine(line As String, code As String, linescounter As Long, lineAccessible As TextBox, Optional isFunction As Boolean = False, Optional functionName As String = "") As String
        Voids.startVoid(line, linescounter)
        ForLoops.Start(line, code, linescounter, isFunction, functionName)
        ClassersInterpreter.start(line)
        Variables.start(line)
        VariableModification.start(line, linescounter)

        Dim returnStart = Returns.start(line, isFunction, functionName)
        If returnStart = "exit" Then
            Return "exit"
        End If

        Dim resultIfCondition = IFconditions.start(line, code, linescounter)
        If IsNumeric(resultIfCondition) Then
            lineAccessible.Text = resultIfCondition.ToString
        Else
        End If

        Dim resultTicketStart = GotoTickets.Start(line)
        If IsNumeric(resultTicketStart) Then
            lineAccessible.Text = resultTicketStart.ToString
        Else
        End If

        Dim resultWhileStart = WhileLoops.Start(line, linescounter, code, isFunction, functionName)
        If IsNumeric(resultWhileStart) Then
            lineAccessible.Text = resultWhileStart.ToString
        Else
        End If

        Dim resultElseConditions = ElseConditions.start(line, linescounter, code)
        If IsNumeric(resultElseConditions) Then
            lineAccessible.Text = resultElseConditions.ToString
        Else
        End If

        If FormatConverters.getBeforeParenthesis(FormatConverters.removeSpacesAtBeginningAndEnd(line)).ToLower = "print" Then
            Dim splitFormatConverters As String = FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(5)
            Dim AbleToReadConverter As String = FormatConverters.getExpression(FormatConverters.ConvertToAbleToRead(splitFormatConverters))
            MsgBox(AbleToReadConverter, , "")
        End If

        Return ""
    End Function
    Public Shared Sub Start(code As String, Optional isFunction As Boolean = False, Optional functionName As String = "")
        Try
            Dim lineAccessible As New TextBox
            lineAccessible.Text = "0"
            Dim splitCode As String() = code.Replace(vbTab, "").Split(codeLinesDelimiter)
            Dim linesCounter As Long = 0

            For linenumbers As Long = 0 To splitCode.Count - 1

                If Not Long.Parse(lineAccessible.Text) > splitCode.Count - 1 Then

                    Dim line As String = splitCode(Long.Parse(lineAccessible.Text))
                    Dim interpretAndGetResult As String = interpretLine(line, code, linesCounter, lineAccessible, isFunction, functionName)
                    If interpretAndGetResult = "exit" Then
                        Exit Sub
                    End If

                    lineAccessible.Text = Long.Parse(lineAccessible.Text + 1).ToString
                    linesCounter = Long.Parse(lineAccessible.Text)
                End If

            Next
        Catch ex As StackOverflowException
            'pup error cause an infinite loop occured
        End Try
    End Sub

End Class
