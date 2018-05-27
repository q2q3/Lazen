Imports System.Threading
Public Class ForLoops
    Public Shared Function countOfCharacterInString(strings As String, character As String) As Int32
        Dim counter = 0
        For Each i As String In strings
            If i = character Then
                counter += 1
            End If
        Next
        Return counter
    End Function
    Public Shared Sub Start(line As String, code As String, linescounter As Long)
        If FormatConverters.getBeforeParenthesis(FormatConverters.removeSpacesAtBeginningAndEnd(line)).ToLower = "for" Then
            Dim GetExpressionOfFor = FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(3).Substring(0, FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(3).Length - 1))
            Dim counter = 0
            Dim variableName = ""
            Dim StartIndex = ""
            Dim StopIndex = ""
            Dim variableClasser = ""
            For Each i As String In GetExpressionOfFor.Split("::")
                If i <> "" Then
                    'counters
                    '0 = variable name
                    '1 = start index
                    '2 = stop index
                    If counter = 0 Then
                        Dim variableEntire = FormatConverters.getExpression(i)
                        variableName = FormatConverters.getClasserAndVariableDelimited(variableEntire, "variable")
                        variableClasser = FormatConverters.getClasserAndVariableDelimited(variableEntire, "classer")
                    ElseIf counter = 2 Then
                        StartIndex = FormatConverters.getExpression(FormatConverters.removeSpacesAtBeginningAndEnd(i))
                    ElseIf counter = 4 Then
                        StopIndex = FormatConverters.getExpression(FormatConverters.removeSpacesAtBeginningAndEnd(i))
                    End If
                End If
                counter += 1
            Next
            ' MsgBox("variablename: " & variableName)
            '  MsgBox("startindex: " & StartIndex)
            '  MsgBox("stopindex: " & StopIndex)
            ' MsgBox("variableclasser: " & variableClasser)
            If Not IsNumeric(StartIndex) Then
                'pup error cause startindex is not numeric
                'MsgBox("notnumeric")
                Exit Sub
            End If
            If Not IsNumeric(StopIndex) Then
                'pup error cause stopindex is not numeric
                '  MsgBox("notnumeric")
                Exit Sub
            End If
            '  MsgBox("startindex: " & StartIndex)
            ' MsgBox("stopindex: " & StopIndex)
            If Variables.VariableExists(variableName, variableClasser) Then
                ' MsgBox("variableExistsmadafucker: " & variableName)
                Variables.EditVariable(variableName, StartIndex, variableClasser)
            Else
                If Variables.ClasserExists(variableClasser) Then
                    Variables.CreateVariable(variableName, StartIndex, variableClasser)
                Else
                    'pup error message because classer is not existing
                    Exit Sub
                End If
            End If
            Dim lineOfForStart = linescounter
            Dim countOfAccoladesOuvrantes = 0
            Dim linesCounting = 0
            Dim stopLine = 0
            Dim esfirstline = True
            Dim tellToExitFor = False
            For countLines = linescounter + 1 To code.Split(ControlChars.Lf).Count - 1

                Dim actualLine = code.Split(ControlChars.Lf)(countLines)

                For Each i As String In actualLine
                    If i = "{" Then
                        countOfAccoladesOuvrantes += 1
                    ElseIf i = "}" Then
                        If countOfAccoladesOuvrantes > 0 Then
                            countOfAccoladesOuvrantes -= 1
                        Else
                            stopLine = countLines
                            tellToExitFor = True
                            Exit For
                        End If
                    End If
                Next
                If tellToExitFor Then
                    Exit For
                End If
            Next
            Dim boutdecode = ""
            For getCodeOfLoop = linescounter + 1 To stopLine - 1
                boutdecode += code.Split(ControlChars.Lf)(getCodeOfLoop) & ControlChars.Lf
            Next
            Dim countBoutDeCodeOuvrante = countOfCharacterInString(boutdecode, "{")
            Dim finalresultForLoop = boutdecode
            '  For Each i As String In boutdecode

            '   If i = "}" Then
            '   If countBoutDeCodeOuvrante >= 1 Then
            '   countBoutDeCodeOuvrante -= 1
            'Else
            '     Exit For
            ' End If
            '  End If
            '      finalresultForLoop += i
            '      Next
            For i4 = lineOfForStart To Convert.ToInt32(lineOfForStart + linesCounting)
                Interpret.UsedFunctionsListBox.Items.Add(i4.ToString)
            Next
            '  MsgBox("startindex: " & StartIndex)
            '  MsgBox("stopindex: " & StopIndex)
            Dim doIHaveToExitLoop = False
            For loopFor = Long.Parse(StartIndex) + 1 To Long.Parse(StopIndex)
                For Each i As String In finalresultForLoop.Replace(vbTab, "").Split(ControlChars.Lf)
                    If FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(i).ToLower) = "break" Then
                        doIHaveToExitLoop = True
                    End If
                Next
                If doIHaveToExitLoop Then
                    Exit For
                End If
                '  MsgBox("finalresult: " & finalresultForLoop)
                Interpret.Start(finalresultForLoop)
                Variables.EditVariable(variableName, loopFor, variableClasser)
            Next
        End If
    End Sub
End Class