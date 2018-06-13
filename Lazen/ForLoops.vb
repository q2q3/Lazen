Imports System.Threading
Public Class ForLoops
    Public Shared listOfForLoopsLines As New ListBox
    Public Shared listOfForLoopsVariables As New ListBox
    Public Shared Function countOfCharacterInString(strings As String, character As String) As Long
        Dim counter As Long = 0

        For Each i As String In strings
            If i = character Then
                counter += 1
            End If
        Next

        Return counter
    End Function
    Public Shared Sub Start(line As String, code As String, linescounter As Long, Optional isFunction As Boolean = False, Optional functionName As String = "")
        If FormatConverters.getBeforeParenthesis(FormatConverters.removeSpacesAtBeginningAndEnd(line)).ToLower = "for" Then

            Dim GetExpressionOfFor As String = FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(3).
                Substring(0, FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(3).Length - 1))

            Dim counter As Long = 0
            Dim variableName As String = ""
            Dim StartIndex As String = ""
            Dim StopIndex As String = ""
            Dim variableClasser As String = ""


            For Each i As String In GetExpressionOfFor.Split("::")
                If i <> "" Then

                    'counters
                    '0 = variable name
                    '1 = start index
                    '2 = stop index

                    If counter = 0 Then
                        variableName = FormatConverters.getClasserAndVariableDelimited(i, "variable").ToLower
                        variableClasser = FormatConverters.getClasserAndVariableDelimited(i, "classer").ToLower
                    ElseIf counter = 2 Then
                        StartIndex = FormatConverters.getExpression(FormatConverters.removeSpacesAtBeginningAndEnd(i))
                    ElseIf counter = 4 Then
                        StopIndex = FormatConverters.getExpression(FormatConverters.removeSpacesAtBeginningAndEnd(i))
                    End If


                End If

                counter += 1

            Next


            If Not IsNumeric(StartIndex) Then

                'pup error cause startindex is not numeric

                Exit Sub
            End If
            If Not IsNumeric(StopIndex) Then

                'pup error cause stopindex is not numeric

                Exit Sub
            End If

            If Variables.VariableExists(variableName, variableClasser) Then

                Variables.EditVariable(variableName, StartIndex, variableClasser)
                listOfForLoopsVariables.Items.Add(variableName & ";;" & variableClasser)

            Else

                If Variables.ClasserExists(variableClasser) Then
                    Variables.CreateVariable(variableName, StartIndex, variableClasser)
                Else
                    'pup error message because classer is not existing
                    Exit Sub
                End If

                listOfForLoopsVariables.Items.Add(variableName & ";;" & variableClasser)
            End If
            Dim lineOfForStart As Long = linescounter
            Dim countOfAccoladesOuvrantes As Long = 0
            Dim linesCounting As Long = 0
            Dim stopLine As Long = 0
            Dim esfirstline As Boolean = True
            Dim tellToExitFor As Boolean = False
            For countLines As Long = linescounter + 1 To code.Split(ControlChars.Lf).Count - 1

                Dim actualLine As String = FormatConverters.removeSpacesAtBeginningAndEnd(code.Split(ControlChars.Lf)(countLines))

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

            Dim boutdecode As String = ""

            For AddingLines As Long = linescounter To stopLine
                listOfForLoopsLines.Items.Add(AddingLines)
            Next

            For getCodeOfLoop As Long = linescounter + 1 To stopLine - 1
                boutdecode += code.Split(ControlChars.Lf)(getCodeOfLoop) & ControlChars.Lf
            Next

            Dim countBoutDeCodeOuvrante As Long = countOfCharacterInString(boutdecode, "{")
            Dim finalresultForLoop As String = boutdecode

            For i4 As Long = lineOfForStart To Convert.ToInt32(lineOfForStart + linesCounting)
                Interpret.UsedFunctionsListBox.Items.Add(i4.ToString)
            Next

            Dim doIHaveToExitLoop As Boolean = False
            For loopFor As Long = Long.Parse(StartIndex) + 1 To Long.Parse(StopIndex)

                For Each i As String In finalresultForLoop.Replace(vbTab, "").Split(ControlChars.Lf)

                    If FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(i).ToLower) = "break" Then
                        doIHaveToExitLoop = True
                    End If

                Next


                If doIHaveToExitLoop Then
                    Exit For
                End If
                Interpret.Start(finalresultForLoop, isFunction, functionName)
                Variables.EditVariable(variableName, loopFor, variableClasser)
            Next
        End If
    End Sub
End Class