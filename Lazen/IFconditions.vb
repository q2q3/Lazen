Public Class IFconditions
    Public Shared Function start(line As String, code As String, linescounter As Long)
        If FormatConverters.removeSpacesAtBeginningAndEnd(line).ToLower.StartsWith("if") Then
            Dim getCondition = FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(2)).Substring(0, FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(2)).LastIndexOf("{"))

            If getCondition.Contains("//") Or getCondition.Contains("><") Then
                '// = or
                '>< = and
                'if("5" + "5" = "10" >< "8" + "8" = "16"){
                '
                '}

                'if("5" + "5" = "10" // "5" + "8" = "10"){
                '
                '}

                'if("5" + "5" = "10" >< "8" + "8" = "16" // "2" + "2" = "14"){
                '
                '}

                If Not getCondition.Contains("//") Then
                    If getCondition.Contains("><") Then

                        If verifyandofcondition(getCondition) Then
                            'allow to execute code
                        Else
                            Dim lineStart = linescounter
                            Dim lineStop = 0
                            Dim ouvrantes = 0
                            Dim exitforsecond = False
                            For countLines = lineStart + 1 To code.Split(ControlChars.Lf).Count - 1
                                Dim i = code.Split(ControlChars.Lf)(countLines)
                                For Each countchar As String In i
                                    If countchar = "{" Then
                                        ouvrantes += 1
                                    ElseIf countchar = "}" Then
                                        If ouvrantes > 0 Then
                                            ouvrantes -= 1
                                        Else
                                            lineStop = countLines
                                            exitforsecond = True
                                            Exit For
                                        End If
                                    End If
                                Next
                                If exitforsecond = True Then
                                    Exit For
                                End If
                            Next
                            Return Long.Parse(lineStop - 1).ToString
                        End If
                    End If
                ElseIf getCondition.Contains("//") Then
                    Dim amountofor = 0
                    For Each countAmountOfOr As String In getCondition.Split("//")
                        If Not countAmountOfOr = "" Then
                            amountofor += 1
                        End If
                    Next
                    Dim amountOfTrueConditions = 0
                    For Each i As String In getCondition.Split("//")
                        If Not i = "" Then
                            If verifyandofcondition(i) Then
                                amountOfTrueConditions += 1
                            End If
                        End If
                    Next
                    If Not amountOfTrueConditions > 0 Then
                        Dim lineStart = linescounter
                        Dim lineStop = 0
                        Dim ouvrantes = 0
                        Dim exitforsecond = False
                        For countLines = lineStart + 1 To code.Split(ControlChars.Lf).Count - 1
                            Dim i = code.Split(ControlChars.Lf)(countLines)
                            For Each countchar As String In i
                                If countchar = "{" Then
                                    ouvrantes += 1
                                ElseIf countchar = "}" Then
                                    If ouvrantes > 0 Then
                                        ouvrantes -= 1
                                    Else
                                        lineStop = countLines
                                        exitforsecond = True
                                        Exit For
                                    End If
                                End If
                            Next
                            If exitforsecond = True Then
                                Exit For
                            End If
                        Next
                        Return Long.Parse(lineStop - 1).ToString
                    Else
                        'allow to execute code
                    End If
                End If
            Else
                Return startcondition(getCondition, code, line, linescounter)
            End If
        End If


    End Function
    Public Shared Function verifyandofcondition(getcondition As String)
        Dim listOfVerifyConditions As New ListBox
        Dim counterOfCondition = 0
        For Each i As String In getcondition.Split("><")
            If Not i = "" Then
                Dim conditionPartOriginal = i
                If counterOfCondition = getcondition.Split("><").Count - 1 Then
                    conditionPartOriginal = conditionPartOriginal
                End If
                If conditionPartOriginal.StartsWith("<") Then
                    conditionPartOriginal = conditionPartOriginal.Substring(1)
                End If
                conditionPartOriginal = FormatConverters.removeSpacesAtBeginningAndEnd(FormatConverters.ConvertToAbleToRead(conditionPartOriginal))
                listOfVerifyConditions.Items.Add(verifycondition(conditionPartOriginal))
            End If
            counterOfCondition += 1
        Next

        For Each i2 As String In listOfVerifyConditions.Items
            If i2 = "0" Then
                Return False
                Exit Function
            End If
        Next
        Return True
    End Function
    Public Shared Function verifycondition(getconditions As String)

        If FormatConverters.getExpression(FormatConverters.ConvertToAbleToRead(getconditions)) = "1" Then
            Return "1"
            Exit Function
        ElseIf FormatConverters.getExpression(FormatConverters.ConvertToAbleToRead(getconditions)) = "0" Then
            Return "0"
            Exit Function
        End If
        Dim getcondition = getconditions
        Dim getfirstobject
        Dim getsecondobject
        If getcondition.Contains("!=") Then
            getfirstobject = FormatConverters.getExpression(FormatConverters.removeSpacesAtBeginningAndEnd(getcondition.Split("!=")(0)))
            getsecondobject = FormatConverters.getExpression(FormatConverters.removeSpacesAtBeginningAndEnd(getcondition.Split("!=")(1).Substring(1).Substring(0, getcondition.Split("!=")(1).Substring(1).Length)))

            If getfirstobject <> getsecondobject Then
                Return "1"
            Else
                Return "0"
            End If
        ElseIf getcondition.Contains("<=") Then
            getfirstobject = FormatConverters.getExpression(FormatConverters.removeSpacesAtBeginningAndEnd(getcondition.Split("<=")(0)))
            getsecondobject = FormatConverters.getExpression(FormatConverters.removeSpacesAtBeginningAndEnd(getcondition.Split("<=")(1).Substring(1).Substring(0, getcondition.Split("<=")(1).Substring(1).Length)))
            If Long.Parse(getfirstobject) <= Long.Parse(getsecondobject) Then
                Return "1"
            Else
                Return "0"
            End If
        ElseIf getcondition.Contains(">=") Then
            getfirstobject = FormatConverters.getExpression(FormatConverters.removeSpacesAtBeginningAndEnd(getcondition.Split(">=")(0)))
            getsecondobject = FormatConverters.getExpression(FormatConverters.removeSpacesAtBeginningAndEnd(getcondition.Split(">=")(1).Substring(1).Substring(0, getcondition.Split(">=")(1).Substring(1).Length)))
            If Long.Parse(getfirstobject) >= Long.Parse(getsecondobject) Then
                Return "1"
            Else
                Return "0"
            End If
        ElseIf getcondition.Contains("=") Then
            getfirstobject = FormatConverters.getExpression(FormatConverters.removeSpacesAtBeginningAndEnd(getcondition.Split("=")(0)))
            getsecondobject = FormatConverters.getExpression(FormatConverters.removeSpacesAtBeginningAndEnd(getcondition.Split("=")(1).Substring(1).Substring(0, getcondition.Split("=")(1).Substring(1).Length)))
            If getfirstobject = getsecondobject Then
                Return "1"
            Else
                Return "0"
            End If
        ElseIf getcondition.Contains("<") Then
            getfirstobject = FormatConverters.getExpression(FormatConverters.removeSpacesAtBeginningAndEnd(getcondition.Split("<")(0)))
            getsecondobject = FormatConverters.getExpression(FormatConverters.removeSpacesAtBeginningAndEnd(getcondition.Split("<")(1).Substring(1).Substring(0, getcondition.Split("<")(1).Substring(1).Length)))
            If Long.Parse(getfirstobject) < Long.Parse(getsecondobject) Then
                Return "1"
            Else
                Return "0"
            End If
            Return ""
        ElseIf getcondition.Contains(">") Then
            getfirstobject = FormatConverters.getExpression(FormatConverters.removeSpacesAtBeginningAndEnd(getcondition.Split(">")(0)))
            getsecondobject = FormatConverters.getExpression(FormatConverters.removeSpacesAtBeginningAndEnd(getcondition.Split(">")(1).Substring(1).Substring(0, getcondition.Split(">")(1).Substring(1).Length)))
            If Long.Parse(getfirstobject) > Long.Parse(getsecondobject) Then
                Return "1"
            Else
                Return "0"
            End If
        Else
            Return ""
        End If
    End Function
    Public Shared Function startcondition(getcondition As String, code As String, line As String, linescounter As String)
        If verifycondition(getcondition) = "1" Then

        Else
            Dim lineStart = linescounter
            Dim lineStop = 0
            Dim ouvrantes = 0
            Dim exitforsecond = False
            For countLines = lineStart + 1 To code.Split(ControlChars.Lf).Count - 1
                Dim i = code.Split(ControlChars.Lf)(countLines)
                For Each countchar As String In i
                    If countchar = "{" Then
                        ouvrantes += 1
                    ElseIf countchar = "}" Then
                        If ouvrantes > 0 Then
                            ouvrantes -= 1
                        Else
                            lineStop = countLines
                            exitforsecond = True
                            Exit For
                        End If
                    End If
                Next
                If exitforsecond = True Then
                    Exit For
                End If
            Next
            Return Long.Parse(lineStop - 1).ToString
            Exit Function
        End If
        Return "abc"

    End Function
End Class