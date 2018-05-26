Public Class WhileLoops
    Public Shared Function getCodeOfWhile(input As String, linescounter As Long)
        Dim tellToExitFor = False
        Dim lineStart = linescounter
        Dim lineStop As Long = 0
        For i = linescounter + 1 To input.Split(ControlChars.Lf).Count - 1
            Dim getLine = input.Split(ControlChars.Lf)(i)
            Dim ouvrantes = 0
            For Each i2 In getLine
                If i2 = "{" Then
                    ouvrantes += 1
                ElseIf i2 = "}" Then
                    If ouvrantes > 0 Then
                        ouvrantes -= 1
                    Else
                        lineStop = i
                        tellToExitFor = True
                        Exit For
                    End If
                End If
            Next
            If tellToExitFor Then
                Exit For
            End If
        Next
        Dim composeCode = ""
        For composeScript = lineStart + 1 To lineStop - 1
            Dim getLineOfScript = input.Split(ControlChars.Lf)(composeScript)
            composeCode += getLineOfScript & ControlChars.Lf
        Next
        '  MsgBox("composecode: " & composeCode)
        Return composeCode
    End Function
    Public Shared Function Start(line As String, linescounter As Long, code As String)
        If FormatConverters.removeSpacesAtBeginningAndEnd(line).ToLower.StartsWith("while") Then
            'while(&Math.Get(1 + 1) = 2){
            '     Print("1 + 1 = 2");
            '}
            Dim getCondition = FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(5).Substring(0, FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(5).LastIndexOf("{")))
            ' Dim getCondition = FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(5, FormatConverters.removeSpacesAtBeginningAndEnd(line).LastIndexOf("{")))
            'MsgBox("getcondition : " & getCondition)
            Dim getCode = getCodeOfWhile(code, linescounter)

            Dim buildCode = ""
            For i2 = linescounter + 1 To code.Split(ControlChars.Lf).Count - 2
                buildCode += code.Split(ControlChars.Lf)(i2) & ControlChars.Lf
            Next

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

                        If IFconditions.verifyandofcondition(getCondition) Then
                            While (countAmountOfTrueConditions(getCondition))
                                ' MsgBox(buildCode)
                                Interpret.Start(buildCode)
                            End While
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
                    Dim amountOfTrueConditions = countAmountOfTrueConditions(getCondition)
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
                        While (countAmountOfTrueConditions(getCondition))
                            'MsgBox(buildCode)
                            Interpret.Start(buildCode)
                        End While
                    End If
                End If
            Else
                Return startcondition(getCondition, buildCode, line, linescounter)
            End If
        End If
    End Function
    Public Shared Function countAmountOfTrueConditions(getcondition As String)
        Dim amountOfTrueConditions = 0
        For Each i As String In getcondition.Split("//")
            If Not i = "" Then
                If IFconditions.verifyandofcondition(i) Then
                    amountOfTrueConditions += 1
                End If
            End If
        Next
        Return amountOfTrueConditions
    End Function
    Public Shared Function startcondition(getcondition As String, code As String, line As String, linescounter As String)
        ' MsgBox("code: " & code)
        ' MsgBox("verifying condition : " & getcondition)
        Dim verifyCondition = ""
        verifyCondition = IFconditions.verifycondition(getcondition)
        If verifyCondition = "1" Then
            '  MsgBox(code)
            '   MsgBox("ok")
            While (IFconditions.verifycondition(getcondition)) = "1"
                Interpret.Start(code)
            End While
        Else
            Dim lineStart = linescounter
            Dim lineStop = 0
            Dim ouvrantes = 0
            Dim tellToExitFor = False
            '  MsgBox("code: " & code)
            For i = lineStart + 1 To Interpret.entireCode.Split(ControlChars.Lf).Count - 1
                Dim getLine = Interpret.entireCode.Split(ControlChars.Lf)(i)
                For Each i2 In getLine
                    If i2 = "{" Then
                        ouvrantes += 1
                    ElseIf i2 = "}" Then
                        If ouvrantes > 0 Then
                            ouvrantes -= 1
                        Else
                            lineStop = i
                            tellToExitFor = True
                            Exit For
                        End If
                    End If
                Next
                If tellToExitFor Then
                    Exit For
                End If
            Next
            ' MsgBox("linestop: " & lineStop.ToString)
            Return Long.Parse(lineStop - 1).ToString
            Exit Function
        End If
        Return "abc"

    End Function
End Class