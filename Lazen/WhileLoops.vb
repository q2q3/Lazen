Public Class WhileLoops
    Public Shared ListOfWhileLoopsLines As New ListBox
    Public Shared Function getCodeOfWhile(input As String, linescounter As Long) As String

        Dim tellToExitFor As Boolean = False
        Dim lineStart As Long = linescounter
        Dim lineStop As Long = 0
        Dim composeCode As String = ""

        For i As Long = linescounter + 1 To input.Split(ControlChars.Lf).Count - 1

            Dim getLine As String = FormatConverters.removeSpacesAtBeginningAndEnd(input.Split(ControlChars.Lf)(i))
            Dim ouvrantes As Long = 0

            For Each i2 As String In getLine

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

        For composeLines As Long = lineStart To lineStop
            ListOfWhileLoopsLines.Items.Add(composeLines)
        Next

        For composeScript As Long = lineStart + 1 To lineStop - 1
            Dim getLineOfScript As String = input.Split(ControlChars.Lf)(composeScript)
            composeCode += getLineOfScript & ControlChars.Lf
        Next

        Return composeCode
    End Function
    Public Shared Function Start(line As String, linescounter As Long, code As String, Optional isFunction As Boolean = False, Optional functionName As String = "")
        If FormatConverters.getBeforeParenthesis(FormatConverters.removeSpacesAtBeginningAndEnd(line)).ToLower = "while" Then
            ''''''''''''''''''''''''''''''''''

            'examples

            'while(&Math.Get(1 + 1) = 2){
            '     Print("1 + 1 = 2");
            '}


            '// = or
            '>< = and

            'while("5" + "5" = "10" >< "8" + "8" = "16"){

            '}

            'while("5" + "5" = "10" // "5" + "8" = "10"){

            '}

            'while("5" + "5" = "10" >< "8" + "8" = "16" // "2" + "2" = "14"){

            '}


            '''''''''''''''''''''''''''''''''''

            Dim getCondition As String = FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(5).
            Substring(0, FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(5).LastIndexOf("{")))

            Dim getCode As String = getCodeOfWhile(code, linescounter)
            Dim condition As String = code.Split(ControlChars.Lf)(linescounter).Substring(code.Split(ControlChars.Lf)(linescounter).IndexOf("("))

            Dim buildCode As String = ""
            Dim ouvrantes2 As Long = 1

            ''''''''''''''''

            If condition.EndsWith("{") Then condition = condition.Substring(0, condition.Length - 1)
            condition = FormatConverters.ConvertToAbleToRead(condition)

            ''''''''''''''''



            '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

            For i2 As Long = linescounter + 1 To code.Split(ControlChars.Lf).Count - 1

                Dim getLine As String = FormatConverters.removeSpacesAtBeginningAndEnd(code.Split(ControlChars.Lf)(i2))

                For Each i3 As String In getLine
                    If i3 = "{" Then
                        ouvrantes2 += 1
                    ElseIf i3 = "}" Then
                        ouvrantes2 -= 1
                    End If

                    If ouvrantes2 = 0 Then
                        Exit For
                    End If
                Next

                buildCode += getLine & ControlChars.Lf

            Next

            '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''



            If getCondition.Contains("//") Or getCondition.Contains("><") Then

                If Not getCondition.Contains("//") Then
                    If getCondition.Contains("><") Then

                        If IFconditions.verifyandofcondition(getCondition) Then

                            While (countAmountOfTrueConditions(getCondition))

                                Interpret.Start(buildCode, isFunction, functionName)

                            End While

                        Else

                            Dim lineStart As Long = linescounter
                            Dim lineStop As Long = 0
                            Dim ouvrantes As Long = 0
                            Dim exitforsecond As Boolean = False
                            For countLines As Long = lineStart + 1 To code.Split(ControlChars.Lf).Count - 1

                                Dim i As String = FormatConverters.removeSpacesAtBeginningAndEnd(code.Split(ControlChars.Lf)(countLines))

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
                    Dim amountofor As Long = 0

                    For Each countAmountOfOr As String In getCondition.Split("//")

                        If Not countAmountOfOr = "" Then
                            amountofor += 1
                        End If

                    Next

                    Dim amountOfTrueConditions As Long = countAmountOfTrueConditions(getCondition)

                    If Not amountOfTrueConditions > 0 Then

                        '''''''''''''''''''''''''''''''''''''''''

                        Dim lineStart As Long = linescounter
                        Dim lineStop As Long = 0
                        Dim ouvrantes As Long = 0
                        Dim exitforsecond As Boolean = False

                        '''''''''''''''''''''''''''''''''''''''''

                        For countLines As Long = lineStart + 1 To code.Split(ControlChars.Lf).Count - 1

                            Dim i As String = FormatConverters.removeSpacesAtBeginningAndEnd(code.Split(ControlChars.Lf)(countLines))

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

                        While (countAmountOfTrueConditions(getCondition) >= 1)
                            Interpret.Start(buildCode, isFunction, functionName)
                        End While

                    End If
                End If

            Else
                Return startcondition(condition, buildCode, line, linescounter, isFunction, functionName)
            End If
        End If

        Return "ERROR-IN-WHILELOOPS"

    End Function
    Public Shared Function countAmountOfTrueConditions(getcondition As String) As Long
        Dim amountOfTrueConditions As Long = 0

        For Each i As String In getcondition.Split("//")

            If Not i = "" Then
                If IFconditions.verifyandofcondition(i) Then
                    amountOfTrueConditions += 1
                End If
            End If

        Next

        Return amountOfTrueConditions
    End Function
    Public Shared Function startcondition(getcondition As String, code As String, line As String, linescounter As String, Optional isFunction As Boolean = False, Optional functionName As String = "")

        Dim verifyCondition As String = ""
        verifyCondition = IFconditions.verifycondition(getcondition)

        If verifyCondition = "1" Then

            While (IFconditions.verifycondition(getcondition)) = "1"
                Interpret.Start(code, isFunction, functionName)
            End While

        Else

            Dim lineStart As Long = linescounter
            Dim lineStop As Long = 0
            Dim ouvrantes As Long = 1
            Dim tellToExitFor As Boolean = False

            For i As Long = lineStart + 1 To Interpret.entireCode.Split(ControlChars.Lf).Count - 1

                Dim getLine As String = Interpret.entireCode.Split(ControlChars.Lf)(i)

                For Each i2 As String In getLine
                    If getLine.Contains("}") Then
                        ouvrantes -= 1
                    ElseIf getLine.Contains("{") Then
                        ouvrantes += 1
                    End If

                    If ouvrantes = 1 Then
                        lineStop = i
                        tellToExitFor = True
                        Exit For
                    End If

                Next

                If tellToExitFor Then
                    Exit For
                End If

            Next

            Return Long.Parse(lineStop - 1).ToString
            Exit Function

        End If

        Return "abc"
    End Function
End Class