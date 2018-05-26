Public Class Functions
    Public Shared listOfFunctionsNames As New ListBox
    Public Shared listOfFunctionsArguments As New ListBox
    Public Shared listOfFunctionsCodes As New ListBox
    Public Shared listOfFunctionNamesForReturn As New ListBox
    Public Shared listOfFunctionReturns As New ListBox
    Public Shared listOfLines As New ListBox
    Public Shared lineStartForCopy As New ListBox
    Public Shared Function start(line As String, linescounter As Long, code As String)
        If FormatConverters.removeSpacesAtBeginningAndEnd(line).ToLower.StartsWith("function") Then
            Dim getFunctionName = FormatConverters.removeSpacesAtBeginningAndEnd(FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(0, FormatConverters.removeSpacesAtBeginningAndEnd(line).IndexOf("(")).Substring(8)).ToLower
            'function hey
            Dim getFunctionArguments = FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(FormatConverters.removeSpacesAtBeginningAndEnd(line).IndexOf("(")).Substring(0, FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(FormatConverters.removeSpacesAtBeginningAndEnd(line).IndexOf("(")).Length - 1)).ToLower

            If Not FormatConverters.isNothingOrSpace(getFunctionName) Then

                If Not Variables.ClasserExists(getFunctionName) Then
                    Variables.CreateClasser(getFunctionName)
                    Dim ouvrantes As Long = 0
                    Dim lineStart = linescounter + 1
                    Dim lineStop = 0
                    Dim tellToExitFor = False
                    For countLines = linescounter + 1 To code.Split(ControlChars.Lf).Count - 1
                        Dim getLine = code.Split(ControlChars.Lf)(countLines)
                        For Each i As String In getLine
                            If i = "{" Then
                                ouvrantes += 1
                            ElseIf i = "}" Then
                                If ouvrantes > 0 Then
                                    ouvrantes -= 1
                                Else
                                    lineStop = countLines
                                    tellToExitFor = True
                                    Exit For
                                End If

                            End If
                        Next
                        If tellToExitFor Then
                            Exit For
                        End If
                    Next

                    Dim codebuilt = ""

                    For lineStartToLineStop = lineStart To lineStop - 1
                        Interpret.UsedLinesForFunctions.Items.Add(lineStartToLineStop)
                        codebuilt += code.Split(ControlChars.Lf)(lineStartToLineStop) & ControlChars.Lf
                    Next

                    If checkIfReturnsSomething(codebuilt) Then
                        For Each i As String In getFunctionArguments.Split("::")
                            If Not FormatConverters.isNothingOrSpace(i) Then
                                Dim getArgName = FormatConverters.removeSpacesAtBeginningAndEnd(i)
                                Variables.CreateVariable(getArgName, "", FormatConverters.removeSpacesAtBeginningAndEnd(getFunctionName))
                            End If
                        Next
                        listOfFunctionsNames.Items.Add(getFunctionName.ToLower)
                        'listOfFunctionNamesForReturn.Items.Add(getFunctionName)
                        listOfFunctionReturns.Items.Add("NOTHING")
                        listOfFunctionsCodes.Items.Add(codebuilt)
                        listOfFunctionsArguments.Items.Add(getFunctionArguments)
                        lineStartForCopy.Items.Add(lineStart)

                        For lineStartToLineStop2 = lineStart To lineStop - 1
                            listOfLines.Items.Add(lineStartToLineStop2 & "-" & getFunctionName.ToLower)
                        Next


                        '  MsgBox("linestop function: " & lineStop)
                        '  MsgBox("functionName: " & getFunctionName)
                        '  MsgBox("functionCode: " & codebuilt)
                        '  MsgBox("functionArgs: " & getFunctionArguments)

                        Return lineStop
                    Else
                        'pup error cause function returns nothing
                        Return lineStop
                    End If
                Else
                    'pup error cause a classer with the name getfunctionname is already existing
                    Exit Function
                End If
            Else
                'pup error cause function name is missing
                Exit Function
            End If
        End If
    End Function
    Public Shared Function getNameOfFunction(linescounter As Long)
        'print("hey");
        'function hello(argument1){
        'lolilol
        'return "hey"; ---> 3
        '}
        'print("end of program");
        Dim code = Interpret.entireCode
        For i = linescounter To 0 Step -1
            'MsgBox("i: " & i)
            Dim getline = code.Replace(vbTab, "").Split(ControlChars.Lf)(i)
            '  MsgBox("getline : " & getline & " / line: " & i)
            If FormatConverters.removeSpacesAtBeginningAndEnd(getline).ToLower.StartsWith("function") Then
                Dim funcName = FormatConverters.removeSpacesAtBeginningAndEnd(FormatConverters.removeSpacesAtBeginningAndEnd(getline).ToLower.Substring(8)).Substring(0, FormatConverters.removeSpacesAtBeginningAndEnd(FormatConverters.removeSpacesAtBeginningAndEnd(getline).ToLower.Substring(8)).IndexOf("(")).ToLower
                Return funcName
                Exit Function
            End If
        Next
        Return ""
    End Function
    Public Shared Function checkIfReturnsSomething(code As String) As Boolean
        Dim realcode = code.Replace(vbTab, "")
        For Each i As String In realcode.Split(ControlChars.Lf)
            If FormatConverters.removeSpacesAtBeginningAndEnd(i).ToLower.StartsWith("return ") Then
                Return True
                Exit Function
                Exit For
            End If
        Next
        Return False
    End Function
End Class