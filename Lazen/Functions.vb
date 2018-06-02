Public Class Functions
    Public Shared listOfFunctionsNames As New ListBox
    Public Shared listOfFunctionsArguments As New ListBox
    Public Shared listOfFunctionsCodes As New ListBox
    Public Shared listOfFunctionNamesForReturn As New ListBox
    Public Shared listOfFunctionReturns As New ListBox
    Public Shared listOfLines As New ListBox
    Public Shared lineStartForCopy As New ListBox

    Public Shared Function start(line As String, linescounter As Long, code As String)

        If FormatConverters.removeSpacesAtBeginningAndEnd(line).ToLower.StartsWith("function ") Then
            Dim getFunctionName As String = FormatConverters.removeSpacesAtBeginningAndEnd(FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(0, FormatConverters.removeSpacesAtBeginningAndEnd(line).IndexOf("(")).Substring(8)).ToLower
            Dim getFunctionArguments As String = FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(FormatConverters.removeSpacesAtBeginningAndEnd(line).IndexOf("(")).Substring(0, FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(FormatConverters.removeSpacesAtBeginningAndEnd(line).IndexOf("(")).Length - 1)).ToLower

            If Not FormatConverters.isNothingOrSpace(getFunctionName) Then

                If Not Variables.ClasserExists(getFunctionName) Then
                    Variables.CreateClasser(getFunctionName)

                    Dim ouvrantes As Long = 0
                    Dim lineStart As Long = linescounter + 1
                    Dim lineStop As Long = 0
                    Dim tellToExitFor As Boolean = False
                    Dim codebuilt As String = ""

                    For countLines = linescounter + 1 To code.Split(ControlChars.Lf).Count - 1

                        Dim getLine As String = code.Split(ControlChars.Lf)(countLines)

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

                    For lineStartToLineStop As Long = lineStart To lineStop - 1
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
                        listOfFunctionReturns.Items.Add("LAZEN-NOTHING")
                        listOfFunctionsCodes.Items.Add(codebuilt)
                        listOfFunctionsArguments.Items.Add(getFunctionArguments)
                        lineStartForCopy.Items.Add(lineStart)

                        For lineStartToLineStop2 As Long = lineStart To lineStop - 1
                            listOfLines.Items.Add(lineStartToLineStop2 & "-" & getFunctionName.ToLower)
                        Next

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
    Public Shared Function getNameOfFunction(linescounter As Long) As String
        Dim code As String = Interpret.entireCode
        For i As Long = linescounter To 0 Step -1

            Dim getline As Long = code.Replace(vbTab, "").Split(ControlChars.Lf)(i)

            If FormatConverters.removeSpacesAtBeginningAndEnd(getline).ToLower.StartsWith("function") Then
                Dim funcName As String = FormatConverters.removeSpacesAtBeginningAndEnd(FormatConverters.removeSpacesAtBeginningAndEnd(getline).ToLower.Substring(8)).Substring(0, FormatConverters.removeSpacesAtBeginningAndEnd(FormatConverters.removeSpacesAtBeginningAndEnd(getline).ToLower.Substring(8)).IndexOf("(")).ToLower
                Return funcName
                Exit Function
            End If

        Next
        Return ""
    End Function
    Public Shared Function checkIfReturnsSomething(code As String) As Boolean
        Dim realcode As String = code.Replace(vbTab, "")

        For Each i As String In realcode.Split(ControlChars.Lf)

            If FormatConverters.removeSpacesAtBeginningAndEnd(i).ToLower.StartsWith("return ") Then
                Return True
                Exit Function
            End If

        Next

        Return False
    End Function
End Class