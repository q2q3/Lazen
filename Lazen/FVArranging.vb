Public Class FVArranging
    Public Shared Function Arrange(code As String) As String
        Dim linescounter As Long = 0
        Dim functionsAndVoidsCode As String = ""
        Dim listOfLinesToDelete As New List(Of Long)
        Dim result As String = ""
        Dim realResult As String = ""

        For Each line As String In code.Split(ControlChars.Lf)
            If FormatConverters.removeSpacesAtBeginningAndEnd(line).ToLower.StartsWith("function ") Then

                Dim ouvrantes As Long = 0
                Dim lineStart As Long = linescounter
                Dim lineStop As Long = 0
                Dim tellToExitFor As Boolean = False
                Dim codebuilt As String = ""

                For countLines As Long = linescounter + 1 To code.Split(ControlChars.Lf).Count - 1

                    Dim getLine As String = FormatConverters.removeSpacesAtBeginningAndEnd(code.Split(ControlChars.Lf)(countLines))

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

                Dim codeOfFunction As String = ""

                For i As Long = lineStart To lineStop
                    listOfLinesToDelete.Add(i)
                    codeOfFunction &= code.Split(ControlChars.Lf)(i) & ControlChars.Lf
                Next

                functionsAndVoidsCode &= codeOfFunction & ControlChars.Lf

            Else
            End If


            If FormatConverters.removeSpacesAtBeginningAndEnd(line).ToLower.StartsWith("void ") Then

                Dim ouvrantes As Long = 0
                Dim lineStart As Long = linescounter
                Dim lineStop As Long = 0
                Dim tellToExitFor As Boolean = False
                Dim codebuilt As String = ""

                For countLines As Long = linescounter + 1 To code.Split(ControlChars.Lf).Count - 1

                    Dim getLine As String = FormatConverters.removeSpacesAtBeginningAndEnd(code.Split(ControlChars.Lf)(countLines))

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

                Dim codeOfVoid As String = ""

                For i As Long = lineStart To lineStop
                    listOfLinesToDelete.Add(i)
                    codeOfVoid &= code.Split(ControlChars.Lf)(i) & ControlChars.Lf
                Next

                functionsAndVoidsCode &= codeOfVoid & ControlChars.Lf

            Else
            End If


            linescounter += 1
        Next
        result = FormatConverters.DeleteLines(code, listOfLinesToDelete)
        realResult = functionsAndVoidsCode & ControlChars.Lf & result

        Return realResult
    End Function
End Class