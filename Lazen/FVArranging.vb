Public Class FVArranging
    Public Shared Function Arrange(code As String) As String
        Dim linescounter As Long = 0
        Dim functionsAndVoidsCode As String = ""
        Dim listOfLinesToDelete As New List(Of Long)
        Dim result As String = ""
        Dim realResult As String = ""

        For Each line As String In code.Split(ControlChars.Lf)
            If FormatConverters.removeSpacesAtBeginningAndEnd(line).ToLower.StartsWith("function ") Then
                Dim resultFunctionsStart = Functions.start(line, linescounter, code)
                If IsNumeric(resultFunctionsStart) Then

                    Dim lineStart As Long = linescounter
                    Dim lineStop As Long = resultFunctionsStart

                    Dim codeOfFunction As String = ""

                    For i As Long = lineStart To lineStop
                        listOfLinesToDelete.Add(i)
                        codeOfFunction &= code.Split(ControlChars.Lf)(i) & ControlChars.Lf
                    Next

                    functionsAndVoidsCode &= codeOfFunction & ControlChars.Lf

                Else
                End If

            End If

            If FormatConverters.removeSpacesAtBeginningAndEnd(line).ToLower.StartsWith("void ") Then
                Dim resultVoidsStart = Voids.start(line, linescounter, code)
                If IsNumeric(resultVoidsStart) Then

                    Dim lineStart As Long = linescounter
                    Dim lineStop As Long = resultVoidsStart

                    Dim codeOfVoid As String = ""

                    For i As Long = lineStart To lineStop
                        listOfLinesToDelete.Add(i)
                        codeOfVoid &= code.Split(ControlChars.Lf)(i) & ControlChars.Lf
                    Next

                    functionsAndVoidsCode &= codeOfVoid & ControlChars.Lf

                Else
                End If

            End If

            linescounter += 1
        Next
        result = FormatConverters.DeleteLines(code, listOfLinesToDelete)
        realResult = functionsAndVoidsCode & ControlChars.Lf & result

        Return realResult
    End Function
End Class