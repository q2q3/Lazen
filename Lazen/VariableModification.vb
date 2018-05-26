Public Class VariableModification
    Public Shared Sub start(line, linescounter)
        If line.ToLower.Contains("(") AndAlso line.ToLower.Contains(")") AndAlso line.ToLower.Contains(":=") Then
            Dim varname = ""
            Dim varclasser = ""
            Dim varnewvalue = ""
            Dim splitDelimiterFirst = FormatConverters.removeSpacesAtBeginningAndEnd(FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(0, FormatConverters.removeSpacesAtBeginningAndEnd(line).IndexOf(":=")))
            Dim splitDelimiterSecond = FormatConverters.removeSpacesAtBeginningAndEnd(FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(FormatConverters.removeSpacesAtBeginningAndEnd(line).ToLower.IndexOf(":=") + 2))
            Try
                varname = splitDelimiterFirst.Substring(0, splitDelimiterFirst.IndexOf("(")).ToLower
                varclasser = FormatConverters.removeSpacesAtBeginningAndEnd(FormatConverters.ConvertToAbleToRead(splitDelimiterFirst.Substring(splitDelimiterFirst.IndexOf("(") + 1))).ToLower
                If Variables.ClasserExists(varclasser.ToLower) Then

                    varnewvalue = FormatConverters.getExpression(splitDelimiterSecond)

                    If Variables.VariableExists(varname, varclasser) Then
                        Variables.EditVariable(varname, varnewvalue, varclasser)
                    Else
                        'pup error cause variable doesn't exists
                    End If

                    '    MsgBox("varnewvalue: " & varnewvalue)
                Else
                    'pup error cause classer doesn't exists
                End If
            Catch ex As Exception

            End Try
            'editvariable(varaiblename;;classer :: newvalue);
        End If
    End Sub
End Class