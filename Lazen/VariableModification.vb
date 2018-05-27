Public Class VariableModification
    Public Shared Sub start(line As String, linescounter As Long)
        If FormatConverters.getBeforeParenthesis(FormatConverters.removeSpacesAtBeginningAndEnd(line)).ToLower = "edit" Then

            Dim splitUntilParenthesis As String = FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(4))
            Dim getVariableAndClasserToEdit As String = FormatConverters.removeSpacesAtBeginningAndEnd(splitUntilParenthesis.Substring(0, splitUntilParenthesis.IndexOf(":")))
            Dim getNewValue As String = FormatConverters.getExpression(FormatConverters.removeSpacesAtBeginningAndEnd(splitUntilParenthesis.Substring(splitUntilParenthesis.IndexOf(":") + 2)))

            Dim getVariableName As String = FormatConverters.getClasserAndVariableDelimited(getVariableAndClasserToEdit, "variable").ToLower
            Dim getClasser As String = FormatConverters.getClasserAndVariableDelimited(getVariableAndClasserToEdit, "classer").ToLower

            Variables.EditVariable(getVariableName, getNewValue, getClasser)

        End If
    End Sub
End Class